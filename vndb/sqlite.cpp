#include "vndb.h"
template<typename T>
using vector = util::svector<T>;//can change to std::vector
using string = util::string;//can change to std::string
static constexpr std::string_view sql_select_by_id =
  "select * from @table where id = @id;"sv;
static constexpr std::string_view sql_insert_vn =
  R"EOF(insert or replace into VNs values (
        @id, @title, @original, @released,
        @languages, @orig_lang, @platform, @aliases,
        @length, @description, @links, @image, @image_nsfw,
        @anime, @relations, @tags, @popularity, @rating,
        @votecount, @screens, @staff, '[]', '[]' ,'[]',
        -1, -1, @date, 0)EOF"sv;
static constexpr std::string_view sql_insert_producer =
  R"EOF(insert or relpace into producers values (
        @id, @name, @original, @type, @language,
        @links, @aliases, @description, @relations)EOF"sv;
static constexpr std::string_view sql_insert_release =
  R"EOF(insert or relpace into releases values (
        @id, @title, @original, @released, @type,
        @patch, @languages, @website, @notes, @minage,
        @platforms, @resolution, @voiced, @animation,
        @vn, @producers)EOF"sv;
// Open an in memory database and copy the contents of the datebase located
// in filename into it. Written using a simlar interface as most sqlite3 api
// functions.
extern "C" {
int sqlite_open_db_in_memory(const char* filename, sqlite3 **db_ptr){
  sqlite3 *file = nullptr;
  //insure calling sqlite3_close on *dp_ptr after an error will work.
  *dp_ptr = nullptr;
  int err = sqlite3_open_v2(filename.data(), &file, 
                            SQLITE_OPEN_READWRITE, nullptr);
  if(err != SQLITE_OK){ goto end; }

  err = sqlite3_open_v2(":memory:", db_ptr, SQLITE_OPEN_READWRITE, nullptr);
  if(err != SQLITE_OK){ goto end; }

  sqlite3 *mem = *db_ptr;
  sqlite3_backup *bkup = sqlite3_backup_init(mem, "main", file, "main");
  if(!bkup){
    err = sqlite3_errcode(mem);
    goto end;
  }
  err = sqlite3_backup_step(bkup, -1);  //Copy all the pages at once.
  //we just opened both db connections, so these should be impossible
  assert(err != SQLITE_BUSY && err != SQLITE_LOCKED);
  
  //returns SQLITE_OK if there were no errors on bkup
  err = sqlite3_backup_finish(bkup);

 end:
  sqlite3_close(file);
  return err;
}
}  
  
  
  
}
//Convert a list stored in a string using delim as a seperator into a
//json array. eg "a,b,c",',' -> ["a","b","c"];
json parse_delimted_string(const char *str, char delim){
  std::vector<std::string> arr;
  while(*str){
    const char *start = str;
    while(*str && *str != delim){
      ++str;
    }
    arr.emplace_back(start, str - start);
    if(*str == '\0'){ break; }
    ++str;//skip past newline
  }
  return json(std::move(arr));
}
//add a vn given as json into the database
//This will probably be a member function so stmt will likely
//not be a parameter but a member variable.
int sqlite_insert_vn(json vn, sqlite3_stmt_wrapper& stmt){
  int idx = 0;
  //get_ptr is used for values that might be null, get/get_ref are
  //used for values that can't be null.
  stmt.bind(idx++, vn["id"].get<int>())
  stmt.bind(idx++, vn["title"].get_ref<json::string_t>());
  stmt.bind(idx++, vn["original"].get_ptr<json::string_view_t>());
  stmt.bind(idx++, vn["date"].get_ptr<json::string_view_t>());
  stmt.bind(idx++, vn["languages"]);
  stmt.bind(idx++, vn["orig_lang"]);
  stmt.bind(idx++, vn["platforms"]);
  stmt.bind(idx++, vn["aliases"].get_ptr<json::string_view_t>());
  stmt.bind(idx++, vn["length"].get_ptr<int>());
  stmt.bind(idx++, vn["description"].get_ptr<json::string_view_t>());
  stmt.bind(idx++, vn["links"]);
  stmt.bind(idx++, vn["image"].get_ptr<json::string_view_t>());
  stmt.bind(idx++, vn["image_nsfw"].get<json::boolean_t>());
  stmt.bind(idx++, vn["anime"]);
  stmt.bind(idx++, vn["relations"]);
  stmt.bind(idx++, vn["tags"]);
  stmt.bind(idx++, vn["popularity"].get<int>());
  stmt.bind(idx++, vn["rating"].get<int>());
  stmt.bind(idx++, vn["votecount"].get<int>());
  stmt.bind(idx++, vn["screens"]);
  stmt.bind(idx++, vn["staff"]);
  //PLACEHOLDER: this should bind the current date/time.
  stmt.bind(idx++, time());
}
int sqlite_insert_vn_tags(json vn, 
                          sqlite3_wrapper &db){
  return sqlite_insert_vn_tags(vn["id"].get<int>(), vn["tags"], db);
}
int sqlite_insert_vn_tags(int vn_id, json vn_tags, 
                          sqlite3_wrapper &db){
  //We make the vn_id part of the command, and use a parameter for the tags,
  //this is a compromise between the two extremes.
  static constexpr char *fmt_template =
    "(insert or replace into vn_tags values (%d, @tag))";
  static constexpr int fmt_template_size = constexpr_strlen(fmt_template);
  static constexpr int bufsz = fmt_template_size + 32;
  if(vn_tags.empty()){
    return SQLITE_OK;
  }
  char buf[bufsz];
  int nbytes = snprintf(buf, bufsz, fmt_template, vn_id);
  //according to the documentation we should include the null terminator in
  //the size, if we know it's there, its weird.
  sqlite3_stmt_wrapper stmt = dp.prepare_stmt(std::string_view(buf, nbytes+1));
  if(!stmt){
    return db.err();
  }
  //vn_tags has the form [[tag_id, tag_score (0-3), spoiler_level(0-2)]*].
  //we only care about the id here.
  int err = db.begin_transaction();
  if(err != SQLITE_OK){ return err; }
  for(auto&& tag : vn_tags){
    int tag_id = tag[0];
    stmt.bind(0, tag_id);
    if((err = stmt.exec()) != SQLITE_OK){
      db.rollback_transaction();
      return err;
    }
  }
  return db.commit_transaction();
}
//It's probably more useful to get a VN as json rather than a C++ object
json sqlite_get_VN(sqlite_stmt_wrapper &stmt){
  json ret;
  int idx = 0;
  //We don't need to deal with NULL for most values since the api returns
  //empty arrays / objects instead of NULL, so only scalar values can be NULL.
  ret.emplace("id", stmt.get_column<int>(idx++));
  ret.emplace("title", stmt.get_column<std::string_view>(idx++));
  ret.emplace("original", stmt.get_column<std::string_view>(idx++));
  ret.emplace("date", stmt.get_column<std::string_view>(idx++));
  ret.emplace("languages", stmt.get_column<json>(idx++));
  ret.emplace("orig_lang", stmt.get_column<json>(idx++));
  ret.emplace("platforms", stmt.get_column<json>(idx++));
  ret.emplace("aliases", stmt.get_column<json>(idx++));
  ret.emplace("length", stmt.get_column<int>(idx++));
  ret.emplace("description", stmt.get_column<std::string_view>(idx++));
  ret.emplace("links", stmt.get_column<json>(idx++));
  ret.emplace("image_link", stmt.get_column<std::string_view>(idx++));
  ret.emplace("image_nsfw", stmt.get_column<int>(idx++));
  ret.emplace("anime", stmt.get_column<json>(idx++));
  ret.emplace("relations", stmt.get_column<json>(idx++));
  ret.emplace("tags", stmt.get_column<json>(idx++));
  ret.emplace("popularity", stmt.get_column<int>(idx++));
  ret.emplace("rating", stmt.get_column<int>(idx++));
  ret.emplace("votecount", stmt.get_column<int>(idx++));
  ret.emplace("screens", stmt.get_column<json>(idx++));
  ret.emplace("staff", stmt.get_column<json>(idx++));
  ret.emplace("relases", stmt.get_column<json>(idx++));
  ret.emplace("producers", stmt.get_column<json>(idx++));
  ret.emplace("characters", stmt.get_column<json>(idx++));
  //ret.emplace("list_info", stmt.get_column<json>(idx++));
  return ret;
}
static constexpr std::string_view sql_insert_producer =
  R"EOF(insert or relpace into producers values (
        @id, @name, @original, @type, @language,
        @links, @aliases, @description, @relations)EOF"sv;
int sqlite_insert_producer(json producer, sqlite3_stmt_wrapper& stmt){
  int idx = 0;
  stmt.bind(idx++, producer["id"].get<int>());
  stmt.bind(idx++, producer["name"].get<json::string_t>());
  stmt.bind(idx++, producer["original"].get<json::string_t>());
  stmt.bind(idx++, producer["original"].get<json::string_t>());


//should probably move to seperate file
json parse_json_path_acc(json val, const char *path);
/*
  Find the location refered to by the given path, path has the form
  $(.{objectname}|[{index}])+

*/
json parse_json_path(json val, const char *path){
  if(*path != '$'){
    //invalid path;
    return json_null;
  } else {
    return parse_json_path_acc(val, path + 1);
  }
}

json parse_json_path_acc(json val, const char *path){
  json next;
  if(*path == '['){
    int idx = strtol(path + 1, &path, 0);
    if(*path != ']'){
      //Issue error message.
      return json_null;
    }
    path++;
    //check that idx is valid.
    next = val[idx];
  } else if(*path == '.'){
    path++;
    const char* start;
    while(*path && *path != '[' && *path != '.'){
      ++path;
    }
    std::string_view name(start, path - start);
    //check that name is in the json object.
    next = val[name];
  } else {
    //invalid path
    return json_null;
  }
  if(*path == '\0'){
    return next;
  } else {
    return parse_json_path_acc(next, path);
  }
}
