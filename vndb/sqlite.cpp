#include "vndb.h"
#include "sql.h"

template<typename T>
using vector = util::svector<T>;//can change to std::vector
using string = util::string;//can change to std::string

//Convert a list stored in a string using delim as a seperator into a
//json array. eg "a,b,c",',' -> ["a","b","c"];
static json parse_delimted_string(const char *str, char delim){
  json::array_t arr;
  while(*str){
    const char *start = str;
    while(*str && *str != delim){
      ++str;
    }
    arr.emplace_back(json::string_t(start, str - start));
    if(*str == '\0'){ break; }
    ++str;//skip past newline
  }
  return json(std::move(arr));
}
static json parse_delimted_string(const std::string *str, char delim){
  if(!str){
    return json(json::value_t::array);
  } else {
    return parse_delimted_string(str.c_str(), delim);
  }
}
//add a vn given as json into the database
int sqlite_insert_vn(const json &vn, sqlite3_stmt_wrapper& stmt){
  int idx = 1;
  //get_ptr is used for values that might be null, get/get_ref are
  //used for values that can't be null.
  stmt.bind(idx++, vn["id"].get<int>());
  stmt.bind(idx++, vn["title"].get_ref<json::string_t>());
  stmt.bind(idx++, vn["original"].get_ptr<json::string_t>());
  stmt.bind(idx++, vn["date"].get_ptr<json::string_t>());
  stmt.bind(idx++, vn["languages"]);
  stmt.bind(idx++, vn["orig_lang"]);
  stmt.bind(idx++, vn["platforms"]);
  stmt.bind(idx++,
            parse_delimted_string(vn["aliases"].get_ptr<json::string_t>()), '\n');
  stmt.bind(idx++, vn["length"].get_ptr<int64_t>());
  stmt.bind(idx++, vn["description"].get_ptr<json::string_t>());
  stmt.bind(idx++, vn["links"]);
  stmt.bind(idx++, vn["image"].get_ptr<json::string_t>());
  stmt.bind(idx++, vn["image_nsfw"].get<json::boolean_t>());
  stmt.bind(idx++, vn["anime"]);
  stmt.bind(idx++, vn["relations"]);
  stmt.bind(idx++, vn["tags"]);
  stmt.bind(idx++, vn["popularity"].get<double>());
  stmt.bind(idx++, vn["rating"].get<double>());
  stmt.bind(idx++, vn["votecount"].get<int>());
  stmt.bind(idx++, vn["screens"]);
  stmt.bind(idx++, vn["staff"]);
  //bind the current date/time, I may remove this.
  stmt.bind(idx++, time(NULL));
//  DEBUG_PRINTF("Executing sql: %s\n", sqlite3_expanded_sql(stmt.unwrap()));
  return stmt.exec();
}
//insert a release into the database.
int sqlite_insert_release(const json &release,
                          sqlite3_stmt_wrapper& stmt){
  int idx = 1;
  stmt.bind(idx++, release["id"].get<int>());
  stmt.bind(idx++, release["title"].get_ref<json::string_t>());
  stmt.bind(idx++, release["original"].get_ptr<json::string_t>());
  stmt.bind(idx++, release["released"].get_ptr<json::string_t>());
  stmt.bind(idx++, release["type"].get<std::string_view>());
  stmt.bind(idx++, release["patch"].get<bool>());
  stmt.bind(idx++, release["languages"]);
  stmt.bind(idx++, release["website"].get_ptr<json::string_t>());
  stmt.bind(idx++, release["notes"].get_ptr<json::string_t>());
  stmt.bind(idx++, release["minage"].get_ptr<int64_t>());
  stmt.bind(idx++, release["platforms"]);
  stmt.bind(idx++, release["resolution"].get_ptr<json::string_t>());
  stmt.bind(idx++, release["voiced"].get_ptr<int64_t>());
  stmt.bind(idx++, release["animation"]);
  stmt.bind(idx++, release["vn"]);
  stmt.bind(idx++, release["producers"]);
  return stmt.exec();
}
// Add relations between each VN and producer from release.
// Since the relations table use foreign keys it is an error
// to try and add a relation for a non-existant VN/producer.
int sqlite_insert_relations(const json& release,
                            sqlite3_stmt_wrapper& stmt){
  json& vns = release["vn"];
  json& producers = release["producers"];
  //I'm pretty sure vns can't be empty, but it doesn't really matter.
  if(vns.empty() || producers.empty()){
    return SQLITE_OK;
  }
  //order of columns is vn, producer, release.
  stmt.bind(3, release["id"].get<int>());
  for(auto &&vn : vns){
    stmt.bind(1, vn["id"].get<int>());
    for(auto &&producer : producers){
      stmt.bind(2, producer["id"].get<int>());
      err = stmt.exec(false);//Don't reset the bindings
      if(err != SQLITE_OK){
        return err;
      }
    }
  }
  return SQLITE_OK;
}
int sqlite_insert_producer(const json& producer,
                           sqlite3_stmt_wrapper& stmt){
  int idx = 1;
  stmt.bind(idx++, producer["id"].get<int>());
  stmt.bind(idx++, producer["name"].get<std::string_view>());
  stmt.bind(idx++, producer["original"].get_ptr<json::string_t>());
  stmt.bind(idx++, producer["type"].get_ptr<json::string_t>());
  stmt.bind(idx++, producer["language"].get_ptr<json::string_t>());
  stmt.bind(idx++, producer["links"]);
  stmt.bind(idx++,
            parse_delimted_string(producer["aliases"].get_ptr<json::string_t>(), '\n'))
  stmt.bind(idx++, producer["description"].get_ptr<json::string_t>());
  //This gives the relationship between this producer and other producers not vns.
  stmt.bind(idx++, producer["relations"].get_ptr<json::string_t>());
  return stmt.exec();
}
int sqlite_insert_staff(const json& staff,
                        sqlite3_stmt_wrapper& stmt){
  stmt.bind(idx++, staff["id"].get<int>());
  stmt.bind(idx++, staff["name"].get<std::string_view>());
  stmt.bind(idx++, staff["original"].get_ptr<json::string_t>());
  stmt.bind(idx++, staff["language"].get_ptr<json::string_t>());
  stmt.bind(idx++, staff["gender"].get_ptr<json::string_t>());
  stmt.bind(idx++,
            parse_delimted_string(staff["aliases"].get_ptr<json::string_t>(), '\n'))
  stmt.bind(idx++, staff["description"].get_ptr<json::string_t>());
  stmt.bind(idx++, staff["image"].get_ptr<json::string_t>());
  stmt.bind(idx++, staff["traits"]);
  stmt.bind(idx++, staff["vns"]);
  stmt.bind(idx++, staff["voiced"]);
  return stmt.exec();
}
int sqlite_insert_character(const json& chara,
                           sqlite3_stmt_wrapper& stmt){
  stmt.bind(idx++, chara["id"].get<int>());
  stmt.bind(idx++, chara["name"].get<std::string_view>());
  stmt.bind(idx++, chara["original"].get_ptr<json::string_t>());
  stmt.bind(idx++, chara["gender"].get_ptr<json::string_t>());
  stmt.bind(idx++,
            parse_delimted_string(chara["aliases"].get_ptr<json::string_t>(), '\n'))
  stmt.bind(idx++, chara["description"].get_ptr<json::string_t>());
  stmt.bind(idx++, chara["image"].get_ptr<json::string_t>());
  stmt.bind(idx++, chara["traits"]);
  stmt.bind(idx++, chara["vns"]);
  stmt.bind(idx++, chara["voiced"]);
  return stmt.exec();
}
int sqlite_insert_vn_tags(int vn_id, json vn_tags,
                          sqlite3_wrapper &db);
int sqlite_insert_vn_tags(json vn,
                          sqlite3_wrapper &db){
  return sqlite_insert_vn_tags(vn["id"].get<int>(), vn["tags"], db);
}
int sqlite_insert_vn_tags(int vn_id, json vn_tags,
                          sqlite3_wrapper &db){
  if(vn_tags.empty()){
    return SQLITE_OK;
  }
  auto stmt = db.prepare_stmt(sql_insert_vn_tags);
  if(!stmt){
    return db.err();
  }
  stmt.bind(1, vn_id);
  int err = db.begin_transaction();
  if(err != SQLITE_OK){ return err; }
  for(auto &&tag : vn_tags){
    int tag_id = tag[0];
    stmt.bind(2, tag_id);
    if((err = stmt.exec(false)) != SQLITE_OK){
      db.rollback_transaction();
      return err;
    }
  }
  return db.commit_transaction();
}
//It's probably more useful to get a VN as json rather than a C++ object
json sqlite_get_VN(sqlite3_stmt_wrapper &stmt){
  json ret;
  int idx = 1;
  //We don't need to deal with NULL for most values since the api returns
  //empty arrays / objects instead of NULL, so only scalar values can be NULL.
  ret.emplace("id", stmt.get_column<int>(idx++));
  ret.emplace("title", stmt.get_column<const char*>(idx++));
  ret.emplace("original", stmt.get_column<const char*>(idx++));
  ret.emplace("date", stmt.get_column<const char*>(idx++));
  ret.emplace("languages", stmt.get_column<json>(idx++));
  ret.emplace("orig_lang", stmt.get_column<json>(idx++));
  ret.emplace("platforms", stmt.get_column<json>(idx++));
  ret.emplace("aliases", stmt.get_column<json>(idx++));
  ret.emplace("length", stmt.get_column<int>(idx++));
  ret.emplace("description", stmt.get_column<const char*>(idx++));
  ret.emplace("links", stmt.get_column<json>(idx++));
  ret.emplace("image_link", stmt.get_column<const char*>(idx++));
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
/*
int sqlite_insert_producer(json producer, sqlite3_stmt_wrapper& stmt){
  int idx = 0;
  stmt.bind(idx++, producer["id"].get<int>());
  stmt.bind(idx++, producer["name"].get<json::string_view_t>());
  stmt.bind(idx++, producer["original"].get<json::string_view_t>());
  stmt.bind(idx++, producer["original"].get<json::string_view_t>());
}
*/

std::vector<std::string_view>&
sqlite3_stmt_wrapper::get_row_text(std::vector<std::string_view>& row,
                                   const char *nullstr){
  int ncols = this->get_ncolumns();
  row.clear();
  if(ncols <= 0){
    return row;
  }
  row.reserve(ncols);
  std::string_view nullsv;
  if(nullstr){
    nullsv = std::string_view(nullstr, strlen(nullstr));
  }
  for(int i = 0; i < ncols; i++){
    const char *str = (const char*)sqlite3_column_text(stmt, i);
    if(str){
      row.emplace_back(str, sqlite3_column_bytes(stmt, i));
    } else {
      row.emplace_back(nullsv);
    }
  }
  return row;
}
//Given a string that may or may not represent a json array/object
//return a json value that is either the value obtained by parsing
//the string if it is valid json or just the string itself if it isn't.
static json parse_possible_json(std::string_view text){
  //Only arrays and objects are stored as json, so if it
  //can't be one of those it must just be a string.
  if(text[0] != '[' && text[0] != '{'){
    json(text);
  }
  json_parser p(text);
  json j;
  bool is_json = p.try_parse(j);
  if(is_json){
    return j;
  } else {
    return json(text);
  }
}
json sqlite3_stmt_wrapper::get_row_json(){
  int ncols = this->get_ncolumns();
  json::object_t obj;
  for(int i = 0; i < ncols; i++){
    auto name = this->column_name(i);
    switch(this->column_type(i)){
      case(sqlite3_type::integer):
        obj.emplace(name, json(this->get_column<int>(i)));
        break;
      case sqlite3_type::floating:
        obj.emplace(name, json(this->get_column<double>(i)));
        break;
      case sqlite3_type::null:
        obj.emplace(name, json_null);
        break;
      //I know this will never come up in this application,
      //if this were possible I'd probably encode the input in base64,
      case sqlite3_type::blob:{
        fprintf(stderr, "Error found unexpectd blob type in sql table %s\n",
                sqlite3_column_table_name(this->stmt, i));
        return json_null;        /*
        //Convert into
        json::array_t arr;
        size_t blob_size = this->get_column_bytes(i);
        uintptr_t* blob = (uintptr_t*)get_column<void*>(i);
        for(size_t j = 0; j < (blob_size/8); j++){
           arr.emplace_back(blob[j]);
        }
        j *= 8;
        uint8_t buf[sizeof(uinptr_t)] = {0};
        if(j < blob_size){
          uint8_t* tail = ((uint8_t*)blob) + j;
          memcpy(buf, tail, blob_size - j);
          arr.emplace_back(*((uintptr_t*)buf));
        }
        obj.emplace(name, arr);
        */
      }
      //We need to determine if the column is meant to be json or just a string.
      //sqlite doesn't have a json type, but the sqlite3_column_decltype function
      //Lets us get the type the column was declared to have as a string, so
      //we can use that to see if a column is supposed to be json. This is just
      //an optimization, if we can't get the declared type we just try to parse 
      //the string as json and insert it as a string if we fail.
      case sqlite3_type::text {
        std::string_view col_text = this->get_column<std::string_view>(i);
        const char *col_type_name = sqlite3_column_decltype(this->stmt, i);
        if(col_type_name){
          if(sqlite3_stricmp(col_type_name, "json") == 0){
            obj.emplace(name, json::parse(col_text));
          } else {
            obj.emplace(name, col_text);
          }
        } else {
          obj.emplace(name, parse_possible_json(col_text));
        }
        break;
      }
    }
  }
  return json(std::move(obj));
}

#if 0
//Fairly unoptimized version of the json path type used by the sqlite json
//extension. Just uses a json array to hold the path elements, which isn't
//super efficent, a tagged pointer would be better than json, since there are only
//two possible types.
struct json_path {
  std::vector<json> path;
  json_path() = default;
  json_path(const json_path &other) = default;
  json_path(json_path &&other) = default;

  //Appending elements to the path
  json_path& append(std::string_view obj_name){
    path.emplace_back(obj_name);
    return *this;
  }
  json_path& append(int64_t idx){
    path.emplace_back(idx);
    return *this;
  }
  json_path& append(const json_path &p){
    path.append(p.path);
    return *this;
  }
  json_path& operator/=(std::string_view obj_name){
    return append(obj_name);
  }
  json_path& operator/=(int64_t idx){
    return append(idx);
  }
  json_path& operator/=(const json_path &p){
    append(p);
  }
  json_path& operator/(int64_t idx){
    json_path p(*this);
    return p.append(idx);
  }
  json_path& operator/(std::string_view obj_name){
    json_path p(*this);
    return p.append(obj_name);
  }
  json_path& operator/(const json_path &p){
    json_path p2(*this);
    return p2.append(p);
  }
  //Convert a path into a string
  std::string to_string(){
    std::string ret("$");
    char buf[64] num_to_string_buf;
    for(auto&& elt : path){
      if(!append_path_element_to_string(elt, num_to_string_buf)){
        return "";
      }
    }
    return ret;
  }
  char* format_int(int val, char *buf){
    snprintf(buf, 64, "%d", val);
    return buf;
  }
  bool append_path_element_to_string(std::string &str, json elt,
                                     char *buf){
    if(elt.is_string()){
      str.push_back('.');
      str.append(elt.get<json::string_view_t>());
      return true;
    } else if (elt.is_number_integer()){
      int idx = elt.get<int>();
      assert(idx >= 0);
      str.push_back('[');
      str.append(format_int(idx));
      str.push_back(']');
      return true;
    } else {
      fprintf(stderr, "Error unexpected type in json_path : %s\n");
      return false;
    }
  }
  //parse a path given as a string.
  bool parse(const char *str){
    const char *start;//for use in printing error messages.
    if(*str++ != '$'){
      return false;
    }
    char c;
    while((c = *str) != '\0'){
      if(*str == '.'){
        char *obj_name = ++str;
        //I'm not going to verify that this is a valid object name, I'll
        //just deal with that when looking for the object.
        while((c = *str)){
          if(c == '.' || c == '['){ break; }
          ++str;
        }
        path.emplace_back(std::string_view(obj_name, str));
      } else if(*str == ']'){
        char *idx_str = ++str;
        long idx = strtol(idx_str, &str, 10);
        if(*str != ']' && *str != '\0' || idx < 0){
          fprintf(stderr,
                  "Error parsing number in json path %s at index %d\n",
                  start, str - start);
          return false;
        }
        path.emplace_back(idx);
      } else {
        fprintf(stderr,
                "Error malformed json path %s, error at index %d\n",
                start, str-start);
        return false;
      }
    }
    return true;
  }
  //Return the a pointer to the value the json object pointed
  //to by 'ptr' has at the location given by this path, or nullptr if
  //no such location exists.
  json* follow(json *ptr){
    for(auto &&elt : path){
      if(elt.is_int()){
        if(!ptr->is_array() || elt >= ptr->size()){
          return nullptr;
        } else {
          ptr = &((*ptr)[elt.get<int>()]);
        }
      } else {//elt.is_string()
        //json::find is defined to always return json::end when called on
        //a non object type.
        auto it = ptr->find(elt.get<json::string_view_t>());
        if(it == ptr->end()){
          return nullptr;
        }
        ptr = &(*it);
      }
    }
    return ptr;
  }
  //version of follow_path that takes its argument by value, the only
  //difference is that this will als return nullptr for an empty path.
  json* follow(json val){
    if(path.empty()){ return nullptr; }
    return follow(&val);
  }
};
#endif
