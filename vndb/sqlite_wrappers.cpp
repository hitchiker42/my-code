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
    return parse_delimted_string(str->c_str(), delim);
  }
}
//add a vn given as json into the database
int sqlite_insert_vn(const json &vn, sqlite3_stmt_wrapper& stmt){
  int idx = 1;
  //get_ptr is used for values that might be null, get/get_ref are
  //used for values that can't be null.
  stmt.bind(idx++, vn["id"].get<int>());
  stmt.bind(idx++, vn["title"].get_ref<const json::string_t>());
  stmt.bind(idx++, vn["original"].get_ptr<const json::string_t>());
  stmt.bind(idx++, vn["released"].get_ptr<const json::string_t>());
  stmt.bind(idx++, vn["languages"]);
  stmt.bind(idx++, vn["orig_lang"]);
  stmt.bind(idx++, vn["platforms"]);
  stmt.bind(idx++,
            parse_delimted_string(vn["aliases"].get_ptr<const json::string_t>(), '\n'));
  stmt.bind(idx++, vn["length"].get_ptr<int64_t>());
  stmt.bind(idx++, vn["description"].get_ptr<const json::string_t>());
  stmt.bind(idx++, vn["links"]);
  stmt.bind(idx++, vn["image"].get_ptr<const json::string_t>());
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
  int ret = stmt.exec();
  if(ret != SQLITE_OK){
    DEBUG_PRINTF("Error executing SQL %s.\n", stmt.get_sql().c_str());
  }
  stmt.reset_bindings();
  return ret;
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
  int ret = stmt.exec();
  if(ret != SQLITE_OK){
    DEBUG_PRINTF("Error executing SQL %s.\n", stmt.get_sql().c_str());
  }
  stmt.reset_bindings();
  return ret;
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
            parse_delimted_string(producer["aliases"].get_ptr<json::string_t>(), '\n'));
  stmt.bind(idx++, producer["description"].get_ptr<json::string_t>());
  //This gives the relationship between this producer and other producers not vns.
  stmt.bind(idx++, producer["relations"].get_ptr<json::string_t>());
  int ret = stmt.exec();
  if(ret != SQLITE_OK){
    DEBUG_PRINTF("Error executing SQL %s.\n", stmt.get_sql().c_str());
  }
  stmt.reset_bindings();
  return ret;
}
int sqlite_insert_character(const json& chara,
                            sqlite3_stmt_wrapper& stmt){
  int idx = 1;
  stmt.bind(idx++, chara["id"].get<int>());
  stmt.bind(idx++, chara["name"].get<std::string_view>());
  stmt.bind(idx++, chara["original"].get_ptr<json::string_t>());
  stmt.bind(idx++, chara["gender"].get_ptr<json::string_t>());
  stmt.bind(idx++,
            parse_delimted_string(chara["aliases"].get_ptr<json::string_t>(), '\n'));
  stmt.bind(idx++, chara["description"].get_ptr<json::string_t>());
  stmt.bind(idx++, chara["image"].get_ptr<json::string_t>());
  stmt.bind(idx++, chara["traits"]);
  stmt.bind(idx++, chara["vns"]);
  stmt.bind(idx++, chara["voiced"]);
  int ret = stmt.exec();
  if(ret != SQLITE_OK){
    DEBUG_PRINTF("Error executing SQL %s.\n", stmt.get_sql().c_str());
  }
  stmt.reset_bindings();
  return ret;
}
int sqlite_insert_staff(const json& staff,
                        sqlite3_stmt_wrapper& stmt){
  int idx = 1;
  stmt.bind(idx++, staff["id"].get<int>());
  stmt.bind(idx++, staff["name"].get<std::string_view>());
  stmt.bind(idx++, staff["original"].get_ptr<json::string_t>());
  stmt.bind(idx++, staff["gender"].get_ptr<json::string_t>());
  stmt.bind(idx++, staff["language"].get<std::string_view>());
  stmt.bind(idx++, staff["links"]);
  stmt.bind(idx++, staff["description"].get_ptr<json::string_t>());
  stmt.bind(idx++, staff["aliases"]);
  stmt.bind(idx++, staff["main_alias"].get<int>());
  stmt.bind(idx++, staff["vns"]);
  stmt.bind(idx++, staff["voiced"]);
  int ret = stmt.exec();
  if(ret != SQLITE_OK){
    DEBUG_PRINTF("Error executing SQL %s.\n", stmt.get_sql().c_str());
  }
  stmt.reset_bindings();
  return ret;
}

// Add relations between each VN and producer from release.
// Since the relations table use foreign keys it is an error
// to try and add a relation for a non-existant VN/producer.
int sqlite_insert_vn_producer_relations(const json& release,
                                        sqlite3_stmt_wrapper& stmt){
  return sqlite_insert_vn_producer_relations(release["id"].get<int>(),
                                             release["vn"], release["producers"],
                                             stmt);
}
int sqlite_insert_vn_producer_relations(int release_id,
                                        const json& vns,
                                        const json& producers,
                                        sqlite3_stmt_wrapper& stmt){
  //I'm pretty sure vns can't be empty, but it doesn't really matter.
  if(vns.empty() || producers.empty()){
    return SQLITE_OK;
  }
  //order of columns is vn, producer, release.
  stmt.bind(3, release_id);
  for(auto &&vn : vns){
    stmt.bind(1, vn["id"].get<int>());
    for(auto &&producer : producers){
      stmt.bind(2, producer["id"].get<int>());
      int err = stmt.exec();
      if(err != SQLITE_OK){
        return err;
      }
    }
  }
  stmt.reset_bindings();
  return SQLITE_OK;
}
int sqlite_insert_vn_character_actor_relations(const json& actor,
                                               sqlite3_stmt_wrapper &stmt){
 return sqlite_insert_vn_character_actor_relations(actor["id"].get<int>(),
                                                   actor["voiced"], stmt);
}
int sqlite_insert_vn_character_actor_relations(int actor_id,
                                               const json& voiced,
                                               sqlite3_stmt_wrapper &stmt){
  if(voiced.empty()){
    return SQLITE_OK;
  }
  stmt.bind(3, actor_id);
  //voiced is array of {vn_id(id), alias_id(aid), character_id(cid), note(note)}
  for(auto &&voice : voiced){//now thats a weird variable name
    stmt.bind(1, voice["id"].get<int>());
    stmt.bind(2, voice["cid"].get<int>());
    int err = stmt.exec();
    if(err != SQLITE_OK){
      return err;
    }
  }
  stmt.reset_bindings();
  return SQLITE_OK;
}

int sqlite_insert_vn_staff_relations(const json& staff,
                                     sqlite3_stmt_wrapper &stmt){
 return sqlite_insert_vn_staff_relations(staff["id"].get<int>(),
                                         staff["vns"], stmt);
}
int sqlite_insert_vn_staff_relations(int staff_id,
                                     const json& vn_info,
                                     sqlite3_stmt_wrapper &stmt){
  if(vn_info.empty()){
    return SQLITE_OK;
  }
  stmt.bind(2, staff_id);
  //vn_info is an array of {vn_id(id), alias_id(aid),
  //                        role(role), note(note)}
  for(auto &&vn : vn_info){
    stmt.bind(1, vn["id"].get<int>());
    int err = stmt.exec();
    if(err != SQLITE_OK){
      return err;
    }
  }
  stmt.reset_bindings();
  return SQLITE_OK;
}
int sqlite_insert_staff_aliases(const json& staff,
                                sqlite3_stmt_wrapper &stmt){
  return sqlite_insert_staff_aliases(staff["id"].get<int>(),
                                     staff["aliases"], stmt);
}
int sqlite_insert_staff_aliases(int staff_id,
                                const json& aliases,
                                sqlite3_stmt_wrapper &stmt){
  //aliases should never be empty since everyone has at least one
  //otherwise you would'nt exist.
  stmt.bind(1, staff_id);
  //aliases is an array of [alias id, name(romanji), name(original)]
  for(auto &&alias : aliases){
    stmt.bind(2, alias[0].get<int>());
    stmt.bind(3, alias[1].get<std::string_view>());
    int err = stmt.exec();
    if(err != SQLITE_OK){
      return err;
    }
  }
  stmt.reset_bindings();
  return SQLITE_OK;
}
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

//We need to determine if the column is meant to be json or just a string.
//sqlite doesn't have a json type, but the sqlite3_column_decltype function
//Lets us get the type the column was declared to have as a string, so
//we can use that to see if a column is supposed to be json. This is just
//an optimization, if we can't get the declared type we just try to parse
//the string as json and insert it as a string if we fail.
static json get_text_column_as_json(sqlite3_stmt_wrapper *stmt, int idx){
  std::string_view col_text = stmt->get_column<std::string_view>(idx);
  const char *col_type_name = sqlite3_column_decltype(stmt->stmt, idx);
  if(col_type_name){
    if(sqlite3_stricmp(col_type_name, "json") == 0){
      return json::parse(col_text);
    } else {
      return json(col_text);
    }
  } else {
    //Only arrays and objects are stored as json, so if it
    //can't be one of those it must just be a string.
    if(col_text[0] != '[' && col_text[0] != '{'){
      json(text);
    }
    json_parser p(col_text);
    json j;
    bool is_json = p.try_parse(j);
    if(is_json){
      return j;
    } else {
      return json(col_text);
    }
  }
}
static json get_blob_column_as_json(sqlite3_stmt_wrapper *stmt, int idx){
  //For blobs (currently the only blobs I use are for image storage) we just
  //store a string indicating this is a blob and its size. An alternative
  //would be base64 encoding or extending the json type to support binary data.
  char buf[128];
  snprintf(buf, 128, "#<%d byte blob>", sqlite3_column_bytes(stmt->stmt, idx));
  return json(std::string_view(buf));
}
//This is different from get_column<json>, get_column<json> assumes the
//column holds valid json and parses it, This converts the column from
//whatever type it is into json.
static json get_column_json(sqlite3_stmt_wrapper *stmt, int idx){
  switch(stmt->column_type(idx)){
    case sqlite3_type::integer:
      return json(stmt->get_column<int>(idx));
    case sqlite3_type::floating:
      return json(stmt->get_column<double>(idx));
    case sqlite3_type::null:
      return json_null;
    case sqlite3_type::blob:
      return get_blob_column_as_json(stmt, idx);
    case sqlite3_type::text:
      return get_text_column_as_json(stmt, idx);
  }
  unreachable();
}
json sqlite3_stmt_wrapper::get_row_json_obj(){
  int ncols = this->get_ncolumns();
  json::object_t obj;
  for(int i = 0; i < ncols; i++){
    auto name = this->column_name(i);
    obj.emplace(name, get_column_json(this, i));
  }
  return json(std::move(obj));
}
json sqlite3_stmt_wrapper::get_row_json_arr(){
  int ncols = this->get_ncolumns();
  json::array_t arr;
  arr.reserve(ncols);
  for(int i = 0; i < ncols; i++){
    arr.emplace_back(get_column_json(this, i));
  }
  return json(std::move(arr));
}
json sqlite3_stmt_wrapper::exec_json(bool as_objects, int *err_ptr){
  std::vector<json> ret;
  int err;
  if(!err_ptr){ err_ptr = &err; }
  //I'm not 100% sure if it's safe to call this here, but as far
  //as I can tell it should be.
  int ncols = this->get_ncolumns();
  vndb_log->log_debug("In exec_json: ncols = %d before starting stmt.\n", ncols);
  bool logged = false;
  while((err = sqlite3_step(this->stmt)) == SQLITE_ROW){
    if(!logged){
      int ncols = this->get_ncolumns();
      vndb_log->log_debug("In exec_json: ncols = %d after starting stmt.\n", ncols);
      logged = true;
    }
    if(ncols == 1){ //flatten array if there's only one column
      ret.emplace_back(get_column_json(this, 0));
    } else {
      json row = get_row_json(as_objects);
      //This should never happen.
      if(row.is_null()){
        return SQLITE_ABORT;
      }
      ret.emplace_back(row);
    }
  }
  *err_ptr = reset();
  if(*err_ptr != SQLITE_OK){
    return json_null;
  } else {
    return ret;
  }
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
