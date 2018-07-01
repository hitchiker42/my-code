/*
  Simple interactive loop, read until we encounter a semicolon, it's not
  the most elegent solution but it allows multi-line commands without
  having to actually parse the input.

  Input takes the form of: <command> <arguments>*
  where the commands are:
  help; | print help message
  sql stmt; | execute sql statement, bind result to some global variable
  set variable value; | set the value of variable to the result value
  print value; | print the result of evaluating value to stdout
  write filename value; | print value to the file 'filename'
  select ...; | shorthand for sql select ...

  TODO: need commands to modify vnlist/wishlist, which will
  require an active vndb connection.

  variables are fairly simple, the only operations you can perform
  on a variable are to print it or access a part of it.

  The main types are the different structures in the vndb namespace.
  You can also have arrays of these structures. Individual fields
  of structs can be accessed as well as elements of arrays.

  Variables are simply a means of saving the results of sql queries
  and accessing them using a more simple syntax, everything can be
  done using sql if you want.

c = character
p = producer
r = release
s = staff
u = user
v = vn
*/

static constexpr std::string_view help_string =
R"EOF(help; | print this help message
sql stmt; | execute sql statement, bind result to the variable 'last'
select select-stmt; | shorthand for sql select select-stmt
set variable value; | set the value of 'variable' to the result value
print value; | print the result of evaluating value to stdout
write value filename; | print value to the file 'filename'
view image; | view the image using SDL
[open-]url | open the url in the default web browser
[open-]vndb ([cprsuv]id | json object)
)EOF"sv;
#ifdef __unix__
[[noreturn]] void open_url_exec(const char *url){
  char buf[256];
  char *argv[3];
  char *bufptr = buf;
  argv[0] = bufptr;
  bufptr = stpcpy(bufptr, "xdg-open");
  argv[1] = ++bufptr;
  if(!is_prefix("http", url)){
    bufptr = mempcpy(buf, "https://", constexpr_strlen("https://"));
  }
  strncpy(bufptr, url, 256 - (bufptr-buf));
  argv[2] = nullptr;
  if(execvp("xdg-open", argv) < 0){
    exit(1);
  }
}
void open_url(const char *url){
  if(fork() == 0){
    open_url_exec(url);
  } else {
    return;
  }
}
#else
void open_url(const char *url){
  char buf[128];
  char *bufptr = buf;
  if(!is_prefix("http", url)){
    bufptr = mempcpy(buf, "https://", constexpr_strlen("https://"));
  }
  strncpy(bufptr, url, 128);
  ShellExecute(NULL, "open", buf, NULL, NULL, SW_SHOWNORMAL);
}
#endif


json* eval_expr(vdb_main *vndb, std::string_view expr);

//Functions to proved generic access to readline type libraries, so that I'm not
//tied to one particular library.
void vndb_init_readline();
char *vndb_readline(const char *prompt);
void vndb_add_to_history(const char *str, bool copy = false);
int set_var_to_sql(vndb_main *vndb, std::string_view var,
                   std::string_view sql, bool as_object = true){
  auto stmt = vndb->db.prepare_stmt(sql);
  if(!stmt){
    fprintf(stderr, "Failed to compile sql.\n");
    return vndb->db.errcode();
  }
  int err;
  vndb->symbol_table[var] = stmt.exec_json(as_object, &err);
  if(err != SQLITE_OK){
    fprintf(stderr, "Failed to execute sql.\n");
    return err;
  }
  return SQLITE_OK;
}
//path is: \[number|name\]+
// where number = [0-9]+ and name = [a-zA-z][[:alnum:]]+
//I may extend this defination later, but this is the eaisest to parse.
json* follow_json_path(const json *val, const char *path){
  const json *current = val;
  const char *ptr = path;
  char c;
  //TODO: Need to check that index is in range / subobject exists and
  //print an error if not. This shouldn't crash the program if given a
  //path to a nonexistant object.
  while((c = *ptr) != '\0'){
    if(c != '['){
      printf("Unexpected character %c in json path, expected '[' or '\\0'.\n", c);
      return nullptr;
    }
    if(c >= '0' && c <= '9'){
      int idx = strtol(ptr, &ptr, 10);
      if(current->type() != json::value_t::array_t){
        printf("Error expected json array but got a(n) %s.\n", current->type_name());
        return nullptr;
      }
      if(idx > current->size()){
        printf("Index %d out of range for array of size %lu.\n",
               idx, current->size());
        return nullptr;
      }
      if(*ptr != ']'){
        printf("Missing closing ']' in json path.\n");
        return nullptr;
      }

      current = &current->operator[](idx);
    } else {
      const char *next_ptr = strchr(ptr, ']');
      if(!next_ptr){
        printf("Missing closing ']' in json path.\n");
        return nullptr;
      }
      std::string_view name(ptr, next_ptr - ptr);
      current = current->find_or_null(name);
      if(!current){
        printf("Could not find '%.*s' in json object.\n", name.size(), name.data());
        return nullptr;
      }
    }
    ptr++;//skip ']' character.
  }
  return current;
}
//expr is a string with a ';' as the final character. Currenly
//the only supported expressions are numbers and variables.
bool eval_expr(vdb_main *vndb, std::string_view expr, json *val_ptr){
  char *ptr = expr.data();
  assert(expr.back() == ';');
  while(*ptr == ' '){ ++ptr; }  //skip space
  if(isdigit(*ptr)){ //Just a number.
    long val = strtol(ptr, &ptr, 10);
    while(*ptr == ' '){ ++ptr; }  //skip space
    if(*ptr != ';'){
      printf("Excess characters at end of expression : %.*s.\n",
             expr.size() - (ptr - expr.data()), ptr);
      return false;
    } else {
      *val_ptr = val;
      return true;
    }
  } else if(isalpha(*ptr)){
    char *var_start = ptr;
    do { ++ptr; } while(isalnum(*ptr));
    std::string_view var(var_start, ptr - var_start);
    json *val = hash_find_or_null(vndb->symbol_table, var);
    if(!val){
      printf("Undefined variables %.*s.\n", var.size(), var.data());
      return false;
    }
    if(*ptr == '['){
      val = follow_json_path();
      if(!val){ return false; }
    }
    *val_ptr = *val;
    return true;
  } else {
    printf("Unexpected character '%c' in expression '%.*s'.\n",
           *ptr, expr.size(), expr.data());
    return false;
  }
}
int do_sql_command(vndb_main *vndb, const char *sql){
  //Variables in sql must begin with a '$' ala perl and bash, this
  //makes it much eaiser to find them and avoids name conflicts.
  //TODO: Decide if variables have to follow the same restrictions
  //      as parameters for prepared statements or if they can
  //      be part of the actual command.
  char *var_start = nullptr;
  if((var_start = strchr(sql, '$')) != nullptr){
    //expand variables
  }
  int err = set_var_to_sql(vndb, "last", sql);
  if(err != SQLITE_OK){
    return err;
  }
  //print rows, We may print results differently depending on if there was
  //one column in the result or more than one.
  return SQLITE_OK;
}
enum class command_type {
  sql,
  select,
  set,
  print,
  write,
  view,
  help
};
static constexpr util::array command_names("sql"sv, "select"sv, "set"sv, "print"sv,
                                           "write"sv, "help"sv, "view"sv);
int do_command(vndb_main *vndb, std::string_view command){
  char *cmd_end = strpbrk(command.data(), " ;");
  std::string_view cmd = command.substr(0, cmd_end - command.data());
  int cmd_val = is_unique_prefix(cmd, command_names.data(), command_names.size());
  if(cmd_val < 0){
    if(cmd_val == -1){
      printf("Unknown command '%.*s.\n", cmd.size(), cmd.data());
    } else if(cmd_val == -2){
      printf("Ambiguous command '%.*s.\n", cmd.size(), cmd.data());
    }
    return cmd_val;
  }
  //translate cmd to command_type
  command_type cmd_type;
  switch(cmd_type){
    case command_type::help:{
      printf("%s.\n", help_string.data());
      return 0;
    }
    case command_type::sql:
    case command_type::select:{
      char *sql = (cmd_type == command_type::sql ? cmd_end : command.data());
      return do_sql_command(vndb, sql);
    }
    //For print & set Make sure the value that eval_expr returns
    //is pointing to is still in scope.

    //Trying to commbine the write and print commands just results
    //in more code due to scoping issues inside of switch statements.
    case command_type::write:{
      char *filename_end = strchr(cmd_end, ' ');
      if(!filename_end){
        printf("Malformed write command.\n");
        return -1;
      }
      std::string_view filename(cmd_end+1, filename_end - cmd_end);
      FILE_wrapper out(std::string_view());
      if(!out){
        printf("Could not open file %.*s.\n", filename.size(), filename.data());
        return -1;
      }
      json val;
      std::string_view expr = command.substr(cmd.size() + filename.size() + 1);
      if(!eval_expr(vndb, expr, &val)){
        return -1;
      } else {
        val.pprint(out);
        return 0;
      }
    }
    case command_type::print:{
      json val;
      if(!eval_expr(vndb,command.substr(cmd.size()), &val)){
        return -1;
      } else {
        val->pprint(stdout);
        return 0;
      }
    }
    case command_type::set:{
      char *name_end = strchr(cmd_end, ' ');
      if(!name_end){
        printf("Malformed set command.\n");
        return -1;
      }
      json val;
      std::string_view expr = command.substr(name_end - command.data());
      if(!eval_expr(vndb, expr, &val)){
        return -1;
      }
      //might be off by one for the size.
      std::string_view name = command.substr(cmd.size()+1, name_end - cmd_end);
      vndb->symbol_table[name] = val;
      return 0;
    }
    case command_type::view:{
      if(!sdl_running){
        printf("SDL is not running so images can not be displayed.\n");
        return -1;
      }
      //This should be sql_select_vn_image_by_id, compiled.
      sqlite3_stmt_wrapper stmt;
      while(*cmd_end == ' '){
        ++cmd_end;
      }
      char *tmp;
      int id = strtol(cmd_end, &tmp, 10);
      if(cmd_end == tmp){
        printf("Missing id in view command.\n");
        return -1;
      }
      int res = stmt.step();
      if(res == SQLITE_DONE){
        printf("Could not find an image for vn %d.\n", id);
        //Not sure what to return here, it's not really an error but
        //it's not successful either.
        return 0;
      } else if(res == SQLITE_ROW){
        void *data = stmt.get_column<void*>(0);
        size_t data_size = stmt.get_column_bytes(0);
        SDL_Event evt;
        init_jpeg_user_event(&evt, data, data_size);
        SDL_PushEvent(&evt);
        //The pointer to data is only valid until we call stmt.step/reset
        //so we need to wait for the other thread to be done with it.
        SDL_SemWait(vndb->sdl_sem);
        return 0
      } else {
        printf("Error executing sql.\n");
        return -1;
      }
    }
  }
}

[[noreturn]] void run_interactively(vndb_main &vndb){
  util::string_buf buf;
  char *lineptr = nullptr;
  char *endptr = nullptr;
  int err = 0;
  vndb_init_readline();
  if(!vndb.init_sdl()){
    printf("Could not initialize SDL, will not be able to display images.\n");
  }
  while(1){
    char *prompt = "vndb >";
    buf.clear();
    lineptr = vndb_readline(prompt);
    if(!lineptr){ goto end; }
    if(!(endptr = strchr(lineptr, ';'))){
      prompt = " ... >";
      do {
        buf.append(lineptr).append(' ');//translate newlines to spaces
        free(lineptr);
        lineptr = vndb_readline(prompt);
        if(!lineptr){ goto end; }
      } while(!(endptr = strchr(lineptr, ";")));
    }
    //append everything upto and including the semicolon.
    buf.append(lineptr, (endptr - lineptr) + 1);
    //make sure there's no actual text after the semicolon
    while(*(++endptr) != '\0'){
      if(!isspace(*endptr)){
        printf("Unexpected text following ';' : \"%s\".\n", endptr);
        goto next;
      }
    }
    //grab the memory allocated by the buffer so we can store in the history.
    util::string_view line = buf.move_to_string_view();
    line.release_memory(); //we're transfering ownership to the history manager.
    vndb_add_history(line.data());
    //There isn't really anything to do with the return value of do_command...
    err = do_command(vndb, line);
  next:
    free(lineptr);
  }
 end:
  free(lineptr);
  exit(abs(err));
}
/*
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
*/
