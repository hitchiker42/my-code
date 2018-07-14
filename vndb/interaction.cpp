#include "vndb.h"
/*
//TODO: add update command
  Simple interactive loop, read until we encounter a semicolon, it's not
  the most elegent solution but it allows multi-line commands without
  having to actually parse the input.

  Input takes the form of: <command> <arguments>*;
  where the commands are:
  help | print help message
  sql stmt | execute sql statement, bind result to some global variable
  set variable value | set the value of variable to the result value
  print value | print the result of evaluating value to stdout
  write filename value | print value to the file 'filename'
  select ... | shorthand for sql select ...

  There are also some auxiliary commands which take the form
  .command args* # note the lack of a semicolon
  these are:
  .help | print help message.
  .vars | print a list of defined variables.
  .log [n] | print the last n lines of the current log file, or
             the entire file if n is not given. (currently
             the whole file is always printed).

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
R"EOF(Input takes the form of: <command> <arguments>*
Commands are terminated with semicolons, not newlines.
help                 | print this help message
sql stmt             | execute sql statement, bind result to the variable 'last'
select select-stmt   | shorthand for sql select select-stmt
set name [=] expr    | set the value of 'variable' to the result of expr
length expr          | print the length of an array or number of values in an object
type expr            | print the type of an expression
print value          | print the result of evaluating value to stdout
write value filename | print value to the file 'filename'
view image           | view the image using SDL
[open-]url url       | open the url in the default web browser
[open-]vndb [cprsuv]id | open the vndb page for the object of the
                         given type, with the given id. (FIXME bad description).

There aro also several utility commands which begin with a '.'
and are newline rather than semicolon terminated.
.help | print help message.
.vars | print a list of defined variables.
.log [n] | print the last n lines of the current log file, or
           the entire file if n is not given. (currently
           the whole file is always printed).
)EOF"sv;
//characters which indicate the end of a varible
static constexpr const char *variable_delimiters = " ;()+-*/%&|!<>=";
const char* skip_space(const char *str){
  while(*str && *str == ' '){
    ++str;
  }
  return str;
}
std::string_view skip_space(std::string_view sv){
  size_t idx = 0;
  while(idx < sv.size() &&
        sv[idx] == ' '){
    ++idx;
  }
  return sv.substr(idx);
}

#ifdef __unix__
[[noreturn]] void open_url_exec(const char *url){
  char buf[256];
  char *argv[3];
  char *bufptr = buf;
  argv[0] = bufptr;
  bufptr = stpcpy(bufptr, "xdg-open");
  argv[1] = ++bufptr;
  if(!is_prefix_of("http", url)){
    bufptr = (char*)mempcpy(buf, "https://", constexpr_strlen("https://"));
  }
  strncpy(bufptr, url, 256 - (bufptr-buf));
  argv[2] = nullptr;
  execvp("xdg-open", argv);
  //exec only returns on error, so if we get here there was an error.
  exit(1);
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
//val_ptr needs to point to an actual json value.
bool eval_expr(vndb_main *vndb, std::string_view expr, json *val_ptr);
int set_val_to_sql(vndb_main *vndb, sqlite3_stmt_wrapper& stmt,
                   json* val_ptr, bool as_object = true){
  int err;
  json val = stmt.exec_json(as_object, &err);
  if(err != SQLITE_OK){
    fprintf(stderr, "Failed to execute sql.\n");
    return err;
  }
  *val_ptr = std::move(val);
  return SQLITE_OK;
}
int set_val_to_sql(vndb_main *vndb, std::string_view sql,
                    json* val_ptr, bool as_object = true){
  auto stmt = vndb->db.prepare_stmt(sql);
  if(!stmt){
    return vndb->db.errcode();
  }
  return set_val_to_sql(vndb, stmt, val_ptr, as_object);
}
int set_var_to_sql(vndb_main *vndb, std::string_view var,
                   sqlite3_stmt_wrapper& stmt, bool as_object = true){
  int err;
  json val = stmt.exec_json(as_object, &err);
  if(err != SQLITE_OK){
    fprintf(stderr, "Failed to execute sql.\n");
    return err;
  }
  //vndb_log->log_debug("Result of sql \"%s\" = \"%s\".\n",
  //stmt.get_sql().c_str(), val.dump().c_str()); 
  auto var_name = util::string_view(var, true);
  vndb_log->log_debug("Adding variable %.*s.\n",
                      (int)var_name.size(), var_name.data());
  /*auto [it, inserted] = */
  vndb->symbol_table.insert_or_assign(std::move(var_name),
                                      std::move(val));
  return SQLITE_OK;
}
int set_var_to_sql(vndb_main *vndb, std::string_view var,
                   std::string_view sql, bool as_object = true){
  auto stmt = vndb->db.prepare_stmt(sql);
  if(!stmt){
    return vndb->db.errcode();
  }
  return set_var_to_sql(vndb, var, stmt, as_object);
}

//path is: \[number|name\]+
// where number = [0-9]+ and name = [a-zA-z][[:alnum:]]+
//I may extend this defination later, but this is the eaisest to parse.
json* follow_json_path(const json *val, std::string_view path){
  const json *current = val;
  const char *ptr = path.data();
  const char *end = &path.back();
  //TODO: Need to check that index is in range / subobject exists and
  //print an error if not. This shouldn't crash the program if given a
  //path to a nonexistant object.
  while(ptr < end){
    if(*ptr != '['){
      printf("Unexpected character %c in json path, expected '[' or end of path.\n", *ptr);
      return nullptr;
    }
    char c = *(++ptr);
    if(c >= '0' && c <= '9'){
      long idx = strtol(ptr, (char**)(&ptr), 10);
      if(!current->is_array()){
        printf("Error expected json array but got a(n) %s.\n", current->type_name());
        return nullptr;
      }
      if(idx > (long)current->size()){
        printf("Index %ld out of range for array of size %lu.\n",
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
        printf("Could not find '%.*s' in json object.\n", (int)name.size(), name.data());
        return nullptr;
      }
      ptr = next_ptr;
    }
    ptr++;//skip ']' character.
  }
  return (json*)current;
}
bool expand_variable(vndb_main *vndb, std::string_view var, json *val_ptr){
  //First find the name of the variable and make sure it exists.
  int start = (var[0] == '$' ? 1 : 0);//ignore a '$' prefix.
  size_t idx = start+1;
  //could definately be optimized
  
  while(idx < var.size() && var[idx] != '['){
    ++idx;
  }
  util::string_view var_base = var.substr(start, idx - start);
  json *val = find_or_null(vndb->symbol_table, var_base);
  if(!val){
    printf("Undefined variable %.*s.\n",
           (int)var_base.size(), var_base.data());
    return false;
  }
  //now follow the path if one exists
  if(idx < var.size()){
    val = follow_json_path(val, var.substr(idx));
      if(!val){ return false; }
  }
  *val_ptr = *val;
  return true;
}
//expr is a string with a ';' as the final character. Currenly
//the only supported expressions are numbers and variables.
bool eval_expr(vndb_main *vndb, std::string_view expr, json *val_ptr){
  const char *ptr = expr.data();
  assert(expr.back() == ';');
  ptr = skip_space(ptr);
  char ch = *ptr;
  if(isdigit(ch)){ //Just a number.
    long val = strtol(ptr, (char**)&ptr, 10);
    while(*ptr == ' '){ ++ptr; }  //skip space
    if(*ptr != ';'){
      printf("Excess characters at end of expression : %.*s.\n",
             (int)(expr.size() - (ptr - expr.data())), ptr);
      return false;
    } else {
      *val_ptr = val;
      return true;
    }
  } else if((ch == '$') || isalpha(ch)){
    const char *var_start = ptr;
    //Find the end of the variable currently only ' ' and ';'
    //have any real use, but I do intend to add more complex expressions    
    ptr = strpbrk(ptr, variable_delimiters);
    std::string_view var(var_start, ptr - var_start);
    //Kind of a hack, I may limit sql to only be in parentheses.
    if(var == "select"sv){
      return (set_val_to_sql(vndb, var_start, val_ptr) == SQLITE_OK);
    }
    return expand_variable(vndb, var, val_ptr);
  } else if(ch == '('){
    const char* endptr = &expr.back();
    //find closing parentheses
    const char* end = (char*)memrchr(endptr, ')', endptr - ptr);
    std::string_view sql(ptr, end - ptr);
    return (set_val_to_sql(vndb, sql, val_ptr) == SQLITE_OK);
  } else {
    printf("Unexpected character '%c' in expression '%.*s'.\n",
           *ptr, (int)expr.size(), expr.data());
    return false;
  }
}
//Look for the start of a variable, a dollar sign followed by
//an alphanumeric character. We need this function since a dollar
//sign can also be part of a regexp.
const char* find_var_start(const char *str){
  const char *var_start = strchr(str, '$');
  if(!var_start){ return nullptr; }
  return(isalnum(*(var_start + 1)) ? var_start : nullptr);
}
int do_sql_command(vndb_main *vndb, const char *sql){
  //Variables in sql must begin with a '$' ala perl and bash, this
  //makes it much eaiser to find them and avoids name conflicts.
  const char *var_start = nullptr;
  if((var_start = find_var_start(sql)) != nullptr){
    //Replace each instance of a variable with a paramater for a
    //prepared statement, then bind the variables to those statements.
    //This is the safest way to do this (i.e it prevents sql injection).
    //I may also eventually allow doing actual text substution.
    vndb->buf.clear();
    const char *prev_var_end = sql;
    std::vector<std::string_view> vars;
    do {
      vndb->buf.append(prev_var_end, var_start - prev_var_end).append(" ? ");
      prev_var_end = strpbrk(var_start, variable_delimiters);
      vars.emplace_back(var_start, prev_var_end - var_start);
      if(*prev_var_end == ';'){ break; }
    } while((var_start = find_var_start(prev_var_end)));
    vndb->buf.append(prev_var_end);//append rest of sql.
    util::string_view sql_sv = vndb->buf.move_to_string_view();
    auto stmt = vndb->db.prepare_stmt(sql_sv);
    if(!stmt){
      printf("failed to compile sql '%.*s'.\n", (int)sql_sv.size(), sql_sv.data());
      return vndb->db.errcode();
    }
    int idx = 1;
    json val;
    for(auto &&var : vars){
      if(!expand_variable(vndb, var, &val)){
        return -1;
      }
      stmt.bind(idx++, val);
    }
    int err = set_var_to_sql(vndb, "last", stmt);
    if(err != SQLITE_OK){
      return err;
    }
  } else { //no variables to expand
    int err = set_var_to_sql(vndb, "last", sql);
    if(err != SQLITE_OK){
      return err;
    }
  }
  //print rows, We may print results differently depending on if there was
  //one column in the result or more than one.
  const json &last = vndb->symbol_table["last"];
  if(last.empty()){ //No results found
    //Maybe print something like "No results for sql command.\n"
    fputc('\n', stdout);
    return SQLITE_OK;
  }
  //TODO: add some formatting options
  if(last.is_array() && !(last[0].is_object())){
    if(last.size() == 1){
      last[0].write(stdout);
    } else {
      auto it = last.begin();
      it->write(stdout);
      while(++it != last.end()){
        printf(" | ");
        it->write(stdout);
      }
    }
    fputc('\n', stdout);
  } else {
    for(auto &&row : last){
      auto &row_obj = row.get_ref<json::object_t>();
      auto it = row_obj.begin();
      it->second.write(stdout);
      while(++it != row_obj.end()){
        printf(" | ");
        it->second.write(stdout);
      }
      fputc('\n', stdout);
    }
  }
  return SQLITE_OK;
}
enum class command_type {
  help,
  length,
  print,
  select,
  set,
  sql,
  type,
  view,
  write
};
static constexpr util::array command_names(
  util::va_init_tag,
  "help"sv,
  "length"sv,
  "print"sv,
  "select"sv,
  "set"sv,
  "sql"sv,
  "type"sv,
  "view"sv,
  "write"sv
);
int do_command(vndb_main *vndb, std::string_view command){
  const char *cmd_start = skip_space(command.data());
  const char *cmd_end = strpbrk(cmd_start, " ;");
  std::string_view cmd(cmd_start, cmd_end - cmd_start);
  int cmd_val = is_unique_prefix(cmd, command_names.data(), command_names.size());
  if(cmd_val < 0){
    if(cmd_val == -1){
      printf("Unknown command '%.*s.\n", (int)cmd.size(), cmd.data());
    } else if(cmd_val == -2){
      printf("Ambiguous command '%.*s.\n", (int)cmd.size(), cmd.data());
    }
    return cmd_val;
  }
  //translate cmd to command_type
  command_type cmd_type = command_type(cmd_val);
  switch(cmd_type){
    case command_type::help:{
      printf("%s.\n", help_string.data());
      return 0;
    }
    case command_type::sql:
    case command_type::select:{
      const char *sql = (cmd_type == command_type::sql ? cmd_end : command.data());
      return do_sql_command(vndb, sql);
    }
    //For print & set Make sure the value that eval_expr returns
    //is pointing to is still in scope.

    //Trying to commbine the write and print commands just results
    //in more code due to scoping issues inside of switch statements.
    case command_type::write:{
      const char *filename_end = strchr(skip_space(cmd_end), ' ');
      if(!filename_end){
        printf("Malformed write command.\n");
        return -1;
      }
      std::string_view filename(cmd_end+1, filename_end - cmd_end);
      FILE_wrapper out(filename, "w");
      if(!out){
        printf("Could not open file %.*s.\n", (int)filename.size(), filename.data());
        return -1;
      }
      json val;
      std::string_view expr = command.substr(cmd.size() + filename.size() + 1);
      if(!eval_expr(vndb, expr, &val)){
        return -1;
      } else {
        val.pprint(out.unwrap());
        return 0;
      }
    }
    case command_type::print:{
      json val;
      if(!eval_expr(vndb, command.substr(cmd.size()), &val)){
        return -1;
      } else {
        val.pprint(stdout);
        fputc('\n', stdout);
        return 0;
      }
    }
    case command_type::length:{
      json val;
      if(!eval_expr(vndb,command.substr(cmd.size()), &val)){
        return -1;
      } else {
        if(!val.is_array() && !val.is_object()){
          printf("Expr has non compound type %s.\n", val.type_name());
          return -1;
        } else {
          printf("%lu.\n", val.size());
          return 0;
        }
      }
    }
    case command_type::type:{
      json val;
      if(!eval_expr(vndb,command.substr(cmd.size()), &val)){
        return -1;
      } else {
        printf("%s.\n", val.type_name());
        return 0;
      }
    }
    //set name [=] expr
    case command_type::set:{
      const char *name_start = skip_space(cmd_end);
      const char *name_end = strpbrk(name_start, " =");
      if(!name_end){
        printf("Malformed set command.\n");
        return -1;
      }
      const char *expr_start = skip_space(name_end);
      if(*expr_start == '='){ 
        ++expr_start;
        expr_start = skip_space(expr_start);
      }
      json val;
      std::string_view expr = command.substr(name_end - command.data());
      if(!eval_expr(vndb, expr, &val)){
        return -1;
      }
      //might be off by one for the size.
      auto name =
        util::string_view(command.substr(cmd.size()+1, name_end - name_start), true);
      vndb_log->log_debug("Adding new variable %.*s.\n", (int)name.size(), name.data());
      vndb->symbol_table.insert_or_assign(std::move(name), std::move(val));
      return 0;
    }
    case command_type::view:{
      if(!sdl_running){
        printf("SDL is not running so images can not be displayed.\n");
        return -1;
      }
      //This should be sql_select_vn_image_by_id, compiled.
      sqlite3_stmt_wrapper& stmt =
        vndb->get_select_by_id_stmt(vndb_main::table_type::vn_images);
      cmd_end = skip_space(cmd_end);
      char *tmp;
      int id = strtol(cmd_end, &tmp, 10);
      if(cmd_end == tmp){
        printf("Missing id in view command.\n");
        return -1;
      }
      stmt.bind(1, id);
      int res = stmt.step();
      if(res == SQLITE_DONE){
        printf("Could not find an image for vn %d.\n", id);
        //Not sure what to return here, it's not really an error but
        //it's not successful either.
        return 0;
      } else if(res == SQLITE_ROW){
        const void *data = stmt.get_column<const void*>(0);
        size_t data_size = stmt.get_column_bytes(0);
        SDL_Event evt;
        init_jpeg_user_event(&evt, data, data_size);
        SDL_PushEvent(&evt);
        //The pointer to data is only valid until we call stmt.step/reset
        //so we need to wait for the other thread to be done with it.
        SDL_SemWait(vndb->sdl_sem);
        assert(stmt.step() == SQLITE_DONE);
        return stmt.reset();
      } else {
        printf("Error executing sql.\n");
        return -1;
      }
    }
    default:{
      printf("Unknown command.\n");
      return -1;
    }
  }
}
enum class dot_command_type {
  help,
  vars,
  log
};
static constexpr util::array dot_command_names(util::va_init_tag,
                                               "help"sv, "vars"sv, "log"sv);
int do_dot_command(vndb_main *vndb, std::string_view command){
  std::string_view cmd = skip_space(command);
  assert(command[0] == '.');
  int cmd_val = is_unique_prefix(cmd.substr(1), dot_command_names.data(),
                                 dot_command_names.size());
  if(cmd_val < 0){
    if(cmd_val == -1){
      printf("Unknown dot command '%.*s.\n", (int)cmd.size(), cmd.data());
    } else if(cmd_val == -2){
      printf("Ambiguous dot command '%.*s.\n", (int)cmd.size(), cmd.data());
    }
    return cmd_val;
  }
  auto cmd_type = dot_command_type(cmd_val);
  switch(cmd_type){
    case dot_command_type::help:
      printf("%s.\n", help_string.data());
      return 0;
    case dot_command_type::vars:{
      for(auto &&[key, val] : vndb->symbol_table){
        printf("'%.*s'\n", (int)key.size(),key.data());
      }
      return 0;
    }
    case dot_command_type::log:{
      FILE_wrapper log_file(current_log_file, "r");
      //a bit lazy and inefficent but it should work.
      printf("%s\n", log_file.to_string().c_str());
      return 0;
    }
    default:
      printf("Unknown dot command");
      return -1;
  }
}
//Functions to proved generic access to readline type libraries, so that I'm not
//tied to one particular library.
#define HAVE_READLINE
#ifdef HAVE_READLINE
#include <readline/readline.h>
#include <readline/history.h>
#endif

#ifdef HAVE_EDITLINE
#include <editline/readline.h>
#endif

#if (defined HAVE_READLINE) || (defined HAVE_EDITLINE)
static constexpr bool have_history = true;
char *vndb_readline(const char *prompt){
  return readline(prompt);
}
void vndb_add_to_history(const char *str, bool copy = false){
  if(copy){
    const char *tmp = str;
    str = strdup(tmp);
  }
  add_history(str);
}
int vndb_read_history(const char *filename){
  return read_history(filename);
}
int vndb_write_history(const char *filename){
  return write_history(filename);
}
void vndb_limit_history(int cnt) {
  stifle_history(cnt);
}
#elif (defined USE_LINENOISE)
#else
static constexpr bool have_history = true;
#endif
[[noreturn]] void run_interactively(vndb_main &vndb){
  char *lineptr = nullptr;
  char *endptr = nullptr;
  int err = 0;
  util::string_view line;
  static constexpr size_t histfile_name_bufsz = 512;
  static constexpr const char *histfile_basename = ".vndb_cpp_history";
  char histfile_name[histfile_name_bufsz];
  if(have_history){
    char *homedir = getenv("HOME");
    snprintf(histfile_name, histfile_name_bufsz,
             "%s/.%s", homedir, histfile_basename);
    vndb_read_history(histfile_name);
    vndb_limit_history(2000);
  }
  if(vndb.gui == vndb_main::gui_type::sdl){
    if(!vndb.init_sdl()){
      printf("Could not initialize SDL, will not be able to display images.\n");
    }
  } else if(vndb.gui == vndb_main::gui_type::fltk){
    printf("FLTK gui is currently unimplemented.\n");
  }
  while(1){
    const char *prompt = "vndb >";
    vndb.buf.clear();
    lineptr = vndb_readline(prompt);
    if(!lineptr){
      fputc('\n', stdout);
      err = 0;
      goto end;
    }
    char c = *skip_space(lineptr);//first nonspace character
    if(c == '.'){
      if(do_dot_command(&vndb, lineptr) >= 0){
        vndb_add_to_history(lineptr);
        continue;
      } else {
        free(lineptr);
      }
    }
    if(!(endptr = strchr(lineptr, ';'))){
      prompt = " ... >";
      do {
        vndb.buf.append(lineptr).append(' ');//translate newlines to spaces
        free(lineptr);
        lineptr = vndb_readline(prompt);
        if(!lineptr){ goto end; }
      } while(!(endptr = strchr(lineptr, ';')));
    }
    //append everything upto and including the semicolon.
    vndb.buf.append(lineptr, (endptr - lineptr) + 1);
    //make sure there's no actual text after the semicolon
    while(*(++endptr) != '\0'){
      if(!isspace(*endptr)){
        printf("Unexpected text following ';' : \"%s\".\n", endptr);
        goto next;
      }
    }
    //grab the memory allocated by the buffer so we can store in the history.
    line = vndb.buf.move_to_string_view();
    line.release_memory(); //we're transfering ownership to the history manager.
    vndb_add_to_history(line.data());
    //There isn't really anything to do with the return value of do_command...
    err = do_command(&vndb, line);
  next:
    free(lineptr);
  }
 end:
  free(lineptr);
  write_history(histfile_name);
  if(sdl_running){
    SDL_Event evt;
    evt.type = SDL_QUIT;
    SDL_PushEvent(&evt);
    SDL_SemWait(vndb.sdl_sem);
  }
  exit(abs(err));
}
