/*
  Simple interactive loop, read until we encounter a semicolon, it's not
  the most elegent solution but it allows multi-line commands without
  having to actually parse the input.

  Input takes the form of: <command> <arguments>*
  where the commands are:

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
#include "vndb.h"
#if (defined UNIX)
#include <unistd.h>
#include <fcntl.h>
#elif (defined _WIN32)
#include <windows.h>
#endif

static constexpr std::string_view help = 
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
#ifdef UNIX
void set_close_exec(){
  DIR* dirp = opendir("/proc/self/fd");
  struct dirent d;
  while((d = readdir(dirp)) != nullptr){
    int fd = strtol(d->d_name, NULL, 0);
    fcntl(fd, F_SETFD, fcntl(fd, F_GETFD) | FD_CLOEXEC);
  }
  closedir(dirp);
  return;
}
#else
void set_close_exec(){}
#endif
#ifdef UNIX
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

char *vndb_readline(const char *prompt);
void vndb_add_to_history(const char *str, bool copy = false);
int is_unique_prefix(std::string_view prefix, std::string_view *strs, int nstrs);
enum class command_type {
  sql,
  select,
  set,
  print,
  write,
  view,
  help 
};
static constexpr util::array commands("sql"sv, "select"sv, "set"sv, "print"sv,
                                      "write"sv, "help"sv, "view"sv);
int do_command(vndb_main *vndb, std::string_view command){
  char *cmd_end = strpbrk(command.data(), " ;");
  std::string_view cmd = command.substr(0, cmd_end - command.data());
  //translate cmd to command_type
  command_type cmd_type;
  if(cmd_type == command_type::sql || cmd_type == command_type::select){
    char *sql = (cmd_type == command_type::sql ? cmd_end : command.data());
    auto stmt = vndb->db.prepare_stmt(sql);
    if(!stmt){
      fprintf(stderr, "Failed to compile sql.\n");
    }
    int err;
    symbol_table["last"] = stmt.exec_json(&err);
    if(err != SQLITE_OK){
      fprintf(stderr, "Failed to execute sql.\n");
    }
    json &result = symbol_table["last"];
    //print rows
  }
}
    

int main(int argc, char* argv[]){


  using_history();
  string_buf buf;
  char *lineptr, *endptr;
  while(lineptr = readline("> ")){
    if(!(endptr = strchr(lineptr, ';'))){
      buf.append(lineptr).append(' ');//translate newlines to spaces
      free(lineptr);
      continue;
    } else {
      //make sure there's no actual text after the semicolon
      while(*(++endptr) != '\0'){
        if(!isspace(*endptr)){
          //print some error
        }
      }
      //Check if the semicolon is the only thing on the line and just
      //append it to the last line in that case.

      //grab the memory allocated by the buffer so we can store in the history.
      util::string_view sv = buf.move_to_string_view();
      //Unset the owned flag of sv so it doesn't get freed.
      add_history(str.data());

      //Exectute whatever command was in sv & print the results or whatever.
    }
  }
}
