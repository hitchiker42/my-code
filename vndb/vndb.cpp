#include "vndb.h"
//Get 100 VNs from server and write into outfile
bool test_connection(const char *outfile, int start = 1){
  vndb_connection conn;
  if(!conn.logged_in){
    if(conn.error){
      fprintf(stderr, "Error failed to login with error:\n");      
      json err = conn.get_error();
      err.print(stdout);
    } else {
      fprintf(stderr, "Error failed to connect to server\n");
    }
    return false;
  }
  FILE_wrapper f(outfile, "w");
  if(!f){
    fprintf(stderr, "Error opening %s\n", outfile);
    return false;
  }
  json dbstats = conn.dbstats();
  if(conn.error){
    fprintf(stderr, "Error getting dbstats\n");
    return false;
  }
  printf("VNDB stats : %s\n", dbstats.dump().c_str());
  //Get 100 to make sure we test the ability to get multiple pages of results.
  json vns = conn.get_vns(start, 100);
  if(conn.error){
    fprintf(stderr, "Error sending/receiving to/from server\n");
    return false;
  }
  printf("Got %ld vns from server.\n", vns.size());
  vns.print(f.unwrap());
  return true;
}
int main(int argc, char* argv[]){
  if(!init_vndb_ssl_ctx()){
    fprintf(stderr, "Error, failed to initialize ssl context\n");
    return -1;
  }
  //TODO: add argument parsing
  const char *outfile = "conn_test.out";
  printf("Running connection test\n");
  if(!test_connection(outfile)){
    free_vndb_ssl_ctx();
    return -1;
  }
  printf("Test seemed to succeed, results in %s\n",outfile);
  return 0;
}
#if 0
int main(int argc, char* argv[]){

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
    write value filename; | print value to the file 'filename'
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
  */
  using_history();
  string_buf buf;
  char *lineptr, *endptr;
  while(lineptr = readline("> ")){
    if(!(endptr = strchr(lineptr, ';'))){
      buf.append(lineptr).append('\n');
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

#endif
