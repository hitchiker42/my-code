#include "vndb.h"
#include "sql.h"
bool vndb_main::init_insert_stmts(){
  static constexpr std::array<std::string_view, this->num_tables_total> sql = {{
      sql_insert_vn, sql_insert_release, sql_insert_producer, 
      sql_insert_character, sql_insert_staff, sql_insert_tag, sql_insert_trait,
      sql_insert_vn_producer_relation, sql_insert_vn_character_actor_relation,
      sql_insert_staff_alias, sql_insert_vn_tags, sql_insert_character_traits
    }};
  using table_type = vndb_main::table_type;
  static constexpr std::array<table_type, this->num_tables_total> types = {{
      table_type::VN, table_type::release, table_type::producer,
      table_type::character, table_type::staff, table
      table_type::tags, table_type::traits,
      table_type::vn_producer_relations, table_type::vn_character_actor_relations,
      table_type::vn_staff_relations,table_type::staff_aliases,
      table_type::vn_tags, table_type::character_traits,
      table_type::vn_images, table_type::character_images
    }};
  for(int i = 0; i < 5; i ++){
    auto stmt = this->db.prepare_stmt(sql[i]);
    if(!stmt){
      return false;
    }
    this->insert_stmts[to_underlying(types[i])] = stmt;
  }
  return true;
}
bool vndb_main::init_get_id_stmts(){
  std::array<std::string_view, this->num_base_tables> sql = {{
      sql_select_vn_by_id, sql_select_release_by_id,
      sql_select_producer_by_id, sql_select_character_by_id,
      sql_select_staff_by_id, sql_select_tag_by_id, 
      sql_select_trait_by_id
  }};
  using table_type = vndb_main::table_type;
  static constexpr std::array<table_type, this->num_base_tables> types = {{
      table_type::VNs, table_type::releases, table_type::producers,
      table_type::characters, table_type::staff, table_type::tags,
      table_type::traits
  }};
  for(int i = 0; i < 5; i ++){
    auto stmt = this->db.prepare_stmt(sql[i]);
    if(!stmt){
      return false;
    }
    this->get_by_id_stmts[to_underlying(types[i])] = stmt;
  }
  return true;
}

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
    fprintf(stderr, "%s\n", conn.buf.c_str());
    return false;
  }
  printf("Got %ld vns from server.\n", vns.size());
  vns.print(f.unwrap());
  return true;
}
//Read a list of vns as json from a file (likely created as a result of
//the above test_connection function) and insert them into the given database.
//stmt needs to be a statement to insert a vn.
bool test_insert_vns(sqlite3_wrapper &db, sqlite3_stmt_wrapper& stmt,
                     const char *json_file){
  FILE_wrapper f(json_file, "r");
  if(!f){
    fprintf(stderr, "Error opening %s\n", json_file);
    return false;
  }
  fprintf(stderr, "Parsing json file\n");
  json vns_json = json::parse(f.unwrap());
  if(vns_json.is_null()){
    fprintf(stderr, "Error parsing json\n");
    return false;
  }
  //It's assumed that there are 100 VNs, but its written
  //to support any number.
  json::array_t &vns = vns_json.get_ref<json::array_t>();
  int num_vns = vns.size();
  int err = SQLITE_OK;
  //insert first 10 as indivual insert statements.
  fprintf(stderr, "Begining to insert VNs.\n");
  for(int i = 0; i < std::min(10, num_vns); i++){
    if((err = sqlite_insert_vn(vns[i], stmt)) != SQLITE_OK){ goto error; }
  }
  fprintf(stderr, "Inserted first 10 VNs.\n");
  //Insert the rest of the vns using transactions, 10 vns per transaction.
  for(int i = 10; i < num_vns; i+=10){
    if((err = db.begin_transaction()) != SQLITE_OK){ goto error; }
    for(int j = i; j < std::min(num_vns, i + 10); j++){
      if((err = sqlite_insert_vn(vns[j], stmt)) != SQLITE_OK){ 
        db.rollback_transaction();
        goto error; 
      }
    }
    if((err = db.commit_transaction()) != SQLITE_OK){ goto error; }
  }
  fprintf(stderr, "Finished inserting VNs.\n");
  return true;
 error:
  db.print_errmsg("Recieved sqlite error");
  return false;
}
bool download_all(const char *database_filename){
  vndb_main vndb(database_filename);
  //compile sql, connect to servert & get dbstats (for progress bars).
  if(!vndb.db ||
     !vndb.init_insert_stmts() ||
     !vndb.connect() ||
     !vndb.init_db_stats()){
    return false;
  }
  printf("Downloading VNs\n");
  if(!vndb.download_and_insert_all(vndb::object_type::VN)){ return false; }
  printf("Downloading Releases\n");
  if(!vndb.download_and_insert_all(vndb::object_type::release)){ return false; }
  printf("Downloading Producers\n");
  if(!vndb.download_and_insert_all(vndb::object_type::producer)){ return false; }
  printf("Downloading Characters\n");
  if(!vndb.download_and_insert_all(vndb::object_type::character)){ return false; }
  printf("Downloading Staff\n");
  if(!vndb.download_and_insert_all(vndb::object_type::staff)){ return false; }
  return true;
}
int run_connection_test(){
  //TODO: add argument parsing
  const char *outfile = "conn_test.out";
  printf("Running connection test\n");
  if(!test_connection(outfile)){
    return -1;
  }
  printf("Test seemed to succeed, results in %s\n",outfile);
  return 0;
}
int run_insertion_test(){
  sqlite3_wrapper db("test.db");
  if(!db){
    fprintf(stderr, "Error opening sqlite database.\n");
  }
  int err;
  if((err = db.exec_file(db_init_file)) != SQLITE_OK){
    //IF err == SQLITE_CANTOPEN there was an error opening db_init_file
    //and an error message was already printed.
    if(err != SQLITE_CANTOPEN){
      db.print_errmsg("Error initializing datbase");
    }
    return -1;
  }
  sqlite3_stmt_wrapper stmt = db.prepare_stmt(sql_insert_vn);
  if(!stmt){
    db.print_errmsg("Error compiling sql statement");
    return -1;
  }
  //negate return value since 0 is success for the shell.
  return !test_insert_vns(db, stmt, "conn_test.out");
}
int main(int argc, char* argv[]){
  if(!init_vndb_ssl_ctx()){
    fprintf(stderr, "Error, failed to initialize ssl context\n");
    return -1;
  }
  atexit(free_vndb_ssl_ctx);
  if(argc > 1){
    if(argv[1][0] == 'c'){
      return run_connection_test();
    } else {
      return run_insertion_test();
    }
  }
  return run_insertion_test();
}

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
