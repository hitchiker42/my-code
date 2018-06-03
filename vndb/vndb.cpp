#include "vndb.h"
#include "sql.h"
std::unique_ptr<util::logger> vndb_log;

bool vndb_main::init_insert_stmts(){
  static constexpr std::array<std::string_view, this->num_tables_total> sql = {{
      sql_insert_vn, sql_insert_release, sql_insert_producer, 
      sql_insert_character, sql_insert_staff, sql_insert_tag, sql_insert_trait,
      sql_insert_vn_producer_relation, sql_insert_vn_character_actor_relation,
      sql_insert_staff_alias, sql_insert_vn_tag, sql_insert_character_trait
    }};
  using table_type = vndb_main::table_type;
  static constexpr std::array<table_type, this->num_tables_total> types = {{
      table_type::VNs, table_type::releases, table_type::producers,
      table_type::characters, table_type::staff,
      table_type::tags, table_type::traits,
      table_type::vn_producer_relations, table_type::vn_character_actor_relations,
      table_type::vn_staff_relations,table_type::staff_aliases,
      table_type::vn_tags, table_type::character_traits,
      table_type::vn_images, table_type::character_images
    }};
  for(int i = 0; i < 5; i ++){
    sqlite3_stmt *ptr = this->db.prepare_stmt_ptr(sql[i]);
    if(!ptr){
      this->db.print_errmsg("Failed to compile statement");
      fprintf(stderr, "%s\n", sql[i].data());
      return false;
    }
    this->insert_stmts[to_underlying(types[i])].stmt = ptr;
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
    sqlite3_stmt *ptr = this->db.prepare_stmt_ptr(sql[i]);
    if(!ptr){
      return false;
    }
    this->get_by_id_stmts[to_underlying(types[i])].stmt = ptr;
  }
  return true;
}
bool vndb_main::build_vn_tags(){
  sqlite3_wrapper &db = this->db;
  auto stmt = db.prepare_stmt("select id, tags from VNs;");
  if(!stmt){
    db.print_errmsg("Failed to compile sql");
  }
  auto &ins_stmt = this->get_insert_stmt(vndb_main::table_type::vn_tags);
  db.begin_transaction();
  int res;
  while((res = stmt.step()) == SQLITE_ROW){
    int id = stmt.get_column<int>(0);
    json tags = stmt.get_column<json>(1);
    int err = sqlite_insert_vn_tags(id, tags, ins_stmt);
    if(err != SQLITE_OK){
      db.print_errmsg("Failed to insert into vn_tags");
      vndb_log->printf(util::log_level::warn,
                       "Failed to insert tags for vn %d, tags were %s\n",
                       id, tags.dump().c_str());
      db.rollback_transaction();
      return false;
    }
  }
  if(res != SQLITE_DONE){
      db.print_errmsg("Failure running 'select id,tags from VNs;'");
      db.rollback_transaction();
      return false;
  }
  return (db.commit_transaction() == SQLITE_OK);
}  
bool vndb_main::build_character_traits(){
  sqlite3_wrapper &db = this->db;
  auto stmt = db.prepare_stmt("select id, traits from characters;");
  if(!stmt){
    db.print_errmsg("Failed to compile sql");
  }
  auto &ins_stmt = this->get_insert_stmt(vndb_main::table_type::character_traits);
  db.begin_transaction();
  int res;
  while((res = stmt.step()) == SQLITE_ROW){
    int id = stmt.get_column<int>(0);
    json traits = stmt.get_column<json>(1);
    int err = sqlite_insert_character_traits(id, traits, ins_stmt);
    if(err != SQLITE_OK){
      db.print_errmsg("Failed to insert into character_traits");
      vndb_log->printf(util::log_level::warn,
                       "Failed to insert tags for character %d, traits were %s\n",
                       id, traits.dump().c_str());
      db.rollback_transaction();
      return false;
    }
  }
  if(res != SQLITE_DONE){
      db.print_errmsg("Failure running 'select id,traits from characters;'");
      db.rollback_transaction();
      return false;
  }
  return (db.commit_transaction() == SQLITE_OK);
}
//uses the releases table
bool vndb_main::build_vn_producer_relations(){
  sqlite3_wrapper &db = this->db;
  auto stmt = db.prepare_stmt("select id, vn, producers from releases;");
  if(!stmt){
    db.print_errmsg("Failed to compile sql");
  }
  auto &ins_stmt = 
    this->get_insert_stmt(vndb_main::table_type::vn_producer_relations);
  db.begin_transaction();
  int res;
  while((res = stmt.step()) == SQLITE_ROW){
    int id = stmt.get_column<int>(0);
    json vns = stmt.get_column<json>(1);
    json producers = stmt.get_column<json>(2);
    int err = sqlite_insert_vn_producer_relations(id, vns, producers, ins_stmt);
    if(err != SQLITE_OK){
      db.print_errmsg("Failed to insert into vn_producer_relations");
      vndb_log->printf(util::log_level::warn,
                       "Failed to insert relation for release %d,"
                       "vns = %s, producers = %s.\n",
                       id, vns.dump().c_str(), producers.dump().c_str());
      db.rollback_transaction();
      return false;
    }
  }
  if(res != SQLITE_DONE){
      db.print_errmsg("failure running 'select id, vn, producers from releases;'");
      db.rollback_transaction();
      return false;
  }
  return (db.commit_transaction() == SQLITE_OK);
}
//builds vn_character_actor_relations, vn_staff_relations and staff_aliases.
bool vndb_main::build_staff_derived_tables(){
  sqlite3_wrapper &db = this->db;
  auto stmt = db.prepare_stmt("select id, aliases, vns, voiced from staff;");
  if(!stmt){
    db.print_errmsg("Failed to compile sql");
  }
  auto &vca_ins_stmt = 
    this->get_insert_stmt(vndb_main::table_type::vn_character_actor_relations);
  auto &vs_ins_stmt = 
    this->get_insert_stmt(vndb_main::table_type::vn_staff_relations);
  auto &sa_ins_stmt = 
    this->get_insert_stmt(vndb_main::table_type::staff_aliases);
  db.begin_transaction();
  int res;
  while((res = stmt.step()) == SQLITE_ROW){
    int id = stmt.get_column<int>(0);
    json aliases = stmt.get_column<json>(1);
    json vns = stmt.get_column<json>(2);
    json voiced = stmt.get_column<json>(3);
    int err = sqlite_insert_vn_character_actor_relations(id, voiced, vca_ins_stmt);
    if(err != SQLITE_OK){
      db.print_errmsg("Failed to insert into vn_character_actor_relations");
      vndb_log->printf(util::log_level::warn,
                       "Failed to insert into vn_character_actor_relations for "
                       "staff = %d, voiced = %s\n",
                       id, voiced.dump().c_str());
      db.rollback_transaction();
      return false;
    }
    err = sqlite_insert_vn_staff_relations(id, vns, vs_ins_stmt);
    if(err != SQLITE_OK){
      db.print_errmsg("Failed to insert into vn_staff_relations");
      vndb_log->printf(util::log_level::warn,
                       "Failed to insert into vn_staff_relations for "
                       "staff = %d, vn = %s\n",
                       id, vns.dump().c_str());
      db.rollback_transaction();
      return false;
    }
    err = sqlite_insert_staff_aliases(id, aliases, sa_ins_stmt);
    if(err != SQLITE_OK){
      db.print_errmsg("Failed to insert into staff_aliases");
      vndb_log->printf(util::log_level::warn,
                       "Failed to insert into staff_aliases for "
                       "staff = %d, aliases = %s\n",
                       id, aliases.dump().c_str());
      db.rollback_transaction();
      return false;
    }
  }
  if(res != SQLITE_DONE){
      db.print_errmsg("Failure running 'select id, aliases, vns, voiced from staff;'");
      db.rollback_transaction();
      return false;
  }
  return (db.commit_transaction() == SQLITE_OK); 
}
static bool build_image_table(sqlite3_wrapper &db,
                              sqlite3_stmt_wrapper &stmt,
                              sqlite3_stmt_wrapper &ins_stmt,
                              const char *name){
  http_connection conn("s.vndb.org", "443");
  db.begin_transaction();
  int res;
  util::svector<char> buf;
  while((res = stmt.step()) == SQLITE_ROW){
    int id = stmt.get_column<int>(0);
    std::string_view uri = stmt.get_column<std::string_view>(1);
    int err = conn.http_get(uri, buf);
    if(!err){
      fprintf(stderr, "Error downloding https://s.vndb.org%s.\n", uri.data());
      db.rollback_transaction();
      return false;
    }
    ins_stmt.bind(1, id);
    ins_stmt.bind(2, (void*)buf.data(), buf.size());
    err = ins_stmt.exec();
    if(err != SQLITE_OK){
      fprintf(stderr, "Error inserting %s image.\n", name);
      db.rollback_transaction();
      return false;
    }
  }
  if(res != SQLITE_DONE){
    fprintf(stderr, "Failure running '%s' : %s(%d).",
            stmt.get_sql_template().data(), db.errmsg(), db.errcode());
    db.rollback_transaction();
    return false;
  }
  return (db.commit_transaction() == SQLITE_OK); 
}  
/*
  The links for vn images should be of the form:
  https://s.vndb.org/cv/.+
  and for character images:
  https://s.vndb.org/ch/.+
*/
bool vndb_main::build_vn_images(){  
  sqlite3_wrapper &db = this->db;
  //substr(image,19) cuts the leading "https://s.vndb.org" from the link
  auto stmt = db.prepare_stmt("select id, substr(image, 19) from VNs");
  if(!stmt){
    db.print_errmsg("Failed to compile sql");
  }
  auto &ins_stmt = this->get_insert_stmt(vndb_main::table_type::vn_images);
  return build_image_table(db, stmt, ins_stmt, "VNs");
}  
bool vndb_main::build_character_images(){
  sqlite3_wrapper &db = this->db;
  //substr(image,19) cuts the leading "https://s.vndb.org" from the link
  auto stmt = db.prepare_stmt("select id, substr(image, 19) from characters");
  if(!stmt){
    db.print_errmsg("Failed to compile sql");
  }
  auto &ins_stmt = this->get_insert_stmt(vndb_main::table_type::vn_images);
  return build_image_table(db, stmt, ins_stmt, "characters");
}  
bool download_all(const char *database_filename, int vn_start = 1,
                  int release_start = 1, int producer_start = 1, int character_start = 1,
                  int staff_start = 1){
  vndb_main vndb(database_filename);
  //compile sql, connect to servert & get dbstats (for progress bars).
  if(!vndb.db){
    fprintf(stderr, "Failed to open database file %s.\n", database_filename);
    return false;
  }
  if(!vndb.init_db()){
    fprintf(stderr, "Failed to run database_init.sql.\n");
    return false;
  }
  if(!vndb.init_insert_stmts()){
    fprintf(stderr, "Failed to compile sql insert statements.\n");
    return false;
  }
  if(!vndb.connect()){
    fprintf(stderr, "Failed to connect to vndb server.\n");
    return false;
  }
  if(!vndb.init_db_stats()){
    fprintf(stderr, "Failed to run dbstats command.\n");
    return false;
  }
  if(vn_start > 0){
    printf("Downloading VNs\n");
    if(vndb.download_and_insert_all(vndb::object_type::VN,vn_start) <= 0){ return false; }
  }
  if(release_start > 0){
    printf("Downloading Releases\n");
    if(vndb.download_and_insert_all(vndb::object_type::release,release_start) <= 0){ return false; }
  }
  if(producer_start > 0){
    printf("Downloading Producers\n");
    if(vndb.download_and_insert_all(vndb::object_type::producer,producer_start)<= 0){ return false; }
  }
  if(character_start > 0){
    printf("Downloading Characters\n");
    if(vndb.download_and_insert_all(vndb::object_type::character,character_start) <= 0){ return false; }
  }
  if(staff_start > 0){
    printf("Downloading Staff\n");
    if(vndb.download_and_insert_all(vndb::object_type::staff,staff_start) <= 0){ return false; }
  }
  return true;
}
int main(int argc, char* argv[]){
  //Keep the old log file, I may extend this to keep the last N log files.
  rename(default_log_file, default_log_file_bkup);
  vndb_log = std::make_unique<util::logger>(default_log_file, util::log_level::debug);
  if(!init_vndb_ssl_ctx()){
    fprintf(stderr, "Error, failed to initialize ssl context\n");
    return -1;
  }
  atexit(free_vndb_ssl_ctx);
  util::array<int, vndb_main::num_base_tables> start_indexes(1);
  if(argc > 1){
    for(int i = 1; i < argc; i++){
      start_indexes.push_back(strtol(argv[i], nullptr, 10));
    }
  }
  while(start_indexes.capacity() > 0){
    start_indexes.push_back(1);
  }
  //Temporary, skips VNs, releases and producers.
  start_indexes[0] = start_indexes[1] = start_indexes[2] = start_indexes[3] = -1;
  bool success = download_all(default_db_file, start_indexes[0], start_indexes[1],
                              start_indexes[2], start_indexes[3], start_indexes[4]);
  return (success ? EXIT_SUCCESS : EXIT_FAILURE);
/*
  if(argc > 1){
    if(argv[1][0] == 'c'){
      return run_connection_test();
    } else {
      return run_insertion_test();
    }
  }
  return run_insertion_test();
*/
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
#endif
