#include "vndb.h"
#include "sql.h"
#include <chrono>
//Even though we include this we don't need to link with pthreads since
//we don't actually use any thread functions.
#include <thread>
namespace util {
static void sleep(double seconds){
  std::chrono::duration<double> dur(seconds);
  std::this_thread::sleep_for(dur);
}
}
//std::unique_ptr<util::logger> vndb_log;
std::unordered_map<std::string_view, vndb_main::table_type> vndb_main::table_name_map;

bool vndb_main::init_insert_stmts(){
  static constexpr std::array<std::string_view, this->num_tables_total> sql = {{
      sql_insert_vn, sql_insert_release, sql_insert_producer,
      sql_insert_character, sql_insert_staff, sql_insert_tag, sql_insert_trait,
      sql_insert_vn_producer_relation, sql_insert_vn_character_actor_relation,
      sql_insert_vn_staff_relation, sql_insert_staff_alias,
      sql_insert_vn_tag, sql_insert_character_trait,
      sql_insert_vnlist_entry, sql_insert_votelist_entry,
      sql_insert_wishlist_entry, sql_insert_vn_image, sql_insert_character_image
    }};
  if(this->insert_stmts_initialized){
    return true;
  }
  for(int i = 0; i < this->num_tables_total; i ++){
    //prepare statement with a hint that we'll be keeping the statement object
    //around for a while.
    sqlite3_stmt *ptr = this->db.prepare_stmt_ptr(sql[i], true);
    if(!ptr){
      if(this->db.errcode() == SQLITE_OK){
        fprintf(stderr, "Missing sql insert statement for table %s.\n",
                this->table_names[i].data());
        return false;
      } else {
        this->db.print_errmsg("Failed to compile statement");
        fprintf(stderr, "%s\n", sql[i].data());
        return false;
      }
    }
    this->insert_stmts[i].stmt = ptr;
  }
  this->insert_stmts_initialized = true;
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
  if(this->get_by_id_stmts_initialized){
    return true;
  }
  for(int i = 0; i < this->num_base_tables; i ++){
    sqlite3_stmt *ptr = this->db.prepare_stmt_ptr(sql[i], true);
    if(!ptr){
      return false;
    }
    this->get_by_id_stmts[to_underlying(types[i])].stmt = ptr;
  }
  this->get_by_id_stmts_initialized = true;
  return true;
}
vndb_main::bitvector vndb_main::build_missing_ids_bitvector(sqlite3_stmt_wrapper& stmt,
                                                 int size){
  bitvector ret(size, 0);
  int res;
  while((res = stmt.step()) == SQLITE_ROW){
    int id = stmt.get_column<int>(0);
    ret[id] = 1;
  }
  if(res != SQLITE_DONE){
    //return an empty bitvector on error
    return bitvector(0);
  }
  stmt.reset();
  return ret;
}

//This is (for me) a pretty big function, but trying to break it into
//multiple functions just makes it harder to understand.
bool vndb_main::build_vn_tags(){
  sqlite3_wrapper &db = this->db;
  auto stmt = db.prepare_stmt("select id, tags from VNs;");
  if(!stmt){
    db.print_errmsg("Failed to compile sql");
    return false;
  }
  auto missing_ids_stmt = db.prepare_stmt(sql_find_missing_tags);
  if(!missing_ids_stmt){
    db.print_errmsg("Failed to compile sql");
    return false;
  }
  //VNs can be tagged with tags that haven't been approved yet, so we need to
  //pass the maximum tag id to the insert function so we can ignore them.
  int max_tag_id = this->db_info["max_tag_id"].get<int>();
  //VNs can be tagged with deleted tags, so we need to know which tags
  //don't actually exist, we use a bitvector for this. (bitvector is
  //a typedef in the struct, currently for std::vector<bool>).
  bitvector missing_tags = this->build_missing_ids_bitvector(missing_ids_stmt,
                                                             max_tag_id + 1);
  if(missing_tags.empty()){
    return false;
  }
  auto &ins_stmt = this->get_insert_stmt(vndb_main::table_type::vn_tags);
  db.begin_transaction();
  int res;
  struct progress_bar pb(this->db_info["num_vns"].get<int>(), "vn_tags");
  pb.display();
  while((res = stmt.step()) == SQLITE_ROW){
    int vn_id = stmt.get_column<int>(0);
    json tags = stmt.get_column<json>(1);
    if(tags.empty()){ 
      ++pb;
      continue; 
    }
    ins_stmt.bind(1, vn_id);
    //vndb_log->log_debug("Inserting tags for vn %d.\n", vn_id);
    //the code below used to be in a seperate function, but I had to pass it
    //a ton of parameters than it ended up being eaiser just to put it here.
    //tags is an array of [id, score, spoiler_level];
    for(auto &&tag : tags){
      int tag_id = tag[0].get<int>();
      if((tag_id > max_tag_id) || (missing_tags[tag_id])){
        vndb_log->log_debug("Skipping deleted tag %d for vn %d.\n",
                            tag_id, vn_id);
        continue;
      }
      int tag_score = tag[1].get<double>();
      ins_stmt.bind(2, tag_id);
      ins_stmt.bind(3, tag_score);
      int err = ins_stmt.exec();
      if(err != SQLITE_OK){
        db.print_errmsg("Failed to insert into vn_tags");
        vndb_log->log_warn("Failed to insert tag %d for vn %d\n",
                           tag_id, vn_id);
        db.rollback_transaction();
        return false;
      }
    }
    ins_stmt.reset_bindings();
    ++pb;
  }
  if(res != SQLITE_DONE){
      db.print_errmsg("Failure running 'select id,tags from VNs;'");
      db.rollback_transaction();
      return false;
  }
  pb.finish();
  res = db.commit_transaction();
  return (res == SQLITE_OK);
}
bool vndb_main::build_character_traits(){
  sqlite3_wrapper &db = this->db;
  auto stmt = db.prepare_stmt("select id, traits from characters;");
  if(!stmt){
    db.print_errmsg("Failed to compile sql");
  }
  auto missing_ids_stmt = db.prepare_stmt(sql_find_missing_traits);
  if(!missing_ids_stmt){
    db.print_errmsg("Failed to compile sql");
    return false;
  }
  int max_trait_id = this->db_info["max_trait_id"].get<int>();
  //Same as with tags deleted traits can still be attached to characters so we
  //need to identify them so we can skip them.
  bitvector missing_traits = this->build_missing_ids_bitvector(missing_ids_stmt,
                                                               max_trait_id + 1);
  if(missing_traits.empty()){
    return false;
  }
  struct progress_bar pb(this->db_info["num_characters"].get<int>(), "character_traits");
  auto &ins_stmt = this->get_insert_stmt(vndb_main::table_type::character_traits);
  db.begin_transaction();
  int res;
  pb.display();
  while((res = stmt.step()) == SQLITE_ROW){
    int character_id = stmt.get_column<int>(0);
    json traits = stmt.get_column<json>(1);
    if(traits.empty()){ 
      ++pb;
      continue; 
    }
    ins_stmt.bind(1, character_id);
    //traits is an array of [id, spoliler_level]
    for(auto &&trait : traits){
      int trait_id = trait[0].get<int>();
      if((trait_id > max_trait_id) || (missing_traits[trait_id] != 0)){
        vndb_log->log_debug("Skipping deleted trait %d for character %d.\n",
                            trait_id, character_id);
        continue;
      }
      ins_stmt.bind(2, trait_id);
      int err = ins_stmt.exec();
      if(err != SQLITE_OK){
        db.print_errmsg("Failed to insert into character_traits");
        vndb_log->log_warn("Failed to insert trait %d for character %d.\n",
                           trait_id, character_id);
        db.rollback_transaction();
        return false;
      }
    }
    ins_stmt.reset_bindings();
    ++pb;
  }
  if(res != SQLITE_DONE){
      db.print_errmsg("Failure running 'select id,traits from characters;'");
      db.rollback_transaction();
      return false;
  }
  pb.finish();
  res = db.commit_transaction();
  return (res == SQLITE_OK);
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
  struct progress_bar pb(this->db_info["num_releases"].get<int>(), "vn_producer_relations");
  db.begin_transaction();
  int res;
  while((res = stmt.step()) == SQLITE_ROW){
    int id = stmt.get_column<int>(0);
    json vns = stmt.get_column<json>(1);
    json producers = stmt.get_column<json>(2);
    int err = sqlite_insert_vn_producer_relations(id, vns, producers, ins_stmt);
    if(err != SQLITE_OK){
      db.print_errmsg("Failed to insert into vn_producer_relations");
      vndb_log->log_warn("Failed to insert relation for release %d,"
                         "vns = %s, producers = %s.\n",
                         id, vns.dump().c_str(), producers.dump().c_str());
      db.rollback_transaction();
      return false;
    }
    ++pb;
  }
  if(res != SQLITE_DONE){
      db.print_errmsg("failure running 'select id, vn, producers from releases;'");
      db.rollback_transaction();
      return false;
  }
  pb.finish();
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
  struct progress_bar pb(this->db_info["num_staff"].get<int>(), "staff_tables");
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
    ++pb;
  }
  if(res != SQLITE_DONE){
      db.print_errmsg("Failure running 'select id, aliases, vns, voiced from staff;'");
      db.rollback_transaction();
      return false;
  }
  pb.finish();
  return (db.commit_transaction() == SQLITE_OK);
}
static bool build_image_table(sqlite3_wrapper &db,
                              sqlite3_stmt_wrapper &stmt,
                              sqlite3_stmt_wrapper &ins_stmt,
                              progress_bar *pb){
  http_connection conn("s.vndb.org", "443");
  db.begin_transaction();
  int res, cnt = 0;
  util::svector<char> buf;
  pb->display();
  while((res = stmt.step()) == SQLITE_ROW){
    int id = stmt.get_column<int>(0);
    std::string_view uri = stmt.get_column<std::string_view>(1);
    int err = conn.http_get(uri, buf);
    //If we can't read from the bio, wait a bit and retry.
    if(err == -1){      
      vndb_log->log_debug("Error reading from bio, waiting then retrying.");
      int wait = 15;
      while(err == -1){
        wait *= 2;
        vndb_log->log_debug("Waiting %d seconds.\n", wait);
        util::sleep(wait);
        vndb_log->log_debug("Trying to reconnect.\n", wait);
        if(!conn.reconnect()){
          fprintf(stderr,"Couldn't reconnect.\n");
          return false;          
        }
        vndb_log->log_debug("Reconnected.\n");
        err = conn.http_get(uri, buf);
        vndb_log->log_debug("Retried request got %d response.\n", err);
      }
    }
    if(err != 0){
      fprintf(stderr, "Error downloding https://s.vndb.org%s.\nGot response %d.\n",
              uri.data(), err);
      db.rollback_transaction();
      return false;
    }
    ins_stmt.bind(1, id);
    ins_stmt.bind(2, (void*)buf.data(), buf.size());
    err = ins_stmt.exec();
    if(err != SQLITE_OK){
      fprintf(stderr, "Error inserting %s.\n", pb->title);
      db.rollback_transaction();
      return false;
    }
    //Every 128 images commit the transaction & reset the connection.
    if((++cnt & 0x7f) == 0){
      db.commit_transaction();
      conn.reconnect();
      db.begin_transaction();
    }
    pb->update(1);
  }
  if(res != SQLITE_DONE){
    fprintf(stderr, "Failure running '%s' : %s(%d).",
            stmt.get_sql_template().data(), db.errmsg(), db.errcode());
    db.rollback_transaction();
    return false;
  }
  return (db.commit_transaction() == SQLITE_OK);
}
int vndb_main::build_derived_tables(){
  printf("Bulding vn_tags table.\n");
  if(!this->build_vn_tags()){
    return 1;
  }
  printf("Bulding character_traits table.\n");
  if(!this->build_character_traits()){
    return 2;
  }
  printf("Bulding vn_producer_relations table.\n");
  if(!this->build_vn_producer_relations()){
    return 3;
  }
  printf("Bulding staff derived tables.\n");
  if(!this->build_staff_derived_tables()){
    return 4;
  }
  return 0;
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
  auto stmt = db.prepare_stmt(
    "select id, substr(image, 19) from VNs where image is not null");
  if(!stmt){
    db.print_errmsg("Failed to compile sql");
  }
  auto &ins_stmt = this->get_insert_stmt(vndb_main::table_type::vn_images);

  progress_bar pb(db_info["num_vns"].get<int>(), "VN images");
  return build_image_table(db, stmt, ins_stmt, &pb);
}
bool vndb_main::build_character_images(){
  sqlite3_wrapper &db = this->db;
  //substr(image,19) cuts the leading "https://s.vndb.org" from the link
  auto stmt = db.prepare_stmt(
    "select id, substr(image, 19) from characters where image is not null");
  if(!stmt){
    db.print_errmsg("Failed to compile sql");
  }
  auto &ins_stmt = this->get_insert_stmt(vndb_main::table_type::vn_images);
  progress_bar pb(db_info["num_characters"].get<int>(), "VN images");
  return build_image_table(db, stmt, ins_stmt, &pb);
}
static int get_image_count(sqlite3_wrapper &db, const char *what){
  char buf[256];
  snprintf(buf, 256, "select count(*) from %s_images where image is not null;", what);
  auto stmt = db.prepare_stmt(buf);
  if(!stmt){
    db.print_errmsg("Failed to compile sql");
    return -1;
  }
  if(stmt.step() != SQLITE_ROW){
    db.print_errmsg("Error executing sql"); 
    return -1;
  }
  int count = stmt.get_column<int>(1);
  if(stmt.step() != SQLITE_DONE){
    vndb_log->log_warn("Too many rows returned when querying image count.\n");
  }
  return count;
}  
bool vndb_main::update_vn_images(){
  sqlite3_wrapper &db = this->db;
  int img_count = get_image_count(db, "vn");
  if(img_count < 0){ return false; }
  //substr(image,19) cuts the leading "https://s.vndb.org" from the link
  auto stmt = db.prepare_stmt(
    R"(select id, substr(image, 19) from VNs
         where image is not null and
               not exists (select vn from vn_images 
                            where vn = VNs.id and image is not null);)");
  if(!stmt){
    db.print_errmsg("Failed to compile sql");
  }
  auto &ins_stmt = this->get_insert_stmt(vndb_main::table_type::vn_images);

  progress_bar pb(db_info["num_vns"].get<int>() - img_count, "VN images");
  return build_image_table(db, stmt, ins_stmt, &pb);
}
bool vndb_main::update_character_images(){
  sqlite3_wrapper &db = this->db;
  int img_count = get_image_count(db, "character");
  if(img_count < 0){ return false; }  
  //substr(image,19) cuts the leading "https://s.vndb.org" from the link
  auto stmt = db.prepare_stmt(
    R"(select id, substr(image, 19) from characters
         where image is not null and
               not exists (select character from character_images 
                            where character = characters.id and image is not null);)");
  if(!stmt){
    db.print_errmsg("Failed to compile sql");
  }
  auto &ins_stmt = this->get_insert_stmt(vndb_main::table_type::vn_images);
  progress_bar pb(db_info["num_characters"].get<int>() - img_count, "character images");
  return build_image_table(db, stmt, ins_stmt, &pb);
}
#ifdef VNDB_CPP_MAIN
int main(int argc, char* argv[]){
  //Keep the old log file, I may extend this to keep the last N log files.
  rename(default_log_file, default_log_file_bkup);
  vndb_log = std::make_unique<util::logger>(default_log_file, util::log_level::debug);
  if(!init_vndb_ssl_ctx()){
    fprintf(stderr, "Error, failed to initialize ssl context\n");
    return -1;
  }
  atexit(free_vndb_ssl_ctx);
  vndb_main vndb(default_db_file);
  // if(!vndb.init_all(true, true)){
  //   return EXIT_FAILURE;
  // }
  // return (vndb.update_all() ? EXIT_SUCCESS : EXIT_FAILURE);

  if(!vndb.init_all()){
    return EXIT_FAILURE;
  }
  int err = vndb.build_derived_tables();
  if(err != 0){
    fprintf(stderr, "Failed to build derived tables error on function %d.\n", err);
  }
  return err;
 // return err;
/*
  if(!vndb.build_vn_producer_relations()){
    fprintf(stderr, "Failed to build vn_producer_relations.\n");
    return EXIT_FAILURE;
  }
  if(!vndb.build_staff_derived_tables()){
    fprintf(stderr, "Failed to build staff derived tables.\n");
    return EXIT_FAILURE;
  }
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
*/
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
#endif
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
