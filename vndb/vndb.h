#ifndef __VNDB_H__
#define __VNDB_H__
//standard library headers
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include <unistd.h>
#include <stdio.h>
#include <string>
#include <vector>
#include <string_view>
//external library headers
#include <openssl/ssl.h>
#include <openssl/bio.h>
#include <openssl/err.h>
#include <sqlite3.h>
//I assume SQLITE_OK == 0 at several points in my code, so make sure it is.
static_assert(SQLITE_OK == 0);
//local headers.
#include "util.h"
#include "string_buf.h"
#include "filesystem.h"
#include "progress_bar.h"
#include "image.h"
#include "json.hpp"
#include "logger.h"

extern SSL_CTX* vndb_ctx;
bool init_vndb_ssl_ctx();
void free_vndb_ssl_ctx();

//This needs to be defined and initialized in whatever file has main.
//It should be initialized before anything else.
extern std::unique_ptr<util::logger> vndb_log;
static constexpr const char* default_log_file = "vndb.log";
static constexpr const char* default_log_file_bkup = "vndb.log.bkup";

using string_buf = util::string_buf;

//json aliases
namespace json_ns = nlohmann;
using json = json_ns::basic_json<>;
using json_parser = json_ns::detail::parser<json>;
//json null constant, for convience.
static const json json_null = json(json::value_t::null);
//Constants for the hostname & ports
static constexpr const char* vndb_port_number = "19534";
static constexpr const char* vndb_tls_port_number = "19535";
static constexpr const char* vndb_hostname = "api.vndb.org";
//default name for the database file
static constexpr const char* default_db_file = "vn.db";
//file of sql code used to initialize the database tables.
static constexpr const char* db_init_file = "database_init.sql";
namespace vndb {
//Possible targets of the get command
enum class object_type {
  VN = 0,
  release,
  producer,
  character,
  staff
};
constexpr int num_object_types = to_underlying(object_type::staff)+1;
static constexpr std::array<std::string_view, num_object_types>
object_type_names = {{"VNs", "Releases", "Producers", "Characters", "Staff"}};
}
/*
struct SSL_CTX_wrapper {
  SSL_CTX *ctx = nullptr;
  //Default constructor doesn't do anything, since I need a static
  //one of these.
  SSL_CTX_wrapper() = default;
  //There's no reason to ever pass a false argument to this,
  //it just takes an argument to differentate it from the default
  //constructor.
  SSL_CTX_wrapper(bool init){
    if(init){
      this->init();
    }
  }
  ~SSL_CTX_wrapper(){
    SSL_CTX_free(ctx);
  }
  bool init();
}
static SSL_CTX_wrapper vndb_ctx;
*/
//SSL wrappers and a struct dealing with the connecting to the vndb server.
#include "connection.h"
//wrappers for sqlite statements and database connections
#include "sqlite_wrappers.h"
//functions to download and parse tags and traits.
int download_and_insert_tags(sqlite3_wrapper& db);
int download_and_insert_traits(sqlite3_wrapper& db);

//structure holding the main program context.
struct vndb_main {
  enum class table_type : int8_t {
    //Base tables, hold the actual information in the database
    VNs,
    releases,
    producers,
    characters,
    staff,
    tags,
    traits,
    num_base_tables = traits + 1,
    //derived tables, define relations between the base tables.
    vn_producer_relations = num_base_tables,
    vn_character_actor_relations,
    vn_staff_relations,
    staff_aliases,
    vn_tags,
    character_traits,
    //Auxiliary tables image data & vn/wishlist
    //TODO: Maybe move these to before derived tables.
    vnlist,
    //not actually a table but part of the vnlist table, however since
    //it's stored seperately on the server we need to have a seperate insert statement.
    votelist,
    wishlist,
    vn_images,
    character_images,
    num_tables_total = character_images + 1
  };
  static constexpr int num_base_tables = static_cast<int>(table_type::num_base_tables);
  static constexpr int num_tables_total = static_cast<int>(table_type::num_tables_total);
  static constexpr std::array<std::string_view,num_tables_total> table_names = {{
      "VNs"sv,"releases"sv, "producers"sv, "characters"sv, "staff"sv, 
      "tags"sv, "traits"sv,"vn_producer_relations"sv, 
      "vn_character_actor_relations"sv, "vn_staff_relations"sv, "vn_tags"sv,
      "character_traits"sv, "vnlist"sv, "votelist"sv, "wishlist"sv, 
      "vn_images"sv, "character_images"sv      
    }};
  static std::unordered_map<std::string_view, table_type> table_name_map;
  bool is_derived_table(table_type tt){
    auto ttv = util::to_underlying(tt);
    return (ttv >= num_base_tables && ttv < util::to_underlying(table_type::vn_images));
  }
  sqlite3_wrapper db;
  vndb_connection conn;
  //result of the dbstats command from the server, used for progress bars
  json db_stats = json_null;
  //information about local database.
  json db_info = json_null;
  //Booleans to indicate we've already initialized something.
  //Could use a bitfield but I really don't need to worry about space that much.
  //Could alsa make these atomic.
  bool get_by_id_stmts_initialized = false;
  bool insert_stmts_initialized = false;
  bool db_initialized = false;
  //These are not automatically initialized, you need to explicitly initialize
  //them, this is to avoid the overhead of creating prepared statments you
  //may never use.
  std::array<sqlite3_stmt_wrapper, num_base_tables> get_by_id_stmts = {{}};
  std::array<sqlite3_stmt_wrapper, num_tables_total> insert_stmts = {{}};
  //Open the databas in db_filename and create a connection with the given
  //username and password, but don't actually connect yet.
  vndb_main(std::string_view db_filename,
            std::string_view username = "", std::string_view passwd = "",
            bool connect = false)
    : db(db_filename), conn(username, passwd, true, connect) {
    if(!db){
      fprintf(stderr, "Failed to open database file %s.\n",
              db_filename.data());
    }
  }
  bool init_insert_stmts();
  bool init_get_id_stmts();
  //get_dbstats defaults to true, but it's only meaningful if
  //do_connect is also true.
  bool init_all(bool do_connect = false, bool get_dbstats = true){
    if(!init_db()){
      return false;
    }
    if(!init_insert_stmts()){
      fprintf(stderr, "Failed to compile sql insert statements.\n");
      return false;
    }
    if(!init_get_id_stmts()){
      fprintf(stderr, "Failed to compile sql get_by_id statements.\n");
      return false;
    }
    if(!init_db_info()){
      fprintf(stderr, "Failed to initialize db_info.\n");
      return false;
    }
    if(!init_table_name_map()){
      fprintf(stderr, "Failed to initialize table_name_map.\n");
      return false;
    }
    if(do_connect){
      if(!connect()){
        fprintf(stderr, "Failed to connect to vndb server.\n");
        return false;
      }
      if(get_dbstats){
        if(!init_db_stats()){
          fprintf(stderr, "Failed to run dbstats command.\n");
          return false;
        }
      }
    }
    return true;
  }
  //I'd like to replace this with a bitset I've written but this should work for now.
  using bitvector = std::vector<bool>;
  bitvector build_missing_ids_bitvector(sqlite3_stmt_wrapper& stmt,
                                        int size);
  bool build_vn_tags();
  bool build_character_traits();
  //uses the releases table
  bool build_vn_producer_relations();
  //builds vn_character_actor_relations, vn_staff_relations and staff_aliases.
  bool build_staff_derived_tables();
  //convience function to run the previous 4 functions in one call.
  //returns 0 on success and 1-4 if it fails indicating which function
  //it was that faild.
  int build_derived_tables();
  bool build_vn_images();
  bool build_character_images();
  bool update_vn_images();
  bool update_character_images();
  bool init_table_name_map(){
    if(!table_name_map.empty()){
      return true;
    } else {
      for(int i = 0; i < num_tables_total; i++){
        table_name_map.try_emplace(table_names[i], table_type(i));
      }
      return true;
    }
  }
  bool init_db_stats(){
    if(!db_stats.is_null()){
      return true;
    }
    if(!conn.ensure_logged_in()){
      return false;
    } else {
      db_stats = conn.dbstats();
      return !conn.error;
    }
  }
  bool init_db_info(){
    if(!db_info.is_null()){
      return true;
    }
    auto stmt = db.prepare_stmt("select * from db_info;");
    if(stmt.step() != SQLITE_ROW){
      return false;
    }
    json tmp = stmt.get_row_json();
    if(stmt.step() != SQLITE_DONE){
      return false;
    }
    if(tmp.is_null()){
      return false;
    }
    db_info = std::move(tmp);
    return true;
  }
  std::pair<int,int> get_table_info(table_type what){
    switch(what){
      case table_type::VNs:
        return {db_info["num_vns"].get<int>(),
          db_info["max_vn_id"].get<int>()};
      case table_type::releases:
        return {db_info["num_releases"].get<int>(),
          db_info["max_release_id"].get<int>()};
      case table_type::producers:
        return {db_info["num_producers"].get<int>(),
          db_info["max_producer_id"].get<int>()};
      case table_type::characters:
        return {db_info["num_characters"].get<int>(),
          db_info["max_character_id"].get<int>()};
      case table_type::staff:
        return {db_info["num_staff"].get<int>(),
          db_info["max_staff_id"].get<int>()};
      case table_type::tags:
        return {db_info["num_tags"].get<int>(),
          db_info["max_tag_id"].get<int>()};
      case table_type::traits:
        return {db_info["num_traits"].get<int>(),
          db_info["max_trait_id"].get<int>()};
      default:
        assert(false);
    }
  }
  //Two names for the same function.
  bool connect(bool get_dbstats = false){
    if(!conn.ensure_logged_in()){
      return false;
    }
    if(get_dbstats){
      return init_db_stats();
    } else {
      return true;
    }    
  }
  bool login(bool get_dbstats = false){
    return connect(get_dbstats);
  }
  bool init_db(){
    if(db_initialized){
      return true;
    }
    //If the database couldn't be opened the constructor should've issued
    //a message already so just return false.
    if(!db){
      return false;
    }
    int err = db.exec_file(db_init_file);
    if(err != SQLITE_OK){
      if(err != SQLITE_CANTOPEN){
        db.print_errmsg("Error initializing datbase");
      }
      return false;
    }
    db_initialized = true;
    return true;
  }
  //function to access db_stats via table type, since the names of the tables
  //don't all match up to the names in db_stats.
  int db_stats_count(vndb::object_type what){
    return db_stats_count(table_type(to_underlying(what)));
  }
  int db_stats_count(table_type what){
    if(db_stats.is_null()){
      vndb_log->log_warn("Warning accessing dbstats without "
                         "having called init_db_stats.\n");
      return -1;
    }
    assert(to_underlying(what) < num_base_tables);
    switch(what){
      case table_type::VNs: return db_stats["vn"].get<int>();
      case table_type::characters: return db_stats["chars"].get<int>();
      default: return db_stats[table_names[to_underlying(what)].data()].get<int>();
    }
  }
  json get_by_id(int id, table_type what){
    auto& stmt = get_by_id_stmts[to_underlying(what)];
    stmt.bind(1, id);
    json ret = json_null;
    if(stmt.step()){
      ret = stmt.get_row_json();
      if(stmt.step()){
        fprintf(stderr, "Error multiple rows returned from select by id statement.");
        ret = json_null;
      }
    }
    stmt.reset();
    return ret;
  }
  int insert(json object, table_type what);
  int update_db_info(){
    return db.exec_file("update_db_info.sql");
  }
  sqlite3_wrapper& get_db(){
    return db;
  }
  vndb_connection& get_connection(){
    return conn;
  }
  //not called get_get_by_id_stmt, because that sounds really silly.
  sqlite3_stmt_wrapper& get_select_by_id_stmt(table_type what){
    assert(to_underlying(what) > num_base_tables);
    return get_by_id_stmts[to_underlying(what)];
  }
  sqlite3_stmt_wrapper& get_insert_stmt(table_type what){
    return insert_stmts[to_underlying(what)];
  }
  std::function<int(std::vector<json>&)> gen_insert_callback(vndb::object_type what,
                                                             struct progress_bar *pb){
    auto &ins_stmt = get_insert_stmt(table_type(to_underlying(what)));
    auto &db = this->db;
    auto callback = [&ins_stmt, &db, pb, what](std::vector<json>& items) mutable -> int {
      if(db.begin_transaction() != SQLITE_OK){ return -1; }
      for(auto &&item : items){
        int err = sqlite_insert_object(what, item, ins_stmt);
        if(err != SQLITE_OK){
          db.print_errmsg("Error in callback.");
          fprintf(stderr, "Sql command was %s.\n", ins_stmt.get_sql().data());
          db.rollback_transaction();
          return -1;
        }
      }
      //SQLITE_OK == 0, so this will return nonzero on error
      int ret = db.commit_transaction();
      pb->update(items.size());
      pb->display();
      return ret;
    };
    return callback;
  }
  std::function<int(std::vector<json>&)> gen_insert_callback(table_type what,
                                                             progress_bar *pb){
    assert(to_underlying(what) <= num_base_tables);
    return gen_insert_callback(vndb::object_type(to_underlying(what)),pb);
  }

  int download_and_insert(vndb::object_type what, int start, int stop){
    progress_bar pb(db_stats_count(what) - start,
                    vndb::object_type_names[to_underlying(what)].data());
    auto callback = gen_insert_callback(what, &pb);
    return conn.get(what, start, stop, callback);
  }
  //If start == -1 then do an update. If start == 0 ignore it.
  int download_and_insert_all(vndb::object_type what, int start = 1){
    //Ugly code duplication (due to the progress bar), but the only way I
    //see avoiding it is using some form of new, which is even more ugly.
    if(start > 0){
      progress_bar pb(db_stats_count(what) - start,
                      vndb::object_type_names[to_underlying(what)].data());
      auto callback = gen_insert_callback(what, &pb);
      return conn.get_all(what, callback, start);
    } else if(start < 0) {
      auto [count, max_id] = get_table_info(table_type(to_underlying(what)));
      progress_bar pb(db_stats_count(what) - count,
                      vndb::object_type_names[to_underlying(what)].data());
      auto callback = gen_insert_callback(what, &pb);
      return conn.get_all(what, callback, max_id);
    } else {
      //This is a lie, but a 0 return is usually interpreted as an error.
      return 1;
    }
  }
  //Downloads all objects that have been added to the online database since
  //the local database was last updated.
  bool update_all(){
    return download_all(-1,-1,-1,-1,-1);
  }
  //Passing -1 as start will cause an update for that object type, passing
  //0 for start will skip that object type.
  bool download_all(int vn_start = 1, int release_start = 1,
                    int producer_start = 1, int character_start = 1,
                    int staff_start = 1){
    connect(true);//make sure we're logged in & have dbstats info.
    if(vn_start != 0){
      //printf("Downloading VNs\n");
      if(download_and_insert_all(vndb::object_type::VN, vn_start) <= 0){
        return false;
      }
    }
    if(release_start != 0){
      //printf("Downloading Releases\n");
      if(download_and_insert_all(vndb::object_type::release,release_start) <= 0){
        return false;
      }
    }
    if(producer_start != 0){
      //printf("Downloading Producers\n");
      if(download_and_insert_all(vndb::object_type::producer,producer_start) <= 0){
        return false;
      }
    }
    if(character_start != 0){
      //printf("Downloading Characters\n");
      if(download_and_insert_all(vndb::object_type::character,character_start) <= 0){
        return false;
      }
    }
    if(staff_start != 0){
      //printf("Downloading Staff\n");
      if(download_and_insert_all(vndb::object_type::staff, staff_start) <= 0){
        return false;
      }
    }
    return true;
  }
  int get_max_id(table_type what){
    static constexpr int bufsz = 256;
    char buf[bufsz];
    snprintf(buf, bufsz, "select max(id) from %s;", 
             table_names[to_underlying(what)].data());
    auto stmt = db.prepare_stmt(buf);
    if(!stmt){
      return -1;
    }
    if(!stmt.step()){
      return -1;
    }
    int max = stmt.get_column<int>(1);
    if(stmt.step()){
      return max;
    } else {
      fprintf(stderr, "Error multiple rows returned for get_max_id.\n");
      return -1;
    }
  }
};
//Ensure that the values of vndb_main::table_type and vndb::object_type line up.
static_assert(to_underlying(vndb::object_type::VN) ==
              to_underlying(vndb_main::table_type::VNs));
static_assert(to_underlying(vndb::object_type::release) ==
              to_underlying(vndb_main::table_type::releases));
static_assert(to_underlying(vndb::object_type::producer) ==
              to_underlying(vndb_main::table_type::producers));
static_assert(to_underlying(vndb::object_type::character) ==
              to_underlying(vndb_main::table_type::characters));
static_assert(to_underlying(vndb::object_type::staff) ==
              to_underlying(vndb_main::table_type::staff));

namespace vndb {
using string = std::string;
using date = string;
template<typename T>
using vector = std::vector<T>;
/*enum class object_type {
  VN,
  release,
  producer,
  character,
  staff,
  tag,
  trait
};*/
//Things stored here as json could be changed to use a more specific datatype
struct VN {
  VN() = default;
  VN(json);
  VN(sqlite3_stmt_wrapper&);
  //Values obtained from vndb get vn command.
  int id;
  string title;
  string original;// = "";
  date released;
  //Could probably get rid of these next 3
  json languages;
  json orig_lang;
  json platforms;
  string aliases;// = "";
  int length;// = 0;
  string description;// = "";
  json links;//{json::value_t::object};//could probably remove.
  string image;// = "";
  bool image_nsfw;
  json anime;//{json::value_t::array};//could probably remove.
  json relations;//{json::value_t::array};
  json tags;//{json::value_t::array};
  int popularity;
  int rating;
  int votecount;
  json screens;//{json::value_t::array};
  json staff;//{json::value_t::array};
  //Values from other vndb get commands.
  //Stored in SQL table as json arrays.
  vector<int> releases;
  vector<int> producers;
  vector<int> characters;
  //User specific info
  //null or array of ["wishlist"/"vnlist", priority/rating, date added]
  json list_info;
  //Extra info
  date last_cached;
  int times_accessed;// = 0;
};
//For my purposes releases aren't really that important, but
//they are the only way to connect vns and producers using the vndb api.
struct Release {
  int id;
  string title;
  string original_title = "";
  date released;
  vector<int> vns;
  vector<int> producers;
};

struct Producer {
  int id;
  string name;
  string original = "";
  string type; //amateur/profesional
  //string language; //primary language
  //json links; //link to homepage and/or wikipedia, or neither
  vector<string> aliases; //maybe change to json
  //string description.
  json relations{json::value_t::object};//related producers
};
struct Character {
  //There are lots of details that vndb stores we don't really care about
  //like birthday, blood type and measurements
  int id;
  string name;
  string original_name = "";
  string gender; //enum of male, female, both;
  vector<string> aliases;
  string description;
  string image_link;
  json traits{json::value_t::object};
  json vns{json::value_t::array};
  json voice_actors{json::value_t::array};
};
struct Staff {
  int id;
  string name;
  string original_name;
  json aliases{json::value_t::array};
  json vns{json::value_t::array};
  json characters{json::value_t::array};
};
}
/*
struct Tag {
  int id;
  string name;
  string description;
  bool is_meta;
  int vn_count;
  string category;
  //stored in sql table as json arrays.
  vector<string> aliases;
  vector<int> parents;
};
struct Trait {

struct vndb_object {
  object_type type;
  union {
    VN *vn;
    Release *release;
    Producer *producer;
    Character *character;
    Staff *staff;
  };

}
*/
#endif /* __VNDB_H__ */
