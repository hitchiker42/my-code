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
#include "json.hpp"
#include "logger.h"

extern SSL_CTX* vndb_ctx;
bool init_vndb_ssl_ctx();
void free_vndb_ssl_ctx();

//This needs to be defined and initialized in whatever file has main.
extern std::unique_ptr<util::logger> vndb_log;
static constexpr const char* default_log_file = "vndb.log";

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

//structure holding the main program context.
struct vndb_main {
  enum class table_type {
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
    //Auxiliary tables, for now just tables to hold image data.
    vn_images,
    character_images,
    num_tables_total = character_images + 1
  };
  static constexpr int num_base_tables = static_cast<int>(table_type::num_base_tables);
  static constexpr int num_tables_total = static_cast<int>(table_type::num_tables_total);
  static constexpr std::array<const char *,num_tables_total> table_names = {{
      "VNs","releases", "producers", "characters", "staff", "tags", "traits",
      "vn_producer_relations", "vn_character_actor_relations", "vn_staff_relations",
      "vn_tags", "character_traits", "vn_images", "character_images"
    }};
  sqlite3_wrapper db;
  vndb_connection conn;  
  //result of the dbstats command from the server, used for progress bars
  json db_stats;
  //These are not automatically initialized, you need to explicitly initialize
  //them, this is to avoid the overhead of creating prepared statments you
  //may never use.
  std::array<sqlite3_stmt_wrapper, num_base_tables> get_by_id_stmts = {{}};
  std::array<sqlite3_stmt_wrapper, num_tables_total> insert_stmts = {{}};
  vndb_main(std::string_view db_filename,
            std::string_view username = "", std::string_view passwd = "", 
            bool connect = false)
    : db(db_filename), conn(username, passwd, true, connect) {}
  bool init_insert_stmts();
  bool init_get_id_stmts();
  bool init_db_stats(){
    if(!conn.ensure_logged_in()){
      return false;
    } else {
      db_stats = conn.dbstats();
      return !db_stats.is_null();
    }
  }
  //Two names for the same function.
  bool connect(){
    return conn.login();
  }
  bool login(){
    return conn.login();
  }
  bool init_db(){
    int err = db.exec_file(db_init_file);
    if(err != SQLITE_OK){
      if(err != SQLITE_CANTOPEN){
        db.print_errmsg("Error initializing datbase");
      }
      return false;
    }
    return true;
  }
  //function to access db_stats via table type, since the names of the tables
  //don't all match up to the names in db_stats.
  int db_stats_count(vndb::object_type what){
    return db_stats_count(table_type(to_underlying(what)));
  }
  int db_stats_count(table_type what){
    assert(to_underlying(what) < num_base_tables);
    switch(what){
      case table_type::VNs: return db_stats["vn"].get<int>();
      case table_type::characters: return db_stats["chars"].get<int>();
      default: return db_stats[table_names[to_underlying(what)]].get<int>();
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
        if(sqlite_insert_object(what, item, ins_stmt) != SQLITE_OK){
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
    progress_bar pb(db_stats_count(what), 
                    vndb::object_type_names[to_underlying(what)].data());
    auto callback = gen_insert_callback(what, &pb);
    return conn.get(what, start, stop, callback);
  }
  int download_and_insert_all(vndb::object_type what){
    progress_bar pb(db_stats_count(what), 
                    vndb::object_type_names[to_underlying(what)].data());
    auto callback = gen_insert_callback(what, &pb);
    return conn.get_all(what, callback);
  }
  int get_max_id(table_type what){
    static constexpr int bufsz = 256;
    char buf[bufsz];
    snprintf(buf, bufsz, "select max(id) from %s;", table_names[to_underlying(what)]);
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
