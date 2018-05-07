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
#include <fmt/format.h>
#include <openssl/ssl.h>
#include <openssl/bio.h>
#include <openssl/err.h>
#include <sqlite3.h>
//local headers.
#include "util.h"
#include "string_buf.h"
#include "filesystem.h"
#include "json.hpp"
static SSL_CTX* vndb_ctx;
bool init_vndb_ssl_ctx();
void free_vndb_ssl_ctx();

using string_buf = util::string_buf;

namespace json_ns = nlohmann;
using json = json_ns::basic_json<>;
using json_parser = json_ns::detail::parser<json>;
static const json json_null = json(json::value_t::null);
//Constants for the hostname, ports and login command.
static constexpr const char* vndb_port_number = "19534";
static constexpr const char* vndb_tls_port_number = "19535";
static constexpr const char* vndb_hostname = "api.vndb.org";

struct BIO_wrapper {
  BIO* bio = NULL;
  BIO_wrapper() = default;
  BIO_wrapper(const char* hostname, const char* port,
              SSL_CTX* ctx = vndb_ctx);
  ~BIO_wrapper();
  bool is_encrypted();
  void reset();
  bool connect();
  bool reconnect(){
    reset();
    return connect();
  }
  ssize_t write(std::string_view msg);
  ssize_t read(char *buf, size_t len);
  ssize_t read_delim(util::svector<char> &buf, char delim);
  //  ssize_t read_delim(const char **buf, size_t *bufsz, char delim);
  operator BIO*() const noexcept {
    return bio;
  }
};
//definition of different vndb objects, since we need this in
//vndb_connection.
namespace vndb {
enum class object_type {
  VN = 0,
  release,
  producer,
  character,
  staff
};
}
struct vndb_connection {
  static constexpr char EOT = 0x4; // message terminator 
  BIO_wrapper bio;
  string_buf buf;
  std::string username;
  std::string passwd;
  bool logged_in = false;
  bool error = false;//set to true if server returns an error
  static constexpr const char *hostname = vndb_hostname;
  static constexpr const char *port = vndb_tls_port_number;
  vndb_connection(std::string_view username, std::string_view passwd);
  vndb_connection();

  //both write and write_buf will append an EOT if it is missing
  int write_buf();//write the contents of buf to bio.
  int write(const char *str, size_t len);//write str to bio.
  int read();//synchronous read from bio into buf, reads until EOT.
  //Given a 'get' command in buf, append an appropiate options argument
  //using the given string as the argument to sort, send it to the
  //server, and read the results. If more than one page of results
  //is available repeat until there are no more results left.
  //The value returned from the server is checked for errors.
  //returns number of results, or -1 on error, on error any 
  //results already retrieved are kept.
  int send_get_command(std::vector<json>& results,
                       util::string_view sort_by = "id");
  //Internal helper function for send_get_command.
  json send_get_command_once(int page_no,
                             util::string_view sort_by = "id");
  //assumes an error response is in buf, parses it as json.
  json get_error();
  bool login();
  bool relogin(){
    bool connected = bio.reconnect();
    logged_in = (connected ? login() : false);
    return logged_in;
  }
  //TODO: make get_vns just call this internally.
  //send a get command for the given type to get entries
  //where id>= start && id < stop
  int get(vndb::object_type what, int start, int stop,
          std::vector<json>& results);
  int get(vndb::object_type what, int start,
          std::vector<json>& results){
    return get(what, start, start + 25, results);
  }
  //Get a number of vns from the server starting at a given id
  //and ending at either a given id, or id+25 (which is the max
  //number of vns we can get per response.
  int get_vns(int start, int stop, std::vector<json>& vec);

  int get_vns(int start, std::vector<json>& vec){
    return get_vns(start, start + 25, vec);
  }
  //You need to check this->error after calling this, since on error
  //the error response is stored in the vector.
  std::vector<json> get_vns(int start, int count = 25){
    std::vector<json> ret;
    int err = get_vns(start, start + count, ret);
    return ret;
  }
  json dbstats();
};
struct sqlite3_wrapper {
  sqlite3 *db;
  //Set to the result of the last sqlite function
  int err = SQLITE_OK;
  sqlite3_wrapper(std::string_view filename,
                  int flags = SQLITE_OPEN_READWRITE | SQLITE_OPEN_CREATE){
    err = sqlite3_open_v2(filename.data(), &db, flags, NULL);
  }
  ~sqlite3_wrapper(){
    sqlite3_close(db);
  }
  operator bool(){
    return err != SQLITE_OK;
  }
  operator sqlite3*(){
    return db;
  }
};
struct sqlite3_stmt_wrapper {
  sqlite3_stmt* stmt;
  sqlite3_stmt_wrapper(std::string_view sql, sqlite3* db);
  
};
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
  staff
};*/
//Things stored here as json could be changed to use a more specific datatype
struct VN {
  //Values obtained from vndb get vn command.
  int id;
  string title;
  string original_title = "";
  date released;
  //Could probably get rid of these next 3
  json languages{json::value_t::array};
  json orig_lang{json::value_t::array};
  json platforms{json::value_t::array};
  string aliases = "";
  int length = 0;
  string description = "";
  json links{json::value_t::object};//could probably remove.
  string image_link = "";
  bool image_nsfw;
  json anime{json::value_t::array};//could probably remove.
  json relations{json::value_t::array};
  json tags{json::value_t::array};
  int popularity;
  int rating;
  int votecount;
  json screens{json::value_t::array};
  json staff{json::value_t::array};
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
  int times_accessed = 0;  
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
  string original_name = "";
  string type; //amateur/profesional
  vector<string> aliases; //maybe change to json
  json relations{json::value_t::object};
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
