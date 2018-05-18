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

//struct dealing with SSL details.
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
  staff,
//Not obtained by the api, but rather from complete dumps that are downloaded.
  tag,
  trait
};
constexpr int num_object_types = to_underlying(object_type::trait)+1;
}
//Struct dealing with communication with the vndb server.
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
//being written it C sqlite uses cpp defines for constants, which
//makes sense, but in C++ we have the benifit enums being compile
//time constants, so use that.
enum class sqlite3_type {
  integer = SQLITE_INTEGER,
  floating = SQLITE_FLOAT,
  text = SQLITE_TEXT,
  blob = SQLITE_BLOB,
  null = SQLITE_NULL
};
//struct dealing with generic sqlite connection.
struct sqlite3_wrapper {
  sqlite3 *db;
  //Set to the result of the last sqlite function
  int db_err = SQLITE_OK;
  sqlite3_wrapper(std::string_view filename,
                  int flags = SQLITE_OPEN_READWRITE | SQLITE_OPEN_CREATE){
    err = sqlite3_open_v2(filename.data(), &db, flags, NULL);
  }
  ~sqlite3_wrapper(){
    sqlite3_close(db);
  }
  //Conversion to bool to check for error
  operator bool(){
    return db_err != SQLITE_OK;
  }
  //implicit conversion to underlying pointer
  operator sqlite3*(){
    return db;
  }
  //Gets the last result from a sqlite3 call in a member function,
  //usually this is used to get an error code. But it will return
  //SQLITE_OK if the last function was successful.
  int err() const {
    return db_err;
  }
  //Gets the last error from any function that uses this connection,
  //undefined if the last function was successful.
  int errcode() const {
    return sqlite3_errcode(db);
  }
  //gets the string version of errcode();
  const char* errmsg(){
    return sqlite3_errmsg(db);
  }
  //Like perror
  void print_errmsg(const char *s){
    if(s && *s){
      fprintf(stderr, "%s: %s\n", s, sqlite3_errmsg(db));
    } else {
      fprintf(stderr, "%s\n", sqlite3_errmsg(db));
    }
  }
  //errstr gets string version of this->err
  const char* errstr(){
    return sqlite3_errstr(db_err);
  }
  //get the string version of the given error code.
  static const char* errstr(int errno){
    return sqlite3_errstr(errno);
  }
  //evaluate the sql in 'sql', the sql is executed for side effects only.
  int exec(const char *sql){
    return sqlite3_exec(db, sql, NULL, NULL);
  }
  int begin_transaction(){
    return exec("BEGIN TRANSACTION;");
  }
  int rollback_transaction(){
    return exec("ROLLBACK TRANSACTION;");
  }
  int commit_transaction(){
    return exec("COMMIT TRANSACTION;");
  }
  int set_read_only(){
    return exec("PRAGMA query_only = true");
  }
  int clear_read_only(){
    return exec("PRAGMA query_only = false");
  }
  int toggle_read_only(bool toggle){
    return (toggle ? set_read_only() : clear_read_only());
  }
  sqlite3_stmt_wrapper prepare_stmt(const std::string_view& sv){
    
    return prepare_stmt(sv.data(), sv.size(), nullptr);
  }
  sqlite3_stmt_wrapper prepare_stmt(const char* sql, int len = -1,
                                    const char** tail = nullptr){
    sqlite3_stmt *stmt;
    db_err = sqlite3_prepare(db, sql, len, &stmt, tail);
    return sqlite3_stmt_wrapper(stmt);
  }
};
struct sqlite3_stmt_wrapper {
  sqlite3_stmt* stmt = nullptr;
  sqlite3_stmt_wrapper(sqlite3_stmt *stmt) : stmt{stmt} {}
  //apperently if you have a null terminated string including it in the
  //size will be faster. so keep that in mind.
  sqlite3_stmt_wrapper(std::string_view sql, sqlite3* db,
                       bool persistant = false){
    sqlite3_prepare_v3(db, sql.data(), sql.size(),
                       (persistant ? SQLITE_PREPARE_PERSISTANT : 0),
                       &stmt, nullptr);
  }
  //if 'sql' contains multiple sql stamtement this should be used,
  //it will modify 'sql' so it contains the unused portion of it's 
  //original value.
  sqlite3_stmt_wrapper(std::string_view *sql, sqlite3* db,
                       bool persistant = false){
    char *tail;
    int err = sqlite3_prepare_v3(db, sql.data(), sql.size(),
                                 (persistant ? SQLITE_PREPARE_PERSISTANT : 0),
                                 &stmt, &tail);
    if(err == SQLITE_OK){
      std::string_view sql_tail(tail, sql.size() - (tail - sql.data()));
      *sql = sql_tail;
    }
  }
  //same parameters as underlying sqlite function,
  //Conversion to bool to check for error
  operator bool(){
    return stmt;
  }
  //implict conversion to underlying pointer
  operator sqlite3*(){
    return stmt;
  }
  //This is convient but doesn't offer a way to tell between an error
  //and successful terminaton.
  bool step(){
    int res = sqlite3_step(stmt);
    return res == SQLITE_ROW;
  }
  //returns SQLITE_OK (0) if there were no errors in step, and nonzero
  //if the last step caused an error. So you need to check the return
  //value of this to test for error.
  int reset(){
    int ret = sqlite3_reset(stmt);
    //Not sure what happens when you call this after an error
    //but I doubt it'll do anything that bad.
    sqlite3_clear_bindings(stmt);
    return ret;
  }
  //there's no need to call this since the destructor will do it for you,
  //but it can be called manualy to check for errors.
  int finalize(){
    int ret = sqlite3_finalize(stmt);
    //Make sure to set stmt to NULL to prevent the destructor
    //from causing a double free.
    stmt = nullptr;
    return ret;
  }
  //Run this statement to completion for side effects only
  int exec(bool should_reset = true){
    while((err = sqlite3_step(stmt)) == SQLITE_ROW); //execute the sql
    if(should_reset){
      return reset();
    } else {
      return (err == SQLITE_DONE ? SQLITE_OK : err);
    }
  }
  //Convience function to execute a complete statement, the callback
  //is given '*this' as its argument. returns SQLITE_OK on success,
  //or an error code if there was an error, returns SQLITE_ABORT if
  //the callback returns a non-zero value, like sqlite3_exec does.
  int exec(std::function<int(sqlite_stmt_wrapper&)> &f,
           bool should_reset = true){
    int err;
    while((err = sqlite3_step(stmt)) == SQLITE_ROW){
      err = f(*this);
      if(err != 0){ return SQLITE_ABORT; }
    }
    //Don't just return err, since on success it will be SQLITE_DONE,
    //which is non-zero, and we want to return 0 on success.
    if(should_reset){
      return reset();
    } else {
      return (err == SQLITE_DONE ? SQLITE_OK : err);
    }
  }
  //Convience function to execute a complete statement, follows
  //the api of the builtin sqlite3_exec, with the char**s replaced
  //with vectors of std::string_views, the views need to be copied
  //if you want to save the text.
  int exec(std::function<int(int, std::vector<std::string_view>&,
                             std::vector<std::string_view>&)> &f,
           bool should_reset = true){
    int err;
    std::vector<std::string_view> row_text, row_names;
    while((err = sqlite3_step(stmt)) == SQLITE_ROW){
      row_text = get_row_text(row_text);
      row_names = get_row_names(row_names);
      err = f(get_ncolumns(), row_text, row_names);
      if(err != 0){ return SQLITE_ABORT; }
    }
    if(should_reset){
      return reset();
    } else {
      return (err == SQLITE_DONE ? SQLITE_OK : err);
    }
  }
  //Same as above but without the column names.
  int exec(std::function<int(int, std::vector<std::string_view>&)> &f,
           bool should_reset = true){
    int err;
    std::vector<std::string_view> row_text;
    while((err = sqlite3_step(stmt)) == SQLITE_ROW){
      row_text = get_row_text(row_text);
      err = f(get_ncolumns(), row_text);
      if(err != 0){ return SQLITE_ABORT; }
    }
    if(should_reset){
      return reset();
    } else {
      return (err == SQLITE_DONE ? SQLITE_OK : err);
    }
  }
  //base template for getting columns, specializations are below.
  template<typename T>
  T get_column(int idx);
  template<typename T>
  T get_column_default(int idx, const T deflt){
    return (sqlite_column_type(stmt, idx) == SQLITE_NULL ?
            deflt : get_column<T>(idx));
  }
  //Get length of a text/blob typed column
  size_t get_column_bytes(int idx){
    return sqlite3_column_bytes(stmt, idx);
  }
  //Get the number of columns in the current row.
  int get_ncolumns(){
    return sqlite3_data_count(stmt);    
  }
  std::string_view column_name(int i){
    return sqlite3_column_name(stmt, i);
  }
  sqlite3_type column_type(int i){
    return sqlite3_type(sqlite3_column_type(i));
  }
  //returns a vector of all the columns of the current row converted into
  //text. The text is not copied so is only valid until the next call
  //to step (or reset/finalize).
  //by default sql nulls are translated into nullptrs, but nullstr can
  //be used to provide an alternate value (i.e "NULL").
  std::vector<std::string_view> get_row_text(const char *nullstr = nullptr){
    std::vector<std::string_view> ret;
    return get_row_text(ret, nullstr);
  }
  std::vector<std::string_view>& get_row_text(std::vector<std::string_view>& row,
                                         const char *nullstr = nullptr);

  std::vector<std::string_view> get_row_names(){
    std::vector<std::string_view> ret;
    return get_row_names();
  }
  std::vector<std::string_view>& get_row_names(std::vector<std::string_view>& row){
    int ncols = get_ncolumns();
    row.clear();
    for(int i = 0; i < ncols; i++){
      row.emplace_back(column_name(i));
    }
    return row;
  }
  json get_row_json();
  //Single overloaded function to replace the sqlite_bind_type functions.
  int bind(int idx, double val){
    return sqlite3_bind_double(stmt, idx, val);
  };
  int bind(int idx, int val){
    return sqlite3_bind_int(stmt, idx, val);
  }
  int bind(int idx, int64_t val){
    return sqlite3_bind_int64(stmt, idx, val);
  }
  int bind(int idx, uint64_t val){
    return sqlite3_bind_int64(stmt, idx, val);
  }
  //This works for any string type, but always makes a copy
  int bind(int idx, std::string_view sv){
    return sqlite3_bind_text(stmt, idx, sv.data(), sv.size(), SQLITE_TRANSIENT);
  }
  int bind_null(int idx){
    return sqlite3_bind_null(stmt, idx);
  }
  //bind a potentially null pointer, if ptr == nullptr, the index is bound
  //to NULL, otherwise it is bound to *ptr
  template<typename T>
  int bind(int idx, const T* ptr){
    if(ptr){
      return bind(idx, *ptr);
    } else {
      return bind_null(idx);
    }
  }
  //direct access to sqlite_bind_text to allow binding a static string
  //or transfering ownership of a string to sqlite.
  int bind(int idx, const char* str, int len,
           void(*destroy)(void*)){
    return sqlite3_bind_text(stmt, idx, str, len, destroy);
  }
  //bind json by converting it into a string.
  int bind(int idx, json j){
    if(j.is_null()){
      return bind_null();
    }
    util::svector<char> buf;
    auto it = std::back_inserter(buf);
    json.write(it);
    size_t sz = buf.size();
    //Transfer ownership of the string to sqlite.
    const char* str = buf.take_memory();
    return sqlite3_bind_text(stmt, idx, str, sz, free);
  }
  template<typename T>
  int bind_name(std::string_view name, T val){
    sqlite3_stmt_bind(stmt, sqlite3_bind_parameter_index(stmt, name.data()), val);
  }
  template<typename T>
  int multibind_impl(int idx, T val){
    return bind(idx, val);
  }
  template<typename T, typename ... Ts>
  int multibind_impl(int idx, T val, Ts&&... rest){
    int err = bind(idx, val);
    if(err != SQLITE_OK){
      return err;
    } else {
      return multibind_impl(stmt, idx+1, rest...);
    }
  }
  template<typename ... Ts>
  int multibind(Ts&& ... args){
    return multibind_impl(stmt, 0, std::forward<Ts>(args)...);
  }
};
template<>
inline int sqlite3_stmt_wrapper::get_column<int>(int idx){
  return sqlite3_column_int(this->stmt, idx);
}
template<>
inline double sqlite3_stmt_wrapper::get_column<double>(int idx){
  return sqlite3_column_double(this->stmt, idx);
}
template<>
inline int64_t sqlite3_stmt_wrapper::get_column<int64_t>(int idx){
  return sqlite3_column_int64(this->stmt, idx);
}
//Be careful this returns a nullptr if the column is NULL.
template<>
inline const char* sqlite3_stmt_wrapper::get_column<const char*>(int idx){
  return static_cast<const char*>(sqlite3_column_text(this->stmt, idx));
}
template<>
inline std::string_view sqlite3_stmt_wrapper::get_column<std::string_view>(int idx){
  return std::string_view(this->get_column<const char*>(idx),
                          sqlite3_column_bytes(this->stmt, idx));
}
//Void* indicates a blob
template<>
inline void* sqlite3_stmt_wrapper::get_column<void*>(int idx){
  return static_cast<const char*>(sqlite3_column_blob(this->stmt, idx));
}
template<>
inline json sqlite3_stmt_wrapper::get_column<json>(int idx){
  return json::parse(this->get_column<std::string_view>(idx));
}
//Contains the objects associated with inserting and querying the database
struct vndb_sql_context {  
  sqlite3_wrapper db;
  //These are not automatically initialized, you need to explicitly initialize
  //them, this is to avoid the overhead of creating prepared statments you
  //may never use.
  std::array<sqlite3_stmt_wrapper, vndb::num_object_types> get_by_id_stmts = {{}};
  std::array<sqlite3_stmt_wrapper, vndb::num_object_types> insert_stmts = {{}};
  
  json get_by_id(int id, vndb::object_type what){
    
  int insert(json object, vndb::object_type what);

  sqlite3_wrapper get_db(){
    return db;
  }
  //not called get_get_by_id_stmt, because that sounds really silly.
  sqlite3_stmt_wrapper get_select_by_id_stmt(vndb::object_type what){
    return get_by_id_stmts[to_underlying(what)];
  }
  sqlite3_stmt_wrapper get_insert_stmt(vndb::object_type what){
    return get_by_id_stmts[to_underlying(what)];
  }

  int exec(std::string_view sql);//execute sql for side effects
  //execute sql calling f after each row
  int exec(std::string_view sql, std::function<int(sqlite_stmt_wrapper&)> &f);
  //execute sql and parse into a json array.
  json exec_json(std::string_view sql);

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
