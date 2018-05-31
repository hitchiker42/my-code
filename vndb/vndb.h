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

extern SSL_CTX* vndb_ctx;
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
//struct dealing with SSL details.
struct BIO_wrapper {
  BIO* bio = NULL;
  BIO_wrapper() = default;
  BIO_wrapper(const char* hostname, const char* port,
              SSL_CTX* ctx = vndb_ctx);
  BIO_wrapper(std::string_view hostname, std::string_view port,
              SSL_CTX* ctx = vndb_ctx)
    : BIO_wrapper(hostname.data(), port.data(), ctx) {}
  ~BIO_wrapper();
  bool is_encrypted();
  void reset();
  bool connect();
  bool reconnect(){
    reset();
    return connect();
  }
  ssize_t write(std::string_view msg);
  //Read upto len bytes into buf, if BIO_read returns 0 an error message
  //is printed (though you don't need to treat it as an error).
  ssize_t read(char *buf, size_t len);
  //Read exactly n bytes into buf.
  ssize_t read_n(char *buf, size_t n);
  //these next two functions repeatidly call BIO_read, if there is an error
  //the length of buf is set to 0 and the error code is returned, otherwise
  //the total number of bytes read is returned.

  //read until BIO_read returns 0.
  ssize_t read_all(util::svector<char> &buf);
  //read until delim is read, delim is assumed to be the last character of
  //the message, so only the last character read is ever checked.
  ssize_t read_delim(util::svector<char> &buf, char delim);
  //read until delim is read, delim is assumed to be a suffix of the message.
  ssize_t read_delim(util::svector<char> &buf, const char* delim);
  //  ssize_t read_delim(const char **buf, size_t *bufsz, char delim);
  operator BIO*() const noexcept {
    return bio;
  }
};
struct http_connection {
  //For vndb we need to exclude the www, since I'm not going to handle
  //redirects, and www.vndb.org will give a redirect response.
  std::string hostname = "vndb.org";
  std::string port = "443";
  BIO_wrapper bio;
  int err = 0;
  http_connection(bool do_connect = true)
    : bio(hostname.c_str(), port.c_str(), vndb_ctx) {
    if(do_connect){
      init();
    }
  }
  http_connection(std::string_view hostname, std::string_view port, 
                  bool do_connect = true)
    : hostname{hostname}, port{port}, bio(hostname, port, vndb_ctx) {
      if(do_connect){
        init();
      }
    }
  void init(){
    if(bio){
      err = !bio.connect();
    } else {
      err = 1;
    }
  }
  //Perform an http get request for 'uri' and copy the response body into buf.
  //return 0 for a 200 response, a negitive number if there was an error in
  //the underlying connection and return the http response code otherwise.
  int http_get(std::string_view uri, util::svector<char>& buf);
  //indicates if there was an error in the constructor.
  operator bool(){
    return err == 0;
  }
};
//definition of different vndb objects, since we need this in
//vndb_connection.
namespace vndb {
//Possible targets of the get command
enum class object_type {
  VN = 0,
  release,
  producer,
  character,
  staff
};
constexpr int num_object_types = to_underlying(object_type::trait)+1;
static constexpr std::array<std::string_view, num_object_types> 
object_type_names = {{"VNs", "Releases", "Producers", "Characters", "Staff"}};
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
  bool wait_on_throttle = true;
  bool throttled = false;
  static constexpr const char *hostname = vndb_hostname;
  static constexpr const char *port = vndb_tls_port_number;
  //Get commands, without the filter or options, to get all possible
  //information for the different types of vndb objects.
  static constexpr std::string_view get_vn_command_base =
    "get vn basic,details,anime,relations,tags,stats,screens,staff "sv;
  static constexpr std::string_view get_release_command_base =
    "get release basic,details,vn,producers "sv;
  static constexpr std::string_view get_producer_command_base =
    "get producer basic,details,relations "sv;
  //Excludes the character measurements, since I really don't need them.
  static constexpr std::string_view get_character_command_base =
    "get character basic,details,traits,vns,voiced "sv;
  static constexpr std::string_view get_staff_command_base =
    "get staff basic,details,aliases,vns,voiced "sv;
  static constexpr std::string_view get_get_command_base(vndb::object_type what){
    assert(what != vndb::object_type::tag);
    assert(what != vndb::object_type::trait);
    switch(what){
      case(vndb::object_type::VN): return get_vn_command;
      case(vndb::object_type::release): return get_release_command;
      case(vndb::object_type::producer): return get_producer_command;
      case(vndb::object_type::character): return get_character_command;
      case(vndb::object_type::staff):  return get_staff_command;
      default:
        assert(false);
    }
  }


  vndb_connection(std::string_view username, std::string_view passwd,
                  bool wait = true, bool login = true)
      : bio(vndb_hostname, vndb_tls_port_number, vndb_ctx),
        buf(4096),
        username(username), passwd(passwd), wait_on_throttle{wait} {
    if(login && this->bio.connect()){
      this->logged_in = this->login();
    }
  }
  vndb_connection(bool wait = true, bool login = true)
    : vndb_connection("","",wait,login) {}

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
  //results already retrieved are kept, and the json object containing
  //the error response can be obtained using get_error;
  int send_get_command(std::vector<json>& results,
                       util::string_view sort_by = "id");
  //Run a get command and call 'callback' with each page of results as
  //they are retrieved. The callback is free to modify the vector of items
  //it is given. If 'callback' returns a nonzero value then the function
  //will immediately terminate and return that value.
  using get_callback = std::function<int(std::vector<json>&)>;
  int send_get_command(get_callback &callback,
                       util::string_view sort_by = "id");
  //Internal helper function for send_get_command.
  json send_get_command_once(int page_no,
                             util::string_view sort_by = "id");
  //assumes an error response is in buf, parses it as json.
  json get_error();
  bool login();
  //returns true if already logged in, and if not tries to login and returns
  //the result of that attempt.
  bool ensure_logged_in(){
    if(logged_in){ 
      return true; 
    } else {
      return relogin();
    }
  }
  //sets username and passwd, can be used to switch from an anonymous
  //session to a specific user.
  bool login(std::string_view username, std::string_view passwd){
    this->username = username;
    this->passwd = passwd;
    return relogin();
  }
  //just a different name for the above function
  bool relogin(std::string_view username, std::string_view passwd){
    return login(username, passwd);
  }
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
  int get(vndb::object_type what, int start, int stop,
          get_callback& callback);
  int get_all(vndb::object_type what, get_callback& callback);
  //Get a number of vns from the server starting at a given id
  //and ending at either a given id, or id+25 (which is the max
  //number of vns we can get per response.
  int get_vns(int start, int stop, std::vector<json>& vec){
   return get(vndb::object_type::VN, start, stop, vec);
  }
  int get_vns(int start, std::vector<json>& vec){
    return get_vns(start, start + 25, vec);
  }
  //You need to check this->error after calling this, since on error
  //the error response is stored in the vector.
  std::vector<json> get_vns(int start, int count = 25){
    std::vector<json> ret;
    (void)get_vns(start, start + count, ret);
    return ret;
  }
  json dbstats();
  //TODO: change these so they get 100 results at a time, since
  //thats allowed.
  json get_vnlist(std::vector<json>& vec){
    buf.append("get vnlist (uid = 0)");
    return send_get_command(vec);
  }
  json get_wishlist(std::vector<json>& vec){
    buf.append("get wishlist (uid = 0)");
    return send_get_command(vec);
  }
  //Functions to add/remove/modify votes / entries on user lists.
  //value is a score on [10,100]
  bool send_set_command();
  bool set_vote(int vn_id, int value);
  bool remove_vote(int vn_id);
  //priority is 0:high, 1:medium, 2:low, 3:blacklist
  bool set_wishlist(int vn_id, int priority);
  bool remove_from_wishlist(int vn_id);
  //status 0=Unknown, 1=playing, 2=finished, 3=stalled, 4=dropped.
  bool set_vnlist(int vn_id, int status = 0);
  //I've never set a note on my vnlist, so I doubt I'll use this
  bool set_vnlist(int vn_id, std::string_view note, int status = 0);
  bool remove_from_vnlist(int vn_id);
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
struct sqlite3_stmt_wrapper {
  sqlite3_stmt* stmt = nullptr;
  sqlite3_stmt_wrapper(sqlite3_stmt *stmt) : stmt{stmt} {}
  //apperently if you have a null terminated string including it in the
  //size will be faster. so keep that in mind.
  sqlite3_stmt_wrapper(std::string_view sql, sqlite3* db,
                       bool persistant = false){
    sqlite3_prepare_v3(db, sql.data(), sql.size(),
                       (persistant ? SQLITE_PREPARE_PERSISTENT : 0),
                       &stmt, nullptr);
  }
  //if 'sql' contains multiple sql stamtement this should be used,
  //it will modify 'sql' so it contains the unused portion of it's
  //original value.
  sqlite3_stmt_wrapper(std::string_view *sql, sqlite3* db,
                       bool persistant = false){
    const char *tail;
    int err = sqlite3_prepare_v3(db, sql->data(), sql->size(),
                                 (persistant ? SQLITE_PREPARE_PERSISTENT : 0),
                                 &stmt, &tail);
    if(err == SQLITE_OK){
      std::string_view sql_tail(tail, sql->size() - (tail - sql->data()));
      *sql = sql_tail;
    }
  }
  ~sqlite3_stmt_wrapper(){
    sqlite3_finalize(stmt);
  }
  //same parameters as underlying sqlite function,
  //Conversion to bool to check for error
  operator bool(){
    return stmt;
  }
  //implict conversion to underlying pointer
  operator sqlite3_stmt*(){
    return stmt;
  }
  sqlite3_stmt* unwrap(){
    return stmt;
  }
  //This is convient but doesn't offer a way to tell between an error
  //and successful terminaton.
  bool step(){
    int res = sqlite3_step(stmt);
    return res == SQLITE_ROW;
  }
  bool step_explicit(){
    return sqlite3_step(stmt);
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
    int err;
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
  int exec(std::function<int(sqlite3_stmt_wrapper&)> &f,
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
    return (sqlite3_column_type(stmt, idx) == SQLITE_NULL ?
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
    return sqlite3_type(sqlite3_column_type(stmt,i));
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
    return get_row_names(ret);
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
  int bind(int idx, const std::string& s){
    return sqlite3_bind_text(stmt, idx, s.data(), s.size(), SQLITE_TRANSIENT);
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
      return bind_null(idx);
    }
    util::svector<char> buf;
    auto it = std::back_inserter(buf);
    j.write(it);
    size_t sz = buf.size();
    //Transfer ownership of the string to sqlite.
    const char* str = buf.take_memory();
    return sqlite3_bind_text(stmt, idx, str, sz, free);
  }
  template<typename T>
  int bind_name(std::string_view name, T val){
    return sqlite3_stmt_bind(stmt, sqlite3_bind_parameter_index(stmt, name.data()), val);
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
  //Cast is from unsigned char to char, so static_cast doesn't work (dunno why)
  return reinterpret_cast<const char*>(sqlite3_column_text(this->stmt, idx));
}
template<>
inline std::string_view sqlite3_stmt_wrapper::get_column<std::string_view>(int idx){
  return std::string_view(this->get_column<const char*>(idx),
                          sqlite3_column_bytes(this->stmt, idx));
}
//Void* indicates a blob
template<>
inline const void* sqlite3_stmt_wrapper::get_column<const void*>(int idx){
  return sqlite3_column_blob(this->stmt, idx);
}
template<>
inline json sqlite3_stmt_wrapper::get_column<json>(int idx){
  return json::parse(this->get_column<std::string_view>(idx));
}

//struct dealing with generic sqlite connection.
struct sqlite3_wrapper {
  sqlite3 *db;
  //Set to the result of the last sqlite function
  int db_err = SQLITE_OK;
  
  bool in_transaction = false;
  sqlite3_wrapper(std::string_view filename,
                  int flags = SQLITE_OPEN_READWRITE | SQLITE_OPEN_CREATE){
    db_err = sqlite3_open_v2(filename.data(), &db, flags, NULL);
  }
  ~sqlite3_wrapper(){
    sqlite3_close(db);
  }
  //Conversion to bool to check for error in constructor
  operator bool(){
    return db;
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
      fprintf(stderr, "%s: %s(%d)\n", s, sqlite3_errmsg(db),
              sqlite3_extended_errcode(db));
    } else {
      fprintf(stderr, "%s(%d)\n", sqlite3_errmsg(db),
              sqlite3_extended_errcode(db));
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
    return sqlite3_exec(db, sql, NULL, NULL, NULL);
  }
  //Execute the sql code stored in filename, return 0 on success and an error
  //code on failure. If 'filename' can not be opened return SQLITE_CANTOPEN.
  int exec_file(const char *filename){
    FILE_wrapper sql_file(filename, "r");
    if(!sql_file){
      fprintf(stderr, "Error opening %s\n", filename);
      return SQLITE_CANTOPEN;
    }
    std::string sql = sql_file.to_string();
    return exec(sql.c_str());
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

int sqlite_insert_vn(json vn, sqlite3_stmt_wrapper& stmt);
int sqlite_insert_release(const json &release,
                          sqlite3_stmt_wrapper& stmt);
int sqlite_insert_producer(const json& producer,
                           sqlite3_stmt_wrapper& stmt);
int sqlite_insert_character(const json& chara,
                           sqlite3_stmt_wrapper& stmt);
int sqlite_insert_staff(const json& staff,
                        sqlite3_stmt_wrapper& stmt);
template<vndb::object_type what>
int sqlite_insert_object(const json& obj, sqlite3_stmt_wrapper& stmt){
  if constexpr(what == vndb::object_type::VN){
    return sqlite_insert_vn(obj, stmt);
  } else if(what == vndb::object_type::release){
    return sqlite_insert_release(obj, stmt);
  } else if(what == vndb::object_type::producer){
    return sqlite_insert_prouducer(obj, stmt);
  } else if(what == vndb::object_type::character){
    return sqlite_insert_character(obj, stmt);
  } else if(what == vndb::object_type::staff){
    return sqlite_insert_staff(obj,stmt);
  }
}
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
  //function to access db_stats via table type, since the names of the tables
  //don't all match up to the names in db_stats.
  int db_stats_count(table_type what){
    assert(to_underlying(what) < num_base_tables);
    switch(what){
      case table_type::VNs: return db_stats["vn"].get<int>();
      case table_type::characters: return db_stats["chars"].get<int>();
      default: return db_stats[table_names[to_underlying(what)]].get<int>();
    }
  }
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
      db_stats = conn->dbstats();
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
    return (db.exec_file(db_init_file) == SQLITE_OK);
  }
  json get_by_id(int id, table_type what){
    auto stmt = get_by_id_stmts[];
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
  sqlite3_stmt_wrapper get_select_by_id_stmt(table_type what){
    assert(to_underlying(what) > num_base_tables);
    return &get_by_id_stmts[to_underlying(what)];
  }
  sqlite3_stmt_wrapper get_insert_stmt(table_type what){
    return insert_stmts[to_underlying(what)];
  }
  std::function<int(std::vector<json>)> gen_insert_callback(vndb::object_type what,
                                                            struct progress_bar *pb){
    auto ins_stmt = get_insert_stmt(what);
    auto callback = [ins_stmt, &db, pb](std::vector<json> items) -> int {
      if(db.begin_transaction() != SQLITE_OK){ return -1; }
      for(auto &&item : items){
        if(sqlite_insert_object<what>(item, ins_stmt) != SQLITE_OK){
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
  std::function<int(std::vector<json>)> gen_insert_callback(table_type what){
    assert(to_underlying(what) <= num_base_tables);
    return gen_insert_callback(vndb::object_type(to_underlying(what)));
  }
  
  int download_and_insert(vndb::object_type what, int start, int stop){
    progress_bar pb(db_stats_count(what), 
                    vndb::object_type_names[to_underlying(what)]);
    auto callback = gen_insert_callback(what, &pb);
    return connection.get(what, callback, start, stop);
  }
  int download_and_insert_all(vndb::object_type what){
    progress_bar pb(db_stats_count(what), 
                    vndb::object_type_names[to_underlying(what)]);
    auto callback = gen_insert_callback(what, &pb);
    return connection.get(what, callback, start, stop);
  }
  int get_max_id(table_type what){
    static constexpr bufsz = 256;
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
