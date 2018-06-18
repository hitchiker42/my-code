#ifndef __VNDB_H__
#error "Don't include sqlite_wrappers.h directly use vndb.h"
#endif
//simple string using memory allocated with sqlite_malloc, used pretty
//much exclusively to wrap the return value of sqlite3_expanded_sql.
struct sqlite_string {
  typedef const char* pointer;
  typedef const char& reference;
  typedef char value_type;
  typedef size_t size_type;
  typedef ptrdiff_t difference_type;
  typedef pointer iterator;
  char *str;
  size_t len;
  sqlite_string(char *str, bool copy = false)
    : str{copy ? sqlite_strdup(str) : str}, len{strlen(str)} {}
  sqlite_string(char *str, size_t len, bool copy = false)
    : str{copy ? sqlite_strdup(str, len)  : str}, len{len} {}
  ~sqlite_string(){
    sqlite3_free(str);
  }
  operator std::string_view(){
    return std::string_view(str, len);
  }
  const char* c_str() const {
    return str;
  }
  const char* data() const {
    return str;
  }
  size_t size() const {
    return len;
  }
  char operator[](size_type idx) const {
    return str[idx];
  }
  iterator begin() const {
    return iterator(str);
  }
  iterator end() const {
    return iterator(str+len);
  }
  static char* sqlite_strdup(const char *str, size_t len = -1){
    if(len == (size_t)-1){
      len = strlen(str);
    }
    char *ret = (char*)sqlite3_malloc(len+1);
    memcpy(ret, str, len + 1);
    return ret;
  }
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
struct sqlite3_wrapper; //forward declaration of database wrapper

struct sqlite3_stmt_wrapper {
  sqlite3_stmt* stmt = nullptr;
  sqlite3_stmt_wrapper() = default;
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
  sqlite3_stmt_wrapper(sqlite3_stmt_wrapper &&other)
    : sqlite3_stmt_wrapper(other.stmt) {
      other.stmt = nullptr;
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
  //Returns sql stmt with currently bound parameters expanded
  sqlite_string get_sql(){
    return sqlite3_expanded_sql(stmt);
  }
  //Returns the string used to initialize the stmt, i.e without expanding
  //any parameters.
  std::string_view get_sql_template(){
    return sqlite3_sql(stmt);
  }
  //This is convient but doesn't offer a way to tell between an error
  //and successful terminaton.
/*  bool step(){
    int res = sqlite3_step(stmt);
    return res == SQLITE_ROW;
  }*/
  int step(){
    return sqlite3_step(stmt);
  }
  //Important: This needs to be called after each complete executition of
  //the statement.

  //returns SQLITE_OK (0) if there were no errors in step, and nonzero
  //if the last step caused an error. So you need to check the return
  //value of this to test for error.
  int reset(){
    return sqlite3_reset(stmt);
  }
  //There's no real need to ever reset the bindings, but doing so can help 
  //to point out errors, and potentially release resources if the stmt
  //outlives the paramters.
  int reset_bindings(){
    return sqlite3_clear_bindings(stmt);
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
  int exec(){
    int err;
    while((err = sqlite3_step(stmt)) == SQLITE_ROW); //execute the sql
    //reset will return the appropiate error code if sqlite_step failed;
    return reset();
  }
  //Convience function to execute a complete statement, the callback
  //is given '*this' as its argument. returns SQLITE_OK on success,
  //or an error code if there was an error, returns SQLITE_ABORT if
  //the callback returns a non-zero value, like sqlite3_exec does.
  int exec(std::function<int(sqlite3_stmt_wrapper&)> &f){
    int err;
    while((err = sqlite3_step(stmt)) == SQLITE_ROW){
      err = f(*this);
      if(err != 0){ return SQLITE_ABORT; }
    }
    //reset will return the appropiate error code if sqlite_step failed;
    return reset();
  }
  //Convience function to execute a complete statement, follows
  //the api of the builtin sqlite3_exec, with the char**s replaced
  //with vectors of std::string_views, the views need to be copied
  //if you want to save the text.
  int exec(std::function<int(int, std::vector<std::string_view>&,
                             std::vector<std::string_view>&)> &f){
    int err;
    std::vector<std::string_view> row_text, row_names;
    while((err = sqlite3_step(stmt)) == SQLITE_ROW){
      row_text = get_row_text(row_text);
      row_names = get_row_names(row_names);
      err = f(get_ncolumns(), row_text, row_names);
      if(err != 0){ return SQLITE_ABORT; }
    }
    //reset will return the appropiate error code if sqlite_step failed;
    return reset();
  }
  //Same as above but without the column names.
  int exec(std::function<int(int, std::vector<std::string_view>&)> &f){
    int err;
    std::vector<std::string_view> row_text;
    while((err = sqlite3_step(stmt)) == SQLITE_ROW){
      row_text = get_row_text(row_text);
      err = f(get_ncolumns(), row_text);
      if(err != 0){ return SQLITE_ABORT; }
    }
    //reset will return the appropiate error code if sqlite_step failed;
    return reset();
  }
  //Run this statement and collect the results (as json objects) into
  //a json array.
  json exec_json(int *err_ptr = nullptr){
    std::vector<json> rows;
    int err;
    if(!err_ptr){ err_ptr = &err; }
    while((err = sqlite3_step(stmt)) == SQLITE_ROW){
      json row = get_row_json();
      //This should never happen.
      if(row.is_null()){
        return SQLITE_ABORT;
      }
      rows.emplace_back(row);
    }
    *err_ptr = reset();
    if(*err_ptr != SQLITE_OK){
      return json_null;
    } else {
      return rows;
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
  sqlite3_wrapper get_db();
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
  int bind(int idx, const char* c_str){
    return sqlite3_bind_text(stmt, idx, c_str, strlen(c_str), SQLITE_TRANSIENT);
  }
  int bind_null(int idx){
    return sqlite3_bind_null(stmt, idx);
  }
  //bind a potentially null pointer, if ptr == nullptr, the index is bound
  //to NULL, otherwise it is bound to *ptr. Excludes char*s since they
  //bind to strings.
  template<typename T,
           std::enable_if_t<!std::is_same_v<std::remove_cv_t<T>, char*>, int> = 0>
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
  //Bind a blob, there's no way to do this without explicitly giving
  //a length,
  int bind(int idx, const void* blob, int len,
           void(*destroy)(void*) = SQLITE_TRANSIENT){
    return sqlite3_bind_blob(stmt, idx, blob, len, destroy);
  }
  //bind json by converting it into a string.
  int bind(int idx, const json &j){
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
  //if is_view is true we don't call sqlite3_close in the destructor, this
  //is mostly used for pointers returned from sqlite3_stmt_wrapper::get_db.
  bool is_view = false;
  bool in_transaction = false;
  sqlite3_wrapper(std::string_view filename,
                  int flags = SQLITE_OPEN_READWRITE | SQLITE_OPEN_CREATE){
    db_err = sqlite3_open_v2(filename.data(), &db, flags, NULL);
  }
  sqlite3_wrapper(sqlite3_wrapper &&other)
    : db{other.db}, db_err{other.db_err}, in_transaction{other.in_transaction} {
      other.db = nullptr;
  }
  //If given a raw database pointer assume we don't take ownership, and so
  //shouldn't destroy the db connectin when we go out of scope.
  sqlite3_wrapper(sqlite3 *db_ptr)
    : db{db_ptr}, is_view{true} {}

  ~sqlite3_wrapper(){
    if(!is_view){
      sqlite3_close(db);
    }
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
  template<typename T, typename ... Ts>
  void print_errmsg(const char *fmt, const T& arg, const Ts&... args){
    fprintf(stderr, fmt, arg, args...);
    fprintf(stderr, "%s(%d)\n", sqlite3_errmsg(db),
            sqlite3_extended_errcode(db));
  }
  //errstr gets string version of this->err
  const char* errstr(){
    return sqlite3_errstr(db_err);
  }
  //get the string version of the given error code.
  static const char* errstr(int errno){
    return sqlite3_errstr(errno);
  }
  const char *filename(const char *name = "main"){
    return sqlite3_db_filename(db, name);
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
  sqlite3_stmt_wrapper prepare_stmt(const std::string_view& sv, 
                                    bool persistant = false){
    return prepare_stmt(sv.data(), sv.size(), nullptr, persistant);
  }
  sqlite3_stmt_wrapper prepare_stmt(const char* sql, int len = -1,
                                    const char** tail = nullptr,
                                    bool persistant = false){
    sqlite3_stmt *stmt = prepare_stmt_ptr(sql, len, tail, persistant);
    return sqlite3_stmt_wrapper(stmt);
  }
  sqlite3_stmt* prepare_stmt_ptr(const std::string_view& sv,
                                 bool persistant = false){

    return prepare_stmt_ptr(sv.data(), sv.size(), nullptr, persistant);
  }
  sqlite3_stmt* prepare_stmt_ptr(const char* sql, int len = -1,
                                 const char** tail = nullptr,
                                 bool persistant = false){
    sqlite3_stmt *stmt;
//    vndb_log->log_debug("Compiling sql '%s'.\n", sql);
    db_err = sqlite3_prepare_v3(db, sql, len, 
                                (persistant ? SQLITE_PREPARE_PERSISTENT : 0),
                                &stmt, tail);
    return stmt;
  }
};

int sqlite_insert_vn(const json &vn, sqlite3_stmt_wrapper& stmt);
int sqlite_insert_release(const json &release,
                          sqlite3_stmt_wrapper& stmt);
int sqlite_insert_producer(const json& producer,
                           sqlite3_stmt_wrapper& stmt);
int sqlite_insert_character(const json& chara,
                           sqlite3_stmt_wrapper& stmt);
int sqlite_insert_staff(const json& staff,
                        sqlite3_stmt_wrapper& stmt);
//All of the following may execute stmt more than once so they should
//be called within an explicit transaction.
//Most of these have two versions one which takes a json object as returned
//by the vndb get command and one which takes more specific arguments, the
//actual functions are the ones with more specific arguments, the others
//are just for convenience.
int sqlite_insert_vn_producer_relations(const json& release,
                                        sqlite3_stmt_wrapper& stmt);
int sqlite_insert_vn_producer_relations(int release_id,
                                        const json& vns, const json& producers,
                                        sqlite3_stmt_wrapper& stmt);
int sqlite_insert_vn_character_actor_relations(const json& actor,
                                               sqlite3_stmt_wrapper &stmt);
int sqlite_insert_vn_character_actor_relations(int actor_id,
                                               const json& voiced,
                                               sqlite3_stmt_wrapper &stmt);
int sqlite_insert_vn_staff_relations(const json& staff,
                                     sqlite3_stmt_wrapper &stmt);
int sqlite_insert_vn_staff_relations(int staff_id,
                                     const json& vn_info,
                                     sqlite3_stmt_wrapper &stmt);
int sqlite_insert_staff_aliases(const json& staff,
                                sqlite3_stmt_wrapper &stmt);
int sqlite_insert_staff_aliases(int staff_id,
                                const json& aliases,
                                sqlite3_stmt_wrapper &stmt);
//template<vndb::object_type what>
inline int sqlite_insert_object(vndb::object_type what, const json& obj, sqlite3_stmt_wrapper& stmt){
  vndb_log->printf(util::log_level::debug, "Inserting %s, id = %d.\n",
                   vndb::object_type_names[to_underlying(what)].data(),
                   obj["id"].get<int>());
  if (what == vndb::object_type::VN){
    return sqlite_insert_vn(obj, stmt);
  } else if(what == vndb::object_type::release){
    return sqlite_insert_release(obj, stmt);
  } else if(what == vndb::object_type::producer){
    return sqlite_insert_producer(obj, stmt);
  } else if(what == vndb::object_type::character){
    return sqlite_insert_character(obj, stmt);
  } else if(what == vndb::object_type::staff){
    return sqlite_insert_staff(obj,stmt);
  } else {
    assert(false);
  }
}
inline sqlite3_wrapper sqlite3_stmt_wrapper::get_db(){
  return sqlite3_wrapper(sqlite3_db_handle(this->stmt));
}
