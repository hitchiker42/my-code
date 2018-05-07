#include "vndb.h"
template<typename T>
using vector = util::svector<T>;//can change to std::vector
using string = util::string;//can change to std::string
static constexpr std::string_view sql_select_by_id =
  "select * from @table where id = @id;"sv;
static constexpr std::string_view sql_insert_vn =
  R"EOF(insert or replace into VNs values (
        @id, @title, @original, @released,
        @languages, @orig_lang, @platform, @aliases,
        @length, @description, @links, @image, @image_nsfw,
        @anime, @relations, @tags, @popularity, @rating,
        @votecount, @screens, @staff, "[]", "[]" ,"[]",
        -1, -1, @date, 0)EOF"sv;

//Get the current row that stmt refers to, will all values converted to strings.
static vector<string> sqlite3_stmt_get_row(sqlite3_stmt* stmt){
  int ncols = sqlite3_data_count(stmt);
  vector<string> ret;
  if(ncols <= 0){
    return ret;
  }
  ret.reserve(ncols);
  
  for(int i = 0; i < ncols; i++){
    const char *str = sqlite3_column_text(stmt, i);
    if(!str){
      ret.emplace_back("NULL");
    } else {
      ret.emplace_back(str, sqlite3_column_bytes(stmt, i));
    }
  }
  return ret;
}
static std::pair<vector<vector<string>>, bool> sqlite3_stmt_get_table(sqlite3_stmt* stmt){
  int code;
  vector<vector<string>> ret;
  //We assume we're the only one accessing the db, so SQLITE_BUSY can't occur.
  while((code = sqlite3_step(stmt)) == SQLITE_ROW){
    ret.emplace_back(sqlite3_stmt_get_row(stmt));
  }
  return {ret, code == SQLITE_DONE};
}
void null_sqlite3_stmt_bindings(sqlite3_stmt* stmt){
  int parameter_cnt = sqlite3_bind_parameter_count(stmt);
  for(int i = 0; i < parameter_cnt; i++){
    sqlite3_bind_null(stmt, i);
  }
}
int sqlite3_stmt_bind(sqlite3_stmt *stmt, int idx, double val){
  return sqlite3_bind_double(stmt, idx, val);
}
int sqlite3_stmt_bind(sqlite3_stmt *stmt, int idx, int val){
  return sqlite3_bind_int(stmt, idx, val);
}
int sqlite3_stmt_bind(sqlite3_stmt *stmt, int idx, int64_t val){
  return sqlite3_bind_int64(stmt, idx, val);
}
int sqlite_stmt_bind(sqlite3_stmt *stmt, int idx, std::string_view sv){
  return sqlite3_bind_text(stmt, idx, sv.data(), sv.size(), SQLITE_TRANSIENT);
}
template<typename T>
int sqlite_stmt_named_bind(sqlite3_stmt *stmt, std::string_view name, T val){
  sqlite_stmt_bind(stmt, sqlite3_bind_parameter_index(stmt, name.data()), val);
}

template<typename T>
int sqlite_stmt_multibind_impl(sqlite3_stmt *stmt, int idx, T val){
  return sqlite_stmt_bind(stmt, idx, val);
}
template<typename T, typename ... Ts>
int sqlite_stmt_multibind_impl(sqlite3_stmt *stmt, int idx, T val, Ts&&... rest){
  int err = sqlite_stmt_bind(stmt, idx, val);
  if(err != SQLITE_OK){
    return err;
  } else {
    return sqlite_stmt_multibind(stmt, idx+1, rest...);
  }
}
template<typename ... Ts>
int sqlite_stmt_multibind(sqlite3_stmt *stmt, Ts&& ... args){
  return sqlite_stmt_multibind_impl(stmt, 0, std::forward<Ts>(args)...);
}
