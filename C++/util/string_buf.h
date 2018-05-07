#ifndef _STRING_BUF_H_
#define _STRING_BUF_H_

#include "string_view.h"
#include "my_string.h"
#include "svector.h"
namespace util {
/*
  String buffer with optimizations to store long strings as pointers
  and only copy when converting to a string.

  Keeps an internal buffer which numbers, chars and small strings are
  copied to and a vector of pointers for long strings. When a long
  string is appended the internal buffer is first copied and appended
  to the vector.
*/
struct string_view_buf {
  util::svector<util::string_view> vec;
  size_t sz = 0;//total length of string_views in vec.
  //Could make this a template parameter.
  static constexpr size_t bufsz = 256;
  //Add implict conversion to const char* to util::array to use as
  //the buffer type, for convenience.
  // struct buf_type : util::array<char,bufsz> {
  //   using util::array::array;
  //   operator char*(){
  //     return data();
  //   }
  // };
  //We need to use as explicit constructor to get the initial size to be 0
  util::array<char,bufsz> buf = util::array<char,bufsz>('\0',0);

  //strings of this size or smaller are always copied to avoid
  //flushing the buffer excessively.

  static constexpr size_t small_string_limit = 16;

  constexpr size_t size() const {
    return sz + buf.size();
  }
  void ensure_buf_sz(size_t count){
    if(buf.capacity() < count){
      flush_internal_buffer();
    }
  }
  bool buf_can_fit(size_t count){
    if(buf.capacity() < count){
      flush_internal_buffer();
      return (count < bufsz);
    }
    return true;
  }
  //Internal funciton, assumes dst can hold size() bytes.
  //Concatenates all elements of vec into dst, then clears vec.
  //If buf is not empty copies and clears it as well.
  void concatenate_internal_vector(char *dest){
    //const char *start = dest;
    for (const auto &sv : vec) {
      dest = (char*)mempcpy(dest, sv.data(), sv.size());
    }
    if(buf.size() > 0){
      this->sz += buf.size();
      dest = (char*)mempcpy(dest, buf.data(), buf.size());
      buf.clear();
    }
    *dest = '\0';
    vec.clear();
  }
  //Combine all string_views in buf into a single string_view using
  //a newly allocated block of size()+1 bytes.
  void compress_internal_vector(){
    if((vec.size() > 1) || (buf.size() > 0)){ //Don't do unecessary work
      char *dst = (char*)malloc(size()+1);
      concatenate_internal_vector(dst);
      vec.emplace_back(dst, sz, util::string_view::flag_both);
      assert(buf.size() == 0);
    }
  }
  void flush_internal_buffer(){
    if(buf.size() > 0){
      vec.emplace_back(buf.data(), buf.size(), true);
      sz += buf.size();
      buf.clear();
    }
  }
  void clear(){
    buf.clear();
    vec.clear();
    sz = 0;
  }
  //Return a std::string containing a copy of the buffer's contents.
  //compresses internal buffer, if necessary.
  //Potentially allocates memory twice.
  std::string to_string(){
    compress_internal_vector();
    return vec[0].to_std_string();
  }
  //Destructively convert the buffer into a std::string
  //Only ever allocates memory once, but leaves the internal buffer empty.
  std::string move_to_string(){
    std::string ret(size(), '\0');//allocate sz bytes owned by a std::string
    char *mem = ret.data(); //legal in C++17
    concatenate_internal_vector(mem);
    sz = 0;
    return ret;
  }
  //Compress internal buffer and get a view into it, either as a
  //std::string_view, util::string_view or raw c string.
  std::string_view std_string_view(){
    compress_internal_vector();
    return vec[0].to_std_string_view();
  }
  string_view to_string_view(){
    compress_internal_vector();
    return vec[0];
  }
  const char* c_str(){
    return to_string_view().data();
  }
  //Compress internal buffer, then transfer owership of that memory
  //to a newly created string_view and return that. This avoids multiple
  //allocations and leaves the string_view_buf empty.
  string_view move_to_string_view(){
    compress_internal_vector();
    string_view ret(std::move(vec[0]));
    vec.clear();
    sz = 0;
    return ret;
  }
  //flush buffer and append string_view(ptr, sz) to vec
  string_view_buf& append_view(const char *ptr, size_t sz){
    flush_internal_buffer();
    vec.emplace_back(ptr, sz);
    this->sz += sz;
    return (*this);
  }
  string_view_buf& append_bytes(const char *bytes, size_t sz){
    if(buf.capacity() < sz){
      flush_internal_buffer();
      //Append directly to vec if it won't fit in the buffer.
      if(sz > bufsz){
        vec.emplace_back(bytes, sz, true);
        this->sz += sz;
        return (*this);
      }
    }
    //warning this is super unsafe without being sure that
    //buf.size() + sz < buf.max_size();
    memcpy(buf.end(), bytes, sz);
    buf.set_length(buf.size() + sz);
    return (*this);
  }
  //Append a shallow copy of a string, as either a cstring,
  //std::string, std::string_view or string_view. Uses enable_if to ensure
  //only these types are allowed.
  template <typename T,
            std::enable_if_t<std::is_constructible_v<string_view, T>, int> = 0>
  string_view_buf& append(const T& str){
    util::string_view sv(str);
    if(sv.size() > small_string_limit){
      return this->append_view(sv.data(), sv.size());
    } else {
      return this->append_bytes(sv.data(), sv.size());
    }
  }
  //Append a deep copy of a string.
  template <typename T,
            std::enable_if_t<std::is_constructible_v<string_view, T>, int> = 0>
  string_view_buf& append_copy(const T& str){
    util::string_view sv(str);
    return this->append_bytes(sv.data(), sv.size());
  }

  //Append a deep/shallow copy of a string depending on a boolean argument.
  template <typename T,
            std::enable_if_t<std::is_constructible_v<string_view, T>, int> = 0>
  string_view_buf& append(const T& str, bool copy){
    return (copy ? append_copy(str) : append(str));
  }
  //alias of append_bytes
  string_view_buf& append(const char *str, size_t sz){
    return append_bytes(str, sz);
  }
  string_view_buf& append(char c){
    ensure_buf_sz(1);
    buf.push_back(c);
    return (*this);
  }

  //Template to allow for specializations with user defined types.
  template<typename T,
           std::enable_if_t<std::is_compound_v<T> &&
                            !std::is_constructible_v<string_view, T> &&
                            !std::is_same_v<void*, std::remove_cv_t<T>>, int> = 0>
  string_view_buf& append(const T& val);

  //Using 32 for the size is probably overkill, but it should be safe.
#define gen_append_fn(name, type, fmt)                          \
  string_view_buf& name(const type val){                             \
    ensure_buf_sz(32);                                          \
    size_t nbytes = snprintf(buf.end(), buf.capacity(), fmt, val); \
    assert(nbytes > 0 && nbytes < buf.capacity());              \
    buf.set_length(buf.size() + nbytes);                        \
    return (*this);                                             \
  }
  //gen_append_fn(append, char, "%c");
//Explicitly signed and unsigned chars are treated as numbers.
gen_append_fn(append, signed char, "%hhd");
gen_append_fn(append, unsigned char, "%hhu");
gen_append_fn(append, int, "%d");
gen_append_fn(append, unsigned int, "%u");
gen_append_fn(append_hex, unsigned int, "%x");
gen_append_fn(append, long, "%ld");
gen_append_fn(append, unsigned long, "%lu");
gen_append_fn(append_hex, unsigned long, "%lx");
gen_append_fn(append, long long, "%lld");
gen_append_fn(append, unsigned long long, "%llu");
gen_append_fn(append_hex, unsigned long long, "%llx");
gen_append_fn(append, void*, "%p");
//should these use %g?
gen_append_fn(append, float, "%f");
gen_append_fn(append, double, "%f");
gen_append_fn(append_hex, double, "%a");
#undef gen_append_fn

  string_view_buf& append_float(double val, int prec = 6, int width = 0,
                           char spec = 'd'){
    //alternatively you could pass prec and width to snprintf via
    //the use of "%*.*", but this way allows me to reuse
    //append_formated;
    char fmt[32];
    size_t nbytes = snprintf(fmt, sizeof(fmt),
                             "%%%d.%d%c", prec, width, spec);
    //If you use up 32 bytes with just 2 digits you did something
    //wrong, so just ignore it and print normally.
    if(nbytes >= sizeof(fmt)){
      fprintf(stderr,
              "Error, overlong format spec in string_view_buf::append_float.\n");
      //use the given spec, just ignore prec/width
      fmt[0] = '%'; fmt[1] = spec; fmt[2] = '\0';
    }
    append_formated(fmt, val);
    return (*this);
  }
//TODO: Maybe, Add a function to append a formatted number, with
//a given width, precision and printf flags, but without having to
//use a user provided string.

  //Append a string formatted as if by sprintf,
  template<typename ... Ts>
  string_view_buf& append_formated(const char *fmt, const Ts&... args){
    //We have no idea of the length so just try to append it to
    //the buffer and deal with failure if we need to.
    int nbytes = snprintf(buf.end(), buf.capacity(), fmt,  args...);
    if(nbytes < 0){
      fprintf(stderr, "Formatting error in string_view_buf::append_formatted,"
              "format string was \"%s\".\n", fmt);
      return (*this);
    }
    if((size_t)nbytes > buf.capacity()){
      flush_internal_buffer();
      if((size_t)nbytes > bufsz){
        char *str = (char*)malloc(nbytes+1);
        snprintf(str, nbytes+1, fmt, args...);
        vec.emplace_back(str, nbytes, string_view::flag_both);
      } else {
        snprintf(buf.data(), buf.capacity(), fmt,  args...);
        buf.set_length(nbytes);
      }
    } else {
      buf.set_length(buf.size() + nbytes);
    }
    return (*this);
  }
};
/*
  Simple string buffer which just copies everything to an internal
  dynamic array.
*/
struct string_buf {
  util::svector<char> buf{64}; 
  string_buf() = default;
  string_buf(size_t sz) : buf(sz) {}
  size_t size() const {
    return buf.size();
  }
  const char* data() const {
    return buf.data();
  }
  char* data() {
    return buf.data();
  }
  void null_terminate(){
    buf.push_back('\0');
  }
  void clear(){
    buf.clear();
  }
  std::string to_string() const {
    return std::string(buf.data(), buf.size());
  }
  std::string to_std_string() const {
    return std::string(buf.data(), buf.size());
  }
  //Destructively convert the buffer into a util::string,
  //memory is transfered from the buffer to the string.
  util::string move_to_string(){
    null_terminate();
    util::string ret{buf.data(), buf.size()-1, true};
    (void)buf.take_memory();
    return ret;
  }
  std::string_view std_string_view(){
    null_terminate();
    return std::string_view(buf.data(), buf.size()-1);
  }
  string_view to_string_view(){
    null_terminate();
    return string_view(buf.data(), buf.size()-1);
  }
  const char* c_str(){
    return to_string_view().data();
  }

  void shrink(size_t sz){
    assert(sz <= buf.size());
    buf.resize(sz);
  }
  //Compress internal buffer, then transfer owership of that memory
  //to a newly created string_view and return that. This avoids multiple
  //allocations and leaves the string_buf empty.
  string_view move_to_string_view(){
    null_terminate();
    util::string_view ret{buf.data(), buf.size()-1, string_view::flag_both};
    (void)buf.take_memory();
    return ret;
  }
  string_buf& append_bytes(const char *bytes, size_t sz){
    buf.append(bytes, sz);
    return (*this);
  }
  //Append a copy of a string like type.
  template <typename T,            
            std::enable_if_t<std::is_constructible_v<string_view, T>,
                             int> = 0>
  string_buf& append(const T& str){
    return append_bytes(std::data(str), std::size(str));
  }
  //alias of append_bytes
  string_buf& append(const char *str, size_t sz){
    return append_bytes(str, sz);
  }
  string_buf& append(const char *str){
    return append_bytes(str, strlen(str));
  }
  string_buf& append(char c){
    buf.push_back(c);
    return (*this);
  }

  
  //Template to allow for specializations with user defined types.
  template<typename T,
           std::enable_if_t<std::is_compound_v<T> &&
                            !std::is_constructible_v<string_view, T> &&
                            !std::is_same_v<void*, std::remove_cv_t<T>>, int> = 0>
  string_buf& append(const T& val);

  //Using 32 for the size is probably overkill, but it should be safe.
#define gen_append_fn(name, type, fmt)                          \
  string_buf& name(const type val){                             \
    buf.check_length(32);                                               \
    size_t nbytes = snprintf(buf.end(), buf.capacity() - buf.size(), fmt, val); \
    assert(nbytes > 0 && nbytes < buf.capacity());              \
    buf.set_length(buf.size() + nbytes);                        \
    return (*this);                                             \
  }
//Explicitly signed and unsigned chars are treated as numbers.
gen_append_fn(append, signed char, "%hhd");
gen_append_fn(append, unsigned char, "%hhu");
gen_append_fn(append, int, "%d");
gen_append_fn(append, unsigned int, "%u");
gen_append_fn(append_hex, unsigned int, "%x");
gen_append_fn(append, long, "%ld");
gen_append_fn(append, unsigned long, "%lu");
gen_append_fn(append_hex, unsigned long, "%lx");
gen_append_fn(append, long long, "%lld");
gen_append_fn(append, unsigned long long, "%llu");
gen_append_fn(append_hex, unsigned long long, "%llx");
gen_append_fn(append, void*, "%p");
//should these use %g?
gen_append_fn(append, float, "%f");
gen_append_fn(append, double, "%f");
gen_append_fn(append_hex, double, "%a");
#undef gen_append_fn

//TODO: Maybe, Add a function to append a formatted number, with
//a given width, precision and printf flags, but without having to
//use a user provided string.

  //Append a string formatted as if by sprintf,
  template<typename ... Ts>
  string_buf& append_formatted(const char *fmt, const Ts&... args){
    size_t space_left = buf.capacity() - buf.size();
    //We have no idea of the length so just try to append it to
    //the buffer and deal with failure if we need to.
    int nbytes = snprintf(buf.end(), space_left,
                          fmt,  args...);
    if(nbytes < 0){
      fprintf(stderr, "Formatting error in string_buf::append_formatted,"
              "format string was \"%s\".\n", fmt);
      return (*this);
    }
    if(nbytes >= space_left){
      buf.reserve(nbytes+1);
      nbytes = snprintf(buf.end(), nbytes + 1,
                        fmt,  args...);
    }
    buf.set_length(buf.size() + nbytes);
    return (*this);
  }
};
}//namespace util

#endif /*_STRING_BUF_H_*/
