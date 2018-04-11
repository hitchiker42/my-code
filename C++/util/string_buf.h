#ifndef _STRING_BUF_H_
#define _STRING_BUF_H_

#include "string_view.h"
#include "svector.h"
namespace util {
/*
  A possible optimization is to use a discriminated union and keep integers/floats
  as seperate types until we create the final string.
*/
struct string_buf {
  //we convert everything to a custom string view type, 
  //This should be std::vector<const string_view> but that doesn't compile
  //for some complicated reason.
  util::svector<util::string_view> vec;
  size_t sz = 0;//total size
  //internal buffer for string conversions
  static constexpr size_t bufsz = 128;
  char buf[bufsz];

  //Internal funciton, assumes dst can hold sz bytes.
  //Concatenates all elements of vec into dst, then clears vec.
  void concatenate_internal_buffer(char *dst){
    for (const auto &sv : vec) {
      const char *src = sv.data();
      memcpy(dst, src, sv.size());
      dst += sv.size();
    }
    *dst = '\0';
    vec.clear();
  }
  //Combine all string_views in buf into a single string_view using
  //a newly allocated block of sz bytes.
  void compress_internal_buffer(){
    if(vec.size() > 1){ //Don't do unecessary work if there's only one string
      char *dst = (char*)malloc(sz+1);
      concatenate_internal_buffer(dst);
      vec.emplace_back(dst, sz, string_view::flag_both);
    }
  }

  //Functions to convert the buffer into some kind of string

  //Return a std::string containing a copy of the buffer's contents.
  //compresses internal buffer, if necessary.
  //Potentially allocates memory twice.
  std::string to_string(){
    compress_internal_buffer();
    return vec[0].to_std_string();
  }
  //Destructively convert the buffer into a std::string
  //Only ever allocates memory once, but leaves the internal buffer empty.
  std::string move_to_string(){
    std::string ret(sz, '\0');//allocate sz bytes owned by a std::string
    char *mem = ret.data(); //legal in C++17
    concatenate_internal_buffer(mem);
    return ret;
  }
  //Convert into a std::string_view, the returned object is only a shallow
  //copy, meaning it is only valid during the lifetime of the string_buf object,
  //or whatever object owns the memory in vec[0]
  std::string_view to_std_string_view(){
    compress_internal_buffer();
    return vec[0].to_std_string_view();
  }
  string_view to_string_view(){
    compress_internal_buffer();
    return vec[0];
  }
  //Compress internal buffer, then transfer owership of that memory
  //to a newly created string_view and return that. This avoids multiple
  //allocations and leaves the string_buf in a valid staepte.
  string_view move_to_string_view(){
    compress_internal_buffer();
    //Even though string_view has no copy/move assignment operator this
    //is leagal (in C++17) due to mandatory copy elision.
    string_view ret = string_view(std::move(vec[0]));
    return ret;
  }

  //Append a shallow copy of a string, as either a cstring,
  //std::string, std::string_view or string_view. Uses enable_if to ensure
  //only these types are allowed.
  template <typename T,
            std::enable_if_t<std::is_constructible_v<string_view, T>, int> = 0>
  string_buf& append(const T& str){
    vec.emplace_back(str);
    sz += vec.back().size();
    return (*this);
  }
  //Append a deep copy of a string.
  template <typename T,
            std::enable_if_t<std::is_constructible_v<string_view, T>, int> = 0>
  string_buf& append_copy(T str){
    vec.emplace_back(str, true);
    sz += vec.back().size();
    return (*this);
  }
  //Append a deep/shallow copy of a string depending on a boolean argument.
  template <typename T,
            std::enable_if_t<std::is_constructible_v<string_view, T>, int> = 0>
  string_buf& append(T str, bool copy){
    return (copy ? append_copy(str) : append(str));
  }
#define gen_append_fn(name, type, fmt)                        \
  string_buf& name(const type val){                           \
    size_t nbytes = snprintf(buf, bufsz, fmt, val);     \
    assert(nbytes < bufsz);                             \
    vec.emplace_back(buf, nbytes, true, true);          \
    sz += nbytes;                                       \
    return (*this);                                     \
  }
gen_append_fn(append, int, "%d");
gen_append_fn(append, unsigned int, "%u");
gen_append_fn(append_hex, unsigned int, "%x");
gen_append_fn(append, long, "%ld");
gen_append_fn(append, unsigned long, "%lu");
gen_append_fn(append_hex, unsigned long, "%lx");
gen_append_fn(append, long long, "%lld");
gen_append_fn(append, unsigned long long, "%llu");
gen_append_fn(append_hex, unsigned long long, "%llx");
//should these use %g?
gen_append_fn(append, float, "%f");
gen_append_fn(append, double, "%f");
gen_append_fn(append_hex, double, "%a");
#undef gen_append_fn

  //This struct isn't designed for appending single chars, but it should
  //at least provide the ability
  string_buf& append(const char c){
    buf[0] = c;
    buf[1] = '\0';
    vec.emplace_back(buf, 1, true, true);
    sz++;
    return (*this);
  }
  // template <typename T>
  // string_buf& append_object(const T& obj){
  //   static std::ostringstream ss;
  //   ss << obj;
  // }

//TODO: Maybe, Add a function to append a formatted number, with
//a given width, precision and printf flags, but without having to
//use a user provided string.


  //Append a string formatted as if by sprintf,
  template<typename ... Ts>
  string_buf& append_formated(const char *fmt, const Ts&... args){
    size_t nbytes = snprintf(buf, bufsz, fmt,  args...);
    if(nbytes > bufsz){
      char *str = (char*)malloc(nbytes+1);
      snprintf(str, nbytes+1, fmt, args...);
      vec.emplace_back(str, nbytes, string_view::flag_both);
    } else {
      vec.emplace_back(buf, nbytes, true, true);
    }
    sz += nbytes;
    return (*this);
  }
};
}//namespace util

#endif /*_STRING_BUF_H_*/

#if 0
//A streambuf using a simple block of characters as the output stream
//as well as the put area. Similar to a std::basic_stringbuf, except
//that it is possible to take the block of memory from it directly
//rather than just making a copy.
struct basic_membuf : std::basic_streambuf<char>  {
  using Base = std::basic_streambuf<char>;
  using Traits = std::char_traits<char>;

  static constexpr size_t initial_size = 32;
  size_t bufsz = 0;
  char *buf = nullptr;
  basic_membuf() {
    Base::setp(nullptr, nullptr);
  }
  basic_membuf(size_t sz) : bufsz{sz}, buf{(char*)calloc(sz,1)} {
    Base::setp(buf, buf+bufsz);
  }
  ~basic_membuf() {
    free(buf);
  }
  char* bufptr(){
    return Base::pptr();
  }
  ptrdiff_t buflen() {
    return (Base::pptr() - Base::pbase());
  }
  bool is_full(){
    return (Base::pptr() == Base::epptr());
  }
  //Replaces buf with a newly allocated block of a minimum new_sz
  //bytes, copies the memory from pbase()-pptr() to the new memory, 
  //and resets the values of pbase, pptr and epptr. Returns pptr()
  char* reallocate(size_t new_sz){
    size_t len = buflen();
    if(!len){
      bufsz = initial_size;
      buf = (char*)calloc(bufsz,1);
    } else {
      bufsz = std::max(new_sz, bufsz*2);
      char* newbuf = (char*)calloc(bufsz,1);
      memcpy(newbuf, buf, len);
      free(buf);
      buf = newbuf;
    }
    Base::setp(buf, buf+bufsz);
    Base::pbump(len);
    return (buf + len);
  }
    
  int overflow(int ch) override {
    printf("Calling Overflow\n");
    if (ch == Traits::eof()) {
      return ch;
    }
    char* ptr = reallocate(bufsz*2);
    *(ptr) = (char)ch;
    Base::pbump(1);
    return ch;
  }
  std::streamsize xsputn(const char *str, std::streamsize count) override {
    printf("Calling xsputn\n");
    auto needed = buflen() + count;
    if(needed < bufsz){
      memcpy(bufptr(), str, count);
      Base::pbump(count);
    } else {
      char *ptr = reallocate(needed);
      memcpy(ptr, str, count);
      Base::pbump(count);
    }
    return count;
  }
  //Return a copy of the output buffer as a std::string.
  std::string to_std_string(){
    return std::string(buf, buflen());
  }
  //Returns a string_view of the current output, is invalidated if
  //this goes out of scope, or if reallocate is called.
  string_view to_string_view(){
    return string_view(buf, buflen(), false, false);
  }
  string_view copy_to_string_view(){
    return string_view(buf, buflen(), true);
  }
  //Transfer ownership of the output buffer to a string_view and return it.
  string_view move_to_string_view(){
    //We use calloc for allocation, so unless the buffer is full the
    //string_view will be null terminated.
    uint8_t flags = (is_full() ? string_view::flag_owned : string_view::flag_both);
    auto ret = string_view(buf, buflen(), flags);
    bufsz = 0;
    buf = nullptr;
    //C++17 mandates that the copy/move must be elided here but 
    //Guaranteed to be equivalent to a move
    return ret;
  } 
};

struct basic_memstream : std::basic_ostream<char> {
  using Base = std::basic_ostream<char>;
  using Traits = std::char_traits<char>;
  using streambuf_type = basic_membuf;
  using ostream_type = std::basic_ostream<char>;

  streambuf_type buf;

  basic_memstream() : ostream_type(), buf() {
    this->init(&buf);
  }
  
  streambuf_type* rdbuf() const {
    return const_cast<streambuf_type*>(&buf);
  }
  string_view sv(){
    return buf.to_string_view();
  }
  string_view move_to_sv(){
    return buf.move_to_string_view();
  }
};
  
  
#endif
