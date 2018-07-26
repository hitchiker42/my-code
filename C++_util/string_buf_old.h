//Old version.
#if 0
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
  /*
  static constexpr size_t bufsz = 256;
  util::array<char,bufsz> buf;
*/

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
  template<typename ... Ts>
  string_buf& write(Ts&&... Args){
    append(std::forward<Ts>(Args)...);
  }
};
#endif
