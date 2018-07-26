#ifndef __MY_STRING_H__
#define __MY_STRING_H__
namespace util {
//TODO: make this allocator aware;
//Immutable string which owns its own memory.
struct string {
  typedef const char* pointer;
  typedef const char& reference;
  typedef char value_type;
  typedef size_t size_type;
  typedef ptrdiff_t difference_type;
  typedef pointer iterator;
  //max size of a string stored it the object, includes the null terminator;
  //on 64 bit platforms its only twice the size of a pointer, so it doesn't
  //increase the size much.
  static constexpr int small_string_size = 16;
  union {
    const char* ptr = nullptr;
    char buf[small_string_size];
  };
  size_type sz = 0;//doesn't include null terminator.
  //Trivial constructors
  constexpr string() noexcept : sz{0} {};
  //This is the only constructor that lets you transfer ownership of memory
  //to a string, this is rarely useful outside of the move constructor.
  constexpr string(const char *str, size_type len, bool nocopy) noexcept 
    : sz{len} {
    if(!nocopy || (len < small_string_size)){
      new (this) string(str, len);
    } else {
      ptr = str;
    }
  }
  //verison that takes a void* so we don't need to cast the
  //return value of malloc
  constexpr string(void *ptr, size_type sz) noexcept
    : ptr{(const char *)ptr}, sz{sz} {}
  //main constructor
  constexpr string(const char *str, size_type len) noexcept
    : sz{len} {
    if(sz  < small_string_size){
      memcpy(buf, str, sz);
      buf[sz] = '\0';
    } else {      
      char* tmp = (char*)malloc(sz+1);
      memcpy(tmp, str, sz);
      tmp[sz] = '\0';
      ptr = tmp;
    }
  }
  //use data & size rather than direct accessors, especially since
  //we can't get a data pointer directly without checking the size.
  constexpr string(const string &other) noexcept 
    : string(other.data(), other.size()) {}
  //Move constructor, sets other to the empty string.
  constexpr string(string&& other) noexcept {
    //memcpy not allowed in constexpr
    for(int i = 0; i < small_string_size; i++){
      buf[i] = other.buf[i];
    }
    sz = other.sz;
    other.buf[0] = '\0';
  }
  //strings are immutable, the copy assigment/move operators are
  //really just to allow reusing a variable
  string& operator=(string &other) {
    this->~string();
    new(this) string(other);
    return *this;
  }
  string& operator=(string &&other) {
    this->~string();
    new(this) string(other);
    return *this;
  }
  constexpr string(const char* cstr) noexcept
    : string(cstr, constexpr_strlen(cstr)) {}
  string(const std::string& str) noexcept
    : string(str.data(), str.size()) {}
  constexpr string(const std::string_view& sv) noexcept
    : string(sv.data(), sv.size()) {}

  ~string() {
    if(sz >= small_string_size){
      free((void*)ptr);
    }
  }
  std::string to_std_string() const {
    return std::string(data(), size());
  }
  constexpr std::string_view to_std_string_view() const {
    return std::string_view(data(), size());
  }
  //Implicit conversion to std::string_view
  constexpr operator std::string_view() const {
    return this->to_std_string_view();
  }
  //Standard container functions.
  constexpr const char* data() const {
    if(sz >= small_string_size){
      return ptr;
    } else {
      return buf;
    }
  }
  constexpr size_t size() const {
    return sz;
  }
  constexpr char operator[](size_type idx) const {
    return data()[idx];
  }
  constexpr iterator begin() const {
    return iterator(data());
  }
  constexpr iterator end() const {
    return iterator(data()+sz);
  }
  //behaves the same as std::string::compare
  constexpr int compare(const string &other) const {
    size_type len = std::min(sz, other.sz);
    int cmp_result = constexpr_strncmp(data(), other.data(), len);
    if(cmp_result == 0){
      return three_way_compare(sz, other.sz);
    } else {
      return cmp_result;
    }
  }
  #define do_cmp(lhs, rhs) lhs.compare(rhs)
  generate_comparison_operators_via_compare(string, do_cmp);
  #undef do_cmp

  //Substring functions, return string_views 
  string_view substr(size_t start) const {
    return substr(start, sz);
  }
  string_view substr(size_type start, size_type end) const {
    return string_view(ptr + start, end - start, false, false);
  }
  //Substring functions which return a copy, this is the only way to get
  //null terminated substrings.
  string substr_copy(size_t start) const {
    return substr_copy(start, sz);
  }
  string substr_copy(size_type start, size_type end) const {
    return string(ptr + start, end - start);
  }
};
//Copying the std libraries naming hierarchy.
//string can't be a literal b/c of non-trival destructor.
namespace literals {
inline namespace string_literals {
  inline string operator "" _s(const char *str, size_t sz){
    return string{str, sz};
  }
}
}
}

namespace std {
//Specialization of std::hash such that a hash of a util::string 
//is the same as a std::string/string_view with the same contents. 
template<> struct hash<::util::string> {
  size_t operator()(const ::util::string& arg){
    return std::hash<std::string_view>{}(arg.to_std_string_view());
  }
};
}
#endif /* __MY_STRING_H__ */
