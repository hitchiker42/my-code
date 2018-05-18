#ifndef __STRING_VIEW_H__
#define __STRING_VIEW_H__
#include <string.h>
#include <stdlib.h>
#include <stdint.h>
#include <string>
#include <string_view>
#include <iterator>
#include <functional> //for std::hash
#include "templates.h"
namespace util {
/*
  Custom string_view class, holds an immutable string, whose
  memory may or may not be owned by the object.

  Stores a pointer to the string, the size of the string and
  flags indicating if the string is owned, and if it is null terminated.

  Size is limited to 4GB, since we use a 32 bit int to hold the size,
  to insure we don't use more space than a std::string_view (on a 64 bit system)

  We could use the high bits of a size_t to store the flags
  to allow larger strings

  When copying a null terminated string the null terminator is also copied.
*/
struct string_view {
  using pointer =  const char*;
  using reference = const char&;
  using value_type = char;
  using size_type = uint32_t;
  using difference_type = ptrdiff_t;
  using iterator =  pointer;

  const char* ptr;
    //We could store the flag bits in the top 2 bits of the size, this
  //would get us a 62 bit size type, at the cost of code complexity
  //and overhead in masking/unmasking the flag bits.
  // struct size_and_flags {
  //   constexpr uint64_t mask = (0x3ull << 62);
  //   uint64_t val;
  //   size_and_flags(uint64_t sz, uint8_t flags)
  //     : val{sz & (((uint64_t)flags) << 62)} {};
  //   uint64_t size(){
  //     return val & ~mask;
  //   }
  //   uint8_t flags(){
  //     return (uint8_t)(val >> 62);
  //   }
  //   uint64_t set_size(uint64_t sz){
  //     val &= mask; //clear old size
  //     val |= sz; //set new size
  //     return sz;
  //   }
  //   uint8_t set_flags(uint8_t flags){
  //     val &= ~mask;
  //     val |= ((uint64_t)flags << 62);
  //   }
  // };

  size_type sz;
  uint8_t flags;
  //int padding : 24;
  //Maybe TODO: make flags into an enum.
  static constexpr uint8_t flag_owned = 0x1;
  static constexpr uint8_t flag_null_terminated = 0x2;
  static constexpr uint8_t flag_both = 0x3;
  //Trivial constructors
  constexpr string_view() noexcept : ptr{nullptr}, sz{0}, flags{0} {}
  //This is the only constructor that lets you transfer ownership of memory
  //to a string_view, this is rarely useful outside of the move constructor.
  constexpr string_view(const char *ptr, size_t sz, uint8_t flags) noexcept
    : ptr{ptr}, sz{(size_type)sz}, flags{flags} {}
  //verison that takes a void* so we don't need to cast the
  //return value of malloc
  constexpr string_view(void *ptr, size_t sz, uint8_t flags) noexcept
    : ptr{(const char *)ptr}, sz{(size_type)sz}, flags{flags} {}
  //main constructor, allocates a new string if copy is true.
  //null_terminated as argument is only relevent if copy is false.
  constexpr string_view(const char *ptr, size_t sz, bool copy = false,
                        bool null_terminated = true) noexcept
    : ptr{(copy ? copy_string(ptr, (size_type)sz) : ptr)}, sz{(size_type)sz},
      flags{(uint8_t)(copy | ((uint8_t)(null_terminated || copy) << 1))} {}

  //Copying a string_view to another string view implicitly will always
  //make a shallow copy, so be careful about dangling pointers.
  constexpr string_view(const string_view &sv) noexcept
    : ptr{sv.ptr}, sz{sv.sz}, flags{(uint8_t)(sv.flags & ~flag_owned)} {};
  //Move constructor, if sv owns storage than ownership is transfered
  //to the new string_view, sv is still valid afterward, but will no longer
  //attempt to free memory when destructed.
  constexpr string_view(string_view&& sv) noexcept
    : ptr{sv.ptr}, sz{sv.sz}, flags{sv.flags} {
    sv.flags &= ~flag_owned;
  } 
  //string views are immutable, the copy assigment/move operators are
  //really just to allow reusing a variable
  string_view& operator=(string_view &other) {
    this->~string_view();
    new(this) string_view(other);
    return *this;
  }
  string_view& operator=(string_view &&other) {
    this->~string_view();
    //Need to make sure to use std::move here (which is kinda weird).
    new(this) string_view(std::move(other));
    return *this;
  }

  constexpr string_view(const char* cstr, bool copy = false) noexcept
    : string_view(cstr, constexpr_strlen(cstr), copy, true) {}
  //Constructing a string_view from a std::string or
  //std::string_view must be done explicitly (why?)
  explicit string_view(const std::string& str, bool copy = false) noexcept
    : string_view(str.data(), (size_type)str.size(), copy, true) {}
  //when using a string_view we can only guarantee that we're null terminated
  //if we make a copy.
  explicit constexpr string_view(const std::string_view& sv, bool copy = false) noexcept
    : string_view(sv.data(), (size_type)sv.size(), copy, copy) {}

  //Destructor frees memory if it is owned.
  ~string_view() {
    if(flags & flag_owned){
      free((void*)ptr);
    }
  }
  //copy str, always append a null terminator to the new string.
  static const char* copy_string(const char *str, size_type sz) {
    char *buf = (char*)malloc(sz + 1);
    memcpy(buf, str, sz);
    buf[sz] = '\0';
    return (const char*)buf;
  }
  constexpr bool owns_memory() const {
    return (flags & flag_owned);
  }
  constexpr bool is_null_terminated() const {
    return (flags & flag_null_terminated);
  }
  //Creates a copy of the string view with the same flag bits,
  //meaning if this string_view owns its memory we allocate a new
  //copy of the memory so the new string_view also owns its own memory.
  string_view clone(){
    return string_view(ptr, sz, owns_memory(),
                       owns_memory() || is_null_terminated());
  }

  std::string to_std_string() const {
    return std::string(data(), size());
  }
  constexpr std::string_view to_std_string_view() const {
    return std::string_view(data(), size());
  }
  //Implicit conversion to std::string_view (make sure to keep `this` in
  //scope if it owns memory).
  constexpr operator std::string_view() const {
    return this->to_std_string_view();
  }
  //Standard container functions.
  constexpr const char* data() const {
    return ptr;
  }
  constexpr size_t size() const {
    return sz;
  }
  constexpr char operator[](size_type idx) const {
    return ptr[idx];
  }
  constexpr iterator begin() const {
    return iterator(ptr);
  }
  constexpr iterator end() const {
    return iterator(ptr+sz);
  }
  //behaves the same as std::string_view::compare
  constexpr int compare(const string_view &other) const {
    size_type len = std::min(sz, other.sz);
    int cmp_result = constexpr_strncmp(data(), other.data(), len);
    if(cmp_result == 0){
      return three_way_compare(sz, other.sz);
    } else {
      return cmp_result;
    }
  }
  int compare(const std::string &other) const {
    return other.compare(this->to_std_string_view());
  }
  constexpr int compare(const std::string_view &other) const {
    return other.compare(this->to_std_string_view());
  }
  constexpr int compare(const char *other) const {
    size_t len = strlen(other);
    int cmp_result = strncmp(data(), other, std::min(size(), len));
    return (cmp_result ? cmp_result : three_way_compare(size(), len));
  }
  
  friend bool operator==(const util::string_view& lhs, const std::string_view rhs){
    return lhs.compare(rhs) == 0;
  }
  friend bool operator==(const std::string_view lhs, const util::string_view& rhs){
    return rhs.compare(lhs) == 0;
  }
  friend bool operator==(const util::string_view& lhs, const std::string rhs){
    return lhs.compare(rhs) == 0;
  }
  friend bool operator==(const std::string lhs, const util::string_view& rhs){
    return rhs.compare(lhs) == 0;
  }
  friend bool operator==(const util::string_view& lhs, const char* rhs){
    return lhs.compare(rhs) == 0;
  }
  friend bool operator==(const char* lhs, const util::string_view& rhs){
    return rhs.compare(lhs) == 0;
  }  
  #define do_cmp(lhs, rhs) lhs.compare(rhs)
  generate_comparison_operators_via_compare(string_view, do_cmp);
  #undef do_cmp

  //Substring functions
  string_view substr(size_t start) const {
    return substr(start, size());
  }
  string_view substr(size_type start, size_type stop) const {
    return string_view(ptr + start, stop - start, false, false);
  }
  string_view substr(iterator start) const {
    return string_view(start, end() - start, false, is_null_terminated());
  }
  string_view substr(iterator start, iterator stop) const {
    return string_view(start, stop - start, false, false);
  }
  //Substring functions which return a copy, this is the only way to get
  //arbitrary null terminated substrings.
  string_view substr_copy(size_t start) const {
    return substr_copy(start, sz);
  }
  string_view substr_copy(size_type start, size_type stop) const {
    return string_view(ptr + start, stop - start, true);
  }
};
//Copying the std libraries naming hierarchy.
namespace literals {
inline namespace string_literals {
  inline string_view operator "" _sv(const char *str, size_t sz){
    return string_view{str, string_view::size_type(sz), false, true};
  }
}
}
}

namespace std {
//Specialization of std::hash such that a hash of a util::string_view 
//is the same as a std::string/string_view with the same contents. 
template<> struct hash<::util::string_view> {
  size_t operator()(const ::util::string_view& arg){
    return std::hash<std::string_view>{}(arg.to_std_string_view());
  }
};
}
#endif /* __STRING_VIEW_H__ */
