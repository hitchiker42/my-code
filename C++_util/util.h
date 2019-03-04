#ifndef __UTIL_H__
#define __UTIL_H__
//C headers
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <limits.h>
//C++ headers
#include <algorithm>
#include <array>
#include <functional>
#include <initializer_list>
#include <iterator>
#include <numeric>
#include <map>
#include <type_traits>
#include <string>
#include <string_view>
#include <vector>
#include <utility>
#include <unordered_map>

#include "macros.h"
#include "templates.h"
#include "constexpr_fns.h"
#include "my_array.h"
#include "svector.h"
//TODO: This header shouldn't have this much code in it, move the
//      code to other headers, this should mostly just include
//      other headers.
using namespace std::literals::string_view_literals;
using namespace std::literals::string_literals;
namespace util {
inline constexpr uintptr_t
pointer_tag_bitmask = (sizeof(uintptr_t) == 8 ? 0x7 : 0x3);
inline constexpr uintptr_t
tagged_pointer_bitmask = ~pointer_tag_bitmask;

template <typename T>
int compare(const T& lhs, const T& rhs){
  return three_way_compare(lhs, rhs);
}
//some templates that rely on other class definations
inline namespace templates {
//std::unordered map extended with overloads to operator()
//to lookup values and return either a pointer or default value.
template<typename K, typename V,
         class Hash = std::hash<K>,
         class KeyEq = std::equal_to<K>,
         class Allocator = std::allocator<std::pair<const K, V>>>
struct unordered_map : std::unordered_map<K,V,Hash,KeyEq,Allocator> {
  using std::unordered_map<K,V,Hash,KeyEq,Allocator>::unordered_map;
  V* operator()(const K& key) const {
    auto it = this->find(key);
    if(it != this->end()){
      return &(it->second);
    } else {
      return nullptr;
    }
  }
  const V& operator()(const K& key, const V& deflt) const {
    auto it = this->find(key);
    if(it != this->end()){
      return it->second;
    } else {
      return deflt;
    }
  }
};
#if 0
/*
  Returns a pair of bucket iterators, the first points to the
  first instance of key, the second is the end iterator of the bucket.
*/
template<typename K, typename V>
decltype(auto) //since the return type is a mess
unordered_multimap_find_first(std::unordered_multimap<K,V> &ht, const K& key){
  auto idx = ht.bucket(key);
  auto start = ht.begin(idx);
  auto stop = ht.end(idx);
  auto cmp = ht.key_eq();
  while(!cmp(start->first, key) && start != stop){
    ++start;
  }
  return std::make_pair(start,stop);
}
/*
  Returns the equivlent of:
  std::make_pair(ht.equal_range(key).first, ht.count(key));
  without having to iterate across the range multiple times.
*/
template<typename K, typename V>
decltype(auto)
unordered_multimap_equal_range_count(std::unordered_multimap<K,V> &ht, const K& key){
  auto [start, stop] = unordered_multimap_find_first(ht, key);
  decltype(ht.count(key)) count = 0;
  auto cmp = ht.key_eq();
  if(start != stop){
    auto next = start;
    do {
      ++count;
    } while(++next != stop && cmp(key, next->first));
  }
  return std::make_pair(start, count);
}
#endif
} // namespace templates
} // namespace util
//Functions to make working with tagged pointers eaiser,

static constexpr inline int tolower_ascii(int c){
  return (c > 0x40 && c < 0x5B ? c | 0x20 : c);
}
static inline void string_tolower(char *str){
  while(*str){
    *str = tolower_ascii(*str);
    ++str;
  }
};
static inline void string_tolower(std::string *str){
  string_tolower((char*)str->data());
}
static inline void string_tolower(std::string str){
  string_tolower((char*)str.data());
}
static inline std::string string_tolower(const std::string_view str){
  std::string ret(str.size(), '\0');
  for(size_t i = 0; i < str.size(); i++){
    ret[i] = tolower_ascii(str[i]);
  }
  return ret;
}

int string_compare_natural(const char *a, const char *b,
                           bool foldcase = false, bool ignore_leading_zeros = false);
int string_compare_natural(const std::string &a, const std::string &b,
                           bool foldcase = false, bool ignore_leading_zeros = false);
//Wrapper around strcmp that can be used in stl algorithms.
static inline bool strcmp_less(const char *str1, const char *str2){
  return strcmp(str1, str2) < 0;
}
static inline bool is_nonempty_string(const char *s){
  return (s && s[0]);
}
static inline bool is_empty_string(const char *s){
  return (!s || !s[0]);
}
static inline bool is_prefix_of(const std::string_view prefix,
                                const std::string_view sv){
  if(prefix.size() > sv.size()){ return false; }
  return strncmp(prefix.data(), sv.data(), prefix.size()) == 0;
}
static inline bool has_prefix(const std::string_view sv,
                              const std::string_view prefix){
  return is_prefix_of(prefix, sv);
}
/*
  Simple (if inefficent) definitions of some gnu extension functions for
  non gnuc compilers (aka microsoft).
*/
#ifndef __GNUC__
typedef fs::file_status fs_status;
static inline void* mempcpy(void *dest, const void* src, size_t n){
  memcpy(dest, src, n);
  return dest + n;
}
//defined in util.cpp as a helper function, may as well export it.
static inline char* stpcpy(char *dest, const char *src){
  size_t len = strlen(src);
  return (char*)mempcpy(dest, src, len);
}
static inline char* strchrnul(const char *str, int c){
  size_t n = strlen(str);
  return ((char*)memchr(str, c, n) || (str + n));
}
#endif

#if 0
//Convert a number into a string stored in a static buffer. This is
//only meant to be used in cases where the result is immediately copied,
//eg. by appending it to another string.
template <typename T> char *to_chars_static(T val);
extern template char *to_chars_static(int val);
extern template char *to_chars_static(unsigned int val);
extern template char *to_chars_static(long val);
extern template char *to_chars_static(unsigned long val);
extern template char *to_chars_static(double val);
extern template char *to_chars_static(float val);

struct bitset_8 {
  uint8_t bits;
  bool operator[] (size_t i) const {
    return (bits & (1 << i));
  }
};
struct bitset_16 {
  uint16_t bits;
  bool operator[] (size_t i) const {
    return (bits & (1 << i));
  }
};
struct bitset_32 {
  uint32_t bits;
  bool operator[] (size_t i) const {
    return (bits & (1 << i));
  }
};
#endif
/*
  In case you need a custom hash function.
*/
namespace util {
/*
  templatized version.
  template<typename T> static constexpr T fnv_prime;
  template<typename T> static constexpr T fnv_offset_basis;
  template<> static constexpr T fnv_prime<uint32_t> = 16777619U;
  template<> static constexpr T fnv_offset_basis<uint32_t> = 2166136261U;
  template<> static constexpr uint64_t fnv_prime = 1099511628211UL;
  template<> static constexpr uint64_t fnv_offset_basis = 0xCBF29CE484222325UL;
  //Versions for 128, 256, 512 and 1024 bytes also exist.
*/
static constexpr uint64_t fnv_prime_64 = 1099511628211UL;
static constexpr uint64_t fnv_offset_basis_64 = 14695981039346656037UL;
static constexpr uint32_t fnv_prime_32 = 16777619U;
static constexpr uint32_t fnv_offset_basis_32 = 2166136261U;
//TODO, maybe: make this a template and specialize it.
[[maybe_unused]] static uint64_t fnv_hash(const void *key, size_t keylen,
                                          uint64_t seed = fnv_offset_basis_64){
  const unsigned char *raw_data = reinterpret_cast<const unsigned char*>(key);
  uint64_t hash = seed;
  for(size_t i=0; i < keylen; i++){
    hash = (hash ^ raw_data[i]) * fnv_prime_64;
  }
  return hash;
}
extern uint64_t murmur_hash(const void *key, size_t keylen, uint64_t seed);
template<typename T>
uint64_t fnv_hash(const T& key){
  return fnv_hash(&key, sizeof(T));
}
template<typename T,
         std::enable_if_t<is_raw_data_v<T>, int> = 0>
uint64_t fnv_hash(const T& key){
  return fnv_hash(std::data(key), std::size(key) * sizeof(*std::data(key)));
}
template<typename T,
         std::enable_if_t<is_iterable_v<T> && !is_raw_data_v<T>, int> = 0>
uint64_t fnv_hash(const T& key){
  uint64_t hash = fnv_offset_basis_64;
  for(const auto &elt : key){
    hash = hash_combine(hash, fnv_hash(elt));
  }
  return hash;
}

template<>
inline uint64_t fnv_hash(const char* const& key){
  return fnv_hash(key, strlen(key));
}
//copy a string_view to a malloc'd buffer
static inline char *strdup_sv(std::string_view sv){
  char *buf = (char*)malloc(sv.size() + 1);
  memcpy(buf, sv.data(), sv.size());
  buf[sv.size()] = '\0';
  return buf;
}
//Like strdup but with an explicit size argument
static inline void *memdup(const void *src, size_t sz){
  void *dest = malloc(sz);
  memcpy(dest, src, sz);
  return dest;
}
//Wrappers around strtoX which return a std::optional value, internally
//they use errno to check the result.
std::optional<long> strtol_opt(const char *str,
                               const char** endptr = nullptr, int base = 0);
std::optional<unsigned long> strtoul_opt(const char *str,
                                         const char** endptr = nullptr,
                                         int base = 0);
std::optional<double> strtod_opt(const char *str,
                                 const char** endptr = nullptr, int base = 0);
//extern "C" {
/*inline constexpr size_t memspn_table(const uint8_t *str, size_t len,
                                     const uint8_t accept[256]){
  unsigned int i=0;
  //this is for speed, but I'm not sure how much it's worth it
  while(i+4 <= len){
    if(!accept[str[i]]){return i;}
    if(!accept[str[i+1]]){return i+1;}
    if(!accept[str[i+2]]){return i+2;}
    if(!accept[str[i+3]]){return i+3;}
    i+=4;
  }
  switch(len-i){
    case 3: if(!accept[str[i]]){return i;} i++;
    case 2: if(!accept[str[i]]){return i;} i++;
    case 1: if(!accept[str[i]]){return i;}
  }
  return len;
}
inline constexpr size_t memcspn_table(const uint8_t *str, size_t len,
                                      const uint8_t reject[256]){
  unsigned int i=0;
  while(i+4 <= len){
    if(reject[str[i]]){return i;}
    if(reject[str[i+1]]){return i+1;}
    if(reject[str[i+2]]){return i+2;}
    if(reject[str[i+3]]){return i+3;}
    i+=4;
  }
  switch(len-i){
    case 3: if(reject[str[i]]){return i;} i++;
    case 2: if(reject[str[i]]){return i;} i++;
    case 1: if(reject[str[i]]){return i;}
  }
  return len;
}*/
inline constexpr size_t memspn_table(const void *str, size_t len,
                                     const uint8_t accept[256]){
  size_t i = 0;
  while(i < len && accept[((uint8_t*)str)[i]]){
    i++;
  }
  return i;
}
inline constexpr size_t memcspn_table(const void *str, size_t len,
                                      const uint8_t reject[256]){
  size_t i = 0;
  while(i < len && !reject[((uint8_t*)str)[i]]){
    i++;
  }
  return i;
}
inline constexpr size_t memrspn_table(const void *str, size_t len,
                                      const uint8_t accept[256]){
  ssize_t i = len-1;
  while(i >= 0 && accept[((uint8_t*)str)[i]]){
    i--;
  }
  return i;
}
inline constexpr size_t memrcspn_table(const void *str, size_t len,
                                       const uint8_t reject[256]){
  ssize_t i = len-1;
  while(i >= 0 && !reject[((uint8_t*)str)[i]]){
    i--;
  }
  return i;
}
//Needs to be a macro since it deals with stack allocated memory.
#define build_span_table(table, bytes, len)                     \
  for(unsigned int i = 0; i < len; i++){                        \
    table[((uint8_t*)bytes)[i]] = 1;                            \
  }
inline constexpr size_t memspn(const void *buf, size_t len,
                               const void *accept, size_t len2){
  uint8_t bytes[256] = {0};
  build_span_table(bytes, accept, len2);
  return memspn_table(buf, len, bytes);
}
inline constexpr size_t memcspn(const void *buf, size_t len,
                                const void *reject, size_t len2){
  uint8_t bytes[256] = {0};
  build_span_table(bytes, reject, len2);
  return memcspn_table(buf, len, bytes);
}
inline constexpr size_t memrspn(const void *buf, size_t len,
                                const void *accept, size_t len2){
  uint8_t bytes[256] = {0};
  build_span_table(bytes, accept, len2);
  return memrspn_table(buf, len, bytes);
}
inline constexpr size_t memrcspn(const void *buf, size_t len,
                                 const void *reject, size_t len2){
  uint8_t bytes[256] = {0};
  build_span_table(bytes, reject, len2);
  return memrcspn_table(buf, len, bytes);
}
#undef build_span_table
//} //extern "C"
} // Namespace util

#if 0
//Wrappers around strtoX which internally zero errno and set it to
//EINVAL if no conversion is performed, and also have sane defaults
//for endptr and base.
//Declared in the top level namspace to avoid possible issues
//from hiding the standard strtoX functions in the util namespace
//and since util::util_strtol is just a bit too verbose.

//This is a bit ugly, but it's better than writing the same code 3 times
//and using templates is just as verbose
#define strtoX_init()                           \
  errno = 0;                                    \
  const char *end;                              \
  if(!endptr){                                  \
    endptr = &end;                              \
  }
#define strtoX_finish()                         \
  if(str == *endptr){                           \
    errno = EINVAL;                             \
  }                                             \
  return ret;

inline long long util_strtol(const char* str,
                             const char** endptr = nullptr, int base = 0){
  strtoX_init();
  long long ret = strtoll(str, endptr, base);
  strtoX_finish();
}
inline unsigned long long util_strtoul(const char* str,
                           const char** endptr = nullptr, int base = 0){
  strtoX_init();
  unsigned long long ret = strtoll(str, endptr, base);
  strtoX_finish();
}
inline double util_strtoul(const char* str,
               const char** endptr = nullptr, int base = 0){
  strtoX_init();
  unsigned long long ret = strtoll(str, endptr, base);
  strtoX_finish();
}
#undef strtoX_init
#undef strtoX_finish
#endif

#if (defined NEED_TIMER) || (defined NEED_TIMERS)
#include "time_util.h"
#endif
/*
namespace util {
template <typename T> inline const char *printf_spec = nullptr;
template <> inline const char *printf_spec<char> = "%c";
template <> inline const char *printf_spec<signed char> = "%hhd";
template <> inline const char *printf_spec<unsigned char> = "%hhu";
template <> inline const char *printf_spec<int> = "%d";
template <> inline const char *printf_spec<unsigned int> = "%u";
template <> inline const char *printf_spec<long> = "%ld";
template <> inline const char *printf_spec<unsigned long> = "%lu";
template <> inline const char *printf_spec<long long> = "%lld";
template <> inline const char *printf_spec<unsigned long long> = "%llu";
//template <> inline const char *printf_spec<size_t> = "%zu";
//template <> inline const char *printf_spec<ssize_t> = "%zd";
template <> inline const char *printf_spec<double> = "%f";
template <> inline const char *printf_spec<float> = "%f";
template <> inline const char *printf_spec<void*> = "%p";
}
*/
#endif /* __UTIL_H__ */
