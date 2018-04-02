#ifndef __UTIL_H__
#define __UTIL_H__
#include "common.h"
#include "templates.h"
#include "my_array.h"
#include "svector.h"
/*
  Low level bit manipulation, using compiler intrinsics
*/
//Macro to simplify generating functions from compilier intrinsics,
//the weird "if constexpr" part is because some intrinsics are
//undefined for 0, so we need to test for that, but some aren't and
//so we don't want to do a test for those.
#define gen_bitwise_fn_template(name, builtin, suffix, default_value)   \
  template <typename T,                                                 \
            std::enable_if_t<sizeof(T) <= sizeof(unsigned int), int> = 0> \
  constexpr int name(T x){                                              \
    if constexpr (default_value != 0){                                  \
      return (x ? builtin(x) : default_value);                            \
    } else {                                                              \
      return builtin(x);                                                  \
    }                                                                     \
  }                                                                   \
  template <typename T,                                                 \
            std::enable_if_t<(sizeof(T) > sizeof(unsigned int)), int> = 0> \
  constexpr int name(T x){                                              \
    if constexpr (default_value != 0){                                  \
      return (x ? CAT(builtin,suffix)(x) : default_value);            \
    } else {                                                          \
      return CAT(builtin,suffix)(x);                                    \
    }                                                                   \
  }


#if (defined __GNUC__)
//returns -1 if x is 0, since __builtin_ctz is undefined for 0
gen_bitwise_fn_template(ctz, __builtin_ctz, ll, -1);
gen_bitwise_fn_template(clz, __builtin_clz, ll, -1);
gen_bitwise_fn_template(popcount, __builtin_popcount, ll, 0);
#elif (defined _MSC_VER)
#include <intrin.h>
gen_bitwise_fn_template(ctz, _BitScanForward, 64, -1);
gen_bitwise_fn_template(clz, _BitScanReverse, 64, -1);
//Unlike the gcc version this will simply generate an invalid instruction
//for systems witout SSE4.1, so be careful
gen_bitwise_fn_template(popcount, __builtin_popcount, ll, 0);
#else
//These are all really naive algorithms, since I'm only really
//supporting GNUC compatible compiliers and visual studio they should
//never actually get used.
template <typename T>
constexpr int ctz(T x){
  //make sure we have an unsigned value
  auto y = std::make_unsigned_t<T>(x);
  int ret = 0;
  while(!(y & 1)){
    y >>= 1;
    ++ret;
  }
  return ret;
}
template <typename T>
constexpr int clz(T x){
  //make sure we have an unsigned value
  auto y = std::make_unsigned_t<T>(x);
  auto mask = (std::make_unsigned_t<T>(1) << ((sizeof(T)*CHAR_BIT)-1));
  int ret = 0;
  while(!(y & mask)){
    y <<= 1;
    ++ret;
  }
  return ret;
}
template <typename T>
constexpr int popcount(T x){
  //make sure we have an unsigned value
  auto y = std::make_unsigned_t<T>(x);
  int ret = 0;
  for(i = 0; i < sizeof(y)*CHAR_BIT; i++){
    ret += (y & (((decltype(y))1) << i));
  }
  return ret
};
#endif
template<typename T,
         std::enable_if_t<std::is_integral_v<T>,int> = 0>
constexpr int lg2(T x){
  return (std::max(sizeof(T),sizeof(int))*CHAR_BIT) - (clz(x) + 1);
}

#define define_rel_ops_generic(T, qualifier)            \
  qualifier bool operator !=(const T& a, const T& b){   \
    return !(a == b);                                   \
  }                                                     \
  qualifier bool operator <=(const T& a, const T& b){   \
    return a == b || a < b;                             \
  }                                                     \
  qualifier bool operator >(const T& a, const T& b){    \
    return !(a <= b);                                   \
  }                                                     \
  qualifier bool operator >=(const T& a, const T& b){   \
    return !(a < b);                                    \
  }
#define define_rel_ops_in_class(T)              \
  define_rel_ops_generic(T, friend)


//Functions and classes in namespace util.
namespace util {
template <typename T>
int compare(const T& lhs, const T& rhs){
  return three_way_compare(lhs, rhs);
}
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
}
//Functions to make working with tagged pointers eaiser,
//test if ptr is a tagged pointer, by default checks any of the bits
//that would be 0 for a word aligned pointer (i.e last 2 bits for
//a 32bit machine and last 3 for a 64 bit machine).
constexpr bool test_ptr_tag(const void *ptr, 
                            const int bit = 
                            (sizeof(uintptr_t) == 8 ? 0x7 : 0x3)){
  return (((uintptr_t)ptr) & bit);
}
//Needs to be a template sistatic nce a paramater of type void*&
//will only accept actual void*'s
//could change these into macros to eliminate the templates.
template<typename T>
const T*& set_ptr_tag(const T*& ptr, int bit){
  ptr = (T*)(((uintptr_t)ptr) | bit);
  return ptr;
}
template<typename T>
const T*& clear_ptr_tag(const T*& ptr, int bit){
  ptr = (T*)(((uintptr_t)ptr) & ~bit);
  return ptr;
}
template<typename T>
T get_tagged_ptr(T ptr){
  if constexpr(sizeof(uintptr_t) == 8){ // 64 bit
    return (T)((uintptr_t)ptr & ~0x7);
  } else { // 32 bit
    return (T)((uintptr_t)ptr & ~0x3);
  }
}
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
//These functions which ignore spaces exist because item names
//are compared ignoring spaces for some reason, and there actually
//exist items which differ only by spaces which should be compared as
//equal so it's not just a theoretical issue.
static inline std::string string_nospace(const std::string_view str){
  std::string ret(str.size(), '\0');
  int idx = 0;
  for(auto c : str){
    if(c != ' '){
      ret[idx++] = c;
    }
  }
  ret.resize(idx);
  return ret;
}
static inline std::string string_tolower_nospace(const std::string_view str){
  std::string ret(str.size(), '\0');
  int idx = 0;
  for(auto c : str){
    if(c != ' '){
      ret[idx++] = tolower_ascii(c);
    }
  }
  ret.resize(idx);
  return ret;
}

//compares str1 and str2 as if by strcmp but ignoring any spaces.
int strcmp_nospace(const char *str1, const char *str2);
//same as above, but additionally ignores case.
int strcasecmp_nospace(const char *str1, const char *str2);
#ifdef PLATFORM_WINDOWS
int strcasecmp(const char *str1, const char *str2);
#endif
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
//constexpr versions of C functions
static constexpr size_t constexpr_strlen(const char *s, size_t len = 0){
  return *s ? constexpr_strlen(s+1,len+1) : len;
}
static constexpr int constexpr_strcmp(const char *s, const char *s2){
  return (*s == *s2 ? (*s ? constexpr_strcmp(s+1,s2+1) : 0) : (*s - *s2));
}
static constexpr int constexpr_strncmp(const char *s, const char *s2, size_t n){
  return (*s == *s2 ?
          ((n && *s) ? constexpr_strncmp(s+1,s2+1,n-1) : 0) : (*s - *s2));
}
/**
 Creates a string to refer to 'count' instances of 'noun'
 If count == 1 outputs "'article' 'noun'",
  otherwise outputs "'count' 'noun''plural'".
 If 1 < count < 10, count is output as a word (eg "one", "two"),
 otherwise it is output as a number.

 Grammar is complicated, so trying to make this do anymore work than just
 selecting between singular and plural is a bad idea.
*/
std::string format_countable_noun(const std::string &article,
                                  const std::string &noun, int count,
                                  const std::string &plural = "s");

static inline std::string& capitalize(std::string *str){
  str->front() = std::toupper(str->front());
  return (*str);
}
static inline const char* possessive_suffix(const std::string &s){
  return (s.back() == 's' ? "'" : "'s");
}
constexpr const char* possessive_suffix(const char *s){
  if(*(s + (constexpr_strlen(s)-1)) == 's'){
    return "'";
  } else {
    return "'s";
  }
}
static inline std::string make_possessive(const std::string &s){
  return s + possessive_suffix(s);
}
/*
  Functions for uniform operations on std::strings, C strings, and std::string_views.
*/
static constexpr const char* as_c_str(const char *s){
  return s;
}
static constexpr size_t string_length(const char *s){
  return constexpr_strlen(s);
}
static inline const char* as_c_str(const std::string& s){
  return s.c_str();
}
static inline size_t string_length(const std::string& s){
  return s.size();
}
static constexpr size_t string_length(const std::string_view &s){
  return s.size();
}
//WARNING: There is no string_view c_str method, so this may cause errors
//if the string_view was not created from a c string.
static constexpr const char* as_c_str(const std::string_view &s){
  return s.data();
}

/*
  Convert a number into english.
*/
const std::string format_cardinal_number(long number);
const std::string format_ordinal_number(long number);
/*
  String trim function which returns a view into the string, rather
  than a copy.
  //TODO: regex version.
*/
std::string_view string_trim(const std::string &str,
                             const char* delim = " \n\t");
/*
  Filesystem convience functions, maybe move to seperate header.
*/
#if (defined WHOREMASTER)
#ifdef PLATFORM_WINDOWS
typedef fs::file_status fs_status;
//defined in util.cpp as a helper function, may as well export it.
char *stpcpy(char *dest, const char *src);
#else
#include <sys/stat.h>
typedef struct stat fs_status;
#endif
typedef fs::path fs_path;
template<typename ... Components>
fs_path build_fs_path(Components&& ... comps){
  fs_path p;
  //C++17 fold expression
  return (p /= ... /= comps);
}
fs_path build_fs_path(std::initializer_list<const char *> comps);
//Simple functions to test if a file is readable and if it's a directory
//or a regular file. This is generally all we need to know.
bool get_fs_status(const fs_path &p, fs_status *st);//doubles as existence test.

bool fs_is_readable(const fs_status &st);
static inline bool fs_is_readable(const fs_path &p){
  fs_status st;
  return (get_fs_status(p, &st) ? fs_is_readable(st) : false);
}

bool fs_is_directory(const fs_status &st);
static inline bool fs_is_directory(const fs_path &p){
  fs_status st;
  return (get_fs_status(p, &st) ? fs_is_directory(st) : false);
}

bool fs_is_regular_file(const fs_status &st);
static inline bool fs_is_regular_file(const fs_path &p){
  fs_status st;
  return (get_fs_status(p, &st) ? fs_is_directory(st) : false);
}
#endif
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
/*
  In case you need a custom hash function.
*/
static constexpr uint64_t fnv_prime_64 = 1099511628211UL;
static constexpr uint64_t fnv_offset_basis_64 = 14695981039346656037UL;
static constexpr uint32_t fnv_prime_32 = 16777619U;
static constexpr uint32_t fnv_offset_basis_32 = _322166136261U;
//TODO, maybe: make this a template and specialize for 32 and 64 bit types.
[[maybe_unused]] static uint64_t fnv_hash(const void *key, size_t keylen,
                                          uint64_t seed = fnv_offset_basis_64){
  const unsigned char *raw_data = reinterpret_cast<unsigned char*>(key);
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
}
    
template<>
uint64_t fnv_hash(const char* key){
  return fnv_hash(key, strlen(key));
}
/*
  These functions all behave in the same way, they set errno to 0,
  call the underlying function, set *value to its result, and if
  an error occured, or no number was found,  they return true,
  otherwise they return false.
  Not too complicated, but it saves having to mess with errno.
*/
bool strtol_checked(long *value, const char *str,
                    const char** endptr = nullptr, int base = 0);
bool strtoul_checked(long *value, const char *str,
                    const char** endptr = nullptr, int base = 0);
bool strtod_checked(double *value, const char *str,
                    const char** endptr = nullptr, int base = 0);
extern "C" {
size_t memspn(const uint8_t *buf, size_t len,
              const uint8_t *accept, size_t len2);
size_t memcspn(const uint8_t *buf, size_t len,
               const uint8_t *reject, size_t len2);
size_t memspn_table(const uint8_t *buf, size_t len,
                    const uint8_t accept[256]);
size_t memcspn_table(const uint8_t *buf, size_t len,
                     const uint8_t reject[256]);
}
#if 0
namespace util {
/*
  A struct representing a date in a calendar with
  12 months of 30 days each per year.
  -Years are unbounded and can be negitive.
  -day and month are normalized after any operation
  -addition and subtraction operators are overloaded on integers
    to adjust by day.
*/
struct simple_date {
  int year = 0;
  int month = 0;
  int day = 0;
  void normalize() {
    if (day < 1) {
      do {
        day += 30;
        month--;
      } while (day < 1);
    } else if (day > 30) {
      do {
        day -= 30;
        month++;
      } while (day > 30);
    }
    if(month < 1){
      do {
        month += 12;
        year--;
      } while(month < 1);
    } else if (month > 12) {
      do {
        month -= 12;
        year++;
      } while(month > 12);
    }
  }
  void adjust_day(int amt){
    day += amt;
    normalize();
  }
  void adjust_month(int amt){
    month += amt;
    normalize();
  }
  void adjust_year(int amt){
    year += amt;
  }
  simple_date operator+= (int amt) {
    adjust_day(amt);
    return *this;
  }
  simple_date operator-= (int amt) {
    //I'm not sure if the 'this->' is necessary or not.
    return this->operator+=(-amt);
  }
  simple_date operator++() {
    return this->operator+=(1);
  }
  simple_date operator++(int) {
    auto ret = *this;
    this->operator+=(1);
    return ret;
  }
  simple_date operator--(int) {
    auto ret = *this;
    this->operator+=(-1);
    return ret;
  }
}
enum class Number_Parse_Error {
  SUCCESS = 0,
  OVERFLOW,
  UNDERFLOW,
  INVALID
};

};
#endif
#endif
