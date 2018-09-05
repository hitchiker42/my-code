#ifndef __CONSTEXPR_FNS_H__
#define __CONSTEXPR_FNS_H__
namespace util {
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
//constexpr versions of <string.h> functions
#ifdef __GNUC__
static constexpr size_t constexpr_strlen(const char *s){
  return __builtin_strlen(s);
}
static constexpr int constexpr_strcmp(const char *s1, const char *s2){
  return __builtin_strcmp(s1,s2);
}
static constexpr int constexpr_strncmp(const char *s1, const char *s2, size_t n){
  return __builtin_strncmp(s1,s2,n);
}
static constexpr int constexpr_memcmp(const void *s1, const void *s2, size_t n){
  return __builtin_memcmp(s1,s2,n);
}
#else
static constexpr size_t constexpr_strlen(const char *s, size_t len = 0){
  return *s ? constexpr_strlen(s+1,len+1) : len;
}
static constexpr int constexpr_strcmp(const char *s1, const char *s2){
  return (*s1 == *s2 ? (*s ? constexpr_strcmp(s1+1,s2+1) : 0) : (*s1 - *s2));
}
static constexpr int constexpr_strncmp(const char *s1, const char *s2, size_t n){
  return (n ? (*s == *s2 ?
               (*s ? constexpr_strncmp(s+1,s2+1,n-1) : 0) : (*s - *s2)) : 0);
}
static constexpr int constexpr_memcmp(const unsigned char *s1,
                                      const unsigned char *s2, size_t n){
  return (n ? (*s == *s2 ? constexpr_strncmp(s+1,s2+1,n-1) : (*s - *s2)) : 0);
}
static constexpr int constexpr_memcmp(const void *s1, const void *s2, size_t n){
  return constexpr_memcmp(static_cast<unsigned char *>(s1),
                          static_cast<unsigned char*>(s2), n);
}
#endif
}
#endif /* __CONSTEXPR_FNS_H__ */
