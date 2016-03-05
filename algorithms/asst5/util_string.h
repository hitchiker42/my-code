#ifndef __UTIL_STRING_H__
#define __UTIL_STRING_H__
#ifdef __cplusplus
extern "C" {
#endif
#include "C_util.h"
/*
  A type and helper functions for explicit length strings.
  Everything is in this header, there is no coorsponding .c file
*/
typedef struct string string;
#if 0
enum string_encoding {
  NO_ENCODING = 0,//Or else unknown encoding
  ASCII_ENCODING,
  UTF8_ENCODING,
  UTF16_ENCODING,
  UTF32_ENCODING
};
/*
  Most of the stuff here is not used, but would be useful to
  add in at some point later.
*/
struct string {
  union {
    uint8_t *mem;
    uint8_t *ascii;
    uint8_t *utf8;
    uint16_t *utf16;
    uint32_t *utf32;
  };
  uint32_t sz;//do you really need strings longer than 4GB
  uint8_t encoding;
  uint8_t copy_on_write;
};
#endif
struct string {
  union {
    uint8_t *mem;
    char *str;
  };
  union {
    int64_t sz;
    int64_t len;
  };
};
/**/
static string make_string(uint8_t *str, int len){
  string ret = {.mem = str, .sz = len};
  return ret;
}
static string string_from_cstr(char *str){
  string ret = {.mem = (uint8_t*)str, .sz = strlen(str)};
  return ret;
}
static char *string_to_cstr(string str){
  char *ret = xmalloc(str.len + 1);
  memcpy(ret, str.str, str.len);
  ret[str.len] = '\0';
  return ret;
}
static int string_cmp(string a, string b){
  if(a.sz != b.sz){
    return ((long)a.sz - (long)b.sz);
  } else {
    return memcmp(a.mem, b.mem, a.sz);
  }
}
static int string_ptr_cmp(string *a, string *b){
  if(a->sz != b->sz){
    return ((long)a->sz - (long)b->sz);
  } else {
    return memcmp(a->mem, b->mem, a->sz);
  }
}
static int string_eq(string a, string b){
  return (a.sz == b.sz && !memcmp(a.mem, b.mem, a.sz));
}
static string string_cat(string a, string b){
  string ret;
  ret.sz = a.sz + b.sz;
  ret.mem = xmalloc(ret.sz * sizeof(uint8_t));
  memcpy(ret.mem, a.mem, a.sz);
  memcpy(ret.mem + a.sz, b.mem, b.sz);
  return ret;
}
static string sub_string(string a, int start, int end){
  return make_string(a.mem + start, MIN(a.sz-start, end-start));
}
static string copy_string(string a){
  string ret = {.sz = a.sz};
  ret.mem = xmalloc(a.sz);
  memcpy(ret.mem, a.mem, a.sz);
  return ret;
}
/*
  Create a null terminated version of str on the stack
*/
#define string_dupa(a)                          \
  __extension__({                               \
      string ret = {.sz = a.sz};                \
      ret.mem = alloca(a.sz);                   \
      memcpy(ret.mem, a.mem, a.sz);             \
      ret;})
#define string_to_cstra(s)                    \
  __extension__({                               \
      char *ret = alloca(s.len+1);            \
      memcpy(ret, s.str, s.len);            \
      ret[s.len] = '\0';                  \
      ret;})
#ifdef __cplusplus
}
#endif
#endif /* __UTIL_STRING_H__ */
