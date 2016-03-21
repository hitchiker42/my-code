#ifndef __UTIL_STRING_H__
#define __UTIL_STRING_H__
#ifdef __cplusplus
extern "C" {
#endif
#include "C_util.h"
/*
  A type and helper functions for explicit length strings.
  It is generally enough to include this header, the corrsponding
  .c file only contains more complex algorithms.
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

#define STRING_FMT(s) (int)s.len, s.str
//make a string from a c string literal
#define make_string_lit(s)                      \
  ({string ret = {.str = s, .sz = sizeof(s)-1}; \
    ret;})
static string make_string(uint8_t *str, int len){
  string ret = {.mem = str, .sz = len};
  return ret;
}
static string* make_string_ptr(uint8_t *str, int len){
  string *ret = xmalloc(sizeof(string));
  ret->mem = str;
  ret->sz = len;
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
static int string_ptr_eq(string *a, string *b){
  return (a->sz == b->sz && !memcmp(a->mem, b->mem, a->sz));
}
static string string_cat(string a, string b){
  string ret;
  ret.sz = a.sz + b.sz;
  ret.mem = xmalloc(ret.sz * sizeof(uint8_t));
  memcpy(ret.mem, a.mem, a.sz);
  memcpy(ret.mem + a.sz, b.mem, b.sz);
  return ret;
}
//string cat which modifies/frees it's arguments
//probably more useful honestly
static string string_ncat(string a, string b){
  string ret;
  ret.sz = a.sz + b.sz;
  ret.mem = realloc(a.mem, ret.sz);
  memcpy(ret.mem + a.sz, b.mem, b.sz);
  free(b.mem);
  return ret;
}
//concatenate an arbitary # of strings
string string_catn(int n, ...);
#define STRING_CATN(...) VA_FUNC(string_catn, __VA_ARGS__)
string string_ncatn(int n, ...);
#define STRING_NCATN(...) VA_FUNC(string_ncatn, __VA_ARGS__)

static string sub_string(string a, int start, int end){
  start = (start ? start : 0);
  end = (end ? end : a.len);
  return make_string(a.mem + start, MIN(a.sz-start, end-start));
}
static string copy_string(string a){
  string ret = {.sz = a.sz};
  ret.mem = xmalloc(a.sz);
  memcpy(ret.mem, a.mem, a.sz);
  return ret;
}
//Return the index of the first occurace of c in a, or -1 if not found
static int string_index(string a, uint8_t c){
  int i = 0;
  while(i<a.len){
    if(a.mem[i] == c){
      return i;
    }
    i++;
  }
  return -1;
}
//search for b in a, naively (i.e avg O(n+m), worst O(nm))
static int string_search(string a, string b){
  if(b.len > a.len){
    return -1;
  }
  int i = 0;
  while(i+b.len < a.len){
    int j = 0;
    while(a.str[i++] == b.str[j++]);
    if(j == b.len){
      return i - j;
    }
  }
  return -1;
}
uint8_t *boyer_moore(uint8_t *str, uint32_t len,
                     uint8_t *pat, uint32_t patlen);
uint8_t **boyer_moore_all(uint8_t *str, uint32_t len,
                          uint8_t *pat, uint32_t patlen, int *n_matches);
static int boyer_moore_string(string str, string pat){
  uint8_t *match = boyer_moore(str.mem, str.len, pat.mem, pat.len);
  if(!match){
    return -1;
  } else {
    return (int)(match - str.mem);
  }
}
/*
  Seperate 'input' into tokens using the set of bytes in 'delim' to delimit
  the tokens.

  The tokens and size arguments work similarly to the 'getline' function.
  If tokens is NULL and *size is 0, a buffer is allocated and stored in
  *tokens and *size is set to its size. If tokens is not NULL *size
  is taken to be its size and if necessary it will be reallocated
  (and size updated to reflect the new size).

  Returns the number of tokens parsed (aka the number of elements stored in tokens).

  The values stored in tokens point into the input string, so it should not be
  modified.
*/
int tokenize(string input, string delim,
             string **tokens, int *size);
//convience function to tokenize a whitespace delimited string
static inline int tokenize_ws(string input, string **tokens, int *size){
  string ws_str = make_string_lit(" \n\t");
  return tokenize(input, ws_str, tokens, size);
}
/*
  Create an optionally null terminated copy of str on the stack
*/
#define string_dupa(a)                          \
  __extension__({                               \
      string ret = {.sz = a.sz};                \
      ret.mem = alloca(a.sz);                   \
      memcpy(ret.mem, a.mem, a.sz);             \
      ret;})
#define string_to_cstra(s)                      \
  __extension__({                               \
      char *ret = alloca(s.len+1);              \
      memcpy(ret, s.str, s.len);                \
      ret[s.len] = '\0';                        \
      ret;})
#ifdef __cplusplus
}
#endif
#endif /* __UTIL_STRING_H__ */
