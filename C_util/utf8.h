/*
  This header and it's corrsponding file have a lot of conditionally compiled
  sections, this is so they can be used outside of scilisp as a generic
  implementation of utf8 stuff
*/
#ifndef _SL_UTF8_H_
#define _SL_UTF8_H_
#if(defined _SCILISP_)
#include "common.h"
#else
#include <stdint.h>
#include <stdlib.h>
#include <errno.h>
#endif
#define is_ascii(c) (!(c & 0x80))

typedef union utf8_char_str utf8_char_str;
//utf8 never needs more than 8 bytes to encode any character, this
//struct allows returning an 8 byte character array from a function
//which means utf8_encode_char doesn't need to allocate memory
union utf8_char_str {
  uint64_t val;
  char str[8];
};
int utf8_char_size(unsigned char c);
uint32_t utf8_decode_char(const char *str, size_t *used);
utf8_char_str utf8_encode_char(uint32_t c);// , size_t *size);
/*
//decode a utf8 string str of size bytes into a utf32 string, storing
//the length of the string in length, if it is not null
uint32_t *utf8_decode_string(const char *str, size_t size, size_t *length);
//encode a utf32 string str of length 'length' into a utf8 string, storing
//the size in bytes into size, if it is not null
uint8_t *utf8_encode_string(uint32_t *str, size_t length, size_t *size);
*/
/*
  return the nth character of str, this is only really useful
  if only the nth character is needed. This is O(n), but does
  optimize a bit for ascii strings, still if you need more than
  just 1 character use an iterator;

  implicitly assumes that n < length of str
 */
uint32_t utf8_nth_char(const char *str, uint32_t n);
//return a pointer to the nth character of str, mainly
//of use in implementing a substring procedure
const char *utf8_nth_ptr(const char *str, uint32_t n);
/* searching/length/utility functions */
/* Return a pointer to the first occurance of chr in str */
const char* utf8_strchr(const char *str, uint32_t len, uint32_t chr);
/* Return a pointer to the last occurance of chr in str*/
const char* utf8_rstrchr(const char *str, uint32_t len, uint32_t chr);
/* return the index of the character at the byte location pos */
uint32_t utf8_byte_pos_to_char_pos(const char *str, uint32_t len, uint32_t pos);
/* return the byte index of the character at the location pos*/
uint32_t utf8_char_pos_to_byte_pos(const char *str, uint32_t len, uint32_t pos);
#if (defined _SCILISP_)
//used to iterate across a utf8 string by character vaguely efficiently
typedef struct utf8_str_iter utf8_str_iter;
struct utf8_str_iter {
  sl_string str;
  uint32_t chars_read;
  uint32_t pos;
};

utf8_str_iter* make_string_iter(sl_string *str);
sl_char string_iter_next(utf8_str_iter *iter);
sl_char string_iter_prev(utf8_str_iter *iter);
sl_char string_iter_next_n(utf8_str_iter *iter, unsigned int n);
sl_char string_iter_prev_n(utf8_str_iter *iter, unsigned int n);
#endif
#endif
