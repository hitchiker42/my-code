#ifndef __UNICODE_H__
#define __UNICODE_H__
#ifdef __cplusplus
extern "C" {
#endif

#include <stdint.h>
#include <stdlib.h>
#include <errno.h>

#define is_ascii(c) (!(c & 0x80))

typedef union utf8_char_str utf8_char_str;
typedef union utf16_char_str utf16_char_str;
//utf8 never needs more than 8 bytes to encode any character, this
//struct allows returning an 8 byte character array from a function
//which means utf8_encode_char doesn't need to allocate memory
union utf8_char_str {
  uint64_t val;
  char str[8];
};
union utf16_char_str {
  uint64_t val;
  struct {
    char str[5];
    uint8_t byte_order;
  };
};
/*
  Return the number of bytes need by a utf8 character with c as a leading byte
*/
int utf8_char_size(unsigned char c);
/*
  Read a utf8 encoded character from str and store the number of bytes
  read in used (if used is not NULL).
*/
uint32_t utf8_decode_char(const char *str, int *used);
/*
  Encode the unicode code point 'c' and return a null terminated string
  (stored as a 64 bit integer) containing the utf8 byte sequence for the character.
*/
utf8_char_str utf8_encode_char(uint32_t c);// , size_t *size);

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

uint32_t utf16_decode_char();
#if (defined __LIBCUTIL__)
//used to iterate across a utf8 string by character vaguely efficiently
typedef struct utf8_str_iter utf8_str_iter;
struct utf8_str_iter {
  string str;
  uint32_t chars_read;
  uint32_t pos;
};

utf8_str_iter* make_string_iter(string *str);
int string_iter_next(utf8_str_iter *iter);
int string_iter_prev(utf8_str_iter *iter);
int string_iter_next_n(utf8_str_iter *iter, unsigned int n);
int string_iter_prev_n(utf8_str_iter *iter, unsigned int n);

//decode a utf8 string into a utf32 string
string utf8_decode_string(string utf8_str);
//encode a utf32 string as utf8
string utf8_encode_string(string utf32_str);
#else 
//decode a utf8 string str of size bytes into a utf32 string, storing
//the length of the string in length, if it is not null
uint32_t *utf8_decode_string(const char *str, size_t size, size_t *length);
//encode a utf32 string str of length 'length' into a utf8 string, storing
//the size in bytes into size, if it is not null
uint8_t *utf8_encode_string(uint32_t *str, size_t length, size_t *size);
#endif //__LIBCUTIL__
#ifdef __cplusplus
}
#endif
#endif /* __UNICODE__H__ */
