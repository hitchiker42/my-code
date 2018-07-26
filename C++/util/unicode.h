#ifndef __UTIL_UNICODE_H__
#define __UTIL_UNICODE_H__
#include <stdint.h>
namespace util {
//Table of the number of bytes used for a valid utf8 character begining with
//the given byte. 0 is for continuation bytes and -1 is for invalid bytes.
inline constexpr std::array<int8_t, 256> utf8_char_size_table = {{
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
    1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
    2, 2, 2, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3,
    4, 4, 4, 4, 4, 4, 4, 4, 5, 5, 5, 5, 6, 6, -1, -1
  }};

//determine the expected number of bytes for a character given
//the leading byte,
inline constexpr int utf8_char_size(uint8_t c){
  return utf8_char_size_table[c];
}
inline constexpr bool utf8_is_ascii(uint8_t c){
  return (c < 0x80);
}
//These assume c is a valid utf8 byte (i.e not 0xff/0xfe).
inline constexpr bool is_utf8_lead_char(uint8_t c){
  return utf8_char_size_table[c] > 0;
}
inline constexpr bool is_utf8_cont_char(uint8_t c){
  return ((c & 0xC0) == 0x80);
}
inline constexpr int utf8_char_size(char c){
  return utf8_char_size(static_cast<uint8_t>(c));
}
//decode a character from str, returns -1 on error,
//the number of bytes consumed is utf8_char_size(*str).
int32_t utf8_decode_char(const uint8_t *str);

inline uint32_t utf8_decode_char(const uint8_t *str, size_t *used = nullptr){
  auto [c, len] = utf8_decode_char_impl(str);
  if(used){ *used = len; }
  return c;
}
//convert a character from a codepoint to a utf8 encoded sequence of bytes.
//The bytes are returned as a literal array which is null terminated,
//the maximum length of a utf8 character is 6 bytes + 1 for the null terminator
//and we round up to 8 since it's going to take that much space anyway.
std::array<char,8> utf8_encode_char(uint32_t c);
//convert a character from a codepoint to a utf8 encoded sequence of bytes.
//The bytes are stored in buf and the number of bytes used is returned.
size_t utf8_encode_char(uint32_t c, uint8_t *buf);

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
//An iterator over the characters stored in a utf8 string. Can act as a C++ iterator.
struct utf8_string_iter {
  using pointer =  const uint32_t*;
  using reference = const uint32_t&;
  using value_type = uint32_t;
  using size_type = size_t;
  using difference_type = ptrdiff_t;
  using iterator_category = std::bidirectional_iterator_tag;
  
  const uint8_t *str;
  //cache the next character and the number of bytes it uses.
  mutable int32_t c = -1;
  mutable int32_t nbytes = -1;
  void get_next() const {
    if(
    c = utf8_decode_char(str, &nbytes);
  }
  

  utf8_string_iter& operator++(){
  
  
};
uint32_t utf16_decode_char(const uint16_t *str, int byteorder, size_t *used);
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
std::u32string utf8_decode_string(std::string_view utf8_str);
//encode a utf32 string as utf8
std::string utf8_encode_string(std::u32string_view utf32_str);
//Same as above but taking and returning more basic types.
util::svector<uint32_t> utf8_decode_string(const char *str, size_t size);
util::svector<char> utf8_encode_string(uint32_t *str, size_t length);
} //namespace util
#endif /* __UTIL_UNICODE_H__ */
