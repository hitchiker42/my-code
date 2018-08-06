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
//TODO: see if there is any need to have 2 overloads.
//determine the expected number of bytes for a character given
//the leading byte,
inline constexpr int utf8_char_size(uint8_t c){
  return utf8_char_size_table[c];
}
inline constexpr int utf8_char_size(int c){
  return utf8_char_size_table[c];
}
//returns true if c is an ascii character (i.e c < 128)
inline constexpr bool utf8_is_ascii(uint8_t c){
  return (c < 0x80);
}
inline constexpr bool utf8_is_ascii(int c){
  return (c < 0x80);
}
//These assume c is a valid utf8 byte (i.e not 0xff/0xfe).
inline constexpr bool is_utf8_lead_char(uint8_t c){
  return utf8_char_size_table[c] > 0;
}
inline constexpr bool is_utf8_cont_char(uint8_t c){
  return ((c & 0xC0) == 0x80);
}
//decode a character from str, returns -1 on error,
//the number of bytes consumed is utf8_char_size(*str).
int32_t utf8_decode_char(const uint8_t *str);
//decode a character and update the pointer, if there is an error the
//pointer is not modified
inline int32_t utf8_decode_char(const uint8_t **str){
  int32_t ret = utf8_decode_char(*str);
  if(ret >= 0){
    *str = (*str) + utf8_char_size(**str);
  }
  return ret;
}
//returns a pointer to the start of the previous character,
//does not check if that character is valid.
inline uint8_t* utf8_prev_char_start(const uint8_t *str){
  do {
    --str;
  } while(is_utf8_cont_char(*str));
  return str;
}
//Decodes the character that ends on (*str)-1 and updates
//str to point to the begining of that character.
inline int32_t utf8_decode_char_backwards(const uint8_t **str){
  const uint8_t *start = utf8_prev_char_start(*str);
  int32_t ret = utf8_decode_char(start);
  if(ret >= 0){
    *str = start;
  }
  return ret;
}
//I think this should work...
inline int32_t utf8_next_char(const uint8_t *& str){
  return utf8_decode_char(&str);
}
inline int32_t utf8_prev_char(const uint8_t *& str){
  return utf8_decode_char_backwards(&str);
}
template<typename T,
         std::enable_if<std::is_integral_t<T>, int> = 0>
inline int32_t utf8_decode_char(const uint8_t *str, T* used){
  int32_t ret = utf8_decode_char(str);
  *used = static_cast<T>(utf8_char_size(*str));
  return ret;
}
//The bytes are returned as a literal array which is null terminated,
//the maximum length of a utf8 character is 6 bytes + 1 for the null terminator
//and we round up to 8 since it's going to take that much space anyway.
std::array<char,8> utf8_encode_char(uint32_t c);
//Same as above but the return value is packed into a 64 bit integer.
uint64_t utf8_encode_char_packed(uint32_t c);
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
//decode a utf8 string into a utf32 string
std::u32string utf8_decode_string(std::string_view utf8_str);
//encode a utf32 string as utf8
std::string utf8_encode_string(std::u32string_view utf32_str);
//Same as above but taking and returning more basic types.
util::svector<uint32_t> utf8_decode_string(const char *str, size_t size);
util::svector<char> utf8_encode_string(uint32_t *str, size_t length);

#if 0
//utf16 is awful, but we're kinda stuck with it (mostly due to windows),
//so I have minimal utf16 conversion functions.
inline constexpr utf16_char_size(uint16_t c){
  return ((c <= 0xd7ff || c >= 0xe000) ? 1 : 2);
}
int32_t utf16_decode_char(const uint16_t *str, bool big_endian = false);
std::array<uint16,2> utf16_encode_char(const uint32_t c, bool big_endian = false);
#endif
//An iterator over the characters stored in a utf8 string stored as a pointer.
//Acts as a bidirectional c++ iterator.
//With a bit of work could be adapted to work with a bidirectional iterator instead
//of just a pointer.
//An invalid utf8 sequence causes the iterator to return -1, when advanced
//it will automatically attempt to resynch to the next lead character.
struct utf8_string_iter {
  using pointer =  const char32_t*;
  using reference = const char32_t&;
  using value_type = char32_t;
  using size_type = size_t;
  using difference_type = ptrdiff_t;
  using iterator_category = std::bidirectional_iterator_tag;

  const uint8_t *str;
  mutable char32_t c = 0;

  utf8_string_iter() : str{nullptr} {}
  utf8_string_iter(const uint8_t* str) : str{str} {}
  utf8_string_iter(const char* cstr) : str{static_cast<const uint8_t*>(cstr)} {}
  utf8_string_iter(std::string_view sv) // also works for std::string
    : str{static_cast<const uint8_t*>(sv.data())} {}
  utf8_string_iter(const utf8_string_iter& other) = default;

  char32_t get_char() const {
    if(c == 0){
      c = utf8_decode_char(str);
    }
    return c;
  }
  void advance(){
    if(get_char() < 0){//resync underlying stream.
      do {
        ++str;
      } while(!is_utf8_lead_char(*str));
    } else {
      str += utf8_char_size(*str);
    }
    //Do not precompute next character, we may be past the end.
  }
  void reverse(){
    //Its fine to compute the value here since decrementing the begin iterator
    //is already undefined behavior.
    c = utf8_decode_char_backwards(&str);
  }
    
  char32_t operator*() const {
    return get_char();
  }    
  utf8_string_iter& operator++(){//pre-increment
    advance();
    return *this;
  }
  utf8_string_iter operator++(int){//post-increment
    get_char();//insure char is computed before copying to avoid doing it 2x
    auto ret = *this;
    advance();
    return ret;
  }
  utf8_string_iter& operator--(){//pre-decrement
    reverse();
    return *this;
  }
  utf8_string_iter operator--(int){//post-decrement
    //Don't call get_char, this could be an end iterator.
    auto ret = *this;
    reverse();
    return ret;
  }
  //Equality needs to be defined so that it ignores the cached character. 
  bool operator==(const utf8_string_iter& rhs) const {
    return str == rhs.str;
  }
  bool operator!=(const utf8_string_iter& rhs) const {
    return str != rhs.str;
  }
};
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
*/
#if 0
/*
  An iterator adaptor that decodes utf8 from an iterator over the
  underlying byte sequence. It can be used like a bidirectional iterator
  but it technically only satisifies forward iterator requirements due to
  having to modify the iterator when it is first dereferenced.
*/
template<typename BidiIt>
struct utf8_iterator {
  using value_type = char32_t;
  using pointer_type = const char32_t*;
  using reference_type = const char32_t&;
  using difference_type = ptrdiff_t;
  using iterator_category = std::input_iterator_tag;

  BidiIt it;
  char32_t val = 0;
#endif
} //namespace util
#endif /* __UTIL_UNICODE_H__ */
