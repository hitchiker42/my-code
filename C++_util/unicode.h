#ifndef __UTIL_UNICODE_H__
#define __UTIL_UNICODE_H__
#include <stdint.h>
#include <errno.h>
#include <array>
#include <string>
#include <type_traits>
#include "templates.h"
namespace util {
//I initially implemented utf8 with a 6 bytes maximum, it turns out
//that you only need a 4 byte maximum since unicode is limited to 21 bytes.
static inline constexpr size_t utf8_max_char_size = 4;
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
    4, 4, 4, 4, 4, 4, 4, 4,
  //5, 5, 5, 5, 6, 6, //These are possible but not used
   -1,-1,-1,-1,-1,-1,-1,-1
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
//return the number of bytes needed to encode the codepoint cp using utf8
inline constexpr int utf8_codepoint_size(int cp){
    return (cp < 0x80 ? 1 : (cp < 0x800 ? 2 : (cp < 0x10000 ? 3 : 4)));
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
/*
  A lot of these functions could probably be made constexpr,
  but doing so would likely require some changes and I 
  don't really see an advantage to making them constexpr.
*/
//decode a character from str, returns -1 on error,
//the number of bytes consumed is utf8_char_size(*str).
inline int32_t utf8_decode_char(const uint8_t *str);
inline int32_t utf8_decode_char(const char*str){
  return utf8_decode_char((const uint8_t*)str);
}
//Optimized version with no error checking. Only use this if you know that str
//is valid utf8.
inline int32_t utf8_decode_char_unchecked(const uint8_t *src){
  static constexpr std::array<uint8_t, utf8_max_char_size> utf8_lead_char_mask =
    {{0x7f, 0x1F, 0x0F, 0x07}};
  int remain = utf8_char_size(*src) - 1;
  int32_t ret = (*src++) & utf8_lead_char_mask[remain];
  //Non unrolled loop to minimize size for inlining
  while(remain--){
    ret <<= 6;
    ret |= (*src++) & 0x3F;
  }
  return static_cast<int32_t>(ret);
}
inline int32_t utf8_decode_char_unchecked(const char *src){
  return utf8_decode_char_unchecked((uint8_t*)src);
}
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
inline const uint8_t* utf8_prev_char_start(const uint8_t *str){
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
inline int32_t utf8_next_char(const uint8_t *str, size_t *index){
  int32_t ret =  utf8_decode_char(str + *index);
  *index += utf8_char_size(*str);
  return ret;
}
inline int32_t utf8_prev_char(const uint8_t *str, size_t *index){
  const uint8_t *start = utf8_prev_char_start(str);
  int32_t ret =  utf8_decode_char(start);
  *index -= utf8_char_size(*start);
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
         std::enable_if_t<std::is_integral_v<T>, int> = 0>
inline int32_t utf8_decode_char(const uint8_t *str, T* used){
  int32_t ret = utf8_decode_char(str);
  *used = static_cast<T>(utf8_char_size(*str));
  return ret;
}
inline bool is_valid_codepoint(int32_t ch){
  return (ch >= 0 && ch <= 0x10FFFF && //in unicode range
          (ch < 0xD800 || ch > 0xDFFF) && //not a utf16 surrogate
          (ch != 0xFFFE) && (ch != 0xFFFF));//defined as illegal
}
inline size_t utf8_encode_char(int32_t c, uint8_t *buf);
inline size_t utf8_encode_char(int32_t c, char *buf){
  return utf8_encode_char(c, (uint8_t*)buf);
}
//The bytes are returned as a literal array which is null terminated,
//the maximum length of a utf8 character is 6 bytes + 1 for the null terminator
//and we round up to 8 since it's going to take that much space anyway.
inline std::array<char,utf8_max_char_size> utf8_encode_char(int32_t c){
  std::array<char,utf8_max_char_size> ret = {{0}};
  utf8_encode_char(c, ret.data());
  return ret;
}
//Same as above but the return value is packed into a 64 bit integer.
inline int32_t utf8_encode_char_packed(int32_t c){
 std::array<char,utf8_max_char_size> arr = utf8_encode_char(c);
 return *((int32_t*)arr.data());
}
//convert a character from a codepoint to a utf8 encoded sequence of bytes.
//The bytes are stored in buf and the number of bytes used is returned.
inline size_t utf8_encode_char_unchecked(int32_t c, uint8_t *buf){
  if(c < 0x80) {
    *buf++ = c;
    return 1;
  } else if(c < 0x800) {
    *buf++ = 0xC0 | ((c >> 6) & 0x1F);
    *buf++ = 0x80 | (c & 0x3F);
    return 2;
  } else if(c < 0x10000) {
    *buf++ = 0xE0 | ((c >> 12) & 0xF);
    *buf++ = 0x80 | ((c >> 6) & 0x3F);
    *buf++ = 0x80 | (c & 0x3F);
    return 3;
  } else if(c < 0x200000) {
    *buf++ = 0xF0 | ((c >> 18) & 0x7);
    *buf++ = 0x80 | ((c >> 12) & 0x3F);
    *buf++ = 0x80 | ((c >> 6) & 0x3F);
    *buf++ = 0x80 | (c & 0x3F);
    return 4;
  }
}
inline size_t utf8_encode_char_unchecked(int32_t c, char *buf){
  return utf8_encode_char_unchecked(c, (uint8_t*)buf);
}
inline std::u32string utf8_decode_string(std::string_view utf8_str){
  std::u32string ret;
  errno = 0;
  const uint8_t *str = (const uint8_t*)utf8_str.data();
  const uint8_t *end = (const uint8_t*)(utf8_str.data() + utf8_str.size());
  while(str < end){
    char32_t c = utf8_decode_char(&str);
    if(errno != 0){
      return U"";
    }
    ret.push_back(c);
  }
  return ret;
}
//encode a utf32 string as utf8
inline std::string utf8_encode_string(std::u32string_view utf32_str){
  //A couples notes:
  //We can't call member functions recklessly since everytime we do
  //we need to reset errno.
  //We essentially manage memory ourselves for simplicity.
  //inilial capacity is the small string size for stdlibc++
  size_t sz = 0, capacity = 15, str_sz = utf32_str.size();
  std::string ret(capacity, 0);
  errno = 0;
  for(size_t i = 0; i < str_sz; i++){
    if((sz + utf8_max_char_size) > capacity){
      capacity *= 2;
      //Add 1 to keep capacity at (2^n)-1 (2^n with null terminator)
      ret.resize(++capacity);
      errno = 0;
    }
    sz += utf8_encode_char(utf32_str[i], ret.data() + sz);
    if(errno != 0){
      return u8"";
    }
  }
  ret.resize(sz);
  return ret;
}
/*
  Simple UTF-8 spec
  bits of    | First      | Last         | Num Bytes | Leading    |
  code point | code point | code point   |           | byte       |
  ----------------------------------------------------------------|
  |    7     |  0x0       |  0x7F        |    1      | 0b0xxxxxxx |
  |    11    |  0x80      |  0x7FF       |    2      | 0b110xxxxx |
  |    16    |  0x800     |  0xFFFF      |    3      | 0b1110xxxx |
  |    21    |  0x10000   |  0x1FFFFF    |    4      | 0b11110xxx |

  All bytes following the leading byte in a multibyte sequence have the form
  0b10xxxxxx
  in any instance the bytes 0b11111110 and 0b11111111 are illegal.
  The xs above are the bits of the actual codepoint.
  A valid utf8 character must use the fewest bytes needed.
 */
inline int32_t utf8_decode_char(const uint8_t* src){
  //utf8_min_codepoint[i] is minimum value of a valid codepoint for a
  //byte sequence of length i+1.
  static constexpr std::array<int, utf8_max_char_size> utf8_min_codepoint =
    {{0, 0x80, 0x800, 0x10000}};
  static constexpr std::array<uint8_t, utf8_max_char_size> utf8_lead_char_mask =
    {{0x7f, 0x1F, 0x0F, 0x07}};
  if(!src){
    errno = EINVAL;
    return -1;
  }
  int len = utf8_char_size(*src);
  if(len <= 0){
    errno = EILSEQ;
    return -1;
  }
  //early return for ascii.
  if(len == 1){
    return *src;
  }
  int remain = len - 1;
  int32_t min = utf8_min_codepoint[remain];
  int32_t ret = (*src++) & utf8_lead_char_mask[remain];
  uint8_t ch = 0;
  //We could unroll this loop, but that seems unecessary.
  while(remain--){
    ch = *src++;
    if((ch & 0xC0) != 0x80) {
      errno = EILSEQ;
      return -1;
    }
    ret <<= 6;
    ret |= ch & 0x3F;
  }
  if(ret < min) {
    errno = EILSEQ;
    return -1;
  }
  return static_cast<int32_t>(ret);
}
inline size_t utf8_encode_char(int32_t c, uint8_t *buf){
  if(!is_valid_codepoint(c)){
    errno = EILSEQ;
    return 0;
  }
  return utf8_encode_char_unchecked(c, buf);
}
//Count the number of characters in a utf8 string, doesn't check
//that the string is valid utf8, just counts utf8 lead characters.
inline constexpr size_t utf8_strlen(const uint8_t *str, size_t nbytes){
  size_t len = 0;
  for(size_t idx = 0; idx < nbytes; idx++){
    len += is_utf8_lead_char(str[idx]);
  }
  return len;
}
inline constexpr size_t utf8_strlen(const uint8_t *str){
  size_t len = 0;
  while(*str != '\0'){
    len += is_utf8_lead_char(*str++);
  }
  return len;
}
inline constexpr size_t utf8_strlen(std::string_view sv){
  return utf8_strlen((const uint8_t*)sv.data(), sv.size());
}
inline constexpr size_t utf8_strlen(const char* str, size_t nbytes){
  return utf8_strlen((const uint8_t*)str, nbytes);
}
inline constexpr size_t utf8_strlen(const char* str){
  return utf8_strlen((const uint8_t*)str);
}
//Validate a utf8 string and return its length in characters, return
//(size_t)-1 if it is invalid utf8.
inline constexpr size_t utf8_strlen_validate(const uint8_t *str, size_t nbytes){
  size_t len = 0;
  size_t idx = 0;
  while(idx < nbytes){
    int cnt = utf8_char_size(str[idx++]);
    if(cnt <= 0){ return -1; }
    while(--cnt){
      if(idx >= nbytes ||
         !is_utf8_cont_char(str[idx++])){
        return -1;
      }
    }
    len++;
  }
  return len;
}
inline constexpr size_t utf8_strlen_validate(const uint8_t *str){
  size_t len = 0;
  while(*str != '\0'){
    int cnt = utf8_char_size(*str++);
    while(--cnt){
      //Will catch null since it's not a cont_char.
      if(!is_utf8_cont_char(*str++)){
        return -1;
      }
    }
    len++;
  }
  return len;
}
//utf16 is awful, but we're kinda stuck with it (mostly due to windows),
//so I have minimal utf16 conversion functions.
/*
  utf16 surrogate info.
    High \ Low  DC00    DC01    …       DFFF
    D800        010000  010001  …       0103FF
    D801        010400  010401  …       0107FF
      ⋮         ⋮       ⋮       ⋱       ⋮
    DBFF        10FC00  10FC01  …       10FFFF

    0x010000 is subtracted from the code point, leaving a 20-bit number in the range 0..0x0FFFFF.
    The top ten bits (a number in the range 0..0x03FF) are added to 0xD800 to give the first 16-bit code unit or high surrogate, which will be in the range 0xD800..0xDBFF.
    The low ten bits (also in the range 0..0x03FF) are added to 0xDC00 to give the second 16-bit code unit or low surrogate, which will be in the range 0xDC00..0xDFFF.

    D800 to DFFF are reserved for the high and low surregates
*/
inline constexpr int utf16_char_size(uint16_t c){
  return ((c <= 0xd7ff || c >= 0xe000) ? 1 : 2);
}
//decode a character from a native endian utf16 byte stream.
inline constexpr int32_t utf16_decode_char(const uint16_t *str){
  if(!str){
    errno = EINVAL;
    return -1;
  }
  uint16_t c = *str++;
  if(c <= 0xD7FF || c >= 0xE000){
    return c;
  }
  if(c >= 0xDC00){
    errno = EILSEQ;
    return -1;
  }
  uint16_t c2 = *str++;
  if(c2 < 0xDC00 || c2 > 0xDFF){
    errno = EILSEQ;
    return -1;
  }
  //Each of these is a 10 bit integer
  int32_t high = c - 0xD800;
  int32_t low = c2 - 0xDC00;
  //now we need to put them together into a 20 bit integer
  int32_t ret = (high << 10) | low;
  return ret + 0x10000;
}
//encode a character to a native endian utf16 byte stream.
inline size_t utf16_encode_char(int32_t c, uint16_t *buf){
  if(!is_valid_codepoint(c)){
    errno = EILSEQ;
    return 0;
  }
  if(c < 0x10000){
    *buf = c;
    return 1;
  } else {
    c -= 0x10000;
    *buf = 0xD800 + (c >> 10);
    *(buf+1) = 0xDC00 + (c & 0x03FF);
    return 2;
  }
}
//An iterator over the characters stored in a utf8 string stored as a pointer.
//Acts as a bidirectional c++ iterator.
//With a bit of work could be adapted to work with a bidirectional iterator instead
//of just a pointer.
//An invalid utf8 sequence causes the iterator to return -1, when advanced
//it will automatically attempt to resynch to the next lead character.
struct utf8_string_iter {
  using pointer =  const int32_t*;
  using reference = const int32_t&;
  using value_type = int32_t;
  using size_type = size_t;
  using difference_type = ptrdiff_t;
  using iterator_category = std::bidirectional_iterator_tag;

  const uint8_t *str;
  mutable int32_t c = 0;

  utf8_string_iter() : str{nullptr} {}
  utf8_string_iter(const uint8_t* str) : str{str} {}
  utf8_string_iter(const char* cstr) :
    str{reinterpret_cast<const uint8_t*>(cstr)} {}
  utf8_string_iter(std::string_view sv) // also works for std::string
    : str{reinterpret_cast<const uint8_t*>(sv.data())} {}
  utf8_string_iter(const utf8_string_iter& other) = default;

  template<typename T>
  static std::pair<utf8_string_iter, utf8_string_iter> utf8_iter_range_pair(const T& str){
    return {utf8_string_iter(std::begin(str)), utf8_string_iter(std::end(str))};
  }
  template<typename T>
  static util::range<utf8_string_iter> utf8_iter_range(const T& str){
    return util::range<utf8_string_iter>(utf8_iter_range_pair(str));
  }

  int32_t get_char() const {
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
    c = 0;
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
} //namespace util
#endif /* __UTIL_UNICODE_H__ */
