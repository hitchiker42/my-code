#include "unicode.h"
/*
  Simple UTF-8 spec
  bits of    | First      | Last         | Num Bytes | Leading    |
  code point | code point | code point   |           | byte       |
  ----------------------------------------------------------------|
  |    7     |  0x0       |  0x7F        |    1      | 0b0xxxxxxx |
  |    11    |  0x80      |  0x7FF       |    2      | 0b110xxxxx |
  |    16    |  0x800     |  0xFFFF      |    3      | 0b1110xxxx |
  |    21    |  0x10000   |  0x1FFFFF    |    4      | 0b11110xxx |
  |    26    |  0x200000  |  0x3FFFFFF   |    5      | 0b111110xx |
  |    31    |  0x4000000 |  0x7FFFFFFF  |    6      | 0b1111110x |

  All bytes following the leading byte in a multibyte sequence have the form
  0b10xxxxxx
  in any instance the bytes 0b11111110 and 0b11111111 are illegal.
  The xs above are the bits of the actual codepoint. 
  A valid utf8 character must use the fewest bytes needed.  
 */
int32_t utf8_decode_char(const uint8_t* src){
  //utf8_min_codepoint[i] is minimum value of a valid codepoint for a
  //byte sequence of length i+1.
  static constexpr std::array<int,6> utf8_min_codepoint = 
    {{0, 0x80, 0x800, 0x10000, 0x200000, 0x4000000}};
  static constexpr std::array<uint8_t, 8> utf8_lead_char_mask = 
    {{0x7f, 0x1F, 0x0F, 0x07, 0x03, 0x01}};  
  uint8_t ch;
  uint32_t ret, min;

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
    return *str;
  }
  int remain = len - 1;
  int32_t min = utf8_min_codepoint[remain];
  uint32_t ret = (*src++) & utf8_lead_char_mask[remain];
  while(remain--) {
    uint8_t ch = *src++;
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
int is_valid_codepoint(int32_t ch){
    return ch >= 0 && ch <= 0x10FFFF
        && (ch < 0xD800 || ch > 0xDFFF)
        && (ch != 0xFFFE) && (ch != 0xFFFF);
}
std::array<uint8_t,8> utf8_encode_char(uint32_t c){
  std::array<uint8_t,8> retval;
  uint8_t *dest = retval.data();
  if(!utf8_isutf32(c)) {
    *dest = '\0'
    return retval;
  }
  if(c < 0x80) {
    *dest++ = c;
  } else if(c < 0x800) {
    *dest++ = 0xC0 | ((c >> 6) & 0x1F);
    *dest++ = 0x80 | (c & 0x3F);
  } else if(c < 0x10000) {
    *dest++ = 0xE0 | ((c >> 12) & 0xF);
    *dest++ = 0x80 | ((c >> 6) & 0x3F);
    *dest++ = 0x80 | (c & 0x3F);
  } else if(c < 0x200000) {
    *dest++ = 0xF0 | ((c >> 18) & 0x7);
    *dest++ = 0x80 | ((c >> 12) & 0x3F);
    *dest++ = 0x80 | ((c >> 6) & 0x3F);
    *dest++ = 0x80 | (c & 0x3F);
  } else if(c < 0x4000000) {
    *dest++ = 0xF8 | ((c >> 24) & 0x3);
    *dest++ = 0x80 | ((c >> 18) & 0x3F);
    *dest++ = 0x80 | ((c >> 12) & 0x3F);
    *dest++ = 0x80 | ((c >> 6) & 0x3F);
    *dest++ = 0x80 | (c & 0x3F);
  } else {
    *dest++ = 0xFC | ((c >> 30) & 0x1);
    *dest++ = 0x80 | ((c >> 24) & 0x3F);
    *dest++ = 0x80 | ((c >> 18) & 0x3F);
    *dest++ = 0x80 | ((c >> 12) & 0x3F);
    *dest++ = 0x80 | ((c >> 6) & 0x3F);
    *dest++ = 0x80 | (c & 0x3F);
  }
  *dest = '\0';
  return retval;
}
/*
  this assumes str contains only valid utf8 characters,
  I'll write something else for validating the string as we go
*/
uint32_t utf8_nth_char(const uint8_t *str, uint32_t n){
  //optimize ascii strings/leading ascii characters
  int i=0;
  while(is_ascii(str[i]) && i < n){
    i++;
  }
  if (i == n) {
    return str[i];
  }
  //ok, we actually have multibyte characters
  //from now on i is the actual index, n is the # of characters
  int j = i, size;
  while(j < n){
    uint8_t c = str[j];
    size = utf8_char_size(c);
    i += size;
    j++;
  }
  return utf8_decode_char(str+i, NULL);
}
const char *utf8_nth_ptr(const uint8_t *str, uint32_t n){
  //optimize ascii strings/leading ascii characters
  int i = 0;
  while(!(str[i] & 0x80) && i < n){
    i++;
  }
  if (i == n) {
    return str + i;
  }
  //ok, we actually have multibyte characters
  //from now on i is the actual index, j is the # of characters
  int j = i, size;
  while(j < n){
    uint8_t c = str[i];
    size = utf8_char_size(c);
    i += size;
    j++;
  }
  return str + i;
}
const char* utf8_strchr(const uint8_t *str, uint32_t len, uint32_t chr){
  size_t sz;
  int i = 0;
  int32_t c;
  //utf8_decode_char will set errno on error, by zeroing it here we make error
  //checking a bit eaiser.
  errno = 0;
  while(i < len){
    c = utf8_decode_char(str + i, &sz);
    if(c == -1){ 
      return nullptr; 
    }
    if(c == chr){
      return str+i;
    }
    i += sz;
  }
  return nullptr;
}
const char* utf8_rstrchr(const uint8_t *str, uint32_t len, uint32_t chr){
  size_t sz;
  //j is the index of the last occurance of chr
  int i = 0, j = -1;
  uint32_t c;
  /*
    It might be possible to start at the end and search backwards, but
    for now I'm searching the whole string from the begining
  */
  while(i < len){
    c = utf8_decode_char(str + i, &sz);
    //somekind of error checking here
    if(c == chr){
      j = i;
    }
    i+=sz;
  }
  return (j == -1 ? NULL : str + j);
}
uint32_t utf8_byte_pos_to_char_pos(const uint8_t *str, uint32_t len, uint32_t pos){
  uint32_t i = 0;
  //optimize ascii
  while(is_ascii(str[i++]) && i < pos);
  if(i == pos){return pos;}
  i--;
  uint32_t char_pos = i;
  while(i < pos){
    uint8_t c = str[i];
    int size = utf8_char_size(c);
    i += size;
    char_pos++;
  }
  return char_pos;
}
uint32_t utf8_char_pos_to_byte_pos(const uint8_t *str, uint32_t len, uint32_t pos){
  uint32_t char_pos = 0;
  //optimize ascii
  while(is_ascii(str[char_pos++]) && char_pos < pos);
  if(char_pos == pos){return pos;}
  char_pos--;
  uint32_t byte_pos = char_pos;
  while(char_pos < pos){
    uint8_t c = str[char_pos++];
    int size = utf8_char_size(c);
    byte_pos += size;
  }
  return byte_pos;
}
union utf16_char {
  uint16_t *val;
  struct {
    uint8_t a;
    uint8_t b;
  } be;
  struct {
    uint8_t b;
    uint8_t a;
  } le;
};
uint32_t utf16_decode_char(const uint8_t *str, int byteorder, int *used){
  if(!str){
    errno = EINVAL;
    return (uint32_t)-1;
  }
  uint16_t c = *((const uint16_t *)str);
  str += 2;
  //99% of the time this is all we need
  if(byteorder == BIG_ENDIAN){
    c = __builtin_bswap16(c);
  } else {
    assert(byteorder == LITTLE_ENDIAN);
  }
  if(c <= 0xD7FF || c >= 0xE000){
    return c;
  }
  if(c >= 0xDC00){
    errno = EILSEQ;
    return (uint32_t)-1;
  }
  uint16_t c2 = *((const uint16_t *)str);
  str += 2;
  if(byteorder == BIG_ENDIAN){
    c2 = __builtin_bswap16(c2);
  }
  if(c2 < 0xDC00 || c2 > 0xDFF){
    errno = EILSEQ;
    return (uint32_t)-1;
  }
  //Each of these is a 10 bit integer
  uint32_t high = c - 0xD800;
  uint32_t low = c2 - 0xDC00;
  //now we need to put them together into a 20 bit integer
  uint32_t ret = (high << 10) | low;
  return ret + 0x10000;
}
//utf16_char_str utf16_encode_str(uint32_t c){
//  if(c >
/*
    High \ Low 	DC00 	DC01 	…       DFFF
    D800 	010000 	010001 	… 	0103FF
    D801 	010400 	010401 	… 	0107FF
      ⋮ 	⋮ 	⋮ 	⋱ 	⋮
    DBFF 	10FC00 	10FC01 	… 	10FFFF


    0x010000 is subtracted from the code point, leaving a 20-bit number in the range 0..0x0FFFFF.
    The top ten bits (a number in the range 0..0x03FF) are added to 0xD800 to give the first 16-bit code unit or high surrogate, which will be in the range 0xD800..0xDBFF.
    The low ten bits (also in the range 0..0x03FF) are added to 0xDC00 to give the second 16-bit code unit or low surrogate, which will be in the range 0xDC00..0xDFFF.

    D800 to DFFF are reserved for the high and low surregates

*/
utf8_str_iter* make_string_iter(sl_string *str){
  utf8_str_iter *iter = malloc(sizeof(utf8_str_iter));
  iter->str = *str;
  return iter;
}

int32_t string_iter_next(utf8_str_iter *iter){
  int32_t retval;
  if(iter->str.len <= iter->pos){
    return EOF;
  }
  uint8_t c = iter->str.str[iter->pos];
  if(is_ascii(c)){
    iter->pos++;
    iter->chars_read++;
    return c;
  } else {
    //it might be more effective to write another function to decode a
    //character with a maximum possible length, but I'll do this for now
    uint32_t size = utf8_char_size(c);
    uint32_t pos = iter->pos;
    iter->pos += size;
    if (iter->pos >= iter->str.len) {
      return EOF;
    } else {
      retval = utf8_decode_char(iter->str.str, NULL);
      iter->chars_read++;
      return retval;
    }
  }
}
int32_t string_iter_prev(utf8_str_iter *iter){
  if(iter->pos == 0){
    return EOF;//it's more like begining of file but eh
  }
  uint32_t pos = iter->pos - 1;
  uint8_t c = iter->str.str[pos];
  if(is_ascii(c)){
    iter->pos = pos;
    iter->chars_read--;
    return c;
  }
  while(utf8_cont_char(iter->str.str[pos])){
    pos--;
  }
  int32_t retval = utf8_decode_char(iter->str.str + pos, NULL);
  iter->pos = pos;
  iter->chars_read--;
  return retval;
}
int32_t string_iter_next_n(utf8_str_iter *iter, unsigned int num_chars){
  //like with nth char optimize ascii
  int i = iter->pos, n = 0, start_pos = iter->pos;
  //make sure we can read num_chars, assuming they're all ascii
  if(iter->str.len <= iter->pos + num_chars){
    return EOF;
  }
  while(is_ascii(iter->str.str[i]) && n < (num_chars -1)){
    i++; n++;
  }
  //this might be off by one, I'm not sure if the above should be
  //n< num_chars or n<(num_chars-1)
  if(n == num_chars){
    return iter->str.str[i];
  }
  while(n < (num_chars-1)){
    //we only need to decode the last char, so we just need to know
    //the size of the preciding characters, which means we only need to
    //look at the first byte
    uint32_t size = utf8_char_size(iter->str.str[i]);
    i += size;
    if(i >= iter->str.len){
      return EOF;
    }
    n++;
  }
  iter->pos = i;
  int32_t retval = string_iter_next(iter);
  if(retval == EOF){
    //reset the position, which string_iter_next doesn't
    iter->pos = start_pos;
    return EOF;
  }
  //only increment chars_read if we actualy have a character to return
  iter->chars_read += num_chars;
  return retval;
}

int32_t string_iter_prev_n(utf8_str_iter *iter, unsigned int num_chars){
  //this is definately the lazy way to do this
  assert(num_chars > 0);
  int start_pos = iter->pos, start_chars_read = iter->chars_read, n=0;
  int32_t retval;
  while(n < num_chars){
    retval = string_iter_prev(iter);
    if(retval == EOF){
      iter->pos = start_pos;
      iter->chars_read = start_chars_read;
      return retval;
    }
    n++;
  }
  //I think this should work, but I might be off by an iteration
  return retval;
}
