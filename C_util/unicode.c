#include "utf8.h"
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
  in any instance the bytes 0b11111110 and 0b11111111 are illegal
 */
/*
  these two functions assume c isn't 0xff/0xfe
*/
static inline int utf8_lead_char(uint8_t c){
  return is_ascii(c) || (c & 0x40);
}
static inline int utf8_cont_char(uint8_t c){
  return ((c & 0x80)  && !(c & 0x40));
}

/*
  Things to add
  char *utf8_char_before_or_at(char *str, int n);
  if str[n] is not a lead byte return a pointer to the first leading byte
  found before str[n], otherwise just return str+n

  char *utf8_char_after_or_at(char *str, int n);
  same as utf8_char_before_or_at except the first leading byte after s[n] is returned
  if str[n] is a continuation byte

  sl_char nth_char(sl_string *str);
  return the nth character in the string str, if str is composed of only ascii
  characters this is just str->str[n] however if str has multibyte characters
  this might not be the case

  structure used to iterate across a utf8 string by characters (quickly)
  struct str_iterator {
    sl_string str;
    int char_num;
    int pos;
  }

  sl_char str_iter_next(str_iterator *iter);
  return the next character, or EOF if at the end of the string

  sl_char str_iter_prev(str_iterator *iter);//probably not useful

  sl_string *string_map_chars(sl_string *str, sl_char(*fp)(sl_char));
  iterate across the string str calling fp on each character,
  this might be a bit slow since we need to create a new string
  and encode the return value of fp to utf-8

*/
inline int utf8_char_size(unsigned char c){
  int size = 0;
  if(!(c & 0x80)){
    size = 1;
  } else if ((c & 0xE0) == 0xC0) {
    size = 2;
  } else if((c & 0xF0) == 0xE0) {
    size = 3;
  } else if((c & 0xF8) == 0xF0) {
    size = 4;
  } else if((c & 0xFC) == 0xF8) {
    size = 5;
  } else if((c & 0xFE) == 0xFC) {
    size = 6;
  }
  return size;
}
/*
  This is mostly from libutf8 with minor changes
*/
uint32_t utf8_decode_char(const uint8_t* src, int *used){
  uint8_t ch;
  uint32_t ret, min;
  int remain;
  if(!src){
    errno = EINVAL;
    return (uint32_t)-1;
  }
  if(used){*used = 1;}

  ch = *src++;

  if(ch & 0x80) {
    if((ch & 0xE0) == 0xC0) {
      min = 0x80;
      remain = 1;
      if(used) *used = 2;
      ret = ch & 0x1F;
    } else if((ch & 0xF0) == 0xE0) {
      min = 0x800;
      remain = 2;
      if(used) *used = 3;
      ret = ch & 0x0F;
    } else if((ch & 0xF8) == 0xF0) {
      min = 0x10000;
      remain = 3;
      if(used) *used = 4;
      ret = ch & 0x07;
    } else if((ch & 0xFC) == 0xF8) {
      min = 0x200000;
      remain = 4;
      if(used) *used = 5;
      ret = ch & 0x03;
    } else if((ch & 0xFE) == 0xFC) {
      min = 0x4000000;
      remain = 5;
      if(used) *used = 6;
      ret = ch & 0x01;
    } else {
      errno = EILSEQ;
      return (wchar_t)-1;
    }

    while(remain--) {
      ch = *src++;
      if((ch & 0xC0) != 0x80) {
        errno = EILSEQ;
        return (uint32_t)-1;
      }
      ret <<= 6;
      ret |= ch & 0x3F;
    }

    if(ret < min) {
      errno = EILSEQ;
      return (uint32_t)-1;
    }

    return ret;
  }
  return ch;
}

int utf8_isutf32(int32_t ch){
    return ch >= 0 && ch <= 0x10FFFF
        && (ch < 0xD800 || ch > 0xDFFF)
        && (ch != 0xFFFE) && (ch != 0xFFFF);
}

//8 is only 64 bits, i.e a long, so it's no problem returning that
utf8_char_str utf8_encode_char(uint32_t c){
  utf8_char_str retval = {0};//this way the string is nul terminated (usually)
  //the string's length can be obtained by strlen or by examining the first char
  char *dest = retval.str;
  if(!utf8_isutf32(c)) {
    retval.val = -1;
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
  uint32_t c;
  while(i < len){
    c = utf8_decode_char(str + i, &sz);
    //somekind of error checking  here
    if(c == chr){
      return str+i;
    }
    i+=sz;
  }
  return NULL;
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
#if (defined _SCILISP_)
utf8_str_iter* make_string_iter(sl_string *str){
  utf8_str_iter *iter = sl_malloc(sizeof(utf8_str_iter));
  iter->str = *str;
  return iter;
}

sl_char string_iter_next(utf8_str_iter *iter){
  sl_char retval;
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
sl_char string_iter_prev(utf8_str_iter *iter){
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
  sl_char retval = utf8_decode_char(iter->str.str + pos, NULL);
  iter->pos = pos;
  iter->chars_read--;
  return retval;
}
sl_char string_iter_next_n(utf8_str_iter *iter, unsigned int num_chars){
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
  sl_char retval = string_iter_next(iter);
  if(retval == EOF){
    //reset the position, which string_iter_next doesn't
    iter->pos = start_pos;
    return EOF;
  }
  //only increment chars_read if we actualy have a character to return
  iter->chars_read += num_chars;
  return retval;
}

sl_char string_iter_prev_n(utf8_str_iter *iter, unsigned int num_chars){
  //this is definately the lazy way to do this
  assert(num_chars > 0);
  int start_pos = iter->pos, start_chars_read = iter->chars_read, n=0;
  sl_char retval;
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
#endif
