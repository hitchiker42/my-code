#ifndef _SL_STRING_BUF_H
#define _SL_STRING_BUF_H
#include "sl_string.h"
//#define USE_CORDS
/*
  header file for string buffers which are an internal type used to construct
  strings via concatenation of characters or strings.

  There are a few major functions of string buffers:
  -append or prepend a string or character to the string buffer, both c strings
  and sl_strings can be appended
  -convert the contents of a string buffer into an actual string
  -delete the last character appended to the buffer, this works if and only if
  the last action performed on the buffer was appending a single character.
  this is similar to unreading a character from a stream
  -concatenate two string buffers, this is functionally identical to converting
  the second buffer to a string and appending it to the first buffer

  A string buffer can be created with make_string_buf, or created and initialized
  using string_buf_from_X, where X is char, string, or cstr
*/
#ifndef STRING_BUF_SIZE
#define STRING_BUF_SIZE 128
#endif
typedef struct sl_string_buf sl_string_buf;
struct sl_string_buf {
  char buf[STRING_BUF_SIZE];
  char *bufptr;
  struct string_segment *segments_start;
  struct string_segment *segments_end;
  uint32_t total_len; //not including the characters in the buffer
};
#define make_string_buf()                                       \
  ({sl_string_buf *buf = alloca(sizeof(sl_string_buf));         \
    string_buf_init(buf);                                       \
    buf;})
#define string_buf_alloca() make_string_buf()
#define string_buf_init(__buf)                          \
  __buf->bufptr = __buf->buf;                           \
  __buf->segments_end = (__buf->segments_start = NULL); \
  __buf->total_len = 0
#define string_buf_reinit(_buf) string_buf_init(_buf)
void string_buf_flush(sl_string_buf *buf);
void string_buf_append_strn(sl_string_buf *buf,
                            const char *str, uint32_t len);
void string_buf_append_strn_copy(sl_string_buf *buf,
                                 const char *str, uint32_t len);
static inline void string_buf_append_char(sl_string_buf *buf, char c){
  if((buf->bufptr-buf->buf) >= STRING_BUF_SIZE){
    string_buf_flush(buf);
  }
  *buf->bufptr++ = c;
}
/*
  The default versions of string_buf_append_str & friends assume that
  the string they are passed will remain constant for the duration
  of the life of the buffer, they only copy the strings given when
  converting the buffer into a string. If the string will be modified
  use the _copy versions of the functions
*/
static inline void string_buf_append_str(sl_string_buf *buf, sl_string *str){
  string_buf_append_strn(buf, str->str, str->len);
}
static inline void string_buf_append_cstr(sl_string_buf *buf, char *str){
  string_buf_append_strn(buf, str, strlen(str));
}
static inline void string_buf_append_cstrn(sl_string_buf *buf,
                                      char *str, uint32_t n){
  string_buf_append_strn(buf, str, n);
}

static inline void string_buf_append_string_copy(sl_string_buf *buf,
                                            sl_string *str){
  string_buf_append_strn_copy(buf, str->str, str->len);
}
static inline void string_buf_append_cstr_copy(sl_string_buf *buf, char *str){
  string_buf_append_strn_copy(buf, str, strlen(str));
}
static inline void string_buf_append_cstrn_copy(sl_string_buf *buf,
                                      char *str, uint32_t n){
  string_buf_append_strn_copy(buf, str, n);
}
static inline void string_buf_unread_char(sl_string_buf *buf){
  //assumes that the last thing appended to buf was a single character
  //If this is not the case it results in undefined behavior
  buf->bufptr--;
}
sl_string* sl_sprint(sl_obj obj);
static inline void string_buf_append_obj(sl_string_buf *buf, sl_obj obj){
  sl_string *str = sl_sprint(obj);
  string_buf_append_str(buf, str);
}

//this is solely for convience, literally all it does is convert buf2
//into a string and append that to buf1
void string_buf_concat(sl_string_buf *buf1, sl_string_buf *buf2);

sl_string* string_buf_to_string(sl_string_buf* buf);
//The length of the resulting string is stored in len, if it is non-null
const char *string_buf_to_cstr(sl_string_buf *buf, uint32_t *len);
sl_obj string_buf_to_string_obj(sl_string_buf *buf);
sl_string_buf *malloc_string_buf();

#define string_buf_from_x(x, typename)                  \
  ({sl_string_buf *buf = make_string_buf();             \
  string_buf_append_##typename(buf, x);                 \
  buf;})
#define string_buf_from_string(__str) string_buf_from_x(__str, str)
#define string_buf_from_char(__c) string_buf_from_x(__c, char)
#define string_buf_from_cstr(__str) string_buf_from_x(__str, cstr)


#if (defined STRING_BUF_MACROS)
//some macros for faster string buffers, and shorter names
//because the names don't have the sl(_string) prefix
//they are only defined if requested


#define buf_append(__buf,s) string_buf_append_str(__buf, s)
#define buf_append_str(__buf,s) string_buf_append_str(__buf, s)
#define buf_append_char(__buf,c) string_buf_append_char(__buf, c)
#define buf_append_cstr(__buf,s) string_buf_append_cstr(__buf, s)
#define buf_append_cstrn(__buf,s,n) string_buf_append_cstr(__buf, s, n)
#define buf_delete_char(__buf) string_buf_delete_char(__buf)

#define buf_to_str(__buf) string_buf_to_string(__buf)
#define buf_append_str_obj(__buf, obj) buf_append(__buf, XSTRING(obj))
#define buf_append_obj(__buf, obj) string_buf_append_obj(__buf, obj);
#endif /* STRING_BUF_MACROS */
#endif /* _SL_STRING_BUF_H_ */
