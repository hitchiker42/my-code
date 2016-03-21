#ifndef _STRING_BUF_H
#define _STRING_BUF_H
#include "C_util.h"
#include "util_string.h"
/*

  There are a few major functions of string buffers:
  -append or prepend a string, byte array, or character to the string buffer    
  -convert the contents of a string buffer into an actual string
  -delete the last character appended to the buffer, this works if and only if
    the last action performed on the buffer was appending a single character.
    this is similar to unreading a character from a stream
  -concatenate two string buffers, this is functionally identical to converting
    the second buffer to a string and appending it to the first buffer

  A string buffer can be created with make_string_buf, or created and initialized
  using string_buf_from_X, where X is char, cstr, or buf

  A quick note about performance. Appending a string to the string buffer
  requires flushing the internal buffer which is used to append characters,
  which requires allocating memory for a new string. If only a few, known,
  characters need to be added to the string buffer it is better to add
  a string constant rather than the indivual characters. 
*/
#ifndef STRING_BUF_SIZE
#define STRING_BUF_SIZE 128
#endif
typedef struct string_buf string_buf;
struct string_buf {
  uint8_t buf[STRING_BUF_SIZE];
  uint8_t *bufptr;
  struct string_segment *segments_start;
  struct string_segment *segments_end;
  uint32_t total_len; //not including the characters in the buffer
};
/*
  String bufs are stack allocated by default, you can malloc them
  if you want, but if you need to you're probably using them wrong.
*/
#define make_string_buf()                               \
  ({string_buf *buf = alloca(sizeof(string_buf)); \
    string_buf_init(buf);                               \
    buf;})
#define string_buf_alloca() make_string_buf()
#define string_buf_init(__buf)                          \
  __buf->bufptr = __buf->buf;                           \
  __buf->segments_end = (__buf->segments_start = NULL); \
  __buf->total_len = 0
#define string_buf_reinit(_buf) string_buf_init(_buf)

void string_buf_flush(string_buf *buf);
void string_buf_append_strn(string_buf *buf,
                            const char *str, uint32_t len);
void string_buf_append_strn_copy(string_buf *buf,
                                 const char *str, uint32_t len);
/*
  These functions internally call snprintf to determine how many bytes
  are needed to hold the restult, and if this is less than STRING_BUF_SIZE
  write the result into the internal buffer.
*/
void string_buf_sprintf(string_buf *buf, const char *fmt, ...);
void string_buf_vsnprintf(string_buf *buf, const char *fmt, va_list ap);

static inline void string_buf_append_char(string_buf *buf, char c){
  if((buf->bufptr-buf->buf) >= STRING_BUF_SIZE){
    string_buf_flush(buf);
  }
  *buf->bufptr++ = (uint8_t)c;
}
/*
  The default versions of string_buf_append_str & friends assume that
  the string they are passed will remain constant for the duration
  of the life of the buffer, they only copy the strings given when
  converting the buffer into a string. If the string will be modified
  use the _copy versions of the functions
*/
static inline void string_buf_append_cstr(string_buf *buf, char *str){
  string_buf_append_strn(buf, str, strlen(str));
}
static inline void string_buf_append_cstrn(string_buf *buf, 
                                           char *str, uint32_t n){
  string_buf_append_strn(buf, str, n);
}
static inline void string_buf_append_cstr_copy(string_buf *buf, char *str){
  string_buf_append_strn_copy(buf, str, strlen(str));
}
static inline void string_buf_append_cstrn_copy(string_buf *buf,
                                                char *str, uint32_t n){
  string_buf_append_strn_copy(buf, str, n);
}
static inline void string_buf_append_string(string_buf *buf, string str){
  string_buf_append_strn(buf, str.str, str.sz);
}

static inline void string_buf_append_string_copy(string_buf *buf, string str){
  string_buf_append_strn_copy(buf, str.str, str.sz);
}
static inline char string_buf_unread_char(string_buf *buf){
  //assumes that the last thing appended to buf was a single character
  //If this is not the case it results in undefined behavior
  return *buf->bufptr--;
}
//needs to be a macro for sizeof to work
#define string_buf_append_string_literal(buf, str)      \
  string_buf_append_cstrn(buf, str, sizeof(str)-1)

//this is solely for convience, literally all it does is convert buf2
//into a string and append that to buf1
void string_buf_concat(string_buf *buf1, string_buf *buf2);

string string_buf_to_string(string_buf* buf);
string *string_buf_to_string_ptr(string_buf *buf);
//The length of the resulting string is stored in len, if it is non-null
const char *string_buf_to_cstr(string_buf *buf, uint32_t *len);

string_buf *malloc_string_buf();

#define string_buf_from_x(x, typename)                  \
  ({string_buf *buf = make_string_buf();                \
    string_buf_append_##typename(buf, x);               \
    buf;})

#define string_buf_from_string(__str) string_buf_from_x(__str, string)
#define string_buf_from_char(__c) string_buf_from_x(__c, char)
#define string_buf_from_cstr(__str) string_buf_from_x(__str, cstr)


#if (defined STRING_BUF_MACROS)
//some macros with shorter names than the string buffer funcitons,
//since some of the functions can get pretty long. They are only
//defined if requested to avoid namespace pollution.

#define buf_append(__buf,s) string_buf_append_string(__buf, s)
#define buf_append_str(__buf,s) string_buf_append_string(__buf, s)
#define buf_append_char(__buf,c) string_buf_append_char(__buf, c)
#define buf_append_cstr(__buf,s) string_buf_append_cstr(__buf, s)
#define buf_append_cstrn(__buf,s,n) string_buf_append_cstrn(__buf, s, n)
#define buf_append_str_lit(buf, str) string_buf_append_string_literal(buf, str)
#define buf_append_literal(buf, str) string_buf_append_string_literal(buf, str)
#define buf_sprintf(buf, fmt, ...) string_buf_sprintf(buf, fmt, ##__VA_ARGS__)
#define buf_delete_char(__buf) string_buf_delete_char(__buf)

#define buf_to_str(__buf) string_buf_to_string(__buf)
#endif /* STRING_BUF_MACROS */
#endif /* _STRING_BUF_H_ */
