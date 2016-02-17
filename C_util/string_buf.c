#include "string_buf.h"
/*
  String buffers use A linked list to store segments of the string
  as it is being built. When a string is appended to the buffer
  it is added to the list without being copied (by default) and only
  copied when turning the buffer into a string, avoiding excessive copying
  and memory allocation.

  Characters are appended to and internal buffer, which is converted into
  a string when full, or when a string is appended. This means performance
  wise it is best to avoid intermixing appending characters and strings
  to the string buffer
*/
/*the string segement structure is an implementation detail*/
typedef struct string_segment string_segment;
struct string_segment {
  const char *str;
  string_segment *next;
  uint32_t len;
};
string_segment *make_string_segment(const char *str, uint32_t len){
  string_segment *segment = xmalloc(sizeof(struct string_segment));
  segment->str = str;
  segment->len = len;
  segment->next = NULL;
  return segment;
}
static void string_buf_force_flush(string_buf *buf);
static inline void string_buf_append_string_segment(string_buf *buf,
                                                    string_segment *segment){
  if(buf->buf != buf->bufptr){
    string_buf_force_flush(buf);
  }
  if(buf->segments_start == NULL){
    buf->segments_start = segment;
    buf->segments_end = segment;
  } else {
    buf->segments_end->next = segment;
    buf->segments_end = segment;
  }
  buf->total_len += segment->len;
  return;
}
/*
  Copy the current contents of the buffer, append it to the list of
  segments and mark the buffer as empty. This version doesn't
  check if the buffer is empty so should only be used if it known
  that the buffer is non-empty
*/
static void string_buf_force_flush(string_buf *buf){
  uint32_t len = buf->bufptr - buf->buf;
  char *segment_str = memdup(buf->buf, len);
  string_segment *segment = make_string_segment(segment_str, len);
  buf->bufptr = buf->buf;
  string_buf_append_string_segment(buf, segment);
  return;
}
/*
  Same as above but check if the buffer is empty first and don't
  do anything if it is
*/
void string_buf_flush(string_buf *buf){
  if(buf->buf == buf->bufptr){
    return;
  } else {
    string_buf_flush_force(buf);
  }
}
/*
  Append a string of known length to the list. If the buffer is nonempty
  it is flushed first. The string is not copied.
*/
void string_buf_append_strn(string_buf *buf,
                            const char *str, uint32_t len){
  string_segment *segment = make_string_segment(str,len);
  string_buf_append_string_segment(buf, segment);
}
//since we need to copy the string no matter what we try to copy it into
//the buffer first to avoid allocating memory and flushing the buffer
//and only allocate/flush if it won't fit
void string_buf_append_strn_copy(string_buf *buf,
                                 const char *str, uint32_t len){
  if((STRING_BUF_SIZE - (buf->bufptr-buf->buf)) > len){
    memcpy(buf->bufptr, str, len);
    buf->bufptr += len;
    return;
  } else {
    char *str_copy = memdup(str, len);
    string_segment *segment = make_string_segment(str, len);
    string_buf_append_string_segment(buf, segment);
  }
}
/*
  We call vsnprintf once to determine the size needed, then again
  to actualy print the restult. This is slower than requiring the
  user to manually specify the length, but formatted output is 
  going to be slow no matter what so we may as well be safe.
*/
void string_buf_vsprintf(string_buf *buf, const char *fmt, va_list ap){
  va_list ap2;
  va_copy(ap2, ap);
  int nbytes = vsnprintf(NULL, 0, fmt, ap2);
  va_end(ap2);
  //The +1 is for the terminating nul byte, we don't need it but snprintf
  //insists on putting it there (the return value of the previous call
  //doesn't count the nul byte).
  if((nbytes+1) < STRING_BUF_SIZE){
    if((nbytes+1) > ((STRING_BUF_SIZE - (buf->bufptr - buf->buf)))){
      string_buf_flush(buf);
    }
    vsnprintf(buf->bufptr, nbytes+1, fmt, ap);
    buf->bufptr += nbytes;
    return;
  } else {
    char *mem = xmalloc(nbytes+1);
    vsnprintf(mem, nbytes+1, fmt, ap);
    string_buf_append_strn(buf, mem, nbytes);
    return;
  }
}
void string_buf_sprintf(string_buf *buf, const char *fmt, ...){
  va_list ap;
  va_start(ap, fmt);
  string_buf_vsnprintf(buf, fmt, ap);
  va_end(ap);
  return;
}
  
/*
  Convert A string buffer into a string, if the list of string segments
  is empty then the buffer itself is just copied into a string, otherwise
  a buffer the size of the total length of the string is created and each
  element of the list of string segments is copied into the buffer. This
  means the string created, unlike the buffer, is independent from any
  other string.
*/
string string_buf_to_string(string_buf *buf){
  uint32_t buflen = buf->bufptr - buf->buf;
  //  buf->bufptr = buf->buf;
  if(buf->segments_start == NULL){
    //if we never flushed the buffer this is much easier
    char *mem = xmalloc(buflen);
    memcpy(mem, buf->buf, buflen);
    return make_string(mem, buflen, 0);
  } else {
    uint32_t len = buflen + buf->total_len;
    char *str_mem = xmalloc(len*sizeof(char));
    char *strptr = str_mem;
    string_segment *segment = buf->segments_start;
    string_segment *tmp;
    do {
      memcpy(strptr, segment->str, segment->len);
      strptr += segment->len;
      tmp = segment;
      segment = segment->next;
      free(tmp);
    } while(segment != NULL);
    memcpy(strptr, buf->buf, buflen);
    return make_string(str_mem, len);
  }
}
string *string_buf_to_string_ptr(string_buf *buf){
  string tmp = string_buf_to_string(buf);
  string *ret = xmalloc(sizeof(string));
  *ret = tmp;
  return ret;
}

const char *string_buf_to_cstr(string_buf *buf, uint32_t *len){
  string str = string_buf_to_string(buf);
  if(len != NULL){
    *len = str.len;
  }
  return str.str;
}

string_buf *malloc_string_buf(){
  string_buf *buf = malloc(sizeof(string_buf));
  buf->bufptr = buf->buf;
  return buf;
}
