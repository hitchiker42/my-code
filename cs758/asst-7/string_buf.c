#include "string_buf.h"
/*
  String buffers use A linked list to store segments of the string
  as it is being built. When a string is appended to the buffer (by default)
  it is added to the list without being copied and only copied when
  turning the buffer into a string, avoiding excessive copying and
  memory allocation
*/
/*the string segement structure is private to implementation of string bufs*/
typedef struct string_segment string_segment;
struct string_segment {
  const char *str;
  string_segment *next;
  uint32_t len;
};
string_segment *make_string_segment(const char *str, uint32_t len){
  string_segment *segment = sl_malloc(sizeof(struct string_segment));
  segment->str = str;
  segment->len = len;
  segment->next = NULL;
  return segment;
}
void string_buf_flush_force(sl_string_buf *buf);
SL_INLINE void string_buf_append_string_segment(sl_string_buf *buf,
                                                    string_segment *segment){
  if(buf->buf != buf->bufptr){
    string_buf_flush_force(buf);
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
void string_buf_flush_force(sl_string_buf *buf){
  uint32_t len = buf->bufptr - buf->buf;
  char *segment_str = sl_memdup_atomic(buf->buf, len);
  string_segment *segment = make_string_segment(segment_str, len);
  buf->bufptr = buf->buf;
  string_buf_append_string_segment(buf, segment);
  return;
}
/*
  Same as above but check if the buffer is empty first and don't 
  do anything if it is
*/
void string_buf_flush(sl_string_buf *buf){
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
void string_buf_append_strn(sl_string_buf *buf,
                            const char *str, uint32_t len){
  string_segment *segment = make_string_segment(str,len);
  string_buf_append_string_segment(buf, segment);
}
//since we need to copy the string no matter what we try to copy it into
//the buffer first to avoid allocating memoryf and flushing the buffer
//and only allocate/flush if it won't fit
void string_buf_append_strn_copy(sl_string_buf *buf,
                                 const char *str, uint32_t len){
  if((STRING_BUF_SIZE - (buf->bufptr-buf->buf)) > len){
    memcpy(buf->bufptr, str, len);
    buf->bufptr += len;
    return;
  } else {
    char *str_copy = sl_memdup_atomic(str, len);
    string_segment *segment = make_string_segment(str, len);
    string_buf_append_string_segment(buf, segment);
  }
}
/*
  Convert A string buffer into a string, if the list of string segments
  is empty then the buffer itself is just copied into a string, otherwise
  a buffer the size of the total length of the string is created and each
  element of the list of string segments is copied into the buffer. This
  means the string created, unlike the buffer, is independent from any
  other string.
*/
sl_string *string_buf_to_string(sl_string_buf *buf){
  uint32_t buflen = buf->bufptr - buf->buf;
  //  buf->bufptr = buf->buf;
  if(buf->segments_start == NULL){
    //if we never flushed the buffer this is much easier
    return make_string_unboxed(buf->buf, buflen, 0);
  } else {
    uint32_t len = buflen + buf->total_len;
    char *str_mem = sl_malloc_atomic(len*sizeof(char));
    char *strptr = str_mem;
    string_segment *segment = buf->segments_start;
    do {
      memcpy(strptr, segment->str, segment->len);
      strptr += segment->len;
      segment = segment->next;
    } while(segment != NULL);
    memcpy(strptr, buf->buf, buflen);
    return make_string_unboxed_nocopy(str_mem, len, 0);
  }
}
const char *string_buf_to_cstr(sl_string_buf *buf, uint32_t *len){
  sl_string *str = string_buf_to_string(buf);
  if(len != NULL){
    *len = str->len;
  }
  return str->str;
}
sl_obj string_buf_to_string_obj(sl_string_buf *buf){
  sl_string *str = string_buf_to_string(buf);
  return make_sl_obj((sl_obj)str, SL_vector);
}
sl_string_buf *malloc_string_buf(){
  sl_string_buf *buf = sl_malloc(sizeof(sl_string_buf));
  buf->bufptr = buf->buf;
  return buf;
}
