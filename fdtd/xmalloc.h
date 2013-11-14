#ifndef _XMALLOC_H_
#define _XMALLOC_H_
#include <stdlib.h>
#include <stdio.h>
#define fatal(message)                          \
  fprintf(stderr,"Fatal Error:" message "\n");  \
  abort()
static inline void* xmalloc (size_t size){
  register void* value = malloc(size);
  if (value == 0){
    fatal("virtual memory exhausted");
  }
  return value;
}
static inline void* xrealloc (void* ptr, size_t size){
  register void* value = realloc(ptr,size);
  if (value == 0){
    fatal ("Virtual memory exhausted");
  }
  return value;
}
static inline void* xcalloc (size_t nmemb,size_t size){
  register void* value = calloc(nmemb,size);
  if (value == 0){
    fatal("virtual memory exhausted");
  }
  return value;
}
/*static inline int xmemalign(void** value,size_t alignment,size_t size){
  register int retval;
  retval=posix_memalign(value,alignment,size);
  if (!retval){
    fatal("virtual memory exhausted");
  }
  return retval;
  }*/
static inline void* xmemalign(size_t size,size_t align){
  void* value;
  posix_memalign(&value,align,size);
  if (!value){
    fatal("virtual memory exhausted");
  }
  return value;
}
#endif
