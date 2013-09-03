#ifndef _XMALLOC_H_
#define _XMALLOC_H_
#include <stdlib.h>
#include <stdio.h>
#define fatal(message)                          \
  fprintf(stderr,"Fatal Error:" message "\n");  \
  abort()
extern inline void* xmalloc (size_t size){
  register void* value = malloc(size);
  if (value == 0){
    fatal("virtual memory exhausted");
  }
  return value;
}
extern inline void* xrealloc (void* ptr, size_t size){
  register void* value = realloc(ptr,size);
  if (value == 0){
    fatal ("Virtual memory exhausted");
  }
  return value;
}
extern inline void* xcalloc (size_t nmemb,size_t size){
  register void* value = calloc(nmemb,size);
  if (value == 0){
    fatal("virtual memory exhausted");
  }
  return value;
}
#endif
