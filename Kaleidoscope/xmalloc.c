#include "xmalloc.h"
void* xmalloc (size_t size){
  register void* value = malloc(size);
  if (value == 0){
    fatal("virtual memory exhausted");
  }
  return value;
}
void* xrealloc (void* ptr, size_t size){
  register void* value = realloc(ptr,size);
  if (value == 0){
    fatal ("Virtual memory exhausted");
  }
  return value;
}
void* xcalloc (size_t nmemb,size_t size){
  register void* value = calloc(nmemb,size);
  if (value == 0){
    fatal("virtual memory exhausted");
  }
  return value;
}
