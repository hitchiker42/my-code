#ifndef __UTIL_H_
#define __UTIL_H_
#define _GNU_SOURCE
#include <stdlib.h>
#include <stdio.h>
#include <alloca.h>
#include <stdint.h>
#include <string.h>
#define ARR_SWAP(arr,i,j)                                       \
  __extension__({ __typeof__(arr[i]) __temp = arr[i];           \
    arr[i] = arr[j];                                            \
    arr[j] = __temp;                                            \
    ;})
#define SWAP(x,y)                                               \
  __extension__({ __typeof__(x) __temp = x;                     \
  x = y;                                                        \
  y = __temp;                                                   \
  ;})
#define MAX(x,y)                                                 \
  __extension__({ __typeof(x) __x = x;                           \
      __typeof(y) __y = y;                                       \
      (__x > __y ? __x : __y);})
#define MIN(x,y)                                                 \
  __extension__({ __typeof(x) __x = x;                           \
      __typeof(y) __y = y;                                       \
      (__x < __y ? __x : __y);})
#ifdef DEBUG
#define DEBUG_PRINTF(fmt, args...) fprintf(stderr, fmt, ##args)
#else
#define DEBUG_PRINTF(...)
#endif
typedef unsigned int uint;
typedef unsigned long ulong;
static __attribute__((unused,malloc))  void *xmalloc(size_t sz){
  void *temp = calloc(sz,1);
  if(!temp && sz){
    fprintf(stderr,"Error, out of memory\n");
    exit(1);
  }
  return temp;
}
static  __attribute__((unused,malloc)) void *xmalloc_atomic(size_t sz){
  void *temp = malloc(sz);
  if(!temp && sz){
    fprintf(stderr,"Error, out of memory\n");
    exit(1);
  }
  return temp;
}
static __attribute__((unused))  void *xrealloc(void *ptr, size_t sz){
  void *temp = realloc(ptr, sz);
  if(!temp && sz){
    fprintf(stderr,"Error, out of memory\n");
    exit(1);
  }
  return temp;
}
static __attribute__((unused))  int string_eq(const char *x, const char *y){
  return !strcmp(x,y);
}
static __attribute__((unused)) void *memdup(const void *src, size_t sz){
  void *dest = xmalloc_atomic(sz);
  memcpy(dest,src,sz);
  return dest;
}
#define index_2d(x,y,N) (y*N + x)
#define index_3d(x,y,z,N,M) ((z * (M+N)) + y*N + x)
#endif
