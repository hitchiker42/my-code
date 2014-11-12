#ifndef __UTIL_H_
#define __UTIL_H_
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
#define ARR_MINMAX(arr, len, comp)              \
  __extension__({                               \
      __typeof__(arr[0]) min = arr[0];          \
      int i;                                    \
      for(i=1;i<len;i++){                       \
        if(comp(arr[i], min)){                  \
          min = arr[i];                         \
        }                                       \
      }                                         \
      min;})

#ifdef DEBUG
#define DEBUG_PRINTF(fmt, args...) fprintf(stderr, fmt, ##args)
#define HERE() fprintf("here in %s at line %d in function %s\n",        \
                       __FILE__,__LINE__,__func__)
#else
#define DEBUG_PRINTF(...)
#define HERE()
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
typedef struct svector svector;
__extension__ struct svector {
  union {
    double *doubles;
    uint64_t *quadwords;
    uint32_t *doublewords;
    uint16_t *words;
    uint8_t *bytes;
    void *pointers;
  };
  size_t len;
  size_t size;
};
struct buffer {
  void *mem;
  size_t size;
};

//macros for using an svector as an fifo queue
/*
  Add elt to the end of svector s, extending s if it's too small.
  elt is only evaluated once, but s is evaluated multiple times
*/
#define svector_push(elt, s)                                            \
  __extension__({                                                       \
      __typeof__(elt)* temp = (__typeof__(elt)*)s->pointers;        \
      if(s->len >= s->size){                                            \
        xrealloc(s->pointers, s->size*2 * sizeof(__typeof__(elt)));     \
        s->size *=2;                                                    \
      }                                                                 \
      temp[s->len++] = elt;                                             \
      ;})
/*
  remove an element of type 'type' from the end of s. It is necessary
  to specify the type so the correct size element can be removed
*/
#define svector_pop(s, type)                    \
  __extension__({                               \
      type retval;                              \
      type *temp = (type*)s->pointers;          \
      retval = temp[--s->len];                  \
      retval;})
//this says 0 is a power of two, if thats an issue check val != 0 before
//calling this 
#define is_power_of_2(val) (~(val & (val-1)))
//Finds the nearest power of two greater than val
//assumes val is > 0 (not >= 0, because if val == 0 this will fail)
#define nearest_power_of_2(val) (1 << (sizeof(long) - __builtin_clzl(val)))
off_t file_len_by_fd(int fd);
off_t file_len_by_name(const char *filename);
off_t FILE_len(FILE *file);
struct buffer mmap_file(int fd, int shared);
void *mmap_anon(size_t sz);
static struct buffer null_buffer = {NULL,0};
#define index_2d(x,y,N) (y*N + x)
#define index_3d(x,y,z,N,M) ((z * (M+N)) + y*N + x)
#endif
