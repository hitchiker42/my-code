#ifndef _UTIL_H_
#define _UTIL_H_
/*
  Various utility C functions, specifically functions that would fit as
  stdlib functions.
*/
#include <sys/stat.h>
#include <fcntl.h>
#include <sys/mman.h>
#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>
#include <time.h>
#include <sys/time.h>
#if !(defined PAGE_SIZE)
#if (defined PAGESIZE)
#define PAGE_SIZE PAGESIZE
#else
#define PAGE_SIZE 4096
#endif
#endif
#define SWAP(x,y)                               \
  ({__typeof(x) __temp = x;                     \
    x = y;                                      \
    y = __temp;})
#define ARR_SWAP(arr,i,j)                       \
  ({__typeof(arr[i]) __temp = arr[i];           \
    arr[i] = arr[j];                            \
    arr[j] = __temp;})
#define MIN(_x,_y)                                \
  ({__typeof(_x) x = _x;                          \
    __typeof(_y) y = _y;                          \
    x<y ? x : 0;})
#define MAX(_x,_y)                                \
  ({__typeof(_x) x = _x;                          \
    __typeof(_y) y = _y;                          \
    x>y ? x : y;})
#define IS_POW_OF_2(num) (!(num & (num-1)))
#define NEXT_POW_OF_2(num)                                \
  ({int leading_zeros = __builtin_clzl(num);              \
    (1UL << (64 - leading_zeros));})

//these 3 macros should work the same on floating point numbers
#define SIGN(x) ((x) < 0)
#define NEG(x) (-(x))
#define ABS(x) ((x) < 0 ? -(x) : (x))
//these 3 macros act on the underlying bit patterns
//with some work SIGNBIT could work on floats
//signed shifts are techincally undefined, but who cares
#define SIGNBIT(x)                              \
  ({__typeof(x) tmp = x;                        \
    int shift = (sizeof(x) * CHAR_BIT)-1;       \
  (x & (1 << shift - 1))>>shift;})

#define BITNEG(x) (~(x)+1)

#define BITABS(x) (SIGNBIT(x) ? BITNEG(x) : x)
/*
  Given a file access mode, of the type accepted by open return a string
  representing the access mode that can be passed to fopen.
*/
__attribute__((const)) char *filemode_bits_to_string(int mode);
/*
  Various functions that act on files take a void * as an argument and
  an integer specifiying what that argument is.

  For now these funcitons can take either a file name or a file
  descriptor as an argument. The integer should be 0 for a filname
  and 1 for a file descriptor
*/
off_t file_len_by_fd(int fd);
off_t file_len_by_name(const char *filename);
off_t FILE_len(FILE *file);
#define file_len(x)                                                     \
  ({ __typeof (x) arg = (x);                                            \
    off_t retval = 0;                                                   \
    if(__builtin_types_compatible_p(typeof(x), int) ||                  \
       __builtin_types_compatible_p(typeof(x), long)){                  \
      retval = file_len_by_fd((long)arg);                               \
    } else if (__builtin_types_compatible_p(typeof(x), char*) ||        \
               __builtin_types_compatible_p(typeof(x), uint8_t *)){     \
      retval = file_len_by_name(arg, 0);                                \
    } else if (__builtin_types_compatible_p(typeof(x), FILE*)){         \
      retval = FILE_len(arg);                                           \
    } else {                                                            \
      abort();                                                          \
    }                                                                   \
    retval;})
/*
  return 1 if the file is a regular file
*/
int __regular_filep(void *arg, int is_fd);
#define regular_filep(x)                                                \
  ({ __typeof (x) arg = (x);                                            \
    int retval = 0;                                                     \
    if(__builtin_types_compatible_p(typeof(x), int) ||                  \
       __builtin_types_compatible_p(typeof(x), long)){                  \
      long _fd = (long)arg;                                             \
      retval = __regular_filep(((void*)_fd), 1);                        \
    } else if (__builtin_types_compatible_p(typeof(x), char*) ||        \
               __builtin_types_compatible_p(typeof(x), uint8_t *)){     \
      retval = __regular_filep((void*)(long)arg,0);                     \
    } else {                                                            \
      abort();                                                          \
    }                                                                   \
    retval;})
//mmap the file given by fd, return a struct contanintg a pointer
//to the maping and the length of the mapping, if shared is
//nonzero the mapping is shared, otherwise it is private
void *mmap_file(int fd, int shared, size_t *sz);
/*
  mmap an anonymous region of memory size bytes long and return a pointer to it
*/
void* mmap_anon(size_t size);
/* Akin to strspn/strcspn for byte arrays */
uint32_t memspn(const uint8_t *buf, uint32_t len,
                const uint8_t *accept, uint32_t len2);
uint32_t memcspn(const uint8_t *buf, uint32_t len,
                 const uint8_t *reject, uint32_t len2);
/*
  convert a number of seconds in floating point format to a timespec
*/
struct timespec float_to_timespec(double t);
/*
  convert a timespec into a floating point number of seconds
*/
double timespec_to_float(struct timespec t);
/*
  sleep for the number of seconds indicated by sleep_time, if interputed
  return sleep_time - the time actually slept.
*/
double float_sleep(double sleep_time);
/*
  sleep for the numbor of seconds indicated by sleep time, will always
  sleep for at least sleep_time seconds even if interupted.
*/
void float_sleep_full(double sleep_time);
/* I was going to write a byte array version of strstr but 
   it already exists (memmem, which is a gnu extension) */
#endif
