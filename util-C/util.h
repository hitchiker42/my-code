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
/*
  Macros/Functions/Data Structures which aren't needed in scilisp.
*/
#ifndef _SCILISP_
/* Macros*/
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
    x<y ? x : 0})
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
#define DOWNCASE_ASCII(c) (c > 0x40 && c < 0x5B ? c | 0x20 : c)
#define UPCASE_ASCII(c) (c > 0x60 && c < 0x7B ? c & (~0x20) : c)
#define CHAR_TO_NUMBER(c) (assert(c >= 0x30 && c <= 0x39), c - 0x30)
/* Data Structures */
//don't use cons and don't typedef to avoid poluting the user namespace
#define NIL NULL
struct _cons {
  //this is a rough attempt to provide a generic type, it should be enough
  //for most cases, obviously the user needs to know what type the values
  //actually are, since there's no tag field
  union {
    void *car;
    long car_int;
    double car_double;
  };
  union {
    void *cdr;
    long cdr_int;
    double cdr_double;
  };
}
#define XCAR(ls) (ls->car)
#define XCDR(ls) (ls->cdr)
#define XCAR_INT(ls) (ls->car_int)
#define XCDR_INT(ls) (ls->cdr_int)
#define XCAR_DOUBLE(ls) (ls->car_double)
#define XCDR_DOUBLE(ls) (ls->cdr_double)
#endif /*!_SCILISP_*/
#define get_access_mode(mode) (mode & O_ACCMODE)    
/*
  Given a file access mode, of the type accepted by open return a string
  representing the access mode that can be passed to fopen.
*/
__attribute__((const)) char *filemode_bits_to_string(int mode);
off_t file_len_by_fd(int fd);
off_t file_len_by_filename(const char *filename);
off_t file_len_by_FILE(FILE *file);
int regular_filep_FILE(FILE* file);
int regular_filep_filename(char *filename);
int regular_filep_fd(long fd);
char* read_file_to_string_fd(int fd, size_t *sz);
char* read_file_to_string_FILE(FILE *file, size_t *sz);
char* read_file_to_string_filename(FILE *file, size_t *sz);
/*
  TODO: wrap this in an #if (defined _ISOC11_SOURCE) 
  and define alternative versions for older C versions
*/
#define generic_file_macro(base_name, x, args...)       \
  _Generic((x),                                         \
           long  : base_name##fd,                      \
           int   : base_name##fd,                      \
           FILE* : base_name##FILE,                    \
           char* : base_name##filename)(x,##args)
#define file_len(x)                             \
  generic_file_macro(file_len_by_, x)
#define regular_filep(x)                                \
  generic_file_macro(regular_filep_, x)
#define read_file_to_string(x, szptr)                                   \
  generic_file_macro(read_file_to_string_, x, szptr)
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
  These do the same thing as strspn and friends, but expect a prepopulated
  table, rather than creating a table each time.
*/
uint32_t strspn_table(const uint8_t *str, const uint8_t accept[256]);
uint32_t strcspn_table(const uint8_t *str, const uint8_t reject[256]);
uint32_t memspn_table(const uint8_t *buf, uint32_t len,
                      const uint8_t accept[256]);
uint32_t memcspn_table(const uint8_t *buf, uint32_t len, 
                       const uint8_t reject[256]);
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
/*
  Functionally identical to asprintf, but uses alloca to allocate
  memory on the stack instead of using malloc.
 */
#define asprintf_alloca(fmt, args...)           \
  ({size_t sz = sprintf(NULL, fmt, 0, ##args);  \
    char *str = alloca(sz);                     \
    snprintf(str, sz, fmt, ##args);             \
    str;})

#endif
