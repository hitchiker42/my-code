#ifndef _C_UTIL_H_
#define _C_UTIL_H_
#define _GNU_SOURCE
#ifdef __cplusplus
extern "C" {
#endif
//check for C11
#if defined(__STDC_VERSION__) && (__STDC_VERSION__ >= 201112L)  && \
  !(defined(HAVE_C11)) /* C11 */
#define HAVE_C11
#endif
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
#include <unistd.h>
#include <execinfo.h>
#include <signal.h>
#include <string.h>
#include <math.h>
#include <limits.h>
#include <ctype.h>
#include <errno.h>
#if !(defined PAGE_SIZE)
#if (defined PAGESIZE)
#define PAGE_SIZE PAGESIZE
#else
#define PAGE_SIZE 4096
#endif
#endif

typedef unsigned int uint;
typedef unsigned long ulong;

/* Macros*/
#define SWAP(x,y)                               \
  __extension__ ({__typeof(x) __temp = x;       \
    x = y;                                      \
    y = __temp;})
#define ARR_SWAP(arr,i,j)                       \
  __extension__ ({__typeof(arr[i]) __temp = arr[i];     \
    arr[i] = arr[j];                            \
    arr[j] = __temp;})
#define MIN(_x,_y)                                \
  __extension__ ({__typeof(_x) x = _x;                          \
    __typeof(_y) y = _y;                          \
    x<y ? x : y;})
#define MAX(_x,_y)                                \
  __extension__ ({__typeof(_x) x = _x;                          \
    __typeof(_y) y = _y;                          \
    x>y ? x : y;})
#define IS_POW_OF_2(num) (!(num & (num-1)))
#define NEXT_POW_OF_2(num)                                \
  __extension__ ({int leading_zeros = __builtin_clzl(num);              \
    (1UL << (64 - leading_zeros));})
//The nearest integer to x/y, where x and y are integers
#define IDIV_CEIL(x,y)                           \
  __extension__ ({__typeof(x) quot, rem, _x = x;               \
    __typeof(y) _y = y;                         \
    quot = x/y; rem = x % y;                    \
    (quot + (rem !=  0));})

//these 3 macros should work the same on floating point numbers
#define SIGN(x) ((x) < 0)
#define NEG(x) (-(x))
#define ABS(x) ((x) < 0 ? -(x) : (x))
//these 3 macros act on the underlying bit patterns
//with some work SIGNBIT could work on floats
//signed shifts are techincally undefined, but who cares
#define SIGNBIT(x)                              \
  __extension__ ({__typeof(x) tmp = x;                        \
    int shift = (sizeof(x) * CHAR_BIT)-1;       \
    ((x) & (1 << (shift - 1)))>>shift;})
//test the sign on an n bit integer
#define SIGNBIT_N(x,n)                          \
  __extension__ ({int bit = (1 << (n-1));       \
    (x & bit)>>(n-1);})
#define BITNEG(x) (~(x)+1)
//fails for the most negitive integer (i.e -128 for an 8 bit int)
#define BITABS(x) (SIGNBIT(x) ? BITNEG(x) : x)
#define DOWNCASE_ASCII(c) (c > 0x40 && c < 0x5B ? c | 0x20 : c)
#define UPCASE_ASCII(c) (c > 0x60 && c < 0x7B ? c & (~0x20) : c)
#define CHAR_TO_NUMBER(c) (assert(c >= 0x30 && c <= 0x39), c - 0x30)
#define ARRAY_LEN(a) sizeof(a)/sizeof(a[0])
/*
  Some more math constants beyond those defined in math.h
*/
#define FLOAT_CONST(double_val) CAT(double_val, f)
#define M_PIf CAT(M_PI, f)
#define M_TAUf CAT(M_TAU, f)

#define CLAMP(x, min, max) MAX(MIN(x, max),min)

#define M_SQRT2_2  0.5743491774985175  /* sqrt(2)/2 */
#define M_TAU 2*M_PI
#define M_SQRT3 1.7320508075688772 /* sqrt(3) */

//Control structures
//TODO: Figure out how to font-lock these in emacs
#define unless(cond) if(!(cond))
#define until(cond) while(!(cond))

//Macro overloading

//overload w/upto 8 args, each overload can be uniquely named
//Usage, For a macro FOO with 3 variants, FOO3, FOO2 and FOO1 define as
//#define FOO(...) GET_MACRO(3, __VA_ARGS__, FOO3, FOO2, FOO1)(__VA_ARGS__)
//However unless there's a reason to name the variants uniquely, rather than
//above (i.e FOO1, FOO2, FOO3), it's better to use VFUNC
#define CAT(a,b) PRIMITIVE_CAT(a, b)
#define PRIMITIVE_CAT(a,b) a ## b
#define CAT3(a,b,c) PRIMITIVE_CAT3(a, b, c)
#define PRIMITIVE_CAT3(a,b,c) a ## b ## c
#define CAT4(a,b,c,d) PRIMITIVE_CAT4(a, b, c, d)
#define PRIMITIVE_CAT4(a,b,c,d) a ## b ## c ## d
#define MACROEXPAND(...) __VA_ARGS__
#define GET_MACR0_1(_1,NAME,...) MACROEXPAND(NAME)
#define GET_MACRO_2(_1,_2,NAME,...) MACROEXPAND(NAME)
#define GET_MACRO_3(_1,_2,_3,NAME,...) MACROEXPAND(NAME)
#define GET_MACRO_4(_1,_2,_3,_4,NAME,...) MACROEXPAND(NAME)
#define GET_MACRO_5(_1,_2,_3,_4,_5,NAME,...) MACROEXPAND(NAME)
#define GET_MACRO_6(_1,_2,_3,_4,_5,_6,NAME,...) MACROEXPAND(NAME)
#define GET_MACRO_7(_1,_2,_3,_4,_5,_6,_7,NAME,...) MACROEXPAND(NAME)
#define GET_MACRO_8(_1,_2,_3,_4,_5,_6,_7,_8,NAME,...) MACROEXPAND(NAME)
#define GET_MACRO(n,...) GET_MACRO_##n(__VA_ARGS__)
// overload w/upto 63 args, all overloads must be of the form
// base_n, where n is the number of args
//__NARG__ in effect, computes the number of arguments passed to it
#define __NARG__(...)  __NARG_I_(__VA_ARGS__,__RSEQ_N())
#define __NARG_I_(...) __ARG_N(__VA_ARGS__)
#define __ARG_N(_1, _2, _3, _4, _5, _6, _7, _8, _9,_10,         \
                _11,_12,_13,_14,_15,_16,_17,_18,_19,_20,        \
                _21,_22,_23,_24,_25,_26,_27,_28,_29,_30,        \
                _31,_32,_33,_34,_35,_36,_37,_38,_39,_40,        \
                _41,_42,_43,_44,_45,_46,_47,_48,_49,_50,        \
                _51,_52,_53,_54,_55,_56,_57,_58,_59,_60,        \
                _61,_62,_63,N,...) N
#define __RSEQ_N() 63, 62, 61, 60, 59, 58, 57, 56, 55, 54, 53,  \
                   52, 51, 50, 49, 48, 47, 46, 45, 44, 43, 42,  \
                   41, 40, 39, 38, 37, 36, 35, 34, 33, 32, 31,  \
                   30, 29, 28, 27, 26, 25, 24, 23, 22, 21, 20,  \
                   19, 18, 17, 16, 15, 14, 13, 12, 11, 10, 9,   \
                   8, 7, 6, 5, 4, 3, 2, 1, 0
// expands into func##n(__VA_ARGS__) where n is the number of arguments
#define VFUNC(func, ...) CAT(func, __NARG__(__VA_ARGS__))(__VA_ARGS__)
/*
  Macros for some C extensions
*/
#define ATTRIBUTE(...) __attribute__((__VA_ARGS__))
#define UNREACHABLE __builtin_unreachable()
#define ATTRIBUTE_NORETURN __attribute__((noreturn))
#define ATTRIBUTE_UNUSED __attribute__((unused))
#define ATTRIBUTE_ALIGNED(align) __attribute__((aligned(align)))
#ifndef thread_local
#if (defined HAVE_C11)
#define thread_local _Thread_local
#else
#define thread_local __thread
#endif
#endif
/*
  Really simple linked list
*/
struct cons_t {
  void *car;
  void *cdr;
};
static inline struct cons_t* make_cons(void* car, void* cdr){
  struct cons_t* c = malloc(sizeof(struct cons_t));
  c->car = car;
  c->cdr = cdr;
  return c;
}
#define XCAR(c) ((struct cons_t*)c->car)
#define XCDR(c) ((struct cons_t*)c->cdr)

#define SETCAR(c,x) ((struct cons_t*)c->car = (void*)x)
#define SETCDR(c,x) ((struct cons_t*)c->cdr = (void*)x)

#define XCAAR(c) XCAR(XCAR(c))
#define XCADR(d) XCAR(XCDR(c))
#define XCDAR(c) XCDR(XCAR(c))
#define XCDDR(d) XCDR(XCDR(c))
/* Data Structures */
//#include "svector.h"

#define get_access_mode(mode) (mode & O_ACCMODE)
/*
  Given a file access mode, of the type accepted by open return a string
  representing the access mode that can be passed to fopen.
*/
char * __attribute__((const)) filemode_bits_to_string(int mode);
off_t file_len_by_fd(int fd);
off_t file_len_by_filename(const char *filename);
off_t file_len_by_FILE(FILE *file);
int regular_filep_FILE(FILE* file);
int regular_filep_filename(const char *filename);
int regular_filep_fd(long fd);
char* read_file_to_string_fd(int fd, size_t *sz);
char* read_file_to_string_FILE(FILE *file, size_t *sz);
char* read_file_to_string_filename(const char *file, size_t *sz);
/*
  Since this just define macros the _Generic is never acutally evaluated
  so this is valid in any version of C or C++, however they can only
  be used in C11.

  TODO: wrap this in an #if (defined _ISOC11_SOURCE)
  and define alternative versions for older C versions
*/
#define generic_file_macro(base_name, x, ...)       \
  _Generic((x),                                         \
           long  : base_name##fd,                      \
           int   : base_name##fd,                      \
           FILE* : base_name##FILE,                    \
           char* : base_name##filename,                 \
           const char* : base_name##filename)(x,##__VA_ARGS__)
#define file_len(x)                             \
  generic_file_macro(file_len_by_, x)
#define regular_filep(x)                                \
  generic_file_macro(regular_filep_, x)
#define read_file_to_string(x, szptr)                                   \
  generic_file_macro(read_file_to_string_, x, szptr)



//mmap the file given by fd, return a struct contanintg a pointer
//to the maping and the length of the mapping, if shared is
//nonzero the mapping is shared, otherwise it is private
void *mmap_file(int fd, int shared, int prot, size_t *sz);
void *mmap_filename(const char *file, int shared, int prot, size_t *sz);
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
  Functions for dealing with time, functions which return a time use
  clock_gettime if available, and gettimeofday otherwise.

  There are 3 formats of time used here:
    double precision floating point numbers, in seconds
    nano seconds since the epoch, as a 64 bit integer
    struct timespec, nano seconds since the epoch with nanoseconds
      and seconds stored in seperate integers
*/
/*
  Return the current time as a floating point number of seconds
*/
double float_time();
/*
  Return the current time as nanoseconds
*/
int64_t nano_time();
/*
  return a timespec with the current time, this is mostly for convience
*/
struct timespec get_current_time();
/*
  Convert between time representations
*/
double timespec_to_float(struct timespec t);
struct timespec float_to_timespec(double t);

double nsec_to_float(time_t nsec);
int64_t float_to_nsec(double t);

struct timespec nsec_to_timespec(int64_t nsec);
int64_t timespec_to_nsec(struct timespec);

//one function to turn a stuct timeval into a supported type
struct timespec timeval_to_timespec(struct timeval tv);
/*
  convert a number of seconds in floating point format to a timespec
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
  Parse an integer in the given base, mostly the same as strtol
  but leading 0s are ignored instead of indicating octal.
  if there is an error errno is set and it is stored in err, if it's not null.
  It takes a length argument to support parsing of non-null
  terminated strings.

  This function actually does the string to integer convesrion itself,
  i.e it doesn't call strtol.
*/
long parse_integer(char *str, char **endptr,
  int len, int base, int *err);
/*
  Functionally identical to asprintf, but uses alloca to allocate
  memory on the stack instead of using malloc.
 */
#define asprintf_alloca(fmt, ...)                                       \
  __extension__ ({size_t sz = sprintf(NULL, fmt, 0, ##__VA_ARGS__);     \
      char *str = alloca(sz);                                           \
      snprintf(str, sz, fmt, ##__VA_ARGS__);                            \
      str;})

static void print_backtrace(int __attribute__((unused)) signo){
  #define BACKTRACE_BUF_SIZE 128
  void *buffer[BACKTRACE_BUF_SIZE];
  int stack_entries = backtrace(buffer, BACKTRACE_BUF_SIZE);
  //just write the backtrace straight to stderr
  backtrace_symbols_fd(buffer, stack_entries, STDERR_FILENO);
}
static void ATTRIBUTE_UNUSED enable_backtraces(void){
  struct sigaction act;
  act.sa_handler = print_backtrace;
  act.sa_flags = SA_RESETHAND;
  sigaction(SIGSEGV, &act, NULL);
  sigaction(SIGABRT, &act, NULL);
}

char* binary_int_to_string(uint64_t x, int bitdepth, char *buf);
#define format_binary_int(x, bitdepth)         \
  __extension__ ({char buf[bitdepth+1];        \
    binary_int_to_string(x, bitdepth, buf);})
#define format_binary_int_generic(x)                            \
  __extension__ ({int bitdepth = __typeof(x) * CHAR_BIT;        \
      char buf[bitdepth+1];                                     \
    binary_int_to_string(x, bitdepth, buf);})
#define IOTA(...) VFUNC(IOTA,...)
#define IOTA1(start)                            \
  iota(start, 0, 0)
#define IOTA2(start, stop)                      \
  iota(start, stop, 0);
#define IOTA3(start, stop, step)                \
  iota(start, stop, step)
int *iota(int start, int stop, int step);
#ifdef NEED_XMALLOC
#ifndef OOM_FUN
static void oom_fun(void){
  fputs("Out of memory\n",stderr);
  raise(SIGABRT);
}
#endif
#ifndef XMALLOC
#define XMALLOC
static inline void* xmalloc(size_t sz){
  void *mem = malloc(sz);
  if(!mem && sz != 0){
    oom_fun();
  }
  return mem;
}
#endif
#ifndef ZMALLOC
#define ZMALLOC
static inline void* zmalloc(size_t sz){
  void *mem = calloc(sz, 1);
  if(!mem && sz != 0){
    oom_fun();
  }
  return mem;
}
#endif
#endif /* NEED_XMALLOC */
#ifdef __cplusplus
}
#endif
#endif
