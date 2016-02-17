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
  Header for C utility library that includes various features
  provided by the standary library of most higher level programing
  languages (i.e hash tables, dynamic arrays, sorting, file abstraction).
*/
#include <assert.h>
#include <ctype.h>
#include <errno.h>
#include <fcntl.h>
#include <limits.h>
#include <math.h>
#include <signal.h>
#include <stdarg.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <sys/time.h>
#include <time.h>
#include <unistd.h>

#if !(defined PAGE_SIZE)
#if (defined PAGESIZE)
#define PAGE_SIZE PAGESIZE
#else
#define PAGE_SIZE 4096
#endif
#endif

//Conditional definitions of debugging macros/enabling backtraces
#include "debug.h"

/* typedefs */
typedef unsigned int uint;
typedef unsigned long ulong;
/*
  Type of comparision functions.
  For sorting functions should return true if the arguments
  are correctly ordered, and false if they are not.
  For data structures should return -1, 0 or 1 if the first argument is
  less than, equal to, or greater than the second argument, respectively.
*/
typedef int(*cmp_fun)(void*,void*);
/*
  Types of sorting functions.
*/
typedef void(*sort_fn)(void**, size_t, cmp_fun);
typedef void(*int_sort_fn)(uint64_t*, size_t);
/* Macros, no macro evaluates its arguments more than once*/
#define SWAP(x,y)                               \
  __extension__                                 \
  ({__typeof(x) __temp = x;                     \
    x = y;                                      \
    y = __temp;})
#define ARR_SWAP(arr,i,j)                       \
  __extension__                                 \
  ({__typeof(arr[i]) __temp = arr[i];           \
    arr[i] = arr[j];                            \
    arr[j] = __temp;})
#define MIN(_x,_y)                              \
  __extension__                                 \
  ({__typeof(_x) x = _x;                        \
    __typeof(_y) y = _y;                        \
    x<y ? x : y;})
#define MAX(_x,_y)                              \
  __extension__                                 \
  ({__typeof(_x) x = _x;                        \
    __typeof(_y) y = _y;                        \
    x>y ? x : y;})

#define TO_BOOLEAN(val) (val ? 1 : 0)
#define IS_POW_OF_2(num) (!(num & (num-1)))
#define NEXT_POW_OF_2(num)                              \
    (1UL << ((sizeof(num)*CHAR_BIT) - __builtin_clzl(num)))
#define LOG_2(num)                                      \
    ((sizeof(num)*CHAR_BIT) - (__builtin_clzl(num)+1))
//The nearest integer greater than x/y, where x and y are integers
#define IDIV_CEIL(x,y)                          \
  __extension__                                 \
  ({__typeof(x) quot, rem, _x = x;              \
    __typeof(y) _y = y;                         \
    quot = x/y; rem = x % y;                    \
    (quot + (rem != 0));})

//these 3 macros should work the same on floating point numbers
#define SIGN(x) ((x) < 0)
#define NEG(x) (-(x))
#define ABS(x) ((x) < 0 ? -(x) : (x))
//these 3 macros act on the underlying bit patterns
//with some work SIGNBIT could work on floats
//signed shifts are techincally undefined, but who cares
#define SIGNBIT(x)                              \
  __extension__                                 \
  ({__typeof(x) tmp = x;                        \
    int shift = (sizeof(x) * CHAR_BIT)-1;       \
    ((x) & (1 << (shift - 1)))>>shift;})
//test the sign on an n bit integer
#define SIGNBIT_N(x,n)                          \
  __extension__                                 \
  ({int bit = (1 << (n-1));                     \
    (x & bit)>>(n-1);})
#define BITNEG(x) (~(x)+1)
//fails for the most negitive integer (i.e -128 for an 8 bit int)
#define BITABS(x) (SIGNBIT(x) ? BITNEG(x) : x)

#define get_byte(val, byte)                     \
  (((val) >> ((byte)*CHAR_BIT)) & 0xffu)
#define GET_BIT(val, bit)                       \
  (((val) & (1ul << (bit))) ? 1 : 0)
#define SET_BIT(val, bit)                       \
  ((val) |= (1ul << (bit)))
#define UNSET_BIT(val, bit)                     \
  ((val) &= ~(1ul << (bit)))
#define FLIP_BIT(val, bit)                      \
  ((val) ^= (1ul << (bit)))
/*
  You need to use a union to do this to avoid breaking strict aliasing.
  Also the standard *(type*)&val idom is technically undefined behavior,
  though pretty much every C compiler will do what you want.
*/
#define BITCAST(val, type)                      \
  __extension__                                 \
  ({union {__typeof(val) old;                   \
      type new;} tmp;                           \
    tmp.old = val;                              \
    tmp.new;})
/*
  DOWN/UPCASE_ASCII work on any ascii values, but only do anything on
  values corresponding to alphabetic characters;
*/
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
#define M_SQRT2_2  0.5743491774985175  /* sqrt(2)/2 */
#define M_TAU 2*M_PI
#define M_SQRT3 1.7320508075688772 /* sqrt(3) */

//bound x to the range [min,max]
#define clamp(x, min, max) max(min(x, max),min)
//return 0 if x < edge, otherwise 1
#define step(edge, x) (x < edge ? 0 : 1)

//control structures
//todo: figure out how to font-lock these in emacs
#define unless(cond) if(!(cond))
#define until(cond) while(!(cond))

//preprocessor tricks/macro overloading

//macros to use in place of '#' and '##'
#define cat(a,b) primitive_cat(a, b)
#define primitive_cat(a,b) a ## b
#define cat3(a,b,c) primitive_cat3(a, b, c)
#define primitive_cat3(a,b,c) a ## b ## c
#define cat4(a,b,c,d) primitive_cat4(a, b, c, d)
#define primitive_cat4(a,b,c,d) a ## b ## c ## d
#define macroexpand(...) __va_args__
#define stringify(...) #__va_args__
//overload w/upto 8 args, each overload can be uniquely named
//usage, for a macro foo with 3 variants, foo3, foo2 and foo1 define as
//#define foo(...) get_macro(3, __va_args__, foo3, foo2, foo1)(__va_args__)
//however unless there's a reason to name the variants uniquely, rather than
//above (i.e foo1, foo2, foo3), it's better to use vfunc

#define get_macr0_1(_1,name,...) macroexpand(name)
#define get_macro_2(_1,_2,name,...) macroexpand(name)
#define get_macro_3(_1,_2,_3,name,...) macroexpand(name)
#define get_macro_4(_1,_2,_3,_4,name,...) macroexpand(name)
#define get_macro_5(_1,_2,_3,_4,_5,name,...) macroexpand(name)
#define get_macro_6(_1,_2,_3,_4,_5,_6,name,...) macroexpand(name)
#define get_macro_7(_1,_2,_3,_4,_5,_6,_7,name,...) macroexpand(name)
#define get_macro_8(_1,_2,_3,_4,_5,_6,_7,_8,name,...) macroexpand(name)
#define get_macro(n,...) get_macro_##n(__va_args__)
// overload w/upto 63 args, all overloads must be of the form
// base_n, where n is the number of args
//__narg__ in effect, computes the number of arguments passed to it
#define __narg__(...)  __narg_i_(__va_args__,__rseq_n())
#define __narg_i_(...) __arg_n(__va_args__)
#define __arg_n(_1, _2, _3, _4, _5, _6, _7, _8, _9,_10,         \
                _11,_12,_13,_14,_15,_16,_17,_18,_19,_20,        \
                _21,_22,_23,_24,_25,_26,_27,_28,_29,_30,        \
                _31,_32,_33,_34,_35,_36,_37,_38,_39,_40,        \
                _41,_42,_43,_44,_45,_46,_47,_48,_49,_50,        \
                _51,_52,_53,_54,_55,_56,_57,_58,_59,_60,        \
                _61,_62,_63,n,...) n
#define __rseq_n() 63, 62, 61, 60, 59, 58, 57, 56, 55, 54, 53,  \
                   52, 51, 50, 49, 48, 47, 46, 45, 44, 43, 42,  \
                   41, 40, 39, 38, 37, 36, 35, 34, 33, 32, 31,  \
                   30, 29, 28, 27, 26, 25, 24, 23, 22, 21, 20,  \
                   19, 18, 17, 16, 15, 14, 13, 12, 11, 10, 9,   \
                   8, 7, 6, 5, 4, 3, 2, 1, 0
// expands into func##n(__va_args__) where n is the number of arguments
#define vfunc(func, ...) cat(func, __narg__(__va_args__))(__va_args__)

// calls the variadic function func with the number of arguments as
// its first argument.
#define va_func(func, ...) func(__narg__(__va_args__), __va_args__)
//i could add a preprocessor loop, if i was feeling evil
/*
  macros for some c extensions
*/
#define attribute(...) __attribute__((__va_args__))
#define unreachable() __builtin_unreachable()
#define atribute_noreturn() __attribute__((noreturn))
#define attribute_noreturn __attribute__((noreturn))
#define attribute_unused __attribute__((unused))
#define attribute_aligned(align) __attribute__((aligned(align)))
/*
  always inline can be used to define fast generic versions of
  functions paramaterized by function pointers.
  consider the code:
    always_inline __sort_generic(void **arr, size_t len, cmp_fun cmp){
      ...
    }
    sort_generic(void **arr, size_t len, cmp_fun cmp){
      __sort_generic(arr,len,cmp);
    }
    int cmp_lt(void*x,void*y){return x < y;}
    sort_u64(uint64_t *arr, size_t len){
      __sort_generic((void**)arr, len, cmp_lt)
    }
  since __sort_generic the compiler will generate sort_u64 using
  a simple less than comparision, rather than making a function call
  to compare things. if __sort_generic isn't declared always_inline
  the compiler will only do this with o3 level optimization.

  __attribute__((always_inline)) by itself causes compiler warnings
*/
#define always_inline inline __attribute__((always_inline))
//always inline functions are almost always static
#define always_inline static always_inline

#ifndef thread_local
#if (defined have_c11)
#define thread_local _thread_local
#else
#define thread_local __thread
#endif
#endif
/* data structures */
/*
  really simple linked list
*/
typedef struct cons_t cons_t;
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
#define cons(x,y) make_cons(x,y);
#define xcar(c) ((struct cons_t*)c->car)
#define xcdr(c) ((struct cons_t*)c->cdr)

#define xsetcar(c,x) ((struct cons_t*)c->car = (void*)x)
#define xsetcdr(c,x) ((struct cons_t*)c->cdr = (void*)x)

#define xcaar(c) xcar(xcar(c))
#define xcadr(d) xcar(xcdr(c))
#define xcdar(c) xcdr(xcar(c))
#define xcddr(d) xcdr(xcdr(c))
/*
  really basic fifo queue (code in c_util.c)
*/
struct queue {
  struct queue_node *head;
  struct queue_node *tail;
};
struct queue* make_queue(void);
void queue_push(struct queue *q, void *data);
//Queue pop returns NULL if q is empty, so if you need to store NULL in
//the queue use queue_is_empty to check if the queue is empty.
void *queue_pop(struct queue *q);
int queue_is_empty(struct queue *q);

//binary heap (code in heap.c)
typedef struct heap binary_heap;
struct heap {
  void **heap;
  int (*cmp)(void*,void*);
  int len;
  int mem;
};
binary_heap *make_new_heap(void *arr, int len, cmp_fun cmp);
binary_heap *heap_sort(binary_heap *heap);
void* heap_pop(binary_heap *heap);
void heap_add(binary_heap *heap, void *new_element);
void destroy_heap(binary_heap *heap);
//these are static in heap.c
//void heapify(binary_heap *heap);
//void sift_up(binary_heap *heap, int index);
//void sift_down(binary_heap *heap, long root, long end);

/*
  Further data structures, with their own headers:
  svector.h: Simple dynamic array/vector
  rbtree.h: red/black tree
  hash.h: (optionally threadsafe) hashtable
*/

/*
  Filesystem abstractions (admittedly not very complex abstractions) 
*/

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
  This emulates the file options of the 'test' command (or the -X perl fxns).
  It's really just a wrapper for stat, but it's eaiser to use.
*/
int filetest_filename(const char *filename, char test);
int filetest_fd(int fd, char test);
int filetest_FILE(FILE *file, char test);
/*
  Since this just define macros the _Generic is never acutally evaluated
  so this is valid in any version of C or C++, however they can only
  be used in C11.

  TODO: wrap this in an #if (defined _ISOC11_SOURCE)
  and define alternative versions for older C versions
*/
#define generic_file_macro(base_name, x, ...)       \
  _Generic((x),                                         \
           long  : CAT(base_name, fd),                      \
           int   : CAT(base_name, fd),                           \
           FILE* : CAT(base_name, FILE),                    \
           char* : CAT(base_name, filename),                 \
           const char* : CAT(base_name, filename))(x,##__VA_ARGS__)
#define file_len(x)                             \
  generic_file_macro(file_len_by_, x)
#define regular_filep(x)                                \
  generic_file_macro(regular_filep_, x)
#define read_file_to_string(x, szptr)                                   \
  generic_file_macro(read_file_to_string_, x, szptr)
#define filetest(x, test)                       \
  generic_file_macro(filetest_, x, test)

/* 
   TODO: streams (adapt from scilisp code)
*/

/*
  Wrappers for mmap
*/
//mmap the file given by fd, return a pointer to the maping and
//if sz is not NULL store the size of the mapping in it.
//You almost alawys want the size (you need it to unmap the file)
//if shared is nonzero the mapping is shared, otherwise it is private
void *mmap_file(int fd, int shared, int prot, size_t *sz);
void *mmap_filename(const char *file, int shared, int prot, size_t *sz);
/*
  mmap an anonymous region of memory size bytes long and return a pointer to it
*/
void* mmap_anon(size_t size);
/*
  Byte array versions of string functions
*/
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

//same as strdup/strdupa
void *memdup(const void *src, size_t sz);
#define memdupa(src, sz)                        \
  __extension__                                 \
  ({const uint8_t *dest = alloca(sz);           \
    memcpy(dest, src, sz);                      \
    dest;})

/*
  Functions for dealing with time, functions which return a time use
  clock_gettime if available, and gettimeofday otherwise.

  There are 3 formats of time used here:
    double precision floating point numbers, in seconds
    nanoseconds since the epoch, as a 64 bit integer
    struct timespec, nanoseconds since the epoch with nanoseconds
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
  sleep for the numbor of seconds indicated by sleep time.
  float_sleep will return the ammount of time remaning if interupted
  float_sleep_full will always sleep for at least sleep_time
    seconds even if interupted.
*/
double float_sleep(double sleep_time);
void float_sleep_full(double sleep_time);

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
  Print a 'bitdepth' bit integer 'x' in binary to buf.
*/
char* binary_int_to_string(uint64_t x, int bitdepth, char *buf);
/*
  Create an array of ints starting at 'start' incrementing by 'step'
  and stopping at 'stop'.
  If one argument is provided it is taken as stop, start is set to 0,
    and step is set to 1.
  If two arguments are provide step is set to 1.
*/
int *iota(int start, int stop, int step);
#define IOTA(...) VFUNC(IOTA,...)
#define IOTA1(start)                            \
  iota(start, 0, 0)
#define IOTA2(start, stop)                      \
  iota(start, stop, 0);
#define IOTA3(start, stop, step)                \
  iota(start, stop, step)
/*
  Sorting functions
*/
/*
  Functions for sorting arrays of generic data.
*/
void insertion_sort_generic(void **input, size_t len, cmp_fun cmp);
//Qsort uses insertion sort once arrays get down to a certain size
//by default this is 4.
void qsort_generic(void **arr, size_t len, cmp_fun cmp);
void mergesort_generic(void **arr, size_t len, cmp_fun cmp);
/*
  Radix sort only works on integers, so these functions take a function
  pointer which should get an integer key from each input value, the
  input is sorted based on these integers.
*/
void radix_sort_u8_keys(void **in, size_t sz, uint64_t(*get_key)(void*));
void radix_sort_u16_keys(void **in, size_t sz, uint64_t(*get_key)(void*));
/*
  Functions for sorting unsigned 64 bit integers
 */
/*
  Currently there are only 2 types of radix sort, but I'll add more.
  They are named radix_sort_'hist_size'_'type_to_sort'.
*/
void radix_sort_u8_u64(uint64_t *in, size_t sz);
void radix_sort_u16_u64(uint64_t *in, size_t sz);
//alias of one of the two previous functions
void radix_sort_u64(uint64_t *in, size_t sz);
void mergesort_u64(uint64_t *input, size_t len);
void qsort_u64(uint64_t *input, size_t len);
void insertion_sort_u64(uint64_t *input, size_t len);
void heapsort_u64(uint64_t *input, size_t len);
/*
  Functionally identical to asprintf, but uses alloca to allocate
  memory on the stack instead of using malloc.
 */
#define asprintf_alloca(fmt, ...)                                       \
  __extension__ ({size_t sz = snprintf(NULL, fmt, 0, ##__VA_ARGS__);    \
      char *str = alloca(sz);                                           \
      snprintf(str, sz, fmt, ##__VA_ARGS__);                            \
      str;})
/*
  Malloc wrappers, which call a given function if out of memory, which
  by default prints an error message and aborts.

  xmalloc wraps malloc, and zmalloc wraps calloc.
*/
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
#ifdef __cplusplus
}
#endif
#endif
