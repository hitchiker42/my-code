#ifndef _C_UTIL_H_
#define _C_UTIL_H_
#ifndef _GNU_SOURCE
#define _GNU_SOURCE
#endif
#ifdef __cplusplus
extern "C" {
#endif
#if defined(__HAVE_CONFIG_H__)
#include "config.h"
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
//This can be included standalone if you just need debuging macros
#include "debug.h"
//svectors, aka simple dynamic arrays, the header doesn't need anything from
//the rest of this file, so it can be included here.
#include "svector.h"

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

#define ARRAY_LEN(a) sizeof(a)/sizeof(a[0])
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
//this might be defined in debug.h, so only conditionally define it
#ifndef ORDINAL_SUFFIX
#define ORDINAL_SUFFIX(num)                     \
  ({char *suffix = "th";                        \
    if(num == 1){suffix = "st";}                \
    if(num == 2){suffix = "nd";}                \
    if(num == 3){suffix = "rd";};               \
    suffix;})
#endif

/*
  Some more math constants beyond those defined in math.h
*/
#define FLOAT_CONST(double_val) CAT(double_val, f)
#define M_PIf CAT(M_PI, f)
#define M_TAUf CAT(M_TAU, f)
#define M_SQRT2_2  0.5743491774985175  /* sqrt(2)/2 */
#define M_TAU 2*M_PI
#define M_SQRT3 1.7320508075688772 /* sqrt(3) */

//Bound x to the range [min,max]
#define CLAMP(x, min, max) max(min(x, max),min)
//Return 0 if x < edge, otherwise 1
#define STEP(edge, x) (x < edge ? 0 : 1)

//Control structures
//todo: figure out how to font-lock these in emacs
#define unless(cond) if(!(cond))
#define until(cond) while(!(cond))

#define DOTIMES(var, count)                     \
  for(ssize_t var = 0; var < count; var++)

/*
  This is rather complex internally, but it should `just work`.
  The outer for loop is the part that actually loops, the inner loop only gets
  executed once per iteration of the outer loop. The _once_ variable has two
  purposes, it insures that the inner loop gets executed once and only once per
  iteration, and it allows breaking out of the inner loop to break the entire loop.
  
  These are the values once has over the course of a normal loop:
  start outer loop: once = 1;
  start inner loop: once = 1; step inner loop: once = 0; inner loop terminates
  step outer loop: once = 1; check count and execute inner loop.

  and when breaking out of the inner loop:
  start outer loop: once = 1;
  start inner loop: once = 1; break;
  step outer loop: once = 0; outer loop terminates;
  

  Something like: for(int i=0;i<size;i++){__typeof(arr[0]) var = arr[i]; ...
  would work, but would require the user to put a closing brace and no opening 
  brace, which would be weird. This way FOR_EACH behaves identically to a regular
  for loop in terms of syntax.
*/
//these 2 use an explicitly named index
#define FOR_EACH_EXPLICIT(var, idx, arr, size)                          \
  for(ssize_t idx = 0, _once_ = 1;                                      \
      _once_ && idx < size; _once_ = !_once_, idx++)                    \
    for(__typeof(arr[0]) var = arr[_i_]; _once_; _once = !_once_)

//same as above
#define FOR_EACH_PTR_EXPLICIT(var, idx, arr, size)                      \
  for(ssize_t idx = 0, _once_ = 1;                                      \
      _once_ && idx < size; _once_ = !_once_, idx++)                    \
    for(__typeof(&(arr[0])) var = &(arr[_i_]); _once_; _once = !_once_)
//these use an effectively anonymous index
#define for_each(var, arr, size) FOR_EACH_EXPLICIT(var, _i_, arr, size)
#define for_each_ptr(var, arr, size) FOR_EACH_PTR_EXPLICIT(var, _i_, arr, size)

//these 4 use i/j as the index
#define FOR_EACH(var, arr, size) FOR_EACH_EXPLICIT(var, i, arr, size)
#define FOR_EACH_PTR(var, arr, size) FOR_EACH_PTR_EXPLICIT(var, i, arr, size)

#define FOR_EACH_2(var, arr, size) FOR_EACH_EXPLICIT(var, j, arr, size)
#define FOR_EACH_PTR_2(var, arr, size) FOR_EACH_PTR_EXPLICIT(var, j, arr, size)
    
    
//Given pointer to a struct element, the struct name, and the element name
//return a pointer to the struct the element is contained in 
//(from the linux kernel)
#define container_of(ptr, type, member) ({              \
const typeof( ((type *)0)->member ) *__mptr = (ptr);	\
    (type *)( (char *)__mptr - offsetof(type,member) ); })

//preprocessor tricks/macro overloading

//Macros to use in place of '#' and '##'
#define CAT(a,b) PRIMITIVE_CAT(a, b)
#define PRIMITIVE_CAT(a,b) a ## b
#define CAT3(a, b, c) PRIMITIVE_CAT3(a, b, c)
#define PRIMITIVE_CAT3(a, b, c) a ## b ##cC
#define CAT4(a, b, c, d) PRIMITIVE_CAT4(a, b, c, d)
#define PRIMITIVE_CAT4(a, b, c, d) a ## b ## c ## d
#define MACROEXPAND(...) __VA_ARGS__
#define STRINGIFY(...) #__VA_ARGS__
//Overload w/upto 8 args, each overload can be uniquely named
//Usage, for a macro foo with 3 variants, foo3, foo2 and foo1 define as
//#define foo(...) GET_MACRO(3, __VA_ARGS__, FOO3, FOO2, FOO1)(__VA_ARGS__)
//however unless there's a reason to name the variants uniquely, rather than
//above (i.e foo1, foo2, foo3), it's better to use vfunc

#define GET_MACR0_1(_1,NAME,...) MACROEXPAND(NAME)
#define GET_MACRO_2(_1,_2,NAME,...) MACROEXPAND(NAME)
#define GET_MACRO_3(_1,_2,_3,NAME,...) MACROEXPAND(NAME)
#define GET_MACRO_4(_1,_2,_3,_4,NAME,...) MACROEXPAND(NAME)
#define GET_MACRO_5(_1,_2,_3,_4,_5,NAME,...) MACROEXPAND(NAME)
#define GET_MACRO_6(_1,_2,_3,_4,_5,_6,NAME,...) MACROEXPAND(NAME)
#define GET_MACRO_7(_1,_2,_3,_4,_5,_6,_7,NAME,...) MACROEXPAND(NAME)
#define GET_MACRO_8(_1,_2,_3,_4,_5,_6,_7,_8,NAME,...) MACROEXPAND(NAME)
#define GET_MACRO(N,...) GET_MACRO_##N(__VA_ARGS__)
// overload w/upto 63 args, all overloads must be of the form
// base_n, where n is the number of args
//__narg__ in effect, computes the number of arguments passed to it
#define __NARG__(...)  __NARG_I_(__VA_ARGS__,__RSEQ_N())
#define __NARG_I_(...) __ARG_N(__VA_ARGS__)
#define __ARG_N(_1, _2, _3, _4, _5, _6, _7, _8, _9,_10,         \
                _11,_12,_13,_14,_15,_16,_17,_18,_19,_20,        \
                _21,_22,_23,_24,_25,_26,_27,_28,_29,_30,        \
                _31,_32,_33,_34,_35,_36,_37,_38,_39,_40,        \
                _41,_42,_43,_44,_45,_46,_47,_48,_49,_50,        \
                _51,_52,_53,_54,_55,_56,_57,_58,_59,_60,        \
                _61,_62,_63,n,...) n
#define __RSEQ_N() 63, 62, 61, 60, 59, 58, 57, 56, 55, 54, 53,  \
                   52, 51, 50, 49, 48, 47, 46, 45, 44, 43, 42,  \
                   41, 40, 39, 38, 37, 36, 35, 34, 33, 32, 31,  \
                   30, 29, 28, 27, 26, 25, 24, 23, 22, 21, 20,  \
                   19, 18, 17, 16, 15, 14, 13, 12, 11, 10, 9,   \
                   8, 7, 6, 5, 4, 3, 2, 1, 0
// Expands into func##n(__va_args__) where n is the number of arguments
#define VFUNC(func, ...) CAT(func, __NARG__(__VA_ARGS__))(__VA_ARGS__)

// Calls the variadic function func with the number of arguments as
// its first argument.
#define VA_FUNC(func, ...) func(__NARG__(__VA_ARGS__), __VA_ARGS__)
//I could add a preprocessor loop, if I was feeling evil
/*
  Macros for some C extensions
*/
#define ATTRIBUTE(...) __attribute__((__va_args__))
#define UNREACHABLE() __builtin_unreachable()
#define ATRIBUTE_NORETURN() __attribute__((noreturn))
#define ATTRIBUTE_NORETURN __attribute__((noreturn))
#define ATTRIBUTE_UNUSED __attribute__((unused))
#define ATTRIBUTE_ALIGNED(align) __attribute__((aligned(align)))
#define BUILTIN_EXPECT(expr, expected) __builtin_expect(expr, expected);
#define EXPR_LIKELY(expr) __builtin_expect(expr, 1)
#define EXPR_UNLIKELY(expr) __builtin_expect(expr, 0)
/*
  Always inline can be used to define fast generic versions of
  functions paramaterized by function pointers.
  Consider the code:
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
    Since __sort_generic the compiler will generate sort_u64 using
  a simple less than comparision, rather than making a function call
  to compare things. If __sort_generic isn't declared always_inline
  the compiler will only do this with O3 level optimization.

  __attribute__((always_inline)) by itself causes compiler warnings
*/
#define ALWAYS_INLINE inline __attribute__((always_inline))
//always inline functions are almost always static
#define always_inline static ALWAYS_INLINE

#ifndef thread_local
#if (defined HAVE_C11)
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
#define CONS(x,y) make_cons(x,y);
#define XCAR(c) ((struct cons_t*)c->car)
#define XCDR(c) ((struct cons_t*)c->cdr)

#define XSETCAR(c,x) ((struct cons_t*)c->car = (void*)x)
#define XSETCDR(c,x) ((struct cons_t*)c->cdr = (void*)x)

#define XCAAR(c) XCAR(XCAR(c))
#define XCADR(d) XCAR(XCDR(c))
#define XCDAR(c) XCDR(XCAR(c))
#define XCDDR(d) XCDR(XCDR(c))
/*
  Really basic fifo queue (code in c_util.c)
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
  Random numbers, etc.
  The random number generator provided here is a xorshift+ generator,
  from 'Vigna, Sebastiano (April 2014).
         "Further scramblings of Marsaglia's xorshift generators".'
  It is fast, passes most statistical tests for randomness, has
  a 128 bit period, and requires very little code.

  The mersenne twister is probably faster for large quantities of
  random numbers, and has a much longer period, but the code is
  also much much more complicated. 
*/
//you can't return an array in C so this needs to be a struct
typedef struct util_rand_state util_rand_state;
struct util_rand_state {
  uint64_t state[2];
};
//returns 64bit integers unifornly distributed between 0-2^64-1
uint64_t util_rand(void);
uint64_t util_rand_r(util_rand_state *state);
//double precison numbers in the range [0,1), with 53 bit precision
double util_drand(void);
double util_drand_r(util_rand_state *state);
//Return a random signed integer in the range min-max. This is actually
//a bit more complicated than it initally seems, as using mod results
//in an uneven distribution
int64_t util_rand_range_r(int64_t min, int64_t max, util_rand_state *state);
int64_t util_rand_range(int64_t min, int64_t max);
//returns the current random state, this is a copy, so changing it
//won't modify the internal state
util_rand_state util_get_rand_state(void);
//sets the internal random state to state and returns the old value
util_rand_state util_set_rand_state(util_rand_state *state);
//automatically generates a radnom state using a known form of randomness,
//usually either /dev/urandom or the current time.
util_rand_state util_auto_rand_state(void);
//initializes the random state
void util_srand(uint64_t a, uint64_t b);
void util_srand_auto(void);//calls util_auto_rand_state to get the state

//macros used to set how util_auto_rand_state works
#ifndef USE_SEED_URANDOM
#define USE_SEED_URANDOM 0
#endif
#ifndef USE_SEED_TIME
#define USE_SEED_TIME 1
#endif

void shuffle_array(void **arr, size_t len);
/*
  Basic hash function, never know when you need a hash function.
*/
#define fnv_prime_64 1099511628211UL
#define fnv_offset_basis_64 14695981039346656037UL
static uint64_t fnv_hash(const void *key, size_t keylen){
  const uint8_t *raw_data=(const uint8_t *)key;
  size_t i;
  uint64_t hash=fnv_offset_basis_64;
  for(i=0; i < keylen; i++){
    hash = (hash ^ raw_data[i])*fnv_prime_64;
  }
  return hash;
}
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
#ifndef XREALLOC
#define XREALLOC
static inline void* xrealloc(void *ptr, size_t sz){
  void *mem = xrealloc(ptr, sz);
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
