#ifndef __UTIL_H__
#define __UTIL_H__
#ifdef __cplusplus
extern "C" {
#endif
/*
  Standalone header providing a subset of the features of the
  whole utility library.
*/
//check for C11
#if defined(__STDC_VERSION__) && (__STDC_VERSION__ >= 201112L)  &&      \
  !(defined(HAVE_C11)) /* C11 */
#define HAVE_C11
#endif
//Include pretty much all the system headers
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

/* typedefs */
typedef unsigned int uint;
typedef unsigned long ulong;

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
#define NEXT_POW_OF_2(num)                                      \
  (1UL << ((sizeof(num)*CHAR_BIT) - __builtin_clzl(num)))
#define LOG_2(num)                                      \
  ((sizeof(num)*CHAR_BIT) - (__builtin_clzl(num)+1))
#define LOG_2_FLOOR(num) LOG_2(num)
#define LOG_2_CEIL(num)                                                 \
  ((sizeof(num)*CHAR_BIT) - (__builtin_clzl(num)+IS_POW_OF_2(num)))
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
  Also the standard *(type*)&val idiom is technically undefined behavior,
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

#define ORDINAL_SUFFIX(num)                     \
  ({char *suffix = "th";                        \
    if(num == 1){suffix = "st";}                \
    if(num == 2){suffix = "nd";}                \
    if(num == 3){suffix = "rd";};               \
    suffix;})

/*
  Some more math constants beyond those defined in math.h, 
  as well as single precision versions of some existing constants
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
#define container_of(ptr, type, member) ({                      \
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
    52, 51, 50, 49, 48, 47, 46, 45, 44, 43, 42,                 \
    41, 40, 39, 38, 37, 36, 35, 34, 33, 32, 31,                 \
    30, 29, 28, 27, 26, 25, 24, 23, 22, 21, 20,                 \
    19, 18, 17, 16, 15, 14, 13, 12, 11, 10, 9,                  \
    8, 7, 6, 5, 4, 3, 2, 1, 0
// Expands into func##n(__va_args__) where n is the number of arguments
#define VFUNC(func, ...) CAT(func, __NARG__(__VA_ARGS__))(__VA_ARGS__)

// Calls the variadic function func with the number of arguments as
// its first argument.
#define VA_FUNC(func, ...) func(__NARG__(__VA_ARGS__), __VA_ARGS__)
/*
  Map a macro over a set of args, how this works is rather complicated, but it
  does work.
*/
#define EVAL0(...) __VA_ARGS__
#define EVAL1(...) EVAL0 (EVAL0 (EVAL0 (__VA_ARGS__)))
#define EVAL2(...) EVAL1 (EVAL1 (EVAL1 (__VA_ARGS__)))
#define EVAL3(...) EVAL2 (EVAL2 (EVAL2 (__VA_ARGS__)))
#define EVAL4(...) EVAL3 (EVAL3 (EVAL3 (__VA_ARGS__)))
#define EVAL(...)  EVAL4 (EVAL4 (EVAL4 (__VA_ARGS__)))

#define MAP_END(...)
#define MAP_OUT

#define MAP_GET_END() 0, MAP_END
#define MAP_NEXT0(test, next, ...) next MAP_OUT
#define MAP_NEXT1(test, next) MAP_NEXT0 (test, next, 0)
#define MAP_NEXT(test, next)  MAP_NEXT1 (MAP_GET_END test, next)

#define MAP0(f, x, peek, ...) f(x) MAP_NEXT (peek, MAP1) (f, peek, __VA_ARGS__)
#define MAP1(f, x, peek, ...) f(x) MAP_NEXT (peek, MAP0) (f, peek, __VA_ARGS__)
#define MAP(f, ...) EVAL (MAP1 (f, __VA_ARGS__, (), 0))
/*
  This does what X macros do, but without the user having to define anything.
*/
#define STRINGIFY_W_COMMA(arg) #arg,
/*
  Generate an enum, an array containing the enum values as strings and a function
  to convert an enum value to a string.
*/
#define GEN_ENUM(name, ...)                                             \
  enum name {__VA_ARGS__};                                              \
  const char *CAT(name, _strings)[] = {MAP(STRINGIFY_W_COMMA, __VA_ARGS__) ""}; \
  const char *CAT(name, _to_string)(enum name x){return CAT(name, _strings[x]);}
/*
  Same as above but allows the user to set the value of the first enum member.
*/
#define GEN_ENUM_OFFSET(name, offset, x, ...)                           \
  enum name {x = offset, __VA_ARGS__};                                  \
  const char *CAT(name, _strings)[] = {STRINGIFY(x),                    \
                                       MAP(STRINGIFY_W_COMMA, __VA_ARGS__) ""}; \
  const char *CAT(name, _to_string)(enum name x){return CAT(name, _strings[x]);}

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

//a few debugging macros
#if !(defined NDEBUG)
#define DEBUG_PRINTF(fmt,...)                   \
  fprintf(stderr,fmt,##__VA_ARGS__)
#define HERE_FMT(fmt,...)                                       \
  fprintf(stderr,"file: %s, line: %d, function: %s\n" fmt,      \
          __FILE__,__LINE__,__func__,##__VA_ARGS__)
#define HERE() HERE_FMT("")
#define HERE_STR(str) HERE_FMT("%s",str)
#define BREAKPOINT() raise(SIGTRAP)
#else
#define DEBUG_PRINTF(fmt,...)
#define HERE_FMT(fmt,...)
#define HERE()
#define HERE_STR(str)
#define BREAKPOINT()
#endif
/*
  Malloc wrappers, which call a given function if out of memory, which
  by default prints an error message and aborts.

  xmalloc wraps malloc, and zmalloc wraps calloc.
*/
static void oom_fun(void){
  fputs("Out of memory\n",stderr);
  raise(SIGABRT);
}
static inline void* xmalloc(size_t sz){
  void *mem = malloc(sz);
  if(!mem && sz != 0){
    oom_fun();
  }
  return mem;
}
static inline void* xrealloc(void *ptr, size_t sz){
  void *mem = realloc(ptr, sz);
  if(!mem && sz != 0){
    oom_fun();
  }
  return mem;
}
static inline void* zmalloc(size_t sz){
  void *mem = calloc(sz, 1);
  if(!mem && sz != 0){
    oom_fun();
  }
  return mem;
}
/*
  Dynamically resizable array, functions are avaiable in lower and uppercase 
  forms, lowercase take pointers and are generally functions, uppercase take
  literal values and are always macros.
*/
typedef struct svector svector;
struct svector {
  union {
    void **data;    
    uint8_t *bytes;
  };
  //len and size are implicily in terms of either bytes or void*'s depending on
  //the function used, so you need to be careful if mixing byte functions and
  //regular functions.
  int len;
  int size;
};
/*
  Ensure svector vec can hold sz more elements.
  Used to go into an infinite loop for a svector with size 0, but I fixed that

  This doesn't check the return value of realloc.
*/
#define svector_check_size_typed(svec, sz, type)                        \
  ({struct svector *tmp = svec;                                         \
    int needed = tmp->len + sz;                                         \
    while(tmp->size <= needed){                                         \
      tmp->size = (tmp->size >= 1 ? tmp->size*2 : 2);                   \
      tmp->data = realloc(tmp->data, tmp->size*sizeof(type));           \
    };})
#define svector_check_size(svec, sz) svector_check_size_typed(svec, sz, void*)
#define svector_check_size_bytes(svec, sz)      \
  svector_check_size_typed(svec, sz, uint8_t)
/*
  Functions come in two versions, lowercase are functions and take pointers to
  svectors, uppercase are macros and take literal svectors. Functions can't take
  literal svectors since they often need to be modified.
*/
static struct svector make_svector(int sz){
  svector ret;
  ret.len = 0;
  ret.size = sz;
  ret.bytes = xmalloc(sz);
  return ret;
}
#define svector_data(x) (x->data)
#define SVECTOR_DATA(x) (x.data)

#define svector_bytes(x) (x->bytes)
#define SVECTOR_BYTES(x) (x.bytes)

#define svector_len(x) (x->len)
#define SVECTOR_LEN(x) (x.len)

#define SVECTOR_POP(vec)                        \
  (vec.data[--vec.len])
#define SVECTOR_POP_BYTE(vec)                   \
  (vec.bytes[--vec.len])
static inline void* svector_pop(struct svector *vec){
  return vec->data[--vec->len];
}
static inline uint8_t svector_pop_byte(struct svector *vec){
  return vec->bytes[--vec->len];
}

#define SVECTOR_PUSH(elt, vec)                  \
  svector_check_size(&vec, 1);                  \
  vec.data[vec.len++] = elt
#define SVECTOR_PUSH_BYTE(elt, vec)             \
  svector_check_size_bytes(&vec, 1);   \
  vec.data[vec.len++] = elt
static inline void svector_push(void *elt, struct svector* vec){
  svector_check_size(vec,1);
  vec->data[vec->len++] = elt;
}
static inline void svector_push_byte(uint8_t elt, struct svector* vec){
  svector_check_size_bytes(vec,1);
  vec->bytes[vec->len++] = elt;
}

#define SVECTOR_MULTIPUSH(vec, elts, nelts)                 \
  svector_check_size(&vec, nelts);                              \
  memcpy(vec.data + vec.len, elts, nelts*sizeof(void*));        \
  vec.len += nelts
#define SVECTOR_MULTIPUSH_BYTES(vec, elts, nelts)           \
  svector_check_size_bytes(&vec, nelts);                        \
  memcpy(vec.data + vec.len, elts, nelts*sizeof(uint8_t));      \
  vec.len += nelts
static inline void svector_multipush(svector *vec, void **elts, size_t len){
  svector_check_size(vec, len);
  memcpy(vec->data + vec->len, elts, len*sizeof(void*));
  vec->len += len;
}
static inline void svector_multipush_bytes(svector *vec,
                                           uint8_t *elts, size_t len){
  svector_check_size_bytes(vec, len);
  memcpy(vec->data + vec->len, elts, len*sizeof(uint8_t));
  vec->len += len;
}
  
//is an lvalue
#define SVECTOR_REF(vec, idx)                   \
  (vec.data[idx])
#define SVECTOR_REF_BYTE(vec, idx)                   \
  (vec.bytes[idx])
static inline void* svector_ref(const struct svector *vec, int idx){
  return vec->data[idx];
}
static inline uint8_t svector_ref_byte(const struct svector *vec, int idx){
  return vec->bytes[idx];
}

#define SVECTOR_SET(vec, idx, elt)              \
  (vec.data[idx] = elt)
#define SVECTOR_SET_BYTE(vec, idx, elt)              \
  (vec.bytes[idx] = elt)
static inline void svector_set(struct svector *vec, int idx, void *elt){
  vec->data[idx] = elt;
}
static inline void svector_set_byte(struct svector *vec, int idx, uint8_t elt){
  vec->bytes[idx] = elt;
}

#define SVECTOR_SWAP(vec, i, j)                 \
  __extension__                                 \
  ({__typeof(vec.data[i]) __temp = vec.data[i]; \
    vec.data[i] = vec.data[j];                  \
    vec.data[j] = __temp;})
#define SVECTOR_SWAP_BYTES(vec, i, j)           \
  __extension__                                 \
  ({__typeof(vec.bytes[i]) __temp = vec.bytes[i]; \
    vec.bytes[i] = vec.bytes[j];                  \
    vec.bytes[j] = __temp;})
static inline void svector_swap(struct svector *vec, int i, int j){
  void *temp = vec->data[i];
  vec->data[i] = vec->data[j];
  vec->data[j] = temp;
}
static inline void svector_swap_bytes(struct svector *vec, int i, int j){
  uint8_t temp = vec->bytes[i];
  vec->bytes[i] = vec->bytes[j];
  vec->bytes[j] = temp;
}
/*
  Time functions, these are useful often enough to include.
*/
static inline double timespec_to_float(struct timespec t){
  return t.tv_sec + (t.tv_nsec / 1e9);
}
static inline double nsec_to_float(int64_t t){
  //nanoseconds to fractions of a second
  double fsec = (t % 1000000000)/1e9;
  int64_t sec = t/1000000000;//integer number of seconds
  return (sec + fsec);
}
static inline int64_t timespec_to_nsec(struct timespec t){
  return t.tv_nsec + t.tv_sec * 1000000000;
}
static inline struct timespec get_current_time(){
  struct timespec ts;
  //OSX doesn't have this funciton
#if (defined _POSIX_TIMERS) && (_POSIX_TIMERS > 0)
  clock_gettime(CLOCK_REALTIME, &ts);
#else
  struct timeval tv;
  gettimeofday(&tv, NULL);
  ts =  timeval_to_timespec(tv);
#endif
  return ts;
}
static inline double float_time(){
  return timespec_to_float(get_current_time());
}
static inline int64_t nano_time(){
  return timespec_to_nsec(get_current_time());
}
static inline double float_sleep(double sleep_time){
  struct timespec ts;
  if(sleep_time < 0){//we can't turn back time...
    return 0;
  }
  ts.tv_sec = sleep_time;
  ts.tv_nsec = (sleep_time - (double)ts.tv_sec) * 1e9;
  int err = nanosleep(&ts, &ts);
  if(err == -1){
    //if we passed an invalid time it's an issue with this function
    assert(errno != EINVAL);
    return timespec_to_float(ts);
  }
  return 0;
}
#ifdef __cplusplus
}
#endif
#endif /* __UTIL_H__ */
