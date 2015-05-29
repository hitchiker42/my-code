#include <stdint.h> //just makes typedefs from builtin types
/*
  Compile time overloading via macros
*/
//overload w/upto 8 args, each overload can be uniquely names
#define GET_MACR0_1(_1,NAME,...) NAME
#define GET_MACRO_2(_1,_2,NAME,...) NAME
#define GET_MACRO_3(_1,_2,_3,NAME,...) NAME
#define GET_MACRO_4(_1,_2,_3,_4,NAME,...) NAME
#define GET_MACRO_5(_1,_2,_3,_4,_5,NAME,...) NAME
#define GET_MACRO_6(_1,_2,_3,_4,_5,_6,NAME,...) NAME
#define GET_MACRO_7(_1,_2,_3,_4,_5,_6,_7,NAME,...) NAME
#define GET_MACRO_8(_1,_2,_3,_4,_5,_6,_7,_8,NAME,...) NAME
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

#define _VFUNC_(name, n) name##n
#define _VFUNC(name, n) _VFUNC_(name, n)
// expands into func##n(__VA_ARGS__) where n is the number of arguments
#define VFUNC(func, ...) _VFUNC(func, __NARG__(__VA_ARGS__))(__VA_ARGS__)
