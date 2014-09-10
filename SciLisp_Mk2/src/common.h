#ifndef _SL_COMMON_H_
#define _SL_COMMON_H_
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <pthread.h>
#include <unistd.h>
#include <string.h>
#include <stdarg.h>
#include <signal.h>
#define SL_INLINE static inline
#define SL_LIKELY(test) __builtin_expect(test, 1)
#define SL_UNLIKELY(test,expr) __builtin_expect(test, 0)
#define SL_EXPECT(test,expect) __builtin_expect(test, expect)
#define SL_UNREACHABLE __builtin_unreachable()
#define SL_NORETURN __attribute__((noreturn))
#define SL_UNUSED __attribute__((unused))
//Allocation Macros
#ifndef SL_MALLOC
#define SL_MALLOC malloc
#endif
#ifndef SL_FREE
#define SL_FREE free
#endif
#ifndef SL_REALLOC
#define SL_REALLOC realloc
#endif
#define sl_malloc SL_MALLOC
#define sl_free SL_FREE
#define sl_realloc SL_REALLOC
#define sl_strdup strdup
//Any definations required by types.h/other headers should go
//above here.
#include "types.h"
//and any definations that need types.h/other headers shoud go
//below here
//make writing -9-9 faster, since I'll be writing them a lot
#define sl_0 (make_int(0))
#define sl_1 (make_int(1))
#define sl_2 (make_int(2))
#define sl_3 (make_int(3))
#define sl_4 (make_int(4))
#define sl_5 (make_int(5))
#define sl_6 (make_int(6))
#define sl_7 (make_int(7))
#define sl_8 (make_int(8))
#define sl_9 (make_int(9))
#define sl_m1 (make_int(-1))
#define sl_m2 (make_int(-2))
#define sl_m3 (make_int(-3))
#define sl_m4 (make_int(-4))
#define sl_m5 (make_int(-5))
#define sl_m6 (make_int(-6))
#define sl_m7 (make_int(-7))
#define sl_m8 (make_int(-8))
#define sl_m9 (make_int(-9))

//temporary defination untill I get a better one
//the prototype should stay the same however
void raise_error(sl_obj tag, sl_obj data){
  raise(SIGUSR1);
}
    
#endif
