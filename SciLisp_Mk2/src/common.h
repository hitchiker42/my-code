#ifndef _SL_COMMON_H_
#define _SL_COMMON_H_
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <pthread.h>
#include <unistd.h>
#include <string.h>
#include <stdarg.h>
#define SL_INLINE static inline
#define SL_LIKELY(test) __builtin_expect(test, 1)
#define SL_UNLIKELY(test,expr) __builtin_expect(test, 0)
#define SL_EXPECT(test,expect) __builtin_expect(test, expect)
#define SL_UNREACHABLE __builtin_unreachable()
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
//need include types.h after the above macro definations
#include "types.h"
#endif
