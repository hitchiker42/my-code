/*
  This file implements the C11 stdatomic.h header, using gcc builtins,
  and replacing _Atomic with volatile for versions of C prior to C11
*/
#ifndef __ATOMIC_H__
#define __ATOMIC_H__
#if defined(__STDC_VERSION__) && (__STDC_VERSION__ >= 201112L)
#define HAVE_C11
#else //not C11
#define _Atomic volatile
#endif
#include <stdint.h>
//for some reason stdatomic only provides atomic_int_{leastN,fastN}_t
typedef _Atomic int8_t atomic_int8_t;
typedef _Atomic uint8_t atomic_uint8_t;
typedef _Atomic int16_t atomic_int16_t;
typedef _Atomic uint16_t atomic_uint16_t;
typedef _Atomic int32_t atomic_int32_t;
typedef _Atomic uint32_t atomic_uint32_t;
typedef _Atomic int64_t atomic_int64_t;
typedef _Atomic uint64_t atomic_uint64_t;
#ifdef HAVE_C11
#include <stdatomic.h>
#define ATOMIC_ORDER_RW __ATOMIC_SEQ_CST
#define ATOMIC_ORDER_RD __ATOMIC_ACQUIRE
#define ATOMIC_ORDER_WR __ATOMIC_RELEASE
#define atomic_compare_exchange(ptr, expected, val)     \
  atomic_compare_exchange_strong(ptr, expected, val)
#define atomic_compare_exchange_explicit(ptr, expected, val, suc, fail)      \
  atomic_compare_exchange_strong(ptr, expected, val, suc, fail
#else /*!C11*/
/*
  Provide a version of C11 atomics using gcc builtins
*/
typedef enum {
  memory_order_relaxed = __ATOMIC_RELAXED,
  memory_order_consume = __ATOMIC_CONSUME,
  memory_order_acquire = __ATOMIC_ACQUIRE,
  memory_order_release = __ATOMIC_RELEASE,
  memory_order_acq_rel = __ATOMIC_ACQ_REL,
  memory_order_seq_cst = __ATOMIC_SEQ_CST
} memory_order;
#define ATOMIC_ORDER_RW __ATOMIC_SEQ_CST
#define ATOMIC_ORDER_RD __ATOMIC_ACQUIRE
#define ATOMIC_ORDER_WR __ATOMIC_RELEASE
#define atomic_load(ptr, ret)                   \
  (__atomic_load(ptr,ret,ATOMIC_ORDER_RW))
#define atomic_store(ptr, val)                  \
  (__atomic_store(ptr, val, ATOMIC_ORDER_RW))
#define atomic_exchange(ptr,val)                        \
  (__atomic_exchange_n(ptr,val,ATOMIC_ORDER_RW))
#define atomic_compare_exchange(ptr, expected, val)                     \
  (__atomic_compare_exchange_n(ptr, expected, val, 0,                   \
                               ATOMIC_ORDER_RW, ATOMIC_ORDER_RW))

#define atomic_load_explicit(ptr, ret, mo)      \
  (__atomic_load(ptr,ret,mo))
#define atomic_store_explicit(ptr, val, mo)     \
  (__atomic_store(ptr, val, mo))
#define atomic_exchange_explicit(ptr,val,mo)    \
  (__atomic_exchange(ptr,val,mo))
#define atomic_compare_exchange_explicit(ptr, expected, val, suc, fail) \
  (__atomic_compare_exchange_n(ptr, expected, val, 0,                   \
                               suc, fail))
#define atomic_fetch_add(ptr,val)                             \
  (__atomic_fetch_add(ptr,val,ATOMIC_ORDER_RW))
#define atomic_fetch_sub(ptr,val)                             \
  (__atomic_fetch_sub(ptr,val,ATOMIC_ORDER_RW))
#define atomic_fetch_and(ptr,val)                             \
  (__atomic_fetch_and(ptr,val,ATOMIC_ORDER_RW))
#define atomic_fetch_xor(ptr,val)                             \
  (__atomic_fetch_xor(ptr,val,ATOMIC_ORDER_RW))
#define atomic_fetch_or(ptr,val)                              \
  (__atomic_fetch_or(ptr,val,ATOMIC_ORDER_RW))

#define atomic_fetch_add_explicit(ptr,val,mo)                             \
  (__atomic_fetch_add(ptr,val,mo))
#define atomic_fetch_sub_explicit(ptr,val,mo)                             \
  (__atomic_fetch_sub(ptr,val,mo))
#define atomic_fetch_and_explicit(ptr,val,mo)                             \
  (__atomic_fetch_and(ptr,val,mo))
#define atomic_fetch_xor_explicit(ptr,val,mo)                             \
  (__atomic_fetch_xor(ptr,val,mo))
#define atomic_fetch_or_explicit(ptr,val,mo)                              \
  (__atomic_fetch_or(ptr,val,mo))
#endif /* defined __HAVE_C11__

/*
  These aren't available via C11 (or C++11) atomics, so they need
  to use gcc builtins.
  These implement atomic operations and return the value of 'ptr'
  after the operation, rather than before, (these are like ++x vs x++).

  This can be much faster for some operations than the verison which
  returns the previous value. For example on x86_64 all the bitwise
  fetch operations require a loop with cmpxchg, while these versions
  only require a single instruction.
*/

#define atomic_add(ptr,val)                             \
  (__atomic_add_fetch(ptr,val,ATOMIC_ORDER_RW))
#define atomic_inc(ptr)                                 \
  (__atomic_add_fetch(ptr, 1, ATOMIC_ORDER_RW))
#define atomic_sub(ptr,val)                             \
  (__atomic_sub_fetch(ptr,val,ATOMIC_ORDER_RW))
#define atomic_dec(ptr)                                 \
  (__atomic_sub_fetch(ptr, 1, ATOMIC_ORDER_RW))
#define atomic_and(ptr,val)                             \
  (__atomic_and_fetch(ptr,val,ATOMIC_ORDER_RW))
#define atomic_xor(ptr,val)                             \
  (__atomic_xor_fetch(ptr,val,ATOMIC_ORDER_RW))
#define atomic_or(ptr,val)                              \
  (__atomic_or_fetch(ptr,val,ATOMIC_ORDER_RW))

#define atomic_add_explicit(ptr,val)                    \
  (__atomic_add_fetch(ptr,val,ATOMIC_ORDER_RW))
#define atomic_inc_explicit(ptr)                        \
  (__atomic_add_fetch(ptr, 1, ATOMIC_ORDER_RW))
#define atomic_sub_explicit(ptr,val)                    \
  (__atomic_sub_fetch(ptr,val,ATOMIC_ORDER_RW))
#define atomic_dec_explicit(ptr)                        \
  (__atomic_sub_fetch(ptr, 1, ATOMIC_ORDER_RW))
#define atomic_and_explicit(ptr,val)                    \
  (__atomic_and_fetch(ptr,val,ATOMIC_ORDER_RW))
#define atomic_xor_explicit(ptr,val)                    \
  (__atomic_xor_fetch(ptr,val,ATOMIC_ORDER_RW))
#define atomic_or_explicit(ptr,val)                     \
  (__atomic_or_fetch(ptr,val,ATOMIC_ORDER_RW))
  
/*
  These versions take all arguments as pointers, allowing them to be used on
  types for which actual atomic cpu instructions can't be used.
*/
#define atomic_load_ptr(ptr)                    \
  (__atomic_load(ptr, ATOMIC_ORDER_RW))
#define atomic_store_ptr(ptr, val)              \
  (__atomic_store(ptr, val, ATOMIC_ORDER_RW))
#define atomic_exchange_ptr(ptr,val)                \
  (__atomic_exchange(ptr,val,ATOMIC_ORDER_RW))
#define atomic_cmpxchg_ptr(ptr, expected, val)                  \
  (__atomic_compare_exchange(ptr, expected, val, 0,             \
                             ATOMIC_ORDER_RW, ATOMIC_ORDER_RW))
/*
  These provide a simpler way to use compare exchange, the default version
  takes loc*, expected*, val, these take loc*, expected, val; this allows a
  literal value to be used for expected.

  The code for the macros themselves is based on the ways gcc implements the
  stdatomic macros.
*/
#define atomic_cmpxchg(ptr, expected, val)              \
  ({ __typeof__ (ptr) __ptr = (ptr);                    \
    __typeof__ (*ptr) __tmp = expected;                 \
    atomic_compare_exchange(ptr, &__tmp, val);})
#define atomic_cmpxchg_explicit(ptr, expected, val, suc, fail)          \
  ({ __typeof__ (ptr) __ptr = (ptr);                                    \
    __typeof__ (*ptr) __tmp = expected;                                 \
    atomic_compare_exchange_explicit(ptr, &__tmp, val, suc, fail);})

//these are just some synonyms
#define atomic_pre_inc(ptr)                     \
  (atomic_add(ptr,1))
#define atomic_pre_dec(ptr)                     \
  (atomic_add(ptr,-1))
#define atomic_xadd(ptr,val)                    \
  (atomic_fetch_add(ptr,val))
#define atomic_post_inc(ptr)                    \
  (atomic_fetch_add(ptr,1))
#define atomic_post_dec(ptr)                    \
  (atomic_fetch_add(ptr,-1))
#define atomic_write(ptr, val) atomic_store(ptr,val)
#define atomic_read(ptr) atomic_load(ptr)
/*
  put this in a spin wait loop to hopefully minimize the cpu load of
  the waiting thread.
*/
#ifdef __x86_64__
#define spin_pause() __asm__ volatile ("pause")
#else
#define spin_pasue() __asm__ volatile ("")
#endif
//all of the other atomic operations(xsub,xand,xxor,etc..) aren't
//really atomic (on x86_64), they use cmpxchg. I may
//add macros for them later

#endif /* __ATOMIC_H__ */
