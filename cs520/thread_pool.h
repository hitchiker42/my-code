#ifndef __THREAD_POOL_H
#define __THREAD_POOL_H
#include <stdint.h>
#include <pthread.h>
#include <semaphore.h>
#include <err.h>
#include <errno.h>
#ifndef POOL_SIZE
#define POOL_SIZE 8
#endif
#ifndef MAX_PROMISE_WAITERS
#define MAX_PROMISE_WAITERS 128
#endif
typedef struct pooled_thread pooled_thread;
typedef struct promise_t promise_t;
//accessor functions guarantee atomic access to the fields of this 
//struct, if you don't use them you need to insure the fields are
//only accessed atomicly
enum PROMISE_STATE {
  PROMISE_UNFULFILLED=0,
  PROMISE_FULFILLED=1,
  PROMISE_CANCLED=2;
}
//needs to be stored in memory such that state is on a 32 bit boundry
struct promise_t {
  void *data;
  volatile int state;
};
//data that should be kept private unless there's a good reason to use it
pooled_thread *thread_pool;
int32_t max_thread_no;
pthread_once_t pool_initialized;
int32_t pool_size;
//atomic function macros
//desired is a pointer
#define atomic_compare_exchange(ptr,expected,desired)           \
  __atomic_compare_exchange(ptr,expected,desired,0,             \
                            __ATOMIC_SEQ_CST,__ATOMIC_SEQ_CST)
//desired isn't a pointer
#define atomic_compare_exchange_n(ptr,expected,desired)                 \
  __atomic_compare_exchange_n(ptr,expected,desired,0,                   \
                              __ATOMIC_SEQ_CST,__ATOMIC_SEQ_CST)
#define atomic_add(ptr,val)                     \
  __atomic_add_fetch(ptr,val,__ATOMIC_SEQ_CST)
#define atomic_sub(ptr,val)                     \
  __atomic_sub_fetch(ptr,val,__ATOMIC_SEQ_CST)
#define atomic_fetch_add(ptr,val)               \
  __atomic_fetch_add(ptr,val,__ATOMIC_SEQ_CST)
#define atomic_load_n(ptr)                      \
  __atomic_load_n(ptr,__ATOMIC_SEQ_CST)
#define atomic_load(ptr,loc)                    \
  __atomic_load(ptr,loc,__ATOMIC_SEQ_CST)
#define atomic_store(ptr,loc)                   \
  __atomic_store_n(ptr,loc,__ATOMIC_SEQ_CST)
#define atomic_store_n(ptr,val)                 \
  __atomic_store_n(ptr,val,__ATOMIC_SEQ_CST)
#define atomic_or(ptr,val)                      \
  __atomic_or_fetch(ptr,val,__ATOMIC_SEQ_CST)
#define atomic_and(ptr,val)                     \
  __atomic_and_fetch(ptr,val,__ATOMIC_SEQ_CST)
#define atomic_xor(ptr,val)                     \
  __atomic_xor_fetch(ptr,val,__ATOMIC_SEQ_CST)
#endif
