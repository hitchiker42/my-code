/* Fairly minimal low level thread library for x86_64 linux
   contains routines for creating threads, exiting from threads,
   various kinds of locking, atomic operations and signaling threads
   Copyright (C) Tucker DiNapoli 2014
*/
#include <linux/futex.h>
#include <asm/unistd.h>
#include <time.h>
#include <errno.h>
#include <stdio.h>
#include <stdarg.h>
#define PTHREAD_CLONE_FLAGS   (CLONE_VM|CLONE_FS|CLONE_FILES|CLONE_SIGHAND|\
                               CLONE_SETTLS|CLONE_PARENT_SETTID|\
                               CLONE_CHILD_CLEARTID|CLONE_SYSVSEM|0)
//flagss that entail the least ammount of copying, meaning that
//the new thread shares pretty much everything with it's parent
#define SIMPLE_CLONE_FLAGS  (CLONE_VM|CLONE_SIGHAND|CLONE_SYSVSEM|CLONE_PTRACE| \
                             CLONE_PARENT_SETTID|CLONE_FS|CLONE_FILES|CLONE_THREAD|0)
#include "inline_asm_macros.h"
#define atomic_store_n(ptr,val)                 \
  __atomic_store_n(ptr,val,__ATOMIC_SEQ_CST)
/* Assembly for futex routines.
   futexes are a linux exclusive means of 'fast userspace locking'
   the futex syscall itself is a means of waiting for a value at a specific
   adress to change. It is called similarly to compare and swap, with a pointer
   and an expected value. If the value is not equal to the pointer (this
   comparison is done atomically (using cmpxchg)) then the futex call
   immediately returns, if not then the kernel suspends the thread untill
   another thread calls futex to wake it up.
   (futex takes an argument to decide what kind of operation to perform)

   There are many ways of using this, the simplest way is using it to
   implement a simple lock, but in can be used to implement a whole bunch
   of other types of locks.

   NOTE: the way linux syscalls indicate errors is a bit odd, all errors return
   negitive values, but not all negitive values indicate error. Specifically
   errors will always return a value in the range of -4095 to -1. (looking
   at the futex code the value returned is -errno for whatever the error is)
   This comparison is much eaiser to make using unsigned values, because it
   only requires one comparision, if(retval>=(unsigned)-4095){error;}
*/
static const uint32_t min_error_val_u32=-4095;
static const int32_t min_error_val_32=-4095;
static const uint64_t min_error_val_u64=-4095;
static const int64_t min_error_val_64=-4095;
/*these next two functions will be useful for using futexs
  for things other than basic locks
*/

//interfate to the raw syscall (sligthly inefficent)
/* Akin to:
   int futex(int *uaddr_, int op_, int val_,
   const struct timespec *timeout_, int *uaddr2_,int val3_){
   __asm__ volatile("movq %0,%%rax\n\t"
                    "syscall\n\t"
                    "retq" : : "i"(__NR_futex));}
   but inline safe and without emitting warnings(also sets errno properly)
 */
int futex(int *uaddr_, int op_, int val_,
          const struct timespec *timeout_, int *uaddr2_,int val3_);
inline_safe_syscall6(int, futex, int*, uaddr, int, op, int, val,
                     const struct timespec*,timeout, int*, uaddr2, int, val3,
                     __NR_futex);
//interface to raw sysycall with FUTEX_WAIT as op argument
//check that *uadder==val, if not return immediately
//return value is -EWOULDBLOCK and errno is set to EWOULDBLOCK
//otherwise wait until a FUTEX_WAKE
#if 0
int futex_wait(int *uaddr,int val,const struct timespec *timeout){
  register uint32_t retval __asm__ ("%rax")=__NR_futex;
  register int *futex_addr __asm__("%rdi")=uaddr;
  __asm__ volatile (//Shift args
                    //apperently the timespec argument
                    //gets passed my value, so I need to xor r8 and rcx
                    "xorq %%rcx,%%rcx\n\t"//timeout to arg4
                    "xorq %%r8,%%r8\n\t"//timeout to arg4
                    "movl %1,%%edx\n\t"//val to arg3
                    "movq %2,%%rsi\n\t"
                    "movl %0,%%eax\n\t"
                    "syscall\n"
                    : "+r" (retval)
                    : /*"g" (timeout),*/ "r" (val), "i" (FUTEX_WAIT), 
                      "r" (futex_addr)
                    : "%rsi","%rcx","%rdx","%r8");
  if(retval>=min_error_val_u32){
    //    microsleep(1000);
    //fprintf(stderr,"value of &futex_addr = %d\nvalue of val = %d\n",*futex_addr,val);
    if((-retval)==EWOULDBLOCK){//this means we just skip waiting
      return 1;
    }
    return -1;
  }
  return (int)retval;
}
//interfate to the raw syscall with FUTEX_WAKE as the op argument
//wake up upto val processes waiting on the futex at uaddr
int futex_wake(int *uaddr,int val){//timeout ignored by syscall
  register uint32_t retval __asm__ ("%rax");
  register int *futex_addr __asm__("%rdi")=uaddr;
  __asm__ volatile ("movl %1,%%edx\n\t"//move val to rdx (because it's the third arg
                    "movq %2,%%rsi\n\t"
                    "movq %3,%%rax\n\t"
                    "syscall\n"
                    : "=r" (retval)
                    : "r" (val), "i" (FUTEX_WAKE), "i" (__NR_futex),"r" (futex_addr)
                    :"%rsi","%rdx");
  if(retval>=min_error_val_u32){
    //    atomic_store_n(&errno,-retval);
    return -1;
  }
  return (int)retval;
}
#endif
static inline int
futex_wait_locking(int *uaddr,int val,const struct timespec *timeout,int *uaddr2){
  register uint32_t retval __asm__ ("%rax");
  register int *futex_addr __asm__("%rdi")=uaddr;
  register int *lock __asm__("%r9")=uaddr2;
  __asm__ volatile ("movq $1,%%r10\n\t"
                    "movq $0,%%r11\n"
                    "1:\n\t"//lock
                    "movq $1,%%rax\n\t"
                    "cmpl %%eax,(%%r9)\n\t"//is the lock free?
                    "je 2f\n\t"//if yes try to get it
                    "pause\n\t"//if no pause
                    "jmp 1b\n"//spin again
                    "2:\n\t"
                    "lock cmpxchgl %%r11d,(%%r9)\n\t"//try to get lock
                    "jnz 1b\n"//if we failed spin again
                    //Shift args
                    //apperently the timespec argument
                    //gets passed my value, so I need to xor r8 and rcx
                    "xorq %%rcx,%%rcx\n\t"//timeout to arg4
                    "xorq %%r8,%%r8\n\t"//timeout to arg4
                    "movl %1,%%edx\n\t"//val to arg3
                    "movq %2,%%rsi\n\t"
                    "movl %3,%%eax\n\t"
                    //unlock
                    "lock xchgl %%r10d,(%%r9)\n\t"
                    //race condition?
                    "syscall\n"
                    : "=r" (retval)
                    : /*"g" (timeout),*/ "r" (val), "i" (FUTEX_WAIT), 
                      "i" (__NR_futex), "r" (futex_addr), "r"(lock)
                    : "%rsi","%rcx","%rdx","%r8","%r10","%r11");
  if(retval>=min_error_val_u32){
    //    fprintf(stderr,"value of &futex_addr = %d\nvalue of val = %d\n",*futex_addr,val);
    if(-retval==EWOULDBLOCK){//this means we just skip waiting
      return 1;
    }
    errno=-retval;
    return -1;
  }
  return (int)retval;
}
//interfate to the raw syscall with FUTEX_WAKE as the op argument
//wake up upto val processes waiting on the futex at uaddr
static inline int futex_wake_locking(int *uaddr,int val,int *uaddr2){//timeout ignored by syscall
  register uint32_t retval __asm__ ("%rax");
  register int *futex_addr __asm__("%rdi")=uaddr;
  register int *lock __asm__("%rbx")=uaddr2;
  register long one  __asm__("%rbp")=1;
  __asm__ volatile ("movq $0,%%r9\n"
                    "1:\n\t"//lock
                    "movq $1,%%rax\n\t"
                    "cmpl %%eax,(%%rbx)\n\t"//is the lock free?
                    "je 2f\n\t"//if it is try to get it
                    "pause\n\t"//if no pause
                    "jmp 1b\n"//spin again
                    "2:\n\t"
                    "lock cmpxchgl %%r9d,(%%rbx)\n\t"//try to get lock
                    "jnz 1b\n\t"//if we failed spin again
                    "movl %1,%%edx\n\t"//move val to rdx (because it's the third arg
                    "movq %2,%%rsi\n\t"
                    "movq %3,%%rax\n\t"
                    "syscall\n\t"
                    "lock xchgl %%ebp,(%%rbx)\n"
                    : "=r" (retval)
                    : "r" (val), "i" (FUTEX_WAKE), "i" (__NR_futex),
                      "r" (futex_addr), "r" (lock),"r"(one)
                    :"%rsi","%rdx","%r9");
  if(builtin_unlikely(retval>=min_error_val_u32)){
    //    atomic_store_n(&errno,-retval);
    return -1;
  }
  return (int)retval;
}
/*interfate to the raw syscall with FUTEX_CMP_REQUEUE as the op argument
  effectly a double call to futex, first wakes up val procsses waiting
  at uaddr, then essently calls futex with op=FUTEX_WAIT, 
  uaddr=uaddr2 and val=val3 for each of the remaning processes waiting
  on uaddr. Except that if *uaddr2!=val3 then the operation will fail
  with the error EAGAIN
*/
int futex_cmp_requeue(int *uaddr,int val,int *uaddr2,int val3){
  register uint32_t retval __asm__ ("%rax");
  register int *futex_addr __asm__("%rdi")=uaddr;
  __asm__ volatile (//Shift args
                    "movl %1,%%r9d\n\t"//val3 to arg6
                    "movq %2,%%r8\n\t"//uaddr2 to arg5
                    "movl %3,%%edx\n\t"//val to arg3
                    "movq %4,%%rsi\n\t"//op to arg2
                    "movq %5,%%rax\n\t"//syscall no to rax
                    "syscall\n"
                    : "=r" (retval)
                    : "r"(val3), "r"(uaddr2), "r" (val), "i" (FUTEX_CMP_REQUEUE), 
                      "i" (__NR_futex), "r" (futex_addr)
                    : "%rsi","%rcx","%rdx");
  if(retval>=min_error_val_u32){
    //    atomic_store_n(&errno,-retval);
    return -1;
  }
  return (int)retval;
}
/* Same as above but no check that on val3, you probably don't want to use this
   as cmp_requeue avoids a race condition that this has, at least for the 
   typical use. But there might be some other use for this.

 */
int futex_requeue(int *uaddr,int val,int *uaddr2){
  register uint32_t retval __asm__ ("%rax");
  register int *futex_addr __asm__("%rdi")=uaddr;
  __asm__ volatile (//Shift args
                    "movq %1,%%r8\n\t"//uaddr2 to arg5
                    "movl %2,%%edx\n\t"//val to arg3
                    "movq %3,%%rsi\n\t"//op to arg2
                    "movq %4,%%rax\n\t"//syscall no to rax
                    "syscall\n"
                    : "=r" (retval)
                    : "r"(uaddr2), "r" (val), "i" (FUTEX_CMP_REQUEUE), 
                      "i" (__NR_futex), "r" (futex_addr)
                    : "%rsi","%rcx","%rdx");
  if(retval>=min_error_val_u32){
    //    atomic_store_n(&errno,-retval);
    return -1;
  }
  return (int)retval;
}

//futexs 1=free, 0=locked, no waiters -1=locked, waiters
//futex_up and futex_down implement the locking mechanism described
//int the futex(7) man page
//this is equivlent to a basic mutex
#define futex_unlock futex_up
long futex_up(int *uaddr){
  register int *futex_addr __asm__("%rdi")= uaddr;
  register uint64_t retval __asm__("%rax")= __NR_futex;
  __asm__ volatile("lock addq $1,(%%rdi)\n\t"
                   "cmpl $1,(%%rdi)\n\t"
                   "je 1f\n\t"
                   //if we get here it's contested, so setup futex syscall
                   "movq $1,(%%rdi)\n\t"//first set futex to 1
                   "movq $1,%%rdx\n\t"//then move 1 to val argument
                   "movq %2,%%rsi\n\t"//set op to FUTEX_WAKE
                   "movq %0, %%rax\n\t" /*put futex syscall no in rax*/
                   "syscall\n\t"//make the syscall
                   "1:\n\t"
                   : "+r" (retval) : "r"(futex_addr), "i"(FUTEX_WAKE) :"%rsi","%rdx");
  if(retval>=min_error_val_u64){
    //    atomic_store_n(&errno,-retval);
    return -1;
  }
  return (long)retval;
}
#define futex_lock futex_down
long futex_down(int *uaddr){
  register uint64_t retval __asm__("%rax")=__NR_futex;
  register int *futex_addr __asm__("%rdi") = uaddr;
  __asm__ volatile("lock subq $1,(%%rdi)\n\t"
                   "cmpq $0,(%%rdi)\n\t"
                   "je 1f\n\t"
                   //if we get here it's contested, so setup futex syscall
                   "movq $-1,(%%rdi)\n\t"//first set the futex to -1
                   "movq $-1,%%rdx\n\t"//then move a -1 into the val argument
                   "movq %2,%%rsi\n\t"//set op to FUTEX_WAIT
                   "xorq %%rcx,%%rcx\n\t"//set timer argument to NULL
                   "xorq %%r8,%%r8\n\t"//set timer argument to NULL
                   "syscall\n\t"//make the syscall
                   "1:\n\t"
                   : "+r" (retval) : "r"(futex_addr),"i"(FUTEX_WAIT) :"%rsi","%rdx","%rcx","%r8");
  if(retval>=min_error_val_u64){
    //    atomic_store_n(&errno,-retval);
    return -1;
  }
  return (long)retval;
}
//these don't actually use the futex sycall, but they're named this way
//for convience
#define futex_spin_unlock futex_spin_up
void futex_spin_up(register int *uaddr){        //unlock spin lock
  register long one=1;
  __asm__ volatile ("lock xchgq %0,(%1)\n"
                    : : "r" (one), "r" (uaddr));
}
#define futex_spin_lock futex_spin_down
void futex_spin_down(register int *uaddr){
  register int zero = 0;
  __asm__ volatile("1:\n\t"//start spining
                   "movq $1, %%rax\n\t"
                   "cmpl %%eax,(%0)\n\t"//is the lock free?
                   "je 2f\n\t"//if yes try to get it
                   "pause\n\t"//if no pause
                   "jmp 1b\n"//spin again
                   "2:\n\t"
                   "lock cmpxchgl %1,(%0)\n\t"//try to get lock
                   "jnz 1b\n"//if we failed spin again
                   : : "r" (uaddr), "r" (zero) : "%rax");
}
#define futex_spin_lock_slow futex_spin_down_slow
void futex_spin_down_slow(register int *uaddr){
  register int zero = 0;
  __asm__ volatile("1:\n\t"//start spining
                   "movq $1,%%rax\n\t"
                   "cmpl %%eax,(%0)\n\t"//is the lock free?
                   "je 2f\n\t"//if yes try to get it
                   "pause\n\t"//if no pause
                   //slow version, pause a couple times
                   "pause\n\t"
                   "pause\n\t"
                   "pause\n\t"
                   "pause\n\t"
                   "jmp 1b\n"//spin again
                   "2:\n\t"
                   "lock cmpxchgl %1,(%0)\n\t"//try to get lock
                   "jnz 1b\n"//if we failed spin again
                   : : "r" (uaddr), "r" (zero) :"%rax");
}
//Routines for creating threads using the linux clone syscall
//the syscall itself acts like fork, it returns 0 in the new child thread
//and the childs tid in the parent thread
/*akin to 
  long clone_syscall(unsigned long flags,void *child_stack,void *ptid,
                   void *ctid,struct pt_regs *regs){
  register long retval __asm__ ("%rax");
  
  __asm__("movq %1,%%rax\n\t"
          "syscall\n"
          : "=r" (retval) : "i" (__NR_clone));
  return retval;
  but inline safe
}
*/
inline_safe_syscall5(long,clone_syscall,unsigned long, flags,void *, child_stack,
                     void *, ptid,void *, ctid,struct pt_regs *,regs,
                     __NR_clone);
//a wrapper around clone similar to the glibc wrapper/pthread_create
long my_clone(unsigned long flags_,void *child_stack_,void *ptid_,
              void (*fn)(void*),void *arg){
  register long retval __asm__ ("%rax") = __NR_clone;
  register unsigned long flags __asm__("%rdi")=flags_;
  register void *child_stack __asm__("%rsi")=child_stack_;
  register void *ptid __asm__("%rdx")=ptid_;
  __asm__ volatile ("xorq %%rcx,%%rcx\n\t"
                    "movq  $0,%%r8\n\t"
                    "syscall\n"
                    :"+r"(retval) : "r" (flags),"r" (child_stack), "r" (ptid)
                    :"rcx","r8");
  if((uint64_t)retval >= min_error_val_u64){//no new thread is created on error
    //    atomic_store_n(&errno,-retval);
    return -1;
  } else {
    if(retval > 0) {
      return retval;
    } else {
      //this is in the new thread
      fn(arg);
      __builtin_unreachable();//tell gcc that we can never get here
    }
  }
}

//int rt_sigprocmask(int how,const sigset_t *set,sigset_t *oldset)
inline_safe_syscall3(int, rt_sigprocmask, int, how, const sigset_t *,
                     set, sigset_t*, oldset,__NR_rt_sigprocmask);
//the glibc wrapper to _exit doesn't actually call _exit it calls
//exit_group, which isn't what we want, so this is actually a wrapper
//over the _exit syscall, it's akin to pthread_exit
extern inline void __attribute__((noreturn)) thread_exit(int status_){
  register int status __asm__ ("%rdi")=status_;
  __asm__ volatile ("mov %0,%%rax\n\t"
          "syscall\n"
          : : "i" (__NR_exit), "r" (status));
  __builtin_unreachable();
}
//just call this with -1 in the first arg, this will signal in the
//current thread group, akin to pthread_kill
/*declares int tgkill(int tgid,int tid,int sig)
 */
int tgkill(int tgid_,int tid_,int sig_);
inline_safe_syscall3(int,tgkill,int, tgid, int, tid, int, sig,__NR_tgkill);
//can't fail
#define gettid()                                \
  ({register uint64_t tid __asm__ ("rax");          \
  __asm__("movq %1,%%rax\n"                      \
          "syscall"                              \
          : "=r" (tid) : "i" (__NR_gettid));     \
  tid;})
#define gettgid()                                \
  ({register uint64_t tid __asm__ ("rax");           \
    __asm__("movq %1,%%rax\n"                    \
            "syscall"                            \
            : "=r" (tid) : "i" (__NR_getpid));   \
  tid;})

/* Atomic operations using a combination of builtin gcc functions
   and assembly. All the gcc functions are called with
   __ATOMIC_SEQ_CST as the memory model, this is the strongest memory model
   and so insures things are atomic.
*/
//desired is a pointer
#define atomic_compare_exchange(ptr,expected,desired)           \
  __atomic_compare_exchange(ptr,expected,desired,0,             \
                            __ATOMIC_SEQ_CST,__ATOMIC_SEQ_CST)
//desired isn't a pointer
#define atomic_compare_exchange_n(ptr,expected,desired)           \
  __atomic_compare_exchange_n(ptr,expected,desired,0,             \
                            __ATOMIC_SEQ_CST,__ATOMIC_SEQ_CST)
//indiviual bit manipulation instructions
//bit test and set
#define atomic_bit_set(bit_index,mem_pointer)           \
  __asm__ volatile ("lock bts %0,%1\n"                            \
          : : "r" (bit_index), "m" (mem_pointer))
#define atomic_bit_test_and_set(bit_index,mem_pointer)          \
  ({ uint8_t test;                                              \
    __asm__("lock bts %1,%2\n\t"                                \
            "setc %0\n"                                         \
            : "g"(test): "r" (bit_index), "m" (mem_pointer));   \
    test;})
//bit test and compliment
#define atomic_bit_compliment(bit_index,mem_pointer)    \
  __asm__ volatile ("lock btc %0,%1\n"                            \
          : : "r" (bit_index), "m" (mem_pointer))
#define atomic_bit_test_and_compliment(bit_index,mem_pointer)   \
  ({ uint8_t test;                                              \
    __asm__("lock btc %1,%2\n\t"                                \
            "setc %0\n"                                         \
            : "g"(test): "r" (bit_index), "m" (mem_pointer));   \
    test;})
//bit test and reset
#define atomic_bit_reset(bit_index,mem_pointer)         \
  __asm__ volatile ("lock btr %1,%0\n"                            \
          : : "r" (bit_index), "m" (mem_pointer))
#define atomic_bit_test_and_reset(bit_index,mem_pointer)        \
  ({ uint8_t test;                                              \
    __asm__("lock btr %1,%2\n\t"                                \
            "setc %0\n"                                         \
            : "g"(test): "r" (bit_index), "m" (mem_pointer));   \
    test;})
#define atomic_not(mem_pointer)                 \
  __asm__("lock not %0\n"                       \
          : "+m" (mem_pointer));
/* atomic_add_fetch is lock add val,(ptr)
   while atomic_fetch_add is lock xadd val,(ptr)
 */
#define atomic_inc(ptr)                                 \
  __asm__ volatile("lock incq (%0)" : :"r" (ptr));
#define atomic_dec(ptr)                                 \
  __asm__ volatile("lock decq (%0)" : :"r" (ptr));
#define atomic_add(ptr,val)                     \
  __atomic_add_fetch(ptr,val,__ATOMIC_SEQ_CST)
#define atomic_sub(ptr,val)                     \
  __atomic_sub_fetch(ptr,val,__ATOMIC_SEQ_CST)
#define atomic_fetch_add(ptr,val)               \
  __atomic_fetch_add(ptr,val,__ATOMIC_SEQ_CST)
#define atomic_load_n(ptr)                     \
  __atomic_load_n(ptr,__ATOMIC_SEQ_CST)
#define atomic_load(ptr,loc)                    \
  __atomic_load(ptr,loc,__ATOMIC_SEQ_CST)
#define atomic_store(ptr,loc)                 \
  __atomic_store_n(ptr,loc,__ATOMIC_SEQ_CST)
//with this (and pretty much any binary operation but add) the difference
//between fetch_or and or_fetch is a lot bigger, or_fetch is just a lock or
//whereas fetch_or translates into a cmpxcgh loop
#define atomic_or(ptr,val)                      \
  __atomic_or_fetch(ptr,val,__ATOMIC_SEQ_CST)
#define atomic_and(ptr,val)                     \
  __atomic_and_fetch(ptr,val,__ATOMIC_SEQ_CST)
#define atomic_xor(ptr,val)                     \
  __atomic_xor_fetch(ptr,val,__ATOMIC_SEQ_CST)
union word128 {
  uint128_t uint128;
  struct {
    uint64_t low;
    uint64_t high;
  };
  struct {
    uint32_t one;
    uint32_t two;
    uint32_t three;
    uint32_t four;
  };
  uint16_t words[8];
  uint8_t bytes[16];
};
static inline union word128 *compare_and_swap_16(union word128 *ptr,
                                                 union word128 val,
                                                 union word128 expected){
  register uint64_t rdx __asm__ ("%rdx") = expected.low;
  register uint64_t rax __asm__ ("%rax") = expected.high;
  register uint64_t rcx __asm__ ("%rdx") = val.low;
  register uint64_t rbx __asm__ ("%rax") = val.high;
  __asm__ volatile ("cmpxchg16b (%0)\n"
                    : "+r" (ptr) : "r"(rdx),"r"(rax),"r"(rcx),"r"(rbx));
  return ptr;
}
#if 0
static int32_t io_lock __attribute__((aligned (16)))=1;
#define locked_stdio_body(fn,rettype,args...)   \
  rettype retval;                               \
  futex_spin_lock_slow(&io_lock);               \
  fn##_unlocked(args);                          \
  futex_spin_unlock(&io_lock);                  \
  return retval
int my_putc(int c, FILE* stream){
  locked_stdio_body(putc,int,c,stream);
}
int my_putchar(int c){
  locked_stdio_body(putchar,int,c,stream);
}

int my_getc(int c, FILE* stream){
  locked_stdio_body(getc,int,c,stream);
}
int my_getchar(int c){
  locked_stdio_body(getchar,int,c,stream);
}
int my_fputs(const char *s, FILE *stream){
  locked_stdio_body(fputs,int,s,stream);
}
size_t my_fwrite(const void *ptr,size_t size,size_t nmemb,FILE *stream){
  locked_stdio_body(fwrite,size_t,ptr,size,nmemb,stream);
}

int my_fprintf(FILE *stream,const char *fmt, ...){
  char *buf;
  va_list ap;
  va_start(ap,fmt);
  vasprintf(&buf,fmt,ap);
  my_fputs(buf,stream);
  free(buf);
  return 0;//better return value later
}
  /*  uint64_t size_guess=200;
  int vsnprintf_retval;
  char *buf_end=buf+size_guess;
  char *buf=alloca(size_guess);

 START:
  vsnprintf_retval=vnnprintf(buf,size_guess,fmt,ap);
  if(vsnprintf_retval>size_guess){
    size_guess*=2;
    buf_end*/
  
#endif
