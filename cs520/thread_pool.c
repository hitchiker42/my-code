#include "thread_pool.h"
sigset_t promise_wait_set;
struct pooled_thread {
  void (*work_function)(void *);
  void *input_data;
  void *output_data;
  sem_t semaphore;
  pthread_t thread_id;
  int32_t thread_no;
  phtread_spinlock_t data_lock;
};

int promise_wait(promise_t promise,const struct timespec *timeout){
  register uint32_t retval __asm__ ("%rax")=__NR_futex;
  register int *futex_addr __asm__("%rdi")=&promise.state;
  __asm__ volatile ("cmpq (%rdi),0\n\t"
                    "jeq 1f\n\t"
                    //Shift args                    
                    "movq %1,%%rcx\n\t"
                    //not sure why I need to do this, but I do
                    "xorq %%r8,%%r8\n\t"
                    "movl $0,%%edx\n\t"//val to arg3
                    "movq %2,%%rsi\n\t"
                    "movl %0,%%eax\n\t"
                    "syscall\n"
                    "1:\n\t"
                    : "+r" (retval)
                    : "g"  (timeout), "i" (FUTEX_WAIT), 
                      "r" (futex_addr)
                    : "%rsi","%rcx","%rdx","%r8");
  if(retval>=(uint32_t)-4095 && ((errno=-retval)!=EWOULDBLOCK) ){    
    perror("futex failure");
    exit(1);
//    return -1;//probably shuold 
  }
  return promise.state;
}
int promise_wake(promise_t promise){  
  register uint32_t retval __asm__ ("%rax")=__NR_futex;x
  register int *futex_addr __asm__("%rdi")=&promise.state;
  __asm__ volatile ("movl %1,%%edx\n\t"//move val to rdx (because it's the third arg
                    "movq %2,%%rsi\n\t"
                    "syscall\n"
                    : "+r" (retval)
                    : "i" (MAX_PROMISE_WAITERS), "i" (FUTEX_WAKE), "r" (futex_addr)
                    :"%rsi","%rdx");//probably more becaues of the syscall
  if(retval>=(uint32_t(-4095)){
      errno=-retval;
      perror("futex failure");
      exit(1);
  }
  return (int)retval;
}
extern inline int get_promise_state(promise_t promise){
  return promise.state;
}
inline void cancel_promise(promise_t promise){
  atomic_store_n(&promise.state,PROMISE_CANCLED);
  promise_wake(promise);
}
inline void fulfill_promise(promise_t promise, void *data){
  atomic_store_n(&promise.data,data);
  atomic_store_n(&promise.state,PROMISE_FULFILLED);
  promise_wake(promise);
}

void thread_pool_init(){
  
