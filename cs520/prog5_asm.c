//really this is more memcpy than strcpy, but the reason I wrote it
//is that I know I'll only be coping 6-50 bytes and the cost of aligning the
//data and copying in chunks isn't worth it. Plus it's small enough to inline
static inline char *my_strcpy(uint8_t *dest_,const uint8_t *src_,uint64_t len_){
  uint8_t *retval=dest_;
  register uint8_t *dest __asm__ ("%rdi")=dest_;
  const register uint8_t *src __asm__ ("%rsi")=src_;
  register uint64_t len __asm__ ("%rcx")=len_;
  __asm__("rep movsb"
          : : "r" (dest), "r" (src), "r" (len));
  return (char*)dest_;
}
//the glibc wrapper to _exit doesn't actually call _exit it calls
//exit_group, which isn't what we want, so this is actually a wrapper
//over the _exit syscall
static inline void __attribute__((noreturn)) thread_exit(int status_){
  register int status __asm__ ("%rdi")=status_;
  __asm__ volatile ("mov %0,%%rax\n\t"
          "syscall\n"
          : : "i" (__NR_exit), "r" (status));
  __builtin_unreachable();
}
//just call this with -1 in the first arg, this will signal in the
//current thread group.
//Need to add register specific vars if I want to inline this
static int tgkill(int tgid_, int tid_, int sig_){
  register int retval __asm__ ("%rax")=__NR_tgkill;
  register int tgid  __asm__ ("%rdi")=tgid_;
  register int tid __asm__ ("%rsi")=tid_;
  register int sig __asm__ ("%rdx")=sig_;
  __asm__ volatile ("syscall\n"
                    : "+r" (retval): "r" (tgid),"r"(tid),"r"(sig));
  return retval;
}
/* Assembly for futex routines
   the futex stuff will probably only be used for inter thread
   communication (i.e passing arguments/indicating a thread
   needs more data,etc) since my hash table is done using
   atomic opperations
*/
/*these next two functions will be useful for using futexs
  for things other than basic locks
*/

//interfate to the raw syscall
//check that *uadder==val then wait until a FUTEX_WAKE
int futex_wait(int *uaddr,int val){//ignore timeout argument
  register int retval __asm__ ("%rax");
  register int *futex_addr __asm__("%rdi")=uaddr;
  __asm__ volatile ("movl %1,%%edx\n\t"//move val to rdx (because it's the third arg
                    "movq %2,%%rsi\n\t"
                    "movq %3,%%rax\n\t"
                    "xorq %%rcx,%%rcx\n\t"
                    "xorq %%r8,%%r8\n\t"
                    //                    "lock subq $1,(%4)\n\t"
                    //hopefully if another thread adds to the futex before
                    //the kernel makes us wait the syscall return
                    //immediately
                    "syscall\n"
                    : "=r" (retval)
                    : "r" (val), "i" (FUTEX_WAIT), "i" (__NR_futex), "r" (futex_addr)
                    : "%rsi","%rcx","%rdx","%r8");
  return retval;
}
//interfate to the raw syscall
//wake up upto val processes waiting on the futex at uaddr
int futex_wake(int *uaddr,int val){//timeout ignored by syscall
  PRINT_MSG("Calling futex wake\n")
  register int retval __asm__ ("%rax");
  register int *futex_addr __asm__("%rdi")=uaddr;
  __asm__ volatile ("movl %1,%%edx\n\t"//move val to rdx (because it's the third arg
                    "movq %2,%%rsi\n\t"
                    "movq %3,%%rax\n\t"
                    "syscall\n"
                    : "=r" (retval)
                    : "r" (val), "i" (FUTEX_WAKE), "i" (__NR_futex),"r" (futex_addr)
                    :"%rsi","%rdx");
  PRINT_MSG("Called futex wake\n")
  return retval;
}
//futexs 1=free, 0=locked, no waiters -1=locked, waiters
//futex_up and futex_down implement the locking mechanism described
//int the futex(7) man page
long __attribute__((noinline))futex_up(int *uaddr){
  register int *futex_addr __asm__("%rdi") = uaddr;
  register long retval __asm__("%rax");
  __asm__ volatile("lock addq $1,(%1)\n\t"
                   "testq $1,(%1)\n\t"
                   //assume non contested case is most common, only jmp
                   //if contested
                   "jne 1f\n\t"
                   "retq\n"
                   "1:\n\t"
                   //if we get here it's contested, so setup futex syscall
                   "movq $1,(%1)\n\t"
                   "movq $0,%%rsi\n\t"
                   "movq $1,%%rdx\n\t"
                   "movq $202, %%rax\n\t" /*put futex syscall no in rax*/
                   "syscall\n\t"
                   : "=r" (retval) : "r"(futex_addr) :"%rsi","%rdx");
  return retval;
}

long __attribute ((noinline))futex_down(int *uaddr){
  register int *futex_addr __asm__("%rdi") = uaddr;
  register long retval __asm__("%rax");
  __asm__ volatile("lock subq $1,(%1)\n\t"
                   "cmpq $0,(%1)\n\t"
                   //same idea as with futex_up, assume non contested is most common
                   "jne 1f\n\t"
                   "retq\n"
                   "1:\n\t"
                   //if we get here it's contested, so setup futex syscall
                   "movq $-1,(%1)\n\t"
                   "movq $-1,%%rdx\n\t"
                   "movq $0,%%rsi\n\t"//this is FUTEX_WAIT
                   "xorq %%rcx,%%rcx\n\t"
                   "movq $202,%%rax\n\t"
                   "syscall\n\t"
                   : "=r" (retval) : "r"(futex_addr) :"%rsi","%rdx","%rcx");
  return retval;
}


/*        //unlock spin lock
        "ENTRY futex_spin_up\n\t"
        //much eaiser, we do the same thing no matter what
        "movq $1,%rax\n\t"
        "lock xchg %rax,(%rdi)\n"
        "END futex_spin_up\n\n"

        //lock spin lock
        "ENTRY futex_spin_down\n\t"
        "movq $1,%rax\n\t"
        "xorq %rcx,%rcx\n"
        "1:\n\t"//start spining
        "cmpq %rax,%rdi\n\t"//is the lock free?
        "je 2f\n\t"//if yes try to get it
        "pause\n\t"//if no pause
        "jmp 1b\n"//spin again
        "2:\n\t"
        "lock cmpxchgq %rcx,(%rdi)\n\t"//try to get lock
        "jnz 1b\n"//if we failed spin again
        "END futex_spin_down\n\n"*/
//clone returns 0 for new child and child tid for parent
long clone_syscall(unsigned long flags,void *child_stack,void *ptid,
                   void *ctid,struct pt_regs *regs){
  //this won't actually exist in the generated code, it's just a bookkeeping 
  //mechanism so gcc knows how to return
  register long retval __asm__ ("%rax");
  __asm__("movq %1,%%rax\n\t"
          "syscall\n"
          : "=r" (retval) : "i" (__NR_clone));
  return retval;
}
long my_clone(unsigned long flags_,void *child_stack_,void *ptid_,
              void (*fn)(void*),void *arg){
  register long retval __asm__ ("%rax");
  register unsigned long flags __asm__("%rdi")=flags_;
  register void *child_stack __asm__("%rsi")=child_stack_;
  register void *ptid __asm__("%rdx")=ptid_;
  __asm__("movq %0,%%rax\n\t"
          "xorq %%rcx,%%rcx\n\t" 
          "movq  $1,%%r8\n\t"
          "syscall\n"
          : : "i" (__NR_clone), "r" (retval), "r" (flags),"r" (child_stack), "r" (ptid)
          :"rcx","r8");
  if(retval < 0){
    perror("Clone failed");
    exit(EXIT_FAILURE);
  } else if(retval > 0) {
    return retval;
  } else {
    //this is in the new thread
    fn(arg);
    __builtin_unreachable();//tell gcc that we can never get here
  }
}
void futex_spin_up(register int *uaddr){        //unlock spin lock
  register long one=1;
  __asm__ volatile ("lock xchgq %0,(%1)\n"
                    : : "r" (one), "r" (uaddr));
}

void futex_spin_down(register int *uaddr){
  register int one __asm__ ("%rax")=1;
  register int zero = 0;
  __asm__ volatile("1:\n\t"//start spining
                   "cmpl %0,(%1)\n\t"//is the lock free?
                   "je 2f\n\t"//if yes try to get it
                   "pause\n\t"//if no pause
                   "jmp 1b\n"//spin again
                   "2:\n\t"
                   "lock cmpxchgl %2,(%1)\n\t"//try to get lock
                   "jnz 1b\n"//if we failed spin again
                   : : "r" (one), "r" (uaddr), "r" (zero));
}
/* 
   I don't even know why this doesn't work,
   but if I can't fix this I'll just stick to memcmp 
   because there's not way besides using repne cmpsb
   that I can make this small enough to inline 
*/
static inline int my_string_compare_asm(english_word *x,english_word *y){
  if(x->len != y->len){
    return 0;
  } else {
    uint64_t cnt=y->len;
    __asm__("movq %2,%%rdi\n\t"
            "movq %3,%%rsi\n\t"
            "movq %0,%%rcx\n\t"
            "repne cmpsb"
            : "=g" (cnt) : "0" (cnt),"g" (x->str), "g" (y->str)
            : "%rcx","%rdi","%rsi");
    return !cnt;
  }
}
