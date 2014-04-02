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
