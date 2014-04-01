//macros
/*Agate has simd instructions up to sse4.2*/
#ifndef NUM_PROCS
#ifdef AGATE
#define NUM_PROCS 16
#else
#define NUM_PROCS 2
#endif
#endif
#define PAGE_ROUND_DOWN(x) (((uint64_t)(x)) & (~(PAGESIZE-1)))
#define PAGE_ROUND_UP(x) ( (((uint64_t)(x)) + PAGESIZE-1)  & (~(PAGESIZE-1)) )
//this used to use lseek, but lseek is slower than fstat because
//it involves two syscalls becasue the file position needs to be reset
//after getting the file size.
#define file_size(fd)                           \
  ({struct stat buf;                            \
    fstat(fd,&buf);                             \
    buf.st_size;})
//desired is a pointer
#define atomic_compare_exchange(ptr,expected,desired)           \
  __atomic_compare_exchange(ptr,expected,desired,0,             \
                            __ATOMIC_SEQ_CST,__ATOMIC_SEQ_CST)
//desired isn't a pointer
#define atomic_compare_exchange_n(ptr,expected,desired)           \
  __atomic_compare_exchange_n(ptr,expected,desired,0,             \
                            __ATOMIC_SEQ_CST,__ATOMIC_SEQ_CST)
#define atomic_bts(bit_index,mem_pointer)                       \
  __asm__("lock bts %0,%1\n"                                    \
          : : "r" (bit_index), "m" (mem_pointer))
/* atomic_add_fetch is lock add val,(ptr)
   while atomic_fetch_add is lock xadd val,(ptr)
 */
#define atomic_inc(ptr)                         \
  __asm__ volatile("lock incq (%0)" : :"r" (ptr));
#define atomic_add(ptr,val)                     \
  __atomic_add_fetch(ptr,val,__ATOMIC_SEQ_CST)
#define atomic_sub(ptr,val)                     \
  __atomic_sub_fetch(ptr,val,__ATOMIC_SEQ_CST)
#define atomic_fetch_add(ptr,val)                     \
  __atomic_fetch_add(ptr,val,__ATOMIC_SEQ_CST)
#define atomic_load_n(ptr)                     \
  __atomic_load_n(ptr,__ATOMIC_SEQ_CST)
#define atomic_store_n(ptr,val)                 \
  __atomic_store_n(ptr,val,__ATOMIC_SEQ_CST)
//with this (and pretty much any binary operation but add) the difference
//between fetch_or and or_fetch is a lot bigger, or_fetch is just a lock or
//whereas fetch_or translates into a cmpxcgh loop
#define atomic_or(ptr,val)                                      \
  __atomic_or_fetch(ptr,val,__ATOMIC_SEQ_CST)
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
#define array_swap_ptr(arr,_i,_j)                   \
  ({uint32_t i=_i,j=_j;                             \
    void *temp=arr[i];                              \
    arr[i]=arr[j];                                  \
    arr[j]=temp;})
#define array_swap_typed(arr,_i,_j,type)            \
  ({uint32_t i=_i,j=_j;                             \
    type temp=arr[i];                               \
    arr[i]=arr[j];                                  \
    arr[j]=temp;})
#define SWAP(a,b)                               \
  ({ __typeof__ (a) _a = a;                     \
    a=b;                                        \
    b=_a;                                       \
    ;})
#define PREFETCH_0(mem)                         \
  (__asm__ volatile ("prefetcht0 %0" : : "m" (mem));)
#define PREFETCH_1(mem)                                 \
   (__asm__ volatile ("prefetcht1 %0" : : "m" (mem));)
#define PREFETCH_2(mem)                                 \
   (__asm__ volatile ("prefetcht2 %0" : : "m" (mem));)
#define PREFETCH_NT(mem)                                \
  (__asm__ volatile ("prefetchnta %0" : : "m" (mem));)
#define set_eof_after_word(buf,start)                           \
  while(!eng_accept[buf[--end]]);                               \
  buf[end+1]=0xff
#define builtin_expect(expr,c) __builtin_expect((expr),c)
#define builtin_unlikely(expr) __builtin_expect((expr),0)
#define builtin_likely(expr) __builtin_expect((expr),1)
#define offset_basis_32 2166136261
#define offset_basis_64 14695981039346656037UL
#define fnv_prime_32 16777619
#define fnv_prime_64 1099511628211UL
#define futex_lock futex_down
#define futex_unlock futex_up
#define futex_spin_lock futex_spin_down
#define futex_spin_unlock futex_spin_up

#define DEBUG
#if (defined DEBUG) && !(defined NDEBUG)
#define HERE() fprintf(stderr,"here at %s,line %d\n",__FILE__,__LINE__)
#define PRINT_MSG(string) fprintf(stderr,string);
#define PRINT_FMT(string,fmt...) fprintf(stderr,string,##fmt);
#define PRINT_STRING_ERR(/*english_word* */x) fwrite(x->str,x->len,1,stderr)
#else
#define HERE()
#define PRINT_MSG(string)
#define PRINT_FMT(string,fmt...)
#define PRINT_STRING_ERR(x)
#endif
#define PTHREAD_CLONE_FLAGS   (CLONE_VM|CLONE_FS|CLONE_FILES|CLONE_SIGHAND|\
                               CLONE_SETTLS|CLONE_PARENT_SETTID|\
                               CLONE_CHILD_CLEARTID|CLONE_SYSVSEM|0)
//flagss that entail the least ammount of copying, meaning that
//the new thread shares pretty much everything with it's parent
#define SIMPLE_CLONE_FLAGS  (CLONE_VM|CLONE_SIGHAND|CLONE_SYSVSEM|CLONE_PTRACE| \
                             CLONE_PARENT_SETTID|CLONE_FS|CLONE_FILES|CLONE_THREAD|0)
