//macros
/*Agate has simd instructions up to sse4.2*/
#ifndef NUM_PROCS
#ifdef AGATE
#define NUM_PROCS 16
#else
#define NUM_PROCS 3
#endif
#endif
#define MAX_BUF_SIZE (136*(1<<10))
#define PAGE_ROUND_DOWN(x) (((uint64_t)(x)) & (~(PAGESIZE-1)))
#define PAGE_ROUND_UP(x) ( (((uint64_t)(x)) + PAGESIZE-1)  & (~(PAGESIZE-1)) )
//this used to use lseek, but lseek is slower than fstat because
//it involves two syscalls becasue the file position needs to be reset
//after getting the file size.
#define file_size(fd)                           \
  ({struct stat buf;                            \
    fstat(fd,&buf);                             \
    buf.st_size;})
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


#define DEBUG
#if (defined DEBUG) && !(defined NDEBUG)
#define HERE() fprintf(stderr,"here at %s,line %d\n",__FILE__,__LINE__)
#define PRINT_MSG(string) fprintf(stderr,string);
#define PRINT_FMT(string,fmt...) fprintf(stderr,string,##fmt);
#define PRINT_WORD_ERR(/*english_word* */x) fwrite(x->str,x->len,1,stderr)
#define PRINT_STRING_ERR(str,len) fwrite(str,len,1,stderr)
#define PRINT_STRING_LINE_ERR(str,len) (fwrite(str,len,1,stderr);fputs("\n",stderr)
#define BREAKPOINT() __asm__ volatile ("int $3\n")
#else
#define HERE()
#define PRINT_MSG(string)
#define PRINT_FMT(string,fmt...)
#define PRINT_WORD_ERR(x)
#define PRINT_STRING_ERR(x)
#define PRINT_STRING_LINE_ERR(x)
#define BREAKPOINT()
#endif
