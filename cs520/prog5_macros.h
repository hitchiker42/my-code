//macros
/*Agate has simd instructions up to sse4.2*/
#ifndef NUM_PROCS
#ifdef AGATE
#define NUM_PROCS 16
#else
#define NUM_PROCS 8
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

//Make sure we only have one thread calling exit/printing stuff
#define PROGRAM_ERROR(code)                             \
  if(atomic_add(&in_error,1)){                          \
    code;                                               \
    exit(EXIT_FAILURE);                                 \
  }                                                     \
  else{__asm__ volatile("1:\n\tpause\n\tjmp 1b\n");}
//really hacky version of a cpp loop, I only use it
//to fill an array that's sized based on the number of processors
//and even just doing that I need to use some tricks with joining symbols
//to get it to work
#define CPP_JOIN(x,y) x##y
#define CPP_JOIN2(x,y) CPP_JOIN(x,y)
#define ARRAY_0(val) }
#define ARRAY_1(val) val ARRAY_0(val)
#define ARRAY_2(val) val, ARRAY_1(val)
#define ARRAY_3(val) val, ARRAY_2(val)
#define ARRAY_4(val) val, ARRAY_3(val)
#define ARRAY_5(val) val, ARRAY_4(val)
#define ARRAY_6(val) val, ARRAY_5(val)
#define ARRAY_7(val) val, ARRAY_6(val)
#define ARRAY_8(val) val, ARRAY_7(val)
#define ARRAY_9(val) val, ARRAY_8(val)
#define ARRAY_10(val) val, ARRAY_9(val)
#define ARRAY_11(val) val, ARRAY_10(val)
#define ARRAY_12(val) val, ARRAY_11(val)
#define ARRAY_13(val) val, ARRAY_12(val)
#define ARRAY_14(val) val, ARRAY_13(val)
#define ARRAY_15(val) val, ARRAY_14(val)
#define ARRAY_16(val) val, ARRAY_15(val)
#define ARRAY_17(val) val, ARRAY_16(val)
#define ARRAY_18(val) val, ARRAY_17(val)
#define ARRAY_19(val) val, ARRAY_18(val)
#define ARRAY_N(val,N) CPP_JOIN(ARRAY,N)(val)
#define ARRAY_PROCS(val) {CPP_JOIN2(ARRAY_,NUM_PROCS)(val)
