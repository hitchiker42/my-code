#pragma GCC diagnostic ignored "-Wunused-variable"
#pragma GCC diagnostic ignored "-Wunused-function"
#pragma GCC optimize (2)
/* What this program does
   open files given filename on command line,
   count words in each file, determine words common to each file
   find the 20 most common words that occur in each file

   input will be straight ascii text1

   a word is given by [a-zA-Z]{6,50}

   2GB max file length (so uint32_t can hold any length)
   100 files max, 1 file min

   information stored in one really big hash table (large enough to hold 
   pretty much every english word). This hash table uses open adressing with
   linear probing (meaning no linked lists, and collisions are resolved by
   putting the colliding value into the next free bucket), the large size of
   the hash table means the load factor will be small and this will be efficient.
*/
//includes
#define _GNU_SOURCE
#include <alloca.h>
#include <asm/unistd.h> //syscall numbers
#include <assert.h>
#include <errno.h>
#include <fcntl.h>//open
#include <sched.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <signal.h>
//#include <sys/mman.h>
#include <sys/types.h>//off_t,pid_t,ssize_t,etc
#include <unistd.h>//bunch of stuff, including close
#include <string.h>
#include <linux/futex.h>
#include <sys/time.h>

//macros
/*Agate has simd instructions up to sse4.2*/
#ifndef NUM_PROCS
#ifdef AGATE
#define NUM_PROCS 16
#else
#define NUM_PROCS 8
#endif
#endif
#define PAGE_ROUND_DOWN(x) (((uint64_t)(x)) & (~(PAGESIZE-1)))
#define PAGE_ROUND_UP(x) ( (((uint64_t)(x)) + PAGESIZE-1)  & (~(PAGESIZE-1)) )
#define file_size(fd)                           \
  ({off_t len=lseek(fd,0,SEEK_END);             \
    lseek(fd,0,SEEK_SET);                       \
    len;})
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
#define atomic_add(ptr,val)                     \
  __atomic_add_fetch(ptr,val,__ATOMIC_SEQ_CST)
#define atomic_fetch_add(ptr,val)                     \
  __atomic_fetch_add(ptr,val,__ATOMIC_SEQ_CST)
//with this (and pretty much any binary operation but add) the difference
//between fetch_or and or_fetch is a lot bigger, or_fetch is just a lock or
//whereas fetch_or translates into a cmpxcgh loop
#define atomic_or(ptr,val)                                      \
  __atomic_or_fetch(ptr,val,__ATOMIC_SEQ_CST)
#define gettid()                                \
  ({register uint64_t tid asm ("rax");          \
  __asm__("movq %1,%%rax\n"                      \
          "syscall"                              \
          : "=r" (tid) : "i" (__NR_gettid));     \
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
                               CLONE_CHILD_CLEARTID|CLONE_SYSVMEM|0)
//flagss that entail the least ammount of copying, meaning that 
//the new thread shares pretty much everything with it's parent
#define SIMPLE_CLONE_FLAGS  (CLONE_VM|CLONE_SIGHAND|CLONE_SYSVSEM| \
                              CLONE_PARENT_SETTID|CLONE_THREAD|0)


//typedefs
typedef unsigned __int128 uint128_t;
typedef __int128 int128_t;
typedef struct english_word english_word;
typedef struct internal_buf internal_buf;
typedef union file_bitfield file_bitfield;
struct pt_regs;

//forward declarations
long futex_up(int *futex_addr);
long futex_down(int *futex_addr);
long futex_spin_up(int *futex_addr);
long futex_spin_down(int *futex_addr);
int futex_wake(int *uaddr,int val);
int futex_wait(int *uaddr,int val);
long clone_syscall(unsigned long flags,void *child_stack,void *ptid,
                   void *ctid,struct pt_regs *regs);
//glibc clone wrapper
int clone(int (*fn)(void*),void *child_stack,int flags,void *arg,...);
/*...=pid_t *ptid, struct user_desc *tls, pid_t *ctid*/
void parse_buf(const uint8_t *buf);//core function
struct filebuf read_full_file(char *filename);
struct fileinfo open_file_simple(char *filename);
struct heap sort_words();
struct filebuf setup_block(uint8_t *block,uint32_t block_size);
long my_clone(unsigned long flags,void *child_stack,void *ptid,
              void (*fn)(void*),void *arg);
static int tgkill(int tgid, int tid, int sig);

//global scalars/uninitialized arrays
//allocate NUM_PROCS*2MB space for the thread stacks
static uint8_t thread_stacks[NUM_PROCS*(2<<20)];
#define THREAD_STACK_TOP(thread_id)             \
  ((thread_stacks+((thread_id+1)*(2<<20)))-1)
static uint8_t* thread_bufs[NUM_PROCS];
static uint8_t thread_status[NUM_PROCS];
//static int thread_futexes[NUM_PROCS];
static __thread uint64_t thread_id;
//static uint64_t next_thread_id=0;
static pid_t thread_pids[NUM_PROCS];
//this queue is only accessed after locking the thread_queue_lock
//Both the queue update and incremunting/decrementing the 
//queue index have to be done in one atomic step, meaning 
//if I didn't lock I would have to do something using cmpxchg
//and concidering how lightweight my spinlocks are I think it's
//ok to lok for this 
static uint8_t thread_queue[NUM_PROCS];
static uint8_t thread_queue_index;
static /*volatile*/ int thread_queue_lock __attribute__((aligned (16))) = 0;
static /*volatile*/ int main_thread_wait __attribute__((aligned (16))) = 0;
static sigset_t sigwait_set;
/*
  
  The basic idea with threading is to have NUM_PROC-1 worker threads 
  which are activly parsing buffers and updating the hash table and 
  1 thread to allocate data to those working threads. For simplicity
  call the allocating thread the main thread. The main thread first
  allocates and starts the worker threads then sleeps untill a worker
  is finished. The main thread gets the next block of data, assuming 
  there is any left and gives it to the worker thread. If any other 
  threads finished in the meantime allocate data for them untill all 
  threads are working again, then wait again. Do this untill there is 
  no data left. Then sort the data, I'm not sure if this should be 
  done with multiple threads.
  
  the basic idea for the implementation is:
  use a stack/queue for threads,
  after a thread has finished doing work it does the following:
  futex_spin_lock(&thread_queue_lock:
  thread_queue[thread_queue_index++]=thread_id;
  futex_wake(&main_thread_wait,1);
  futex_spin_unlock(&thread_queue_lock);
  sigwait(&sigwait_set,&signo);
  //check if the signal indicates that there is more data or
  //that we're done, and take action based on that 
*/

/*Assuming there that are ~1000000 english words, and probably half of those
  are 6-50 letters long, 8 MB should be large enough for a hash table that 
  won't exceed 50% capacity regardless of input. and 8 MB isn't so large as
  to be ridiculous, it's the same size as the default stack
  also no need to monitor the capacity because of how big the hash table is.

  though this may have its own issues in that iterating over this will take
  quite a lot of time. I think I need an auxaliry array with the indices of
  the used buckets.
*/
//it'd be nice to have this be an array of structs rather than an array of 
//pointers but that would prevent atomic updates. if I did change I'd use
//static english_word global_hash_table[1<<17];
//this uses the same ammount of memory, but doesn't use pointers it only
//holds 1/4 as many entries but it means I almost never have to call malloc
static english_word *global_hash_table[1<<20];
static const uint64_t global_hash_table_size=1<<20;//size in 8 byte blocks
//this hash table is big, really big, so big infact that iterating through
//it would be wasteful because of how many empty entries there are. So i use
//this array to keep track of which blocks of the hash table are actually
//in use
static uint32_t hash_table_indices[1<<18];
//This keeps track of in hash_table_indices to but the next entry, and as 
//a side effect keeps track of the number of entries in the hash table
static uint32_t indices_index=0;
static uint64_t num_files;
static const uint64_t PAGESIZE=4096;
static const uint64_t buf_size=128*(1<<10);//128kb, default mmap threshold for malloc

//structure definitions
struct internal_buf {
  char *buf;
  int file_id;
  int buf_num;
};
struct filebuf {
  uint8_t *buf;
  off_t len;
};
struct fileinfo {
  long fd;
  off_t len;
};
union file_bitfield{
  uint128_t uint128;
  struct {
    uint64_t low;
    uint64_t high;
  };
};
struct heap {english_word **heap; uint32_t size;};
static file_bitfield all_file_bits={.uint128=0};
/*structure for storing data about each word
  contains:
  -the word iself as a char * (not necessarly null terminated)
  -the word length in a 32 bit integer (we really only need 8 bits but 
    using 32 is more efficent alignment wise...probably)
  -A count of how many times the word has been seen, only uses in 
    the hash table entries
  -A means of uniquely identifying which file the word came from 
    (the actual implementation of this is a bit complicated)
*/
struct english_word {
  char *str;//NOT null terminated
  uint32_t len;
  uint32_t count;
  file_bitfield file_bits;
};

//global initialized arrays
/*1 if char is in the set [A-Za-z] zero otherwise */
static const uint8_t eng_accept[256]=
  {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
   0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
   0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
   0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
   1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
   0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
   0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
   0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
   0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};
//the Nth entry consists of a 64 bit integer with the least significant
//N+1 bits set and the rest of the bits unset
static const uint64_t file_bit_strings[64] =
  {0x1,0x3,0x7,0xf,
   0x1f,0x3f,0x7f,0xff,
   0x1ff,0x3ff,0x7ff,0xfff,
   0x1fff,0x3fff,0x7fff,0xffff,
   0x1ffff,0x3ffff,0x7ffff,0xfffff,
   0x1fffff,0x3fffff,0x7fffff,0xffffff,
   0x1ffffff,0x3ffffff,0x7ffffff,0xfffffff,
   0x1fffffff,0x3fffffff,0x7fffffff,0xffffffff,
   0x1ffffffff,0x3ffffffff,0x7ffffffff,0xfffffffff,
   0x1fffffffff,0x3fffffffff,0x7fffffffff,0xffffffffff,
   0x1ffffffffff,0x3ffffffffff,0x7ffffffffff,0xfffffffffff,
   0x1fffffffffff,0x3fffffffffff,0x7fffffffffff,0xffffffffffff,
   0x1ffffffffffff,0x3ffffffffffff,0x7ffffffffffff,0xfffffffffffff,
   0x1fffffffffffff,0x3fffffffffffff,0x7fffffffffffff,0xffffffffffffff,
   0x1ffffffffffffff,0x3ffffffffffffff,0x7ffffffffffffff,0xfffffffffffffff,
   0x1fffffffffffffff,0x3fffffffffffffff,0x7fffffffffffffff,0xffffffffffffffff};

//memory allocation wrappers
//these should be rewritten so I don't terminate the program if
//I use too much memory
static inline void *xmalloc(size_t sz){
  void *temp=malloc(sz);
  if(!temp && sz){
    perror("Error virtual memory exhausted");
    exit(EXIT_FAILURE);
  }
  return temp;
}
/*Same calling conviention as malloc, rather than calloc*/
static inline void *xcalloc(size_t sz){
  void *temp=calloc(sizeof(char),sz);
  if(!temp && sz){
    perror("Error virtual memory exhausted");
    exit(EXIT_FAILURE);
  }
  return temp;
}
static inline void *xrealloc(void *ptr,size_t sz){
  void *temp=realloc(ptr,sz);
  if(!temp && sz){
    perror("Error virtual memory exhausted");
    exit(EXIT_FAILURE);
  }
  return temp;
}
#define xfree free

/*simple but efficient hash function
  MurmurHash and cityhash are better hash functions but my version of 
  city hash is about 600 lines long (granted it does various lengths)
  and I haven't actually used MurmurHash before.

  These technically inplement the FNV-1a hash rather that the FNV-1 hash
  the only difference is the order of the XOR and multiply but the change
  in order gives FNV-1a much better avalanche behavior.
*/
static uint64_t fnv_hash(const void *key,int keylen){
  const uint8_t *raw_data=(const uint8_t *)key;
  int i;
  uint64_t hash=offset_basis_64;
  for(i=0; i < keylen; i++){
    hash = (hash ^ raw_data[i])*fnv_prime_64;
  }
  return hash;
}
//this might result in a more even distribution, but it might not 
static uint32_t fnv_hash_32_folded(const void *key,int keylen){
  const uint8_t *raw_data=(const uint8_t *)key;
  int i;
  union {
    uint64_t uint64; 
    struct {uint32_t low; uint32_t high;} uint32;
  } hash;
  hash.uint64=offset_basis_64;
  for(i=0; i < keylen; i++){
    hash.uint64 = (hash.uint64 ^ raw_data[i])*fnv_prime_64;
  }
  return (hash.uint32.high ^ hash.uint32.low);
}
//32 bit version, just for completeness
static uint32_t fnv_hash_32(const void *key,int keylen){
  const uint8_t *raw_data=(const uint8_t *)key;
  int i;
  uint64_t hash=offset_basis_32;
  for(i=0; i < keylen; i++){
    hash = (hash ^ raw_data[i])*fnv_prime_32;
  }
  return hash;
}

static int string_compare(english_word *x,english_word *y){
  if(x->len != y->len){
    return 0;
  } else {
    return !memcmp(x->str,y->str,x->len);
  }
}
//since my strings aren't null terminated I can't use
//the %s printf specifier so these functions are a convience to
//make printing eaiser
static void print_string(english_word *x){
  fwrite(x->str,x->len,1,stdout);
}
static void print_word_and_count(english_word *x){
  printf("The word ");
  fwrite(x->str,x->len,1,stdout);
  printf(" occurred %d times\n",x->count);
}
static void print_count_word(english_word *x){
  printf("%-8d ",x->count);
  fwrite(x->str,x->len,1,stdout);
  puts("");
}
/*   
   ideas for storing info:
   One hash table //currently used
   Trie //might use, but unlikely at this point
     -faster than a binary tree (which is why thats not an option)
     -Significantly more compilcated to implement
   Hash tree/trie //almost certantily won't use
     -complicated, not sure if it would be any faster either
*/
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
  __asm__("mov %0,%%rax\n\t"
          "syscall\n"
          : : "i" (__NR_exit), "r" (status));
  __builtin_unreachable();
}
//just call this with -1 in the first arg, this will signal in the
//current thread group.
//Need to add register specific vars if I want to inline this 
static int tgkill(int tgid, int tid, int sig){
  register int retval __asm__ ("%rax");
  __asm__("movq %0,%%rax\n\t"
          "syscall\n"
          : : "i" (__NR_tgkill), "r" (retval));
  return retval;
}
