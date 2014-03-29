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
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
//#include <sys/mman.h>
#include <sys/types.h>//off_t,pid_t,ssize_t,etc
#include <unistd.h>//bunch of stuff, including close
#include <string.h>

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


//typedefs
typedef unsigned __int128 uint128_t;
typedef __int128 int128_t;
typedef struct english_word english_word;
typedef struct internal_buf internal_buf;
typedef union file_bitfield file_bitfield;
struct pt_regs;

//forward declarations
long futex_up(volatile long *futex_addr);
long futex_down(volatile long *futex_addr);
long futex_spin_up(volatile long *futex_addr);
long futex_spin_down(volatile long *futex_addr);
long clone_syscall(unsigned long flags,void *child_stack,void *ptid,
                   void *ctid,struct pt_regs *regs);
char *my_strcpy(uint8_t *dest,const uint8_t *src,uint64_t len);
void parse_buf(const uint8_t *buf);//core function
struct filebuf read_full_file(char *filename);
struct fileinfo open_file_simple(char *filename);
struct heap sort_words();

//global scalars/uninitialized arrays
//allocate NUM_PROCS*2MB space for the thread stacks
static uint8_t thread_stacks[NUM_PROCS*(2<<20)];
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
//pointers but that woud quadruple the size and 32 MB for a hash table
//seems a bit much. I'll profile it later and see if it's worth it 
static english_word *global_hash_table[1<<20];
static const uint64_t global_hash_table_size=1<<20;//size in 8 byte blocks
static uint32_t hash_table_indices[1<<18];
static uint32_t indices_index=0;
static uint64_t num_files;
static volatile long global_futex_lock __attribute__((aligned (16))) = 0;
static const uint64_t PAGESIZE=4096;
static const uint64_t buf_size=2<<12;//2*pagesize

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
  and I haven't actually used MurmurHash before. (I use cityhash in another
  project of mine)

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
static uint32_t fnv_hash_32(const void *key,int keylen){
  const uint8_t *raw_data=(const uint8_t *)key;
  int i;
  uint64_t hash=offset_basis_32;
  for(i=0; i < keylen; i++){
    hash = (hash ^ raw_data[i])*fnv_prime_32;
  }
  return hash;
}

//struct string {char *str;uint32_t len;};
static int string_compare(english_word *x,english_word *y){
  if(x->len != y->len){
    return 0;
  } else {
    return !memcmp(x->str,y->str,x->len);
  }
}

static void print_string(english_word *x){
  fwrite(x->str,x->len,1,stdout);
}

/*   
   ideas for storing info:
   simplest: use seperate hash tables
     -wastes memory
     -a lot of work to resolve the different files at the end
   One hash table
     -need to have a 100 bit bit field (or 64 bit if < 64 files) to store
       info about access from each file
     -requires quite a bit of locking, so I need to figure out how to do things
       atomically if I want speed
     -Extra bitwise or for each update (not a big deal)
     -No complicated resoltuion 
   Trie
     -faster than a binary tree (which is why thats not an option)
     -Significantly more compilcated to implement
   
   Hash tree/trie
     -complicated, not sure if it would be any faster either

   for the one hash table case a hash entry would be
   struct {
     char *str;
     uint32_t len;
     uint32_t count;
     uint64_t/uint128_t file_counter;
     } */
