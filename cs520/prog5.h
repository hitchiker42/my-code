/* TODO: Add two atomic global variables
   out_of_data=0:
   num_threads=NUM_PROCS-1;
   when there's no data left the main thread does
   atomic_inc(out_of_data);
   each worker thread checks this variable before waiting
   for more data, if it's one then the thread atomicly decrements
   num_threads and exits.
   
   the main thread waits untill num_threads == 0 after setting 
   out_of_data to 1
 */
#pragma GCC diagnostic ignored "-Wunused-variable"
#pragma GCC diagnostic ignored "-Wunused-function"
#pragma GCC diagnostic ignored "-Wformat"
#pragma GCC optimize ("Og")
//#pragma GCC optimize ("whole-program")
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
   the hash table means the load factor will be small and this will be 
   efficient. All modifications to this hash table are done atomically
   without using locks in any way.

   A note about the 100 file max, I use a bit vector internally to
   indicate which files any given word has appeared in, this lets me avoid 
   the complexity of merging hash tables while still being able to insure
   that a word has appeared in each file. Because there can be up to 100 
   files I need 128 bits for this bit vector. For convience I sometimes
   represent this value with the 128 bit integer type provided by gcc 
   as an extension. Most of the mainipulations I do treat this value
   as a pair of 64 bit integers for speed most of the time, but it's just
   something to be aware of.
*/
//includes
#define _GNU_SOURCE //makes some nonportable extensions available
#include <stdarg.h>//vfprintf and the va_arg macros
#include <alloca.h>//alloca, unecessary because of _GNU_SOURCE 
#include <asm/unistd.h> //syscall numbers
#include <assert.h>//unused as of now
#include <errno.h>//declares errno and error macros
#include <fcntl.h>//open
#include <sched.h>//the manual page for clone says to include it
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <signal.h>
#include <sys/stat.h>//fstat
#include <time.h>
//#include <sys/mman.h>
#include <sys/types.h>//off_t,pid_t,ssize_t,etc
#include <sys/mman.h>
#include <unistd.h>//bunch of stuff, including close
#include <string.h>
#include <sys/time.h>//not really sure
#include "prog5_macros.h"

//typedefs

typedef unsigned __int128 uint128_t;
typedef __int128 int128_t;

typedef struct thread_fileinfo thread_fileinfo;
typedef struct english_word english_word;
typedef struct fileinfo fileinfo;
typedef union file_bitfield file_bitfield;
struct pt_regs;
typedef int __attribute__((aligned(8))) aligned_int;

//forward declarations
long futex_up(int *futex_addr);//also #defined as futex_unlock
long futex_down(int *futex_addr);//"""" futex_lock
void futex_spin_up(register int *futex_addr);//"""" futex_spin_unlock
void futex_spin_down(register int *futex_addr);//"""" futex_spin_lock
int futex_wake(int *uaddr,int val);
int futex_wait(int *uaddr,int val,const struct timespec *timeout);
long clone_syscall(unsigned long flags,void *child_stack,void *ptid,
                   void *ctid,struct pt_regs *regs);
void* parse_buf(const uint8_t *buf,int file_id,void *mem);//core function
struct fileinfo open_file_simple(char *filename);
struct heap sort_words();
struct fileinfo *setup_block(struct fileinfo *info,uint8_t *buf);
struct fileinfo *setup_fileinfo(char *filename);
int setup_thread_args(int thread_id_num);
int refill_fileinfo_queue();
long my_clone(unsigned long flags,void *child_stack,void *ptid,
              void (*fn)(void*),void *arg);
static int tgkill(int tgid, int tid, int sig);
void print_results(struct heap);
static char* __attribute__((const)) ordinal_suffix(uint32_t num);
//structure definitions
//if I allocate these statically I'll need 100 of them
struct fileinfo {
  off_t len;
  off_t remaining;
  int fd;
  int file_id;//some number between 0-100 indicating what file
  //this represents
  int word_len;
  uint32_t start_offset;
  char last_word[50];
};
union file_bitfield{
  uint128_t uint128;
  struct {
    uint64_t low;
    uint64_t high;
  };
  //just because I can
  struct {
    uint32_t one;
    uint32_t two;
    uint32_t three;
    uint32_t four;
  };
  uint16_t words[8];
  uint8_t bytes[16];
};
struct thread_fileinfo {
  int32_t file_id;
  uint32_t start_offset;
};
struct heap {english_word **heap; uint32_t size;};
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

//global scalars/uninitialized arrays
//The majority of memory I use is allocated statically, and is
//large enough to handle anything I need, this ultimately adds up
//to (1<<20)*sizeof(english_word*) = 8MB (hash table)
//+  (1<<19)*sizeof(uint32_t) = 2MB      (hash table indices)
//+  (2<<20)*NUM_PROCS = 4-32MB          (thread stacks)
//+  136*(1<<10)*NUM_PROCS = 272-2176KB  (thread buffers)
//+  various other data   <= 1MB
// ~14MB + 256KB - ~44MB + 128KB
//which is a lot of memory, to be fair, but no where near
//enough to put a strain on any modern computer's ram.
//In addition to this between 38 (32+6) and 82(32+50) bytes.
//are needed to store each word, this is dynamically allocated

//Imagine that these are thread local variables
//static __thread uint64_t thread_id;
//static __thread void *thread_mem_pointer;
//static __thread uint64_t thread_mem_end;
//allocate NUM_PROCS*2MB space for the thread stacks
//probably could be reduced to about...256Kb I think, but eh
//if I used pthreads this is what they would allocate
static uint8_t thread_stacks[NUM_PROCS*(2<<20)];
#define THREAD_STACK_TOP(thread_id)             \
  ((thread_stacks+((thread_id+1)*(2<<20)))-1)
//static uint64_t next_thread_id=0;
static pid_t thread_pids[NUM_PROCS];
static pid_t tgid;//thread_group_id
//this queue is only accessed after locking the thread_queue_lock
//Becasue the queue update and incremunting/decrementing the
//queue index have to be done in one atomic step.
static uint8_t thread_queue[NUM_PROCS];
static uint8_t thread_queue_index=0;
//holds the information about the data to be given to threads
static struct fileinfo *fileinfo_queue[NUM_PROCS];
//these are really ints, but we need them aligned on 8 byte boundries
//I think, the futex man page says to use aligned integers
int64_t thread_futexes[NUM_PROCS] __attribute__((aligned(16)));
int64_t thread_futex_locks[NUM_PROCS] __attribute__((aligned(16)))=ARRAY_PROCS(1);
uint8_t thread_bufs[NUM_PROCS][MAX_BUF_SIZE];
thread_fileinfo thread_fileinfo_vals[NUM_PROCS];
static int64_t fileinfo_queue_index=-1;
static int32_t thread_queue_lock __attribute__((aligned (16))) = 1;
//this serves as a counter for threads waiting for data
//when it is 0 the main thread waits for so<me worker theread to increment it
//and call futex_wake. Whenever a worker thread finishes it increments this
//and calls futex_wake, thus whenever this is > 0 the main thread will keep 
//passing data to threads (as long as there is data left)
int32_t main_thread_wait __attribute__((aligned (16)))=0;
int32_t main_thread_wait_lock __attribute__((aligned (16)))=1;
sigset_t sigwait_set;
uint64_t num_files;
uint64_t current_file=0;
sigset_t block_sigterm;
sigset_t block_sigtrap;
/* 
   I can't use malloc since my threads aren't visable to libc which messes
   up the internal locking done by malloc, and just messes everything up.
*/
uint8_t *string_mem_pointer;
uint8_t *string_mem_end;
int32_t  string_mem_lock __attribute__((aligned (16)))=1;
char **filenames;//=argv+1
/*
  The basic flow of the program is that there is 1 main thread and 
  N worker threads (where N is the number of processors -1). The worker threads
  scan blocks of memory given to them by the main thread for english words
  and updating the hash table as they find them. 

  The main thread waits while the worker threads are parsing and gets woken up
  by the workers when they need more memory. The worker threads go to sleep
  after signaling the main thread and the main thread wakes then up and gives
  them more data to parse.
*/

/*Assuming there that are ~1000000 english words, and probably half of those
  are 6-50 letters long, 8 MB should be large enough for a hash table that
  won't exceed 50% capacity regardless of input. 
  (8 MB will hold ~800000 words, meaning ~400000 to keep it half full,
  so if given 500000 words it'll probably get to ~60% capacity, which is ok)
*/
//it'd be nice to have this be an array of structs rather than an array of
//pointers but that would prevent atomic updates.
static const uint64_t global_hash_table_size=1<<20;//size in 8 byte blocks
#define GLOBAL_HASH_TABLE_SIZE (1<<20)
static english_word *global_hash_table[GLOBAL_HASH_TABLE_SIZE];

//this hash table is big, really big, so big infact that iterating through
//it would be wasteful because of how many empty entries there are. So i use
//this array to keep track of which blocks of the hash table are actually
//in use, this artifically limits the hash table to only using 50% of
//it's space, but this limit is necessary to avoid collisions
static uint32_t hash_table_indices[GLOBAL_HASH_TABLE_SIZE/2];
//This keeps track of in hash_table_indices to but the next entry, and as
//a side effect keeps track of the number of entries in the hash table
static uint32_t indices_index=0;
static uint32_t next_file_id=1;
static file_bitfield all_file_bits={.uint128=0};
static struct fileinfo fileinfo_mem[100];
static uint32_t fileinfo_mem_index=0;
static english_word border_word_mem[64];//should be more that enough to
static english_word *border_word_mem_ptr=border_word_mem;
//deal with words that fall on the boarder of memory blocks
//only accessed by the main thread, since that's the only thread that
//calls setup_block

#include "prog5_consts.h"
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
#define xfree(x)

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

static inline int string_compare(english_word *x,english_word *y){
  if(x->len != y->len){
    return 0;
  } else {
    return !memcmp(x->str,y->str,x->len);
  }
}
//since my strings aren't null terminated I can't use
//the %s printf specifier so these functions are a convience to
//make printing eaiser
static inline void print_word(english_word *x){
  fwrite(x->str,x->len,1,stdout);
}
static inline void print_string(const char *str, uint32_t len){
  fwrite(str,len,1,stdout);
}
static inline void print_string_line(const char *str, uint32_t len){
  fwrite(str,len,1,stdout);
  fputs("\n",stdout);
}
static inline void print_word_and_count(english_word *x){
  printf("The word ");
  fwrite(x->str,x->len,1,stdout);
  printf(" occurred %d times\n",x->count);
}
static inline void print_count_word(english_word *x){
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
  __asm__ volatile("rep movsb"
                   : : "r" (dest), "r" (src), "r" (len));
  return (char*)dest_;
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
static inline void perror_fmt(const char *fmt,...){
  va_list ap;
  int errsave=errno;
  va_start(ap,fmt);  
  vfprintf(stderr,fmt,ap);
  fprintf(stderr,"%s number %d\n",_sys_errlist[errsave],errsave);
}
static inline void my_perror(const char *msg){
  if(msg){
    fprintf(stderr,"%s: %s; Error number %d\n",msg,_sys_errlist[errno],errno);
  } else {
    fprintf(stderr,"%s; Error number %d\n",_sys_errlist[errno],errno);
  }
}
int microsleep(long msec){
  static struct timespec sleep_time={.tv_sec=0,.tv_nsec=0};
  sleep_time.tv_nsec=msec*1000;
  return nanosleep(&sleep_time,NULL);
}
//Taken from stackoverflow and modified to print in hex rather than decimal
int print_uint128(uint128_t n) {
  char str[40] = {0}; // log10(1 << 128) + '\0'
  char *s = str + sizeof(str) - 1; // start at the end
  while (n != 0) {
    if (s == str) return -1; // never happens

    *--s = "0123456789abcdef"[n % 16]; // save last digit
    n >>= 4;                     // drop it
  }
  while(s-str>8){
    *--s='0';
  }
  *--s='x';
  *--s='0';
  return printf("%s\n", s);
}
#if (defined DEBUG) && !(defined NDEBUG)
static inline void print_fileinfo(struct fileinfo *info){
  fprintf(stderr,"Printing fileinfo struct at memory location %p\n",info);
  fprintf(stderr,"len = %ld\n",info->len);
  fprintf(stderr,"remaining = %ld\n",info->remaining);
  fprintf(stderr,"fd = %d\n",info->fd);
  fprintf(stderr,"file_id = %d\n",info->file_id);
  fprintf(stderr,"word_len = %d\n",info->word_len);
  fprintf(stderr,"start_offset = %d\n",info->start_offset);
  fprintf(stderr,"last_word = ");
  fwrite(info->last_word,info->word_len,1,stderr);
  fprintf(stderr,"\n");
};
#else
#define print_fileinfo(info)
#endif
