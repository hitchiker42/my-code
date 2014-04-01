#pragma GCC diagnostic ignored "-Wunused-variable"
#pragma GCC diagnostic ignored "-Wunused-function"
#pragma GCC optimize (2)
#pragma GCC optimize ("whole-program")
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
#include <sys/stat.h>
//#include <sys/mman.h>
#include <sys/types.h>//off_t,pid_t,ssize_t,etc
#include <unistd.h>//bunch of stuff, including close
#include <string.h>
#include <linux/futex.h>
#include <sys/time.h>
#include "prog5_macros.h"

//typedefs
typedef unsigned __int128 uint128_t;
typedef __int128 int128_t;
typedef struct english_word english_word;
typedef struct internal_buf internal_buf;
typedef union file_bitfield file_bitfield;
struct pt_regs;
typedef int __attribute__((aligned(8))) aligned_int;

//forward declarations
long futex_up(int *futex_addr);
long futex_down(int *futex_addr);
void futex_spin_up(register int *futex_addr);
void futex_spin_down(register int *futex_addr);
int futex_wake(int *uaddr,int val);
int futex_wait(int *uaddr,int val);
long clone_syscall(unsigned long flags,void *child_stack,void *ptid,
                   void *ctid,struct pt_regs *regs);
//glibc clone wrapper
int clone(int (*fn)(void*),void *child_stack,int flags,void *arg,...);
/*...=pid_t *ptid, struct user_desc *tls, pid_t *ctid*/
void parse_buf(const uint8_t *buf,int file_id);//core function
struct filebuf read_full_file(char *filename);
struct fileinfo open_file_simple(char *filename);
struct heap sort_words();
struct fileinfo *setup_block(struct fileinfo *info,uint8_t *buf);
struct fileinfo *setup_fileinfo(char *filename);
long my_clone(unsigned long flags,void *child_stack,void *ptid,
              void (*fn)(void*),void *arg);
static int tgkill(int tgid, int tid, int sig);
void __attribute__((noreturn)) print_results(struct heap);
static char* __attribute__((const)) ordinal_suffix(uint32_t num);

//global scalars/uninitialized arrays
//allocate NUM_PROCS*2MB space for the thread stacks
static uint8_t thread_stacks[NUM_PROCS*(2<<20)];
#define THREAD_STACK_TOP(thread_id)             \
  ((thread_stacks+((thread_id+1)*(2<<20)))-1)
static uint8_t thread_status[NUM_PROCS];
//static int thread_futexes[NUM_PROCS];
static __thread uint64_t thread_id;
//static uint64_t next_thread_id=0;
static pid_t thread_pids[NUM_PROCS];
static uint8_t thread_buf_start_offsets[NUM_PROCS];
static pid_t tgid;
#include "prog5_consts.h"
//this queue is only accessed after locking the thread_queue_lock
//Both the queue update and incremunting/decrementing the
//queue index have to be done in one atomic step, meaning
//if I didn't lock I would have to do something using cmpxchg
//and concidering how lightweight my spinlocks are I think it's
//ok to lok for this
static uint8_t thread_queue[NUM_PROCS];
static uint8_t thread_queue_index=0;
//holds the information about the data to be given to threads
static struct fileinfo *fileinfo_queue[NUM_PROCS];
static uint64_t fileinfo_queue_index=-1;
//the aligned 16 here is probably excessive
static /*volatile*/ int32_t thread_queue_lock __attribute__((aligned (16))) = 1;
//this serves as a counter for threads waiting for data
//when it is 0 the main thread waits for some worker theread to increment it
//and call futex_wake. Whenever a worker thread finishes it increments this
//and calls futex_wake, thus whenever this is > 0 the main thread will keep 
//passing data to threads (as long as there is data left)

int32_t main_thread_wait __attribute__((aligned (16)))=0;
int32_t *main_thread_wait_ptr = &main_thread_wait;
static long thread_futexes[NUM_PROCS] __attribute__((aligned(16)));
static sigset_t sigwait_set;
static uint64_t num_files;
static uint64_t current_file=0;
static char **filenames;//=argv+1
//min_buf_size is the minium size of any one buffer, so if there
//are say 132Kb left in a file it will be read as one buffer
//rather than divining it into a 128kb buffer and a 4kb buffer
static uint8_t thread_bufs[NUM_PROCS][MAX_BUF_SIZE];
static uint32_t thread_file_ids[NUM_PROCS];
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
static const uint64_t global_hash_table_size=1<<18;//size in 8 byte blocks
#define GLOBAL_HASH_TABLE_SIZE (1<<18)
static english_word *global_hash_table[GLOBAL_HASH_TABLE_SIZE];

//this hash table is big, really big, so big infact that iterating through
//it would be wasteful because of how many empty entries there are. So i use
//this array to keep track of which blocks of the hash table are actually
//in use
static uint32_t hash_table_indices[GLOBAL_HASH_TABLE_SIZE/2];
//This keeps track of in hash_table_indices to but the next entry, and as
//a side effect keeps track of the number of entries in the hash table
static uint32_t indices_index=0;
static uint32_t next_file_id=0;
//structure definitions
struct internal_buf {
  char *buf;
  int file_id;
  //  int buf_num;//why do i need this 
};
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
struct filebuf {
  uint8_t *buf;
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
//I NEVER SET THIS, FIX THAT
static file_bitfield all_file_bits={.uint128=0};
static struct fileinfo fileinfo_mem[100];
static uint32_t fileinfo_mem_index=0;
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
static inline void print_string(english_word *x){
  fwrite(x->str,x->len,1,stdout);
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
