#ifndef __PROG5__
#define __PROG5__
#pragma GCC diagnostic ignored "-Wunused-variable"
#pragma GCC diagnostic ignored "-Wunused-function"
#pragma GCC diagnostic ignored "-Wformat"
#pragma GCC optimize ("O2")
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

   This program is always multithreaded, a minimum of 2 threads are needed.

   The number of threads used is determined by the macro NUM_PROCS, which
   defaults to 16 unless NOT_AGATE is defined in which case it defaults to 8.
   if NUM_PROCS is set to less then 2 something will break, so don't do that.
   
   The basic flow of the program is that there is 1 main thread and 
   N worker threads (where N is NUM_PROCS-1). The worker threads
   scan blocks of memory given to them by the main thread for english words
   and updating the hash table as they find them. 
*/
//includes
#define _GNU_SOURCE //makes some nonportable extensions available
#include <alloca.h>//alloca, unecessary because of _GNU_SOURCE 
#include <asm/unistd.h> //syscall numbers
#include <assert.h>//unused as of now
#include <err.h>
#include <errno.h>//declares errno and error macros
#include <fcntl.h>//open
#include <pthread.h>
#include <sched.h>//the manual page for clone says to include it
#include <semaphore.h>
#include <signal.h>
#include <stdarg.h>//vfprintf and the va_arg macros
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <strings.h>
#include <sys/mman.h>
#include <sys/stat.h>//fstat
#include <sys/time.h>//not really sure
#include <sys/types.h>//off_t,pid_t,ssize_t,etc
#include <time.h>
#include <unistd.h>//bunch of stuff, including close
#include <x86intrin.h>
#include "prog5_macros.h"
#endif
//typedefs

typedef unsigned __int128 uint128_t;
typedef __int128 int128_t;

typedef struct thread_fileinfo thread_fileinfo;
typedef struct english_word english_word;
typedef struct fileinfo fileinfo;
typedef union file_bitfield file_bitfield;
struct pt_regs;
typedef int __attribute__((aligned(8))) aligned_int;
typedef aligned_int spin_lock;
//forward declarations
void* parse_buf(const uint8_t *buf,int file_id,void *mem);//core function
struct heap sort_words();
struct fileinfo *setup_block(struct fileinfo *info,uint8_t *buf);
struct fileinfo *setup_fileinfo(char *filename);
int setup_thread_args(int thread_id_num);
int refill_fileinfo_queue();
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
//+  136*(1<<10)*NUM_PROCS = 272-2176KB  (thread buffers)
//+  16 MB (memory for strings)
//+  2MB*NUM_PROCS (16-32MB) (memory for everything else)
//+  various other data   <= 1MB
//=  ~27-~45 MB
//which is a lot of memory, to be fair, but no where near
//enough to put a strain on any modern computer's ram.
static uint8_t string_mem[(2<<23)] __attribute__((aligned(4096)));
uint8_t *string_mem_pointer=string_mem;
//uint8_t *string_mem_pointer;
static pthread_attr_t thread_attrs[NUM_PROCS];
static pthread_attr_t default_thread_attr;
static uint64_t pthread_thread_ids[NUM_PROCS];
static __thread uint64_t thread_id;
static uint8_t thread_mem_block[NUM_PROCS][2<<20];
static __thread void* thread_mem_pointer;
static sem_t main_thread_waiters;
static sem_t thread_semaphores[NUM_PROCS];
static sigset_t full_set;
static pid_t thread_pids[NUM_PROCS];
static pid_t tgid;//thread_group_id
static uint8_t keep_alive[NUM_PROCS];
//this queue is only accessed after locking the thread_queue_lock
//Becasue the queue update and incremunting/decrementing the
//queue index have to be done in one atomic step.
static uint8_t thread_queue[NUM_PROCS];
static uint8_t thread_queue_index=0;
//holds the information about the data to be given to threads
static struct fileinfo *fileinfo_queue[NUM_PROCS];
uint8_t thread_bufs[NUM_PROCS][MAX_BUF_SIZE];
thread_fileinfo thread_fileinfo_vals[NUM_PROCS];
static int64_t fileinfo_queue_index=-1;
static int32_t thread_queue_lock __attribute__((aligned (16))) = 1;
int32_t in_error = 0;
//  sigset_t sigwait_set;
uint64_t num_files;
uint64_t current_file=0;
uint64_t out_of_data=0;
uint64_t live_threads=0;
/*sigset_t block_sigterm;
  sigset_t block_sigtrap;*/
/* 
   I can't use malloc since my threads aren't visable to libc which messes
   up the internal locking done by malloc, and just messes everything up.
*/

uint8_t *string_mem_end;
int32_t  string_mem_lock __attribute__((aligned (16)))=1;
char **filenames;//=argv+1
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
static struct fileinfo fileinfo_mem[200];//double what we need, just to be safe
static uint32_t fileinfo_mem_index=0;
static english_word border_word_mem[8192];
static english_word *border_word_mem_ptr=border_word_mem;
//deal with words that fall on the boarder of memory blocks
//only accessed by the main thread, since that's the only thread that
//calls setup_block

#include "prog5_consts.h"

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
//    return !strncasecmp(x->str,y->str,x->len);
  }
}
//since my strings aren't null terminated I can't use
//the %s printf specifier so these functions are a convience to
//make printing eaiser
static inline void print_word(english_word *x){
  fwrite(x->str,x->len,1,stdout);
}
static inline void println_word(english_word *x){
  fwrite(x->str,x->len,1,stdout);
  fputc('\n',stdout);
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
void spin_lock_unlock(register int *uaddr){        //unlock spin lock
  register long one=1;
  __asm__ volatile ("mfence\n\t"
                    "lock xchgq %0,(%1)\n"
                    : : "r" (one), "r" (uaddr));
}
#define futex_spin_lock futex_spin_down
void spin_lock_lock(register int *uaddr){
  register int zero = 0;
  __asm__ volatile("1:\n\t"//start spining
                   "movq $1, %%rax\n\t"
                   "cmpl %%eax,(%0)\n\t"//is the lock free?
                   "je 2f\n\t"//if yes try to get it
                   "pause\n\t"//if no pause
                   "jmp 1b\n"//spin again
                   "2:\n\t"
                   "lock cmpxchgl %1,(%0)\n\t"//try to get lock
                   "jnz 1b\n"//if we failed spin again
                   : : "r" (uaddr), "r" (zero) : "%rax");
}
#define cond_wait_simple(mx,cnd,test)           \
  pthread_mutex_lock(mx);                       \
  while(test){                                  \
    pthread_cond_wait(cnd,mx);                  \
  }                                             \
  pthread_mutex_unlock(mx)
/* Assuming inputs are alphabetic characters i.e [A-Za-z] then
   in the ascii encoding A=0x41=0b01000001 and a=0x61=0b01100001
   the only difference is the thrird bit, this holds for all letters.
   so upcase just masks off the third bit and downcase sets it, i.e
   upcase(c)=c&0xdf and downcase(c)=c|0x20
*/
typedef union m128i {
  __m128i m128i;
  uint64_t m64i[2];
  uint32_t m32i[4];
  uint16_t m16i[8];
  uint8_t m8i[16];
} m128i;
static const m128i downcase_mask __attribute__((aligned(16)))=
  {.m32i={0x20202020,0x20202020,0x20202020,0x20202020}};
static const m128i upcase_mask __attribute__((aligned(16)))=
  {.m32i={0xdfdfdfdf,0xdfdfdfdf,0xdfdfdfdf,0xdfdfdfdf}};

static void upcase(char *str,int len){
  register __m128i mask=upcase_mask.m128i;
  __m128i chars;
  while(len>=16){
    chars=_mm_loadu_si128((__m128i*)str);
    chars=_mm_and_si128(mask,chars);
    _mm_storeu_si128((__m128i*)str,chars);
    str+=16;
    len-=16;
  }
  if(len&8){
    *(uint64_t*)str&=upcase_mask.m64i[0];
    str+=8;
    len-=8;
  }
  if(len&4){
    *(uint32_t*)str&=upcase_mask.m32i[0];
    str+=4;
    len-=4;
  }
  if(len&2){
    *(uint16_t*)str&=upcase_mask.m16i[0];
    str+=2;
    len-=2;
  }
  if(len&1){
    *(uint8_t*)str&=upcase_mask.m8i[0];
  }
  return;
}
static inline void upcase_short(char *str,int len){
  while(len--){
    *str=*str&(0xdf);
    str++;
  }
}
static void downcase(char *str,int len){
  register __m128i mask=downcase_mask.m128i;
  __m128i chars;
  while(len>=16){
    chars=_mm_loadu_si128((__m128i*)str);
    chars=_mm_or_si128(mask,chars);
    _mm_storeu_si128((__m128i*)str,chars);
    str+=16;
    len-=16;
  }
  if(len&8){
    *(uint64_t*)str|=downcase_mask.m64i[0];
    str+=8;
    len-=8;
  }
  if(len&4){
    *(uint32_t*)str|=downcase_mask.m32i[0];
    str+=4;
    len-=4;
  }
  if(len&2){
    *(uint16_t*)str|=downcase_mask.m16i[0];
    str+=2;
    len-=2;
  }
  if(len&1){
    *(uint8_t*)str|=downcase_mask.m8i[0];
  }
  return;
}
static inline void downcase_short(char *str,int len){
  while(len--){
    *str=*str|0x20;
    str++;
  }
}
