#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <pthread.h>
#include <sys/mman.h>
#include <errno.h>
#include <regex.h>
#ifndef NUM_PROCS
#ifdef AGATE
#define NUM_PROCS 16
#else
#define NUM_PROCS 8
#endif
#endif
#define PAGE_ROUND_DOWN(x) (((uint64_t)(x)) & (~(PAGE_SIZE-1)))
#define PAGE_ROUND_UP(x) ( (((uint64_t)(x)) + PAGE_SIZE-1)  & (~(PAGE_SIZE-1)) )
#define file_size(fd)                           \
  ({off_t len=lseek(fd,0,SEEK_END);             \
    lseek(fd,0,SEEK_SET);                       \
    len;})
#define offset_basis_32 2166136261
#define offset_basis_64 14695981039346656037UL
#define fnv_prime_32 16777619
#define fnv_prime_64 1099511628211UL
/* open files given filename on command line,
   count words in each file, determine words common to each file
   find the 20 most common words that occur in each file

   a word is given by [a-zA-Z]{6,50}

   2GB max file length (so uint32_t can hold any length)
   100 files max, 1 file min
   
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
   } 
*/
static volatile int futex_lock __attribute__((aligned (16))) = 0;

static const uint64_t PAGESIZE=4096;

/*1 if char is in the set [A-Za-z] zero otherwise */
static const char eng_accept[256]=
  {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
   0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
   1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
   1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
   0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
   0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
   0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}
typedef struct internal_buf internal_buf;
struct filebuf {
  char *buf;
  int file_id;
  int buf_num;
};
typedef struct english_word english_word;
typedef struct hash_table hash_table;
//avoid using c strings, use char arrays with explicit lengths 
struct english_word {
  char *str;//NOT a c string
  uint32_t len;
  uint32_t count;
};
struct hash_table {
  english_word *buckets;
  /*all these need to be updated atomically, which should happen by default*/
  uint32_t size;
  uint32_t used;//not sure that I need either this field or the next
  uint32_t entries;
  float capacity;
  float capacity_inc;
};

static inline void *xmalloc(size_t sz){
  void *temp=malloc(sz);
  if(!temp && sz){
    fprintf(stderr,"Error virtual memory exhausted\n");
    exit(EXIT_FAILURE);
  }
  return temp;
}
/*Same calling conviention as malloc, rather than calloc*/
static inline void *xcalloc(size_t sz){
  void *temp=calloc(sizeof(char),sz);
  if(!temp && sz){
    fprintf(stderr,"Error virtual memory exhausted\n");
    exit(EXIT_FAILURE);
  }
  return temp;
}
#define xfree free
static inline size_t /*__attribute__((pure))*/ word_len(char *str){
  size_t count=-1;
  while(eng_accept[str[++count]]);
  return count;
}

static uint64_t fnv_hash(const void *key,int keylen){
  const uint8_t *raw_data=(const uint8_t *)key;
  int i;
  uint64_t hash=offset_basis_64;
  for(i=0; i < keylen; i++){
    hash = (hash ^ raw_data[i])*fnv_prime_64;
  }
  return hash;
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

static int string_compare(struct {char *str; uint32_t len;} x,
                          struct {char *str; uint32_t len;} y){
  if(x.len != y.len){
    return 0;
  } else {
    return !memcmp(x.str,y.str,x.len);
  }
}
static const float ht_growth_threshold=0.8;
static const float ht_growth_factor=2.0;
static hash_table *make_hash_table(size_t size);
static hash_table_add(hash_table *ht,english_word word);
static hash_table_maybe_rehash(hash_table *ht);
static hash_table_rehash(hash_table *ht);
long futex_up(volatile long *futex_addr);
long futex_down(volatile long *futex_addr);
