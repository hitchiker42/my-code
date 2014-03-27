#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <pthread.h>
#ifndef NUM_PROCS
#ifdef AGATE
#define NUM_PROCS 16
#else
#define NUM_PROCS 8
#endif
#endif
static const uint64_t PAGESIZE=4096;
#define PAGE_ROUND_DOWN(x) (((uint64_t)(x)) & (~(PAGE_SIZE-1)))
#define PAGE_ROUND_UP(x) ( (((uint64_t)(x)) + PAGE_SIZE-1)  & (~(PAGE_SIZE-1)) )
#define file_size(fd)                           \
  ({off_t len=lseek(fd,0,SEEK_END);             \
    lseek(fd,0,SEEK_SET);                       \
    len;})
static const char *eng_accept=
  "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ";
typedef struct internal_buf internal_buf;
struct filebuf {
  char *buf;
  int file_id;
  int buf_num;
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
static inline size_t /*__attribute__((pure))*/ word_len(char *str){
  return strspn(str,eng_accept);
}
#define offset_basis_32 2166136261
#define offset_basis_64 14695981039346656037UL
#define fnv_prime_32 16777619
#define fnv_prime_64 1099511628211UL
static uint64_t fnv_hash(const void *key,int keylen){
  const uint8_t *raw_data=(const uint8_t *)key;
  int i;
  uint64_t hash=offset_basis_64;
  for(i=0; i < keylen; i++){
    hash = (hash ^ raw_data[i])*fnv_prime_64;
  }
  return hash;
}
typedef struct english_word english_word;
typedef struct hash_table hash_table;
struct english_word {
  char *str;//NOT a c string
  uint32_t len;
  uint32_t count;
};
static int english_word_compare(english_word x,english_word y){
  if(x.len != y.len){
    return 0;
  } else {
    return !memcmp(x.str,y.str,x.len);
  }
}
static const float ht_growth_threshold=0.8;
static const float ht_growth_factor=2.0;
struct hash_table {
  english_word *buckets;
  /*all these need to be updated atomically, which should happen by default*/
  uint32_t size;
  uint32_t used;//not sure that I need either this field or the next
  uint32_t entries;
  float capacity;
  float capacity_inc;
};
static hash_table* make_hash_table(size_t size){
  //it might be faster to do this in one allocation
  hash_table *retval=xcalloc(sizeof(hash_table));
  retval->size=size;
  retval->capacity_inc=1/(size*5);//lower capacity that I would normally use
  //kinda risky, if we put more than 10 items in one bucket before
  //reaching capacity this will access out of bounds memory
  //but I trust my hash functon to make sure that doesn't happen
  retval->buckets=xcalloc(sizeof(english_word)*size*10);
  retval->lock=xmalloc(sizeof(pthread_spinlock_t));
  /*  if(!pthread_spin_init(retval->lock,PTHREAD_PROCESS_PRIVATE)){
    exit(EXIT_FAILURE);
    }*/
  return retval;
}
static english_word hash_table_add(hash_table *ht,english_word word){
  uint64_t hashv=fnv_hash(word.str,word.len);
  uint32_t bucket_index=hashv%ht->size;
  //  pthread_spin_lock(ht->lock);
  english_word *bucket=ht->buckets+(10*bucket_index);
  if(!bucket){
    ht->used++;
    goto ADD_ENTRY;
  }
  while(*(uint64_t*)bucket){
    if(english_word_compare(word,*bucket)){
      bucket->count++;
      english_word retval=*bucket;
      //  pthread_spin_unlock(ht->lock);
      return retval;
    }
    bucket++;
  }
  ADD_ENTRY:
  *bucket=word;
  ht->capacity+=ht->capacity_inc;
  ht->entries++;
  maybe_rehash_hash_table(ht);
  return word;//count defaults to 1 on word;
}
static hash_table* hashtable_rehash(hash_table *ht){
  uint64_t old_len=ht->size;
  //update hash parameters
  ht->size*=ht_growth_factor;
  ht->capacity/=ht_growth_factor;
  ht->capacity_inc/=ht_growth_factor;
  ht->buckets=xrealloc(ht->buckets,(sizeof(english_word)*ht->size*10));
  //suprisingly important, new memory needs to be zeroed
  memset((void*)(ht->buckets+(old_len*10)),'\0',old_len*10);
  int i,j;
  english_word *bucket,*temp,*old_bucket;
  for(i=0;i<old_len;i++){
    bucket=ht->buckets+(i*10);
    while(*bucket)
      if(bucket->hashv%ht->size==i){
        bucket=bucket->next;
      } else {
        old_bucket=bucket;
        if(bucket->prev){
          bucket->prev->next=bucket->next;
        }
        if(bucket->next){
          bucket->next->prev=bucket->prev;
        }
        if(!ht->buckets[i+old_len%ht->size]){
          //this is an unused bucket
          ht->buckets[i+old_len]=bucket;
          bucket=bucket->next;
          old_bucket->prev=old_bucket->next=NULL;
          ht->used++;
        } else {
          //put old bucket list into a temp variable
          temp=ht->buckets[i+old_len%ht->size];
          ht->buckets[i+old_len%ht->size]=bucket;//set bucket to new value;
          temp->prev=bucket;//relink old bucket list
          //get next value in the bucket we're iterating through
          bucket=bucket->next;
          //now link the current value into the bucket list
          old_bucket->next=temp;
          old_bucket->prev=NULL;
        }
      }
    }
    if(!ht->buckets[i]){ht->used--;}
  }
  return ht;
}
//maybe I should use mmap
char** open_files(char **filenames,uint32_t num_files,uint32_t *bufs){
  char *filename;
  int i=0,j=0;
  //guess on average each file is 2 pages
  char **retval=xmalloc(num_files*2*sizeof(filebuf));
  while(i<num_files){
    filename=filenames[i];
    int fd=open(filename,O_RDONLY);
  if(fd == -1){
    perror("Error opening file");
    exit(1);
  }
  off_t len=file_size(fd);
  uint8_t *buf=xmalloc(PAGE_ROUND_UP(len));
  ssize_t nbytes=read(fd,buf,len);
  if(nbytes == (ssize_t)-1 /*|| nbytes != len*/){
    perror("Error reading from file");
    exit(EXIT_FAILURE);
  }
  if(close(fd) == -1){
    perror("Error closing file");
    exit(EXIT_FAILURE);
  }
  if(len > PAGESIZE){
    uint32_t offset=0;
    while(offset<len){
      *retval[j++]=buf+offset;
      offset++;
      if(j
  }
}
english_word next_word(filebuf){
  uint32_t len=word_len(filebuf->buf+filebuf->index),skip;
  while(len<6){
    filebuf->index+=len;
    skip=strcspn(filebuf->buf+filebuf->index,eng_accept);
    filebuf->index+=skip;
    len=word_len(filebuf->buf+filebuf->index);
  }
  english_word retval={.str=(filebuf->buf+filebuf->index),.len=len,.count=1};
  filebuf->index+=len;
  return retval;
}  
    
