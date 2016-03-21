#define NEED_MALLOC
#define ATOMIC_HASHTABLE
#include "C_util.h"
#include "assert.h"
#include "hash.h"
#include <pthread.h>
#include <semaphore.h>
#define NUM_THREADS 4
#define num_threads NUM_THREADS
void *inc_counter(void *cntr){
  return cntr+1;
}
/*
  Add whitespace delimited words from 'words' to the hashtable ht.
  The value given to echo word is the index of it's first character in words.
*/
void add_to_htable(struct hashtable *ht, char *words, size_t sz){
  uint8_t *ptr = (uint8_t*)words;
  uint32_t word_len;
  long i = 0;
  while(i < sz){
    word_len = memcspn(ptr, sz-i, (uint8_t*)" \n\t", 3);
    //hashtable_add(ht, ptr, word_len, (void*)i);
    hashtable_update(ht, ptr, word_len, inc_counter);
    i += word_len;
    ptr += word_len;
    while(isspace(*ptr)){
      ptr++;
      i++;
    }
  }
  return;
}
void remove_from_htable(struct hashtable *ht, char *words, size_t sz){
  uint8_t *ptr = (uint8_t*)words;
  uint32_t word_len;
  long i = 0;
  while(i < sz){
    word_len = memcspn(ptr, sz-i, (uint8_t*)" \n\t", 3);
    //hashtable_add(ht, ptr, word_len, (void*)i);
    hashtable_remove(ht, ptr, word_len);
    i += word_len;
    ptr += word_len;
    while(isspace(*ptr)){
      ptr++;
      i++;
    }
  }
  return;
}
/*
  These need to be global to be accessable by every thread
*/
char *words;
struct hashtable *ht;
//sem_t sem;
size_t indices[num_threads] = {0};
size_t sizes[num_threads] = {0};
size_t total = 0;
void thread_main(int thread_id){
  DEBUG_PRINTF("Thread %d: index = %lu, size = %lu\n", thread_id,
               indices[thread_id], sizes[thread_id]);
  if(thread_id % 2){
    DEBUG_PRINTF("Thread %d adding to hashtable\n",thread_id);
    add_to_htable(ht, words + indices[thread_id/2], sizes[thread_id/2]);
  } else {
    DEBUG_PRINTF("Thread %d removing from hashtable\n",thread_id);
    remove_from_htable(ht, words + indices[thread_id/2], sizes[thread_id/2]);
  }
  DEBUG_PRINTF("returning from thread %lu\n", pthread_self());
}
void print_entry(void *key, size_t keylen, void *value){
  printf("key: %.*s, value: %ld\n", (int)keylen, (char*)key, (long)value);
  total += (long)value;
}
int main(int argc, char *argv[]){
  if(argc < 2){
    printf("usage ./test_hash input_file\n");
  }
  size_t sz;
  int fd = open(argv[1], O_RDONLY);
  if(fd>0){
    words = mmap_file(fd, 0, PROT_READ, &sz);
    if(!words){exit(1);}
    close(fd);
  } else {
    perror("open");
    exit(1);
  }
  enable_backtraces();
  ht = make_hashtable_default();
  /*
    This is a much simpler version of a program I wrote for a contest
    in a cs class which counts the occurances of each word in a block
    of text. The actual version had a lot more optimizations.
   */
//  if(num_threads > 1){
#ifdef ATOMIC_HASHTABLE
//  pthread_attr_t attr;
//  pthread_attr_setdetachstate(&attr, PTHREAD_CREATE_DETACHED);
//  sem_init(&sem, 0, 0);
  /*
    Could set the start/lengths of each chunk so multiple threads
    cover the same chunk and force collisions.
  */
  /*
    This part can be done much faster.
  */
  size_t divisor = num_threads;
  size_t inc = sz/divisor;
  //Make sure all chunks end on a word boundry
  ulong i;
  for(i=0;i<(num_threads/2)-1;i++){
    size_t idx = indices[i];
    char *endptr = (words + idx) + inc;
    while(!(isspace(*endptr++)));
    sizes[i] = endptr - (words+idx);
    indices[i+1] = indices[i] + sizes[i];
    while(isspace(*endptr)){
      endptr++;
      indices[i+1]++;
    }
  }
  sizes[i] = sz - indices[i];
  DEBUG_PRINTF("Dividing file of size %lu into %d parts:\n", num_threads, sz);
  for(i=0;i<num_threads/2;i++){
    DEBUG_PRINTF("index = %lu, size = %lu\n", indices[i], sizes[i]);
  }
  //the main thread isn't counted in num_threads
  pthread_t threads[num_threads];
  for(i=0;i<num_threads;i++){
    pthread_create(threads+i, NULL,
                   (void*(*)(void*))thread_main, (void*)i);
  }
  for(i=0;i<num_threads;i++){
    DEBUG_PRINTF("joining with thread %lu\n", threads[i]);
    pthread_join(threads[i], NULL);
  }
#else
  DEBUG_PRINTF("adding to htable\n");
  add_to_htable(ht, words, sz);
  DEBUG_PRINTF("removing from htable\n");
  remove_from_htable(ht, words, sz);
#endif  
//  hashtable_iter(ht, print_entry);
  fprintf(stderr, "Total wordcount = %ld\n", total);
  //  hashtable_bucket_counts(ht, stdout);
}
