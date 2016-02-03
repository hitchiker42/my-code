#define NEED_MALLOC
#include "C_util.h"
#include "assert.h"
#define NUM_THREADS 1
#define num_threads NUM_THREADS
/*
  Add whitespace delimited words from 'words' to the hashtable ht.
  The value given to echo word is the index of it's first character in words.
*/
void fill_htable(struct hashtable *ht, char *words, size_t sz){
  char *ptr = words;
  uint32_t word_len;
  long i = 0;
  while(i < sz){
    word_len = memcspn(ptr, sz-i, " \n\t", 3);
#if NUM_THREADS > 1
    hashtable_update(ht, ptr, word_len, inc_counter);
#else
    hashtable_add(ht, ptr, word_len, i);
#endif
    i += word_len;
    ptr += word_len;
    do{i++}while(isspace(*ptr++));//move to start of next word
  }
}
/*
  These need to be global to be accessable by every thread
*/
char *words;
struct hashtable *ht
size_t indices[num_threads] = {0};
size_t sizes[num_threads] = {0};
void *inc_counter(void *cntr){
  return cntr+1;
}
void thread_main(int thread_id){
  fill_htable(ht, words + indices[thread_id], sizes[thread_id]);
}
void print_entry(void *key, size_t keylen, void *value){
  printf("key: %.*s, value: %d\n", key, keylen, (int)value);
}
int main(int argc, char *argv[]){
  if(argc < 2){
    printf("usage ./test_hash input_file\n");
  }
  size_t sz;
  int fd = open(argv[1], O_RDONLY);
  if(fd>0){
    char *file = mmap_file(fd, 0, PROT_READ, &sz);
    if(!file){exit(1);}
    close(fd);
  } else {
    perror("fopen");
  }
  ht = make_hashtable_default();
  /*
    This is a much simpler version of a program I wrote for a contest
    in a cs class which counts the occurances of each word in a block
    of text. The actual version had a lot more optimizations.
   */
  if(num_threads > 1){
    /*
      Make multiple threads try to add some of the same words.
      Break the input into n+1 parts, and assign each thread
      n parts each.
    */
    /*
      This part can be done much faster.
    */
    size_t divisor = num_threads + 1;
    size_t inc = sz/divisor;
    //Make sure all chunks end on a word boundry
    int i;
    for(i=0;i<num_threads-1;i++){
      size_t idx = indices[i];
      char *endptr = words[idx];
      while(!(isspace(*endptr))){
        endptr++;
      }
      sizes[i] = idx + (endptr -(words+idx))
      while(isspace(*endptr++));
      indices[i+1] = idx + (endptr - (words+idx)) + inc;
    }
    sizes[i] = sz - indices[i];
    //the main thread isn't counted in num_threads
    pthread_t threads[num_threads];
    for(i=0;i<num_threads;i++){
      pthread_create(threads+i, NULL, (void*(*)(void*))thread_main, (void*)i);
    }
    for(i=0;i<num_threads;i++){
      pthread_join(threads[i]);
    }
  } else {
    fill_htable(ht, words, sz);
  }
  
  hashtable_iter(ht, print_entry);
}
