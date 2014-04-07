#include "prog5_pthreads.h"
static void *str_malloc(uint64_t size){
  spin_lock_lock(&string_mem_lock);
  if(string_mem_pointer+size>string_mem_end){
    string_mem_pointer=mmap(NULL,(8*(1<<20)),PROT_READ|PROT_WRITE,
                            MAP_PRIVATE|MAP_ANONYMOUS,-1,0);
    string_mem_end=string_mem_pointer+(8*(1<<20));
  }
  //I wish C had something akin to lisp's prog1,in lisp this would be: 
  //(prog1 string_mem_pointer (incf string_mem_pointer size))
  void *retval=string_mem_pointer;
  string_mem_pointer+=size;
  spin_lock_unlock(&string_mem_lock);
  return retval;
}
static int atomic_hash_table_update(english_word *word){
  uint64_t hashv=fnv_hash(word->str,word->len);
  uint64_t index=hashv%global_hash_table_size;
  int low=(word->file_bits.low?1:0);
  uint8_t *mem=NULL;
  //this next line results in a lot of cache misses
  //for obvious reasons
  if(!global_hash_table[index]){//word isn't in the hash table, add it
    mem=str_malloc(word->len);
    word->str=(char*)my_strcpy(mem,(uint8_t*)word->str,word->len);
    void *prev=global_hash_table[index];
    int test=atomic_compare_exchange_n(global_hash_table+index,&prev,word);
    if(test){
      //we added the word
      //this needs to be atomic to prevent two threads writing different
      //values to the same index of indices
      uint64_t old_indices_index=atomic_fetch_add(&indices_index,1);
      //this doesn't need to be atomic, since indices_index will never be
      //decremented, so no one else will change this
      hash_table_indices[old_indices_index]=index;
      goto end1;
    }
    //else, someone else changed the value of global_hash_table[index] before us
  }
  while(1){
    do {
      //see if the value in the table is the same as our value
      //if so update the value already in the table
      if(string_compare(global_hash_table[index],word)){
        //atomically increment word count
        atomic_add(&global_hash_table[index]->count,1);
        //atomiclly update the file index
        if(low){
          atomic_or(&global_hash_table[index]->file_bits.low,word->file_bits.low);
        } else {
          atomic_or(&global_hash_table[index]->file_bits.high,word->file_bits.high);
        }
        goto end0;
      }
    } while(global_hash_table[++index]);
    //not in the table use next free index (if we can)
    if(!mem){
      mem=str_malloc(word->len);
      word->str=(char*)my_strcpy(mem,(uint8_t*)word->str,word->len);
    }
    void *prev=global_hash_table[index];
    int test=atomic_compare_exchange_n(global_hash_table+index,&prev,word);
    if(test){
      uint64_t old_indices_index=atomic_fetch_add(&indices_index,1);
      hash_table_indices[old_indices_index]=index;
      goto end1;
    }
    //if !test the compare exchange failed and we need to keep looping
  }
 end0:
  return 0;
 end1:
  return 1;
}
void *parse_buf(register const uint8_t *buf_,int file_id,void *mem){
  //we know we're going to need buf for awhile, and we need to
  //save it across function calls so put it in rbx
  register const uint8_t *buf __asm__ ("%rbx")=buf_;
  //  const uint8_t *initial_buf=buf;//return value?
  uint64_t index=1;
  int count=0;
  union file_bitfield bitmask=file_bit_masks[file_id-1];
  //this doesn't seem to do much, positively or negitively
  //it might be worth trying different precetch instructions different
  //memory locations and different locations for the instruction
 PREFETCH:{
    __asm__ volatile ("prefetchnta 768(%rbx)");
  }
  //these cause tons of branch mispredictions
  //but I'm not sure how I should fix this
  //un-unrolling the loop probably wouldn't help but I can try
 START:
  if(eng_accept[*(buf)]){goto ACCEPT_0;}
  if(eng_accept[*(buf+1)]){goto ACCEPT_1;}
  if(eng_accept[*(buf+2)]){goto ACCEPT_2;}
  if(eng_accept[*(buf+3)]){goto ACCEPT_3;}
  buf+=4;
  goto START;
 ACCEPT_3:
  buf++;
 ACCEPT_2:
  buf++;
 ACCEPT_1:
  buf++;
 ACCEPT_0:
  if(!eng_accept[*(buf+index)]){goto REJECT_0;}
  if(!eng_accept[*(buf+index+1)]){goto REJECT_1;}
  if(!eng_accept[*(buf+index+2)]){goto REJECT_2;}
  if(!eng_accept[*(buf+index+3)]){goto REJECT_3;}
  index+=4;
  goto ACCEPT_0;
 REJECT_3:
  index++;
 REJECT_2:
  index++;
 REJECT_1:
  index++;
 REJECT_0:
  if(index >= 6 && index <= 50){
    english_word *word=mem;
    *word=(english_word){.str=(char*)buf,.len=index,.count=1,
                         .file_bits=bitmask};
    if(atomic_hash_table_update(word)){
      mem+=32;
    }
  }
  if(buf[index]!=0xff){
    buf+=index;
    index=1;
    goto PREFETCH;
  }
  //I used to free the buffer here, but now buffers are statically allocated
  return mem;
}
//main function for worker threads
void thread_main(void *arg){
    thread_id=(uint64_t)(arg);
    thread_mem_pointer=thread_mem_block+thread_id;
    sem_init(thread_semaphores+thread_id,0,0);
    /*  while(!atomic_load_n(&out_of_data)){
    //do stuff
    thread_mem_pointer=
      parse_buf(thread_bufs[thread_id] +
                thread_fileinfo_vals[thread_id].start_offset,
                thread_fileinfo_vals[thread_id].file_id,thread_mem_pointer);
    if(atomic_load_n(&out_of_data)){
      goto EXIT;
    }
    //tell main thread we're done
    spin_lock_lock(&thread_queue_lock);
    thread_queue[thread_queue_index++]=thread_id;
    spin_lock_unlock(&thread_queue_lock);
    if(builtin_unlikely(sem_post(&main_thread_waiters)!=0)){
      PROGRAM_ERROR(warn("sem_post failure in worker thread %ld\n",thread_id));
    }
    //wait for main thread to give us more data
    if(builtin_unlikely(!sem_wait(thread_semaphores+thread_id)!=0)){
      PROGRAM_ERROR(perror("sem_wait failure\n"));
    }
  }
 EXIT:  
  atomic_add(&live_threads,-1);
  //doesn't matter if this gets bigger than the number of threads at this point
  //since the main thread uses live_threads to test if all threads are done
  sem_post(&main_thread_waiters);
  pthread_exit(EXIT_SUCCESS);*/
}
void *trivial_thread_main(void *arg){
  fprintf(stderr,"Theread number %ld\n",(uint64_t)pthread_self());
  return 0;
}
int main(int argc,char *argv[]){
  //inilialize phread attrs
  pthread_attr_t default_thread_attr;
  pthread_attr_init(&default_thread_attr);
  pthread_attr_setdetachstate(&default_thread_attr,PTHREAD_CREATE_DETACHED);
  pthread_attr_setstacksize(&default_thread_attr,(2<<15));
    size_t stacksz;
  pthread_attr_getstacksize(&default_thread_attr,&stacksz);
  fprintf(stderr,"Attr stack size = %ld \n",stacksz);
  pthread_t *threads=alloca(sizeof(pthread_t)*argc-1);
  if((errno=pthread_create(threads,
                           &default_thread_attr,
                           (void* (*) (void*))trivial_thread_main,
                           NULL))!=0){
    perror("pthread create failed");
    exit(1);
  }
  pthread_exit(0);
}
