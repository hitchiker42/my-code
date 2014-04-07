#include "prog5_pthreads.h" 
#include "my_threads.c"
//strings never need to be freed but the ammout of space needed for them
//varies wildly so I allocate space for strings dynamically
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
/*
  Update the value of word in the global hash table, this means that if word
  isn't in the table then we should add it, and if it is we should increment the

  count on the value in the table and update the file index based on the
  set bit in the file index of word

  uses atomic operations not locking, which should make everything much faster,
  since we can have concurrent access to the hash table unlike with
  locking. (i.e in this version multiple threads can access different parts of
  the hash table all at once whereas with locking one thread blocks all others
  even those accessing a completely different part of the table
  
  return 0 if word in already in the table, return 1 if word was added
*/
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
/*
  main work function, searches buf for english words (matching [a-za-z]{6,50})
  and puts/updates the word in the global hash table.
*/

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
  thread_mem_pointer=thread_mem_block;
  while(!atomic_load_n(&out_of_data)){
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
    atomic_add(&main_thread_wait,1);
    if(builtin_unlikely(!sem_post(&main_thread_waiters))){
      PROGRAM_ERROR(perror("sem_post failure\n"));
    }
    //wait for main thread to give us more data
    if(builtin_unlikely(!sem_wait(thread_semaphors+thread_id)!=0)){
      PROGRAM_ERROR(perror("sem_wait failure\n"));
    }
  }
 EXIT:  
  atomic_dec(&live_threads);
  thread_exit(EXIT_SUCCESS);
}
void main_wait_loop(int have_data){
  uint8_t worker_thread_id=0;
  int32_t futex_retval;
  uint32_t out_of_data_local=0;
  if(!have_data){goto OUT_OF_DATA;}
  while(!sem_wait(&main_thread_waiters)){
    atomic_add(&main_thread_wait,-1);
    if(atomic_load_n(&main_thread_wait)>NUM_PROCS){
      PROGRAM_ERROR(fprintf(stderr,"More threads waiting then exist, exiting\n"));
    }
    spin_lock_lock(&thread_queue_lock);
    worker_thread_id=thread_queue[--thread_queue_index];
    out_of_data_local=(!setup_thread_args(worker_thread_id));
    atomic_store_n(thread_futexes+worker_thread_id,1);
    spin_lock_unlock(&thread_queue_lock);
    /*The issue is here, when setup_thread_args returns 0 it means that 
      there's no data left, but arguments for a thread have already been 
      setup. So I need to make sure I process this last set of data
      before I finish, and also I need to make sure to decrement the 
      count of live threads when the thread finishes, THIS was the
      source of my problems with joining threads (probably)
    */
    if(!sem_post(thread_semaphores+worker_thread_id)){
      PROGRAM_ERROR(perror("sem_post failure\n"));
    }
    if(out_of_data_local){
      PRINT_MSG("going to OUT_OF_DATA because out_of_data_local\n");
      goto OUT_OF_DATA;
    }
  }
  PROGRAM_ERROR(perror("Error in sem_wait"));
  //   PRINT_MSG("Refilling fileinfo queue\n");
  //if(!refill_fileinfo_queue()){
  //PRINT_MSG("going to OUT_OF_DATA because !refill_fileinfo_queue\n");
  //goto OUT_OF_DATA;
  //}
  //    PRINT_MSG("Refilled fileinfo queue\n"); */
OUT_OF_DATA:{
  PRINT_MSG("Out of data\n");
  atomic_store_n(&out_of_data,1);
  //    __asm__ volatile("int $3\n");
  //this can probably just be changed to
  //  while(sem_wait(&main_thread_waiters)){
  //if(!live_threads){break;}
  //}
  //then in the worker threads the exit process is
  //atomic_dec(live_threads);
  //sem_post(main_thread_wait);
  //pthread_exit();
  while(atomic_load_n(&live_threads)>0){
    /*while(atomic_load_n(&main_thread_wait)>0){ 
      atomic_dec(&main_thread_wait);
      PRINT_FMT("main thread locking spin lock (out of data)\n");
      spin_lock_lock(&thread_queue_lock);
      PRINT_FMT("main thread locked spin lock (out of data)\n");
      worker_thread_id=thread_queue[--thread_queue_index];
      spin_lock_unlock(&thread_queue_lock);
      PRINT_FMT("main thread unlocked spin lock (out of data)\n");*/
    //kind of a hack, but it works, just keep looping over all threads waking
    //them up untill they're all done
    for(worker_thread_id=0;worker_thread_id<NUM_PROCS-1;worker_thread_id++){
      atomic_store_n(thread_futexes+worker_thread_id,1);
      futex_retval=
        futex_wake_locking((int*)(thread_futexes+worker_thread_id),1,
                           (int*)(thread_futex_locks+worker_thread_id));
      if(builtin_unlikely(futex_retval==-1)){
        PROGRAM_ERROR(my_perror("Futex failure"));
      }
    }
    __asm__ volatile("pause\n\tpause\n\tpause\n\t");
  }
  PRINT_MSG("All worker threads finished\n");
  struct heap common_words=sort_words();
  print_results(common_words);
  exit(EXIT_SUCCESS);
}
}
//setup the buffer and file_id for the thread with thread_id
//then do the minimal ammount of work possible to insure
//that there is at least one entry in the fileinfo queue
int setup_thread_args(int thread_id_num){
  //nothing should ever leave fileinfo_queue completely empty,
  //fileinfo_queue[0] should always contain a vaild struct fileinfo
  static struct fileinfo *info;
  if(builtin_unlikely(fileinfo_queue_index<0)){
    PROGRAM_ERROR(fprintf(stderr,"Invalid value for fileinfo_queue_index\n"));
  }
  info=setup_block(fileinfo_queue[fileinfo_queue_index],thread_bufs[thread_id_num]);
  //  PRINT_MSG("setup block in setup_thread_args\n");
  thread_fileinfo_vals[thread_id_num].file_id=info->file_id;
  thread_fileinfo_vals[thread_id_num].start_offset=info->start_offset;
  //these conditionals make sure that fileinfo_queue has a valid entry
  //if it does not and there are no files left, we return 0 to indicate
  //that we're out of data
  if(info->remaining){
    return 1;
  }
  if(fileinfo_queue_index>0){
    fileinfo_queue_index--;
    return 1;
  }
  if(fileinfo_queue_index==0){
    if(current_file<num_files){
      info=setup_fileinfo(filenames[current_file++]);
      fileinfo_queue[0]=info;
      return 1;
    } else {
      return 0;
    }
  }
  __builtin_unreachable();
}
//fill the fileinfo queue as much as is possible. The idea is that
//this should get called when there are no threads waiting for data
//so that data can be given quickly when threads need it
int refill_fileinfo_queue(){
  //  PRINT_MSG("Calling refill_fileinfo_queue\n");
  static struct fileinfo *info;
  //  spin_lock_lock(&thread_queue_lock);
  if(current_file<num_files){
    while(fileinfo_queue_index<NUM_PROCS && current_file<num_files){
      info=setup_fileinfo(filenames[current_file++]);
      fileinfo_queue[fileinfo_queue_index++]=info;
    }
    if(fileinfo_queue_index){
      fileinfo_queue_index--;
    }    
  }
  int retval=fileinfo_queue[fileinfo_queue_index]->remaining?1:0;
  //  spin_lock_unlock(&thread_queue_lock);
  return retval;
}
/*
  Read the next block of memory by the file given in the info argument.
  This function deals with words that lie on the boundries of blocks
  it stores the start of a word at the end of a block info an interal
  buffer setup_block is called on info again it completes that word
  based on the start of the next block.
*/
struct fileinfo *setup_block(struct fileinfo *info, uint8_t *buf){
  //  PRINT_MSG("Setting up block\n");
  if(max_buf_size>info->remaining){
    //just read the rest of the file
    ssize_t nbytes=read(info->fd,buf,max_buf_size);
    if(builtin_unlikely(nbytes == (ssize_t)-1)){
      PROGRAM_ERROR(perror("error reading from file"));
    }
    if(builtin_unlikely(close(info->fd) == -1)){
      PROGRAM_ERROR(perror("error closing file"));
    }
    info->remaining=0;
    uint32_t start=nbytes-1;
    if(eng_accept[buf[start]]){
      buf[start+1]=0xff;
    } else {
      while(!eng_accept[buf[--start]]);
      buf[start+1]=0xff;
    }
  } else {
    ssize_t nbytes=read(info->fd,buf,buf_size);
    if(builtin_unlikely(nbytes == (ssize_t)-1 || nbytes < buf_size)){
      PROGRAM_ERROR(perror("error reading from file"));
    }
    info->remaining-=buf_size;
  }
  //this happens independent of weather we read the rest of the
  //file of still have some left, it depends on what was at
  //the end of the last block read from this file
  if(info->word_len){
    uint32_t index=0;
    if(eng_accept[buf[index]] && info->word_len <50){
      do {
        info->last_word[++info->word_len]=
          buf[index++];
      } while (eng_accept[buf[index]] && info->word_len <50);
    }
    if(builtin_unlikely(info->word_len==50)){
      while(eng_accept[buf[++index]]);
      info->word_len=0;
    }
    if(info->word_len>6){
      english_word *word=border_word_mem_ptr;
      *word=(english_word){.str=(char*)info->last_word,.len=info->word_len,
                           .count=1,.file_bits=file_bit_masks[info->file_id-1]};
      if(atomic_hash_table_update(word)){
        border_word_mem_ptr++;
      }
    }
    info->word_len=0;
    info->start_offset=index;
  } else {
    info->start_offset=0;
  }
  //this filles the last_word buf with an incomplete word from the end of
  //buf if there is one, and puts an eof at the end of the last word in buf
  if(info->remaining){
    uint32_t start=buf_size-1;
    uint32_t cur=start;
    if(eng_accept[buf[cur]]){
      while(eng_accept[buf[--cur]]);      
      //should this be (buf_size-1)-start?
      cur++;
      if(buf_size-start<50){
        my_strcpy((uint8_t*)info->last_word,buf+cur,(buf_size-cur));
      }
      info->word_len=buf_size-cur;
    }
    //mark eof at end of the buffer
    while(!eng_accept[buf[--cur]]);
    buf[cur+1]=0xff;
  }
  return info;
}
#include "prog5_heap.c"
//simple little helper function to print ordinal suffixes
static inline char* __attribute__((const)) ordinal_suffix(uint32_t num){
  if(num == 1){return "st";}
  if(num == 2){return "nd";}
  if(num == 3){return "rd";}
  return "th";
}

struct fileinfo *setup_fileinfo(char *filename){
  static struct stat stat_buf;
  int fd=open(filename,O_RDONLY);
  if(builtin_unlikely(fd == -1)){
    PROGRAM_ERROR(perror("error opening file"));
  }
  int stat_retval=fstat(fd,&stat_buf);
  if(builtin_unlikely(stat_retval == -1)){
    PROGRAM_ERROR(perror("error calling stat on file"));
  }
  if(builtin_unlikely(stat_buf.st_size == 0)){
    PROGRAM_ERROR
      (fprintf(stderr,"Found empty file, %s, no words common to all files\n",
               filename));
  }
  //this will probably change to using statically allocated memory
  struct fileinfo *info=fileinfo_mem+fileinfo_mem_index;
  fileinfo_mem_index+=1;
  *info=(struct fileinfo){.fd=fd,.len=stat_buf.st_size,.word_len=0,
                          .file_id=next_file_id++,.remaining=stat_buf.st_size};
  //  PRINT_MSG("Returning from setup_fileinfo\n");
  //  print_fileinfo(info);
  return info;
}
/*//only call with an unused fileinfo, behaves specially for
//files that are less then max_buf_len bytes long
struct fileinfo *init_thread_filestart(struct fileinfo *info,int thread_id_num){
  if(info->remaining < max_buf_size){
    ssize_t nbytes=read(info->fd,thread_bufs[thread_id_num],max_buf_size);
    if(builtin_unlikely(nbytes == (ssize_t)-1)){
      PROGRAM_ERROR(perror("error reading from file"));
    }
    if(builtin_unlikely(close(info->fd) == -1)){
      PROGRAM_ERROR(perror("error closing file"));
    }
    uint8_t *buf=thread_bufs[thread_id_num];
    uint32_t start=nbytes-1;
    if(eng_accept[buf[start]]){
      buf[start+1]=0xff;
    } else {
      while(!eng_accept[buf[--start]]);
      buf[start+1]=0xff;
    }
    thread_fileinfo_vals[thread_id_num].file_id=info->file_id;
    thread_fileinfo_vals[thread_id_num].start_offset=0;
    long tid=my_clone(SIMPLE_CLONE_FLAGS,THREAD_STACK_TOP(thread_id_num),
                      thread_pids+thread_id_num,thread_main,(void*)(long)thread_id_num);
    assert((tid == thread_pids[thread_id_num]) &&  tid != 0);
    return NULL;
  } else {
    info=setup_block(info,thread_bufs[thread_id_num]);
    thread_fileinfo_vals[thread_id_num].file_id=info->file_id;
    thread_fileinfo_vals[thread_id_num].start_offset=0;
    long tid=my_clone(SIMPLE_CLONE_FLAGS,THREAD_STACK_TOP(thread_id_num),
                      thread_pids+thread_id_num,thread_main,(void*)(long)thread_id_num);
    assert((tid == thread_pids[thread_id_num]) && tid!=0);
    return info;
  }
  }*/
struct fileinfo *init_thread(struct fileinfo *info,int thread_id_num){
  info=setup_block(info,thread_bufs[thread_id_num]);
  thread_fileinfo_vals[thread_id_num].file_id=info->file_id;
  thread_fileinfo_vals[thread_id_num].start_offset=0;
  assert(thread_attrs+thread_id_num);
  if((errno=pthread_create((pthread_t*)(&thread_id+thread_id_num),
                           (const pthread_attr_t *)(&default_thread_attr),
                           (void* (*) (void*))thread_main,
                           (void*)(uint64_t)thread_id_num))!=0){
    PROGRAM_ERROR(perror("pthread create failed"));
  }
  if(info->remaining){
    return info;
  } else {
    return NULL;
  }
}
//this can print all it wants since only one thread is running at the point
//this is called
void print_results(struct heap common_words){
#if (defined DEBUG) && !(defined NDEBUG)
  if(!is_sorted(common_words.heap,common_words.size)){
    printf("Failed to sort the heap\n");
  } else {
    printf("Sorted the heap\n");
  }
#endif
  int i;
  uint32_t size=common_words.size;
  for(i=size-1;i>=size-30;i--){
    //We can't use the %s format specifier of printf to print the actual word
    //because its not null terminated
    printf("The %d%s most common word was ",size-i,ordinal_suffix(size-i));
    print_word(common_words.heap[i]);
    printf(", with %d occurances\n",common_words.heap[i]->count);
  }
}
/* with threads heres what to do:
   if(argc>=NUM_PROCS-1){
   //more files than processors
   //just get the threads working on different files to start
   //then setup a queue of fileinfo structs to pass more data to the threads
   } else {
   //more processors than files
   //start num_files threads first
   //then if there is data left start more threads by calling setup_block
   //untill there is no data left or there are NUM_PROCS threads running
   }
   //once threads are started it should be the same pattern regardless
   //of the number of files
   */
void sem_cleanup(){
  sem_destroy(&main_thread_waiters);
}
int main(int argc,char *argv[]){
  struct fileinfo *info;
  int i;
  int have_data=1;
  //  sigemptyset(&block_sigterm);
  //  sigaddset(&block_sigterm,SIGTERM);
  //remove the program name from the arguments (its just eaiser)
  num_files=argc-1;
  PRINT_FMT("Given %ld files\n",num_files);
  filenames=argv+1;
  if(num_files<=0){
    PROGRAM_ERROR(fprintf(stderr,"Error no filenames given\n"));
  }
  tgid=gettgid();
  string_mem_pointer=mmap(NULL,(8*(1<<20)),PROT_READ|PROT_WRITE,
                          MAP_PRIVATE|MAP_ANONYMOUS,-1,0);
  string_mem_end=string_mem_pointer+(8*(1<<20));
  if(builtin_unlikely(string_mem_pointer==MAP_FAILED)){
    PROGRAM_ERROR(perror("mmap failed"));
  }
  //inilialize phread attrs
  pthread_attr_init(&default_thread_attr);
  pthread_attr_setdetachstate(&default_thread_attr,PTHREAD_CREATE_DETACHED);
  pthread_attr_setstacksize(&default_thread_attr,(2<<17));
  if(pthread_setattr_default_np(&default_thread_attr)!=0){
    PROGRAM_ERROR(perror("error setting default thread attrs"));
  }
  atexit(sem_cleanup);
  sem_init(main_thread_waiters,0,0);
  //inlitialize signal handlers
  /* sigfillset(&full_set); */
  /* int sa_flags=SA_RESETHAND; */
  /* const struct sigaction signal_action= */
  /*   {.sa_handler=sem_cleanup_handler,.sa_mask=full_set, */
  /*    .sa_flags=sa_flags}; */
  /* sigaction(SIGABRT,&signal_action,NULL); */
  /* sigaction(SIGTERM,&signal_action,NULL); */
  /* sigaction(SIGSEGV,&signal_action,NULL); */
  /* sigaction(SIGQUIT,&signal_action,NULL); */
  
  /*  for(i=0;i<NUM_PROCS;i++){
    pthread_getattr_default_np(&thread_attrs[i]);
    }*/
  if(num_files==1){
    current_file=1;
    all_file_bits.low=1;
    info=setup_fileinfo(*filenames);
    for(i=1;i<NUM_PROCS;i++){
      info=init_thread(info,i-1);
      live_threads++;
      if(!info){
        break;
      }
    }
    if(!info){
      have_data=0;
    } else {
      fileinfo_queue[++fileinfo_queue_index]=info;
    }
  } else {
    all_file_bits=file_bit_strings[num_files-1];
    if(num_files>=NUM_PROCS-1){
      for(i=1;i<NUM_PROCS;i++){
        info=setup_fileinfo(filenames[current_file++]);
        info=init_thread(info,i-1);
        live_threads++;
        if(info){
          fileinfo_queue[++fileinfo_queue_index]=info;
        }
      }
      if(fileinfo_queue_index==-1){
        if(current_file<num_files){
          fileinfo_queue[++fileinfo_queue_index]=
            setup_fileinfo(filenames[current_file++]);
        } else {
          have_data=0;
        }
      }
    } else {
      int i=1;
      while(current_file<num_files){
        info=setup_fileinfo(filenames[current_file++]);
        info=init_thread(info,i-1);
        live_threads++;
        if(info){
          fileinfo_queue[++fileinfo_queue_index]=info;
        }
        i++;
      }
      if(fileinfo_queue_index>=0){
        do {
          info=fileinfo_queue[fileinfo_queue_index];
          info=init_thread(info,i-1);
          live_threads++;
          if(!info){
            --fileinfo_queue_index;
          }
          i++;
        } while(i<NUM_PROCS && fileinfo_queue_index>=0);
      }
      if(fileinfo_queue_index<0){
        have_data=0;
      }
    }
  }
  PRINT_MSG("Finished starting threads\n");
  main_wait_loop(have_data);
}
