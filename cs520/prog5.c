#include "prog5.h"
#define NDEBUG
#include "my_threads.c"
#undef NDEBUG
void terminate_gracefully(int sig){
  if(sig != SIGSEGV){
    PRINT_FMT("Thread %ld recieved signal %d, exiting thread\n",
              (long)thread_pids[thread_id],sig);
    thread_exit(EXIT_SUCCESS);
  }
  exit(1);
}
const struct sigaction signal_action={.sa_handler=terminate_gracefully};
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
  //this next line results in a lot of cache misses
  //for obvious reasons
  if(!global_hash_table[index]){//word isn't in the hash table, add it
    uint8_t *mem=xmalloc(word->len);
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
struct filebuf read_full_file(char *filename){
  static struct stat stat_buf;
  long fd=open(filename,O_RDONLY);
  if(builtin_unlikely(fd == -1)){
    perror("error opening file");
    exit(1);
  }
  int stat_retval=fstat(fd,&stat_buf);
  if(builtin_unlikely(stat_retval == (off_t)-1)){
    perror("error calling stat on file");
    exit(1);
  }
  uint8_t *buf=xmalloc(stat_buf.st_size);

  ssize_t nbytes=read(fd,buf,stat_buf.st_size);
  if(builtin_unlikely(nbytes == (ssize_t)-1)){
    perror("error reading from file");
    exit(EXIT_FAILURE);
  }
  if(builtin_unlikely(close(fd) == -1)){
    perror("error closing file");
    exit(EXIT_FAILURE);
  }
  uint32_t end=stat_buf.st_size-1;
  while(!eng_accept[buf[--end]]);
  buf[end+1]=0xff;
  return (struct filebuf){.buf=buf,.len=end};
}
/*
   main work function, searches buf for english words (matching [a-za-z]{6,50})
   and puts/updates the word in the global hash table.
*/

void parse_buf(register const uint8_t *buf_,int file_id){
  //we know we're going to need buf for awhile, and we need to
  //save it across function calls so put it in rbx
  register const uint8_t *buf __asm__ ("%rbx")=buf_;
  const uint8_t *initial_buf=buf;//return value?
  uint64_t index=1;
  union file_bitfield bitmask;
  if(file_id>=64){
    file_id-=64;
    bitmask.high=file_bit_masks[file_id];
  } else {
    bitmask.low=file_bit_masks[file_id];
  }
//  PRINT_FMT("Start of parse buf in thread %ld\n",thread_id);
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
    english_word *word=xmalloc(sizeof(english_word));
    *word=(english_word){.str=(char*)buf,.len=index,.count=1,
                         .file_bits=bitmask};
    if(!atomic_hash_table_update(word)){
      free(word);
    }
  }
  if(buf[index]!=0xff){
//    PRINT_FMT("Non word character %#hhx\n",buf[index]);
    buf+=index;
    index=1;
    goto PREFETCH;
  }
//  PRINT_FMT("End of parse buf in thread %ld\n",thread_id);
  //I used to free the buffer here, but now buffers are statically allocated
  return;
}
//main function for worker threads
void thread_main(void *arg){
  int futex_retval=-1;
  thread_id=(uint64_t)(arg);//set the thread local thread_id
  PRINT_FMT("Worker thread %ld started\n",thread_id);
  while(1){
    parse_buf(thread_bufs[thread_id]+thread_buf_start_offsets[thread_id],
              thread_file_ids[thread_id]);//do stuff
//    PRINT_FMT("Worker thread %ld parsed_buf\n",thread_id);
    //tell main thread we're done
    atomic_store_n(thread_status+thread_id,0);//I don't think I use this anymore
    futex_spin_lock(&thread_queue_lock);
//    PRINT_FMT("Worker thread %ld locked spin lock\n",thread_id);
    thread_queue[thread_queue_index++]=thread_id;
    atomic_inc(&main_thread_wait);
    futex_retval=futex_wake(&main_thread_wait,10);
    futex_spin_unlock(&thread_queue_lock);
//    PRINT_FMT("Worker thread %ld unlocked spin lock\n",thread_id);
    if(futex_retval==-1){
      perror("Futex failure\n");
      exit(1);
    }
    //wait for main thread to give us more data
//    PRINT_FMT("Worker thread %ld waiting for main\n",thread_id);
    futex_wait((int*)thread_futexes+thread_id,0,NULL);
    //sigwait(&sigwait_set,&signo);
    atomic_store_n(thread_status+thread_id,1);//this prevents a race contition,maybe
    atomic_store_n(thread_futexes+thread_id,0);//
  }
}
void main_wait_loop(int have_data){
  uint8_t worker_thread_id;
  int32_t futex_retval;
  int live_threads;//used when we run out of data to monitor the number
  //of threads left running
  //if there are already threads waiting on us skip the wait code
  if(atomic_load_n(&main_thread_wait)>1){
    goto ALLOCATE_LOOP;
  }
  while(1){
    //wait untill a thread is done;
//    PRINT_FMT("Main waiting with val %d\n",main_thread_wait);
    futex_retval=futex_wait(&main_thread_wait,const_zero_32,NULL);
//    PRINT_FMT("Main woke up with val %d\n",main_thread_wait);
    if(futex_retval==-1){
      my_perror("Futex failure");
      exit(1);
    }
    if(main_thread_wait<-1){
      fprintf(stderr,"Invalid value in futex main_thread_wait\n");
      exit(1);
    }
  ALLOCATE_LOOP:
    //
    while(atomic_fetch_add(&main_thread_wait,-1)>0){
      if(main_thread_wait>NUM_PROCS){
        fprintf(stderr,"More threads waiting then exist, exiting\n");
        exit(1);
      }
      futex_spin_lock(&thread_queue_lock);
      worker_thread_id=thread_queue[--thread_queue_index];
      futex_spin_unlock(&thread_queue_lock);
      if(!setup_thread_args(worker_thread_id)){
        //if there's no data left, kill this thread and then
        //wait for the others to finish
        tgkill(tgid,thread_pids[worker_thread_id],SIGTERM);
        live_threads=NUM_PROCS-2;
        goto OUT_OF_DATA;
      }
      futex_wake((int*)thread_futexes+worker_thread_id,10);
    }
    //we decremented one more time then we should (we need to do this)
    //so add one to the wait counter to fix that
    atomic_add(&main_thread_wait,1);
    //this returns 0 if the queue has only one entry 
    //and that entry is empty
    if(!refill_fileinfo_queue()){
      live_threads=NUM_PROCS-1;
      goto OUT_OF_DATA;
    }
  }
 OUT_OF_DATA:{
    PRINT_MSG("Out of data\n");
    if(live_threads && main_thread_wait==0){
      goto WAIT;
    }
    while(live_threads>0){
      while(atomic_fetch_add(&main_thread_wait,-1)>0 && live_threads){
        futex_spin_lock(&thread_queue_lock);
        worker_thread_id=thread_queue[--thread_queue_index];
        futex_spin_unlock(&thread_queue_lock);
        //I suppose here is where the race condition might actually matter
        //if we actually needed to make sure the thread was waiting
        //use SIGTERM, if the thread isn't already waiting it'll just terminate
        //instead, I assume this will only terminate that thread, but if not
        //I'll set up a signal hanler to allow indivual theads to terminate
        PRINT_FMT("Killing thread %d\n",worker_thread_id);
        tgkill(tgid,thread_pids[worker_thread_id],SIGTERM);
        live_threads--;
      }
    WAIT:
      PRINT_MSG("Waiting for threads\n");
      futex_wait(&main_thread_wait,const_zero_32,NULL);
    }
    PRINT_MSG("All worker threads finished\n")
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
  info=setup_block(fileinfo_queue[fileinfo_queue_index],thread_bufs[thread_id_num]);
//  PRINT_MSG("setup block in setup_thread_args\n");
  thread_file_ids[thread_id_num]=info->file_id;
  thread_buf_start_offsets[thread_id_num]=info->start_offset;
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
      fileinfo_queue[fileinfo_queue_index]=info;
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
  while(fileinfo_queue_index<NUM_PROCS){
    if(current_file<num_files){
      info=setup_fileinfo(filenames[current_file++]);
      fileinfo_queue[fileinfo_queue_index++]=info;
    } else {
//      PRINT_MSG("Finished refilling fileinfo queue\n");
      break;
    }
  }
  if((fileinfo_queue_index?fileinfo_queue_index--:0) ||
     fileinfo_queue[fileinfo_queue_index]->remaining){
    return 1;
  } else {
    return 0;
  }
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
      perror("error reading from file");
      exit(EXIT_FAILURE);
    }
    if(builtin_unlikely(close(info->fd) == -1)){
      perror("error closing file");
      exit(EXIT_FAILURE);
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
    if(builtin_unlikely(nbytes == (ssize_t)-1)){
      perror("error reading from file");
      exit(EXIT_FAILURE);
    }
    info->remaining-=buf_size;
  }
  //this happens independent of weather we read the rest of the
  //file of still have some left, it depends on what was at
  //the end of the last block read from this file
  if(info->word_len){
    PRINT_MSG("Trying to complete incomplete word ");
    fwrite(info->last_word,info->word_len,1,stderr);
    PRINT_MSG("\n");
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
      english_word *word=xmalloc(sizeof(english_word));
      *word=(english_word){.str=(char*)info->last_word,.len=info->word_len,
                           .count=1,.file_bits={.low=0,.high=0}};
      atomic_hash_table_update(word);
      //deals with copying the string in buf and freeing word if needed
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
    perror("error opening file");
    exit(1);
  }
  int stat_retval=fstat(fd,&stat_buf);
  if(builtin_unlikely(stat_retval == -1)){
    perror("error calling stat on file");
    exit(1);
  }
  if(builtin_unlikely(stat_buf.st_size == 0)){
    fprintf(stderr,"Found empty file, %s, no words common to all files\n",
            filename);
    exit(1);
  }
  //this will probably change to using statically allocated memory
  struct fileinfo *info=fileinfo_mem+fileinfo_mem_index;
  fileinfo_mem_index+=1;
  *info=(struct fileinfo){.fd=fd,.len=stat_buf.st_size,.word_len=0,
                          .file_id=next_file_id++,.remaining=stat_buf.st_size};
  return info;
}
//only call with an unused fileinfo, behaves specially for
//files that are less then max_buf_len bytes long
struct fileinfo *init_thread_filestart(struct fileinfo *info,int thread_id_num){
  if(info->remaining < max_buf_size){
    ssize_t nbytes=read(info->fd,thread_bufs[thread_id_num],max_buf_size);
    if(builtin_unlikely(nbytes == (ssize_t)-1)){
      perror("error reading from file");
      exit(EXIT_FAILURE);
    }
    if(builtin_unlikely(close(info->fd) == -1)){
      perror("error closing file");
      exit(EXIT_FAILURE);
    }
    uint8_t *buf=thread_bufs[thread_id_num];
    uint32_t start=nbytes-1;
    if(eng_accept[buf[start]]){
      buf[start+1]=0xff;
    } else {
      while(!eng_accept[buf[--start]]);
      buf[start+1]=0xff;
    }
    thread_file_ids[thread_id]=info->file_id;
    long tid=my_clone(SIMPLE_CLONE_FLAGS,THREAD_STACK_TOP(thread_id_num),
                      thread_pids+thread_id_num,thread_main,(void*)(long)thread_id_num);
    assert((tid == thread_pids[thread_id_num]) &&  tid != 0);
    return NULL;
  } else {
    info=setup_block(info,thread_bufs[thread_id_num]);
    thread_file_ids[thread_id]=info->file_id;
    long tid=my_clone(SIMPLE_CLONE_FLAGS,THREAD_STACK_TOP(thread_id_num),
             thread_pids+thread_id_num,thread_main,(void*)(long)thread_id_num);
    assert((tid == thread_pids[thread_id_num]) && tid!=0);
    return info;
  }
}
struct fileinfo *init_thread(struct fileinfo *info,int thread_id_num){
  info=setup_block(info,thread_bufs[thread_id_num]);
  thread_file_ids[thread_id]=info->file_id;
  my_clone(SIMPLE_CLONE_FLAGS,THREAD_STACK_TOP(thread_id_num),
           thread_pids+thread_id_num,thread_main,(void*)(long)thread_id_num);
  if(info->remaining){
    return info;
  } else {
    return NULL;
  }
}
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
    print_string(common_words.heap[i]);
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
int main(int argc,char *argv[]){
  //remove the program name from the arguments (its just eaiser)
  num_files=argc-1;
  filenames=argv+1;
  if(num_files<=0){
    fprintf(stderr,"Error no filenames given\n");
    exit(1);
  }
  if(num_files>64){
    all_file_bits.low=file_bit_strings[63];
    all_file_bits.high=file_bit_strings[num_files-64];
  } else {
    all_file_bits.low=file_bit_strings[num_files];
  }
  tgid=gettgid();
  //this is used when threads are waiting for more data
  sigaction(SIGTERM,&signal_action,NULL);
  struct fileinfo *info;
  int i;
  int have_data=1;
  if(num_files>=NUM_PROCS-1){
    for(i=1;i<NUM_PROCS;i++){
      info=setup_fileinfo(filenames[current_file++]);
      info=init_thread_filestart(info,i);
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
      info=init_thread_filestart(info,i);
      if(info){
        fileinfo_queue[++fileinfo_queue_index]=info;
      }
      i++;
    }
    if(fileinfo_queue_index>=0){
      do {
        info=fileinfo_queue[fileinfo_queue_index];
        info=init_thread(info,i);
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
  main_wait_loop(have_data);
}
