#include "prog5.h"
/* using downcase is a pretty big hit, but I need to compare the cost of downcase
   vs the cost of strncasecmp, but I'll probably switch back since not
   using downcase should make things easier at the end

   then again maybe not, I call memcmp a lot, it really might be worth it
   to use a slower inline version, since I imagine the cost of calling it
   is quite high. (same goes for strncasecmp if I use that)
*/
struct heap sort_words_2();
void print_results_heap(struct heap heap);
//strings never need to be freed but the ammout of space needed for them
//varies wildly so I allocate space for strings dynamically
//I don't use malloc because it's faster to do it myself

//this is basically the slowest part of my program,
//try mmaping a larger ammount of memory and using
//atomic_fetch_add(string_mem_ptr,size);
static inline void *str_malloc(uint64_t size){
/*  spin_lock_lock(&string_mem_lock);
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
  return retval;*/
  return atomic_fetch_add(&string_mem_pointer,size);
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
#define UPDATE()                                                  \
  atomic_add(&global_hash_table[index]->count,1);               \
  if(low){                                                              \
    atomic_or(&global_hash_table[index]->file_bits.low,word->file_bits.low); \
  } else {                                                              \
    atomic_or(&global_hash_table[index]->file_bits.high,word->file_bits.high); \
  }                                                                     \
  goto end0;
static int atomic_hash_table_update(english_word *word){
  uint64_t hashv=fnv_hash(word->str,word->len);
  uint64_t index=hashv%global_hash_table_size;
  int low=(word->file_bits.low?1:0);
  uint8_t *mem=NULL;//memory for copying strings
  //this next line results in a lot of cache misses for obvious reasons
  while(1){
    while(global_hash_table[index]){
      if(string_compare(global_hash_table[index],word)){
        UPDATE();
      }
      index++;
    }
    if(!mem){//insures we only allocate memory for a string once
      mem=str_malloc(word->len);
      word->str=(char*)my_strcpy(mem,(uint8_t*)word->str,word->len);
    }
    void *prev=global_hash_table[index];
    if(prev){
      if(string_compare(global_hash_table[index],word)){
        UPDATE();
      }
      continue;
    }
    int test=atomic_compare_exchange_n(global_hash_table+index,&prev,word);
    if(test){
      uint64_t old_indices_index=atomic_fetch_add(&indices_index,1);
//      atomic_store_n(hash_table_indices+old_indices_index,index);
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
static int atomic_hash_table_update(english_word *word){
  uint64_t hashv=fnv_hash(word->str,word->len);
  uint64_t index=hashv%global_hash_table_size;
  int low=(word->file_bits.low?1:0);
  uint8_t *mem=NULL;//memory for copying strings
  //this next line results in a lot of cache misses for obvious reasons
  if(!global_hash_table[index]){//word isn't in the hash table, add it
    mem=str_malloc(word->len);
    word->str=(char*)my_strcpy(mem,(uint8_t*)word->str,word->len);
    void *prev=global_hash_table[index];
    if(prev){
      if(string_compare(global_hash_table[index],word)){
        goto UPDATE;
      }
      goto LOOP;
    }
    int test=atomic_compare_exchange_n(global_hash_table+index,&prev,word);
    if(test){
      //we added the word
      //this needs to be atomic to prevent two threads writing different
      //values to the same index of indices
      uint64_t old_indices_index=atomic_fetch_add(&indices_index,1);
      //this doesn't need to be atomic, since indices_index will never be
      //decremented, so no one else will change this
      atomic_store_n(hash_table_indices+old_indices_index,index);
      goto end1;
    }
    //else, someone else changed the value of global_hash_table[index] before us
  }
  while(1){//loop untill we find a free spot or an existing copy of word
    LOOP:
    do {
      //see if the value in the table is the same as our value
      //if so update the value already in the table
      if(string_compare(global_hash_table[index],word)){
      UPDATE:
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
    if(!mem){//insures we only allocate memory for a string once
      mem=str_malloc(word->len);
      word->str=(char*)my_strcpy(mem,(uint8_t*)word->str,word->len);
    }
    void *prev=global_hash_table[index];
    if(prev){
      if(string_compare(global_hash_table[index],word)){
        goto UPDATE;
      }
      continue;
    }
    int test=atomic_compare_exchange_n(global_hash_table+index,&prev,word);
    if(test){
      uint64_t old_indices_index=atomic_fetch_add(&indices_index,1);
      atomic_store_n(hash_table_indices+old_indices_index,index);
//      hash_table_indices[old_indices_index]=index;
      goto end1;
    }
    //if !test the compare exchange failed and we need to keep looping
  }
 end0:
  return 0;
 end1:
  return 1;
}*/
/*
  main work function, searches buf for english words (matching [a-za-z]{6,50})
  and puts/updates the word in the global hash table.
*/

void *parse_buf(register const uint8_t *buf_,int file_id,void *mem){
  //we know we're going to need buf for awhile, and we need to
  //save it across function calls so put it in rbx
  register const uint8_t *buf __asm__ ("%rbx")=buf_;
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
  //I should unroll the loop to be as large as it can with jmp offsets
  //still fitting in a byte
 START:
  if(eng_accept[*(buf)]){goto ACCEPT_0;}
  if(eng_accept[*(buf+1)]){goto ACCEPT_1;}
  if(eng_accept[*(buf+2)]){goto ACCEPT_2;}
  if(eng_accept[*(buf+3)]){goto ACCEPT_3;}
  buf+=4;
  goto START;
  /*  if(eng_accept[*(buf+4)]){goto ACCEPT_4;}
  if(eng_accept[*(buf+5)]){goto ACCEPT_5;}
  if(eng_accept[*(buf+6)]){goto ACCEPT_6;}
  if(eng_accept[*(buf+7)]){goto ACCEPT_7;}
  buf+=4;
  goto START;
 ACCEPT_7:
  buf++;
 ACCEPT_6:
  buf++;
 ACCEPT_5:
  buf++;
 ACCEPT_4:
 buf++;*/
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
  /*  if(eng_accept[*(buf+index+4)]){goto REJECT_4;}
  if(eng_accept[*(buf+index+5)]){goto REJECT_5;}
  if(eng_accept[*(buf+index+6)]){goto REJECT_6;}
  if(eng_accept[*(buf+index+7)]){goto REJECT_7;}
  buf+=4;
  goto ACCEPT_0;
 REJECT_7:
 index++;
 REJECT_6:
 index++;
 REJECT_5:
 index++;
 REJECT_4:
 index++;*/
 REJECT_3:
  index++;
 REJECT_2:
  index++;
 REJECT_1:
  index++;
 REJECT_0:
  if(index >= 6 && index <= 50){
    downcase_short((char*)buf,index);
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
    goto PREFETCH;//yes I am using goto to implement a loop here
    //I almost never do this but in this case it just seemed to fit
  }
  //I used to free the buffer here, but now buffers are statically allocated
  return mem;
}
//main function for worker threads
void thread_main(void *arg){
  thread_id=(uint64_t)(arg);
  thread_mem_pointer=thread_mem_block+thread_id;
  while(1){
    //do work
    thread_mem_pointer=
      parse_buf(thread_bufs[thread_id] +
                thread_fileinfo_vals[thread_id].start_offset,
                thread_fileinfo_vals[thread_id].file_id,thread_mem_pointer);
    keep_alive[thread_id]=0;
    //tell main thread we're done
    spin_lock_lock(&thread_queue_lock);
    thread_queue[thread_queue_index++]=thread_id;
    spin_lock_unlock(&thread_queue_lock);
    if(builtin_unlikely(sem_post(&main_thread_waiters)!=0)){
      PROGRAM_ERROR(warn("sem_post failure in worker thread %ld\n",thread_id));
    }
    //wait for main thread to give us more data
    if(builtin_unlikely(sem_wait(thread_semaphores+thread_id)!=0)){
      PROGRAM_ERROR(warn("sem_wait failure in worker thread %ld\n",thread_id));
    }
    if(!keep_alive[thread_id]){
      goto EXIT;
    }
  }
 EXIT:  
  atomic_add(&live_threads,-1);
  //doesn't matter if this gets bigger than the number of threads at this point
  //since the main thread uses live_threads to test if all threads are done
  sem_post(&main_thread_waiters);
  pthread_exit(EXIT_SUCCESS);
}
//main function for main/controlling thread
void main_wait_loop(int have_data){
  uint8_t worker_thread_id=0;
  uint32_t out_of_data_local=0;
  if(!have_data){goto OUT_OF_DATA;}
  while(1){
    if(!sem_wait(&main_thread_waiters)){
      PRINT_FMT("locking in main\n");
      spin_lock_lock(&thread_queue_lock);
      worker_thread_id=thread_queue[--thread_queue_index];
      out_of_data_local=(!setup_thread_args(worker_thread_id));
      spin_lock_unlock(&thread_queue_lock);
      PRINT_FMT("unlocked in main\n");
      /*The issue is here, when setup_thread_args returns 0 it means that 
        there's no data left, but arguments for a thread have already been 
        setup. So I need to make sure I process this last set of data
        before I finish, and also I need to make sure to decrement the 
        count of live threads when the thread finishes, THIS was the
        source of my problems with joining threads (probably)
      */
      atomic_store_n(keep_alive+worker_thread_id,1);
      if(sem_post(thread_semaphores+worker_thread_id)!=0){
        PROGRAM_ERROR(perror("sem_post failure\n"));
      }
      if(out_of_data_local){
        PRINT_MSG("going to OUT_OF_DATA because out_of_data_local\n");
        goto OUT_OF_DATA;
      }
    } else {
      PROGRAM_ERROR(perror("Error in sem_wait"));
    }
  }
  //refill_fileinfo_queue goes here if used
 OUT_OF_DATA:{
    PRINT_MSG("Out of data\n");
    int i;
    /*should this be 1 or 2? I think a 1*/
    while(live_threads){
      for(i=0;i<NUM_PROCS-1;i++){
        //wake any waiting worker thread
        sem_post(thread_semaphores+i);
      }
      while(sem_wait(&main_thread_waiters)){
        if(atomic_load_n(&live_threads)==0){break;}
      }
    }
    PRINT_MSG("All worker threads finished\n");
    __asm__ volatile("mfence");
    spin_lock_lock(&thread_queue_lock);    
    struct heap common_words=sort_words();
    print_results_heap(common_words);
    exit(EXIT_SUCCESS);
  }
}
//setup the buffer and file_id for the thread with thread_id
//then do the minimal ammount of work possible to insure
//that there is at least one entry in the fileinfo queue
int setup_thread_args(int thread_id_num){
  //nothing should ever leave fileinfo_queue completely empty,
  //fileinfo_queue[0] should always contain a vaild struct fileinfo
  //on entry to this function
  static struct fileinfo *info;
  if(builtin_unlikely(fileinfo_queue_index<0)){
    PROGRAM_ERROR(fprintf(stderr,"Invalid value for fileinfo_queue_index\n"));
  }
  info=setup_block(fileinfo_queue[fileinfo_queue_index],thread_bufs[thread_id_num]);
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
  __builtin_unreachable();//tell gcc that this is a dead end
}
//fill the fileinfo queue as much as is possible. The idea is that
//this should get called when there are no threads waiting for data
//so that data can be given quickly when threads need it
//currently unused
int refill_fileinfo_queue(){
  static struct fileinfo *info;
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
  //there are two size parameters, buf_size and min_buf_size,
  //max_buf_size is just buf_size+min_buf_size. How this works is that we
  //never process a block less then min_buf_size(unless it's a whole file).
  //By default we process buf_size bytes, but if that would leave less than
  //min_bytes left we just process the rest of the file
  if(max_buf_size>info->remaining){
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
  //this depends on what was at the end of the last block read from this file
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
    if(info->word_len>=6){
      downcase_short((char*)border_word_mem_ptr,info->word_len);
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
  //this fills the last_word buf with an incomplete word from the end of
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
//simple little helper function to print ordinal suffixes
static inline char* __attribute__((const)) ordinal_suffix(uint32_t num){
  if(num == 1){return "st";}
  if(num == 2){return "nd";}
  if(num == 3){return "rd";}
  return "th";
}
//Opens the file <filename> and returns a struct fileinfo
//containing the file descriptor, file size, internal file_id and
//some other fields used elsewhere
struct fileinfo *setup_fileinfo(char *filename){
  static struct stat stat_buf;
  int fd=open(filename,O_RDONLY);
  if(builtin_unlikely(fd == -1)){
    PROGRAM_ERROR(warn("error opening file %s",filename));
  }
  int stat_retval=fstat(fd,&stat_buf);
  if(builtin_unlikely(stat_retval == -1)){
    PROGRAM_ERROR(warn("error calling stat on file %s",filename));
  }
  if(builtin_unlikely(stat_buf.st_size == 0)){
    PROGRAM_ERROR
      (fprintf(stderr,"Found empty file, %s, no words common to all files\n",
               filename));
  }
  struct fileinfo *info=fileinfo_mem+fileinfo_mem_index;
  fileinfo_mem_index+=1;
  *info=(struct fileinfo){.fd=fd,.len=stat_buf.st_size,.word_len=0,
                          .file_id=next_file_id++,.remaining=stat_buf.st_size};
  return info;
}
#include "prog5_heap.c"
//creates a thread with thread_id_num and starts it processing data
//determined by info
struct fileinfo *init_thread(struct fileinfo *info,int thread_id_num){
  info=setup_block(info,thread_bufs[thread_id_num]);
  thread_fileinfo_vals[thread_id_num].file_id=info->file_id;
  thread_fileinfo_vals[thread_id_num].start_offset=0;
  assert(thread_attrs+thread_id_num);
  size_t stacksz;
  pthread_attr_getstacksize(&default_thread_attr,&stacksz);
  PRINT_FMT("Attr stack size = %ld \n",stacksz);
  if((errno=pthread_create((pthread_t*)(pthread_thread_ids+thread_id_num),
                           &default_thread_attr,
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
//print out the most common words, needs a bit of work
//currently unused
void print_results(struct heap common_words){
  if(!is_sorted(common_words.heap,common_words.size)){
    printf("Failed to sort the heap\n");
  } else {
    printf("Sorted the heap\n");
  }
  int i;
  uint32_t size=common_words.size-1;
  //  for(i=size-1;i>=size-30;i--){
  for(i=size;i>size-30;i--){
    //We can't use the %s format specifier of printf to print the actual word
    //because its not null terminated
    printf("The %d%s most common word was ",(size-i)+1,ordinal_suffix(size-i));
    print_word(common_words.heap[i]);
    printf(", with %d occurances\n",common_words.heap[i]->count);
  }
}
int main(int argc,char *argv[]){
  struct fileinfo *info;
  int i,have_data=1;
  //remove the program name from the arguments (its just eaiser)
  num_files=argc-1;
  PRINT_FMT("Given %ld files\n",num_files);
  filenames=argv+1;
  if(num_files<=0){
    PROGRAM_ERROR(fprintf(stderr,"Error no filenames given\n"));
  }
//  tgid=gettgid();//probably don't need this anymore
//  string_mem_pointer=mmap(NULL,(8*(1<<20)),PROT_READ|PROT_WRITE,
//                          MAP_PRIVATE|MAP_ANONYMOUS,-1,0);
//  string_mem_end=string_mem_pointer+(8*(1<<20));
  if(builtin_unlikely(string_mem_pointer==MAP_FAILED)){
    PROGRAM_ERROR(warn("mmap failed %d",errno));
  }
    //inilialize phread attrs
  pthread_attr_t default_thread_attr;
  pthread_attr_init(&default_thread_attr);
  pthread_attr_setdetachstate(&default_thread_attr,PTHREAD_CREATE_DETACHED);
  pthread_attr_setstacksize(&default_thread_attr,(2<<15));
    size_t stacksz;
  pthread_attr_getstacksize(&default_thread_attr,&stacksz);
  for(i=0;i<NUM_PROCS;i++){
    sem_init(thread_semaphores+i,0,0);
  }
  //special case for 1 file
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
      //more files then processors, just start each thread on it's own file
      for(i=1;i<NUM_PROCS;i++){
        info=setup_fileinfo(filenames[current_file++]);
        info=init_thread(info,i-1);
        live_threads++;
        if(info){
          fileinfo_queue[++fileinfo_queue_index]=info;
        }
      }
      if(fileinfo_queue_index==-1){
        //if all the files were less then the block size we get here
        if(current_file<num_files){
          //if there are files left the no problem
          fileinfo_queue[++fileinfo_queue_index]=
            setup_fileinfo(filenames[current_file++]);
        } else {
          //otherwise we have no more data left to process
          have_data=0;
        }
      }
    } else {
      //more processors than files
      int i=1;
      while(current_file<num_files){
        //start a thread for each file
        info=setup_fileinfo(filenames[current_file++]);
        info=init_thread(info,i-1);
        live_threads++;
        if(info){
          fileinfo_queue[++fileinfo_queue_index]=info;
        }
        i++;
      }
      if(fileinfo_queue_index>=0){
        //if there is data left to process start threads unill
        //we have NUM_PROCS threads or no data left to process
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
  __builtin_unreachable();
}
