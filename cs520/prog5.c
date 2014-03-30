#include "prog5.h"
/*
   Update the value of word in the global hash table, this means that if word
   isn't in the table then we should add it, and if it is we should increment the
   count on the value in the table and update the file index based on the
   set bit in the file index of word
*/
//this version uses locking to guarantee atomic access to the hash table
//currently the locking is done using a very small spin lock, though
//it might be best to use locks with waiting, I'll need to profile to be sure
//but it doesn't really matter since I'm probably not going to use this version
/*static english_word* locking_hash_table_update(english_word *word){
  uint64_t hashv=fnv_hash(word->str,word->len);
  uint64_t index=hashv%global_hash_table_size;
  int low=(word->file_bits.low?1:0);
  futex_spin_lock(&global_futex_lock);
  if(!global_hash_table[index]){//word isn't in the hash table, add it
    global_hash_table[index]=word;
  } else {
    do {
      //word is in the hash table, update it
      if(string_compare(global_hash_table[index],word)){
        global_hash_table[index]->count+=1;
        if(low){
          global_hash_table[index]->file_bits.low|=word->file_bits.low;
        } else {
          global_hash_table[index]->file_bits.high|=word->file_bits.high;
        }
        goto END;
        xfree(word);
      }
    } while(global_hash_table[++index]);
    //not in the table use next free index
    global_hash_table[index]=word;
  }
 END:
  futex_spin_unlock(&global_futex_lock);
  return word;
}*/

/*
  same as above but uses atomic operations instead of locking, which
  makes everything much faster, since we can have concurrent access to
  the hash table unlike with locking. (i.e in this version multiple
  threads can access different parts of the hash table all at once
  whereas with locking one thread blocks all others even those accessing
  a completely different part of the table
*/
static english_word* atomic_hash_table_update(english_word *word){
  uint64_t hashv=fnv_hash(word->str,word->len);
  uint64_t index=hashv%global_hash_table_size;
  int low=(word->file_bits.low?1:0);
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
      goto end;
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
        xfree(word);
        goto end;
      }
    } while(global_hash_table[++index]);
    //not in the table use next free index (if we can)
    void *prev=global_hash_table[index];
    int test=atomic_compare_exchange_n(global_hash_table+index,&prev,word);
    if(test){
      uint64_t old_indices_index=atomic_fetch_add(&indices_index,1);
      hash_table_indices[old_indices_index]=index;
      goto end;
    }
    //if !test the compare exchange failed and we need to keep looping
  }
 end:
  return word;
}
//i can't use this ...
/*
void *mmap_file(char *filename){
  int fd=open(filename,O_RDONLY);
  if(fd == -1){
    perror("error opening file");
    exit(1);
  }
  off_t len=file_size(fd);
  uint8_t *buf=mmap(NULL,PAGE_ROUND_UP(len),);
}*/
struct fileinfo open_file_simple(char *filename){
  long fd=open(filename,O_RDONLY);
  if(fd == -1){
    perror("error opening file");
    exit(1);
  }
  off_t len=lseek(fd,0,SEEK_END);
  lseek(fd,0,SEEK_SET);
  if(len == (off_t)-1){
    perror("error seeking file");
    exit(1);
  }
  return (struct fileinfo){.fd=fd,.len=len};
}

struct filebuf read_full_file(char *filename){
  long fd=open(filename,O_RDONLY);
  if(fd == -1){
    perror("error opening file");
    exit(1);
  }
  off_t len=lseek(fd,0,SEEK_END);
  lseek(fd,0,SEEK_SET);
  if(len == (off_t)-1){
    perror("error seeking file");
    exit(1);
  }
  uint8_t *buf=xmalloc(len);

  ssize_t nbytes=read(fd,buf,len);
  if(nbytes == (ssize_t)-1 /*|| nbytes != len*/){
    perror("error reading from file");
    exit(EXIT_FAILURE);
  }
  if(close(fd) == -1){
    perror("error closing file");
    exit(EXIT_FAILURE);
  }
  uint32_t end=len-1;
  while(!eng_accept[buf[--end]]);
  buf[end+1]=0xff;
  return (struct filebuf){.buf=buf,.len=end};
}
#if 0
char** open_files(char **filenames,uint32_t num_files,uint32_t *bufs){
  char *filename;
  int i=0,j=0;
  //guess on average each file is 2 pages
  char **retval=xmalloc(num_files*2*sizeof(filebuf));
  while(i<num_files){
    filename=filenames[i];
  }
  ssize_t nbytes=read(fd,buf,len);
  if(nbytes == (ssize_t)-1 /*|| nbytes != len*/){
    perror("error reading from file");
    exit(exit_failure);
  }
  if(close(fd) == -1){
    perror("error closing file");
    exit(exit_failure);
  }
  if(len > PAGESIZE){
    uint32_t offset=0;
    while(offset<len){
      *retval[j++]=buf+offset;
      offset++;
      //      if(j
    }
  }
}
#endif
/*
   main work function, searches buf for english words (matching [a-za-z]{6,50})
   and puts/updates the word in the global hash table.
   frees the buffer it's given once it's done;
   todo: this need to take another parameter specifing the file index
 */
void parse_buf(register const uint8_t *buf){
  const uint8_t *initial_buf=buf;
  uint64_t index=1;
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
    /*//what used to do, really slow because I copy every word
    void *mem=xmalloc(sizeof(english_word)+index);
    my_strcpy(mem+sizeof(english_word),buf,index);
    english_word *word=mem;
    //need to modify this function to pass the file index to put
    //into this
    *word=(english_word){.str=mem+sizeof(english_word),.len=index,
                         .count=1,.file_bits={.low=0,.high=0}};*/
    english_word *word=xmalloc(sizeof(english_word));
    *word=(english_word){.str=(char*)buf,.len=index,.count=1,
                         .file_bits={.low=0,.high=0}};
    /* english_word word={.str=(char*)buf,.len=index,.count=1,
       .file_bits={.low=0,.high=0}};
       atomic_hash_table_update(word);
     */
    atomic_hash_table_update(word);//frees word if it's not needed
  }
  if(buf[index]!=0xff){
    buf+=index;
    index=1;
    goto START;
  }
  //no one else should need this buffer after us
  xfree((void*)initial_buf);
}

void thread_main(void *arg){
  int signo;
  thread_id=(uint64_t)(arg);//once
  while(1){
    parse_buf(thread_bufs[thread_id]);//do stuff

    //tell main thread we're done
    __atomic_store_n(thread_status+thread_id,0,__ATOMIC_SEQ_CST);
    futex_spin_lock(&thread_queue_lock);
    thread_queue[thread_queue_index++]=thread_id;
    futex_wake(&main_thread_wait,1);
    futex_spin_unlock(&thread_queue_lock);

    //wait for main thread to give us more data
    /* There is a highly unlikely race condition to prevent here
       if the main thread signals us before we start waiting we'd
       end up waiting forever. However the main thread has to refill
       our buffer (entailing a syscall to read), and clean up the end of
       the buffer if necessary before it signals us. So the odds of us
       getting signaled before we start waiting are really low.
       I'll put in code to prevent this, but if it makes a performance
       difference (which it shouldn't) I'll take it out
     */
    sigwait(&sigwait_set,&signo);
    __atomic_store_n(thread_status+thread_id,1,__ATOMIC_SEQ_CST);//this prevents the race contition
    //the main thread runs a loop while(!thread_status[worker_thread_id]){
    //tgkill(-1,thread_pids[worker_thread_id],<SIGNAL>);}
    if(signo == SIGCHLD){
      thread_exit(EXIT_SUCCESS);
    }
  }
}

void main_wait_loop(){
  uint8_t worker_thread_id;
  while(/*there is data left to process*/ 1){
    //this might be a race conditon, I think so, but I think I know how to 
    //fix it, the way it is now a thread might call futex wake before
    //this thread starts waiting. I should be able to fixe htis by using the
    //futex as a counter which gets atomically incremented/decremunted
    int val = -1;
    val=atomic_add(&val,main_thread_wait);
    //wait untill a thread is done;
    futex_wait(&main_thread_wait,val);
    while(__atomic_load_n(&thread_queue_index,__ATOMIC_SEQ_CST) > 0){
      futex_spin_lock(&thread_queue_lock);
      worker_thread_id=thread_queue[thread_queue_index--];
      futex_spin_unlock(&thread_queue_lock);
      //I still need to figure these out
      //      somehow_fill_block(thread_bufs[worker_thread_id]);
      //MAGIC GOES HERE
      setup_block(thread_bufs[worker_thread_id],buf_size);
      while(!(__atomic_load_n(thread_status+worker_thread_id,__ATOMIC_SEQ_CST))){
        tgkill(-1,thread_pids[worker_thread_id],SIGCONT);
        __asm__ volatile("pause\n\tpause\n");//give the worker thread time to set
        //its staus
      }
    }
  }
  int live_threads=NUM_PROCS-1;
  while(live_threads){
    while(thread_queue_index > 0){
      futex_spin_lock(&thread_queue_lock);
      worker_thread_id=thread_queue[thread_queue_index--];
      futex_spin_unlock(&thread_queue_lock);
      //I suppose here is where the race condition might actually matter
      //if we actually needed to make sure the thread was waiting
      //use SIGTERM, if the thread isn't already waiting it'll just terminate
      //instead, I assume this will only terminate that thread, but if not
      //I'll set up a signal hanler to allow indivual theads to terminate
      tgkill(-1,thread_pids[worker_thread_id],SIGTERM);
      live_threads--;
    }
    int val = -1;
    val=atomic_add(&val,main_thread_wait);
    //wait untill another thread is done;
    futex_wait(&main_thread_wait,val);
  }
  struct heap common_words=sort_words();
  //print stuff
  //actually call the libc exit to insure everything gets cleaned up
  //and all the threads actually die
  exit(EXIT_SUCCESS);
}
//this should be merged into another function, it's only a function
//now so I have a place to put this code
void init_threads(){
  //assumes all thread buffers are already setup
  int live_threads=1;
  int next_thread_id=1;
  while(live_threads<NUM_PROCS){
    my_clone(SIMPLE_CLONE_FLAGS,THREAD_STACK_TOP(next_thread_id),
             thread_pids+next_thread_id,thread_main,(void*)(long)next_thread_id);
    live_threads++;
    next_thread_id++;
  }
}
/*
  setup a block of memory to be processed by parse_buf.
  if block[block_size] isn't a word character scan backwards until a word
  character is found and mark the next character as eof (i.e insure the block
  ends immediately after a word character). if block[block_size] is a word
  character scan forward/backward to find the bounds of it, and if it's
  within size limits put/update it in the hash table. following this do the same
  as above to mark the end of the block with eof.

  the core of this function is good, the actual interface to it needs to be
  reworked once i decide how exactly i'm going to do everything.
 */
struct filebuf setup_block(uint8_t *block,uint32_t block_size){
  //start is start of last eng word, it will ultimately
  //refer to the end of the block we're returning
  uint32_t start=block_size,end=block_size;
  if(eng_accept[block[block_size]]){
    while(eng_accept[block[--start]]);//find start of word
    while(eng_accept[block[++end]]);//find end of word
    uint32_t len=end-start;
    if(len>=6 && len<=50){
      void *mem=xmalloc(sizeof(english_word)+len);
      my_strcpy(mem+sizeof(english_word),block-(start+1),len);
      english_word *word=mem;
      //need to modify this function to pass the file index to put
      //into this
      *word=(english_word){.str=mem+sizeof(english_word),.len=len,
                           .count=0,.file_bits={.low=0,.high=0}};
      atomic_hash_table_update(word);//frees word if it's not needed
    }
  }
  //mark end of block with eof
  while(!eng_accept[block[--start]]);
  block[start+1]=0xff;
  return (struct filebuf){.buf=block,.len=start};
}

//auxiliary routines for sorting
#define heap_left_child(i) (2*i+1)
#define heap_right_child(i) (2*i+2)
#define heap_parent(i) (i?(i-1)/2:0)
static english_word** heap_sort(english_word **heap, uint32_t size);
static void sift_down(english_word **heap, uint32_t start, uint32_t end);
static void heapify(english_word **heap, uint32_t index, uint32_t heap_length);
static english_word *heap_pop(english_word **heap,uint32_t heap_length);

static inline void heap_insert(english_word **heap,english_word *new_element,
                               uint32_t heap_index){
  heap[heap_index]=new_element;
  //will always terminate when heap_index = 0 because heap_parent returns 0
  //when given 0
  while(heap[heap_index]->count > heap[heap_parent(heap_index)]->count){
    SWAP(heap[heap_index],heap[heap_parent(heap_index)]);
    heap_index=heap_parent(heap_index);
  }
}
static inline void heapify(english_word **heap,uint32_t index,
                           uint32_t heap_length){
  uint32_t left,right,cur_max;
  while(index<heap_length){
    left=heap_left_child(index);
    right=heap_right_child(index);
    cur_max=index;
    if(heap[left]->count > heap[cur_max]->count){
      cur_max=left;
    }
    if(heap[right]->count > heap[cur_max]->count){
      cur_max=right;
    }
    if(cur_max == index){
      break;
    }
    SWAP(heap[index],heap[cur_max]);
    index=cur_max;
  }
}
//this comes from the psudeocode on the wikipedia heapsort artical
static inline void sift_down(english_word **heap,uint32_t start,uint32_t end){
  uint32_t root=start,child,swap;
  while((child=heap_left_child(root)) <= end){
    swap=root;
    //is left child bigger?
    if(heap[child]->count > heap[swap]->count  ){
      swap=child;
    }
    //is right child bigger?
    if(child+1<=end && heap[child+1]->count > heap[swap]->count){
      swap=child+1;
    }
    if(swap!=root){
      SWAP(heap[root],heap[swap]);
      root=swap;
    } else {
      return;
    }
  }
}

static inline english_word *heap_pop(english_word **heap,uint32_t heap_length){
  english_word *retval=*heap;
  heap[0]=heap[heap_length-1];
  heapify(heap,0,heap_length-2);
  return retval;
}
//this is pretty inefficent, but it should work for now
static inline int heap_swap_min(english_word **heap,english_word *new){
  int i,heap_index=15,min_count[2]={heap[15]->count,heap[15]->count};
  //find the current smallest value
  for(i=16;i<=30;i++){
    if(heap[i]->count < min_count[1]){
      min_count[0]=min_count[1];
      min_count[1]=heap[i]->count;
      heap_index=i;
    }
  }
  heap[heap_index]=new;
  while(heap[heap_index]->count > heap[heap_parent(heap_index)]->count){
    SWAP(heap[heap_index],heap[heap_parent(heap_index)]);
    heap_index=heap_parent(heap_index);
  }
  return min_count[0];
}

//no args because all the data used is global
struct heap sort_words(){
  //maybe this should be thread local
  //this is 32 instead of 20 so we have a full 5 level binary tree
  //which needs 31 blocks and 32 is a much nicer value than 31
  static english_word *most_common[32];
  uint32_t i=0,j=0,minimum=-1;
  //first get twenty words used in every file, if there are less then twenty
  //then this is all we need to do
  PRINT_MSG("start of sort_words\n");
  while(i<31 && j<indices_index){
//    PRINT_FMT("Loop %d\n",j);
    english_word *cur_word=global_hash_table[hash_table_indices[j++]];
//    if(cur_word->file_bits.uint128 == all_file_bits.uint128){
      if(cur_word->count < minimum){
        minimum=cur_word->count;
      }
      heap_insert(most_common,cur_word,i);
      i++;
//    }
  }
  if(i<20){
    PRINT_MSG("Less than twenty words found\n");
    return (struct heap){.heap=heap_sort(most_common,i),.size=i};
  }
  PRINT_MSG("added initial 20 words\n");
  i=30;
  while(j<indices_index){
    english_word *cur_word=global_hash_table[hash_table_indices[j++]];
    if(cur_word->file_bits.uint128 == all_file_bits.uint128){
      if(cur_word->count > minimum){
        minimum=heap_swap_min(most_common,cur_word);
      }
    }
  }
  PRINT_MSG("added all words\n");
  i=31;
  return (struct heap){.heap=heap_sort(most_common,i),.size=i};
}
struct heap sort_words_2(){
  //maybe this should be thread local
  //this is 32 instead of 20 so we have a full 5 level binary tree
  //which needs 31 blocks and 32 is a much nicer value than 31
  english_word **most_common=xmalloc(indices_index*sizeof(english_word*));
  uint32_t i=0,j=0;
  //first get twenty words used in every file, if there are less then twenty
  //then this is all we need to do
  PRINT_MSG("start of sort_words\n");
  while(j<indices_index){
    english_word *cur_word=global_hash_table[hash_table_indices[j++]];
//    if(cur_word->file_bits.uint128 == all_file_bits.uint128){
    heap_insert(most_common,cur_word,i);
    i++;
//    }
  }
  return (struct heap){.heap=heap_sort(most_common,i),.size=i};
}
//works by modifying memory, but returns a value for convience
static inline english_word** heap_sort(english_word **heap,uint32_t size){
  PRINT_MSG("Running heapsort\n");
  uint32_t end=size-1;
  while(end>0){
    SWAP(heap[end],heap[0]);
    end--;
    sift_down(heap,0,end);
  }
  return heap;
}
static int is_sorted(english_word **arr,uint32_t size){
  uint32_t i;
  uint32_t count=-1;
  for(i=size-1;i>0;i--){
    if(arr[i]->count > count){
      return 0;
    } else {
      count=arr[i]->count;
    }
  }
  return 1;
}

/* Assembly for futex routines
   the futex stuff will probably only be used for inter thread
   communication (i.e passing arguments/indicating a thread
   needs more data,etc) since my hash table is done using
   atomic opperations
*/
/*these next two functions will be useful for using futexs
  for things other than basic locks
*/

//interfate to the raw syscall
//check that *uadder==val then wait until a FUTEX_WAKE
int futex_wait(int *uaddr,int val){//ignore timeout argument
  register int retval __asm__ ("%rax");
  __asm__("movl %1,%%edx\n\t"//move val to rdx (because it's the third arg
          "movq %2,%%rsi\n\t"
          "movq %3,%%rax\n\t"
          "xorq %%rcx,%%rcx\n\t"
          "lock subq $1,(%%edi)\n\t"
          //hopefully if another thread adds to the futex before
          //the kernel makes us wait the syscall return
          //immediately
          "syscall\n"
          : "=r" (retval)
          : "r" (val), "i" (FUTEX_WAIT), "i" (__NR_futex));
  return retval;
}
//interfate to the raw syscall
//wake up upto val processes waiting on the futex at uaddr
int futex_wake(int *uaddr,int val){//timeout ignored by syscall
  register int retval __asm__ ("%rax");
  __asm__("movl %1,%%edx\n\t"//move val to rdx (because it's the third arg
          "movq %2,%%rsi\n\t"
          "movq %3,%%rax\n\t"
          "lock addq $1,(%%rdi)\n\t"
          "syscall\n"
          : "=r" (retval)
          : "r" (val), "i" (FUTEX_WAKE), "i" (__NR_futex));
  return retval;
}
//futexs 1=free, 0=locked, no waiters -1=locked, waiters
//futex_up and futex_down implement the locking mechanism described
//int the futex(7) man page
__asm__("\n.macro ENTRY name:req\n\t"
        ".globl \\name;\n\t"
        ".type \\name,@function;\n\t"
        ".p2align 4\n\t"
        "\\name\\():\n\t"
        ".cfi_startproc\n"
        ".endm\n"
        ".macro END name:req\n\t"
        ".cfi_endproc\n\t"
        ".size \\name, .-\\name\n"
        ".endm\n\n"

        //essentially futex unlock
        "ENTRY futex_up\n\t"
        "lock addq $1,(%rdi)\n\t"
        "testq $1,(%rdi)\n\t"
        //assume non contested case is most common, only jmp
        //if contested
        "jne 1f\n\t"
        "retq\n"
        "1:\n\t"
        //if we get here it's contested, so setup futex syscall
        "movq $1,(%rdi)\n\t"
        "movq $0,%rsi\n\t"
        "movq $1,%rdx\n\t"
        "movq $202, %rax\n\t" /*put futex syscall no in rax*/
        "syscall\n\t"
        "retq\n"
        "END futex_up\n\n"

        //essentially futex lock
        "ENTRY futex_down\n\t"
        "lock subq $1,(%rdi)\n\t"
        "cmpq $0,(%rdi)\n\t"
        //same idea as with futex_up, assume non contested is most common
        "jne 1f\n\t"
        "retq\n"
        "1:\n\t"
        //if we get here it's contested, so setup futex syscall
        "movq $-1,(%rdi)\n\t"
        "movq $-1,%rdx\n\t"
        "movq $0,%rsi\n\t"//this is FUTEX_WAIT
        "xorq %rcx,%rcx\n\t"
        "movq $202,%rax\n\t"
        "syscall\n\t"
        "retq\n"
        "END futex_down\n\n"

        //unlock spin lock
        "ENTRY futex_spin_up\n\t"
        //much eaiser, we do the same thing no matter what
        "movq $1,%rax\n\t"
        "lock xchg %rax,(%rdi)\n"
        "END futex_spin_up\n\n"

        //lock spin lock
        "ENTRY futex_spin_down\n\t"
        "movq $1,%rax\n\t"
        "xorq %rcx,%rcx\n"
        "1:\n\t"//start spining
        "cmpq %rax,%rdi\n\t"//is the lock free?
        "je 2f\n\t"//if yes try to get it
        "pause\n\t"//if no pause
        "jmp 1b\n"//spin again
        "2:\n\t"
        "lock cmpxchgq %rcx,(%rdi)\n\t"//try to get lock
        "jnz 1b\n"//if we failed spin again
        "END futex_spin_down\n\n"
        /*This assumes that the cost of aligning memory before copying
          outweighs the benifit for 6-50 byte strings (also that
          rep movsb is fast)
        "ENTRY my_strcpy\n\t"
        "movq %rdx,%rcx\n\t"//move length to rcx
        "movq %rdi,%rax\n\t"//save return value
        "rep movsb\n\t"//actually copy
        "retq\n"
        "END my_strcpy\n"*/);
//would be eaiser to just do a mov and a ret but
//then gcc will complain about returning without a value or something
//clone returns 0 for new child and child tid for parent
long clone_syscall(unsigned long flags,void *child_stack,void *ptid,
                   void *ctid,struct pt_regs *regs){
  register long retval __asm__ ("%rax");
  __asm__("movq %1,%%rax\n\t"
          "syscall\n"
          : "=r" (retval) : "i" (__NR_clone));
  return retval;
}
long my_clone(unsigned long flags,void *child_stack,void *ptid,
              void (*fn)(void*),void *arg){
  register long retval __asm__ ("%rax");
  __asm__("movq %0,%%rax\n\t"
          "xorq %%rcx,%%rcx\n\t"
          "xorq %%r8,%%r8\n\t"
          "syscall\n"
          : : "i" (__NR_clone), "r" (retval) :"rcx","r8");
  if(retval < 0){
    perror("Clone failed");
    exit(EXIT_FAILURE);
  } else if(retval > 0) {
    return retval;
  } else {
    //this is in the new thread
    fn(arg);
    __builtin_unreachable();//tell gcc that we can never get here
  }
}
static inline char* __attribute__((const)) ordinal_sufffix(uint32_t num){
  if(num == 1){
    return "st";
  }
  if(num == 2){
    return "nd";
  }
  if(num == 3){
    return "rd";
  }
  return "th";
}
int main(int argc,char *argv[]){
  //remove the program name from the arguments (its just eaiser)
  num_files=(--argc);
  argv++;
  if(argc <=0){
    fprintf(stderr,"Error no filenames given\n");
  }
  //this is used when threads are waiting for more data
  sigemptyset(&sigwait_set);
  sigaddset(&sigwait_set,SIGCONT);
  sigaddset(&sigwait_set,SIGTERM);
  //temporally assume one file
  PRINT_MSG("program start\n");
  struct filebuf file_buf=read_full_file(argv[0]);
  PRINT_MSG("read file\n");
  parse_buf(file_buf.buf);
  PRINT_MSG("parsed file\n");
  PRINT_FMT("Read %d words\n",indices_index);
  struct heap common_words=sort_words();
  PRINT_MSG("sorted words\n");
#if (defined DEBUG) && !(defined NDEBUG)
  if(!is_sorted(common_words.heap,common_words.size)){
    printf("Failed to sort the heap\n");
  } else {
    printf("Sorted the heap\n");
  }
#endif
  int i;
/*
  for(i=0;i<indices_index;i++){
    print_count_word(global_hash_table[hash_table_indices[i]]);
  }
  return 0;
*/
  uint32_t size=common_words.size;
  for(i=size-1;i>=size-30;i--){
    //We can't use the %s format specifier of printf to print the actual word
    //because its not null terminated
    printf("The %d%s most common word was ",size-i,ordinal_sufffix(size-i));
    fwrite(common_words.heap[i]->str,common_words.heap[i]->len,1,stdout);
    printf(", with %d occurances\n",common_words.heap[i]->count);
  }
  return 0;
}
