/*this is all we need for the hash table, again we're trading space for time
  by having a (really) large hash table we never need to rehash and by
  using open adressing we don't need to use buckets (though open adressing
  has its own flaws).
*/

//desired is a pointer
#define atomic_compare_exchange(ptr,expected,desired)           \
  __atomic_compare_exchange(ptr,expected,desired,0,             \
                            __ATOMIC_SEQ_CST,__ATOMIC_SEQ_CST)
//desired isn't a pointer
#define atomic_compare_exchange_n(ptr,expected,desired)           \
  __atomic_compare_exchange_n(ptr,expected,desired,0,             \
                            __ATOMIC_SEQ_CST,__ATOMIC_SEQ_CST)
#define atomic_bts(bit_index,mem_pointer)                       \
  __asm__("lock bts %0,%1\n"                                    \
          : : "r" (bit_index), "m" (mem_pointer))
/* presumably atomic_add_fetch is just lock add val,(ptr)
   where atomic_fetch_add is lock xadd val,(ptr)
   and I imagine that add is faster that xadd
 */
#define atomic_add(ptr,val)                     \
  __atomic_add_fetch(ptr,val,__ATOMIC_SEQ_CST)
//with this (and pretty much any binary operation but add) the difference
//between fetch_or and or_fetch is a lot bigger, or_fetch is just a lock or
//whereas fetch_or translates into a cmpxcgh loop
#define atomic_or(ptr,val)                                      \
  __atomic_or_fetch(ptr,val,__ATOMIC_SEQ_CST)
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
static english_word* locking_hash_table_update(english_word *word){
  uint64_t hashv=fnv_hash(word->str,word->len);
  uint64_t index=hashv%global_hash_table_size;
  int low=(word->low?1:0);
  futex_spin_lock(&futex_lock);
  if(!global_hash_table[index]){//word isn't in the hash table, add it
    global_hash_table[index]=word;
  } else {
    do {
      //word is in the hash table, update it
      if(string_compare(global_hash_table[index],*word)){
        global_hash_table[index]->count+=1;
        if(low){
          global_hash_table[index]->low|=word->low;
        } else {
          global_hash_table[index]->high|=word->high;
        }
        goto END;
        xfree(word);
      }
    } while(global_hash_table[++index]);
    //not in the table use next free index
    global_hash_table[index]=word;
  }
 END:
  futex_spin_unlock(&futex_lock);
  return word;
}

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
  int low=(word->low?1:0);
  if(!global_hash_table[index]){//word isn't in the hash table, add it
    void *prev=global_hash_table[index];
    int test=atomic_compare_exchange_n(global_hash_table+index,&prev,word);
    if(test){
      //we added the word
      goto END;
    }
    //else, someone else changed the value of global_hash_table[index] before us
  }
  while(1){
    do {
      //see if the value in the table is the same as our value
      //if so update the value already in the table
      if(string_compare(global_hash_table[index],*word)){
        //atomically increment word count
        atomic_add(&global_hash_table[index]->count,1);
        //atomiclly update the file index
        if(low){
          atomic_or(&global_hash_table[index]->low,word->low);
        } else {
          atomic_or(&global_hash_table[index]->high,word->high);
        }
        xfree(word);
        goto END;
      }
    } while(global_hash_table[++index]);
    //not in the table use next free index (if we can)
    void *prev=global_hash_table[index];
    int test=atomic_compare_exchange_n(global_hash_table+index,&prev,word);
    if(test){break;}
    //if !test the compare exchange failed and we need to keep looping
  }
 END:
  return word;
}
//same as above but without a file index
static english_word* atomic_hash_table_update_one(english_word *word){
  uint64_t hashv=fnv_hash(word->str,word->len);
  uint64_t index=hashv%global_hash_table_size;
  if(!global_hash_table[index]){
    //use compare and swap eventually
    void *prev=global_hash_table[index];
    int test=atomic_compare_exchange_n(global_hash_table+index,&prev,word);
    if(test){
      //this happened global_hash_table[index]=word;
      goto END;
    }
  }
  while(1){
    do {
      if(string_compare(global_hash_table[index],*word)){
        //atomically increment word count
        atomic_add(&global_hash_table[index]->count,1);
        xfree(word);
        goto END;
      }
    } while(global_hash_table[++index]);
    //not in the table use next free index
    void *prev=global_hash_table[index];
    int test=atomic_compare_exchange_n(global_hash_table+index,&prev,word);
    if(test){break;}
    //if !test the compare exchange failed and we need to keep looping
  }
 END:
  return word;
}
//I can't use this ...
void *mmap_file(char *filename){
  int fd=open(filename,O_RDONLY);
  if(fd == -1){
    perror("Error opening file");
    exit(1);
  }
  off_t len=file_size(fd);
  uint8_t *buf=mmap(NULL,PAGE_ROUND_UP(len),);
}
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
      //      if(j
    }
  }
}
/* 
   Main work function, searches buf for english words (matching [a-zA-Z]{6,50})
   and puts/updates the word in the global hash table.
   frees the buffer it's given once it's done;
   TODO: this need to take another parameter specifing the file index
 */
void parse_buf(const char *buf){
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
  if(!(index < 6 || index >50)){
    //allocate the memory for the struct and the string at the same time
    //this means we only need to call malloc/free once 
    void *mem=xmalloc(sizeof(english_word)+index);
    //it seems wasteful to copy every word but if I used the string from the
    //buffer I wouldn't be able to free the buffer, and only copying the
    //string when I need in would add another layer of complexity to the already
    //complex process of atomically updating the hash table
    //I might change this later if it proves a performance issue
    my_strcpy(mem+sizeof(english_word),buf,index);
    english_word *word=mem;
    //need to modify this function to pass the file index to put
    //into this 
    *word=(english_word){.str=mem+sizeof(english_word),.len=index,
                         .low=0,.high=0,.count=0};
    atomic_hash_table_update(word);//frees word if it's not needed 
  }
  if(*(buf[index+1])!=0xff){
    index=1;
    goto START;
  }
  //no one else should need this buffer after us
  xfree(buf);
}
/*
  Setup a block of memory to be processed by parse_buf.
  if block[block_size] isn't a word character scan backwards until a word 
  character is found and mark the next character as EOF (i.e insure the block
  ends immediately after a word character). If block[block_size] is a word 
  character scan forward/backward to find the bounds of it, and if it's 
  within size limits put/update it in the hash table. Following this do the same
  as above to mark the end of the block with EOF.

  The core of this function is good, the actual interface to it needs to be
  reworked once I decide how exactly I'm going to do everything.
 */
struct {char *buf;uint32_t len;} setup_block(char *block,uint32_t block_size){
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
                           .low=0,.high=0,.count=0};
      atomic_hash_table_update(word);//frees word if it's not needed 
    }
  }
  //mark end of block with EOF
  while(!eng_accept[block[--start]]);
  block[start+1]=0xff;
  return (struct {char *buf;uint32_t len;}){.buf=block,.len=start};
}

/* Assembly for futex routines and my_strcpy.
   the futex stuff will probably be removed since I'm using atomic
   operations rather that locks
*/
//futexs 1=free, 0=locked, no waiters -1=locked, waiters
__asm__(".macro ENTRY name:req\n"
        ".globl \\name;\n"
        ".type \\name,@function;\n"
        ".p2align 4\n"
        "\\name\\():\n"
        ".cfi_startproc\n"
        ".endm\n"
        ".macro END name:req\n"
        ".cfi_endproc\n"
        ".size \\name, .-\\name\n"
        ".endm\n"

        //essentially futex unlock
        "ENTRY futex_up\n"
        "lock addq $1,(%rdi)\n"
        "testq $1,(%rdi)\n"
        //assume non contested case is most common, only jmp
        //if contested
        "jne 1f\n"
        "retq\n"
        "1:\n"
        //if we get here it's contested, so setup futex syscall
        "movq $1,(%rdi)\n"
        "movq $0,%rsi\n"
        "movq $1,%rdx\n"
        "movq $202, %rax\n" /*put futex syscall no in rax*/
        "syscall \n"
        "retq\n"
        "END futex_up\n"

        //essentially futex lock
        "ENTRY futex_down\n"
        "lock subq $1,(%rdi)\n"
        "cmpq $0,(%rdi)\n"
        //same idea as with futex_up, assume non contested is most common
        "jne 1f\n"
        "retq\n"
        "1:\n"
        //if we get here it's contested, so setup futex syscall
        "movq $-1,(%rdi)\n"
        "movq $-1,%rdx\n"
        "movq $0,%rsi\n"//this is FUTEX_WAIT
        "xorq %rcx,%rcx\n"
        "my_syscall $202\n"
        "retq\n"
        "END futex_down\n"

        //unlock spin lock
        "ENTRY futex_spin_up\n"
        //much eaiser, we do the same thing no matter what
        "movq $1,%rax\n"
        "lock xchg %rax,(%rdi)\n"
        "END futex_spin_up\n"

        //lock spin lock
        "ENTRY futex_spin_down\n"
        "movq $1,%rax\n"
        "xorq %rcx,%rcx\n"
        "1:\n"//start spining
        "cmpq %rax,%rdi\n"//is the lock free?
        "jeq 2f\n"//if yes try to get it
        "pause\n"//if no pause
        "jmp 1b\n"//spin again
        "lock cmpxcghq %rcx,(%rdi)\n"//try to get lock
        "jnz 1b\n"//if we failed spin again
        "END futex_spin_down\n"
        /*This assumes that the cost of aligning memory before copying
          outweighs the benifit for 6-50 byte strings (also that 
          rep movsb is fast)
        */
        "ENTRY my_strcpy\n"
        "movq %rdx,%rcx\n"//move length to rcx
        "movq %rsi,%rax\n"//save return value
        "rep movsb\n"//actually copy
        "retq\n"
        "END my_strcpy\n")
#define PTHREAD_CLONE_FLAGS = (CLONE_VM|CLONE_FS|CLONE_FILES|CLONE_SIGHAND|\
                               CLONE_SETTLS|CLONE_PARENT_SETTID|\
                               CLONE_CHILD_CLEARTID|CLONE_SYSVMEM|0)
#define SIMPLE_CLONE_FLAGS = (CLONE_VM|CLONE_FS|CLONE_SIGHAND|  \
                              CLONE_PARENT_SETTID|0)
//would be eaiser to just do a mov and a ret but
//then gcc will complain about returning without a value or something
//clone returns 0 for new child and child tid for parent
long clone_syscall(unsigned long flags,void *child_stack,void *ptid,
                   void *ctid,struct pt_regs *regs){
  long retval;
  __asm__("movq %1,%rax\n"
          "syscall"
          "movq %rax,%0"
          : "=g" (retval) : "=g" (__NR_clone));
  return;
}

long main(long argc,char *argv[]){
  
}
