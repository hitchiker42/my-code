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
/* Use different means of collision resolution. instead of using
   a linked list in each bucket just increment the bucket if there
   is a collision, this will mean I need to make sure the capacity is
   a lot smaller, 0.5-0.7 or so.

   The best way to do this would be use atomic cmpxchg of pointers.
   So have the actual hash table be an array of pointers, and to update
   use atomic operations.
*/
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
//yes, use mmap, I can't fail if malloc fails, so I need to be able
//to swap pages
//I can't use mmap...damn
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
extern void process_string(const char *buf,uint64_t len);
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
  process_string(buf,index);
  if(*(buf[index+1])!=0xff){
    index=1;
    goto START;
  }
}
char *setup_block(char *block,uint32_t block_size){
  //start is start of last eng word, it will ultimately
  //refer to the end of the block we're returning
  uint32_t start=block_size,end=block_size;
  if(eng_accept[block[block_size]]){
    while(eng_accept[block[--start]]);//find start of word
    while(eng_accept[block[++end]]);//find end of word
    process_string(block-(start+1),end-start);//process word 
  }
  //mark end of block with EOF
  while(!eng_accept[block[--start]]);
  block[start+1]=0xff;
  return (struct {char *buf;uint32_t len;}){.buf=block,.len=start};
}
/*int futex(int *uaddr, int op, int val, const struct timespec *timeout,
        int *uaddr2, int val3);*/
/*Futexes, declares two functions
  usage:
  long lock_mem;
  long *lock=&lock_mem;
  futex_up(lock);
  //scalar code goes here
  futex_down(lock);
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


)
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
