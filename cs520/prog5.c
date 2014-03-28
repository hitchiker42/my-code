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

void parse_buf(const char *buf,uint64_t len){
  uint64_t index_1=0,index_2=0;
  while(1){
    while(!eng_accept[buf[index_1++]]);//skip non word characters
    index_2=index_1;
    while(eng_accept[buf[index_2++]]);
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
        "ENTRY futex_up\n"
        "lock addq $1,(%rdi)\n"
        "testq $1,(%rdi)\n"
        "/*assume most common case is the uncontested case*/\n"
        "jne 1f\n"
        "retq\n"
        "1:\n"
        "/*if we get here it's contested*/\n"
        "movq $1,(%rdi)\n"
        "movq $0,%rsi\n"
        "movq $1,%rdx\n"
        "movq $202, %rax /*futex syscall no*/\n"
        "syscall \n"
        "retq\n"
        "END futex_up\n"
        "\n"
        "/*Effectively this locks a futex*/\n"
        "ENTRY futex_down\n"
        "lock subq $1,(%rdi)\n"
        "cmpq $0,%rdi\n"
        "/*if non contendend rdi is 0*/\n"
        "jne 1f\n"
        "1:\n"
        "retq\n"
        "/*again if we get here it's contended*/\n"
        "movq $-1,(%rdi)\n"
        "movq $-1,%rdx\n"
        "movq $0,%rsi\n"
        "/*fourth argument is a timer, for now just wait forever*/\n"
        "xorq %rcx,%rcx\n"
        "my_syscall $202\n"
        "renq\n"
        "END futex_down\n")
