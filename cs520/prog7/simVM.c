#define _GNU_SOURCE
//I'm doing work on libvirt this summer and being as it's written to
//be portable generally generic types need to be used as opposed to
//using types of specific sizes so thats what I'm doing here for practice

//there are very few library calls in this, I call mmap once to allocate all my
//memory, I call perror incase mmap fails and I call printf to print statistics
#include <stdio.h>
#include <errno.h>
#include <limits.h>//included because UINT_MAX is 2^32
#include <sys/mman.h>
#define DEBUG
#if (defined DEBUG) && !(defined NDEBUG)
#define HERE() fprintf(stderr,"here at %s,line %d\n",__FILE__,__LINE__)
#define PRINT_MSG(string) fprintf(stderr,string);
#define PRINT_FMT(string,fmt...) fprintf(stderr,string,##fmt);
#define PRINT_LN(string) fprintf(stderr,"%s%s",string,"\n")
#define FN_START fprintf(stderr,"starting %s\n",__func__);
#define FN_END fprintf(stderr,"finishing %s\n",__func__);
#else
#define HERE()
#define PRINT_MSG(string)
#define PRINT_FMT(string,fmt...)
#define PRINT_LN()
#define FN_START
#define FN_END
#endif
#define MAX(a,b)                                \
  ({ __typeof__ (a) _a = (a);                   \
    __typeof__ (b) _b = (b);                    \
    _a > _b ? _a : _b;})
#define MIN(a,b)                                \
  ({ __typeof__ (a) _a = (a);                   \
    __typeof__ (b) _b = (b);                    \
    _a < _b ? _a : _b;})
//some bit twiddling hacks for working with powers of two
#define POW_OF_2_ROUND_DOWN(x,pow) (((uint64_t)(x)) & (~((1<<pow)-1)))
#define POW_OF_2_ROUND_UP(x,pow) ( (((uint64_t)(x)) + (1<<pow)-1)  & (~((1<<pow)-1)) )
#define POW_OF_2(x) (!(x & (x-1)))
#define POW_OF_2_LOG_2(x) (__builtin_ctz(x))
typedef struct virtual_memory *vm_handle;
typedef struct page_table_entry page_table_entry;
typedef struct page_table_entry tlb_entry;
typedef unsigned int uint;
typedef unsigned char uchar;
enum replace_algorithm {
  rr_replace=0,
  lru_replace=1,
};
struct page_table_entry {
  uint time_stamp;
  uint dirty_bit :1;
  uint addr :31;
};
//don't actually need a physical memory field
struct virtual_memory {
  uint *vm;
  page_table_entry *page_table;
  tlb_entry *tlb;
  uint mem_size;//need this for cleanup
  uint vm_size;
  uint pm_size;
  uint pg_size;
  uint tlb_size;
  uint pg_table_size;
  uint time;
  uint pg_faults;
  uint tlb_misses;
  uint disk_writes;
  //indices for round robin replacement
  uint tlb_rr_index;
  uint pg_table_rr_index;
  uchar lg2_pg_size;//log_2(page_size), used for shifting
  uchar pg_replace_alg;
  uchar tlb_replace_alg;
};
#define heap_left_child(i) (2*i+1)
#define heap_right_child(i) (2*i+2)
#define heap_parent(i) ((i-1)/2)
static int sift_down(page_table_entry *heap,int start,int size){
  heap+=start;
  int root=start,child,swap;
  while((child=heap_left_child(i)) <=size){
    swap=root;
    if(heap[child].time < heap[swap].time){
      swap=child;
    }
    if(child+1<=size && heap[child+1].time < heap[swap].time){
      swap=child+1;
    }
    if(swap!=root){
      SWAP(heap[root],heap[swap]);
      root=swap;
    } else {
      return root;
    }
  }
}
static inline void __attribute__((noreturn))range_err(uint addr){
  fprintf(stderr,"Error address %#0x is out of range\n",addr);
  exit(EXIT_FAILURE);
}
static inline uint page_of(vm_handle vm,uint addr){
  if(addr>=vm->vm_size){
    range_err(addr);
  }
  return (addr>>vm->lg2_pg_size);
}
static inline void rr_replace_pg(vm_handle vm,uint new_page){
  vm->pg_faults++;
  uint old_pg=vm->pg_table_rr_index;
  vm->pg_table_rr_index=((vm->pg_table_rr_index+1)%vm->pg_table_size);
  if(vm->page_table[old_pg].dirty_bit){
    vm->disk_writes++;
    vm->page_table[old_pg].dirty_bit=0;
  }
  SWAP(vm->page_table[old_pg],vm->page_table[new_page]);
}
static inline void rr_replace_tlb(vm_handle vm,uint new_page){
  vm->tlb_misses++;
  uint old_pg=vm->tlb_rr_index;
  vm->tlb_rr_index=((vm->tlb_rr_index+1)%vm->tlb_size);
  vm->tlb[old_pg]=vm->page_table[new_page]);
}
static inline void lru_replace_pg(vm_handle vm,uint new_page){
  if(vm->page_table[0].dirty_bit){
    vm->disk_writes++;
    vm->page_table[0].dirty_bit=0;
  }
  vm->page_table[0].valid=0;
  SWAP(vm->page_table[0],vm->page_table[new_page]);
  vm->page_table[0].time=vm->time;
  vm->page_table[0].vaild=1;
  sift_down(vm->page_table,0,vm->pg_table_size);
  return;
}
static inline void lru_replace_tlb(vm_handle vm,uint new_page){
  vm->tlb[0]=vm->page_table[new_page]);
  sift_down(vm->tlb,0,vm->tlb_size);
  return;
}
void* __attribute__((alias("create_vm")))createVM(uint,uint,uint,uint,char,char);
static vm_handle create_vm(uint vm_size,uint pm_size,
                           uint pg_size,uint tlb_size,
                           uchar pg_replace_alg, uchar tlb_replace_alg){
  //Not sure that this is correct, need to check
  if(!vm_size                                ||
     !pm_size                                ||
     !pg_size                                ||
     !tlb_size                               ||
     pm_size >= vm_size                      ||
     !POW_OF_2(pg_size)                      ||
     //if we don't cast to long it could overflow
     (long)vm_size*(long)pg_size > UINT_MAX  ||
     pg_replace_alg > 1                      ||
     tlb_replace_alg > 1                     ||
    return NULL;
  }
  uint mem_size=sizeof(struct virtual_memory)+//virtual memory struct
    sizeof(uint)*pg_size*vm_size+//virtual/physical memory
    sizeof(page_table_entry)*vm_size+//page_table
    sizeof(tlb_entry)*tlb_size;//tlb
  //two reasons for this, 1: it's eaiser just to allocate everything at once
  //and 2: mmap will zero out the pages which makes life eaiser
  void *mem=mmap(NULL,mem_size,PROT_READ|PROT_WRITE,
                 MAP_PRIVATE|MAP_ANONYMOUS,-1,0);
  if(mem==MAP_FAILED){
    perror("mmap failure");
    exit(EXIT_FAILURE);
  }
  int i;
  uchar lg2_pg_size=POW_OF_2_LOG_2(pg_size);
  vm_size<<=lg2_pg_size
  pm_size<<=lg2_pg_size;
  vm_handle retval=mem;
  uint *vm=mem+sizeof(struct virtual_memory);
  page_table_entry *pg_table=vm+(vm_size*sizeof(uint));
  uint pg_table_size=vm_size>>lg2_pg_size;
  for(i=0;i<(pg_table_size);i++){
    pg_table[i]={.valid=1,.addr=i<<lg2_pg_size};
  }
  tlb_entry *tlb=pg_table+(pg_table_size*sizeof(page_table_entry));
  for(i=0;i<tlb_size;i++){
    tlb[i]={.addr=i<<lg2_pg_size};
  }
  *retval=(struct virtual_memory)
    {.vm=vm,.page_table=pg_table,.pm_size=pm_size.tlb=tlb,.vm_size=vm_size,
     .pg_size=pg_size,.tlb_size=tlb_size,.lg2_pg_size=lg2_pg_size,
     .pg_replace_alg=pg_replace_alg,.mem_size=mem_size,
     .pg_table_size=pg_table_size,.tlb_replace_alg=tlb_replace_alg};
  return retval;
}
//routines common to read/write int/float
static inline void pg_replace(vm_handle vm,uint page_no){
  if(vm->pg_replace_alg==rr_replace){
    rr_replace_pg(vm,page_no);
  } else {
    lru_replace_pg(vm,page_no);
  }
}
static inline void tlb_replace(vm_handle vm,uint page_no){
  if(vm->tlb_replace_alg==rr_replace){
    rr_replace_tlb(vm,page_no);
  } else {
    lru_replace_tlb(vm,page_no);
  }
}
static inline void vm_update(vm_handle vm,uint page_no){
  handle->time++;
  if(page_no < vm->pg_table_size){
    pg_replace(vm,page_no);
  } else {
    if(vm->pg_replace_alg==lru_replace){
      vm->page_table[page_no].time=vm->time;
      sift_down(vm->page_table,page_no,vm->pg_table_size);
    }
  }
  if(page_no < vm->tlb_size){
    tlb_replace(vm,page_no);
  } else {
    if(vm->tlb_replace_alg==lru_replace){
      vm->tlb[page_no].time=vm->time;
      sift_down(vm->tlb,page_no,vm->tlb_table_size);
    }
  }
}
//these are mostly trivial, the float routines are just trivial wrappers
//around the int rountines to please the type checker, and the int routines
//do most of their work in the functions above. The actual reading/writing
//is done in a linear adress space and so is fairly simple
int __attribute__((alias("read_int")))readInt(void*,uint);
static int read_int(vm_handle vm,uint addr){
  uint page_no=get_page(vm,addr);
  vm_update(vm,page_no);
  return vm->vm[addr];
}
int __attribute__((alias("read_float")))readFloat(void*,uint);
static float read_float(vm_handle vm,uint addr){
  //suprisingly the common thing you would think to do here:
  //int val=read_int(vm,addr);
  //return *(float*)&val
  //is technically undefined behavior (the type punning that is)
  //obviously the complier will do the sane thing but according to
  //the C standard it's undefined (The relevent secton is 6.5 paragraph 7
  //that's in the C11 standard, but it should be the same in C99);
  union {float flt;uint integer;} retval;
  retval.integer=read_int(vm,addr);
  return retval.flt;
}

void __attribute__((alias("write_int")))writeInt(void*,uint);
static void write_int(vm_handle vm,uint addr,int val){
  uint page_no=page_of(vm,addr);//check addr/get page
  vm->page_table[page_no].dirty_bit=1;
  vm_update(vm,page_no);
  vm->vm[addr]=val;
  return;
}
void __attribute__((alias("write_float")))writeFloat(void*,uint);
static void write_float(vm_handle vm,uint addr, float val_){
  union {float flt;uint integer;} val;
  val.flt=val_;
  write_int(vm,addr,val.integer);
  return;
}
void __attribute__((alias("print_statistics")))printStatistics(void *);
void print_statistics(vm_handle handle){
  printf("Number of page faults: %d\nNumber of TLB misses:%d\n"
         "Number of disk writes:%d\n",handle->pg_faults,
         handle->tlb_misses,handle->disk_writes);
  return;
}
void __attribute__((alias("cleanup_vm")))cleanupVM(void *handle);
void cleanup_vm(vm_handle vm){
  if(munmap(vm,vm->mem_size)!=0){
    perror("munmap failed");
    exit(EXIT_FAILURE);
  }
}
