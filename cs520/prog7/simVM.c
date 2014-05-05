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
#include <stdlib.h>
#define NDEBUG
#include <assert.h>
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
#define SWAP(a,b)                               \
  ({ __typeof__ (a) _a = a;                     \
    a=b;                                        \
    b=_a;                                       \
    ;})
//some bit twiddling hacks for working with powers of two
#define POW_OF_2_ROUND_DOWN(x,pow) (((uint64_t)(x)) & (~((1<<pow)-1)))
#define POW_OF_2_ROUND_UP(x,pow) ( (((uint64_t)(x)) + (1<<pow)-1)  & (~((1<<pow)-1)) )
#define POW_OF_2(x) (!(x & (x-1)))
#define POW_OF_2_LOG_2(x) (__builtin_ctz(x))
typedef struct virtual_memory *vm_handle;
typedef struct pgtable_entry pgtable_entry;
typedef struct tlb_entry tlb_entry;
typedef unsigned long ulong;
typedef unsigned int uint;
typedef unsigned short ushort;//for completeness
typedef unsigned char uchar;
enum replace_algorithm {
  rr_replace_alg=0,
  lru_replace_alg=1,
};
//fine I'll be lazy and do things the slow way, looking at how pages are
//actually replaced I realized it's not really lru it's more marking pages
//as they are used and regularly clearing those marks, so a fast implementation
//of lru replacement isn't something that would ever be used in practice
#define ARRAY_MIN(arr,len,get)                  \
  ({uint i;                                      \
    __typeof__ (get(arr[0])) min_val=get(arr[0]);       \
    for(i=1;i<len;i++){                                 \
      if(get(arr[i])<min_val){                          \
        min_val=get(arr[i]);                            \
      }                                                 \
    }                                                   \
    min_val;})
//lots of macros since the code for the tlb and the pagetable is practically identical
//so I'm using macros to make generic functions
#define get_valid_time_tlb(val) (val.in_tlb?val.time:ULONG_MAX)
#define get_valid_time_pgtable(val) (val.in_pgtable?val.time:ULONG_MAX)
#define tlb_lru(vm) (ARRAY_MIN(vm->pg_array,vm->pg_array_size,get_valid_time_tlb))
#define pgtable_lru(vm) (ARRAY_MIN(vm->pg_array,vm->pg_array_size,        \
                                   get_valid_time_pgtable))
/* This'll be super inefficent but oh well, I have one array for all the pages
   I iterate through that whenever I need to replace a page or a tlb entry,
   meaning every operation is O(n) which is awful, but I guess it's fine.
*/
#define inc_mod_n(val,n)                                \
  ({__typeof__(val) retval=val;                         \
    val=(val+1)%n;                                      \
    retval;})
struct pgtable_entry {
  ulong time;
  uchar dirty_bit;
  uchar in_pgtable;
  uchar in_tlb;
};
#define rr_replace(vm,new_ind,thing)                               \
  ({int ind;                                                       \
    do {                                                           \
      ind=inc_mod_n(vm->thing##_rr_index,vm->pg_array_size);       \
    } while(!vm->pg_array[ind].in_##thing);                        \
    vm->pg_array[ind].in_##thing=0;                                \
    vm->pg_array[new_ind].in_##thing=1;                            \
    ind;})
#define lru_replace(vm,new_ind,thing)           \
  ({int ind=thing##_lru(vm);                    \
    assert(vm->pg_array[ind].in_##thing);          \
    vm->pg_array[ind].in_##thing=0;                \
    vm->pg_array[new_ind].in_##thing=1;            \
    ind;})
struct virtual_memory {
  uint *vm;//memory
  pgtable_entry *pg_array;
  uint mem_size;//need this for cleanup
  uint vm_size;uint pm_size;uint pg_size;uint tlb_size;
  uint pgtable_size;uint pg_array_size;
  uint time;
  uint pg_faults;uint tlb_misses;uint disk_writes;
  //indices for round robin replacement
  uint tlb_rr_index;
  uint pgtable_rr_index;
  uchar lg2_pg_size;//log_2(page_size), used for shifting
  uchar pg_replace_alg;
  uchar tlb_replace_alg;
};
static inline void print_vm_struct(vm_handle vm){
  fprintf(stderr,"struct address = %p\n"
          "mem size = %#0x\n"
          "vm size = %#0x\n"
          "pm size = %#0x\n"
          "pg size = %#0x\n"
          "tlb size = %#0x\n"
          "page table size = %#0x\n"
          "log_2(page size) = %#0x\n",
          vm,vm->mem_size,vm->vm_size,vm->pm_size,
          vm->pg_size,vm->tlb_size,vm->pgtable_size,
          vm->lg2_pg_size);
}
static inline void __attribute__((noreturn))range_err(uint addr){
  fprintf(stderr,"Error address %#0x is out of range\n",addr);
  exit(EXIT_FAILURE);
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
     tlb_replace_alg > 1){
    return NULL;
  }
  uint mem_size=sizeof(struct virtual_memory)+//virtual memory struct
    sizeof(uint)*pg_size*vm_size+//virtual/physical memory
    sizeof(pgtable_entry)*vm_size;//page_table
  //three reasons for this, 1: it's eaiser just to allocate everything at once
  //2: mmap will zero out the pages which makes life eaiser
  //3: I only have to call munmap to cleanup everything at the end
  void *mem=mmap(NULL,mem_size,PROT_READ|PROT_WRITE,
                 MAP_PRIVATE|MAP_ANONYMOUS,-1,0);
  if(mem==MAP_FAILED){
    perror("mmap failure");
    exit(EXIT_FAILURE);
  }
  PRINT_FMT("mmap size = %#0x\nmmap addr = %p\n",mem_size,mem);
  int i;
  uchar lg2_pg_size=POW_OF_2_LOG_2(pg_size);
  uint pgtable_size=pm_size;
  uint pg_array_size=vm_size;
  PRINT_FMT("page size = %#0x, lg2 page size = %#0x\n",pg_size,lg2_pg_size);
  vm_size<<=lg2_pg_size;
  pm_size<<=lg2_pg_size;
  vm_handle retval=mem;
  ulong time=0;
  uint *vm=(void*)mem+sizeof(struct virtual_memory);
  PRINT_FMT("vm_size = %#0x\nvm addr = %p\n",vm_size,vm);
  pgtable_entry *pg_array=(void*)vm+(vm_size*sizeof(uint));
  PRINT_FMT("pgtable_entry size = %#0x\n"
            "pgtable_size = %#0x\npage_table addr = %p\n",
            sizeof(pgtable_entry),pgtable_size,pg_array);
  for(i=0;i<tlb_size;i++){
    pg_array[i]=(pgtable_entry)
      {.in_tlb=1,.in_pgtable=1,.time=time++};
  }
  for(;i<pgtable_size;i++){//i starts equal to tlb_size from last loop
    pg_array[i]=(pgtable_entry){.in_pgtable=1,.time=time++};
  }
  HERE();
  *retval=(struct virtual_memory)
    {.vm=vm,
     .pg_array=pg_array,.pg_array_size=pg_array_size,
     .pm_size=pm_size,.vm_size=vm_size,
     .pg_size=pg_size,.tlb_size=tlb_size,
     .lg2_pg_size=lg2_pg_size,
     .pg_replace_alg=pg_replace_alg,.tlb_replace_alg=tlb_replace_alg,
     .mem_size=mem_size,.time=time,
     .pgtable_size=pgtable_size};
  return retval;
}
static inline uint page_of(vm_handle vm,uint addr){
  if(addr>=vm->vm_size){
    range_err(addr);
  }
  return (addr>>vm->lg2_pg_size);
}
static void tlb_replace(vm_handle vm,uint page_no);
#define replace_pg_generic(vm,new_page,alg)             \
  ({vm->pg_faults++;                                    \
    uint old_pg=alg##_replace(vm,new_page,pgtable);     \
    if(vm->pg_array[old_pg].dirty_bit){                 \
      vm->disk_writes++;                                \
      vm->pg_array[old_pg].dirty_bit=0;                 \
    }                                                   \
    if(vm->pg_array[old_pg].in_tlb){                    \
      vm->tlb_misses++;                                 \
      vm->pg_array[old_pg].in_tlb=0;                    \
      vm->pg_array[new_page].in_tlb=1;                  \
    } else {                                            \
      tlb_replace(vm,new_page);                         \
    }                                                   \
    ;})
static inline void replace_pg(vm_handle vm,uint new_page){
  if(vm->pg_replace_alg == rr_replace_alg){
    replace_pg_generic(vm,new_page,rr);
  } else {
    replace_pg_generic(vm,new_page,lru);
  }
}
static inline void tlb_replace(vm_handle vm,uint new_page){
  vm->tlb_misses++;
  if(vm->tlb_replace_alg == rr_replace_alg){
    rr_replace(vm,new_page,tlb);
  } else {
    lru_replace(vm,new_page,tlb);
  }
}
static inline void vm_update(vm_handle vm,uint page_no){
  vm->pg_array[page_no].time=vm->time;//always update time, it's just eaiser
  if(vm->pg_array[page_no].in_pgtable){
    if(vm->pg_array[page_no].in_tlb){
      return;
    } else {
      tlb_replace(vm,page_no);
    }
  } else {
    replace_pg(vm,page_no);
  }
}
//these are mostly trivial, the float routines are just trivial wrappers
//around the int rountines to please the type checker, and the int routines
//do most of their work in the functions above. The actual reading/writing
//is done in a linear adress space and so is fairly simple
int __attribute__((alias("read_int")))readInt(void*,uint);
static int read_int(vm_handle vm,uint addr){
  uint page_no=page_of(vm,addr);
  vm_update(vm,page_no);
  return vm->vm[addr];
}
int __attribute__((alias("read_float")))readFloat(void*,uint);
static float read_float(vm_handle vm,uint addr){
  //suprisingly the common thing you would think to do here:
  //int val=read_int(vm,addr);
  //return *(float*)&val
  //is technically undefined behavior (the type punning t2hat is)
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
  vm->pg_array[page_no].dirty_bit=1;
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
