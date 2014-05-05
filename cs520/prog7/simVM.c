#define _GNU_SOURCE
/* I intended to do this by having one array of pages with metadata indicating 
   the status of each page (in memory, in the tlb, and the dirty bit) and use
   this for everything, this works for lru fine but for rr it works but
   gets different results then expected, this is because I simply loop over
   the whole array of pages to find a page to replace, which I think fits
   the spec fine (it does everything it should, it uses round robin and
   replaces things in a consistant manner) but doesn't match your results.
   So I have what's basically a massive hack to make my results match yours
   I have arrays of indices for the page table and tlb that I use for 
   round robin replacement to keep track of what is in the pagetable and
   tlb, this doesn't sound like a huge hack, but since it doesn't fit
   with the way I wrote the rest of the program it feels really forced, but
   it does work.
*/
//there are very few library calls in this, I call mmap once to allocate all my
//memory, I call perror incase mmap fails and I call printf to print statistics
#include <stdio.h>
#include <errno.h>
#include <limits.h>//included for UINT_MAX and ULONG_MAX
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
/* there are lots of macros since the code for the tlb and the pagetable
   is practically identical, and the code for round robin and tlb replacement
   is fairly similar so I'm using macros to make generic functions. 
   By using the symbol concatenation feature of the preprocessor you
   can write (kinda) polymorphic macros, combined with the gnu extensions
   __typeof__ and statement expressions you can do a lot with macros, it's
   no where near the power of lisp macros but it's still pretty useful.

   Using macros in this way relies a lot on having a consistant naming scheme
   since macros take names as parameters.

   Personally I don't think this adds too much complexity, but that's because
   I'm used to it, and used to lisp macros. But it might make code harder
   to understand for other people I don't know, I certantly think it's 
   easier to follow than c++ templates.
*/
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
//well there were some here, but I switched to using divison in a few places
//when I was debugging to make sure I wasn't messing up on bitwise stuff
//and I just left it in since performance doesn't matter
#define POW_OF_2(x) (!(x & (x-1)))
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
//calculate the minimum of an array arr of length len
//get, which should be a function or macro is called on each
//element of of the array to get the values to be compared
//the index of the minimum value is returned
#define ARRAY_MIN(arr,len,get)                                   \
  ({ulong i,j=0;                                                 \
    __typeof__ (get(arr[0])) min_val=get(arr[0]);                \
    for(i=1;i<len;i++){                                          \
      if(get(arr[i])<min_val){                                   \
        min_val=get(arr[i]);                                     \
        j=i;                                                     \
      }                                                          \
    }                                                            \
    j;})

//macros for finding the least recently used value in the pagetable/tlb
//these first two macros are used to get the time from a page, returning
//the maximum possible time if they aren't in the pagetable/tlb to insure
//only valid pages are considered
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
#define rr_inc(vm,thing)                        \
  ({uint retval=vm->thing##_rr_index++;           \
  vm->thing##_rr_index%=vm->thing##_size;        \
  retval;})
struct pgtable_entry {
  ulong time;
  uchar dirty_bit;
  uchar in_pgtable;
  uchar in_tlb;
};
/*#define rr_replace(vm,new_ind,thing)                               \
  ({int ind;                                                       \
    do {                                                           \
      ind=rr_inc(vm,thing);                                        \
    } while(!vm->pg_array[ind].in_##thing);                        \
    vm->pg_array[ind].in_##thing=0;                                \
    vm->pg_array[new_ind].in_##thing=1;                            \
    ind;})*/
/*generic macros implentiming rr and lru replacement parametized by
  the thing that a value is being replaced in...basically it works
  for either the tlb or the page table depending on the value given for 
  thing
*/
#define rr_replace(vm,new_ind,thing)            \
  ({uint rr_ind=rr_inc(vm,thing);               \
    uint ind=vm->rr_##thing[rr_ind];            \
    vm->pg_array[ind].in_##thing=0;             \
    vm->pg_array[new_ind].in_##thing=1;         \
    vm->rr_##thing[rr_ind]=new_ind;             \
    ind;})
#define lru_replace(vm,new_ind,thing)              \
  ({int ind=thing##_lru(vm);                       \
    assert(vm->pg_array[ind].in_##thing);          \
    vm->pg_array[ind].in_##thing=0;                \
    vm->pg_array[new_ind].in_##thing=1;            \
    ind;})
//main struct used in the program, stores all the state assoicated
//with a virtual memory space
struct virtual_memory {
  uint *vm;//memory
  pgtable_entry *pg_array;
//this is a hack to get rr replacement to fit with the spec  
  uint *rr_pgtable;
  uint *rr_tlb;
  uint mem_size;//need this for cleanup
  uint vm_size;uint pm_size;uint pg_size;uint tlb_size;
  uint pgtable_size;uint pg_array_size;
  uint time;
  uint pg_faults;uint tlb_misses;uint disk_writes;
  //indices for round robin replacement
  uint tlb_rr_index;
  uint pgtable_rr_index;
  uchar pg_replace_alg;
  uchar tlb_replace_alg;
};
//unused, was used for debugging 
static inline void print_vm_struct(vm_handle vm){
  fprintf(stderr,"struct address = %p\n"
          "mem size = %#0x\n"
          "vm size = %#0x\n"
          "pm size = %#0x\n"
          "pg size = %#0x\n"
          "tlb size = %#0x\n"
          "page table size = %#0x\n",
          vm,vm->mem_size,vm->vm_size,vm->pm_size,
          vm->pg_size,vm->tlb_size,vm->pgtable_size);
}
//function called when given an adress that is out of the range of memory
static inline void __attribute__((noreturn))range_err(uint addr){
  fprintf(stderr,"Error address %#0x is out of range\n",addr);
  exit(EXIT_FAILURE);
}
void* __attribute__((alias("create_vm")))createVM(uint,uint,uint,uint,char,char);
//create a new virtual memory space and return an opaque handle to it
static vm_handle create_vm(uint vm_size,uint pm_size,
                           uint pg_size,uint tlb_size,
                           uchar pg_replace_alg, uchar tlb_replace_alg){
  
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
    sizeof(uint)*pm_size*tlb_size+
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
  int i;
  uint pgtable_size=pm_size;
  uint pg_array_size=vm_size;
  vm_size*=pg_size;
  pm_size*=pg_size;
  vm_handle retval=mem;
  ulong time=0;
  uint *vm=(void*)mem+sizeof(struct virtual_memory);
  pgtable_entry *pg_array=(void*)vm+(vm_size*sizeof(uint));
  uint *rr_pgtable=(void*)pg_array+(sizeof(pgtable_entry)*pg_array_size);
  uint *rr_tlb=(void*)rr_pgtable+(sizeof(uint)*pgtable_size);
  for(i=0;i<tlb_size;i++){
    pg_array[i]=(pgtable_entry)
      {.in_tlb=1,.in_pgtable=1,.time=time++};
    rr_pgtable[i]=i;
    rr_tlb[i]=i;
  }
  for(;i<pgtable_size;i++){//i starts equal to tlb_size from last loop
    pg_array[i]=(pgtable_entry){.in_pgtable=1,.time=time++};
    rr_pgtable[i]=i;
  }
  *retval=(struct virtual_memory)
    {.vm=vm,
     .pg_array=pg_array,.pg_array_size=pg_array_size,
     .pm_size=pm_size,.vm_size=vm_size,
     .pg_size=pg_size,.tlb_size=tlb_size,
     .pg_replace_alg=pg_replace_alg,.tlb_replace_alg=tlb_replace_alg,
     .mem_size=mem_size,.time=time,
     .pgtable_size=pgtable_size,
     .pgtable_rr_index=0,.tlb_rr_index=0,
     .rr_pgtable=rr_pgtable,.rr_tlb=rr_tlb,};
  return retval;
}
//return the number of the page that contains the adress addr
static inline uint page_of(vm_handle vm,uint addr){
  if(addr>=vm->vm_size){
    range_err(addr);
  }
  return (addr/vm->pg_size);
}
//assuming x is in arr scan arr to find the index of x
//really unsafe and bad, but I only use it once, and I know it's a hack
#define scan_arr(x,arr)                           \
  ({uint i=0;                                     \
    do{                                           \
      if(arr[i]==x){                              \
        break;                                    \
      }                                           \
    } while (++i);                                 \
    i;})
static void tlb_replace(vm_handle vm,uint page_no);
//replace a page, parameterized by the algorithm used (rr or lru)
#define replace_pg_generic(vm,new_page,alg)                     \
  ({vm->pg_faults++;                                            \
    uint old_pg=alg##_replace(vm,new_page,pgtable);             \
    if(vm->pg_array[old_pg].dirty_bit){                         \
      vm->disk_writes++;                                        \
      vm->pg_array[old_pg].dirty_bit=0;                         \
    }                                                           \
    if(vm->pg_array[old_pg].in_tlb){                            \
      vm->tlb_misses++;                                         \
      vm->pg_array[old_pg].in_tlb=0;                            \
      vm->pg_array[new_page].in_tlb=1;                          \
      if(vm->tlb_replace_alg == rr_replace_alg){                \
        vm->rr_tlb[scan_arr(old_pg,vm->rr_tlb)]=new_page;       \
      }                                                         \
    } else {                                                    \
      tlb_replace(vm,new_page);                                 \
    }                                                           \
    ;})
//function that replaces a page in the pagetable, uses the macro above
static inline void replace_pg(vm_handle vm,uint new_page){
  if(vm->pg_replace_alg == rr_replace_alg){
    replace_pg_generic(vm,new_page,rr);
  } else {
    replace_pg_generic(vm,new_page,lru);
  }
}
//function that replaces a page in the tlb, uses the replacement macros 
static inline void tlb_replace(vm_handle vm,uint new_page){
  vm->tlb_misses++;
  if(vm->tlb_replace_alg == rr_replace_alg){
    rr_replace(vm,new_page,tlb);
  } else {
    lru_replace(vm,new_page,tlb);
  }
}
//function that actually updates the page table and tlb based on the
//where the page given by page_no is in memory
static inline void vm_update(vm_handle vm,uint page_no){
  vm->pg_array[page_no].time=vm->time++;//always update time, it's just eaiser
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

//the rest of these functions are trivial so they don't have
//individual descriptions
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
