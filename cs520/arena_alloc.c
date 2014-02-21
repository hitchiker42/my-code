/* Simple arena allocation, memory is allocated from a large block of memory,
   the arena, memory inside the arena is never freed or garbage collected,
   but the entire arena can be reset/freed in one operation.
   
   Specifically written to allow cords/ropes to be used without garbage
   collection. a cord can be built up via repeated concatenations
   with memory allocated from the arena, then the resulting cord
   can be either written to a file, or converted to a c string and
   the entire arena gets reset. The need for this grew out
   of using cords in an assembler, with out gc. For each
   input file the object file can be built with cords and
   then written to a file, with the arena reset between files.
 */
#include "arena_alloc.h"
#include <asm/unistd.h>
#include <unistd.h>
extern void *memset(void *s,int c,uint64_t n);
#define pow2_roundup(num,multiple) ((uint64_t)(num+multiple-1)&(uint64_t)(~(multiple-1)))
#define round_to_next_page(num) (pow2_roundup(num,_SC_PAGESIZE))
#define align_next_block(blk) (void*)(pow2_roundup(blk,min_align))
static const uint32_t min_align=16;
static const uint64_t arena_size = 0x800000;//8 megs
typedef uint64_t word64;
typedef uint32_t word32;
static void *arena_start;
static void *arena_end;
static void *next_block;
//why use brk and not malloc, because I felt like it
//so long as I call this first then I should be able to use malloc normally
//afterward, as there should be no way for malloc to tell that I changed
//the program break already
void arena_init(){
  //on linux brk returns the new program break, so brk(0) will return the current break
  arena_start=(void*)syscall(__NR_brk,0);
  arena_end=(void*)syscall(__NR_brk,(uint64_t)arena_start+arena_size);
  if(arena_end==arena_start){//brk failed
    _exit(1);//can't use printf yet and I don't feel like writting
    //a wrapper around write right now
  }
  next_block=arena_start;
}
void arena_reset(){
  next_block=arena_start;
  return;
}
void *arena_alloc(uint64_t sz){
  void *retval=next_block;
  next_block=align_next_block(next_block+sz);
  if(next_block>=arena_end){
    return NULL;//that's what malloc does
    //    fprintf(stderr,"Arena memory exhausted\n");
    //now we could do one of a few things
    //but we're just going to exit right now
    //    exit(EXIT_FAILURE);
  }
  return retval;
}
void *arena_calloc(uint64_t sz){
  void *addr=arena_alloc(sz);
  if(addr){
    memset(addr,'\0',sz);
  }
  return addr;
}
