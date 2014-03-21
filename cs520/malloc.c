#include <stdint.h>
static uint8_t *mem_pointer;
static uint8_t *program_break;
static uint8_t *small_block_pointer;
static uint64_t small_blocks=0;
struct mem_block {
  uint8_t *addr;
  uint64_t size;
};
extern uint64_t brk(uint64_t);
/*decalares uint64_t/void* brk(uint64_t/void*)*/
__asm__(".globl brk\n"
        ".p2align 4\n"
        "brk:\n"
        ".cfi_startproc\n"
        "movq $12,%rax\n"
        "syscall\n"
        "retq\n"
        ".cfi_endproc");
static void *mmap_anon(void *addr,size_t length,int prot){
  return mmap(addr,length,prot,MAP_PRIVATE|MAP_ANONYMOUS,-1,0);
}
static void init_malloc(){
  uint64_t current_break=brk(0);
static inline uint64_t ffz(register uint64_t val){//find first 0
  val=~val;//flip bits
  if(val){
    return __builtin_ctzl(val);
  }
  return -1;
}
//macro because it needs to modify val
#ifdef __x86_64__
#define bit_set(val,pos)                        \
  __asm__("btsq %1,%0"                          \
          : "=g" (val)                          \
          : "=g" (pos));
#else
#define bit_set(val,pos)                         \
  ({__typeof__(val) temp=1<<pos;                 \
    val|=temp;})
#endif
#ifdef __x86_64__
#define bit_clear(val,pos)                      \
  __asm__("btrq %1,%0"                          \
          : "=g" (val)                          \
          : "=g" (pos));
#else
#define bit_clear(val,pos)                       \
  ({__typeof__(val) temp=1<<pos;                 \
    temp=~temp;                                  \
    val&=temp;})
#endif

void *malloc_small_block(/*size doesn't matter at this point*/){
  uint64_t first_zero=ffz(small_blocks);
  if(first_zero==(uint64_t)-1){
    return NULL;
  }
  //since this is probably going to be compiled without optimization
  //I need to do strength reduction myself, this is really just
  //first_zero*128
  void *addr=small_block_pointer+(void*)(first_zero<<7);
  bit_set(small_blocks,first_zero);
  return addr;
}
void free_small_block(void *addr){
  uint64_t offset=(uint64_t)(addr-small_block_pointer);
  offset>>=7;//again no optimization, this is just offset/=128
  bit_clear(small_blocks,offset);
  return;
}
