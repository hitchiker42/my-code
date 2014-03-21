#include <stdint.h>
static uint8_t *mem_pointer;
static uint8_t *program_break;
struct mem_block {
  uint8_t *addr;
  uint64_t size;
};
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
  
