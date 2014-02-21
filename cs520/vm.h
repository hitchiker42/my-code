#ifndef CS520_VM
#define CS520_VM
#include <stdint.h>
#include <stdio.h>
#include <pthread.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <errno.h>
#include <signal.h>
#define unsigned unsigned int
//I'm used to using gc (I use it for my lisp compilier) which
//guarantees memory allocated with GC_malloc is zeroed, so
//make the default behavior be to clear memory
#define xmalloc_atomic(sz)                      \
  ({void *temp=malloc(sz);                      \
    if(!temp && sz){                            \
      raise(SIGUSR1);                           \
    }                                           \
    temp;})
#define xmalloc(sz)                             \
  ({void *temp=calloc(sz,1);                    \
    if(!temp && sz){                            \
      raise(SIGUSR1);                           \
    }                                           \
    temp;})
#define MIN(a,b)                                \
  ({ __typeof__ (a) _a = a;                     \
  __typeof__(b) _b = b;                         \
  (_a<_b)?_a:_b;})
#define MAX(a,b)                                \
  ({ __typeof__ (a) _a = a;                     \
  __typeof__(b) _b = b;                         \
  (_a>_b)?_a:_b;})
#define alloca(size) __builtin_alloca (size)
typedef uint32_t vm_word;
typedef float vm_float;
typedef struct vm *vm_ptr;
typedef union vm_op vm_op;
typedef struct vm_objfile vm_objfile;
#define VM_MEM_LIMIT 1048576
//error codes for assembler?
static const uint32_t VM_FILE_NOT_FOUND = -1;
static const uint32_t VM_FILE_CONTAINS_OUTSYMBOLS = -2;
static const uint32_t VM_FILE_IS_NOT_VALID = -3;
//error codes for the vm
static const uint32_t VM_NORMAL_TERMINATION = 0;
static const uint32_t VM_DIVIDE_BY_ZERO = -1;
static const uint32_t VM_ADDRESS_OUT_OF_RANGE = -2;
static const int VM_ILLEGAL_INSTRUCTION = -3;

static const vm_word vm_mem_limit=1048576;//1<<20
static vm_word vm_mem[VM_MEM_LIMIT];//shared by all processors
static vm_word vm_mem_max;//=vm_mem+vm_mem_limit;//needs to be set in main
static vm_word num_processors;
static vm_word cur_proc_id=0;
//use the processor to do the atomic stuff
//otherwise all memory access would need to be locked via mutexes
//the two nonatomic instructions are safe, the first one only moves
//the expected value and the second only depends on the current
//processors zero flag
#define cmpxchg_atomic(reg1,reg2,addr)                          \
  ({uint8_t zf;                                                 \
  __asm__ volatile ("movl %[old],%%rax\n"                       \
                    "lock cmpxchg %[new],%[current]\n"          \
                    "setz %[zf]\n"                              \
                    : [current] "=m" (*addr), [zf] "=g" (zf)    \
                    : [old] "g" (reg1), [new] "g" (reg2)        \
                    : "%rax");                                  \
  zf;})
//each processor is described by a struct vm but memory is global
struct vm {
  //vm registers
  vm_word r0;
  vm_word r1;
  vm_word r2;
  vm_word r3;
  vm_word r4;
  vm_word r5;
  vm_word r6;
  vm_word r7;
  vm_word r8;
  vm_word r9;
  vm_word r10;
  vm_word r11;
  vm_word r12;
  vm_word fp; //r13;
  vm_word sp;//r14;
  vm_word pc;//r15;
  //vm metadata
  vm_word processor_id;//don't change this
  pthread_t thread;//each processor runs in it's own thread
};
typedef enum vm_instruction {
  //enum_name=opcode //memnonic/format;description
  VM_HALT=0x00,//halt;stop the current processor, return value should be stored in r0
  VM_LOAD=0x01,//load reg,addr;reg = *(pc+addr);if pc+addr is not a valid adress halt w/error
  VM_STORE=0x02,//store reg,addr;*(pc+addr)=reg;if reg ''''
  VM_LDIMM=0x03,//ldimm reg,const;reg=const;load immediate into register
  VM_LDADDR=0x04,//ldaddr reg,addr;reg=pc+addr;load adress into register
  VM_LDIND=0x05,//ldind reg1,offset(reg2);reg1=*(reg2+offset)
  VM_STIND=0x06,//stind reg1,offset(reg2);*(reg+offset)=reg1
  VM_ADDF=0x07,//addf reg1,reg1;floating point add
  VM_SUBF=0x08,//subf reg1,reg1;floating point sub
  VM_DIVF=0x09,//divf reg1,reg1;floating point div, error on div by 0
  VM_MULF=0x0a,//mulf reg1,reg1;floating point mul
  VM_ADDI=0x0b,//addi reg1,reg1;integer add
  VM_SUBI=0x0c,//subi reg1,reg1;integer sub
  VM_DIVI=0x0d,//divi reg1,reg1;integer div, error on div by 0
  VM_MULI=0x0e,//muli reg1,reg1;integer mul
  VM_CALL=0x0f,//call addr;call a function, more description elsewhere
  VM_RET=0x10,//ret;return from proc
  VM_BLT=0x11,//blt reg1,reg2,addr;branch to addr if reg1<reg2
  VM_BGT=0x12,//blt reg1,reg2,addr;branch to addr if reg1>reg2
  VM_BEQ=0x13,//blt reg1,reg2,addr;branch to addr if reg1==reg2
  VM_JMP=0x14,//jmp addr;branch to addr
  VM_CMPXCHG=0x15,//cmpxchg reg1,reg2,addr;same as x86 instruction
  VM_GETPID=0x16,//getpid reg;store current processor's id in reg
  VM_GETPN=0x17,//getpn reg;store number of processors in reg
  VM_PUSH=0x18,//push reg;*(--sp)=reg
  VM_POP=0x19,//pop reg;reg=*(sp++)
} vm_instruction;
const char* instr_names[0x20]=
  {"halt","load","store","ldimm","ldaddr","ldind","stind","addf",
   "subf","divf","mulf","addi","subi","divi","muli","call","ret",
   "blt","bgt","beq","jmp","cmpxchg","getpid","getpn","push","pop"};
struct vm_objfile {
  vm_word *objfile;
  vm_word file_len;
  vm_word insymbol_len;
  vm_word outsymbol_len;
  vm_word objcode_len;
  vm_word *insymbols;
  vm_word *outsymbols;
  vm_word *objcode;
};
union vm_op {
  struct {
    struct {
      unsigned op : 8;
      unsigned reg1 :4;
    } low_bits;
    union {
      unsigned addr1 :20;
      struct {
        unsigned reg2 :4;
        unsigned addr2 :16;
      } two;
    } high_bits;
  } generic;
  struct {//format1, halt & ret
    unsigned op :8;
    unsigned padding :24;
  } op;
  struct {//format2 jmp & call
    unsigned op :8;
    unsigned padding :4;
    unsigned addr :20;
  } op_addr;
  struct {//format 3 getpid,getpn,push & pop
    unsigned op :8;
    unsigned reg :4;
    unsigned padding :20;
  } op_reg;
  struct {
    unsigned op :8;
    unsigned reg :4;
    unsigned imm:20;
  } op_reg_imm;
  struct {
    unsigned op :8;
    unsigned reg :4;
    unsigned addr :20;
  } op_reg_addr;
  struct {
    unsigned op :8;
    unsigned reg1 :4;
    unsigned reg2 :4;
    unsigned padding :16;
  } op_reg_reg;  
  struct {
    unsigned op :8;
    unsigned reg1 :4;
    unsigned reg2 :4;
    unsigned offset :16;
  } op_reg_reg_offset;
  struct {
    unsigned op :8;
    unsigned reg1 :4;
    unsigned reg2 :4;
    unsigned  addr :16;
  } op_reg_reg_addr;
  vm_word bits;
};
static inline uint8_t get_opcode(vm_op instr){
  return instr.op.op;
}
#endif
