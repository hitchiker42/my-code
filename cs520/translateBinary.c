//This file which contains several other files concaenated together because of
//the constraints imposed on submitting programs. There is a comment before
//each file identifying the original file name.
//#include "vm.h"
#ifndef CS520_VM
#define CS520_VM
//bits/mman.h doesn't define all the flags we need
#include <alloca.h>
#include <asm/mman.h>
#include <ctype.h>
#include <errno.h>
#include <fcntl.h>
#include <limits.h>
#include <pthread.h>
#include <signal.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <strings.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>
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
  VM_LAST=VM_POP,
} vm_instruction;
static const char* instr_names[0x20]=
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
    int addr :20;
  } op_addr;
  struct {//format 3 getpid,getpn,push & pop
    unsigned op :8;
    unsigned reg :4;
    unsigned padding :20;
  } op_reg;
  struct {
    unsigned op :8;
    unsigned reg :4;
    int imm:20;
  } op_reg_imm;
  struct {
    unsigned op :8;
    unsigned reg :4;
    int addr :20;
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
    int offset :16;
  } op_reg_reg_offset;
  struct {
    unsigned op :8;
    unsigned reg1 :4;
    unsigned reg2 :4;
    int addr :16;
  } op_reg_reg_addr;
  vm_word bits;
};
static inline uint8_t get_opcode(vm_op instr){
  return instr.op.op;
}
#endif

//#include "vm_translate.h"
#ifndef _VM_TRANSLATE_H
#define _VM_TRANSLATE_H
//#include "vm.h"
/* 
   I'd like to think this a pretty clever hack, you can't return
   arrays from functions, but you can return structs/unions, even if they   
   happen to have arrays in them, so using these unions I can
   use small arrays without having to use malloc (to avoid returning
   the adress of a local variable).
   
*/
typedef union doubleword {
  uint32_t uint32;
  uint8_t uint8[4];
} doubleword;
typedef union quadword {
  uint64_t uint64;
  uint8_t uint8[8];
} quadword;
void *allocate_executable_buffer(uint64_t *length);
/* Intel Instruction format:
   Prefixes 0-4 bytes
   Opcode 1-3 bytes
   ModR/M 1 byte, optional, unused if opcode takes no arguments
     MMRRRrrr where M=mod,R=reg/opcode and r=reg/mem
   SIB 1 byte, optional decided by value of mod field
     SSIIIBBB where S=scale,I=index and B=base
   Displacement 0,1,2 or 4 bytes, specified by mod field
   Immediate, 0,1,2 or 4 byte immediate, specified by instruction

  REX prefixes are the bytes from 0x40 to 0x4f, used for 64 bit instructions
  REX = 0100WRXB
  where W R X and B are determined based on the specific instruction
  W = 1 if operands are 64 bits
  R is an extension of the ModR/M reg field
  X is an extension of the SIB index field, alwost never used
  B is an extension of the ModR/M r/m field SIB base field or Opcode reg field

  Values of mod:
  00: if R/M == 100, use SIB for memory operand
      else use the specified register as a base
  01: use a 8 bit displacement, rest is the same as 00
  10: use a 32 bit displacement, rest is the same as 00
  11: register-register operation

  Register-Register encoding (using REX)
  0100WR0B Opcode ModRM [MMRrrrBbbb]
  this thus encodes an asm instruction of the form
  Opcode register Rrrr, register Bbbb
  
  Register-Memory using SIB byte 
  0100WRXB Opcode ModRM[MMRrrr100] SIB[ssXxxxBbbb]
  MM (mod) can't be 11, if 01 add 8bit displacement,
                        if 10 add 32bit displacement
  encodes
  Opcode register Rrrr, (register Bbbb,register Xxxx, immediate ss)

  to encode register , (register);ie no scale/index use
  0100WR0B Opcode ModRM[MMRrrrBbbb]
  MM (mod) can't be 11, if 01 add 8bit displacement,
                        if 10 add 32bit displacement
  encodes
  Opcode register Rrrr,(register Bbbb)
  //Question if the r/m field is 100 (the rsp register)
  //How could you tell not to use the SIB byte?

  
 */
enum intel_registers {
  //B bit from REX prefix and reg field of ModRM
  //REX_B = 0
  RAX = 0x0,
  RCX = 0x1,
  RDX = 0x2,
  RBX = 0x3,
  RSP = 0x4,
  RBP = 0x5,
  RSI = 0x6,
  RDI = 0x7,
  //REX_B = 1
  R8 = 0x8,
  R9 = 0x9,
  R10 = 0xa,
  R11= 0xb,
  R12 = 0xc,
  R13 = 0xd,
  R14 = 0xe,
  R15 = 0xf,
};
typedef union ModRM ModRM;
typedef union SIB SIB;
typedef union REX REX;
typedef struct int64 int64_struct;
struct int24 {
  unsigned zeros :8;
  unsigned int24 :24;
};
union ModRM {
  struct {
    unsigned r_m :3;
    unsigned reg :3;
    unsigned mod :2;
  } fields;
  uint8_t byte;
};
union SIB {
  struct {
    unsigned scale :2;
    unsigned index :3;
    unsigned base :3;
  } fields;
  uint8_t byte;
};
union REX {
  struct {
    unsigned B :1;
    unsigned X :1;
    unsigned R :1;
    unsigned W :1;
    unsigned pad :2;
    unsigned one :1;
    unsigned zero :1;
  } rex_byte;
  uint8_t byte;
};
static inline uint8_t make_rex(uint8_t W,uint8_t R,uint8_t X,uint8_t B){
  REX rex={.rex_byte={.one=1,.W=W,.B=B,.X=X,.R=R}};
  return rex.byte;
}
static inline uint8_t make_modrm(uint8_t mod,uint8_t reg,uint8_t r_m){
  ModRM modrm;
  modrm.fields.mod=mod;
  modrm.fields.reg=reg;
  modrm.fields.r_m=r_m;
  return modrm.byte;
}
#define byte_0(int) (int&0xff000000)
#define byte_1(int) (int&0x00ff0000)
#define byte_2(int) (int&0x0000ff00)
#define byte_3(int) (int&0x000000ff)
static const uint8_t  mov_rdx_rsi[3]={0x48,0x89,0xd6};
static const uint8_t mov_rsi_rdx[3]={0x48,0x89,0xf2};
static const uint8_t xor_rdx[3]={0x48,0x31,0xd2};//xorq %rdx,%rdx
static const uint8_t mov_imm_rcx[3]={0x48,0xc7,0xc1};
static const uint64_t sign_mask=0xffffffff00000000;
typedef enum intel_opcodes {//just opcodes, theres a bunch more to an instruction
  INTEL_RET=0xc3,
  INTEL_ADD=0x03,
  INTEL_ADD_IMM=0x81,//reg field of modrm == 0
  INTEL_SUB=0x2B,
  INTEL_SUB_IMM=0x81,//reg field of modrm == 5
  INTEL_DIV=0xf7,//reg field of modrm == 6
  INTEL_MUL=0xf7,//reg field of modrm == 4
  INTEL_MOV=0x89,//movq only
  INTEL_MOV_MEM=0x8B,//movq only
  INTEL_MOV_IMM=0xC7,//reg field of modrm == 0, 32 bit immediate
  INTEL_CMP=0x3B,
  INTEL_CMP_IMM=0x83,//reg field of modrm==7, 32 bit immediate
  INTEL_JMP_EQ=0x0F84,//followed by a 32 bit relative address
  INTEL_JMP_GT=0x0F8F,//ditto
  INTEL_JMP_LT=0x0F8C,//ditto
  INTEL_JMP=0xE9,//followed by a 32 bit relative adress
  INTEL_LEA=0x8D,
} intel_opcodes;
#define intel_binop(rex,op,modrm)               \
  (rex|(op<<8)|(modrm<<16))
#define intel_imm_op(rex,op,modrm,imm)            \
  (rex|(op<<8)|(modrm<<16)|(((uint64_t)imm<<24)))
#define intel_cond_jmp(rex,op,dest)             \
  (rex|(op<<8)|((uint64_t)dest<<24))
#define intel_jmp(dest)                         \
  (INTEL_JMP|(uint64_t)dest<<8)
#endif
/*Initial jmp is number of datawords*8 + 3
  datawords are 64 bits (sign extended from 32 bit vm520 datawords)
  encode first jmp w/3 bytes of padding to keep data
  32 bit aligned*/

//#include "vm_translate.c"
//translate vm520 instructions into x86_64 instructions
/*
  VM_HALT -> ret
  //not sure how to do load/store
  add/sub -> addl/subl
  mul/div ->  //assuing args are in %edi and %esi
  movl %esi,%eax
  //if div xorl %edx,%edx
  mull %edi or divl %edi
  retl
  fadd/fsub/fdiv/fmul -> //args in %edi and %esi
  movd %edi,%xmm0
  movd %esi,%xmm1
  add/sub/div/mul ps %xmm0,%xmm1
  movd %xmm1,%eax
  retl
  blt/obgt/beq ->
  cmpl %edi,%esi
  jl/jg/je %edx
  ret
  
  cmpxchg -> lock cmpxchgl

*/
/*
  Registers,
   R0->R8,R1->R9,R2->R10,R3->R11,R4->RCX,R5->RDX
   start of code -> RDI
   RAX used in mul/div
   RSI used to hold RDX in mul/div instructions
*/
//#include "vm_translate.h"
static inline uint8_t translate_register(uint8_t vm_reg){
  switch(vm_reg){
    case 0x00:
      return R8;
    case 0x01:
      return R9;
    case 0x02:
      return R10;
    case 0x03:
      return R11;
    case 0x04:
      return RCX;
    case 0x05:
      return RDX;
    default:
      return -1;
  }
}
static inline uint8_t encode_ret(){//hlt -> retq
  return INTEL_RET;
}
//any register in the following is an intel register, not a vm registerv
static inline doubleword encode_binop(uint8_t op,uint8_t reg1,uint8_t reg2){
  uint8_t rex_r=0,rex_b=0;
  if(reg2>=0x8){
    rex_r = 1;
    reg1&=(~0x8);
  }
  if(reg1>=0x8){
    rex_b = 1;
    reg2&=(~0x8);
  }
  uint8_t modrm_byte=make_modrm(0x3,reg1,reg2);
  uint8_t rex_byte=make_rex(1,rex_r,0,rex_b);
  doubleword retval={.uint8={modrm_byte,rex_byte,op}};
  return retval;
}
static inline quadword encode_imm_binop(uint8_t op,uint8_t reg,
                                        uint8_t modrm_reg,doubleword imm){
  uint8_t rex_b;
  if(reg>=0x8){
    rex_b = 1;
    reg&=(~0x8);
  }
  uint8_t rex_byte=make_rex(1,0,0,rex_b);
  uint8_t modrm_byte=make_modrm(0x3,modrm_reg,reg);
  quadword retval;
  retval.uint8[0]=rex_byte;
  retval.uint8[1]=op;
  retval.uint8[2]=modrm_byte;
  memcpy(retval.uint8+3,imm.uint8,4);  
  return retval;
}
static inline doubleword encode_add(uint8_t reg1,uint8_t reg2){
  return encode_binop(INTEL_ADD,reg1,reg2);
}
static inline doubleword encode_sub(uint8_t reg1,uint8_t reg2){
  return encode_binop(INTEL_SUB,reg1,reg2);
}
static inline doubleword encode_cmp(uint8_t reg1,uint8_t reg2){
  return encode_binop(INTEL_CMP,reg1,reg2);
}
//15 bytes of object code
static inline uint8_t* encode_mul(uint8_t reg1,uint8_t reg2){
  static uint8_t objcode[15];//static so that I can return it's adress
  //I assume we return just the low 32 bits  
  //movq %rdx,%rsi // 3*8
  memcpy(objcode,mov_rdx_rsi,3);
  //movq %reg1,%rax //3*8
  memcpy(objcode+3,encode_binop(reg1,RAX,INTEL_MOV).uint8,3);
  //mulq %reg2 //3*8
  uint8_t rex_r;
  if(reg1>=0x8){
    rex_r = 1;
    reg2&=(~0x8);
  }
  uint8_t rex_byte_mul=make_rex(1,rex_r,0,0);
  uint8_t modrm_byte_mul=make_modrm(0x3,0x7,reg1);
  objcode[6]=rex_byte_mul;
  objcode[7]=INTEL_MUL;
  objcode[8]=modrm_byte_mul;
  //movq %rax, %reg2 //3*8
  memcpy(objcode+9,encode_binop(RAX,reg2,INTEL_MOV).uint8,3);
  //movq %rsi,%rdx
  memcpy(objcode+12,mov_rsi_rdx,3);
  return objcode;
}
//18 bytes of object code
static inline uint8_t* encode_div(uint8_t reg1,uint8_t reg2){
  static uint8_t objcode[18];
  //I assume we return just the low 32 bits  
  //movq %rdx,%rsi // 3*8
  memcpy(objcode,mov_rdx_rsi,3);
  //movq %reg1,%rax //3*8
  memcpy(objcode+3,encode_binop(reg1,RAX,INTEL_MOV).uint8,3);
  //xorq %rdx,%rdx
  memcpy(objcode+6,xor_rdx,3);
  //mulq %reg2 //3*8
  uint8_t rex_r;
  if(reg1>=0x8){
    rex_r = 1;
    reg2&=(~0x8);
  }
  uint8_t rex_byte_mul=make_rex(1,rex_r,0,0);
  uint8_t modrm_byte_mul=make_modrm(0x3,0x7,reg1);
  objcode[9]=rex_byte_mul;
  objcode[10]=INTEL_MUL;
  objcode[11]=modrm_byte_mul;
  //movq %rax, %reg2 //3*8
  memcpy(objcode+12,encode_binop(RAX,reg2,INTEL_MOV).uint8,3);
  //movq %rsi,%rdx
  memcpy(objcode+15,mov_rsi_rdx,3);
  return objcode;
}
static inline quadword encode_add_imm(uint8_t reg1,uint32_t imm){
  doubleword imm32;
  imm32.uint32=imm;
  return encode_imm_binop(INTEL_ADD_IMM,reg1,0,imm32);
}
static inline quadword encode_sub_imm(uint8_t reg1,uint32_t imm){
  doubleword imm32;
  imm32.uint32=imm;
  return encode_imm_binop(INTEL_SUB_IMM,reg1,5,imm32);
}
static inline void* encode_mul_imm(uint8_t reg1,uint32_t imm){
  //there really isn't any need to worry about saving registers
  static uint8_t objcode[22];
  memcpy(objcode,mov_imm_rcx,3);
  memcpy(objcode+3,&imm,4);
  memcpy(objcode+7,encode_mul(RCX,reg1),15);
  return objcode;
}
static inline void* encode_div_imm(uint8_t reg1,uint32_t imm){
  static uint8_t objcode[25];
  memcpy(objcode,mov_imm_rcx,3);
  memcpy(objcode+3,&imm,4);
  memcpy(objcode+7,encode_div(RCX,reg1),18);
  return objcode;
}
static inline quadword encode_known_jmp(uint32_t addr){
  quadword retval;
  retval.uint8[0]=INTEL_JMP;
  memcpy(retval.uint8+1,&addr,4);
  return retval;
}
//one of reg1/reg2 might to an adress
static inline void* encode_mov(uint8_t reg1,uint8_t reg2){
}
static inline quadword encode_ldimm(uint8_t reg,uint32_t imm){
  uint8_t rex_b;
  if(reg>=0x8){
    rex_b = 1;
    reg&=(~0x8);
  }
  quadword retval;
  uint8_t rex_byte=make_rex(1,0,0,rex_b);
  uint8_t modrm_byte=make_modrm(0x3,0x0,reg);
  retval.uint8[0]=rex_byte;
  retval.uint8[1]=INTEL_MOV_IMM;
  retval.uint8[2]=modrm_byte;
  memcpy(retval.uint8+3,&imm,4);
  return retval;
  
}
//addr will always be in data section,
//assume that the address passed here is ready to be used
static inline void* encode_ldaddr(uint8_t reg,uint32_t addr){
  
}
static inline void* encode_ldind(uint8_t reg1,uint8_t reg2,uint32_t offset){
}
static inline void* encode_stind(){}
static inline quadword  encode_store(uint8_t reg,uint32_t offset){
  uint8_t rex_r;
  if(reg>=0x8){
    rex_r = 1;
    reg&=(~0x8);
  }
  uint8_t rex_byte=make_rex(1,rex_r,0,0);
  uint8_t modrm_byte=make_modrm(0x2,reg,RDI);
  quadword retval;
  retval.uint8[0]=rex_byte;
  retval.uint8[1]=INTEL_MOV;
  retval.uint8[2]=modrm_byte;
  memcpy(retval.uint8+3,&offset,4);
  return retval;
}
static inline quadword  encode_load(uint8_t reg,uint32_t offset){
  //movq reg1,offset(%rdi)
  uint8_t rex_b;
  if(reg>=0x8){
    rex_b = 1;
    reg&=(~0x8);
  }
  uint8_t rex_byte=make_rex(1,0,0,rex_b);
  uint8_t modrm_byte=make_modrm(0x2,RDI,reg);
  quadword retval;
  retval.uint8[0]=rex_byte;
  retval.uint8[1]=INTEL_MOV_MEM;
  retval.uint8[2]=modrm_byte;
  memcpy(retval.uint8+3,&offset,4);
  return retval;
}
static inline uint8_t* encode_branch(uint8_t reg1,uint8_t reg2,
                                     uint16_t op){
  static uint8_t retval[9];
  doubleword cmp=encode_cmp(reg1,reg2);
  memcpy(retval,cmp.uint8,3);
  memcpy(retval+3,&op,2);
  return retval;
}
static inline uint8_t *encode_blt(uint8_t reg1,uint8_t reg2){
  return encode_branch(reg1,reg2,INTEL_JMP_LT);
}
static inline uint8_t *encode_bgt(uint8_t reg1,uint8_t reg2){
  return encode_branch(reg1,reg2,INTEL_JMP_GT);
}
static inline uint8_t *encode_beq(uint8_t reg1,uint8_t reg2){
  return encode_branch(reg1,reg2,INTEL_JMP_EQ);
}

//#include "translate_binary.h"
//not hygenic, but I do this enough that it's worth putting into a macro
#ifdef DEBUG
#define MSG(fmt,args...) (fprintf(stderr,fmt,##args))
#else
#define MSG(fmt,args...)
#endif
//#include "vm_translate.c"
vm_objfile *verify_file(int fd);
static void print_header(vm_word in,vm_word out,vm_word obj);
#define check_regs(reg_1,reg_2)                                     \
  reg_1=op.op_reg_reg.reg1;                                          \
  reg_2=op.op_reg_reg.reg2;                                          \
  reg_1=translate_register(reg_1);                                    \
  reg_2=translate_register(reg_2);                                    \
  if(reg_1<0 || reg_2<0){goto INVALID_REGISTER;}
#define check_reg(_reg)                                              \
  _reg=op.op_reg.reg;                                                \
  _reg=translate_register(_reg);                                     \
  if(_reg<0){goto INVALID_REGISTER;}
//simple linked list to keep track of branches to resolve
struct branch_list {
  uint32_t branch_ind;
  uint8_t *addr_dest;//fill this place in at the end
  struct branch_list *next;
};
void translateBinary(char *vm_objfile_name,void *buf_ptr,int64_t len){
  uint8_t *buf=(uint8_t *)buf_ptr;
  if(len < 9){goto LENGTH_ERROR;}//we need at least a jmp and a ret
  int fd=open(vm_objfile_name,O_RDONLY);
  if(fd == -1){
    perror("Error opening object file");
    exit(1);
  }
  vm_objfile *vm_obj=verify_file(fd);//closes fd
  if(vm_obj->outsymbol_len != 0){
    fprintf(stderr,"Error, vm object file contains unresolved outsymbols\n");
    exit(2);
  }
  uint32_t vm_len=vm_obj->objcode_len;
  //assume that the generated code will be < 4 gigs,
  //a fairly reasonable assumption
  //this is a buffer to translate vm adresses to intel adresses
  //for vm adress n the intel address will be the nth element of this
  uint32_t *intel_addresses=xmalloc_atomic(sizeof(uint32_t)*vm_len);
  struct branch_list *branches=alloca(sizeof(struct branch_list));
  struct branch_list *branch_list=branches;
  register vm_op op;//current vm op
  uint32_t vm_i=0,buf_i=0;//current vm word/buffer byte
  //CHECK for a jmp and read data before main loop
  op.bits=vm_obj->objcode[0];
  if(op.op.op != VM_JMP){
    fprintf(stderr,"Error, first vm instruction is not a jmp\n");
    exit(3);
  }
  uint32_t data_words=op.op_addr.addr;
  if(len<(9+(8*data_words))){goto LENGTH_ERROR;}//jmp+ret+data
  quadword jmp_instr=encode_known_jmp(8*data_words+3);
  MSG("jmp instr %#lx\n",jmp_instr.uint64);
  memcpy(buf,jmp_instr.uint8,8);
  intel_addresses[vm_i++]=0;
  buf_i+=8;
  for(;vm_i<=data_words;vm_i++,buf_i+=8){
    uint64_t data_word;
    if(vm_obj->objcode[vm_i]>0){
      data_word=(uint64_t)vm_obj->objcode[vm_i];
    } else {
      data_word=(sign_mask|((uint64_t)vm_obj->objcode[vm_i]));
    }
    memcpy(buf+buf_i,&data_word,8);
    intel_addresses[vm_i]=buf_i;
  }
  //main encoding loop
  while(vm_i<vm_len){
    uint8_t reg1,reg2;
    intel_addresses[vm_i]=buf_i;
    op.bits=vm_obj->objcode[vm_i++];//fetch and increment ip/pc
    MSG("vm_op = %#0x\nop=%d\n",op.bits,op.op.op);
    //I'd use dispatch tables if this were performance critical code
    //or I knew I'd need to use it later, but being as this is
    //just a one off assignment switches should be fine
    switch(op.op.op){//opcodes upto 6
      case VM_HALT://->ret
        if(buf_i+1>len){goto LENGTH_ERROR;}
        buf[buf_i]=INTEL_RET;
        buf_i++;
        continue;
      case VM_LOAD:{//->mov offset(%rdi),reg
        if(buf_i+7>len){goto LENGTH_ERROR;}
        check_reg(reg1);
        MSG("address = %d\n",(int)op.op_reg_addr.addr);
        int32_t dest=(int)vm_i+(int)op.op_reg_addr.addr;
        if(dest>(data_words+1) || dest<0){goto INVALID_MEMORY_ACCESS;}
        dest<<=3;
        quadword code=encode_load(reg1,dest);
        memcpy(buf+buf_i,code.uint8,7);
        buf_i+=7;
        continue;
      }
      case VM_STORE:{//->mov reg,offset(%rdi)
        if(buf_i+7>len){goto LENGTH_ERROR;}
        MSG("address = %d\n",op.op_reg_addr.addr);
        check_reg(reg1);
        int32_t dest=(int)vm_i+(int)op.op_reg_addr.addr;
        if(dest>(data_words+1) || dest<0){goto INVALID_MEMORY_ACCESS;}
        dest<<=3;
        quadword code=encode_store(reg1,dest);
        memcpy(buf+buf_i,code.uint8,7);
        buf_i+=7;
        continue;
      }
      case VM_LDIMM:{//->mov imm32,reg
        if(buf_i+7>len){goto LENGTH_ERROR;}       
        check_reg(reg1);
        MSG("op.op_reg_imm.reg=%d\nreg=%d\n",op.op_reg_imm.reg,reg1);
        quadword code=encode_ldimm(reg1,op.op_reg_imm.imm);
        memcpy(buf+buf_i,code.uint8,7);
        buf_i+=7;
        continue;
      }
      case VM_LDADDR://->mov offset,reg,add %rdi,reg
        //        if(buf_i+___>len){goto LENGTH_ERROR;}
      case VM_LDIND:{//->mov offset(%rdi,reg2),reg1
        //        check_regs(reg1,reg2);
        //        int32_t offset=(int)vm_i+(int)op.op_reg_reg_offset.offset;
      }
      case VM_STIND://->mov reg1,offset(%rdi,reg2)
    //add/sub use the same code, except the opcode, obviously
    //-> add/sub reg1,reg2, or add/sub imm32,reg
      case VM_ADDI:
      case VM_SUBI:{
        if(buf_i+3>len){goto LENGTH_ERROR;}
        uint8_t opcode=(op.op.op==VM_ADDI?INTEL_ADD:INTEL_SUB);
        check_regs(reg1,reg2);
        doubleword code=encode_binop(opcode,reg1,reg2);
        memcpy(buf+buf_i,code.uint8,3);
        buf_i+=3;
        continue;
      }
      case VM_DIVI:{//div requires 1 more instruciton than mul
        if(buf_i+18>len){goto LENGTH_ERROR;}
        check_regs(reg1,reg2);
        uint8_t *code=encode_div(reg1,reg2);
        memcpy(buf+buf_i,code,18);
        buf_i+=18;
        continue;
      }
      case VM_MULI:{
        if(buf_i+15>len){goto LENGTH_ERROR;}
        check_regs(reg1,reg2);
        uint8_t *code=encode_mul(reg1,reg2);
        memcpy(buf+buf_i,code,15);
        buf_i+=15;
        continue;
      }
        //and the three branch instrs use the same code
        //->cmp reg1,reg2, j<code> addr
      case VM_BLT:
      case VM_BGT:
      case VM_BEQ:{
        if(buf_i+9>len){goto LENGTH_ERROR;}
        //destination in vm code
        int32_t dest=(int)vm_i+(int)op.op_reg_reg_addr.addr;
        //if the branch jumps into the data section or past
        //the end of the program it's an error
        if(dest>vm_len || dest<(data_words+1)){goto ILLEGAL_BRANCH;}
        uint16_t opcode=(op.op.op==VM_BLT?INTEL_JMP_LT:
                         (op.op.op==VM_BGT?INTEL_JMP_GT:
                          INTEL_JMP_EQ));
        check_regs(reg1,reg2);
        uint8_t *code=encode_branch(reg1,reg2,opcode);
        branches->branch_ind=vm_i;
        branches->addr_dest=buf+buf_i+5;
        branches->next=alloca(sizeof(struct branch_list));
        branches=branches->next;
        memcpy(buf+buf_i,code,9);
        buf_i+=9;
        continue;
      }
      case VM_JMP:
      default:
        goto ILLEGAL_INSTRUCTION;
    }
  }
  if(branches != branch_list){
    //resolve branches
    branches->next=0;
    while(branch_list->next){
      uint32_t addr=intel_addresses[branch_list->branch_ind];
      memcpy(branch_list->addr_dest,&addr,4);
      branch_list=branch_list->next;
    }
  }
  MSG("Finished translating file\nRead %d 32 bit words\nWrote %d bytes\n",vm_i,buf_i);
  return;
 INVALID_REGISTER:
  fprintf(stderr,"Error, Invalid vm register used\n");
  exit(4);
 ILLEGAL_INSTRUCTION:
  fprintf(stderr,"Error, Illegal vm instruction used\n");
  exit(5);
 LENGTH_ERROR:
  fprintf(stderr,
          "Error, Given buffer is not long enough to hold translated code\n");
  exit(6);
 ILLEGAL_BRANCH:
  fprintf(stderr,
          "Error, Branch/jmp instruction destination is outside of the code section\n");
  exit(7);
 INVALID_MEMORY_ACCESS:
  fprintf(stderr,
          "Error, Attempt to access data outside the data section\n");
  exit(8);
}

#define file_size(fd)                           \
  ({off_t len=lseek(fd,0,SEEK_END);             \
    lseek(fd,0,SEEK_SET);                       \
    len;})
vm_objfile *verify_file(int fd){
  off_t len=file_size(fd);
  uint8_t *objfile=xmalloc_atomic(sizeof(struct vm_objfile)+len);
  ssize_t nbytes=read(fd,objfile+sizeof(vm_objfile),len);
  if(nbytes == (ssize_t)-1 /*|| nbytes != len*/){
    perror("Error reading from file");
    exit(EXIT_FAILURE);
  }
  if(close(fd) == -1){
    perror("Error closing file");
    exit(EXIT_FAILURE);
  }
  vm_objfile *obj=(void*)objfile;
  obj->objfile=(void*)objfile+sizeof(vm_objfile);
  obj->file_len=(vm_word)(len>>2);
  obj->insymbol_len=obj->objfile[0];
  obj->outsymbol_len=obj->objfile[1];
  obj->objcode_len=obj->objfile[2];
  if((obj->file_len-3) !=
     (obj->insymbol_len + obj->outsymbol_len + obj->objcode_len)){
    fprintf(stderr,
            "Error, object file size %d is not equal to sum of header words\n",
            obj->file_len-3);
    print_header(obj->insymbol_len,obj->outsymbol_len,obj->objcode_len);
    exit(EXIT_FAILURE);
  }
  obj->insymbols=obj->objfile+3;
  obj->outsymbols=obj->insymbols+obj->insymbol_len;
  obj->objcode=obj->outsymbols+obj->outsymbol_len;
  return obj;
}
static inline void print_header(vm_word in,vm_word out,vm_word obj){
  printf("Insymbol Length %d\nOutSymbol Length %d\nObject-Code Length %d\n",
         in,out,obj);
}
