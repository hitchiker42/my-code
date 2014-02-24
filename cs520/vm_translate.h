#ifndef _VM_TRANSLATE_H
#define _VM_TRANSLATE_H
#include <stdint.h>
#include <sys/mman.h>
//bits/mman.h doesn't define all the flags we need
#include <asm/mman.h>
#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <limits.h>
#include <strings.h>
#include "vm.h"
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
  MM (mod) can't be 11
  encodes
  Opcode register Rrrr, (register Bbbb,register Xxxx, immediate ss)

  
 */

#define REX_W 0x08
#define REX_R 0x04
#define REX_X 0x02
#define REX_B 0x01
#define
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
typedef struct int64 int64_struct;
struct int24 {
  unsigned zeros :8;
  unsigned int24 :24;
};
union ModRM {
  struct {
    unsigned mod :2;
    unsigned reg :3;
    unsigned r_m :3;
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
static inline uint8_t make_rex(uint8_t W,uint8_t R,uint8_t X,uint8_t B){
  return (0x40|(W && REX_W)|(R && REX_R)|(X && REX_X)|(B && REX_B));
}
static inline uint8_t make_modrm(uint8_t mod,uint8_t reg,uint8_t r_m){
  ModRM modrm;
  modrm.fields.mod=mod;
  modrm.fields.reg=reg;
  modrm.fields.r_m=r_m;
  return modrm.byte;
}
typedef enum intel_opcodes {//just opcodes, theres a bunch more to an instruction
  INTEL_RET=0xc3,
  INTEL_ADD=0x03,
  INTEL_ADD_IMM=0x81,//reg field of modrm == 0
  INTEL_SUB=0x2B,
  INTEL_SUB_IMM=0x81,//reg field of modrm == 5
  INTEL_DIV=0xf7,//reg field of modrm == 6
  INTEL_MUL=0xf7,//reg field of modrm == 4
  INTEL_MOV=0x89,//movq only
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
