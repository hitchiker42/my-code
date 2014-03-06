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
  if(reg1>=0x8){
    rex_r = 1;
    reg1&=(~0x8);
  }
  if(reg2>=0x8){
    rex_b = 1;
    reg2&=(~0x8);
  }
  uint8_t modrm_byte=make_modrm(0x3,reg1,reg2);
  uint8_t rex_byte=make_rex(1,rex_r,0,rex_b);
  doubleword retval={.uint8={rex_byte,op,modrm_byte}};
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
  memcpy(objcode+3,encode_binop(INTEL_MOV,reg2,RAX).uint8,3);
  //mulq %reg2 //3*8
  uint8_t rex_b;
  if(reg1>=0x8){
    rex_b = 1;
    reg2&=(~0x8);
  }
  uint8_t rex_byte_mul=make_rex(1,0,0,rex_b);
  uint8_t modrm_byte_mul=make_modrm(0x3,0x5,reg1);
  objcode[6]=rex_byte_mul;
  objcode[7]=INTEL_MUL;
  objcode[8]=modrm_byte_mul;
  //movq %rsi,%rdx
  memcpy(objcode+9,mov_rsi_rdx,3);
  //movq %rax, %reg2 //3*8
  memcpy(objcode+12,encode_binop(INTEL_MOV,RAX,reg2).uint8,3);

  return objcode;
}
//18 bytes of object code
static inline uint8_t* encode_div(uint8_t reg1,uint8_t reg2){
  static uint8_t objcode[18];
  //I assume we return just the low 32 bits
  //movq %rdx,%rsi // 3*8
  memcpy(objcode,mov_rdx_rsi,3);
  //movq %reg1,%rax //3*8
  memcpy(objcode+3,encode_binop(INTEL_MOV,reg2,RAX).uint8,3);
  //xorq %rdx,%rdx
  memcpy(objcode+6,xor_rdx,3);
  //mulq %reg2 //3*8
  uint8_t rex_b;
  if(reg1>=0x8){
    rex_b = 1;
    reg1&=(~0x8);
  }
  uint8_t rex_byte_mul=make_rex(1,0,0,rex_b);
  uint8_t modrm_byte_mul=make_modrm(0x3,0x7,reg1);
  objcode[9]=rex_byte_mul;
  objcode[10]=INTEL_DIV;
  objcode[11]=modrm_byte_mul;
  //movq %rsi,%rdx
  memcpy(objcode+12,mov_rsi_rdx,3);
  //movq %rax, %reg2 //3*8
  memcpy(objcode+15,encode_binop(INTEL_MOV,RAX,reg2).uint8,3);
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
static inline void* encode_stind(uint8_t reg1,uint8_t reg2,uint32_t offset){
  /* movq reg2,%rax //3
     addq offset,%rax //6
     salq $3,%rax // 4
     addq %rdi,%rax //3
     movq reg1,(%rax) //3  
*/
  static uint8_t code[19];
  doubleword mov_reg2_rax=encode_binop(INTEL_MOV,reg2,RAX);
  memcpy(code,mov_reg2_rax.uint8,3);
  memcpy(code+3,add_imm_rax,2);
  memcpy(code+5,&offset,4);
  memcpy(code+9,sal_3_rax,4);
  memcpy(code+13,add_rdi_rax,3);
  uint8_t rex_r;
  if(reg1>=0x8){
    rex_r = 1;
    reg1&=(~0x8);
  }
  uint8_t rex_byte=make_rex(1,rex_r,0,0);
  uint8_t modrm_byte=make_modrm(0x0,reg1,RAX);
  uint8_t mov_op[3]={rex_byte,INTEL_MOV,modrm_byte};
  memcpy(code+16,mov_op,3);
  return code;

}
static inline void* encode_ldind(uint8_t reg1,uint8_t reg2,uint32_t offset){
    /* movq reg2,%rax //3
     addq offset,%rax //6
     salq $3,%rax // 4
     addq %rdi,%rax //3
     movq (%rax),reg1 //3
  */
  static uint8_t code[19];
  doubleword mov_reg2_rax=encode_binop(INTEL_MOV,reg2,RAX);
  memcpy(code,mov_reg2_rax.uint8,3);
  memcpy(code+3,add_imm_rax,2);
  memcpy(code+5,&offset,4);
  memcpy(code+9,sal_3_rax,4);
  memcpy(code+13,add_rdi_rax,3);
  uint8_t rex_b;
  if(reg1>=0x8){
    rex_b = 1;
    reg1&=(~0x8);
  }
  uint8_t rex_byte=make_rex(1,0,0,rex_b);
  uint8_t modrm_byte=make_modrm(0x0,reg1,RAX);
  uint8_t mov_op[3]={rex_byte,INTEL_MOV_MEM,modrm_byte};
  memcpy(code+16,mov_op,3);
  return code;

}
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
  uint8_t rex_r;
  if(reg>=0x8){
    rex_r = 1;
    reg&=(~0x8);
  }
  uint8_t rex_byte=make_rex(1,rex_r,0,0);
  uint8_t modrm_byte=make_modrm(0x2,reg,RDI);
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
