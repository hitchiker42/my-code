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
  blt/bgt/beq ->
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
static inline uint8_t translate_register(uint8_t vm_reg){
  switch(vm_reg){
    case 0x00:
      return R8;
    case 0x01:
      return R9;
    case 0x02:
      return R10:
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

static inline encode_ret(){//hlt -> retq
  return INTEL_RET;
}
static inline uint32_t encode_add(uint8_t reg1,uint8_t reg2){
  uint8_t rex_r=0,rex_b=0;
  if(reg1>0xf || reg2>0xf){
    return -1;//invalid register number;
  }
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
static inline uint32_t encode_sub(uint8_t reg1,uint8_t reg2);
static inline void* encode_mul(uint8_t reg1,uint8_t reg2);
static inline void* encode_div(uint8_t reg1,uint8_t reg2);
static inline void* encode_add_imm(uint8_t reg1,uint32_t imm);
static inline void* encode_sub_imm(uint8_t reg1,uint32_t imm);
static inline void* encode_mul_imm(uint8_t reg1,uint32_t imm);
static inline void* encode_div_imm(uint8_t reg1,uint32_t imm);
//one of rreg1/reg2 might to an adress
static inline void* encode_mov(uint8_t reg1,uint8_t reg2);
static inline void* encode_ldimm();
static inline void* encode_ldaddr();
static inline void* encode_ldind();
static inline void* encode_stind();
static inline void* encode_blt();
static inline void* encode_bgt();
static inline void* encode_beq();
