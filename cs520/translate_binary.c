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
  uint32_t branch_ind;//vm_adress/index in adress translation array
  uint32_t branch_dest;
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
    MSG("op=%d\n",op.bits,op.op.op);
    MSG("intel_adress = %#0x\n",buf_i);
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
        check_reg(reg1);
        int32_t dest=(int)vm_i+(int)op.op_reg_addr.addr;
        if(dest>(data_words+1) || dest<0){goto INVALID_MEMORY_ACCESS;}
        dest<<=3;
        quadword code=encode_store(reg1,dest);
        memcpy(buf+buf_i,code.uint8,7);
        buf_i+=7;
        continue;
      }
      case VM_LDADDR:{//->mov offset,reg,add %rdi,reg
        if(buf_i+7>len){goto LENGTH_ERROR;}
        check_reg(reg1);
        int32_t dest=(int)vm_i+(int)op.op_reg_addr.addr;
        if(dest>(data_words+1) || dest<0){goto INVALID_MEMORY_ACCESS;}
        quadword code=encode_ldimm(reg1,dest);
        memcpy(buf+buf_i,code.uint8,7);
        buf_i+=7;
        continue;
      }
        //        if(buf_i+___>len){goto LENGTH_ERROR;}
      case VM_LDIMM:{//->mov imm32,reg
        if(buf_i+7>len){goto LENGTH_ERROR;}
        check_reg(reg1);
        quadword code=encode_ldimm(reg1,op.op_reg_imm.imm);
        memcpy(buf+buf_i,code.uint8,7);
        buf_i+=7;
        continue;
      }

      case VM_LDIND:{//->mov offset(%rdi,reg2),reg1
        if(buf_i+19>len){goto LENGTH_ERROR;}
        check_regs(reg1,reg2);
        uint8_t *code=encode_ldind(reg1,reg2,op.op_reg_reg_offset.offset);
        memcpy(buf+buf_i,code,19);
        buf_i+=19;
        continue;
      }
      case VM_STIND:{//->mov reg1,offset(%rdi,reg2)
        if(buf_i+19>len){goto LENGTH_ERROR;}
        check_regs(reg1,reg2);
        uint8_t *code=encode_stind(reg1,reg2,op.op_reg_reg_offset.offset);
        memcpy(buf+buf_i,code,19);
        buf_i+=19;
        continue;
      }
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
        branches->branch_dest=dest;
        branches->addr_dest=buf+buf_i+5;
        branches->next=alloca(sizeof(struct branch_list));
        branches=branches->next;
        memcpy(buf+buf_i,code,9);
        buf_i+=9;
        continue;
      }
      case VM_JMP:{
        if(buf_i+5>len){goto LENGTH_ERROR;}
        //destination in vm code
        int32_t dest=(int)vm_i+(int)op.op_addr.addr;
        //if the jmp jumps into the data section or past
        //the end of the program it's an error
        if(dest>vm_len || dest<(data_words+1)){goto ILLEGAL_BRANCH;}
        branches->branch_ind=vm_i;
        branches->branch_dest=dest;
        branches->addr_dest=buf+buf_i+1;
        branches->next=alloca(sizeof(struct branch_list));
        branches=branches->next;
        *(buf+buf_i)=INTEL_JMP;
        buf_i+=5;
        continue;
      }
      default:
        goto ILLEGAL_INSTRUCTION;
    }
  }
  //insert a ret if we need one
  if(op.op.op != VM_HALT){
    if(buf_i+1>len){goto LENGTH_ERROR;}
    buf[buf_i]=INTEL_RET;
    buf_i++;
  }
  //resolve branches
  branches->next=0;
  if(branch_list != branches){
    while(branch_list->next){
      uint32_t addr=intel_addresses[branch_list->branch_dest]-
        intel_addresses[branch_list->branch_ind];
      MSG("translating vm address %#0x to intel adress %#0x\n",
          branch_list->branch_ind,addr);
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
