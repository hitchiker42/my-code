//This file which contains several other files concaenated together because of
//the constraints imposed on submitting programs. There is a comment before
//each file identifying the original file name.
//#include "vm_translate.h"
//would link with this rather than include it, but I can't
//#include "vm_translate.c"
//ditto with this
//#include "vm_translate_aux.c"
//'main' function
//not hygenic, but I do this enough that it's worth putting into a macro
#define check_regs(reg1,reg2)                                       \
  reg1=translate_register(op,op_reg_reg.reg1);                      \
  reg2=translate_register(op,op_reg_reg.reg1);                      \
  if(reg1<0 || reg2<0){goto INVALID_REGISTER;}
#define check_reg(reg)                                              \
  reg=translate_register(op,op_reg.reg);                            \
  if(reg<0){goto INVALID_REGISTER;}
//simple linked list to keep track of branches to resolve
struct branch_list {
  uint32_t branch_ind;
  uint8_t *addr_dest;//fill this place in at the end
  struct branch_list *next;
};
void translate_binary(char *vm_objfile_name,void *buf,int64_t len){
  if(len < 9){goto LENGTH_ERROR;}//we need at least a jmp and a ret
  int fd=open(vm_objfile_name,O_RDONLY);
  if(fd == -1){
    perror("Error opening object file");
    return 1;
  }
  vm_objfile *vm_obj=verify_file(fd);//closes fd
  if(vm_obj->outsymbol_len != 0){
    fprintf(stderr,"Error, vm object file contains unresolved outsymbols\n");
    return 2;
  }
  uint32_t vm_len=obj->outsymbol_len;
  //assume that the generated code will be < 4 gigs,
  //a fairly reasonable assumption
  //this is a buffer to translate vm adresses to intel adresses
  //for vm adress n the intel address will be the nth element of this
  uint32_t intel_addresses=malloc_atomic(sizeof(uint32_t)*vm_len);
  struct branch_list *branches=alloca(sizeof(struct branch_list));
  struct branch_list *branche_list=branches;
  register vm_op op;//current vm op
  uint32_t vm_i=0,buf_i=0;//current vm word/buffer byte
  //CHECK for a jmp and read data before main loop
  vm_op.bits=vm_obj->objcode[0];
  if(vm_op.op.op != VM_JMP){
    fprintf(stderr,"Error, first vm instruction is not a jmp\n");
    return 3;
  }
  uint32_t data_words=vm_op.op_addr.addr;
  if(len<(9+(8*data_words))){goto LENGTH_ERROR;}//jmp+ret+data
  memcpy(buf,encode_known_jmp(8*data_words+3),8);
  intel_addresses[vm_i++]=0;
  buf_i+=8;
  for(;vm_i<data_words;vm_i++,buf_i+=8){
    register uint64_t data_word;
    if(vm_obj->objcode[vm_i]>0){
      data_word=(uint64_t)vm_obj->objcode[vm_i];
    } else {
      data_word=(sign_mask|((uint64_t)vm_obj->objcode[vm_i]));
    }
    memcpy(buf+buf_i,data_word,8);
    intel_addresses[vm_i]=buf_i;
  }
  //main encoding loop
  while(vm_i<vm_len){
    uint8_t reg1,reg2;
    intel_addresses[vm_i]=buf_i;
    op=vm_obj->objcode[vm_i++];//fetch and increment ip/pc
    //I'd use dispatch tables if this were performance critical code
    //or I knew I'd need to use it later, but being as this is
    //just a one off assignment switches should be fine
    switch(op.op.op){//opcodes upto 6
      case VM_HALT://->ret
        if(buf_i+1>len){goto LENGTH_ERROR;}
        buf[buf_i]=INTEL_RET;
        buf_i++;
        continue;
      case VM_LOAD://->mov offset(%rdi),reg
      case VM_STORE:{//->mov reg,offset(%rdi)
        if(buf_i+7>len){goto LENGTH_ERROR;}
        int32_t dest=(int)vm_i+(int)op.op_reg_addr.addr;
        if(dest>(data_words+1)){goto INVALID_MEMORY_ACCESS;}
      }
      case VM_LDIMM:{//->mov imm32,reg
        if(buf_i+7>len){goto LENGTH_ERROR;}
        check_reg(reg1);
        quadword code=encode_ldimm(reg1,op.op_reg_imm.imm);
        memcpy(buf+buf_i,code.uint8,7);
        buf+=7;
        continue;
      }
      case VM_LDADDR://->mov offset,reg,add %rdi,reg
        //        if(buf_i+___>len){goto LENGTH_ERROR;}
        
      case VM_LDIND:{//->mov offset(%rdi,reg2),reg1
        check_regs(reg1,reg2);
        int32_t offset=(int)vm_i+(int)op.op_reg_reg_offset.offset;

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
        uint16_t opcode=(op.op.op==VM_BLT?INTEL_JMP_LT
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
      memcpy(branch_list->addr_dest,addr,4);
      branch_list=branch_list->next;
    }
  }
  return 0;
 INVALID_REGISTER:
  fprintf(stderr,"Error, Invalid vm register used\n");
  return 4;
 ILLEGAL_INSTRUCTION:
  fprintf(stderr,"Error, Illegal vm instruction used\n");
  return 5;
 LENGTH_ERROR:
  fprintf(stderr,
          "Error, Given buffer is not long enough to hold translated code\n");
  return 6;
 ILLEGAL_BRANCH:
  fprintf(stderr,
          "Error, Branch/jmp instruction destination is outside of the code section");
  return 7;
 INVALID_MEMORY_ACCESS:
  fprintf(stderr,
          "Error, Attempt to access data outside the data section");
  return 8;
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
