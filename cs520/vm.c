#include "vm.h"
//unhygneic macro, assumes label HALT
//check if addr is valid relative to vm_mem
#define mem_check_relative(addr)                         \
  (if(addr < 0 || addr > vm_mem_limit){                  \
    goto HALT;                                           \
  })
//check if addr is within the bounds of vm_mem & vm_mem_max
#define mem_check_absolute(addr)                         \
  (if(addr < vm_mem || addr > (vm_mem+vm_mem_limit)){    \
    goto HALT;                                           \
  })
//return the actual memory location (in c) of vm_addr
#define real_addr(vm_addr) (vm_mem+(vm_addr))
//return the value stored in vm register reg_offset
//where reg_offset is between 0 and 15
#define vm_reg_val(vm,reg_offset) (*(vm+reg_offset))
#define vm_math_op(instr,op)                    \
  (*(vm+instr.op_reg_reg.reg1)=                 \
   *(vm+instr.op_reg_reg.reg1) op               \
   *(vm+instr.op_reg_reg.reg2))
//bitcast the registers to floats
//and bitcast the result back to a 32bit word
#define vm_fmath_op(instr,op)                    \
  (*(vm+instr.op_reg_reg.reg1)=*(vm_word*)&      \
   (*(vm_float*)(vm+instr.op_reg_reg.reg1)) op   \
   (*(vm_float*)(vm+instr.op_reg_reg.reg2)))
#define vm_cmp(instr,op)                        \
  (*(vm+instr.op_reg_reg_addr.reg1) op          \
   *(vm+instr.op_reg_reg_addr.reg2))
void vm_loop(vm_ptr vm){
  vm_op instr;
  while(1){
    instr.bits=vm->mem[vm->pc++];//fetch instruction from memory
    switch(instr.op.op){
      case VM_HALT:
        goto HALT;
      case VM_LOAD:{
        uint64_t load_addr=vm_mem+(vm->pc+instr.op_reg_addr.addr);
        mem_check_absolute(load_addr):
        *(vm+instr.op_reg_addr.reg)=*load_addr;
        break;
      }        
      case VM_STORE:{
        uint64_t store_addr=vm_mem+(vm->pc+instr.op_reg_addr.addr);
        mem_check_absolute(store_addr):        
        *store_addr=*(vm+instr.op_reg_addr.reg);
        break;
      }
      case VM_LDIMM:{
        *(vm+instr.op_reg_imm.reg)=instr.op_reg_imm.imm;
        break;
      }
      case VM_LDADDR:{
        vm_word addr=vm->pc+instr.op_reg_addr.addr;
        *(vm+instr.op_reg_addr.reg)=addr;
        break;
      }
      case VM_LDIND:{
        uint64_t load_addr=vm_mem+(*(vm+instr.op_reg_reg_offset.reg2)+
                                   instr.op_reg_reg_offset.offset);
        mem_check_absolute(load_addr):
        *(vm+instr.op_reg_reg_offset.reg)=*load_addr;
        break;
      }
      case VM_STIND:{
        uint64_t store_addr=vm_mem+(*(vm+instr.op_reg_reg_offset.reg2)+
                                   instr.op_reg_reg_offset.offset);
        mem_check_absolute(store_addr):
        *store_addr=*(vm+instr.op_reg_reg_offset.reg);
        break;
      }
      case VM_ADDF:
        vm_fmath_op(instr,+);
        break;
      case VM_SUBF:
        vm_fmath_op(instr,-);
        break;
      case VM_DIVF:
        if(instr.op_reg_reg.reg2 == 0 ||
           instr.op_reg_reg.reg2 == 0x80000000){
          goto HALT;
        }
        vm_fmath_op(instr,/);
        break;
      case VM_MULF:
        vm_fmath_op(instr,*);
        break;
      case VM_ADDI:
        vm_math_op(instr,+);
        break;
      case VM_SUBI:
        vm_math_op(instr,-);
        break;
      case VM_DIVI:
        if(instr.op_reg_reg.reg2 == 0){
          goto HALT;
        }
        vm_math_op(instr,/);
        break;
      case VM_MULI:
        vm_math_op(instr,*);
        break;
      case VM_CALL:
        vm->sp--;
        mem_check_relative(vm->sp);
        *(vm_mem+vm->sp)=vm->pc;
        vm->pc+=instr.op_addr.addr;
        vm->sp--;
        mem_check_relative(vm->sp);
        *(vm_mem+vm->sp)=vm->fp;
        vm->fp=vm->sp;
        vm->sp--;
        mem_check_relative(vm->sp);
        *(vm_mem+vm->sp)=0;
        break;
      case VM_RET:{
        vm_word retval=*(vm_mem+vm->sp++);
        mem_check_relative(vm->sp);
        vm_word saved_fp=*(vm_mem+vm->sp++);
        mem_check_relative(vm->sp);
        vm_word retaddr=*(vm_mem+vm->sp++);
        mem_check_relative(vm->sp);
        mv->fp=saved_fp;
        vm->pc=retaddr;
        mem_check_relative((vm->fp-1));
        *(vm_mem+vm->fp-1)=retval;
        break;
      case VM_BLT:
        if(vm_cmp(instr,<)){
          vm->pc+=instr.op_reg_reg_addr.addr;
        }
        break;
      case VM_BGT:
        if(vm_cmp(instr,>)){
          vm->pc+=instr.op_reg_reg_addr.addr;
        }
        break;
      case VM_BEQ:
        if(vm_cmp(instr,==)){
          vm->pc+=instr.op_reg_reg_addr.addr;
        }
        break;
      case VM_JMP:
        vm->pc+=instr.op_addr.addr;
        break;
      case VM_CMPXCHG:
        mem_check_absolute(vm_mem+vm->pc+instr.op_reg_reg_addr.addr);
        cmpxchg_atomic(instr.op_reg_reg_addr.reg1,instr.op_reg_reg_addr.reg2,
                       vm_mem+vm->pc+instr.op_reg_reg_addr.addr);
        break;
      case VM_GETPID:
        *(vm+instr.op_reg.reg)=vm->processor_id;
        break;
      case VM_GETPN:
        *(vm+instr.op_reg.reg)=num_processors;
        break;
      case VM_PUSH:
        --vm->sp;
        mem_check_relative(vm->sp);//if this was --vm->sp it'd get evaluated twice
        *(vm_mem+vm->sp)=*(vm+instr.op_reg.reg);
        break;
      case VM_POP:
        mem_check_relative(vm->sp);
        *(vm+instr.op_reg.reg)=*(vm_mem+vm->sp++);
        break;
      default:
        goto HALT;
    }
  }
 HALT:
  return;
  }
}
vm_ptr vm_init(){
  vm_ptr processor=xmalloc(sizeof(struct vm));
  //should probably be an atomic add
  processor->processor_id=cur_proc_id++;
  return processor;
}
