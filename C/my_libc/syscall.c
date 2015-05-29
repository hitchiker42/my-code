#include "stdinc.h"
#include "syscall.h"
long syscall(int syscall_no, ...){
  register uint64_t err ("rax");
  //we can't pass the arguments to the asm in c variables, since there's
  //no way to access the arguments from C (other then explicit register vars,
  //but that'd be really annoying)
  __asm__ volatile(//".intel_syntax\n\t" pass -masm=intel to gcc
                   "mov rax, rdi\n\t"
                   "mov rdi, rsi\n\t"
                   "mov rsi, rdx\n\t"
                   "mov rdx, rcx\n\t"
                   "mov r10, r8\n\t"
                   "mov r8, r9\n\t"
                   "mov r9, (rsp + 8)\n\t"
                   "syscall\n\t"
                     : "=r" (err) : : "rax", "rdi", "rsi", "rdx", "r8", "r9", "r10");
  return err;//maybe deal with this at some point
}
