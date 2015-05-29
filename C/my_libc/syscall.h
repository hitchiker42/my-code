#include "stdinc.h"
/*
  x86_64:
  Syscall paramaters:
    syscall number in rax, args in rdi, rsi, rdx, r10, r8, r9
    return value in rax, if -1 >= rax >= -4095 there was an error
*/
//same as the normal libc syscall
long syscall(int syscall_no, ...);
#define __syscall_1(_no)                        \
  ({register uint64_t no ("rax") = _no;         \
    register uint64_t ret ("rax");              \
    __asm__ volatile("syscall\n\t"              \
                     : "=r" (ret) : "0" (no));  \
    ret;})
#define __syscall_1(_no, _arg1)                                 \
  ({register uint64_t no ("rax") = _no;                         \
    register uint64_t arg1 ("rdi") = _arg1;                     \
    register uint64_t ret ("rax");                              \
    __asm__ volatile("syscall\n\t"                              \
                     : "=r" (err) : "0" (no), "r" (arg1));      \
    ret;})
#define __syscall_2(_no, _arg1, _arg2)                                  \
  ({register uint64_t no ("rax") = _no;                                 \
    register uint64_t arg1 ("rdi") = _arg1;                             \
    register uint64_t arg2 ("rsi") = _arg2;                             \
    register uint64_t ret ("rax");                                      \
    __asm__ volatile("syscall\n\t"                                      \
                     : "=r" (err) : "0" (no), "r" (arg1), "r" (arg2));  \
    ret;})
#define __syscall_3(_no, _arg1, _arg2, _arg3)                                  \
  ({register uint64_t no ("rax") = _no;                                 \
    register uint64_t arg1 ("rdi") = _arg1;                             \
    register uint64_t arg2 ("rsi") = _arg2;                             \
    register uint64_t arg3 ("rdx") = _arg3;                             \
    register uint64_t ret ("rax");                                      \
    __asm__ volatile("syscall\n\t"                                      \
                     : "=r" (err) : "0" (no), "r" (arg1), "r" (arg2), "r" (arg3)); \
    ret;})
