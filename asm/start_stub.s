.globl _start

_start: 
        xorq %rbp,%rbp 
        popq %rdi /*move argc to first argument*/
        movq %rsp,%rsi /*move argv to second argument*/
        call main
        movq %rax,%rdi
        movq $60,%rax /**/
        syscall
        int $128
