.globl main
main:
    popl %eax
    movl %eax,%ecx
    jmp _loop
_loop:
    cmpl $0,%ecx
    je retNum
    imult %ecx
    decl %ecx
    jmp _loop
retNum:
    push %eax
    movl $4,%eax
    movl $1,%ebx
    pop %ecx
    movl $1,%edx
    int  $0x80
    movl $1,%eax
    movl $0,%ebx
    int  $0x80
