.macro ENTRY name
        .globl \name;
        .type \name,@function;
        .p2align 4
\name\():
        .cfi_startproc
.endm
.macro END name
.cfi_endproc
.size \name, .-\name
.endm
ENTRY getFP
        movq %rbp,%rax
        movq %rsp,%rdx
        retq
END getFP

        
       
      
        



        
