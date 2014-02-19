#define L(name) .L##name
/* Please consult the file sysdeps/unix/sysv/linux/x86-64/sysdep.h for
   more information about the value -4095 used below.  */

/* Usage: long syscall (syscall_number, arg1, arg2, arg3, arg4, arg5, arg6)
   We need to do some arg shifting, the syscall_number will be in
   rax.  */
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

        .text
ENTRY syscall
        movq %rdi, %rax		/* Syscall number -> rax.  */
        movq %rsi, %rdi		/* shift arg1 - arg5.  */
        movq %rdx, %rsi
        movq %rcx, %rdx
        movq %r8, %r10
        movq %r9, %r8
        movq 8(%rsp),%r9	/* arg6 is on the stack.  */
        syscall			/* Do the system call.  */
/*        cmpq $-4095, %rax	/* Check %rax for error.  */
/*        jae SYSCALL_ERROR_LABEL	/* Jump to error handler if error.  */
        ret			/* Return to caller.  */

END syscall
ENTRY put
        movq $1,%rax
        movq %rsi,%rdx
        movq %rdi,%rsi
        movq $1,%rdi
        syscall
        retq
END put        
ENTRY strlen
        xorq %rcx,%rcx
L1:
        movzbl (%rdi,%rcx),%eax
        incq %rcx
        testb %al,%al
        jnz L1
        movq %rcx,%rax
        retq
END strlen

ENTRY strrev
        callq strlen
        movq %rax,%rcx
        movq %rcx,%r8
        xorq %rsi,%rsi
        xorq %r9,%r9
        shlq $1,%r8
L2:
        movq %rsi,%r9
        subq %rcx,%r9
        movzbl -1(%rdi,%r9),%eax
        movzbl (%rdi,%rsi),%edx
        movb %al,(%rdi,%rsi)
        movb %dl,(%rdi,%r9)
        incq %r9
        testq %r8,%r9
        jne L2
        retq
END strrev

ENTRY main
        mov %rdi,%rdx
        xorq %rcx,%rcx
        testq %rcx,%rdx
        jz L5
L3:
        movq (%rsi,%rcx),%rdi
        callq strrev
        incq %rcx
        testq %rcx,%rdx
        jnz L3
        xorq %rcx,%rcx
L4:
        movq (%rsi,%rcx),%rdi
        callq strlen
        movq %rax,%rsi
        callq put
        incq %rcx
        testq %rcx,%rdx
        jnz L4
L5:
        xorq %rax,%rax
        retq
END main
