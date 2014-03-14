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
#define L(name)	.L##name
#define JB_RBX	0
#define JB_RBP	1
#define JB_R12	2
#define JB_R13	3
#define JB_R14	4
#define JB_R15	5
#define JB_RSP	6
#define JB_PC	7
#define JB_SIZE (8*8)
#define cfi_startproc			.cfi_startproc
#define cfi_endproc			.cfi_endproc
#define cfi_def_cfa(reg, off)		.cfi_def_cfa reg, off
#define cfi_offset(reg, off)		.cfi_offset reg, off
#define cfi_register(r1, r2)		.cfi_register r1, r2
ENTRY EH_setjmp
	/* Save registers.  */
	movq %rbx, (JB_RBX*8)(%rdi)
	movq %rbp, (JB_RBP*8)(%rdi)
	movq %r12, (JB_R12*8)(%rdi)
	movq %r13, (JB_R13*8)(%rdi)
	movq %r14, (JB_R14*8)(%rdi)
	movq %r15, (JB_R15*8)(%rdi)
	lea 8(%rsp), %rdx	/* Save SP as it will be after we return.  */
	movq %rdx, (JB_RSP*8)(%rdi)
	movq (%rsp), %rax	/* Save PC we are returning to now.  */
	movq %rax, (JB_PC*8)(%rdi)
	xorl %eax, %eax
	retq
END EH_setjmp
/* Jump to the position specified by ENV, causing the
   setjmp call there to return VAL, or 1 if VAL is 0.
   void __longjmp (__jmp_buf env, int val).  */
ENTRY EH_longjmp
	/* Restore registers.  */
	movq (JB_RSP*8)(%rdi),%r8
	movq (JB_RBP*8)(%rdi),%r9
	movq (JB_PC*8)(%rdi),%rdx
	/* We add unwind information for the target here.  */
	cfi_def_cfa(%rdi, 0)
	cfi_register(%rsp,%r8)
	cfi_register(%rbp,%r9)
	cfi_register(%rip,%rdx)
	cfi_offset(%rbx,JB_RBX*8)
	cfi_offset(%r12,JB_R12*8)
	cfi_offset(%r13,JB_R13*8)
	cfi_offset(%r14,JB_R14*8)
	cfi_offset(%r15,JB_R15*8)
	movq (JB_RBX*8)(%rdi),%rbx
	movq (JB_R12*8)(%rdi),%r12
	movq (JB_R13*8)(%rdi),%r13
	movq (JB_R14*8)(%rdi),%r14
	movq (JB_R15*8)(%rdi),%r15
	/* Set return value for setjmp.  */
	mov %esi, %eax
	mov %r8,%rsp
	movq %r9,%rbp
	jmpq *%rdx
END EH_longjmp
