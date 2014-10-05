	.file	"temp.c"
	.section	.rodata.str1.8,"aMS",@progbits,1
	.align 8
.LC0:
	.string	"a = %p, a - 128 = %p, a + 128 = %p\nb = %p, b - 64 = %p, b + 64 = %p\nc = %p, c + 32 = %p, c + 48 = %p\n"
	.align 8
.LC1:
	.string	"*a + 128 = %#x, *b + 64 = %#x, *c + 32 = %#x\n"
	.align 8
.LC2:
	.string	"*a + 128 = %#x, *b + 64 = %#x, *c + 32 = %#x\n*(a - 8) = %#x, *(b + 72) = %#x\n*(a - 16) = %#x, *(b + 80) = %#x\n"
	.text
	.globl	main
	.type	main, @function
main:
.LFB21:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	pushq	%r13
	pushq	%r12
	pushq	%rbx
	subq	$8, %rsp
	.cfi_offset 13, -24
	.cfi_offset 12, -32
	.cfi_offset 3, -40
	subq	$144, %rsp
	leaq	15(%rsp), %rbx
	andq	$-16, %rbx
	subq	$80, %rsp
	leaq	15(%rsp), %r12
	andq	$-16, %r12
	subq	$48, %rsp
	leaq	15(%rsp), %r13
	andq	$-16, %r13
	leaq	1024(%rbx), %rcx
	leaq	-1024(%rbx), %rdx
	leaq	384(%r13), %rax
	pushq	%rax
	leaq	256(%r13), %rax
	pushq	%rax
	pushq	%r13
	leaq	512(%r12), %rax
	pushq	%rax
	leaq	-512(%r12), %r9
	movq	%r12, %r8
	movq	%rbx, %rsi
	movl	$.LC0, %edi
	movl	$0, %eax
	call	printf
	addq	$32, %rsp
	movq	256(%r13), %rcx
	movq	512(%r12), %rdx
	movq	1024(%rbx), %rsi
	movl	$.LC1, %edi
	movl	$0, %eax
	call	printf
	movq	$3243, 1024(%rbx)
	movq	$2748, 512(%r12)
	movq	$4095, 256(%r13)
	pushq	640(%r12)
	pushq	-128(%rbx)
	movq	576(%r12), %r9
	movq	-64(%rbx), %r8
	movl	$4095, %ecx
	movl	$2748, %edx
	movl	$3243, %esi
	movl	$.LC2, %edi
	movl	$0, %eax
	call	printf
	addq	$16, %rsp
	leaq	-24(%rbp), %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%rbp
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE21:
	.size	main, .-main
	.ident	"GCC: (GNU) 4.9.1 20140903 (prerelease)"
	.section	.note.GNU-stack,"",@progbits
