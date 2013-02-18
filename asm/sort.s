	.file	"sort.c"
	.section	.rodata
.LC2:
	.string	"%d\n"
	.text
	.globl	main
	.type	main, @function
main:
.LFB0:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	subq	$48, %rsp
	movabsq	$4607182418800017408, %rax
	movq	%rax, -8(%rbp)
	movl	$0, %eax
	movq	%rax, -16(%rbp)
	movl	$12, -20(%rbp)
	movl	$1, -20(%rbp)
	jmp	.L2
.L3:
	movsd	.LC0(%rip), %xmm0
	divsd	-8(%rbp), %xmm0
	movsd	-16(%rbp), %xmm1
	addsd	%xmm1, %xmm0
	movsd	%xmm0, -16(%rbp)
	cvtsi2sd	-20(%rbp), %xmm0
	movsd	-8(%rbp), %xmm1
	mulsd	%xmm1, %xmm0
	movsd	%xmm0, -8(%rbp)
	addl	$1, -20(%rbp)
.L2:
	cmpl	$11, -20(%rbp)
	jle	.L3
	movq	-16(%rbp), %rax
	movq	%rax, -40(%rbp)
	movsd	-40(%rbp), %xmm0
	movl	$.LC2, %edi
	movl	$1, %eax
	call	printf
	movl	$0, %eax
	leave
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE0:
	.size	main, .-main
	.section	.rodata
	.align 8
.LC0:
	.long	0
	.long	1072693248
	.ident	"GCC: (GNU) 4.7.2 20120921 (Red Hat 4.7.2-2)"
	.section	.note.GNU-stack,"",@progbits
