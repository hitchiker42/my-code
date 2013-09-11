	.file	"temp.c"
	.text
	.globl	incx
	.type	incx, @function
incx:
.LFB0:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	movq	%rdi, %rdx
	movl	%esi, %eax
	movq	%rdx, -32(%rbp)
	movl	%eax, -24(%rbp)
	movl	-32(%rbp), %eax
	leal	1(%rax), %ecx
	movl	-28(%rbp), %edx
	movl	-24(%rbp), %eax
	movl	%ecx, -16(%rbp)
	movl	%edx, -12(%rbp)
	movl	%eax, -8(%rbp)
	movq	-16(%rbp), %rax
	movl	-8(%rbp), %edx
	popq	%rbp
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE0:
	.size	incx, .-incx
	.ident	"GCC: (GNU) 4.8.1 20130725 (prerelease)"
	.section	.note.GNU-stack,"",@progbits
