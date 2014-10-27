	.file	"mod.c"
	.text
	.globl	mod10
	.type	mod10, @function
mod10:
.LFB0:
	.cfi_startproc
	movabsq	$6148914691236517206, %rdx
	movq	%rdi, %rax
	imulq	%rdx
	movq	%rdi, %rax
	sarq	$63, %rax
	subq	%rax, %rdx
	leaq	(%rdx,%rdx,2), %rax
	subq	%rax, %rdi
	movq	%rdi, %rax
	ret
	.cfi_endproc
.LFE0:
	.size	mod10, .-mod10
	.ident	"GCC: (GNU) 4.8.2 20140206 (prerelease)"
	.section	.note.GNU-stack,"",@progbits
