	.text
	.file	"fact.0.rs"
	.section	.text.fact,"ax",@progbits
	.globl	fact
	.align	16, 0x90
	.type	fact,@function
fact:
	.cfi_startproc
	movl	$1, %eax
	cmpq	$2, %rdi
	jb	.LBB0_7
	leal	7(%rdi), %edx
	leaq	-2(%rdi), %rcx
	movl	$1, %eax
	testb	$7, %dl
	je	.LBB0_4
	leal	7(%rdi), %edx
	andl	$7, %edx
	negq	%rdx
	movl	$1, %eax
	.align	16, 0x90
.LBB0_3:
	imulq	%rdi, %rax
	decq	%rdi
	incq	%rdx
	jne	.LBB0_3
.LBB0_4:
	cmpq	$7, %rcx
	jb	.LBB0_7
	addq	$-7, %rdi
	.align	16, 0x90
.LBB0_6:
	leaq	7(%rdi), %rcx
	imulq	%rax, %rcx
	leaq	6(%rdi), %rax
	leaq	5(%rdi), %rdx
	imulq	%rdx, %rax
	imulq	%rcx, %rax
	leaq	4(%rdi), %rcx
	leaq	3(%rdi), %rdx
	imulq	%rdx, %rcx
	leaq	2(%rdi), %rdx
	imulq	%rcx, %rdx
	imulq	%rax, %rdx
	leaq	1(%rdi), %rax
	imulq	%rdi, %rax
	imulq	%rdx, %rax
	leaq	-8(%rdi), %rcx
	decq	%rdi
	cmpq	$1, %rdi
	movq	%rcx, %rdi
	ja	.LBB0_6
.LBB0_7:
	retq
.Lfunc_end0:
	.size	fact, .Lfunc_end0-fact
	.cfi_endproc

	.section	.text.fact_tail,"ax",@progbits
	.globl	fact_tail
	.align	16, 0x90
	.type	fact_tail,@function
fact_tail:
	.cfi_startproc
	movl	$1, %eax
	cmpq	$2, %rdi
	jb	.LBB1_7
	leal	7(%rdi), %edx
	leaq	-2(%rdi), %rcx
	movl	$1, %eax
	testb	$7, %dl
	je	.LBB1_4
	leal	7(%rdi), %edx
	andl	$7, %edx
	negq	%rdx
	movl	$1, %eax
	.align	16, 0x90
.LBB1_3:
	imulq	%rdi, %rax
	decq	%rdi
	incq	%rdx
	jne	.LBB1_3
.LBB1_4:
	cmpq	$7, %rcx
	jb	.LBB1_7
	addq	$-7, %rdi
	.align	16, 0x90
.LBB1_6:
	leaq	7(%rdi), %rcx
	imulq	%rax, %rcx
	leaq	6(%rdi), %rax
	leaq	5(%rdi), %rdx
	imulq	%rdx, %rax
	imulq	%rcx, %rax
	leaq	4(%rdi), %rcx
	leaq	3(%rdi), %rdx
	imulq	%rdx, %rcx
	leaq	2(%rdi), %rdx
	imulq	%rcx, %rdx
	imulq	%rax, %rdx
	leaq	1(%rdi), %rax
	imulq	%rdi, %rax
	imulq	%rdx, %rax
	leaq	-8(%rdi), %rcx
	decq	%rdi
	cmpq	$1, %rdi
	movq	%rcx, %rdi
	ja	.LBB1_6
.LBB1_7:
	retq
.Lfunc_end1:
	.size	fact_tail, .Lfunc_end1-fact_tail
	.cfi_endproc


	.section	".note.GNU-stack","",@progbits
