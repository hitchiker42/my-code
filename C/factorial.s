	.file	"factorial.c"
	.text
	.p2align 4,,15
	.globl	factorial
	.type	factorial, @function
factorial:
.LFB0:
	.cfi_startproc
	testl	%edi, %edi
	jle	.L7
	movl	%edi, %edx
	shrl	$2, %edx
	leal	0(,%rdx,4), %ecx
	testl	%ecx, %ecx
	je	.L8
	cmpl	$6, %edi
	jbe	.L8
	movdqa	.LC2(%rip), %xmm4
	xorl	%eax, %eax
	movdqa	.LC0(%rip), %xmm1
	movdqa	.LC1(%rip), %xmm0
	.p2align 4,,10
	.p2align 3
.L4:
	movdqa	%xmm1, %xmm3
	psrldq	$4, %xmm1
	addl	$1, %eax
	movdqa	%xmm0, %xmm2
	cmpl	%eax, %edx
	pmuludq	%xmm0, %xmm3
	paddd	%xmm4, %xmm0
	psrldq	$4, %xmm2
	pmuludq	%xmm1, %xmm2
	pshufd	$8, %xmm3, %xmm1
	pshufd	$8, %xmm2, %xmm2
	punpckldq	%xmm2, %xmm1
	ja	.L4
	movdqa	%xmm1, %xmm2
	cmpl	%ecx, %edi
	leal	1(%rcx), %edx
	psrldq	$8, %xmm2
	movdqa	%xmm2, %xmm0
	psrldq	$4, %xmm2
	pmuludq	%xmm1, %xmm0
	psrldq	$4, %xmm1
	pshufd	$8, %xmm0, %xmm0
	pmuludq	%xmm2, %xmm1
	pshufd	$8, %xmm1, %xmm1
	punpckldq	%xmm1, %xmm0
	movdqa	%xmm0, %xmm1
	psrldq	$4, %xmm1
	movdqa	%xmm1, %xmm2
	psrldq	$4, %xmm1
	pmuludq	%xmm0, %xmm2
	psrldq	$4, %xmm0
	pmuludq	%xmm0, %xmm1
	pshufd	$8, %xmm2, %xmm0
	pshufd	$8, %xmm1, %xmm1
	punpckldq	%xmm1, %xmm0
	movd	%xmm0, -12(%rsp)
	movl	-12(%rsp), %eax
	je	.L13
	.p2align 4,,10
	.p2align 3
.L9:
	imull	%edx, %eax
	addl	$1, %edx
	cmpl	%edx, %edi
	jge	.L9
	rep
	ret
.L7:
	movl	$1, %eax
	ret
.L8:
	movl	$1, %eax
	movl	$1, %edx
	jmp	.L9
.L13:
	ret
	.cfi_endproc
.LFE0:
	.size	factorial, .-factorial
	.p2align 4,,15
	.globl	fact_acc
	.type	fact_acc, @function
fact_acc:
.LFB1:
	.cfi_startproc
	testl	%edi, %edi
	movl	%esi, %eax
	je	.L15
	movl	%edi, %ecx
	movl	%edi, %r8d
	shrl	$2, %ecx
	leal	0(,%rcx,4), %esi
	testl	%esi, %esi
	je	.L22
	cmpl	$6, %edi
	jbe	.L22
	leal	-1(%rdi), %edx
	movl	%edi, -24(%rsp)
	movdqa	.LC3(%rip), %xmm4
	movl	%edx, -20(%rsp)
	leal	-2(%rdi), %edx
	movd	-20(%rsp), %xmm2
	movl	%edx, -16(%rsp)
	leal	-3(%rdi), %edx
	movd	-16(%rsp), %xmm1
	movl	%edx, -12(%rsp)
	xorl	%edx, %edx
	movd	-12(%rsp), %xmm0
	punpckldq	%xmm0, %xmm1
	movd	-24(%rsp), %xmm0
	punpckldq	%xmm2, %xmm0
	punpcklqdq	%xmm1, %xmm0
	movdqa	.LC0(%rip), %xmm1
	.p2align 4,,10
	.p2align 3
.L17:
	movdqa	%xmm1, %xmm3
	psrldq	$4, %xmm1
	addl	$1, %edx
	movdqa	%xmm0, %xmm2
	cmpl	%edx, %ecx
	pmuludq	%xmm0, %xmm3
	paddd	%xmm4, %xmm0
	psrldq	$4, %xmm2
	pmuludq	%xmm1, %xmm2
	pshufd	$8, %xmm3, %xmm1
	pshufd	$8, %xmm2, %xmm2
	punpckldq	%xmm2, %xmm1
	ja	.L17
	movdqa	%xmm1, %xmm2
	subl	%esi, %edi
	psrldq	$8, %xmm2
	movdqa	%xmm2, %xmm0
	psrldq	$4, %xmm2
	pmuludq	%xmm1, %xmm0
	psrldq	$4, %xmm1
	pshufd	$8, %xmm0, %xmm0
	pmuludq	%xmm2, %xmm1
	pshufd	$8, %xmm1, %xmm1
	punpckldq	%xmm1, %xmm0
	movdqa	%xmm0, %xmm1
	psrldq	$4, %xmm1
	movdqa	%xmm1, %xmm2
	psrldq	$4, %xmm1
	pmuludq	%xmm0, %xmm2
	psrldq	$4, %xmm0
	pmuludq	%xmm0, %xmm1
	pshufd	$8, %xmm2, %xmm0
	pshufd	$8, %xmm1, %xmm1
	punpckldq	%xmm1, %xmm0
	movd	%xmm0, -20(%rsp)
	imull	-20(%rsp), %eax
	cmpl	%esi, %r8d
	je	.L15
	.p2align 4,,10
	.p2align 3
.L22:
	imull	%edi, %eax
	subl	$1, %edi
	jne	.L22
.L15:
	rep
	ret
	.cfi_endproc
.LFE1:
	.size	fact_acc, .-fact_acc
	.p2align 4,,15
	.globl	tail_fact
	.type	tail_fact, @function
tail_fact:
.LFB2:
	.cfi_startproc
	testl	%edi, %edi
	je	.L32
	movl	%edi, %edx
	movl	%edi, %esi
	shrl	$2, %edx
	leal	0(,%rdx,4), %ecx
	testl	%ecx, %ecx
	je	.L33
	cmpl	$6, %edi
	jbe	.L33
	leal	-1(%rdi), %eax
	movl	%edi, -24(%rsp)
	movdqa	.LC3(%rip), %xmm4
	movl	%eax, -20(%rsp)
	leal	-2(%rdi), %eax
	movd	-20(%rsp), %xmm2
	movl	%eax, -16(%rsp)
	leal	-3(%rdi), %eax
	movd	-16(%rsp), %xmm1
	movl	%eax, -12(%rsp)
	xorl	%eax, %eax
	movd	-12(%rsp), %xmm0
	punpckldq	%xmm0, %xmm1
	movd	-24(%rsp), %xmm0
	punpckldq	%xmm2, %xmm0
	punpcklqdq	%xmm1, %xmm0
	movdqa	.LC0(%rip), %xmm1
	.p2align 4,,10
	.p2align 3
.L29:
	movdqa	%xmm1, %xmm3
	psrldq	$4, %xmm1
	addl	$1, %eax
	movdqa	%xmm0, %xmm2
	cmpl	%eax, %edx
	pmuludq	%xmm0, %xmm3
	paddd	%xmm4, %xmm0
	psrldq	$4, %xmm2
	pmuludq	%xmm1, %xmm2
	pshufd	$8, %xmm3, %xmm1
	pshufd	$8, %xmm2, %xmm2
	punpckldq	%xmm2, %xmm1
	ja	.L29
	movdqa	%xmm1, %xmm2
	subl	%ecx, %edi
	cmpl	%ecx, %esi
	psrldq	$8, %xmm2
	movdqa	%xmm2, %xmm0
	psrldq	$4, %xmm2
	pmuludq	%xmm1, %xmm0
	psrldq	$4, %xmm1
	pshufd	$8, %xmm0, %xmm0
	pmuludq	%xmm2, %xmm1
	pshufd	$8, %xmm1, %xmm1
	punpckldq	%xmm1, %xmm0
	movdqa	%xmm0, %xmm1
	psrldq	$4, %xmm1
	movdqa	%xmm1, %xmm2
	psrldq	$4, %xmm1
	pmuludq	%xmm0, %xmm2
	psrldq	$4, %xmm0
	pmuludq	%xmm0, %xmm1
	pshufd	$8, %xmm2, %xmm0
	pshufd	$8, %xmm1, %xmm1
	punpckldq	%xmm1, %xmm0
	movd	%xmm0, -24(%rsp)
	movl	-24(%rsp), %eax
	je	.L37
	.p2align 4,,10
	.p2align 3
.L34:
	imull	%edi, %eax
	subl	$1, %edi
	jne	.L34
	rep
	ret
.L32:
	movl	$1, %eax
	ret
.L33:
	movl	$1, %eax
	.p2align 4,,2
	jmp	.L34
.L37:
	.p2align 4,,2
	ret
	.cfi_endproc
.LFE2:
	.size	tail_fact, .-tail_fact
	.section	.rodata.cst16,"aM",@progbits,16
	.align 16
.LC0:
	.long	1
	.long	1
	.long	1
	.long	1
	.align 16
.LC1:
	.long	1
	.long	2
	.long	3
	.long	4
	.align 16
.LC2:
	.long	4
	.long	4
	.long	4
	.long	4
	.align 16
.LC3:
	.long	-4
	.long	-4
	.long	-4
	.long	-4
	.ident	"GCC: (GNU) 4.7.2 20121109 (Red Hat 4.7.2-8)"
	.section	.note.GNU-stack,"",@progbits
