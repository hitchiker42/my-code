	.file	"factorial.c"
	.text
	.p2align 4,,15
	.globl	factorial
	.type	factorial, @function
factorial:
.LFB0:
	.cfi_startproc
	testl	%edi, %edi
	jle	.L4
	addl	$1, %edi
	movl	$1, %eax
	movl	$1, %edx
	.p2align 4,,10
	.p2align 3
.L3:
	imull	%edx, %eax
	addl	$1, %edx
	cmpl	%edi, %edx
	jne	.L3
	rep
	ret
.L4:
	movl	$1, %eax
	ret
	.cfi_endproc
.LFE0:
	.size	factorial, .-factorial
	.ident	"GCC: (GNU) 4.7.2 20121109 (Red Hat 4.7.2-8)"
	.section	.note.GNU-stack,"",@progbits
