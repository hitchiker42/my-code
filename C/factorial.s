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
	.p2align 4,,15
	.globl	fact_acc
	.type	fact_acc, @function
fact_acc:
.LFB1:
	.cfi_startproc
	testl	%edi, %edi
	movl	%esi, %eax
	je	.L8
	.p2align 4,,10
	.p2align 3
.L12:
	imull	%edi, %eax
	subl	$1, %edi
	jne	.L12
.L8:
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
	movl	$1, %eax
	je	.L18
	.p2align 4,,10
	.p2align 3
.L17:
	imull	%edi, %eax
	subl	$1, %edi
	jne	.L17
	rep
	ret
.L18:
	.p2align 4,,5
	rep
	ret
	.cfi_endproc
.LFE2:
	.size	tail_fact, .-tail_fact
	.ident	"GCC: (GNU) 4.7.2 20121109 (Red Hat 4.7.2-8)"
	.section	.note.GNU-stack,"",@progbits
