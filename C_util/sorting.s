	.file	"sorting.c"
	.section	.text.unlikely,"ax",@progbits
.LCOLDB0:
	.text
.LHOTB0:
	.p2align 4,,15
	.type	sift_down, @function
sift_down:
.LFB71:
	.cfi_startproc
	leaq	1(%rsi,%rsi), %rcx
	cmpq	%rcx, %rdx
	jbe	.L1
	movq	(%rdi,%rsi,8), %r9
	jmp	.L7
	.p2align 4,,10
	.p2align 3
.L11:
	leaq	(%rdi,%rax,8), %rcx
	movq	(%rcx), %rsi
	movq	%rsi, (%r10)
	movq	%r9, (%rcx)
	leaq	1(%rax,%rax), %rcx
	cmpq	%rdx, %rcx
	jnb	.L1
	movq	%rax, %rsi
.L7:
	cmpq	%r9, (%rdi,%rcx,8)
	leaq	2(%rsi,%rsi), %r8
	movq	%rcx, %rax
	leaq	(%rdi,%rsi,8), %r10
	cmovbe	%rsi, %rax
	cmpq	%r8, %rdx
	jbe	.L5
	movq	(%rdi,%rax,8), %rcx
	cmpq	%rcx, (%rdi,%r8,8)
	cmova	%r8, %rax
.L5:
	cmpq	%rax, %rsi
	jne	.L11
.L1:
	rep ret
	.cfi_endproc
.LFE71:
	.size	sift_down, .-sift_down
	.section	.text.unlikely
.LCOLDE0:
	.text
.LHOTE0:
	.section	.rodata.str1.1,"aMS",@progbits,1
.LC1:
	.string	"Out of memory\n"
	.section	.text.unlikely
.LCOLDB2:
	.text
.LHOTB2:
	.p2align 4,,15
	.type	oom_fun, @function
oom_fun:
.LFB55:
	.cfi_startproc
	subq	$8, %rsp
	.cfi_def_cfa_offset 16
	movq	stderr(%rip), %rcx
	movl	$.LC1, %edi
	movl	$14, %edx
	movl	$1, %esi
	call	fwrite
	movl	$6, %edi
	addq	$8, %rsp
	.cfi_def_cfa_offset 8
	jmp	raise
	.cfi_endproc
.LFE55:
	.size	oom_fun, .-oom_fun
	.section	.text.unlikely
.LCOLDE2:
	.text
.LHOTE2:
	.section	.text.unlikely
.LCOLDB3:
	.text
.LHOTB3:
	.p2align 4,,15
	.globl	insertion_sort_u64
	.type	insertion_sort_u64, @function
insertion_sort_u64:
.LFB61:
	.cfi_startproc
	cmpq	$1, %rsi
	jbe	.L14
	movl	$1, %r9d
	movl	$1, %eax
	movl	$2, %r10d
	testl	%r9d, %r9d
	movq	(%rdi,%rax,8), %r8
	je	.L17
	.p2align 4,,10
	.p2align 3
.L29:
	leal	-1(%r9), %edx
	movq	%rdx, %rax
	movq	(%rdi,%rdx,8), %rdx
	cmpq	%rdx, %r8
	jbe	.L24
	jmp	.L25
	.p2align 4,,10
	.p2align 3
.L27:
	leal	-1(%rax), %edx
	movq	%rdx, %rcx
	movq	(%rdi,%rdx,8), %rdx
	cmpq	%rdx, %r8
	ja	.L26
	movl	%ecx, %eax
.L24:
	leal	1(%rax), %ecx
	testl	%eax, %eax
	movq	%rdx, (%rdi,%rcx,8)
	jne	.L27
	xorl	%eax, %eax
.L19:
	movq	%r8, (%rdi,%rax)
	movl	%r10d, %eax
	cmpq	%rax, %rsi
	jbe	.L28
.L21:
	addl	$1, %r9d
	addl	$1, %r10d
	movq	(%rdi,%rax,8), %r8
	testl	%r9d, %r9d
	jne	.L29
.L17:
	movq	%r8, (%rdi)
	movl	%r10d, %eax
	jmp	.L21
	.p2align 4,,10
	.p2align 3
.L26:
	salq	$3, %rax
	movq	%r8, (%rdi,%rax)
	movl	%r10d, %eax
	cmpq	%rax, %rsi
	ja	.L21
.L28:
	rep ret
.L14:
	rep ret
.L25:
	movl	%r9d, %eax
	salq	$3, %rax
	jmp	.L19
	.cfi_endproc
.LFE61:
	.size	insertion_sort_u64, .-insertion_sort_u64
	.section	.text.unlikely
.LCOLDE3:
	.text
.LHOTE3:
	.section	.text.unlikely
.LCOLDB4:
	.text
.LHOTB4:
	.p2align 4,,15
	.type	mergesort_internal_u64, @function
mergesort_internal_u64:
.LFB69:
	.cfi_startproc
	cmpq	$4, %rdx
	jbe	.L50
	pushq	%r15
	.cfi_def_cfa_offset 16
	.cfi_offset 15, -16
	pushq	%r14
	.cfi_def_cfa_offset 24
	.cfi_offset 14, -24
	pushq	%r13
	.cfi_def_cfa_offset 32
	.cfi_offset 13, -32
	pushq	%r12
	.cfi_def_cfa_offset 40
	.cfi_offset 12, -40
	movq	%rdi, %r12
	pushq	%rbp
	.cfi_def_cfa_offset 48
	.cfi_offset 6, -48
	pushq	%rbx
	.cfi_def_cfa_offset 56
	.cfi_offset 3, -56
	movq	%rdx, %rbx
	shrq	%rbx
	movq	%rdx, %rbp
	movq	%rsi, %r13
	movq	%rbp, %r14
	leaq	(%r12,%rbx,8), %r15
	subq	$8, %rsp
	.cfi_def_cfa_offset 64
	movq	%rbx, %rdx
	subq	%rbx, %r14
	call	mergesort_internal_u64
	movq	%r13, %rsi
	movq	%r14, %rdx
	movq	%r15, %rdi
	call	mergesort_internal_u64
	leaq	8(%r13), %rax
	movq	%r12, %rsi
	jmp	.L32
	.p2align 4,,10
	.p2align 3
.L51:
	addq	$8, %rsi
	movq	%rdi, -8(%rax)
	subq	$1, %rbx
.L34:
	addq	$8, %rax
.L32:
	testq	%rbx, %rbx
	leaq	-8(%rax), %rcx
	je	.L39
	testq	%r14, %r14
	je	.L39
	movq	(%r15), %rcx
	movq	(%rsi), %rdi
	cmpq	%rdi, %rcx
	ja	.L51
	addq	$8, %r15
	movq	%rcx, -8(%rax)
	subq	$1, %r14
	jmp	.L34
	.p2align 4,,10
	.p2align 3
.L39:
	testq	%rbx, %rbx
	jne	.L52
	testq	%r14, %r14
	jne	.L53
.L38:
	addq	$8, %rsp
	.cfi_def_cfa_offset 56
	leaq	0(,%rbp,8), %rdx
	movq	%r13, %rsi
	popq	%rbx
	.cfi_restore 3
	.cfi_def_cfa_offset 48
	movq	%r12, %rdi
	popq	%rbp
	.cfi_restore 6
	.cfi_def_cfa_offset 40
	popq	%r12
	.cfi_restore 12
	.cfi_def_cfa_offset 32
	popq	%r13
	.cfi_restore 13
	.cfi_def_cfa_offset 24
	popq	%r14
	.cfi_restore 14
	.cfi_def_cfa_offset 16
	popq	%r15
	.cfi_restore 15
	.cfi_def_cfa_offset 8
	jmp	memcpy
	.p2align 4,,10
	.p2align 3
.L50:
	movq	%rdx, %rsi
	jmp	insertion_sort_u64
	.p2align 4,,10
	.p2align 3
.L53:
	.cfi_def_cfa_offset 64
	.cfi_offset 3, -56
	.cfi_offset 6, -48
	.cfi_offset 12, -40
	.cfi_offset 13, -32
	.cfi_offset 14, -24
	.cfi_offset 15, -16
	leaq	0(,%r14,8), %rdx
	movq	%r15, %rsi
	movq	%rcx, %rdi
	call	memcpy
	jmp	.L38
	.p2align 4,,10
	.p2align 3
.L52:
	leaq	0(,%rbx,8), %rdx
	movq	%rcx, %rdi
	call	memcpy
	testq	%r14, %r14
	movq	%rax, %rcx
	je	.L38
	jmp	.L53
	.cfi_endproc
.LFE69:
	.size	mergesort_internal_u64, .-mergesort_internal_u64
	.section	.text.unlikely
.LCOLDE4:
	.text
.LHOTE4:
	.section	.text.unlikely
.LCOLDB5:
	.text
.LHOTB5:
	.p2align 4,,15
	.globl	insertion_sort_generic
	.type	insertion_sort_generic, @function
insertion_sort_generic:
.LFB62:
	.cfi_startproc
	cmpq	$1, %rsi
	jbe	.L68
	pushq	%r15
	.cfi_def_cfa_offset 16
	.cfi_offset 15, -16
	pushq	%r14
	.cfi_def_cfa_offset 24
	.cfi_offset 14, -24
	movl	$1, %eax
	pushq	%r13
	.cfi_def_cfa_offset 32
	.cfi_offset 13, -32
	pushq	%r12
	.cfi_def_cfa_offset 40
	.cfi_offset 12, -40
	movq	%rdi, %r15
	pushq	%rbp
	.cfi_def_cfa_offset 48
	.cfi_offset 6, -48
	pushq	%rbx
	.cfi_def_cfa_offset 56
	.cfi_offset 3, -56
	movq	%rdx, %r12
	subq	$24, %rsp
	.cfi_def_cfa_offset 80
	movq	(%r15,%rax,8), %rbp
	movl	$1, (%rsp)
	movl	(%rsp), %eax
	movq	%rsi, 8(%rsp)
	movl	$2, 4(%rsp)
	testl	%eax, %eax
	movl	%eax, %ebx
	jne	.L63
	jmp	.L71
	.p2align 4,,10
	.p2align 3
.L61:
	movq	(%r14), %rax
	movq	%rax, (%r15,%rbx,8)
	movl	%r13d, %ebx
	testl	%ebx, %ebx
	je	.L72
.L63:
	leal	-1(%rbx), %eax
	movq	%rbp, %rsi
	leaq	(%r15,%rax,8), %r14
	movq	%rax, %r13
	movq	(%r14), %rdi
	call	*%r12
	testl	%eax, %eax
	je	.L61
	movl	4(%rsp), %eax
	salq	$3, %rbx
	cmpq	%rax, 8(%rsp)
	movq	%rbp, (%r15,%rbx)
	jbe	.L73
.L62:
	movl	(%rsp), %edx
	movq	(%r15,%rax,8), %rbp
	addl	$1, 4(%rsp)
	leal	1(%rdx), %ecx
	movl	%ecx, (%rsp)
	movl	(%rsp), %eax
	testl	%eax, %eax
	movl	%eax, %ebx
	jne	.L63
.L71:
	movq	%rbp, (%r15)
	movl	4(%rsp), %eax
	jmp	.L62
	.p2align 4,,10
	.p2align 3
.L72:
	movl	4(%rsp), %eax
	xorl	%ebx, %ebx
	cmpq	%rax, 8(%rsp)
	movq	%rbp, (%r15,%rbx)
	ja	.L62
.L73:
	addq	$24, %rsp
	.cfi_def_cfa_offset 56
	popq	%rbx
	.cfi_restore 3
	.cfi_def_cfa_offset 48
	popq	%rbp
	.cfi_restore 6
	.cfi_def_cfa_offset 40
	popq	%r12
	.cfi_restore 12
	.cfi_def_cfa_offset 32
	popq	%r13
	.cfi_restore 13
	.cfi_def_cfa_offset 24
	popq	%r14
	.cfi_restore 14
	.cfi_def_cfa_offset 16
	popq	%r15
	.cfi_restore 15
	.cfi_def_cfa_offset 8
.L68:
	rep ret
	.cfi_endproc
.LFE62:
	.size	insertion_sort_generic, .-insertion_sort_generic
	.section	.text.unlikely
.LCOLDE5:
	.text
.LHOTE5:
	.section	.text.unlikely
.LCOLDB6:
	.text
.LHOTB6:
	.p2align 4,,15
	.type	mergesort_internal, @function
mergesort_internal:
.LFB67:
	.cfi_startproc
	pushq	%r15
	.cfi_def_cfa_offset 16
	.cfi_offset 15, -16
	pushq	%r14
	.cfi_def_cfa_offset 24
	.cfi_offset 14, -24
	pushq	%r13
	.cfi_def_cfa_offset 32
	.cfi_offset 13, -32
	pushq	%r12
	.cfi_def_cfa_offset 40
	.cfi_offset 12, -40
	movq	%rdx, %r13
	pushq	%rbp
	.cfi_def_cfa_offset 48
	.cfi_offset 6, -48
	pushq	%rbx
	.cfi_def_cfa_offset 56
	.cfi_offset 3, -56
	subq	$40, %rsp
	.cfi_def_cfa_offset 96
	cmpq	$4, %rdx
	jbe	.L94
	movq	%r13, %rbx
	movq	%rdi, %r14
	movq	%rdx, %rbp
	shrq	%rbx
	movq	%rsi, %r15
	movq	%rcx, %r12
	leaq	(%r14,%rbx,8), %r13
	movq	%rdx, 24(%rsp)
	subq	%rbx, %rbp
	movq	%rbx, %rdx
	movq	%rsi, 16(%rsp)
	movq	%rdi, 8(%rsp)
	call	mergesort_internal
	movq	%r15, %rsi
	movq	%r12, %rcx
	movq	%rbp, %rdx
	movq	%r13, %rdi
	leaq	8(%r15), %r15
	call	mergesort_internal
	jmp	.L76
	.p2align 4,,10
	.p2align 3
.L95:
	movq	(%r14), %rax
	subq	$1, %rbx
	addq	$8, %r14
	movq	%rax, -8(%r15)
.L78:
	addq	$8, %r15
.L76:
	testq	%rbx, %rbx
	leaq	-8(%r15), %r9
	je	.L83
	testq	%rbp, %rbp
	je	.L83
	movq	0(%r13), %rsi
	movq	(%r14), %rdi
	call	*%r12
	testl	%eax, %eax
	jne	.L95
	movq	0(%r13), %rax
	subq	$1, %rbp
	addq	$8, %r13
	movq	%rax, -8(%r15)
	jmp	.L78
	.p2align 4,,10
	.p2align 3
.L83:
	testq	%rbx, %rbx
	jne	.L96
	testq	%rbp, %rbp
	jne	.L97
.L82:
	movq	24(%rsp), %rdx
	movq	16(%rsp), %rsi
	movq	8(%rsp), %rdi
	addq	$40, %rsp
	.cfi_remember_state
	.cfi_def_cfa_offset 56
	popq	%rbx
	.cfi_def_cfa_offset 48
	popq	%rbp
	.cfi_def_cfa_offset 40
	popq	%r12
	.cfi_def_cfa_offset 32
	popq	%r13
	.cfi_def_cfa_offset 24
	popq	%r14
	.cfi_def_cfa_offset 16
	popq	%r15
	.cfi_def_cfa_offset 8
	salq	$3, %rdx
	jmp	memcpy
	.p2align 4,,10
	.p2align 3
.L94:
	.cfi_restore_state
	addq	$40, %rsp
	.cfi_remember_state
	.cfi_def_cfa_offset 56
	movq	%r13, %rsi
	movq	%rcx, %rdx
	popq	%rbx
	.cfi_def_cfa_offset 48
	popq	%rbp
	.cfi_def_cfa_offset 40
	popq	%r12
	.cfi_def_cfa_offset 32
	popq	%r13
	.cfi_def_cfa_offset 24
	popq	%r14
	.cfi_def_cfa_offset 16
	popq	%r15
	.cfi_def_cfa_offset 8
	jmp	insertion_sort_generic
	.p2align 4,,10
	.p2align 3
.L97:
	.cfi_restore_state
	leaq	0(,%rbp,8), %rdx
	movq	%r13, %rsi
	movq	%r9, %rdi
	call	memcpy
	jmp	.L82
	.p2align 4,,10
	.p2align 3
.L96:
	leaq	0(,%rbx,8), %rdx
	movq	%r9, %rdi
	movq	%r14, %rsi
	call	memcpy
	testq	%rbp, %rbp
	movq	%rax, %r9
	je	.L82
	jmp	.L97
	.cfi_endproc
.LFE67:
	.size	mergesort_internal, .-mergesort_internal
	.section	.text.unlikely
.LCOLDE6:
	.text
.LHOTE6:
	.section	.text.unlikely
.LCOLDB7:
	.text
.LHOTB7:
	.p2align 4,,15
	.globl	qsort_generic
	.type	qsort_generic, @function
qsort_generic:
.LFB64:
	.cfi_startproc
	pushq	%r15
	.cfi_def_cfa_offset 16
	.cfi_offset 15, -16
	pushq	%r14
	.cfi_def_cfa_offset 24
	.cfi_offset 14, -24
	pushq	%r13
	.cfi_def_cfa_offset 32
	.cfi_offset 13, -32
	pushq	%r12
	.cfi_def_cfa_offset 40
	.cfi_offset 12, -40
	movq	%rdx, %r12
	pushq	%rbp
	.cfi_def_cfa_offset 48
	.cfi_offset 6, -48
	pushq	%rbx
	.cfi_def_cfa_offset 56
	.cfi_offset 3, -56
	subq	$40, %rsp
	.cfi_def_cfa_offset 96
	cmpq	$4, %rsi
	movq	%rdi, 16(%rsp)
	movq	%rsi, 24(%rsp)
	jbe	.L105
.L109:
	movq	24(%rsp), %rax
	movq	16(%rsp), %rdx
	xorl	%ebp, %ebp
	leaq	-1(%rax), %rbx
	movq	%rbx, %rax
	shrq	$63, %rax
	addq	%rbx, %rax
	sarq	%rax
	movq	(%rdx,%rax,8), %rax
	cmpq	%rbp, %rax
	ja	.L100
	cmova	%rax, %rbp
.L100:
	addq	$1, %rbx
	movq	$-1, 8(%rsp)
	.p2align 4,,10
	.p2align 3
.L101:
	addq	$1, 8(%rsp)
	movq	16(%rsp), %r14
	movq	%rbp, %rsi
	movq	8(%rsp), %rax
	movq	(%r14,%rax,8), %rdi
	call	*%r12
	testl	%eax, %eax
	jne	.L101
	leaq	-1(%rbx), %r15
	leaq	(%r14,%r15,8), %r14
	jmp	.L103
	.p2align 4,,10
	.p2align 3
.L112:
	subq	$1, %r15
.L103:
	movq	(%r14), %rsi
	movq	%r14, %r13
	movq	%rbp, %rdi
	subq	$8, %r14
	movq	%r15, %rbx
	call	*%r12
	testl	%eax, %eax
	jne	.L112
	movq	8(%rsp), %rcx
	cmpq	%r15, %rcx
	jge	.L104
	movq	16(%rsp), %rsi
	movq	0(%r13), %rdx
	movq	(%rsi,%rcx,8), %rax
	movq	%rdx, (%rsi,%rcx,8)
	movq	%rax, 0(%r13)
	jmp	.L101
.L104:
	movslq	%r15d, %rbx
	movq	16(%rsp), %r15
	movq	%r12, %rdx
	leaq	1(%rbx), %rbp
	movq	%r15, %rdi
	movq	%rbp, %rsi
	call	qsort_generic
	movq	%rbx, %rcx
	leaq	(%r15,%rbp,8), %rdx
	notq	%rcx
	addq	%rcx, 24(%rsp)
	movq	24(%rsp), %rax
	movq	%rdx, 16(%rsp)
	cmpq	$4, %rax
	ja	.L109
.L105:
	movq	24(%rsp), %rsi
	movq	16(%rsp), %rdi
	addq	$40, %rsp
	.cfi_def_cfa_offset 56
	popq	%rbx
	.cfi_def_cfa_offset 48
	movq	%r12, %rdx
	popq	%rbp
	.cfi_def_cfa_offset 40
	popq	%r12
	.cfi_def_cfa_offset 32
	popq	%r13
	.cfi_def_cfa_offset 24
	popq	%r14
	.cfi_def_cfa_offset 16
	popq	%r15
	.cfi_def_cfa_offset 8
	jmp	insertion_sort_generic
	.cfi_endproc
.LFE64:
	.size	qsort_generic, .-qsort_generic
	.section	.text.unlikely
.LCOLDE7:
	.text
.LHOTE7:
	.section	.text.unlikely
.LCOLDB8:
	.text
.LHOTB8:
	.p2align 4,,15
	.globl	qsort_u64
	.type	qsort_u64, @function
qsort_u64:
.LFB65:
	.cfi_startproc
	pushq	%r13
	.cfi_def_cfa_offset 16
	.cfi_offset 13, -16
	pushq	%r12
	.cfi_def_cfa_offset 24
	.cfi_offset 12, -24
	movq	%rsi, %r12
	pushq	%rbp
	.cfi_def_cfa_offset 32
	.cfi_offset 6, -32
	pushq	%rbx
	.cfi_def_cfa_offset 40
	.cfi_offset 3, -40
	movq	%rdi, %rbp
	subq	$8, %rsp
	.cfi_def_cfa_offset 48
	cmpq	$4, %rsi
	jbe	.L120
.L124:
	leaq	-1(%r12), %rsi
	xorl	%edi, %edi
	movq	%rsi, %rax
	shrq	$63, %rax
	addq	%rsi, %rax
	sarq	%rax
	movq	0(%rbp,%rax,8), %rax
	cmpq	%rdi, %rax
	ja	.L115
	cmova	%rax, %rdi
.L115:
	addq	$1, %rsi
	movq	$-1, %r9
	.p2align 4,,10
	.p2align 3
.L116:
	addq	$1, %r9
	movq	0(%rbp,%r9,8), %r10
	cmpq	%rdi, %r10
	jb	.L116
	leaq	-1(%rsi), %rdx
	leaq	0(%rbp,%rdx,8), %rax
	jmp	.L118
	.p2align 4,,10
	.p2align 3
.L127:
	subq	$1, %rdx
.L118:
	movq	%rax, %r8
	subq	$8, %rax
	movq	8(%rax), %rcx
	movq	%rdx, %rsi
	cmpq	%rcx, %rdi
	jb	.L127
	cmpq	%rdx, %r9
	jge	.L119
	movq	%rcx, 0(%rbp,%r9,8)
	movq	%r10, (%r8)
	jmp	.L116
.L119:
	movslq	%edx, %rbx
	movq	%rbp, %rdi
	leaq	1(%rbx), %r13
	notq	%rbx
	addq	%rbx, %r12
	movq	%r13, %rsi
	leaq	0(%rbp,%r13,8), %rbp
	call	qsort_u64
	cmpq	$4, %r12
	ja	.L124
.L120:
	addq	$8, %rsp
	.cfi_def_cfa_offset 40
	movq	%r12, %rsi
	movq	%rbp, %rdi
	popq	%rbx
	.cfi_def_cfa_offset 32
	popq	%rbp
	.cfi_def_cfa_offset 24
	popq	%r12
	.cfi_def_cfa_offset 16
	popq	%r13
	.cfi_def_cfa_offset 8
	jmp	insertion_sort_u64
	.cfi_endproc
.LFE65:
	.size	qsort_u64, .-qsort_u64
	.section	.text.unlikely
.LCOLDE8:
	.text
.LHOTE8:
	.section	.text.unlikely
.LCOLDB9:
	.text
.LHOTB9:
	.p2align 4,,15
	.globl	mergesort_generic
	.type	mergesort_generic, @function
mergesort_generic:
.LFB68:
	.cfi_startproc
	pushq	%r14
	.cfi_def_cfa_offset 16
	.cfi_offset 14, -16
	pushq	%r13
	.cfi_def_cfa_offset 24
	.cfi_offset 13, -24
	movq	%rdi, %r13
	pushq	%r12
	.cfi_def_cfa_offset 32
	.cfi_offset 12, -32
	leaq	0(,%rsi,8), %r12
	pushq	%rbp
	.cfi_def_cfa_offset 40
	.cfi_offset 6, -40
	pushq	%rbx
	.cfi_def_cfa_offset 48
	.cfi_offset 3, -48
	movq	%rsi, %rbp
	movl	$1, %esi
	movq	%r12, %rdi
	movq	%rdx, %r14
	call	calloc
	testq	%rax, %rax
	movq	%rax, %rbx
	jne	.L129
	testq	%r12, %r12
	je	.L129
	call	oom_fun
.L129:
	movq	%r13, %rdi
	movq	%r14, %rcx
	movq	%rbp, %rdx
	movq	%rbx, %rsi
	call	mergesort_internal
	movq	%rbx, %rdi
	popq	%rbx
	.cfi_def_cfa_offset 40
	popq	%rbp
	.cfi_def_cfa_offset 32
	popq	%r12
	.cfi_def_cfa_offset 24
	popq	%r13
	.cfi_def_cfa_offset 16
	popq	%r14
	.cfi_def_cfa_offset 8
	jmp	free
	.cfi_endproc
.LFE68:
	.size	mergesort_generic, .-mergesort_generic
	.section	.text.unlikely
.LCOLDE9:
	.text
.LHOTE9:
	.section	.text.unlikely
.LCOLDB10:
	.text
.LHOTB10:
	.p2align 4,,15
	.globl	mergesort_u64
	.type	mergesort_u64, @function
mergesort_u64:
.LFB70:
	.cfi_startproc
	pushq	%r13
	.cfi_def_cfa_offset 16
	.cfi_offset 13, -16
	pushq	%r12
	.cfi_def_cfa_offset 24
	.cfi_offset 12, -24
	leaq	0(,%rsi,8), %r12
	pushq	%rbp
	.cfi_def_cfa_offset 32
	.cfi_offset 6, -32
	pushq	%rbx
	.cfi_def_cfa_offset 40
	.cfi_offset 3, -40
	movq	%rdi, %r13
	movq	%rsi, %rbp
	movq	%r12, %rdi
	movl	$1, %esi
	subq	$8, %rsp
	.cfi_def_cfa_offset 48
	call	calloc
	testq	%rax, %rax
	movq	%rax, %rbx
	jne	.L138
	testq	%r12, %r12
	je	.L138
	call	oom_fun
.L138:
	movq	%r13, %rdi
	movq	%rbp, %rdx
	movq	%rbx, %rsi
	call	mergesort_internal_u64
	addq	$8, %rsp
	.cfi_def_cfa_offset 40
	movq	%rbx, %rdi
	popq	%rbx
	.cfi_def_cfa_offset 32
	popq	%rbp
	.cfi_def_cfa_offset 24
	popq	%r12
	.cfi_def_cfa_offset 16
	popq	%r13
	.cfi_def_cfa_offset 8
	jmp	free
	.cfi_endproc
.LFE70:
	.size	mergesort_u64, .-mergesort_u64
	.section	.text.unlikely
.LCOLDE10:
	.text
.LHOTE10:
	.section	.text.unlikely
.LCOLDB11:
	.text
.LHOTB11:
	.p2align 4,,15
	.globl	heapsort_u64
	.type	heapsort_u64, @function
heapsort_u64:
.LFB73:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	leaq	-2(%rsi), %rbp
	pushq	%rbx
	.cfi_def_cfa_offset 24
	.cfi_offset 3, -24
	movq	%rsi, %r11
	movq	%rdi, %rbx
	shrq	%rbp
	.p2align 4,,10
	.p2align 3
.L147:
	movq	%rbp, %rsi
	movq	%r11, %rdx
	movq	%rbx, %rdi
	subq	$1, %rbp
	call	sift_down
	cmpq	$-1, %rbp
	jne	.L147
	subq	$1, %r11
	.p2align 4,,10
	.p2align 3
.L148:
	movq	(%rbx,%r11,8), %rax
	movq	(%rbx), %rdx
	xorl	%esi, %esi
	movq	%rbx, %rdi
	movq	%rdx, (%rbx,%r11,8)
	movq	%r11, %rdx
	movq	%rax, (%rbx)
	call	sift_down
	subq	$1, %r11
	testq	%r11, %r11
	jg	.L148
	popq	%rbx
	.cfi_def_cfa_offset 16
	popq	%rbp
	.cfi_def_cfa_offset 8
	ret
	.cfi_endproc
.LFE73:
	.size	heapsort_u64, .-heapsort_u64
	.section	.text.unlikely
.LCOLDE11:
	.text
.LHOTE11:
	.section	.text.unlikely
.LCOLDB12:
	.text
.LHOTB12:
	.p2align 4,,15
	.globl	radix_sort_u8_u64
	.type	radix_sort_u8_u64, @function
radix_sort_u8_u64:
.LFB74:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movl	$16384, %edx
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	pushq	%r15
	pushq	%r14
	pushq	%r13
	pushq	%r12
	pushq	%rbx
	subq	$120, %rsp
	.cfi_offset 15, -24
	.cfi_offset 14, -32
	.cfi_offset 13, -40
	.cfi_offset 12, -48
	.cfi_offset 3, -56
	movq	%rdi, -144(%rbp)
	movq	%rsi, -136(%rbp)
	subq	$16400, %rsp
	xorl	%esi, %esi
	leaq	15(%rsp), %rax
	movq	%rax, %rbx
	andq	$-16, %rbx
	leaq	2048(%rbx), %rcx
	movq	%rbx, %rdi
	leaq	4096(%rbx), %r12
	leaq	6144(%rbx), %r13
	leaq	8192(%rbx), %r14
	leaq	10240(%rbx), %r15
	movq	%rcx, -152(%rbp)
	leaq	12288(%rbx), %rcx
	movq	%rcx, -120(%rbp)
	leaq	14336(%rbx), %rcx
	movq	%rcx, -128(%rbp)
	call	memset
	movq	-136(%rbp), %rdi
	movq	-144(%rbp), %rax
	testq	%rdi, %rdi
	leaq	(%rax,%rdi,8), %rcx
	je	.L156
	leaq	2048(%rbx), %rdi
	leaq	12288(%rbx), %r8
	leaq	14336(%rbx), %r9
	movq	%rax, %rdx
	.p2align 4,,10
	.p2align 3
.L175:
	movq	(%rdx), %rax
	addq	$8, %rdx
	movzbl	%al, %esi
	addq	$1, (%rbx,%rsi,8)
	movzbl	%ah, %esi
	addq	$1, (%rdi,%rsi,8)
	movq	%rax, %rsi
	shrq	$13, %rsi
	andl	$2040, %esi
	addq	$1, (%r12,%rsi)
	movq	%rax, %rsi
	shrq	$21, %rsi
	andl	$2040, %esi
	addq	$1, 0(%r13,%rsi)
	movq	%rax, %rsi
	shrq	$29, %rsi
	andl	$2040, %esi
	addq	$1, (%r14,%rsi)
	movq	%rax, %rsi
	shrq	$37, %rsi
	andl	$2040, %esi
	addq	$1, (%r15,%rsi)
	movq	%rax, %rsi
	shrq	$56, %rax
	shrq	$45, %rsi
	andl	$2040, %esi
	addq	$1, (%r8,%rsi)
	addq	$1, (%r9,%rax,8)
	cmpq	%rdx, %rcx
	jne	.L175
.L156:
	xorl	%eax, %eax
	xorl	%r11d, %r11d
	xorl	%r10d, %r10d
	xorl	%r9d, %r9d
	xorl	%r8d, %r8d
	xorl	%edi, %edi
	xorl	%esi, %esi
	xorl	%ecx, %ecx
	xorl	%edx, %edx
	movq	%r12, -112(%rbp)
	.p2align 4,,10
	.p2align 3
.L154:
	movq	%rdx, %r12
	addq	(%rbx,%rax,8), %r12
	movq	%rdx, (%rbx,%rax,8)
	movq	%rcx, %rdx
	addq	2048(%rbx,%rax,8), %rdx
	movq	%rcx, 2048(%rbx,%rax,8)
	movq	%rsi, %rcx
	movq	%rdx, -56(%rbp)
	movq	-112(%rbp), %rdx
	addq	(%rdx,%rax,8), %rcx
	movq	%rsi, (%rdx,%rax,8)
	movq	%rdi, %rsi
	movq	%r9, %rdx
	addq	0(%r13,%rax,8), %rsi
	movq	%rdi, 0(%r13,%rax,8)
	movq	%r8, %rdi
	addq	(%r14,%rax,8), %rdi
	movq	%r8, (%r14,%rax,8)
	addq	(%r15,%rax,8), %rdx
	movq	%r9, (%r15,%rax,8)
	movq	%r10, %r8
	movq	-120(%rbp), %r9
	movq	%rcx, -64(%rbp)
	movq	%rsi, -72(%rbp)
	movq	-56(%rbp), %rcx
	movq	%rdi, -80(%rbp)
	movq	-64(%rbp), %rsi
	addq	(%r9,%rax,8), %r8
	movq	%r10, (%r9,%rax,8)
	movq	%r11, %r9
	movq	-128(%rbp), %r10
	movq	%rdx, -88(%rbp)
	movq	%r12, %rdx
	movq	-72(%rbp), %rdi
	addq	(%r10,%rax,8), %r9
	movq	%r11, (%r10,%rax,8)
	addq	$1, %rax
	cmpq	$256, %rax
	movq	%r8, -96(%rbp)
	movq	-80(%rbp), %r8
	movq	-96(%rbp), %r10
	movq	%r9, -104(%rbp)
	movq	-88(%rbp), %r9
	movq	-104(%rbp), %r11
	jne	.L154
	movq	-136(%rbp), %rax
	movl	$1, %esi
	movq	-112(%rbp), %r12
	leaq	0(,%rax,8), %r10
	movq	%r10, %rdi
	movq	%r10, -56(%rbp)
	call	calloc
	testq	%rax, %rax
	movq	-56(%rbp), %r10
	jne	.L157
	testq	%r10, %r10
	je	.L157
	movq	%rax, -64(%rbp)
	call	oom_fun
	cmpq	$0, -136(%rbp)
	movq	-56(%rbp), %r10
	movq	-64(%rbp), %rax
	jne	.L168
.L158:
	movq	%rax, %rdi
	call	free
	leaq	-40(%rbp), %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	popq	%rbp
	.cfi_remember_state
	.cfi_def_cfa 7, 8
	ret
.L157:
	.cfi_restore_state
	cmpq	$0, -136(%rbp)
	je	.L158
.L168:
	movq	-144(%rbp), %rdi
	leaq	(%rdi,%r10), %r9
	movq	%rdi, %rdx
	.p2align 4,,10
	.p2align 3
.L159:
	movq	(%rdx), %rsi
	addq	$8, %rdx
	cmpq	%r9, %rdx
	movzbl	%sil, %ecx
	leaq	(%rbx,%rcx,8), %rdi
	movq	(%rdi), %rcx
	leaq	1(%rcx), %r8
	movq	%rsi, (%rax,%rcx,8)
	movq	%r8, (%rdi)
	jne	.L159
	leaq	(%rax,%r10), %r8
	movq	-152(%rbp), %r9
	movq	-144(%rbp), %r10
	movq	%rax, %rdx
	.p2align 4,,10
	.p2align 3
.L160:
	movq	(%rdx), %rbx
	addq	$8, %rdx
	cmpq	%r8, %rdx
	movzbl	%bh, %ecx
	leaq	(%r9,%rcx,8), %rsi
	movq	(%rsi), %rcx
	leaq	1(%rcx), %rdi
	movq	%rbx, (%r10,%rcx,8)
	movq	%rdi, (%rsi)
	jne	.L160
	movq	-144(%rbp), %r9
	movq	-136(%rbp), %r10
	xorl	%ecx, %ecx
	.p2align 4,,10
	.p2align 3
.L161:
	movq	(%r9,%rcx,8), %rdi
	addq	$1, %rcx
	movq	%rdi, %rdx
	shrq	$13, %rdx
	andl	$2040, %edx
	addq	%r12, %rdx
	cmpq	%rcx, %r10
	movq	(%rdx), %rsi
	leaq	1(%rsi), %r8
	movq	%rdi, (%rax,%rsi,8)
	movq	%r8, (%rdx)
	ja	.L161
	movq	-144(%rbp), %r9
	movq	-136(%rbp), %r10
	xorl	%ecx, %ecx
	.p2align 4,,10
	.p2align 3
.L162:
	movq	(%rax,%rcx,8), %rdi
	addq	$1, %rcx
	movq	%rdi, %rdx
	shrq	$21, %rdx
	andl	$2040, %edx
	addq	%r13, %rdx
	cmpq	%rcx, %r10
	movq	(%rdx), %rsi
	leaq	1(%rsi), %r8
	movq	%rdi, (%r9,%rsi,8)
	movq	%r8, (%rdx)
	ja	.L162
	movq	-144(%rbp), %r9
	movq	-136(%rbp), %r10
	xorl	%ecx, %ecx
	.p2align 4,,10
	.p2align 3
.L163:
	movq	(%r9,%rcx,8), %rdi
	addq	$1, %rcx
	movq	%rdi, %rdx
	shrq	$29, %rdx
	andl	$2040, %edx
	addq	%r14, %rdx
	cmpq	%rcx, %r10
	movq	(%rdx), %rsi
	leaq	1(%rsi), %r8
	movq	%rdi, (%rax,%rsi,8)
	movq	%r8, (%rdx)
	ja	.L163
	movq	-144(%rbp), %r9
	movq	-136(%rbp), %r10
	xorl	%ecx, %ecx
	.p2align 4,,10
	.p2align 3
.L164:
	movq	(%rax,%rcx,8), %rdi
	addq	$1, %rcx
	movq	%rdi, %rdx
	shrq	$37, %rdx
	andl	$2040, %edx
	addq	%r15, %rdx
	cmpq	%rcx, %r10
	movq	(%rdx), %rsi
	leaq	1(%rsi), %r8
	movq	%rdi, (%r9,%rsi,8)
	movq	%r8, (%rdx)
	ja	.L164
	movq	-120(%rbp), %r9
	movq	-144(%rbp), %r10
	xorl	%ecx, %ecx
	movq	-136(%rbp), %r11
	.p2align 4,,10
	.p2align 3
.L166:
	movq	(%r10,%rcx,8), %rdi
	addq	$1, %rcx
	movq	%rdi, %rdx
	shrq	$45, %rdx
	andl	$2040, %edx
	addq	%r9, %rdx
	cmpq	%rcx, %r11
	movq	(%rdx), %rsi
	leaq	1(%rsi), %r8
	movq	%rdi, (%rax,%rsi,8)
	movq	%r8, (%rdx)
	ja	.L166
	movq	-128(%rbp), %r9
	movq	-144(%rbp), %r10
	xorl	%edx, %edx
	movq	-136(%rbp), %r11
	.p2align 4,,10
	.p2align 3
.L167:
	movq	(%rax,%rdx,8), %rsi
	addq	$1, %rdx
	movq	%rsi, %rcx
	shrq	$56, %rcx
	cmpq	%rdx, %r11
	leaq	(%r9,%rcx,8), %rdi
	movq	(%rdi), %rcx
	leaq	1(%rcx), %r8
	movq	%rsi, (%r10,%rcx,8)
	movq	%r8, (%rdi)
	ja	.L167
	jmp	.L158
	.cfi_endproc
.LFE74:
	.size	radix_sort_u8_u64, .-radix_sort_u8_u64
	.section	.text.unlikely
.LCOLDE12:
	.text
.LHOTE12:
	.section	.text.unlikely
.LCOLDB13:
	.text
.LHOTB13:
	.p2align 4,,15
	.globl	radix_sort_compact_u64
	.type	radix_sort_compact_u64, @function
radix_sort_compact_u64:
.LFB75:
	.cfi_startproc
	pushq	%r14
	.cfi_def_cfa_offset 16
	.cfi_offset 14, -16
	pushq	%r13
	.cfi_def_cfa_offset 24
	.cfi_offset 13, -24
	movl	$16384, %edx
	pushq	%r12
	.cfi_def_cfa_offset 32
	.cfi_offset 12, -32
	pushq	%rbp
	.cfi_def_cfa_offset 40
	.cfi_offset 6, -40
	movq	%rsi, %rbp
	pushq	%rbx
	.cfi_def_cfa_offset 48
	.cfi_offset 3, -48
	movq	%rdi, %rbx
	xorl	%esi, %esi
	subq	$16448, %rsp
	.cfi_def_cfa_offset 16496
	leaq	64(%rsp), %rdi
	call	memset
	xorl	%eax, %eax
	movl	$8, %ecx
	movq	%rsp, %rdi
	testq	%rbp, %rbp
	movq	%rsp, %r9
	rep stosq
	je	.L204
	leaq	(%rbx,%rbp,8), %rsi
	movq	%rbx, %rcx
	.p2align 4,,10
	.p2align 3
.L194:
	movq	(%rcx), %rax
	addq	$8, %rcx
	movzbl	%al, %edx
	addq	$1, 64(%rsp,%rdx,8)
	movzbl	%ah, %edx
	addq	$1, 2112(%rsp,%rdx,8)
	movq	%rax, %rdx
	shrq	$16, %rdx
	movzbl	%dl, %edx
	addq	$1, 4160(%rsp,%rdx,8)
	movq	%rax, %rdx
	shrq	$24, %rdx
	movzbl	%dl, %edx
	addq	$1, 6208(%rsp,%rdx,8)
	movq	%rax, %rdx
	shrq	$32, %rdx
	movzbl	%dl, %edx
	addq	$1, 8256(%rsp,%rdx,8)
	movq	%rax, %rdx
	shrq	$40, %rdx
	movzbl	%dl, %edx
	addq	$1, 10304(%rsp,%rdx,8)
	movq	%rax, %rdx
	shrq	$56, %rax
	shrq	$48, %rdx
	movzbl	%dl, %edx
	addq	$1, 12352(%rsp,%rdx,8)
	addq	$1, 14400(%rsp,%rax,8)
	cmpq	%rcx, %rsi
	jne	.L194
	movq	64(%rsp), %rsi
.L193:
	leaq	64(%rsp), %r8
	leaq	64(%r9), %rdi
	xorl	%eax, %eax
	leaq	2048(%r8), %r10
	.p2align 4,,10
	.p2align 3
.L197:
	movq	%r9, %rdx
	movq	%r8, %rcx
	jmp	.L198
	.p2align 4,,10
	.p2align 3
.L195:
	movq	(%rcx), %rsi
	movq	(%rdx), %rax
.L198:
	movq	%rax, (%rcx)
	addq	%rsi, %rax
	addq	$8, %rdx
	movq	%rax, -8(%rdx)
	addq	$2048, %rcx
	cmpq	%rdx, %rdi
	jne	.L195
	addq	$8, %r8
	cmpq	%r8, %r10
	je	.L196
	movq	(%r8), %rsi
	movq	(%rsp), %rax
	jmp	.L197
.L196:
	leaq	0(,%rbp,8), %r12
	movl	$1, %esi
	movq	%r12, %rdi
	call	calloc
	testq	%rax, %rax
	movq	%rax, %r13
	jne	.L199
	testq	%r12, %r12
	je	.L199
	call	oom_fun
.L199:
	movq	%r13, %r10
	xorl	%ecx, %ecx
	xorl	%eax, %eax
	.p2align 4,,10
	.p2align 3
.L200:
	testq	%rbp, %rbp
	je	.L203
	movq	%rax, %r11
	leaq	(%rbx,%r12), %rdi
	movq	%rbx, %rsi
	salq	$8, %r11
	.p2align 4,,10
	.p2align 3
.L201:
	movq	(%rsi), %r9
	addq	$8, %rsi
	movq	%r9, %rdx
	shrq	%cl, %rdx
	movzbl	%dl, %edx
	addq	%r11, %rdx
	cmpq	%rsi, %rdi
	movq	64(%rsp,%rdx,8), %r8
	leaq	1(%r8), %r14
	movq	%r9, (%r10,%r8,8)
	movq	%r14, 64(%rsp,%rdx,8)
	jne	.L201
.L203:
	addq	$1, %rax
	addl	$8, %ecx
	movq	%r10, %rdx
	cmpq	$8, %rax
	movq	%rbx, %r10
	je	.L202
	movq	%rdx, %rbx
	jmp	.L200
.L202:
	addq	$16448, %rsp
	.cfi_remember_state
	.cfi_def_cfa_offset 48
	movq	%r13, %rdi
	popq	%rbx
	.cfi_def_cfa_offset 40
	popq	%rbp
	.cfi_def_cfa_offset 32
	popq	%r12
	.cfi_def_cfa_offset 24
	popq	%r13
	.cfi_def_cfa_offset 16
	popq	%r14
	.cfi_def_cfa_offset 8
	jmp	free
.L204:
	.cfi_restore_state
	xorl	%esi, %esi
	jmp	.L193
	.cfi_endproc
.LFE75:
	.size	radix_sort_compact_u64, .-radix_sort_compact_u64
	.section	.text.unlikely
.LCOLDE13:
	.text
.LHOTE13:
	.section	.text.unlikely
.LCOLDB14:
	.text
.LHOTB14:
	.p2align 4,,15
	.globl	radix_sort_keys
	.type	radix_sort_keys, @function
radix_sort_keys:
.LFB76:
	.cfi_startproc
	pushq	%r15
	.cfi_def_cfa_offset 16
	.cfi_offset 15, -16
	pushq	%r14
	.cfi_def_cfa_offset 24
	.cfi_offset 14, -24
	movq	%rdi, %r15
	pushq	%r13
	.cfi_def_cfa_offset 32
	.cfi_offset 13, -32
	pushq	%r12
	.cfi_def_cfa_offset 40
	.cfi_offset 12, -40
	movq	%rdx, %r12
	pushq	%rbp
	.cfi_def_cfa_offset 48
	.cfi_offset 6, -48
	pushq	%rbx
	.cfi_def_cfa_offset 56
	.cfi_offset 3, -56
	movl	$16384, %edx
	movq	%rsi, %rbx
	xorl	%esi, %esi
	subq	$16488, %rsp
	.cfi_def_cfa_offset 16544
	leaq	96(%rsp), %rdi
	leaq	32(%rsp), %rbp
	call	memset
	xorl	%eax, %eax
	movl	$8, %ecx
	movq	%rbp, %rdi
	testq	%rbx, %rbx
	rep stosq
	je	.L229
	xorl	%r13d, %r13d
	.p2align 4,,10
	.p2align 3
.L219:
	movq	(%r15,%r13,8), %rdi
	addq	$1, %r13
	call	*%r12
	movzbl	%al, %edx
	addq	$1, 96(%rsp,%rdx,8)
	movzbl	%ah, %edx
	addq	$1, 2144(%rsp,%rdx,8)
	movq	%rax, %rdx
	shrq	$16, %rdx
	movzbl	%dl, %edx
	addq	$1, 4192(%rsp,%rdx,8)
	movq	%rax, %rdx
	shrq	$24, %rdx
	movzbl	%dl, %edx
	addq	$1, 6240(%rsp,%rdx,8)
	movq	%rax, %rdx
	shrq	$32, %rdx
	movzbl	%dl, %edx
	addq	$1, 8288(%rsp,%rdx,8)
	movq	%rax, %rdx
	shrq	$40, %rdx
	movzbl	%dl, %edx
	addq	$1, 10336(%rsp,%rdx,8)
	movq	%rax, %rdx
	shrq	$56, %rax
	shrq	$48, %rdx
	movzbl	%dl, %edx
	addq	$1, 12384(%rsp,%rdx,8)
	addq	$1, 14432(%rsp,%rax,8)
	cmpq	%r13, %rbx
	jne	.L219
	movq	96(%rsp), %rsi
.L218:
	leaq	96(%rsp), %r9
	leaq	64(%rbp), %rdi
	xorl	%eax, %eax
	leaq	2048(%r9), %r10
	.p2align 4,,10
	.p2align 3
.L222:
	movq	%rbp, %rdx
	movq	%r9, %rcx
	jmp	.L223
	.p2align 4,,10
	.p2align 3
.L220:
	movq	(%rcx), %rsi
	movq	(%rdx), %rax
.L223:
	movq	%rax, (%rcx)
	addq	%rsi, %rax
	addq	$8, %rdx
	movq	%rax, -8(%rdx)
	addq	$2048, %rcx
	cmpq	%rdi, %rdx
	jne	.L220
	addq	$8, %r9
	cmpq	%r10, %r9
	je	.L221
	movq	(%r9), %rsi
	movq	32(%rsp), %rax
	jmp	.L222
.L221:
	leaq	0(,%rbx,8), %rbp
	movl	$1, %esi
	movq	%rbp, %rdi
	call	calloc
	testq	%rax, %rax
	movq	%rax, 24(%rsp)
	jne	.L224
	testq	%rbp, %rbp
	je	.L224
	call	oom_fun
.L224:
	movq	24(%rsp), %rbp
	movq	$0, 16(%rsp)
	.p2align 4,,10
	.p2align 3
.L225:
	testq	%rbx, %rbx
	je	.L228
	movq	16(%rsp), %r13
	xorl	%r14d, %r14d
	movl	%r13d, %eax
	salq	$8, %r13
	sall	$3, %eax
	movl	%eax, 12(%rsp)
	.p2align 4,,10
	.p2align 3
.L226:
	movq	(%r15,%r14,8), %rdi
	call	*%r12
	movzbl	12(%rsp), %ecx
	shrq	%cl, %rax
	movzbl	%al, %eax
	addq	%r13, %rax
	movq	96(%rsp,%rax,8), %rcx
	leaq	1(%rcx), %rsi
	movq	%rsi, 96(%rsp,%rax,8)
	movq	(%r15,%r14,8), %rax
	addq	$1, %r14
	cmpq	%r14, %rbx
	movq	%rax, 0(%rbp,%rcx,8)
	jne	.L226
.L228:
	addq	$1, 16(%rsp)
	movq	%rbp, %rax
	movq	%r15, %rbp
	movq	16(%rsp), %rdi
	cmpq	$8, %rdi
	je	.L227
	movq	%rax, %r15
	jmp	.L225
.L227:
	movq	24(%rsp), %rdi
	addq	$16488, %rsp
	.cfi_remember_state
	.cfi_def_cfa_offset 56
	popq	%rbx
	.cfi_def_cfa_offset 48
	popq	%rbp
	.cfi_def_cfa_offset 40
	popq	%r12
	.cfi_def_cfa_offset 32
	popq	%r13
	.cfi_def_cfa_offset 24
	popq	%r14
	.cfi_def_cfa_offset 16
	popq	%r15
	.cfi_def_cfa_offset 8
	jmp	free
.L229:
	.cfi_restore_state
	xorl	%esi, %esi
	jmp	.L218
	.cfi_endproc
.LFE76:
	.size	radix_sort_keys, .-radix_sort_keys
	.section	.text.unlikely
.LCOLDE14:
	.text
.LHOTE14:
	.section	.text.unlikely
.LCOLDB15:
	.text
.LHOTB15:
	.p2align 4,,15
	.globl	radix_sort_u16_u64
	.type	radix_sort_u16_u64, @function
radix_sort_u16_u64:
.LFB77:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movl	$2097152, %edx
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	pushq	%r15
	pushq	%r14
	pushq	%r13
	pushq	%r12
	.cfi_offset 15, -24
	.cfi_offset 14, -32
	.cfi_offset 13, -40
	.cfi_offset 12, -48
	movq	%rdi, %r14
	pushq	%rbx
	subq	$40, %rsp
	.cfi_offset 3, -56
	movq	%rsi, -56(%rbp)
	xorl	%esi, %esi
	subq	$2097168, %rsp
	leaq	15(%rsp), %rbx
	andq	$-16, %rbx
	movq	%rbx, %rdi
	leaq	524288(%rbx), %r15
	leaq	1048576(%rbx), %r12
	call	memset
	movq	-56(%rbp), %rax
	leaq	1572864(%rbx), %r13
	movq	%r14, %rcx
	testq	%rax, %rax
	leaq	(%r14,%rax,8), %rsi
	je	.L246
	.p2align 4,,10
	.p2align 3
.L261:
	movq	(%rcx), %rax
	addq	$8, %rcx
	movzwl	%ax, %edx
	addq	$1, (%rbx,%rdx,8)
	movq	%rax, %rdx
	shrq	$13, %rdx
	andl	$524280, %edx
	addq	$1, (%r15,%rdx)
	movq	%rax, %rdx
	shrq	$48, %rax
	shrq	$29, %rdx
	andl	$524280, %edx
	addq	$1, (%r12,%rdx)
	addq	$1, 0(%r13,%rax,8)
	cmpq	%rcx, %rsi
	jne	.L261
.L246:
	xorl	%eax, %eax
	xorl	%edi, %edi
	xorl	%esi, %esi
	xorl	%ecx, %ecx
	xorl	%edx, %edx
	.p2align 4,,10
	.p2align 3
.L244:
	movq	%rdx, %r11
	movq	%rcx, %r10
	addq	(%rbx,%rax,8), %r11
	addq	524288(%rbx,%rax,8), %r10
	movq	%rdx, (%rbx,%rax,8)
	movq	%rsi, %r9
	movq	%rcx, 524288(%rbx,%rax,8)
	movq	%rdi, %r8
	addq	(%r12,%rax,8), %r9
	movq	%rsi, (%r12,%rax,8)
	addq	0(%r13,%rax,8), %r8
	movq	%rdi, 0(%r13,%rax,8)
	addq	$1, %rax
	movq	%r11, %rdx
	cmpq	$65536, %rax
	movq	%r10, %rcx
	movq	%r9, %rsi
	movq	%r8, %rdi
	jne	.L244
	movq	-56(%rbp), %rax
	movl	$1, %esi
	leaq	0(,%rax,8), %r8
	movq	%r8, %rdi
	movq	%r8, -64(%rbp)
	call	calloc
	testq	%rax, %rax
	movq	-64(%rbp), %r8
	jne	.L247
	testq	%r8, %r8
	je	.L247
	movq	%rax, -72(%rbp)
	call	oom_fun
	cmpq	$0, -56(%rbp)
	movq	-64(%rbp), %r8
	movq	-72(%rbp), %rax
	jne	.L254
.L248:
	movq	%rax, %rdi
	call	free
	leaq	-40(%rbp), %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	popq	%rbp
	.cfi_remember_state
	.cfi_def_cfa 7, 8
	ret
.L247:
	.cfi_restore_state
	cmpq	$0, -56(%rbp)
	je	.L248
.L254:
	leaq	(%r14,%r8), %r10
	movq	%r14, %rdx
	.p2align 4,,10
	.p2align 3
.L249:
	movq	(%rdx), %rsi
	addq	$8, %rdx
	cmpq	%r10, %rdx
	movzwl	%si, %ecx
	leaq	(%rbx,%rcx,8), %rdi
	movq	(%rdi), %rcx
	leaq	1(%rcx), %r9
	movq	%rsi, (%rax,%rcx,8)
	movq	%r9, (%rdi)
	jne	.L249
	movq	%rax, %rcx
	addq	%rax, %r8
	.p2align 4,,10
	.p2align 3
.L250:
	movq	(%rcx), %rdi
	addq	$8, %rcx
	movq	%rdi, %rdx
	shrq	$13, %rdx
	andl	$524280, %edx
	addq	%r15, %rdx
	cmpq	%r8, %rcx
	movq	(%rdx), %rsi
	leaq	1(%rsi), %r9
	movq	%rdi, (%r14,%rsi,8)
	movq	%r9, (%rdx)
	jne	.L250
	xorl	%ecx, %ecx
	.p2align 4,,10
	.p2align 3
.L252:
	movq	(%r14,%rcx,8), %rdi
	addq	$1, %rcx
	movq	%rdi, %rdx
	shrq	$29, %rdx
	andl	$524280, %edx
	addq	%r12, %rdx
	cmpq	%rcx, -56(%rbp)
	movq	(%rdx), %rsi
	leaq	1(%rsi), %r8
	movq	%rdi, (%rax,%rsi,8)
	movq	%r8, (%rdx)
	ja	.L252
	xorl	%edx, %edx
	.p2align 4,,10
	.p2align 3
.L253:
	movq	(%rax,%rdx,8), %rsi
	addq	$1, %rdx
	movq	%rsi, %rcx
	shrq	$48, %rcx
	cmpq	%rdx, -56(%rbp)
	leaq	0(%r13,%rcx,8), %rdi
	movq	(%rdi), %rcx
	leaq	1(%rcx), %r8
	movq	%rsi, (%r14,%rcx,8)
	movq	%r8, (%rdi)
	ja	.L253
	jmp	.L248
	.cfi_endproc
.LFE77:
	.size	radix_sort_u16_u64, .-radix_sort_u16_u64
	.section	.text.unlikely
.LCOLDE15:
	.text
.LHOTE15:
	.section	.text.unlikely
.LCOLDB16:
	.text
.LHOTB16:
	.p2align 4,,15
	.globl	radix_sort_u64
	.type	radix_sort_u64, @function
radix_sort_u64:
.LFB78:
	.cfi_startproc
	jmp	radix_sort_u16_u64
	.cfi_endproc
.LFE78:
	.size	radix_sort_u64, .-radix_sort_u64
	.section	.text.unlikely
.LCOLDE16:
	.text
.LHOTE16:
	.ident	"GCC: (GNU) 5.3.0"
	.section	.note.GNU-stack,"",@progbits
