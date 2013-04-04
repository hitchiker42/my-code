	.file	"calculus.c"
	.text
	.p2align 4,,15
	.globl	rk4
	.type	rk4, @function
rk4:
.LFB32:
	.cfi_startproc
	pushq	%rbx
	.cfi_def_cfa_offset 16
	.cfi_offset 3, -16
	movq	%rdi, %rbx
	subq	$80, %rsp
	.cfi_def_cfa_offset 96
	movsd	%xmm1, 72(%rsp)
	movapd	%xmm0, %xmm1
	movsd	%xmm0, 56(%rsp)
	movsd	%xmm2, 64(%rsp)
	movsd	72(%rsp), %xmm0
	call	*%rdi
	movsd	64(%rsp), %xmm2
	movapd	%xmm0, %xmm1
	movsd	72(%rsp), %xmm3
	mulsd	.LC0(%rip), %xmm2
	movapd	%xmm0, %xmm5
	movsd	%xmm5, (%rsp)
	mulsd	%xmm2, %xmm1
	addsd	%xmm2, %xmm3
	movsd	%xmm2, 32(%rsp)
	addsd	56(%rsp), %xmm1
	movsd	%xmm3, 16(%rsp)
	movapd	%xmm3, %xmm0
	call	*%rbx
	movsd	32(%rsp), %xmm2
	movapd	%xmm0, %xmm1
	movapd	%xmm0, %xmm4
	movsd	16(%rsp), %xmm3
	mulsd	%xmm2, %xmm1
	movsd	%xmm4, 32(%rsp)
	movapd	%xmm3, %xmm0
	addsd	56(%rsp), %xmm1
	call	*%rbx
	movsd	64(%rsp), %xmm1
	movapd	%xmm0, %xmm3
	mulsd	%xmm0, %xmm1
	movsd	64(%rsp), %xmm0
	movsd	%xmm3, 16(%rsp)
	addsd	72(%rsp), %xmm0
	addsd	56(%rsp), %xmm1
	call	*%rbx
	movsd	(%rsp), %xmm5
	movapd	%xmm0, %xmm1
	movsd	32(%rsp), %xmm4
	movsd	16(%rsp), %xmm3
	addsd	%xmm5, %xmm1
	movsd	64(%rsp), %xmm0
	addsd	%xmm4, %xmm4
	addsd	%xmm3, %xmm3
	mulsd	.LC1(%rip), %xmm0
	addsd	%xmm4, %xmm1
	addsd	%xmm3, %xmm1
	mulsd	%xmm1, %xmm0
	addsd	56(%rsp), %xmm0
	addq	$80, %rsp
	.cfi_def_cfa_offset 16
	popq	%rbx
	.cfi_def_cfa_offset 8
	ret
	.cfi_endproc
.LFE32:
	.size	rk4, .-rk4
	.p2align 4,,15
	.globl	Stencil_5pt
	.type	Stencil_5pt, @function
Stencil_5pt:
.LFB33:
	.cfi_startproc
	movsd	.LC2(%rip), %xmm0
	ret
	.cfi_endproc
.LFE33:
	.size	Stencil_5pt, .-Stencil_5pt
	.p2align 4,,15
	.globl	secant_meth
	.type	secant_meth, @function
secant_meth:
.LFB34:
	.cfi_startproc
	pushq	%rbx
	.cfi_def_cfa_offset 16
	.cfi_offset 3, -16
	movapd	%xmm0, %xmm3
	movq	%rdi, %rbx
	subq	$48, %rsp
	.cfi_def_cfa_offset 64
	movsd	%xmm1, 32(%rsp)
	movsd	%xmm2, 40(%rsp)
	jmp	.L5
	.p2align 4,,10
	.p2align 3
.L6:
	movapd	%xmm1, %xmm3
.L5:
	movapd	%xmm3, %xmm0
	movsd	%xmm3, (%rsp)
	call	*%rbx
	movapd	%xmm0, %xmm2
	movsd	32(%rsp), %xmm0
	movsd	%xmm2, 16(%rsp)
	call	*%rbx
	movsd	(%rsp), %xmm3
	movsd	16(%rsp), %xmm2
	movapd	%xmm3, %xmm1
	subsd	%xmm2, %xmm0
	subsd	32(%rsp), %xmm1
	movsd	%xmm3, 32(%rsp)
	divsd	%xmm0, %xmm1
	mulsd	%xmm2, %xmm1
	addsd	%xmm3, %xmm1
	movapd	%xmm1, %xmm0
	subsd	%xmm3, %xmm0
	comisd	40(%rsp), %xmm0
	ja	.L6
	addq	$48, %rsp
	.cfi_def_cfa_offset 16
	movapd	%xmm1, %xmm0
	popq	%rbx
	.cfi_def_cfa_offset 8
	ret
	.cfi_endproc
.LFE34:
	.size	secant_meth, .-secant_meth
	.p2align 4,,15
	.globl	map
	.type	map, @function
map:
.LFB35:
	.cfi_startproc
	pushq	%r14
	.cfi_def_cfa_offset 16
	.cfi_offset 14, -16
	pushq	%r13
	.cfi_def_cfa_offset 24
	.cfi_offset 13, -24
	movq	%rdx, %r13
	pushq	%r12
	.cfi_def_cfa_offset 32
	.cfi_offset 12, -32
	movq	%rdi, %r12
	movslq	%esi, %rdi
	salq	$3, %rdi
	pushq	%rbp
	.cfi_def_cfa_offset 40
	.cfi_offset 6, -40
	pushq	%rbx
	.cfi_def_cfa_offset 48
	.cfi_offset 3, -48
	movl	%esi, %ebx
	call	malloc
	testl	%ebx, %ebx
	movq	%rax, %rbp
	jle	.L14
	subl	$1, %ebx
	leaq	8(,%rbx,8), %r14
	xorl	%ebx, %ebx
	.p2align 4,,10
	.p2align 3
.L11:
	movsd	(%r12,%rbx), %xmm0
	call	*%r13
	movsd	%xmm0, 0(%rbp,%rbx)
	addq	$8, %rbx
	cmpq	%r14, %rbx
	jne	.L11
.L14:
	popq	%rbx
	.cfi_def_cfa_offset 40
	movq	%rbp, %rax
	popq	%rbp
	.cfi_def_cfa_offset 32
	popq	%r12
	.cfi_def_cfa_offset 24
	popq	%r13
	.cfi_def_cfa_offset 16
	popq	%r14
	.cfi_def_cfa_offset 8
	ret
	.cfi_endproc
.LFE35:
	.size	map, .-map
	.section	.rodata.cst8,"aM",@progbits,8
	.align 8
.LC0:
	.long	0
	.long	1071644672
	.align 8
.LC1:
	.long	1431655765
	.long	1069897045
	.align 8
.LC2:
	.long	0
	.long	2146959360
	.ident	"GCC: (GNU) 4.7.2 20121109 (Red Hat 4.7.2-8)"
	.section	.note.GNU-stack,"",@progbits
