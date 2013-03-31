	.file	"integrate.c"
	.text
	.p2align 4,,15
	.globl	rk4
	.type	rk4, @function
rk4:
.LFB23:
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
	movsd	72(%rsp), %xmm3
	movapd	%xmm0, %xmm5
	mulsd	.LC0(%rip), %xmm2
	movsd	%xmm5, (%rsp)
	movapd	%xmm2, %xmm1
	movsd	%xmm2, 32(%rsp)
	addsd	%xmm2, %xmm3
	mulsd	%xmm0, %xmm1
	movsd	%xmm3, 16(%rsp)
	movapd	%xmm3, %xmm0
	addsd	56(%rsp), %xmm1
	call	*%rbx
	movsd	32(%rsp), %xmm2
	movapd	%xmm0, %xmm4
	movsd	16(%rsp), %xmm3
	movapd	%xmm2, %xmm1
	movsd	%xmm4, 32(%rsp)
	mulsd	%xmm0, %xmm1
	movapd	%xmm3, %xmm0
	addsd	56(%rsp), %xmm1
	call	*%rbx
	movsd	64(%rsp), %xmm1
	movapd	%xmm0, %xmm3
	mulsd	%xmm0, %xmm1
	movsd	72(%rsp), %xmm0
	movsd	%xmm3, 16(%rsp)
	addsd	64(%rsp), %xmm0
	addsd	56(%rsp), %xmm1
	call	*%rbx
	movsd	32(%rsp), %xmm4
	movsd	(%rsp), %xmm5
	movapd	%xmm4, %xmm1
	movsd	16(%rsp), %xmm3
	movsd	64(%rsp), %xmm2
	addsd	%xmm4, %xmm1
	addsd	%xmm3, %xmm3
	mulsd	.LC1(%rip), %xmm2
	addsd	%xmm5, %xmm1
	addsd	%xmm3, %xmm1
	addsd	%xmm0, %xmm1
	mulsd	%xmm1, %xmm2
	addsd	56(%rsp), %xmm2
	addq	$80, %rsp
	.cfi_def_cfa_offset 16
	popq	%rbx
	.cfi_def_cfa_offset 8
	movapd	%xmm2, %xmm0
	ret
	.cfi_endproc
.LFE23:
	.size	rk4, .-rk4
	.p2align 4,,15
	.globl	Stencil_5pt
	.type	Stencil_5pt, @function
Stencil_5pt:
.LFB24:
	.cfi_startproc
	movsd	.LC2(%rip), %xmm0
	ret
	.cfi_endproc
.LFE24:
	.size	Stencil_5pt, .-Stencil_5pt
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
