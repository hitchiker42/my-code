	.file	"postprocess_qualification.c"
	.section	.text.unlikely,"ax",@progbits
.LCOLDB0:
	.text
.LHOTB0:
	.p2align 4,,15
	.globl	deInterlaceInterpolateLinear_SSE2
	.type	deInterlaceInterpolateLinear_SSE2, @function
deInterlaceInterpolateLinear_SSE2:
.LFB2464:
	.cfi_startproc
	movslq	%esi, %rsi
#APP
# 17 "postprocess_qualification.c" 1
	leaq (%rdi, %rsi), %rax;
	leaq (%rax, %rsi, 4), %rcx;
	movdqa (%rdi), %xmm0;
	movdqa (%rax, %rsi), %xmm1;
	pavgb %xmm1, %xmm0;
	movdqa %xmm0, (%rax);
	movdqa (%rdi,%rsi,4), %xmm0
	;pavgb %xmm0, %xmm1;
	movdqa %xmm1, (%rax, %rsi, 2);
	movdqa (%rcx, %rsi, 2), %xmm1;
	pavgb %xmm1, %xmm0;
	movdqa %xmm0, (%rcx);
	movdqa (%rdi, %rsi, 8), %xmm0;
	pavgb %xmm0, %xmm1;
	movdqa %xmm1, (%rcx,%rsi, 4);
	
# 0 "" 2
#NO_APP
	ret
	.cfi_endproc
.LFE2464:
	.size	deInterlaceInterpolateLinear_SSE2, .-deInterlaceInterpolateLinear_SSE2
	.section	.text.unlikely
.LCOLDE0:
	.text
.LHOTE0:
	.section	.text.unlikely
.LCOLDB1:
	.text
.LHOTB1:
	.p2align 4,,15
	.globl	deInterlaceInterpolateLinear_SSE2_intrinsic
	.type	deInterlaceInterpolateLinear_SSE2_intrinsic, @function
deInterlaceInterpolateLinear_SSE2_intrinsic:
.LFB2465:
	.cfi_startproc
	movslq	%esi, %rsi
	vmovdqa	(%rdi), %xmm0
	leaq	(%rdi,%rsi), %rdx
	leaq	(%rdx,%rsi), %rax
	vpavgb	(%rax), %xmm0, %xmm0
	addq	%rsi, %rax
	vmovaps	%xmm0, (%rdx)
	leaq	(%rax,%rsi), %rcx
	leaq	(%rcx,%rsi), %rdx
	vmovdqa	(%rax), %xmm0
	vpavgb	(%rdx), %xmm0, %xmm0
	addq	%rsi, %rdx
	vmovaps	%xmm0, (%rcx)
	leaq	(%rdx,%rsi), %rcx
	leaq	(%rcx,%rsi), %rax
	vmovdqa	(%rdx), %xmm0
	vpavgb	(%rax), %xmm0, %xmm0
	vmovaps	%xmm0, (%rcx)
	addq	%rsi, %rax
	vmovdqa	(%rax), %xmm0
	vpavgb	(%rax,%rsi,2), %xmm0, %xmm0
	vmovaps	%xmm0, (%rax,%rsi)
	ret
	.cfi_endproc
.LFE2465:
	.size	deInterlaceInterpolateLinear_SSE2_intrinsic, .-deInterlaceInterpolateLinear_SSE2_intrinsic
	.section	.text.unlikely
.LCOLDE1:
	.text
.LHOTE1:
	.section	.text.unlikely
.LCOLDB2:
	.text
.LHOTB2:
	.p2align 4,,15
	.globl	deInterlaceInterpolateLinear_AVX2
	.type	deInterlaceInterpolateLinear_AVX2, @function
deInterlaceInterpolateLinear_AVX2:
.LFB2466:
	.cfi_startproc
	leaq	8(%rsp), %r10
	.cfi_def_cfa 10, 0
	leal	(%rsi,%rsi), %eax
	andq	$-32, %rsp
	vpcmpeqd	%ymm2, %ymm2, %ymm2
	pushq	-8(%r10)
	pushq	%rbp
	leal	(%rax,%rsi), %ecx
	vmovd	%eax, %xmm1
	cltq
	.cfi_escape 0x10,0x6,0x2,0x76,0
	movq	%rsp, %rbp
	pushq	%r10
	.cfi_escape 0xf,0x3,0x76,0x78,0x6
	leal	(%rcx,%rcx), %edx
	vmovdqa	%ymm2, %ymm5
	addq	%rdi, %rax
	movslq	%ecx, %rcx
	movl	%edx, -80(%rbp)
	leal	0(,%rsi,4), %edx
	vmovd	-80(%rbp), %xmm4
	vpinsrd	$1, %edx, %xmm4, %xmm0
	addl	%esi, %edx
	movslq	%edx, %rdx
	vpunpcklqdq	%xmm1, %xmm0, %xmm1
	vpgatherdq	%ymm2, (%rax,%xmm1,1), %ymm0
	vpgatherdq	%ymm5, (%rdi,%xmm1,1), %ymm3
	movslq	%esi, %rax
	vpavgb	%ymm0, %ymm3, %ymm6
	vmovdqa	%ymm6, -80(%rbp)
	movq	-80(%rbp), %r8
	vmovdqa	-80(%rbp), %ymm7
	movq	%r8, (%rdi,%rax,8)
	vmovdqa	%ymm7, -48(%rbp)
	movq	-40(%rbp), %rax
	movq	%rax, (%rdi,%rcx,8)
	movq	-32(%rbp), %rax
	movq	%rax, (%rdi,%rdx,8)
	leal	0(,%rsi,8), %eax
	movq	-24(%rbp), %rdx
	subl	%esi, %eax
	cltq
	movq	%rdx, (%rdi,%rax,8)
	vzeroupper
	popq	%r10
	.cfi_def_cfa 10, 0
	popq	%rbp
	leaq	-8(%r10), %rsp
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE2466:
	.size	deInterlaceInterpolateLinear_AVX2, .-deInterlaceInterpolateLinear_AVX2
	.section	.text.unlikely
.LCOLDE2:
	.text
.LHOTE2:
	.ident	"GCC: (GNU) 4.9.2"
	.section	.note.GNU-stack,"",@progbits
