	.text
	.file	"hello_world.0.rs"
	.section	.text._ZN4main20h3dea27a4719b811ceaaE,"ax",@progbits
	.align	16, 0x90
	.type	_ZN4main20h3dea27a4719b811ceaaE,@function
_ZN4main20h3dea27a4719b811ceaaE:
	.cfi_startproc
	cmpq	%fs:112, %rsp
	ja	.LBB0_2
	movabsq	$56, %r10
	movabsq	$0, %r11
	callq	__morestack
	retq
.LBB0_2:
	subq	$56, %rsp
.Ltmp0:
	.cfi_def_cfa_offset 64
	movups	_ZN4main15__STATIC_FMTSTR20h1ac7ffe0fffdff90paaE(%rip), %xmm0
	movaps	%xmm0, (%rsp)
	xorps	%xmm0, %xmm0
	movaps	%xmm0, 16(%rsp)
	leaq	48(%rsp), %rax
	movq	%rax, 32(%rsp)
	movq	$0, 40(%rsp)
	leaq	(%rsp), %rdi
	callq	_ZN2io5stdio6_print20h6e6dd5907508e2db5TgE@PLT
	addq	$56, %rsp
	retq
.Ltmp1:
	.size	_ZN4main20h3dea27a4719b811ceaaE, .Ltmp1-_ZN4main20h3dea27a4719b811ceaaE
	.cfi_endproc

	.section	.text.main,"ax",@progbits
	.globl	main
	.align	16, 0x90
	.type	main,@function
main:
	.cfi_startproc
	movq	%rsi, %rax
	movq	%rdi, %rcx
	leaq	_ZN4main20h3dea27a4719b811ceaaE(%rip), %rdi
	movq	%rcx, %rsi
	movq	%rax, %rdx
	jmp	_ZN2rt10lang_start20hd30f854400cc61ecOlwE@PLT
.Ltmp2:
	.size	main, .Ltmp2-main
	.cfi_endproc

	.type	str1046,@object
	.section	.rodata.str1046,"a",@progbits
str1046:
	.ascii	"Hello, World!\n"
	.size	str1046, 14

	.type	ref1047,@object
	.section	.data.rel.ro.local.ref1047,"aw",@progbits
	.align	8
ref1047:
	.quad	str1046
	.quad	14
	.size	ref1047, 16

	.type	_ZN4main15__STATIC_FMTSTR20h1ac7ffe0fffdff90paaE,@object
	.section	.data.rel.ro.local._ZN4main15__STATIC_FMTSTR20h1ac7ffe0fffdff90paaE,"aw",@progbits
	.align	8
_ZN4main15__STATIC_FMTSTR20h1ac7ffe0fffdff90paaE:
	.quad	ref1047
	.quad	1
	.size	_ZN4main15__STATIC_FMTSTR20h1ac7ffe0fffdff90paaE, 16


	.section	".note.GNU-stack","",@progbits
