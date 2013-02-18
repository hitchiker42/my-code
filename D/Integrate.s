	.file	"Integrate.d"
	.section	.rodata.cst8,"aM",@progbits,8
	.align	8
.LCPI0_0:
	.quad	9222246136947933184
.LCPI0_1:
	.quad	4611686018427387904
.LCPI0_2:
	.quad	4618441417868443648
	.text
	.globl	_D9Integrate3Rk4FdAdPFddZdYAAd
	.align	16, 0x90
	.type	_D9Integrate3Rk4FdAdPFddZdYAAd,@function
_D9Integrate3Rk4FdAdPFddZdYAAd:
	.cfi_startproc
	subq	$408, %rsp
.Ltmp1:
	.cfi_def_cfa_offset 416
	vxorps	%xmm1, %xmm1, %xmm1
	vmovsd	.LCPI0_0, %xmm2
	vmovsd	%xmm0, 400(%rsp)
	movq	%r8, 392(%rsp)
	movq	%rcx, 384(%rsp)
	movq	%r9, 376(%rsp)
	movq	%rdx, 368(%rsp)
	movq	%rsi, 360(%rsp)
	movq	%rdi, 352(%rsp)
	movl	$0, 348(%rsp)
	movl	$0, 344(%rsp)
	movl	$1, 340(%rsp)
	vmovsd	%xmm2, 328(%rsp)
	vmovsd	%xmm2, 320(%rsp)
	vmovsd	%xmm2, 312(%rsp)
	vmovsd	%xmm2, 304(%rsp)
	vmovsd	%xmm1, 296(%rsp)
	movl	$0, 348(%rsp)
.LBB0_1:
	movslq	348(%rsp), %rax
	cmpq	352(%rsp), %rax
	jae	.LBB0_13
	movslq	348(%rsp), %rax
	cmpq	352(%rsp), %rax
	movq	%rax, 272(%rsp)
	jb	.LBB0_4
	leaq	_D9Integrate8__ModuleZ, %rax
	movl	$6, %esi
	movq	%rax, %rdi
	callq	_d_array_bounds
.LBB0_4:
	movq	360(%rsp), %rax
	movq	272(%rsp), %rcx
	movq	(%rax,%rcx,8), %rax
	movl	$_D10TypeInfo_i6__initZ, %edi
	movq	%rax, %rsi
	callq	_D6object8opEqualsFC6ObjectC6ObjectZb
	testb	$1, %al
	jne	.LBB0_5
	jmp	.LBB0_6
.LBB0_5:
	leaq	368(%rsp), %rdi
	callq	_D4core6vararg13__T6va_argTiZ6va_argFKPvZi
	movl	%eax, 340(%rsp)
	jmp	.LBB0_11
.LBB0_6:
	movslq	348(%rsp), %rax
	cmpq	352(%rsp), %rax
	movq	%rax, 264(%rsp)
	jb	.LBB0_8
	leaq	_D9Integrate8__ModuleZ, %rax
	movl	$9, %esi
	movq	%rax, %rdi
	callq	_d_array_bounds
.LBB0_8:
	movq	360(%rsp), %rax
	movq	264(%rsp), %rcx
	movq	(%rax,%rcx,8), %rax
	movl	$_D10TypeInfo_d6__initZ, %edi
	movq	%rax, %rsi
	callq	_D6object8opEqualsFC6ObjectC6ObjectZb
	testb	$1, %al
	jne	.LBB0_9
	jmp	.LBB0_10
.LBB0_9:
	leaq	368(%rsp), %rdi
	callq	_D4core6vararg13__T6va_argTdZ6va_argFKPvZd
	vmovsd	%xmm0, 296(%rsp)
.LBB0_10:
	jmp	.LBB0_11
.LBB0_11:
	jmp	.LBB0_12
.LBB0_12:
	movl	348(%rsp), %eax
	addl	$1, %eax
	movl	%eax, 348(%rsp)
	jmp	.LBB0_1
.LBB0_13:
	movabsq	$0, %rax
	movq	$0, 288(%rsp)
	movq	$0, 280(%rsp)
	cmpq	280(%rsp), %rax
	jb	.LBB0_15
	leaq	_D9Integrate8__ModuleZ, %rax
	movl	$14, %esi
	movq	%rax, %rdi
	callq	_d_array_bounds
.LBB0_15:
	movq	288(%rsp), %rax
	movq	384(%rsp), %rcx
	movq	392(%rsp), %rdx
	movq	%rdx, 8(%rax)
	movq	%rcx, (%rax)
	movl	$0, 348(%rsp)
.LBB0_16:
	movl	340(%rsp), %eax
	subl	$1, %eax
	cmpl	%eax, 348(%rsp)
	jge	.LBB0_47
	movl	$0, 344(%rsp)
.LBB0_18:
	movslq	344(%rsp), %rax
	cmpq	384(%rsp), %rax
	jae	.LBB0_45
	movq	376(%rsp), %rax
	movslq	348(%rsp), %rcx
	cmpq	280(%rsp), %rcx
	movq	%rax, 256(%rsp)
	movq	%rcx, 248(%rsp)
	jb	.LBB0_21
	leaq	_D9Integrate8__ModuleZ, %rax
	movl	$17, %esi
	movq	%rax, %rdi
	callq	_d_array_bounds
.LBB0_21:
	movq	288(%rsp), %rax
	movq	248(%rsp), %rcx
	shlq	$4, %rcx
	addq	%rcx, %rax
	movslq	344(%rsp), %rcx
	cmpq	(%rax), %rcx
	movq	%rax, 240(%rsp)
	movq	%rcx, 232(%rsp)
	jb	.LBB0_23
	leaq	_D9Integrate8__ModuleZ, %rax
	movl	$17, %esi
	movq	%rax, %rdi
	callq	_d_array_bounds
.LBB0_23:
	movabsq	$2, %rax
	vmovsd	.LCPI0_1, %xmm0
	movq	240(%rsp), %rcx
	movq	8(%rcx), %rdx
	vmovsd	296(%rsp), %xmm1
	movq	232(%rsp), %rsi
	vmovsd	(%rdx,%rsi,8), %xmm2
	vmovsd	%xmm0, 224(%rsp)
	vmovaps	%xmm2, %xmm0
	movq	256(%rsp), %rdx
	movq	%rax, 216(%rsp)
	callq	*%rdx
	vmovsd	400(%rsp), %xmm1
	vmulsd	%xmm0, %xmm1, %xmm0
	vmovsd	%xmm0, 328(%rsp)
	movq	376(%rsp), %rax
	vmovsd	400(%rsp), %xmm0
	vmovsd	224(%rsp), %xmm1
	vdivsd	%xmm1, %xmm0, %xmm0
	vmovsd	296(%rsp), %xmm2
	vaddsd	%xmm0, %xmm2, %xmm1
	movslq	348(%rsp), %rcx
	cmpq	280(%rsp), %rcx
	movq	%rax, 208(%rsp)
	vmovsd	%xmm1, 200(%rsp)
	movq	%rcx, 192(%rsp)
	jb	.LBB0_25
	leaq	_D9Integrate8__ModuleZ, %rax
	movl	$18, %esi
	movq	%rax, %rdi
	callq	_d_array_bounds
.LBB0_25:
	movq	288(%rsp), %rax
	movq	192(%rsp), %rcx
	shlq	$4, %rcx
	addq	%rcx, %rax
	movslq	344(%rsp), %rcx
	cmpq	(%rax), %rcx
	movq	%rax, 184(%rsp)
	movq	%rcx, 176(%rsp)
	jb	.LBB0_27
	leaq	_D9Integrate8__ModuleZ, %rax
	movl	$18, %esi
	movq	%rax, %rdi
	callq	_d_array_bounds
.LBB0_27:
	movabsq	$2, %rax
	vmovsd	.LCPI0_1, %xmm0
	movq	184(%rsp), %rcx
	movq	8(%rcx), %rdx
	vmovsd	328(%rsp), %xmm1
	vdivsd	%xmm0, %xmm1, %xmm1
	movq	176(%rsp), %rsi
	vmovsd	(%rdx,%rsi,8), %xmm2
	vaddsd	%xmm1, %xmm2, %xmm1
	vmovsd	%xmm0, 168(%rsp)
	vmovaps	%xmm1, %xmm0
	vmovsd	200(%rsp), %xmm1
	movq	208(%rsp), %rdx
	movq	%rax, 160(%rsp)
	callq	*%rdx
	vmovsd	400(%rsp), %xmm1
	vmulsd	%xmm0, %xmm1, %xmm0
	vmovsd	%xmm0, 320(%rsp)
	movq	376(%rsp), %rax
	vmovsd	400(%rsp), %xmm0
	vmovsd	168(%rsp), %xmm1
	vdivsd	%xmm1, %xmm0, %xmm0
	vmovsd	296(%rsp), %xmm2
	vaddsd	%xmm0, %xmm2, %xmm1
	movslq	348(%rsp), %rcx
	cmpq	280(%rsp), %rcx
	movq	%rax, 152(%rsp)
	vmovsd	%xmm1, 144(%rsp)
	movq	%rcx, 136(%rsp)
	jb	.LBB0_29
	leaq	_D9Integrate8__ModuleZ, %rax
	movl	$19, %esi
	movq	%rax, %rdi
	callq	_d_array_bounds
.LBB0_29:
	movq	288(%rsp), %rax
	movq	136(%rsp), %rcx
	shlq	$4, %rcx
	addq	%rcx, %rax
	movslq	344(%rsp), %rcx
	cmpq	(%rax), %rcx
	movq	%rax, 128(%rsp)
	movq	%rcx, 120(%rsp)
	jb	.LBB0_31
	leaq	_D9Integrate8__ModuleZ, %rax
	movl	$19, %esi
	movq	%rax, %rdi
	callq	_d_array_bounds
.LBB0_31:
	movabsq	$2, %rax
	vmovsd	.LCPI0_1, %xmm0
	movq	128(%rsp), %rcx
	movq	8(%rcx), %rdx
	vmovsd	320(%rsp), %xmm1
	vdivsd	%xmm0, %xmm1, %xmm0
	movq	120(%rsp), %rsi
	vmovsd	(%rdx,%rsi,8), %xmm1
	vaddsd	%xmm0, %xmm1, %xmm0
	vmovsd	144(%rsp), %xmm1
	movq	152(%rsp), %rdx
	movq	%rax, 112(%rsp)
	callq	*%rdx
	vmovsd	400(%rsp), %xmm1
	vmulsd	%xmm0, %xmm1, %xmm0
	vmovsd	%xmm0, 312(%rsp)
	movq	376(%rsp), %rax
	vmovsd	296(%rsp), %xmm0
	vaddsd	400(%rsp), %xmm0, %xmm1
	movslq	348(%rsp), %rcx
	cmpq	280(%rsp), %rcx
	movq	%rax, 104(%rsp)
	vmovsd	%xmm1, 96(%rsp)
	movq	%rcx, 88(%rsp)
	jb	.LBB0_33
	leaq	_D9Integrate8__ModuleZ, %rax
	movl	$20, %esi
	movq	%rax, %rdi
	callq	_d_array_bounds
.LBB0_33:
	movq	288(%rsp), %rax
	movq	88(%rsp), %rcx
	shlq	$4, %rcx
	addq	%rcx, %rax
	movslq	344(%rsp), %rcx
	cmpq	(%rax), %rcx
	movq	%rax, 80(%rsp)
	movq	%rcx, 72(%rsp)
	jb	.LBB0_35
	leaq	_D9Integrate8__ModuleZ, %rax
	movl	$20, %esi
	movq	%rax, %rdi
	callq	_d_array_bounds
.LBB0_35:
	movq	80(%rsp), %rax
	movq	8(%rax), %rcx
	movq	72(%rsp), %rdx
	vmovsd	(%rcx,%rdx,8), %xmm0
	vaddsd	312(%rsp), %xmm0, %xmm0
	vmovsd	96(%rsp), %xmm1
	movq	104(%rsp), %rcx
	callq	*%rcx
	vmovsd	400(%rsp), %xmm1
	vmulsd	%xmm0, %xmm1, %xmm0
	vmovsd	%xmm0, 304(%rsp)
	movl	348(%rsp), %esi
	addl	348(%rsp), %esi
	movslq	%esi, %rax
	cmpq	280(%rsp), %rax
	movq	%rax, 64(%rsp)
	jb	.LBB0_37
	leaq	_D9Integrate8__ModuleZ, %rax
	movl	$21, %esi
	movq	%rax, %rdi
	callq	_d_array_bounds
.LBB0_37:
	movq	288(%rsp), %rax
	movq	64(%rsp), %rcx
	shlq	$4, %rcx
	addq	%rcx, %rax
	movslq	344(%rsp), %rcx
	cmpq	(%rax), %rcx
	movq	%rax, 56(%rsp)
	movq	%rcx, 48(%rsp)
	jb	.LBB0_39
	leaq	_D9Integrate8__ModuleZ, %rax
	movl	$21, %esi
	movq	%rax, %rdi
	callq	_d_array_bounds
.LBB0_39:
	movq	56(%rsp), %rax
	movq	8(%rax), %rcx
	movq	48(%rsp), %rdx
	shlq	$3, %rdx
	addq	%rdx, %rcx
	movslq	348(%rsp), %rdx
	cmpq	280(%rsp), %rdx
	movq	%rcx, 40(%rsp)
	movq	%rdx, 32(%rsp)
	jb	.LBB0_41
	leaq	_D9Integrate8__ModuleZ, %rax
	movl	$21, %esi
	movq	%rax, %rdi
	callq	_d_array_bounds
.LBB0_41:
	movq	288(%rsp), %rax
	movq	32(%rsp), %rcx
	shlq	$4, %rcx
	addq	%rcx, %rax
	movslq	344(%rsp), %rcx
	cmpq	(%rax), %rcx
	movq	%rax, 24(%rsp)
	movq	%rcx, 16(%rsp)
	jb	.LBB0_43
	leaq	_D9Integrate8__ModuleZ, %rax
	movl	$21, %esi
	movq	%rax, %rdi
	callq	_d_array_bounds
.LBB0_43:
	movabsq	$2, %rax
	vmovsd	.LCPI0_1, %xmm0
	movabsq	$6, %rcx
	vmovsd	.LCPI0_2, %xmm1
	movq	24(%rsp), %rdx
	movq	8(%rdx), %rsi
	vmovsd	400(%rsp), %xmm2
	vdivsd	%xmm1, %xmm2, %xmm1
	vmulsd	320(%rsp), %xmm0, %xmm2
	vmovsd	328(%rsp), %xmm3
	vaddsd	%xmm2, %xmm3, %xmm2
	vmulsd	312(%rsp), %xmm0, %xmm0
	vaddsd	%xmm0, %xmm2, %xmm0
	vaddsd	304(%rsp), %xmm0, %xmm0
	vmulsd	%xmm0, %xmm1, %xmm0
	movq	16(%rsp), %rdi
	vmovsd	(%rsi,%rdi,8), %xmm1
	vaddsd	%xmm0, %xmm1, %xmm0
	movq	40(%rsp), %rsi
	vmovsd	%xmm0, (%rsi)
	movq	%rax, 8(%rsp)
	movq	%rcx, (%rsp)
	movl	344(%rsp), %eax
	addl	$1, %eax
	movl	%eax, 344(%rsp)
	jmp	.LBB0_18
.LBB0_45:
	vmovsd	296(%rsp), %xmm0
	vaddsd	400(%rsp), %xmm0, %xmm0
	vmovsd	%xmm0, 296(%rsp)
	movl	348(%rsp), %eax
	addl	$1, %eax
	movl	%eax, 348(%rsp)
	jmp	.LBB0_16
.LBB0_47:
	movq	280(%rsp), %rax
	movq	288(%rsp), %rdx
	addq	$408, %rsp
	ret
.Ltmp2:
	.size	_D9Integrate3Rk4FdAdPFddZdYAAd, .Ltmp2-_D9Integrate3Rk4FdAdPFddZdYAAd
	.cfi_endproc

	.section	.text._D4core6vararg13__T6va_argTiZ6va_argFKPvZi,"axG",@progbits,_D4core6vararg13__T6va_argTiZ6va_argFKPvZi,comdat
	.weak	_D4core6vararg13__T6va_argTiZ6va_argFKPvZi
	.align	16, 0x90
	.type	_D4core6vararg13__T6va_argTiZ6va_argFKPvZi,@function
_D4core6vararg13__T6va_argTiZ6va_argFKPvZi:
	.cfi_startproc
	movq	(%rdi), %rax
	movl	(%rax), %ecx
	movl	%ecx, -4(%rsp)
	movq	(%rdi), %rax
	addq	$8, %rax
	movq	%rax, (%rdi)
	movl	-4(%rsp), %eax
	ret
.Ltmp3:
	.size	_D4core6vararg13__T6va_argTiZ6va_argFKPvZi, .Ltmp3-_D4core6vararg13__T6va_argTiZ6va_argFKPvZi
	.cfi_endproc

	.section	.text._D4core6vararg13__T6va_argTdZ6va_argFKPvZd,"axG",@progbits,_D4core6vararg13__T6va_argTdZ6va_argFKPvZd,comdat
	.weak	_D4core6vararg13__T6va_argTdZ6va_argFKPvZd
	.align	16, 0x90
	.type	_D4core6vararg13__T6va_argTdZ6va_argFKPvZd,@function
_D4core6vararg13__T6va_argTdZ6va_argFKPvZd:
	.cfi_startproc
	movq	(%rdi), %rax
	vmovsd	(%rax), %xmm0
	vmovsd	%xmm0, -8(%rsp)
	movq	(%rdi), %rax
	addq	$8, %rax
	movq	%rax, (%rdi)
	vmovsd	-8(%rsp), %xmm0
	ret
.Ltmp4:
	.size	_D4core6vararg13__T6va_argTdZ6va_argFKPvZd, .Ltmp4-_D4core6vararg13__T6va_argTdZ6va_argFKPvZd
	.cfi_endproc

	.text
	.align	16, 0x90
	.type	_D9Integrate16__moduleinfoCtorZ,@function
_D9Integrate16__moduleinfoCtorZ:
	.cfi_startproc
	leaq	_D9Integrate11__moduleRefZ, %rax
	movq	_Dmodule_ref, %rcx
	movq	%rcx, _D9Integrate11__moduleRefZ
	movq	%rax, _Dmodule_ref
	ret
.Ltmp5:
	.size	_D9Integrate16__moduleinfoCtorZ, .Ltmp5-_D9Integrate16__moduleinfoCtorZ
	.cfi_endproc

	.type	_D3std6traits15__T8DemangleTkZ8Demangle6__initZ,@object
	.section	.rodata._D3std6traits15__T8DemangleTkZ8Demangle6__initZ,"aG",@progbits,_D3std6traits15__T8DemangleTkZ8Demangle6__initZ,comdat
	.weak	_D3std6traits15__T8DemangleTkZ8Demangle6__initZ
	.align	8
_D3std6traits15__T8DemangleTkZ8Demangle6__initZ:
	.zero	24
	.size	_D3std6traits15__T8DemangleTkZ8Demangle6__initZ, 24

	.type	_D47TypeInfo_S3std6traits15__T8DemangleTkZ8Demangle6__initZ,@object
	.section	.rodata._D47TypeInfo_S3std6traits15__T8DemangleTkZ8Demangle6__initZ,"aG",@progbits,_D47TypeInfo_S3std6traits15__T8DemangleTkZ8Demangle6__initZ,comdat
	.weak	_D47TypeInfo_S3std6traits15__T8DemangleTkZ8Demangle6__initZ
	.align	16
_D47TypeInfo_S3std6traits15__T8DemangleTkZ8Demangle6__initZ:
	.quad	_D15TypeInfo_Struct6__vtblZ
	.quad	0
	.quad	35
	.quad	.str
	.quad	24
	.quad	_D3std6traits15__T8DemangleTkZ8Demangle6__initZ
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.long	1
	.zero	4
	.quad	0
	.quad	0
	.long	8
	.zero	4
	.quad	0
	.quad	0
	.quad	305419896
	.size	_D47TypeInfo_S3std6traits15__T8DemangleTkZ8Demangle6__initZ, 136

	.type	.str,@object
	.section	.rodata,"a",@progbits
	.align	16
.str:
	.asciz	 "std.traits.Demangle!(uint).Demangle"
	.size	.str, 36

	.type	_D9Integrate8__ModuleZ,@object
	.data
	.globl	_D9Integrate8__ModuleZ
	.align	8
_D9Integrate8__ModuleZ:
	.long	2147483652
	.long	0
	.quad	.str1
	.size	_D9Integrate8__ModuleZ, 16

	.type	.str1,@object
	.section	.rodata,"a",@progbits
.str1:
	.asciz	 "Integrate"
	.size	.str1, 10

	.type	_D9Integrate11__moduleRefZ,@object
	.data
	.align	8
_D9Integrate11__moduleRefZ:
	.quad	0
	.quad	_D9Integrate8__ModuleZ
	.size	_D9Integrate11__moduleRefZ, 16

	.section	.ctors,"aw",@progbits
	.align	8
	.quad	_D9Integrate16__moduleinfoCtorZ

	.section	".note.GNU-stack","",@progbits
