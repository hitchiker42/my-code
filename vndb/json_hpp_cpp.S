	.file	"json.cpp"
	.text
	.section	.rodata.str1.1,"aMS",@progbits,1
.LC0:
	.string	"basic_string::_M_create"
	.text
	.align 2
	.p2align 4,,15
	.type	_ZNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEE9_M_createERmm.isra.66, @function
_ZNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEE9_M_createERmm.isra.66:
.LFB9006:
	.cfi_startproc
	movq	(%rdi), %rax
	testq	%rax, %rax
	js	.L12
	cmpq	%rsi, %rax
	jbe	.L10
	addq	%rsi, %rsi
	cmpq	%rsi, %rax
	jnb	.L10
	testq	%rsi, %rsi
	js	.L6
	movq	%rsi, (%rdi)
	leaq	1(%rsi), %rdi
	jmp	_Znwm@PLT
	.p2align 4,,10
	.p2align 3
.L10:
	leaq	1(%rax), %rdi
	jmp	_Znwm@PLT
	.p2align 4,,10
	.p2align 3
.L6:
	movabsq	$9223372036854775807, %rax
	movq	%rax, (%rdi)
	movabsq	$-9223372036854775808, %rdi
	jmp	_Znwm@PLT
.L12:
	leaq	.LC0(%rip), %rdi
	subq	$8, %rsp
	.cfi_def_cfa_offset 16
	call	_ZSt20__throw_length_errorPKc@PLT
	.cfi_endproc
.LFE9006:
	.size	_ZNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEE9_M_createERmm.isra.66, .-_ZNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEE9_M_createERmm.isra.66
	.section	.rodata.str1.8,"aMS",@progbits,1
	.align 8
.LC1:
	.string	"basic_string::_M_construct null not valid"
	.text
	.align 2
	.p2align 4,,15
	.type	_ZNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEE12_M_constructIPcEEvT_S7_St20forward_iterator_tag.isra.67, @function
_ZNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEE12_M_constructIPcEEvT_S7_St20forward_iterator_tag.isra.67:
.LFB9007:
	.cfi_startproc
	pushq	%r12
	.cfi_def_cfa_offset 16
	.cfi_offset 12, -16
	pushq	%rbp
	.cfi_def_cfa_offset 24
	.cfi_offset 6, -24
	movq	%rsi, %r12
	pushq	%rbx
	.cfi_def_cfa_offset 32
	.cfi_offset 3, -32
	movq	%rdi, %rbp
	subq	$16, %rsp
	.cfi_def_cfa_offset 48
	movq	%fs:40, %rax
	movq	%rax, 8(%rsp)
	xorl	%eax, %eax
	testq	%rsi, %rsi
	jne	.L14
	testq	%rdx, %rdx
	jne	.L30
.L14:
	movq	%rdx, %rbx
	subq	%r12, %rbx
	cmpq	$15, %rbx
	movq	%rbx, (%rsp)
	ja	.L31
	movq	0(%rbp), %rdx
	cmpq	$1, %rbx
	movq	%rdx, %rax
	je	.L32
	testq	%rbx, %rbx
	jne	.L16
.L18:
	movq	(%rsp), %rax
	movq	%rax, 8(%rbp)
	movb	$0, (%rdx,%rax)
	movq	8(%rsp), %rax
	xorq	%fs:40, %rax
	jne	.L33
	addq	$16, %rsp
	.cfi_remember_state
	.cfi_def_cfa_offset 32
	popq	%rbx
	.cfi_def_cfa_offset 24
	popq	%rbp
	.cfi_def_cfa_offset 16
	popq	%r12
	.cfi_def_cfa_offset 8
	ret
	.p2align 4,,10
	.p2align 3
.L32:
	.cfi_restore_state
	movzbl	(%r12), %eax
	movb	%al, (%rdx)
	movq	0(%rbp), %rdx
	jmp	.L18
	.p2align 4,,10
	.p2align 3
.L31:
	movq	%rsp, %rdi
	xorl	%esi, %esi
	call	_ZNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEE9_M_createERmm.isra.66
	movq	(%rsp), %rdx
	movq	%rax, 0(%rbp)
	movq	%rdx, 16(%rbp)
.L16:
	movq	%rbx, %rdx
	movq	%r12, %rsi
	movq	%rax, %rdi
	call	memcpy@PLT
	movq	0(%rbp), %rdx
	jmp	.L18
.L30:
	leaq	.LC1(%rip), %rdi
	call	_ZSt19__throw_logic_errorPKc@PLT
.L33:
	call	__stack_chk_fail@PLT
	.cfi_endproc
.LFE9007:
	.size	_ZNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEE12_M_constructIPcEEvT_S7_St20forward_iterator_tag.isra.67, .-_ZNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEE12_M_constructIPcEEvT_S7_St20forward_iterator_tag.isra.67
	.set	_ZNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEE12_M_constructIPKcEEvT_S8_St20forward_iterator_tag.isra.71,_ZNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEE12_M_constructIPcEEvT_S7_St20forward_iterator_tag.isra.67
	.section	.text._ZN9__gnu_cxx12__to_xstringINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEcEET_PFiPT0_mPKS8_P13__va_list_tagEmSB_z,"axG",@progbits,_ZN9__gnu_cxx12__to_xstringINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEcEET_PFiPT0_mPKS8_P13__va_list_tagEmSB_z,comdat
	.p2align 4,,15
	.weak	_ZN9__gnu_cxx12__to_xstringINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEcEET_PFiPT0_mPKS8_P13__va_list_tagEmSB_z
	.type	_ZN9__gnu_cxx12__to_xstringINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEcEET_PFiPT0_mPKS8_P13__va_list_tagEmSB_z, @function
_ZN9__gnu_cxx12__to_xstringINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEcEET_PFiPT0_mPKS8_P13__va_list_tagEmSB_z:
.LFB6502:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsi, %r10
	movq	%rdx, %rsi
	movq	%rcx, %rdx
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	pushq	%r12
	pushq	%rbx
	.cfi_offset 12, -24
	.cfi_offset 3, -32
	movq	%rdi, %r12
	subq	$208, %rsp
	testb	%al, %al
	movq	%r8, -160(%rbp)
	movq	%r9, -152(%rbp)
	je	.L35
	movaps	%xmm0, -144(%rbp)
	movaps	%xmm1, -128(%rbp)
	movaps	%xmm2, -112(%rbp)
	movaps	%xmm3, -96(%rbp)
	movaps	%xmm4, -80(%rbp)
	movaps	%xmm5, -64(%rbp)
	movaps	%xmm6, -48(%rbp)
	movaps	%xmm7, -32(%rbp)
.L35:
	movq	%fs:40, %rax
	movq	%rax, -200(%rbp)
	xorl	%eax, %eax
	leaq	30(%rsi), %rax
	leaq	-224(%rbp), %rcx
	andq	$-16, %rax
	movl	$32, -224(%rbp)
	movl	$48, -220(%rbp)
	subq	%rax, %rsp
	leaq	16(%rbp), %rax
	leaq	15(%rsp), %rbx
	movq	%rax, -216(%rbp)
	leaq	-192(%rbp), %rax
	andq	$-16, %rbx
	movq	%rbx, %rdi
	movq	%rax, -208(%rbp)
	call	*%r10
	leaq	16(%r12), %rdx
	movq	%r12, %rdi
	movq	%rbx, %rsi
	movq	%rdx, (%r12)
	movslq	%eax, %rdx
	addq	%rbx, %rdx
	call	_ZNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEE12_M_constructIPcEEvT_S7_St20forward_iterator_tag.isra.67
	movq	-200(%rbp), %rdi
	xorq	%fs:40, %rdi
	movq	%r12, %rax
	jne	.L38
	leaq	-16(%rbp), %rsp
	popq	%rbx
	popq	%r12
	popq	%rbp
	.cfi_remember_state
	.cfi_def_cfa 7, 8
	ret
.L38:
	.cfi_restore_state
	call	__stack_chk_fail@PLT
	.cfi_endproc
.LFE6502:
	.size	_ZN9__gnu_cxx12__to_xstringINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEcEET_PFiPT0_mPKS8_P13__va_list_tagEmSB_z, .-_ZN9__gnu_cxx12__to_xstringINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEcEET_PFiPT0_mPKS8_P13__va_list_tagEmSB_z
	.section	.text._ZNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEC2EPKcRKS3_,"axG",@progbits,_ZNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEC5EPKcRKS3_,comdat
	.align 2
	.p2align 4,,15
	.weak	_ZNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEC2EPKcRKS3_
	.type	_ZNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEC2EPKcRKS3_, @function
_ZNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEC2EPKcRKS3_:
.LFB6671:
	.cfi_startproc
	pushq	%rbx
	.cfi_def_cfa_offset 16
	.cfi_offset 3, -16
	leaq	16(%rdi), %rax
	movq	%rdi, %rbx
	movq	$-1, %rdx
	subq	$16, %rsp
	.cfi_def_cfa_offset 32
	testq	%rsi, %rsi
	movq	%rax, (%rdi)
	je	.L40
	movq	%rsi, %rdi
	movq	%rsi, 8(%rsp)
	call	strlen@PLT
	movq	8(%rsp), %rsi
	leaq	(%rsi,%rax), %rdx
.L40:
	addq	$16, %rsp
	.cfi_def_cfa_offset 16
	movq	%rbx, %rdi
	popq	%rbx
	.cfi_def_cfa_offset 8
	jmp	_ZNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEE12_M_constructIPKcEEvT_S8_St20forward_iterator_tag.isra.71
	.cfi_endproc
.LFE6671:
	.size	_ZNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEC2EPKcRKS3_, .-_ZNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEC2EPKcRKS3_
	.weak	_ZNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEC1EPKcRKS3_
	.set	_ZNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEC1EPKcRKS3_,_ZNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEC2EPKcRKS3_
	.section	.text._ZNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEE9_M_mutateEmmPKcm,"axG",@progbits,_ZNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEE9_M_mutateEmmPKcm,comdat
	.align 2
	.p2align 4,,15
	.weak	_ZNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEE9_M_mutateEmmPKcm
	.type	_ZNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEE9_M_mutateEmmPKcm, @function
_ZNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEE9_M_mutateEmmPKcm:
.LFB7370:
	.cfi_startproc
	pushq	%r15
	.cfi_def_cfa_offset 16
	.cfi_offset 15, -16
	pushq	%r14
	.cfi_def_cfa_offset 24
	.cfi_offset 14, -24
	movq	%rdx, %r14
	pushq	%r13
	.cfi_def_cfa_offset 32
	.cfi_offset 13, -32
	pushq	%r12
	.cfi_def_cfa_offset 40
	.cfi_offset 12, -40
	leaq	16(%rdi), %r13
	pushq	%rbp
	.cfi_def_cfa_offset 48
	.cfi_offset 6, -48
	pushq	%rbx
	.cfi_def_cfa_offset 56
	.cfi_offset 3, -56
	movq	%rsi, %rbp
	movq	%rdi, %rbx
	subq	$40, %rsp
	.cfi_def_cfa_offset 96
	movq	8(%rdi), %rdx
	movq	%fs:40, %rax
	movq	%rax, 24(%rsp)
	xorl	%eax, %eax
	movq	%r8, %rax
	movq	%rdx, %r12
	subq	%r14, %rax
	subq	%rsi, %r12
	addq	%rdx, %rax
	movl	$15, %esi
	subq	%r14, %r12
	cmpq	(%rdi), %r13
	movq	%rax, 16(%rsp)
	je	.L46
	movq	16(%rdi), %rsi
.L46:
	leaq	16(%rsp), %rdi
	movq	%r8, 8(%rsp)
	movq	%rcx, (%rsp)
	call	_ZNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEE9_M_createERmm.isra.66
	testq	%rbp, %rbp
	movq	%rax, %r15
	movq	(%rsp), %rcx
	movq	8(%rsp), %r8
	je	.L47
	cmpq	$1, %rbp
	movq	(%rbx), %rsi
	jne	.L48
	movzbl	(%rsi), %eax
	movb	%al, (%r15)
.L47:
	testq	%rcx, %rcx
	je	.L49
	testq	%r8, %r8
	je	.L49
	cmpq	$1, %r8
	leaq	(%r15,%rbp), %rdi
	je	.L70
	movq	%r8, %rdx
	movq	%rcx, %rsi
	movq	%r8, (%rsp)
	call	memcpy@PLT
	movq	(%rsp), %r8
	.p2align 4,,10
	.p2align 3
.L49:
	testq	%r12, %r12
	movq	(%rbx), %rdi
	je	.L51
	leaq	0(%rbp,%r14), %rsi
	addq	%r8, %rbp
	addq	%rdi, %rsi
	cmpq	$1, %r12
	leaq	(%r15,%rbp), %rdi
	jne	.L52
	movzbl	(%rsi), %eax
	movb	%al, (%rdi)
	movq	(%rbx), %rdi
.L51:
	cmpq	%rdi, %r13
	je	.L53
	call	_ZdlPv@PLT
.L53:
	movq	16(%rsp), %rax
	movq	%r15, (%rbx)
	movq	%rax, 16(%rbx)
	movq	24(%rsp), %rax
	xorq	%fs:40, %rax
	jne	.L71
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
	ret
	.p2align 4,,10
	.p2align 3
.L52:
	.cfi_restore_state
	movq	%r12, %rdx
	call	memcpy@PLT
	movq	(%rbx), %rdi
	jmp	.L51
	.p2align 4,,10
	.p2align 3
.L48:
	movq	%rbp, %rdx
	movq	%rax, %rdi
	movq	%r8, 8(%rsp)
	movq	%rcx, (%rsp)
	call	memcpy@PLT
	movq	8(%rsp), %r8
	movq	(%rsp), %rcx
	jmp	.L47
	.p2align 4,,10
	.p2align 3
.L70:
	movzbl	(%rcx), %eax
	movb	%al, (%rdi)
	jmp	.L49
.L71:
	call	__stack_chk_fail@PLT
	.cfi_endproc
.LFE7370:
	.size	_ZNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEE9_M_mutateEmmPKcm, .-_ZNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEE9_M_mutateEmmPKcm
	.section	.text._ZNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEE9_M_appendEPKcm,"axG",@progbits,_ZNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEE9_M_appendEPKcm,comdat
	.align 2
	.p2align 4,,15
	.weak	_ZNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEE9_M_appendEPKcm
	.type	_ZNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEE9_M_appendEPKcm, @function
_ZNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEE9_M_appendEPKcm:
.LFB7144:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	pushq	%rbx
	.cfi_def_cfa_offset 24
	.cfi_offset 3, -24
	movq	%rdi, %rbx
	leaq	16(%rbx), %r8
	movl	$15, %eax
	subq	$8, %rsp
	.cfi_def_cfa_offset 32
	movq	8(%rdi), %r9
	movq	(%rdi), %rdi
	cmpq	%r8, %rdi
	leaq	(%rdx,%r9), %rbp
	je	.L73
	movq	16(%rbx), %rax
.L73:
	cmpq	%rax, %rbp
	ja	.L74
	testq	%rdx, %rdx
	je	.L75
	addq	%r9, %rdi
	cmpq	$1, %rdx
	jne	.L76
	movzbl	(%rsi), %eax
	movb	%al, (%rdi)
	movq	(%rbx), %rdi
.L75:
	movq	%rbp, 8(%rbx)
	movb	$0, (%rdi,%rbp)
	addq	$8, %rsp
	.cfi_remember_state
	.cfi_def_cfa_offset 24
	movq	%rbx, %rax
	popq	%rbx
	.cfi_def_cfa_offset 16
	popq	%rbp
	.cfi_def_cfa_offset 8
	ret
	.p2align 4,,10
	.p2align 3
.L74:
	.cfi_restore_state
	movq	%rdx, %r8
	movq	%rsi, %rcx
	movq	%rbx, %rdi
	xorl	%edx, %edx
	movq	%r9, %rsi
	call	_ZNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEE9_M_mutateEmmPKcm
	movq	(%rbx), %rdi
	jmp	.L75
	.p2align 4,,10
	.p2align 3
.L76:
	call	memcpy@PLT
	movq	(%rbx), %rdi
	jmp	.L75
	.cfi_endproc
.LFE7144:
	.size	_ZNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEE9_M_appendEPKcm, .-_ZNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEE9_M_appendEPKcm
	.section	.rodata._ZNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEE10_M_replaceEmmPKcm.str1.1,"aMS",@progbits,1
.LC2:
	.string	"basic_string::_M_replace"
	.section	.text._ZNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEE10_M_replaceEmmPKcm,"axG",@progbits,_ZNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEE10_M_replaceEmmPKcm,comdat
	.align 2
	.p2align 4,,15
	.weak	_ZNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEE10_M_replaceEmmPKcm
	.type	_ZNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEE10_M_replaceEmmPKcm, @function
_ZNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEE10_M_replaceEmmPKcm:
.LFB7371:
	.cfi_startproc
	pushq	%r15
	.cfi_def_cfa_offset 16
	.cfi_offset 15, -16
	pushq	%r14
	.cfi_def_cfa_offset 24
	.cfi_offset 14, -24
	movabsq	$9223372036854775807, %rax
	pushq	%r13
	.cfi_def_cfa_offset 32
	.cfi_offset 13, -32
	pushq	%r12
	.cfi_def_cfa_offset 40
	.cfi_offset 12, -40
	pushq	%rbp
	.cfi_def_cfa_offset 48
	.cfi_offset 6, -48
	pushq	%rbx
	.cfi_def_cfa_offset 56
	.cfi_offset 3, -56
	movq	%rdx, %rbp
	addq	%rbp, %rax
	subq	$40, %rsp
	.cfi_def_cfa_offset 96
	movq	8(%rdi), %rdx
	subq	%rdx, %rax
	cmpq	%rax, %r8
	ja	.L128
	movq	(%rdi), %rax
	leaq	16(%rdi), %r9
	movq	%r8, %r15
	subq	%rbp, %r15
	movq	%rdi, %rbx
	movl	$15, %edi
	leaq	(%rdx,%r15), %r12
	cmpq	%r9, %rax
	je	.L85
	movq	16(%rbx), %rdi
.L85:
	cmpq	%r12, %rdi
	jb	.L86
	movq	%rdx, %r13
	leaq	(%rax,%rsi), %r9
	subq	%rsi, %r13
	cmpq	%r8, %rbp
	setne	%r14b
	subq	%rbp, %r13
	setne	%sil
	andl	%esi, %r14d
	cmpq	%rax, %rcx
	jnb	.L129
.L87:
	testb	%r14b, %r14b
	je	.L91
	cmpq	$1, %r13
	leaq	(%r9,%rbp), %rsi
	leaq	(%r9,%r8), %rdi
	je	.L130
	movq	%r13, %rdx
	movq	%r8, 24(%rsp)
	movq	%rcx, 16(%rsp)
	movq	%r9, 8(%rsp)
	call	memmove@PLT
	movq	24(%rsp), %r8
	movq	16(%rsp), %rcx
	movq	8(%rsp), %r9
	.p2align 4,,10
	.p2align 3
.L91:
	testq	%r8, %r8
	je	.L94
	cmpq	$1, %r8
	jne	.L95
.L127:
	movzbl	(%rcx), %eax
	movb	%al, (%r9)
.L94:
	movq	(%rbx), %rax
	movq	%r12, 8(%rbx)
	movb	$0, (%rax,%r12)
	addq	$40, %rsp
	.cfi_remember_state
	.cfi_def_cfa_offset 56
	movq	%rbx, %rax
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
	ret
	.p2align 4,,10
	.p2align 3
.L129:
	.cfi_restore_state
	addq	%rdx, %rax
	cmpq	%rax, %rcx
	ja	.L87
	testq	%r8, %r8
	je	.L90
	cmpq	%r8, %rbp
	jb	.L90
	cmpq	$1, %r8
	je	.L131
	movq	%r8, %rdx
	movq	%rcx, %rsi
	movq	%r9, %rdi
	movq	%r8, 16(%rsp)
	movq	%rcx, 8(%rsp)
	call	memmove@PLT
	testb	%r14b, %r14b
	movq	%rax, %r9
	movq	8(%rsp), %rcx
	movq	16(%rsp), %r8
	je	.L94
	.p2align 4,,10
	.p2align 3
.L100:
	cmpq	$1, %r13
	leaq	(%r9,%rbp), %rsi
	leaq	(%r9,%r8), %rdi
	je	.L132
	movq	%r13, %rdx
	movq	%r8, 24(%rsp)
	movq	%rcx, 16(%rsp)
	movq	%r9, 8(%rsp)
	call	memmove@PLT
	movq	24(%rsp), %r8
	movq	16(%rsp), %rcx
	movq	8(%rsp), %r9
	.p2align 4,,10
	.p2align 3
.L101:
	cmpq	%r8, %rbp
	jnb	.L94
	leaq	(%rcx,%r8), %rax
	addq	%r9, %rbp
	cmpq	%rax, %rbp
	jnb	.L133
	cmpq	%rcx, %rbp
	ja	.L104
	cmpq	$1, %r8
	leaq	(%rcx,%r15), %rsi
	je	.L134
	movq	%r8, %rdx
	movq	%r9, %rdi
	call	memcpy@PLT
	jmp	.L94
	.p2align 4,,10
	.p2align 3
.L86:
	movq	%rbp, %rdx
	movq	%rbx, %rdi
	call	_ZNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEE9_M_mutateEmmPKcm
	jmp	.L94
	.p2align 4,,10
	.p2align 3
.L95:
	movq	%r8, %rdx
	movq	%rcx, %rsi
	movq	%r9, %rdi
	call	memcpy@PLT
	jmp	.L94
	.p2align 4,,10
	.p2align 3
.L130:
	movzbl	(%rsi), %eax
	movb	%al, (%rdi)
	jmp	.L91
	.p2align 4,,10
	.p2align 3
.L131:
	movzbl	(%rcx), %eax
	movb	%al, (%r9)
.L90:
	testb	%r14b, %r14b
	jne	.L100
	jmp	.L101
	.p2align 4,,10
	.p2align 3
.L133:
	cmpq	$1, %r8
	je	.L127
	movq	%r8, %rdx
	movq	%rcx, %rsi
	movq	%r9, %rdi
	call	memmove@PLT
	jmp	.L94
	.p2align 4,,10
	.p2align 3
.L132:
	movzbl	(%rsi), %eax
	movb	%al, (%rdi)
	jmp	.L101
	.p2align 4,,10
	.p2align 3
.L104:
	subq	%rcx, %rbp
	cmpq	$1, %rbp
	je	.L135
	testq	%rbp, %rbp
	jne	.L136
.L107:
	movq	%r8, %rdx
	leaq	(%r9,%r8), %rsi
	leaq	(%r9,%rbp), %rdi
	subq	%rbp, %rdx
	cmpq	$1, %rdx
	je	.L137
	testq	%rdx, %rdx
	je	.L94
	call	memcpy@PLT
	jmp	.L94
.L135:
	movzbl	(%rcx), %eax
	movb	%al, (%r9)
	jmp	.L107
.L134:
	movzbl	(%rsi), %eax
	movb	%al, (%r9)
	jmp	.L94
.L137:
	movzbl	(%rsi), %eax
	movb	%al, (%rdi)
	jmp	.L94
.L136:
	movq	%r9, %rdi
	movq	%rbp, %rdx
	movq	%rcx, %rsi
	movq	%r8, 8(%rsp)
	call	memmove@PLT
	movq	8(%rsp), %r8
	movq	%rax, %r9
	jmp	.L107
.L128:
	leaq	.LC2(%rip), %rdi
	call	_ZSt20__throw_length_errorPKc@PLT
	.cfi_endproc
.LFE7371:
	.size	_ZNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEE10_M_replaceEmmPKcm, .-_ZNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEE10_M_replaceEmmPKcm
	.section	.text._ZStplIcSt11char_traitsIcESaIcEENSt7__cxx1112basic_stringIT_T0_T1_EEPKS5_OS8_,"axG",@progbits,_ZStplIcSt11char_traitsIcESaIcEENSt7__cxx1112basic_stringIT_T0_T1_EEPKS5_OS8_,comdat
	.p2align 4,,15
	.weak	_ZStplIcSt11char_traitsIcESaIcEENSt7__cxx1112basic_stringIT_T0_T1_EEPKS5_OS8_
	.type	_ZStplIcSt11char_traitsIcESaIcEENSt7__cxx1112basic_stringIT_T0_T1_EEPKS5_OS8_, @function
_ZStplIcSt11char_traitsIcESaIcEENSt7__cxx1112basic_stringIT_T0_T1_EEPKS5_OS8_:
.LFB6662:
	.cfi_startproc
	pushq	%r12
	.cfi_def_cfa_offset 16
	.cfi_offset 12, -16
	pushq	%rbp
	.cfi_def_cfa_offset 24
	.cfi_offset 6, -24
	movq	%rdx, %r12
	pushq	%rbx
	.cfi_def_cfa_offset 32
	.cfi_offset 3, -32
	movq	%rsi, %rbp
	movq	%rdi, %rbx
	movq	%rsi, %rdi
	call	strlen@PLT
	xorl	%edx, %edx
	movq	%rbp, %rcx
	xorl	%esi, %esi
	movq	%rax, %r8
	movq	%r12, %rdi
	call	_ZNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEE10_M_replaceEmmPKcm
	leaq	16(%rbx), %rdx
	movq	%rdx, (%rbx)
	movq	(%rax), %rcx
	leaq	16(%rax), %rdx
	cmpq	%rdx, %rcx
	je	.L142
	movq	%rcx, (%rbx)
	movq	16(%rax), %rcx
	movq	%rcx, 16(%rbx)
.L140:
	movq	8(%rax), %rcx
	movq	%rcx, 8(%rbx)
	movq	%rdx, (%rax)
	movq	$0, 8(%rax)
	movb	$0, 16(%rax)
	movq	%rbx, %rax
	popq	%rbx
	.cfi_remember_state
	.cfi_def_cfa_offset 24
	popq	%rbp
	.cfi_def_cfa_offset 16
	popq	%r12
	.cfi_def_cfa_offset 8
	ret
	.p2align 4,,10
	.p2align 3
.L142:
	.cfi_restore_state
	movdqu	16(%rax), %xmm0
	movups	%xmm0, 16(%rbx)
	jmp	.L140
	.cfi_endproc
.LFE6662:
	.size	_ZStplIcSt11char_traitsIcESaIcEENSt7__cxx1112basic_stringIT_T0_T1_EEPKS5_OS8_, .-_ZStplIcSt11char_traitsIcESaIcEENSt7__cxx1112basic_stringIT_T0_T1_EEPKS5_OS8_
	.section	.rodata._ZN8nlohmann6detail10type_error6createEiRKNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEE.str1.1,"aMS",@progbits,1
.LC3:
	.string	"type_error"
.LC4:
	.string	"%d"
.LC5:
	.string	"basic_string::append"
.LC6:
	.string	"[json.exception."
.LC7:
	.string	"."
.LC8:
	.string	"] "
	.section	.text._ZN8nlohmann6detail10type_error6createEiRKNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEE,"axG",@progbits,_ZN8nlohmann6detail10type_error6createEiRKNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEE,comdat
	.p2align 4,,15
	.weak	_ZN8nlohmann6detail10type_error6createEiRKNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEE
	.type	_ZN8nlohmann6detail10type_error6createEiRKNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEE, @function
_ZN8nlohmann6detail10type_error6createEiRKNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEE:
.LFB4374:
	.cfi_startproc
	.cfi_personality 0x9b,DW.ref.__gxx_personality_v0
	.cfi_lsda 0x1b,.LLSDA4374
	pushq	%r15
	.cfi_def_cfa_offset 16
	.cfi_offset 15, -16
	pushq	%r14
	.cfi_def_cfa_offset 24
	.cfi_offset 14, -24
	movq	%rdx, %r15
	pushq	%r13
	.cfi_def_cfa_offset 32
	.cfi_offset 13, -32
	pushq	%r12
	.cfi_def_cfa_offset 40
	.cfi_offset 12, -40
	leaq	10+.LC3(%rip), %rdx
	pushq	%rbp
	.cfi_def_cfa_offset 48
	.cfi_offset 6, -48
	pushq	%rbx
	.cfi_def_cfa_offset 56
	.cfi_offset 3, -56
	movq	%rdi, %r14
	movl	%esi, %ebx
	subq	$264, %rsp
	.cfi_def_cfa_offset 320
	leaq	48(%rsp), %rdi
	movl	%esi, 28(%rsp)
	leaq	-10(%rdx), %rsi
	movq	%fs:40, %rax
	movq	%rax, 248(%rsp)
	xorl	%eax, %eax
	leaq	16(%rdi), %rax
	movq	%rdi, 16(%rsp)
	movq	%rax, 48(%rsp)
.LEHB0:
	call	_ZNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEE12_M_constructIPKcEEvT_S8_St20forward_iterator_tag.isra.71
.LEHE0:
	leaq	176(%rsp), %rax
	movq	vsnprintf@GOTPCREL(%rip), %rsi
	leaq	.LC4(%rip), %rcx
	movl	%ebx, %r8d
	movl	$16, %edx
	movq	%rax, 8(%rsp)
	movq	%rax, %rdi
	xorl	%eax, %eax
.LEHB1:
	call	_ZN9__gnu_cxx12__to_xstringINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEcEET_PFiPT0_mPKS8_P13__va_list_tagEmSB_z
.LEHE1:
	leaq	112(%rsp), %rbx
	movq	$0, 120(%rsp)
	movb	$0, 128(%rsp)
	leaq	16(%rbx), %rax
	movq	%rax, 112(%rsp)
	movq	56(%rsp), %rax
	addq	$16, %rax
	cmpq	$15, %rax
	movq	%rax, 40(%rsp)
	ja	.L209
.L144:
	leaq	.LC6(%rip), %rsi
	movl	$16, %edx
	movq	%rbx, %rdi
.LEHB2:
	call	_ZNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEE9_M_appendEPKcm
	movq	56(%rsp), %rdx
	movq	48(%rsp), %rsi
	movq	%rbx, %rdi
	call	_ZNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEE9_M_appendEPKcm
.LEHE2:
	movabsq	$9223372036854775807, %rax
	cmpq	%rax, 120(%rsp)
	je	.L210
	leaq	.LC7(%rip), %rsi
	movl	$1, %edx
	movq	%rbx, %rdi
.LEHB3:
	call	_ZNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEE9_M_appendEPKcm
.LEHE3:
	leaq	144(%rsp), %r12
	leaq	16(%r12), %rdx
	movq	%rdx, 144(%rsp)
	movq	(%rax), %rcx
	leaq	16(%rax), %rdx
	cmpq	%rdx, %rcx
	je	.L211
	movq	%rcx, 144(%rsp)
	movq	16(%rax), %rcx
	movq	%rcx, 160(%rsp)
.L154:
	movq	8(%rax), %rcx
	leaq	16(%r12), %rsi
	movb	$0, 16(%rax)
	movq	%rcx, 152(%rsp)
	movq	%rdx, (%rax)
	movq	144(%rsp), %rcx
	movq	$0, 8(%rax)
	movl	$15, %eax
	movq	152(%rsp), %r8
	movq	184(%rsp), %rdx
	cmpq	%rsi, %rcx
	leaq	(%r8,%rdx), %rdi
	je	.L155
	movq	160(%rsp), %rax
.L155:
	cmpq	%rax, %rdi
	movq	176(%rsp), %rsi
	jbe	.L156
	movq	8(%rsp), %rax
	leaq	16(%rax), %r9
	movl	$15, %eax
	cmpq	%r9, %rsi
	je	.L157
	movq	192(%rsp), %rax
.L157:
	cmpq	%rax, %rdi
	jbe	.L212
.L156:
	movq	%r12, %rdi
.LEHB4:
	call	_ZNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEE9_M_appendEPKcm
.LEHE4:
.L158:
	leaq	208(%rsp), %rbp
	leaq	16(%rbp), %rdx
	movq	%rdx, 208(%rsp)
	movq	(%rax), %rcx
	leaq	16(%rax), %rdx
	cmpq	%rdx, %rcx
	je	.L213
	movq	%rcx, 208(%rsp)
	movq	16(%rax), %rcx
	movq	%rcx, 224(%rsp)
.L160:
	movq	8(%rax), %rcx
	movq	%rcx, 216(%rsp)
	movq	%rdx, (%rax)
	movq	$0, 8(%rax)
	movb	$0, 16(%rax)
	movabsq	$9223372036854775807, %rax
	subq	216(%rsp), %rax
	cmpq	$1, %rax
	jbe	.L214
	leaq	.LC8(%rip), %rsi
	movl	$2, %edx
	movq	%rbp, %rdi
.LEHB5:
	call	_ZNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEE9_M_appendEPKcm
.LEHE5:
	leaq	80(%rsp), %r13
	leaq	16(%r13), %rdx
	movq	%rdx, 80(%rsp)
	movq	(%rax), %rcx
	leaq	16(%rax), %rdx
	cmpq	%rdx, %rcx
	je	.L215
	movq	%rcx, 80(%rsp)
	movq	16(%rax), %rcx
	movq	%rcx, 96(%rsp)
.L163:
	movq	8(%rax), %rcx
	movb	$0, 16(%rax)
	movq	%rcx, 88(%rsp)
	movq	%rdx, (%rax)
	movq	$0, 8(%rax)
	movq	208(%rsp), %rdi
	leaq	16(%rbp), %rax
	cmpq	%rax, %rdi
	je	.L164
	call	_ZdlPv@PLT
.L164:
	movq	144(%rsp), %rdi
	addq	$16, %r12
	cmpq	%r12, %rdi
	je	.L165
	call	_ZdlPv@PLT
.L165:
	movq	112(%rsp), %rdi
	addq	$16, %rbx
	cmpq	%rbx, %rdi
	je	.L166
	call	_ZdlPv@PLT
.L166:
	movq	8(%rsp), %rax
	movq	176(%rsp), %rdi
	addq	$16, %rax
	cmpq	%rax, %rdi
	je	.L167
	call	_ZdlPv@PLT
.L167:
	movq	8(%r15), %rdx
	movq	(%r15), %rsi
	movq	%r13, %rdi
.LEHB6:
	call	_ZNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEE9_M_appendEPKcm
.LEHE6:
	leaq	16(%rbp), %rdx
	movq	%rdx, 208(%rsp)
	movq	(%rax), %rcx
	leaq	16(%rax), %rdx
	cmpq	%rdx, %rcx
	je	.L216
	movq	%rcx, 208(%rsp)
	movq	16(%rax), %rcx
	movq	%rcx, 224(%rsp)
.L178:
	movq	8(%rax), %rcx
	addq	$16, %r13
	movq	%rcx, 216(%rsp)
	movq	%rdx, (%rax)
	movq	80(%rsp), %rdi
	movq	$0, 8(%rax)
	movb	$0, 16(%rax)
	cmpq	%r13, %rdi
	je	.L179
	call	_ZdlPv@PLT
.L179:
	movq	16(%rsp), %rax
	movq	48(%rsp), %rdi
	addq	$16, %rax
	cmpq	%rax, %rdi
	je	.L180
	call	_ZdlPv@PLT
.L180:
	leaq	16+_ZTVN8nlohmann6detail9exceptionE(%rip), %rax
	movq	208(%rsp), %rsi
	leaq	16(%r14), %rdi
	movq	%rax, (%r14)
	movl	28(%rsp), %eax
	movl	%eax, 8(%r14)
.LEHB7:
	call	_ZNSt13runtime_errorC1EPKc@PLT
.LEHE7:
	movq	208(%rsp), %rdi
	leaq	16+_ZTVN8nlohmann6detail10type_errorE(%rip), %rax
	addq	$16, %rbp
	movq	%rax, (%r14)
	cmpq	%rbp, %rdi
	je	.L143
	call	_ZdlPv@PLT
.L143:
	movq	248(%rsp), %rbx
	xorq	%fs:40, %rbx
	movq	%r14, %rax
	jne	.L217
	addq	$264, %rsp
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
	ret
	.p2align 4,,10
	.p2align 3
.L212:
	.cfi_restore_state
	movq	8(%rsp), %rdi
	xorl	%edx, %edx
	xorl	%esi, %esi
.LEHB8:
	call	_ZNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEE10_M_replaceEmmPKcm
.LEHE8:
	jmp	.L158
	.p2align 4,,10
	.p2align 3
.L209:
	leaq	40(%rsp), %rdi
	movl	$15, %esi
.LEHB9:
	call	_ZNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEE9_M_createERmm.isra.66
	movq	%rax, %rbp
	movq	120(%rsp), %rax
	movq	112(%rsp), %rdi
	leaq	1(%rax), %rdx
	cmpq	$1, %rdx
	je	.L218
	testq	%rdx, %rdx
	jne	.L219
.L146:
	leaq	16(%rbx), %rax
	cmpq	%rax, %rdi
	je	.L147
	call	_ZdlPv@PLT
.L147:
	movq	40(%rsp), %rax
	movq	%rbp, 112(%rsp)
	movq	%rax, 128(%rsp)
	movabsq	$9223372036854775807, %rax
	subq	120(%rsp), %rax
	cmpq	$15, %rax
	ja	.L144
	leaq	.LC5(%rip), %rdi
	call	_ZSt20__throw_length_errorPKc@PLT
.LEHE9:
	.p2align 4,,10
	.p2align 3
.L213:
	movdqu	16(%rax), %xmm0
	movaps	%xmm0, 224(%rsp)
	jmp	.L160
	.p2align 4,,10
	.p2align 3
.L216:
	movdqu	16(%rax), %xmm0
	movaps	%xmm0, 224(%rsp)
	jmp	.L178
	.p2align 4,,10
	.p2align 3
.L215:
	movdqu	16(%rax), %xmm0
	movaps	%xmm0, 96(%rsp)
	jmp	.L163
	.p2align 4,,10
	.p2align 3
.L211:
	movdqu	16(%rax), %xmm0
	movaps	%xmm0, 160(%rsp)
	jmp	.L154
	.p2align 4,,10
	.p2align 3
.L218:
	movzbl	(%rdi), %eax
	movb	%al, 0(%rbp)
	movq	112(%rsp), %rdi
	jmp	.L146
	.p2align 4,,10
	.p2align 3
.L219:
	movq	%rdi, %rsi
	movq	%rbp, %rdi
	call	memcpy@PLT
	movq	112(%rsp), %rdi
	jmp	.L146
.L197:
.L208:
	movq	%rax, %rbp
.L173:
	movq	112(%rsp), %rdi
	addq	$16, %rbx
	cmpq	%rbx, %rdi
	je	.L151
	call	_ZdlPv@PLT
.L151:
	movq	8(%rsp), %rax
	movq	176(%rsp), %rdi
	addq	$16, %rax
	cmpq	%rax, %rdi
	je	.L175
	call	_ZdlPv@PLT
.L175:
	movq	%rbp, %rbx
.L176:
	movq	16(%rsp), %rdx
	movq	48(%rsp), %rdi
	addq	$16, %rdx
	cmpq	%rdx, %rdi
	je	.L184
.L183:
	call	_ZdlPv@PLT
.L184:
	movq	%rbx, %rdi
.LEHB10:
	call	_Unwind_Resume@PLT
.LEHE10:
.L214:
	leaq	.LC5(%rip), %rdi
.LEHB11:
	call	_ZSt20__throw_length_errorPKc@PLT
.LEHE11:
.L196:
	movq	208(%rsp), %rdi
	addq	$16, %rbp
	movq	%rax, %r13
	cmpq	%rbp, %rdi
	je	.L170
	call	_ZdlPv@PLT
.L170:
	movq	%r13, %rbp
.L171:
	movq	144(%rsp), %rdi
	addq	$16, %r12
	cmpq	%r12, %rdi
	je	.L173
	call	_ZdlPv@PLT
	jmp	.L173
.L210:
	leaq	.LC5(%rip), %rdi
.LEHB12:
	call	_ZSt20__throw_length_errorPKc@PLT
.LEHE12:
.L194:
	jmp	.L208
.L217:
	call	__stack_chk_fail@PLT
.L193:
	movq	80(%rsp), %rdi
	addq	$16, %r13
	movq	%rax, %rbx
	cmpq	%r13, %rdi
	je	.L176
	call	_ZdlPv@PLT
	jmp	.L176
.L195:
	movq	%rax, %rbp
	jmp	.L171
.L198:
	movq	%r14, %rdi
	movq	%rax, %rbx
	addq	$16, %rbp
	call	_ZNSt9exceptionD2Ev@PLT
	movq	208(%rsp), %rdi
	cmpq	%rbp, %rdi
	jne	.L183
	jmp	.L184
.L192:
	movq	%rax, %rbx
	jmp	.L176
	.cfi_endproc
.LFE4374:
	.globl	__gxx_personality_v0
	.section	.gcc_except_table._ZN8nlohmann6detail10type_error6createEiRKNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEE,"aG",@progbits,_ZN8nlohmann6detail10type_error6createEiRKNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEE,comdat
.LLSDA4374:
	.byte	0xff
	.byte	0xff
	.byte	0x1
	.uleb128 .LLSDACSE4374-.LLSDACSB4374
.LLSDACSB4374:
	.uleb128 .LEHB0-.LFB4374
	.uleb128 .LEHE0-.LEHB0
	.uleb128 0
	.uleb128 0
	.uleb128 .LEHB1-.LFB4374
	.uleb128 .LEHE1-.LEHB1
	.uleb128 .L192-.LFB4374
	.uleb128 0
	.uleb128 .LEHB2-.LFB4374
	.uleb128 .LEHE2-.LEHB2
	.uleb128 .L197-.LFB4374
	.uleb128 0
	.uleb128 .LEHB3-.LFB4374
	.uleb128 .LEHE3-.LEHB3
	.uleb128 .L194-.LFB4374
	.uleb128 0
	.uleb128 .LEHB4-.LFB4374
	.uleb128 .LEHE4-.LEHB4
	.uleb128 .L195-.LFB4374
	.uleb128 0
	.uleb128 .LEHB5-.LFB4374
	.uleb128 .LEHE5-.LEHB5
	.uleb128 .L196-.LFB4374
	.uleb128 0
	.uleb128 .LEHB6-.LFB4374
	.uleb128 .LEHE6-.LEHB6
	.uleb128 .L193-.LFB4374
	.uleb128 0
	.uleb128 .LEHB7-.LFB4374
	.uleb128 .LEHE7-.LEHB7
	.uleb128 .L198-.LFB4374
	.uleb128 0
	.uleb128 .LEHB8-.LFB4374
	.uleb128 .LEHE8-.LEHB8
	.uleb128 .L195-.LFB4374
	.uleb128 0
	.uleb128 .LEHB9-.LFB4374
	.uleb128 .LEHE9-.LEHB9
	.uleb128 .L197-.LFB4374
	.uleb128 0
	.uleb128 .LEHB10-.LFB4374
	.uleb128 .LEHE10-.LEHB10
	.uleb128 0
	.uleb128 0
	.uleb128 .LEHB11-.LFB4374
	.uleb128 .LEHE11-.LEHB11
	.uleb128 .L196-.LFB4374
	.uleb128 0
	.uleb128 .LEHB12-.LFB4374
	.uleb128 .LEHE12-.LEHB12
	.uleb128 .L194-.LFB4374
	.uleb128 0
.LLSDACSE4374:
	.section	.text._ZN8nlohmann6detail10type_error6createEiRKNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEE,"axG",@progbits,_ZN8nlohmann6detail10type_error6createEiRKNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEE,comdat
	.size	_ZN8nlohmann6detail10type_error6createEiRKNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEE, .-_ZN8nlohmann6detail10type_error6createEiRKNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEE
	.section	.rodata._ZN8nlohmann10basic_jsonISt3mapSt6vectorNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEblmdSaNS_14adl_serializerEE10json_valueC2ENS_6detail7value_tE.str1.1,"aMS",@progbits,1
.LC9:
	.string	""
	.section	.text._ZN8nlohmann10basic_jsonISt3mapSt6vectorNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEblmdSaNS_14adl_serializerEE10json_valueC2ENS_6detail7value_tE,"axG",@progbits,_ZN8nlohmann10basic_jsonISt3mapSt6vectorNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEblmdSaNS_14adl_serializerEE10json_valueC5ENS_6detail7value_tE,comdat
	.align 2
	.p2align 4,,15
	.weak	_ZN8nlohmann10basic_jsonISt3mapSt6vectorNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEblmdSaNS_14adl_serializerEE10json_valueC2ENS_6detail7value_tE
	.type	_ZN8nlohmann10basic_jsonISt3mapSt6vectorNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEblmdSaNS_14adl_serializerEE10json_valueC2ENS_6detail7value_tE, @function
_ZN8nlohmann10basic_jsonISt3mapSt6vectorNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEblmdSaNS_14adl_serializerEE10json_valueC2ENS_6detail7value_tE:
.LFB7459:
	.cfi_startproc
	.cfi_personality 0x9b,DW.ref.__gxx_personality_v0
	.cfi_lsda 0x1b,.LLSDA7459
	cmpb	$7, %sil
	ja	.L234
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	leaq	.L223(%rip), %rdx
	pushq	%rbx
	.cfi_def_cfa_offset 24
	.cfi_offset 3, -24
	movzbl	%sil, %eax
	movq	%rdi, %rbx
	subq	$8, %rsp
	.cfi_def_cfa_offset 32
	movslq	(%rdx,%rax,4), %rax
	addq	%rdx, %rax
	jmp	*%rax
	.section	.rodata._ZN8nlohmann10basic_jsonISt3mapSt6vectorNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEblmdSaNS_14adl_serializerEE10json_valueC2ENS_6detail7value_tE,"aG",@progbits,_ZN8nlohmann10basic_jsonISt3mapSt6vectorNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEblmdSaNS_14adl_serializerEE10json_valueC5ENS_6detail7value_tE,comdat
	.align 4
	.align 4
.L223:
	.long	.L221-.L223
	.long	.L222-.L223
	.long	.L224-.L223
	.long	.L225-.L223
	.long	.L226-.L223
	.long	.L228-.L223
	.long	.L228-.L223
	.long	.L229-.L223
	.section	.text._ZN8nlohmann10basic_jsonISt3mapSt6vectorNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEblmdSaNS_14adl_serializerEE10json_valueC2ENS_6detail7value_tE,"axG",@progbits,_ZN8nlohmann10basic_jsonISt3mapSt6vectorNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEblmdSaNS_14adl_serializerEE10json_valueC5ENS_6detail7value_tE,comdat
	.p2align 4,,10
	.p2align 3
.L228:
	movq	$0, (%rdi)
	addq	$8, %rsp
	.cfi_remember_state
	.cfi_def_cfa_offset 24
	popq	%rbx
	.cfi_def_cfa_offset 16
	popq	%rbp
	.cfi_def_cfa_offset 8
	ret
	.p2align 4,,10
	.p2align 3
.L221:
	.cfi_restore_state
	movq	$0, (%rdi)
	addq	$8, %rsp
	.cfi_remember_state
	.cfi_def_cfa_offset 24
	popq	%rbx
	.cfi_def_cfa_offset 16
	popq	%rbp
	.cfi_def_cfa_offset 8
	ret
	.p2align 4,,10
	.p2align 3
.L225:
	.cfi_restore_state
	movl	$32, %edi
.LEHB13:
	call	_Znwm@PLT
.LEHE13:
	leaq	.LC9(%rip), %rdx
	movq	%rax, %rbp
	leaq	16(%rax), %rax
	movq	%rbp, %rdi
	movq	%rax, 0(%rbp)
	movq	%rdx, %rsi
.LEHB14:
	call	_ZNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEE12_M_constructIPKcEEvT_S8_St20forward_iterator_tag.isra.71
.LEHE14:
	movq	%rbp, (%rbx)
	addq	$8, %rsp
	.cfi_remember_state
	.cfi_def_cfa_offset 24
	popq	%rbx
	.cfi_def_cfa_offset 16
	popq	%rbp
	.cfi_def_cfa_offset 8
	ret
	.p2align 4,,10
	.p2align 3
.L222:
	.cfi_restore_state
	movl	$48, %edi
.LEHB15:
	call	_Znwm@PLT
	leaq	8(%rax), %rdx
	movl	$0, 8(%rax)
	movq	$0, 16(%rax)
	movq	$0, 40(%rax)
	movq	%rdx, 24(%rax)
	movq	%rdx, 32(%rax)
	movq	%rax, (%rbx)
	addq	$8, %rsp
	.cfi_remember_state
	.cfi_def_cfa_offset 24
	popq	%rbx
	.cfi_def_cfa_offset 16
	popq	%rbp
	.cfi_def_cfa_offset 8
	ret
	.p2align 4,,10
	.p2align 3
.L226:
	.cfi_restore_state
	movb	$0, (%rdi)
	addq	$8, %rsp
	.cfi_remember_state
	.cfi_def_cfa_offset 24
	popq	%rbx
	.cfi_def_cfa_offset 16
	popq	%rbp
	.cfi_def_cfa_offset 8
	ret
	.p2align 4,,10
	.p2align 3
.L224:
	.cfi_restore_state
	movl	$24, %edi
	call	_Znwm@PLT
	movq	$0, (%rax)
	movq	$0, 8(%rax)
	movq	$0, 16(%rax)
	movq	%rax, (%rbx)
	addq	$8, %rsp
	.cfi_remember_state
	.cfi_def_cfa_offset 24
	popq	%rbx
	.cfi_def_cfa_offset 16
	popq	%rbp
	.cfi_def_cfa_offset 8
	ret
	.p2align 4,,10
	.p2align 3
.L229:
	.cfi_restore_state
	movq	$0x000000000, (%rdi)
	addq	$8, %rsp
	.cfi_def_cfa_offset 24
	popq	%rbx
	.cfi_def_cfa_offset 16
	popq	%rbp
	.cfi_def_cfa_offset 8
	ret
	.p2align 4,,10
	.p2align 3
.L234:
	.cfi_restore 3
	.cfi_restore 6
	movq	$0, (%rdi)
	ret
.L232:
	.cfi_def_cfa_offset 32
	.cfi_offset 3, -24
	.cfi_offset 6, -16
	movq	%rax, %rbx
	movq	%rbp, %rdi
	call	_ZdlPv@PLT
	movq	%rbx, %rdi
	call	_Unwind_Resume@PLT
.LEHE15:
	.cfi_endproc
.LFE7459:
	.section	.gcc_except_table._ZN8nlohmann10basic_jsonISt3mapSt6vectorNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEblmdSaNS_14adl_serializerEE10json_valueC2ENS_6detail7value_tE,"aG",@progbits,_ZN8nlohmann10basic_jsonISt3mapSt6vectorNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEblmdSaNS_14adl_serializerEE10json_valueC5ENS_6detail7value_tE,comdat
.LLSDA7459:
	.byte	0xff
	.byte	0xff
	.byte	0x1
	.uleb128 .LLSDACSE7459-.LLSDACSB7459
.LLSDACSB7459:
	.uleb128 .LEHB13-.LFB7459
	.uleb128 .LEHE13-.LEHB13
	.uleb128 0
	.uleb128 0
	.uleb128 .LEHB14-.LFB7459
	.uleb128 .LEHE14-.LEHB14
	.uleb128 .L232-.LFB7459
	.uleb128 0
	.uleb128 .LEHB15-.LFB7459
	.uleb128 .LEHE15-.LEHB15
	.uleb128 0
	.uleb128 0
.LLSDACSE7459:
	.section	.text._ZN8nlohmann10basic_jsonISt3mapSt6vectorNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEblmdSaNS_14adl_serializerEE10json_valueC2ENS_6detail7value_tE,"axG",@progbits,_ZN8nlohmann10basic_jsonISt3mapSt6vectorNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEblmdSaNS_14adl_serializerEE10json_valueC5ENS_6detail7value_tE,comdat
	.size	_ZN8nlohmann10basic_jsonISt3mapSt6vectorNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEblmdSaNS_14adl_serializerEE10json_valueC2ENS_6detail7value_tE, .-_ZN8nlohmann10basic_jsonISt3mapSt6vectorNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEblmdSaNS_14adl_serializerEE10json_valueC2ENS_6detail7value_tE
	.weak	_ZN8nlohmann10basic_jsonISt3mapSt6vectorNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEblmdSaNS_14adl_serializerEE10json_valueC1ENS_6detail7value_tE
	.set	_ZN8nlohmann10basic_jsonISt3mapSt6vectorNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEblmdSaNS_14adl_serializerEE10json_valueC1ENS_6detail7value_tE,_ZN8nlohmann10basic_jsonISt3mapSt6vectorNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEblmdSaNS_14adl_serializerEE10json_valueC2ENS_6detail7value_tE
	.section	.rodata._ZN8nlohmann6detail9from_jsonINS_10basic_jsonISt3mapSt6vectorNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEblmdSaNS_14adl_serializerEEEiLi0EEEvRKT_RT0_.str1.1,"aMS",@progbits,1
.LC11:
	.string	"null"
.LC12:
	.string	"object"
.LC13:
	.string	"array"
.LC14:
	.string	"string"
.LC15:
	.string	"boolean"
.LC16:
	.string	"discarded"
.LC17:
	.string	"number"
.LC18:
	.string	"type must be number, but is "
	.section	.text._ZN8nlohmann6detail9from_jsonINS_10basic_jsonISt3mapSt6vectorNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEblmdSaNS_14adl_serializerEEEiLi0EEEvRKT_RT0_,"axG",@progbits,_ZN8nlohmann6detail9from_jsonINS_10basic_jsonISt3mapSt6vectorNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEblmdSaNS_14adl_serializerEEEiLi0EEEvRKT_RT0_,comdat
	.p2align 4,,15
	.weak	_ZN8nlohmann6detail9from_jsonINS_10basic_jsonISt3mapSt6vectorNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEblmdSaNS_14adl_serializerEEEiLi0EEEvRKT_RT0_
	.type	_ZN8nlohmann6detail9from_jsonINS_10basic_jsonISt3mapSt6vectorNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEblmdSaNS_14adl_serializerEEEiLi0EEEvRKT_RT0_, @function
_ZN8nlohmann6detail9from_jsonINS_10basic_jsonISt3mapSt6vectorNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEblmdSaNS_14adl_serializerEEEiLi0EEEvRKT_RT0_:
.LFB7797:
	.cfi_startproc
	.cfi_personality 0x9b,DW.ref.__gxx_personality_v0
	.cfi_lsda 0x1b,.LLSDA7797
	pushq	%r13
	.cfi_def_cfa_offset 16
	.cfi_offset 13, -16
	pushq	%r12
	.cfi_def_cfa_offset 24
	.cfi_offset 12, -24
	pushq	%rbp
	.cfi_def_cfa_offset 32
	.cfi_offset 6, -32
	pushq	%rbx
	.cfi_def_cfa_offset 40
	.cfi_offset 3, -40
	movq	%rdi, %rbx
	subq	$104, %rsp
	.cfi_def_cfa_offset 144
	movq	%fs:40, %rax
	movq	%rax, 88(%rsp)
	xorl	%eax, %eax
	movzbl	(%rdi), %eax
	cmpb	$5, %al
	je	.L240
	jbe	.L267
	cmpb	$6, %al
	je	.L240
	cmpb	$7, %al
	jne	.L239
	cvttsd2si	8(%rdi), %eax
	movl	%eax, (%rsi)
	jmp	.L238
	.p2align 4,,10
	.p2align 3
.L240:
	movq	8(%rbx), %rax
	movl	%eax, (%rsi)
.L238:
	movq	88(%rsp), %rax
	xorq	%fs:40, %rax
	jne	.L268
	addq	$104, %rsp
	.cfi_remember_state
	.cfi_def_cfa_offset 40
	popq	%rbx
	.cfi_def_cfa_offset 32
	popq	%rbp
	.cfi_def_cfa_offset 24
	popq	%r12
	.cfi_def_cfa_offset 16
	popq	%r13
	.cfi_def_cfa_offset 8
	ret
	.p2align 4,,10
	.p2align 3
.L267:
	.cfi_restore_state
	cmpb	$4, %al
	jne	.L239
	movzbl	8(%rdi), %eax
	movl	%eax, (%rsi)
	jmp	.L238
.L239:
	movl	$32, %edi
	call	__cxa_allocate_exception@PLT
	cmpb	$8, (%rbx)
	movq	%rax, %r12
	ja	.L246
	movzbl	(%rbx), %eax
	leaq	.L248(%rip), %rdx
	movslq	(%rdx,%rax,4), %rax
	addq	%rdx, %rax
	jmp	*%rax
	.section	.rodata._ZN8nlohmann6detail9from_jsonINS_10basic_jsonISt3mapSt6vectorNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEblmdSaNS_14adl_serializerEEEiLi0EEEvRKT_RT0_,"aG",@progbits,_ZN8nlohmann6detail9from_jsonINS_10basic_jsonISt3mapSt6vectorNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEblmdSaNS_14adl_serializerEEEiLi0EEEvRKT_RT0_,comdat
	.align 4
	.align 4
.L248:
	.long	.L262-.L248
	.long	.L249-.L248
	.long	.L250-.L248
	.long	.L251-.L248
	.long	.L252-.L248
	.long	.L246-.L248
	.long	.L246-.L248
	.long	.L246-.L248
	.long	.L253-.L248
	.section	.text._ZN8nlohmann6detail9from_jsonINS_10basic_jsonISt3mapSt6vectorNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEblmdSaNS_14adl_serializerEEEiLi0EEEvRKT_RT0_,"axG",@progbits,_ZN8nlohmann6detail9from_jsonINS_10basic_jsonISt3mapSt6vectorNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEblmdSaNS_14adl_serializerEEEiLi0EEEvRKT_RT0_,comdat
.L253:
	leaq	.LC16(%rip), %rsi
.L247:
	leaq	16(%rsp), %rbx
	leaq	15(%rsp), %rdx
	movq	%rbx, %rdi
.LEHB16:
	call	_ZNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEC1EPKcRKS3_
.LEHE16:
	leaq	48(%rsp), %rbp
	leaq	.LC18(%rip), %rsi
	movq	%rbx, %rdx
	movq	%rbp, %rdi
.LEHB17:
	call	_ZStplIcSt11char_traitsIcESaIcEENSt7__cxx1112basic_stringIT_T0_T1_EEPKS5_OS8_
.LEHE17:
	movq	%rbp, %rdx
	movl	$302, %esi
	movq	%r12, %rdi
.LEHB18:
	call	_ZN8nlohmann6detail10type_error6createEiRKNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEE
.LEHE18:
	movq	48(%rsp), %rdi
	addq	$16, %rbp
	cmpq	%rbp, %rdi
	je	.L254
	call	_ZdlPv@PLT
.L254:
	movq	16(%rsp), %rdi
	addq	$16, %rbx
	cmpq	%rbx, %rdi
	je	.L255
	call	_ZdlPv@PLT
.L255:
	leaq	_ZN8nlohmann6detail10type_errorD1Ev(%rip), %rdx
	leaq	_ZTIN8nlohmann6detail10type_errorE(%rip), %rsi
	movq	%r12, %rdi
.LEHB19:
	call	__cxa_throw@PLT
.L252:
	leaq	.LC15(%rip), %rsi
	jmp	.L247
.L251:
	leaq	.LC14(%rip), %rsi
	jmp	.L247
.L250:
	leaq	.LC13(%rip), %rsi
	jmp	.L247
.L249:
	leaq	.LC12(%rip), %rsi
	jmp	.L247
.L262:
	leaq	.LC11(%rip), %rsi
	jmp	.L247
.L268:
	call	__stack_chk_fail@PLT
.L246:
	leaq	.LC17(%rip), %rsi
	jmp	.L247
.L263:
	movq	%rax, %rbp
.L260:
	movq	%r12, %rdi
	call	__cxa_free_exception@PLT
	movq	%rbp, %rdi
	call	_Unwind_Resume@PLT
.LEHE19:
.L265:
	movq	48(%rsp), %rdi
	addq	$16, %rbp
	movq	%rax, %r13
	cmpq	%rbp, %rdi
	je	.L257
	call	_ZdlPv@PLT
.L257:
	movq	%r13, %rbp
.L258:
	movq	16(%rsp), %rdi
	addq	$16, %rbx
	cmpq	%rbx, %rdi
	je	.L260
	call	_ZdlPv@PLT
	jmp	.L260
.L264:
	movq	%rax, %rbp
	jmp	.L258
	.cfi_endproc
.LFE7797:
	.section	.gcc_except_table._ZN8nlohmann6detail9from_jsonINS_10basic_jsonISt3mapSt6vectorNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEblmdSaNS_14adl_serializerEEEiLi0EEEvRKT_RT0_,"aG",@progbits,_ZN8nlohmann6detail9from_jsonINS_10basic_jsonISt3mapSt6vectorNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEblmdSaNS_14adl_serializerEEEiLi0EEEvRKT_RT0_,comdat
.LLSDA7797:
	.byte	0xff
	.byte	0xff
	.byte	0x1
	.uleb128 .LLSDACSE7797-.LLSDACSB7797
.LLSDACSB7797:
	.uleb128 .LEHB16-.LFB7797
	.uleb128 .LEHE16-.LEHB16
	.uleb128 .L263-.LFB7797
	.uleb128 0
	.uleb128 .LEHB17-.LFB7797
	.uleb128 .LEHE17-.LEHB17
	.uleb128 .L264-.LFB7797
	.uleb128 0
	.uleb128 .LEHB18-.LFB7797
	.uleb128 .LEHE18-.LEHB18
	.uleb128 .L265-.LFB7797
	.uleb128 0
	.uleb128 .LEHB19-.LFB7797
	.uleb128 .LEHE19-.LEHB19
	.uleb128 0
	.uleb128 0
.LLSDACSE7797:
	.section	.text._ZN8nlohmann6detail9from_jsonINS_10basic_jsonISt3mapSt6vectorNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEblmdSaNS_14adl_serializerEEEiLi0EEEvRKT_RT0_,"axG",@progbits,_ZN8nlohmann6detail9from_jsonINS_10basic_jsonISt3mapSt6vectorNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEblmdSaNS_14adl_serializerEEEiLi0EEEvRKT_RT0_,comdat
	.size	_ZN8nlohmann6detail9from_jsonINS_10basic_jsonISt3mapSt6vectorNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEblmdSaNS_14adl_serializerEEEiLi0EEEvRKT_RT0_, .-_ZN8nlohmann6detail9from_jsonINS_10basic_jsonISt3mapSt6vectorNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEblmdSaNS_14adl_serializerEEEiLi0EEEvRKT_RT0_
	.section	.text._ZNSt8_Rb_treeINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESt4pairIKS5_N8nlohmann10basic_jsonISt3mapSt6vectorS5_blmdSaNS8_14adl_serializerEEEESt10_Select1stISE_ESt4lessIvESaISE_EE8_M_eraseEPSt13_Rb_tree_nodeISE_E,"axG",@progbits,_ZNSt8_Rb_treeINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESt4pairIKS5_N8nlohmann10basic_jsonISt3mapSt6vectorS5_blmdSaNS8_14adl_serializerEEEESt10_Select1stISE_ESt4lessIvESaISE_EE8_M_eraseEPSt13_Rb_tree_nodeISE_E,comdat
	.align 2
	.p2align 4,,15
	.weak	_ZNSt8_Rb_treeINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESt4pairIKS5_N8nlohmann10basic_jsonISt3mapSt6vectorS5_blmdSaNS8_14adl_serializerEEEESt10_Select1stISE_ESt4lessIvESaISE_EE8_M_eraseEPSt13_Rb_tree_nodeISE_E
	.type	_ZNSt8_Rb_treeINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESt4pairIKS5_N8nlohmann10basic_jsonISt3mapSt6vectorS5_blmdSaNS8_14adl_serializerEEEESt10_Select1stISE_ESt4lessIvESaISE_EE8_M_eraseEPSt13_Rb_tree_nodeISE_E, @function
_ZNSt8_Rb_treeINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESt4pairIKS5_N8nlohmann10basic_jsonISt3mapSt6vectorS5_blmdSaNS8_14adl_serializerEEEESt10_Select1stISE_ESt4lessIvESaISE_EE8_M_eraseEPSt13_Rb_tree_nodeISE_E:
.LFB8030:
	.cfi_startproc
	testq	%rsi, %rsi
	je	.L290
	pushq	%r15
	.cfi_def_cfa_offset 16
	.cfi_offset 15, -16
	pushq	%r14
	.cfi_def_cfa_offset 24
	.cfi_offset 14, -24
	movq	%rdi, %r14
	pushq	%r13
	.cfi_def_cfa_offset 32
	.cfi_offset 13, -32
	pushq	%r12
	.cfi_def_cfa_offset 40
	.cfi_offset 12, -40
	pushq	%rbp
	.cfi_def_cfa_offset 48
	.cfi_offset 6, -48
	pushq	%rbx
	.cfi_def_cfa_offset 56
	.cfi_offset 3, -56
	movq	%rsi, %rbx
	subq	$8, %rsp
	.cfi_def_cfa_offset 64
.L280:
	movq	24(%rbx), %rsi
	movq	%r14, %rdi
	call	_ZNSt8_Rb_treeINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESt4pairIKS5_N8nlohmann10basic_jsonISt3mapSt6vectorS5_blmdSaNS8_14adl_serializerEEEESt10_Select1stISE_ESt4lessIvESaISE_EE8_M_eraseEPSt13_Rb_tree_nodeISE_E
	movzbl	64(%rbx), %eax
	movq	16(%rbx), %r12
	cmpb	$2, %al
	je	.L272
	cmpb	$3, %al
	je	.L273
	cmpb	$1, %al
	jne	.L271
	movq	72(%rbx), %rdi
	movq	16(%rdi), %rsi
	call	_ZNSt8_Rb_treeINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESt4pairIKS5_N8nlohmann10basic_jsonISt3mapSt6vectorS5_blmdSaNS8_14adl_serializerEEEESt10_Select1stISE_ESt4lessIvESaISE_EE8_M_eraseEPSt13_Rb_tree_nodeISE_E
	movq	72(%rbx), %rdi
	call	_ZdlPv@PLT
.L271:
	movq	32(%rbx), %rdi
	leaq	48(%rbx), %rax
	cmpq	%rax, %rdi
	je	.L279
	call	_ZdlPv@PLT
.L279:
	movq	%rbx, %rdi
	movq	%r12, %rbx
	call	_ZdlPv@PLT
	testq	%r12, %r12
	jne	.L280
	addq	$8, %rsp
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
	ret
	.p2align 4,,10
	.p2align 3
.L273:
	.cfi_restore_state
	movq	72(%rbx), %rdi
	movq	(%rdi), %rax
	leaq	16(%rdi), %rdx
	cmpq	%rdx, %rax
	je	.L278
	movq	%rax, %rdi
	call	_ZdlPv@PLT
.L293:
	movq	72(%rbx), %rdi
.L278:
	call	_ZdlPv@PLT
	jmp	.L271
	.p2align 4,,10
	.p2align 3
.L272:
	movq	72(%rbx), %r15
	movq	8(%r15), %r13
	movq	(%r15), %rbp
	cmpq	%rbp, %r13
	je	.L275
	.p2align 4,,10
	.p2align 3
.L276:
	movzbl	0(%rbp), %esi
	leaq	8(%rbp), %rdi
	addq	$16, %rbp
	call	_ZN8nlohmann10basic_jsonISt3mapSt6vectorNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEblmdSaNS_14adl_serializerEE10json_value7destroyENS_6detail7value_tE
	cmpq	%rbp, %r13
	jne	.L276
	movq	(%r15), %rbp
.L275:
	testq	%rbp, %rbp
	je	.L293
	movq	%rbp, %rdi
	call	_ZdlPv@PLT
	movq	72(%rbx), %rdi
	jmp	.L278
	.p2align 4,,10
	.p2align 3
.L290:
	.cfi_def_cfa_offset 8
	.cfi_restore 3
	.cfi_restore 6
	.cfi_restore 12
	.cfi_restore 13
	.cfi_restore 14
	.cfi_restore 15
	rep ret
	.cfi_endproc
.LFE8030:
	.size	_ZNSt8_Rb_treeINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESt4pairIKS5_N8nlohmann10basic_jsonISt3mapSt6vectorS5_blmdSaNS8_14adl_serializerEEEESt10_Select1stISE_ESt4lessIvESaISE_EE8_M_eraseEPSt13_Rb_tree_nodeISE_E, .-_ZNSt8_Rb_treeINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESt4pairIKS5_N8nlohmann10basic_jsonISt3mapSt6vectorS5_blmdSaNS8_14adl_serializerEEEESt10_Select1stISE_ESt4lessIvESaISE_EE8_M_eraseEPSt13_Rb_tree_nodeISE_E
	.section	.text._ZN8nlohmann10basic_jsonISt3mapSt6vectorNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEblmdSaNS_14adl_serializerEE10json_value7destroyENS_6detail7value_tE,"axG",@progbits,_ZN8nlohmann10basic_jsonISt3mapSt6vectorNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEblmdSaNS_14adl_serializerEE10json_value7destroyENS_6detail7value_tE,comdat
	.align 2
	.p2align 4,,15
	.weak	_ZN8nlohmann10basic_jsonISt3mapSt6vectorNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEblmdSaNS_14adl_serializerEE10json_value7destroyENS_6detail7value_tE
	.type	_ZN8nlohmann10basic_jsonISt3mapSt6vectorNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEblmdSaNS_14adl_serializerEE10json_value7destroyENS_6detail7value_tE, @function
_ZN8nlohmann10basic_jsonISt3mapSt6vectorNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEblmdSaNS_14adl_serializerEE10json_value7destroyENS_6detail7value_tE:
.LFB7053:
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
	movq	%rdi, %r13
	pushq	%rbp
	.cfi_def_cfa_offset 48
	.cfi_offset 6, -48
	pushq	%rbx
	.cfi_def_cfa_offset 56
	.cfi_offset 3, -56
	subq	$40, %rsp
	.cfi_def_cfa_offset 96
	cmpb	$2, %sil
	je	.L296
	cmpb	$3, %sil
	je	.L297
	cmpb	$1, %sil
	je	.L335
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
	ret
	.p2align 4,,10
	.p2align 3
.L335:
	.cfi_restore_state
	movq	(%rdi), %rdi
	movq	16(%rdi), %rsi
	call	_ZNSt8_Rb_treeINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESt4pairIKS5_N8nlohmann10basic_jsonISt3mapSt6vectorS5_blmdSaNS8_14adl_serializerEEEESt10_Select1stISE_ESt4lessIvESaISE_EE8_M_eraseEPSt13_Rb_tree_nodeISE_E
.L318:
	movq	0(%r13), %rdi
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
	jmp	_ZdlPv@PLT
	.p2align 4,,10
	.p2align 3
.L297:
	.cfi_restore_state
	movq	(%rdi), %rax
	movq	(%rax), %rdi
	addq	$16, %rax
	cmpq	%rax, %rdi
	je	.L318
	call	_ZdlPv@PLT
	jmp	.L318
	.p2align 4,,10
	.p2align 3
.L296:
	movq	(%rdi), %rax
	movq	8(%rax), %rdx
	movq	(%rax), %rbx
	movq	%rax, 24(%rsp)
	cmpq	%rbx, %rdx
	movq	%rdx, (%rsp)
	jne	.L316
	jmp	.L299
	.p2align 4,,10
	.p2align 3
.L337:
	cmpb	$3, %al
	je	.L302
	cmpb	$1, %al
	jne	.L300
	movq	8(%rbx), %rdi
	movq	16(%rdi), %rsi
	call	_ZNSt8_Rb_treeINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESt4pairIKS5_N8nlohmann10basic_jsonISt3mapSt6vectorS5_blmdSaNS8_14adl_serializerEEEESt10_Select1stISE_ESt4lessIvESaISE_EE8_M_eraseEPSt13_Rb_tree_nodeISE_E
	movq	8(%rbx), %rdi
	call	_ZdlPv@PLT
.L300:
	addq	$16, %rbx
	cmpq	%rbx, (%rsp)
	je	.L336
.L316:
	movzbl	(%rbx), %eax
	cmpb	$2, %al
	jne	.L337
	movq	8(%rbx), %rax
	movq	8(%rax), %r12
	movq	(%rax), %r15
	movq	%rax, 16(%rsp)
	cmpq	%r15, %r12
	jne	.L313
	jmp	.L304
	.p2align 4,,10
	.p2align 3
.L339:
	cmpb	$3, %al
	je	.L307
	cmpb	$1, %al
	jne	.L305
	movq	8(%r15), %rdi
	movq	16(%rdi), %rsi
	call	_ZNSt8_Rb_treeINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESt4pairIKS5_N8nlohmann10basic_jsonISt3mapSt6vectorS5_blmdSaNS8_14adl_serializerEEEESt10_Select1stISE_ESt4lessIvESaISE_EE8_M_eraseEPSt13_Rb_tree_nodeISE_E
	movq	8(%r15), %rdi
	call	_ZdlPv@PLT
.L305:
	addq	$16, %r15
	cmpq	%r15, %r12
	je	.L338
.L313:
	movzbl	(%r15), %eax
	cmpb	$2, %al
	jne	.L339
	movq	8(%r15), %rax
	movq	8(%rax), %rbp
	movq	(%rax), %r14
	movq	%rax, 8(%rsp)
	cmpq	%r14, %rbp
	je	.L309
	.p2align 4,,10
	.p2align 3
.L310:
	movzbl	(%r14), %esi
	leaq	8(%r14), %rdi
	addq	$16, %r14
	call	_ZN8nlohmann10basic_jsonISt3mapSt6vectorNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEblmdSaNS_14adl_serializerEE10json_value7destroyENS_6detail7value_tE
	cmpq	%r14, %rbp
	jne	.L310
	movq	8(%rsp), %rax
	movq	(%rax), %r14
.L309:
	testq	%r14, %r14
	je	.L333
	movq	%r14, %rdi
	call	_ZdlPv@PLT
.L333:
	movq	8(%r15), %rdi
.L312:
	addq	$16, %r15
	call	_ZdlPv@PLT
	cmpq	%r15, %r12
	jne	.L313
.L338:
	movq	16(%rsp), %rax
	movq	(%rax), %r15
.L304:
	testq	%r15, %r15
	je	.L334
	movq	%r15, %rdi
	call	_ZdlPv@PLT
.L334:
	movq	8(%rbx), %rdi
.L315:
	call	_ZdlPv@PLT
	addq	$16, %rbx
	cmpq	%rbx, (%rsp)
	jne	.L316
.L336:
	movq	24(%rsp), %rax
	movq	(%rax), %rbx
.L299:
	testq	%rbx, %rbx
	je	.L318
	movq	%rbx, %rdi
	call	_ZdlPv@PLT
	jmp	.L318
	.p2align 4,,10
	.p2align 3
.L307:
	movq	8(%r15), %rdi
	movq	(%rdi), %rax
	leaq	16(%rdi), %rcx
	cmpq	%rcx, %rax
	je	.L312
	movq	%rax, %rdi
	call	_ZdlPv@PLT
	jmp	.L333
	.p2align 4,,10
	.p2align 3
.L302:
	movq	8(%rbx), %rdi
	movq	(%rdi), %rax
	leaq	16(%rdi), %rdx
	cmpq	%rdx, %rax
	je	.L315
	movq	%rax, %rdi
	call	_ZdlPv@PLT
	jmp	.L334
	.cfi_endproc
.LFE7053:
	.size	_ZN8nlohmann10basic_jsonISt3mapSt6vectorNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEblmdSaNS_14adl_serializerEE10json_value7destroyENS_6detail7value_tE, .-_ZN8nlohmann10basic_jsonISt3mapSt6vectorNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEblmdSaNS_14adl_serializerEE10json_value7destroyENS_6detail7value_tE
	.section	.text._ZN8nlohmann6detail20get_arithmetic_valueINS_10basic_jsonISt3mapSt6vectorNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEblmdSaNS_14adl_serializerEEEdLi0EEEvRKT_RT0_,"axG",@progbits,_ZN8nlohmann6detail20get_arithmetic_valueINS_10basic_jsonISt3mapSt6vectorNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEblmdSaNS_14adl_serializerEEEdLi0EEEvRKT_RT0_,comdat
	.p2align 4,,15
	.weak	_ZN8nlohmann6detail20get_arithmetic_valueINS_10basic_jsonISt3mapSt6vectorNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEblmdSaNS_14adl_serializerEEEdLi0EEEvRKT_RT0_
	.type	_ZN8nlohmann6detail20get_arithmetic_valueINS_10basic_jsonISt3mapSt6vectorNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEblmdSaNS_14adl_serializerEEEdLi0EEEvRKT_RT0_, @function
_ZN8nlohmann6detail20get_arithmetic_valueINS_10basic_jsonISt3mapSt6vectorNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEblmdSaNS_14adl_serializerEEEdLi0EEEvRKT_RT0_:
.LFB8057:
	.cfi_startproc
	.cfi_personality 0x9b,DW.ref.__gxx_personality_v0
	.cfi_lsda 0x1b,.LLSDA8057
	pushq	%r13
	.cfi_def_cfa_offset 16
	.cfi_offset 13, -16
	pushq	%r12
	.cfi_def_cfa_offset 24
	.cfi_offset 12, -24
	pushq	%rbp
	.cfi_def_cfa_offset 32
	.cfi_offset 6, -32
	pushq	%rbx
	.cfi_def_cfa_offset 40
	.cfi_offset 3, -40
	movq	%rdi, %rbx
	subq	$104, %rsp
	.cfi_def_cfa_offset 144
	movq	%fs:40, %rax
	movq	%rax, 88(%rsp)
	xorl	%eax, %eax
	movzbl	(%rdi), %eax
	cmpb	$6, %al
	je	.L342
	cmpb	$7, %al
	je	.L343
	cmpb	$5, %al
	je	.L371
	movl	$32, %edi
	call	__cxa_allocate_exception@PLT
	cmpb	$8, (%rbx)
	movq	%rax, %r12
	ja	.L348
	movzbl	(%rbx), %eax
	leaq	.L350(%rip), %rdx
	movslq	(%rdx,%rax,4), %rax
	addq	%rdx, %rax
	jmp	*%rax
	.section	.rodata._ZN8nlohmann6detail20get_arithmetic_valueINS_10basic_jsonISt3mapSt6vectorNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEblmdSaNS_14adl_serializerEEEdLi0EEEvRKT_RT0_,"aG",@progbits,_ZN8nlohmann6detail20get_arithmetic_valueINS_10basic_jsonISt3mapSt6vectorNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEblmdSaNS_14adl_serializerEEEdLi0EEEvRKT_RT0_,comdat
	.align 4
	.align 4
.L350:
	.long	.L364-.L350
	.long	.L351-.L350
	.long	.L352-.L350
	.long	.L353-.L350
	.long	.L354-.L350
	.long	.L348-.L350
	.long	.L348-.L350
	.long	.L348-.L350
	.long	.L355-.L350
	.section	.text._ZN8nlohmann6detail20get_arithmetic_valueINS_10basic_jsonISt3mapSt6vectorNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEblmdSaNS_14adl_serializerEEEdLi0EEEvRKT_RT0_,"axG",@progbits,_ZN8nlohmann6detail20get_arithmetic_valueINS_10basic_jsonISt3mapSt6vectorNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEblmdSaNS_14adl_serializerEEEdLi0EEEvRKT_RT0_,comdat
	.p2align 4,,10
	.p2align 3
.L343:
	movsd	8(%rdi), %xmm0
	movsd	%xmm0, (%rsi)
.L340:
	movq	88(%rsp), %rax
	xorq	%fs:40, %rax
	jne	.L372
	addq	$104, %rsp
	.cfi_remember_state
	.cfi_def_cfa_offset 40
	popq	%rbx
	.cfi_def_cfa_offset 32
	popq	%rbp
	.cfi_def_cfa_offset 24
	popq	%r12
	.cfi_def_cfa_offset 16
	popq	%r13
	.cfi_def_cfa_offset 8
	ret
	.p2align 4,,10
	.p2align 3
.L371:
	.cfi_restore_state
	pxor	%xmm0, %xmm0
	cvtsi2sdq	8(%rdi), %xmm0
.L370:
	movsd	%xmm0, (%rsi)
	jmp	.L340
	.p2align 4,,10
	.p2align 3
.L342:
	movq	8(%rdi), %rax
	testq	%rax, %rax
	js	.L345
	pxor	%xmm0, %xmm0
	cvtsi2sdq	%rax, %xmm0
	jmp	.L370
	.p2align 4,,10
	.p2align 3
.L345:
	movq	%rax, %rdx
	pxor	%xmm0, %xmm0
	shrq	%rdx
	andl	$1, %eax
	orq	%rax, %rdx
	cvtsi2sdq	%rdx, %xmm0
	addsd	%xmm0, %xmm0
	jmp	.L370
.L355:
	leaq	.LC16(%rip), %rsi
.L349:
	leaq	16(%rsp), %rbx
	leaq	15(%rsp), %rdx
	movq	%rbx, %rdi
.LEHB20:
	call	_ZNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEC1EPKcRKS3_
.LEHE20:
	leaq	48(%rsp), %rbp
	leaq	.LC18(%rip), %rsi
	movq	%rbx, %rdx
	movq	%rbp, %rdi
.LEHB21:
	call	_ZStplIcSt11char_traitsIcESaIcEENSt7__cxx1112basic_stringIT_T0_T1_EEPKS5_OS8_
.LEHE21:
	movq	%rbp, %rdx
	movl	$302, %esi
	movq	%r12, %rdi
.LEHB22:
	call	_ZN8nlohmann6detail10type_error6createEiRKNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEE
.LEHE22:
	movq	48(%rsp), %rdi
	addq	$16, %rbp
	cmpq	%rbp, %rdi
	je	.L356
	call	_ZdlPv@PLT
.L356:
	movq	16(%rsp), %rdi
	addq	$16, %rbx
	cmpq	%rbx, %rdi
	je	.L357
	call	_ZdlPv@PLT
.L357:
	leaq	_ZN8nlohmann6detail10type_errorD1Ev(%rip), %rdx
	leaq	_ZTIN8nlohmann6detail10type_errorE(%rip), %rsi
	movq	%r12, %rdi
.LEHB23:
	call	__cxa_throw@PLT
.L354:
	leaq	.LC15(%rip), %rsi
	jmp	.L349
.L353:
	leaq	.LC14(%rip), %rsi
	jmp	.L349
.L352:
	leaq	.LC13(%rip), %rsi
	jmp	.L349
.L351:
	leaq	.LC12(%rip), %rsi
	jmp	.L349
.L364:
	leaq	.LC11(%rip), %rsi
	jmp	.L349
.L372:
	call	__stack_chk_fail@PLT
.L348:
	leaq	.LC17(%rip), %rsi
	jmp	.L349
.L365:
	movq	%rax, %rbp
.L362:
	movq	%r12, %rdi
	call	__cxa_free_exception@PLT
	movq	%rbp, %rdi
	call	_Unwind_Resume@PLT
.LEHE23:
.L367:
	movq	48(%rsp), %rdi
	addq	$16, %rbp
	movq	%rax, %r13
	cmpq	%rbp, %rdi
	je	.L359
	call	_ZdlPv@PLT
.L359:
	movq	%r13, %rbp
.L360:
	movq	16(%rsp), %rdi
	addq	$16, %rbx
	cmpq	%rbx, %rdi
	je	.L362
	call	_ZdlPv@PLT
	jmp	.L362
.L366:
	movq	%rax, %rbp
	jmp	.L360
	.cfi_endproc
.LFE8057:
	.section	.gcc_except_table._ZN8nlohmann6detail20get_arithmetic_valueINS_10basic_jsonISt3mapSt6vectorNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEblmdSaNS_14adl_serializerEEEdLi0EEEvRKT_RT0_,"aG",@progbits,_ZN8nlohmann6detail20get_arithmetic_valueINS_10basic_jsonISt3mapSt6vectorNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEblmdSaNS_14adl_serializerEEEdLi0EEEvRKT_RT0_,comdat
.LLSDA8057:
	.byte	0xff
	.byte	0xff
	.byte	0x1
	.uleb128 .LLSDACSE8057-.LLSDACSB8057
.LLSDACSB8057:
	.uleb128 .LEHB20-.LFB8057
	.uleb128 .LEHE20-.LEHB20
	.uleb128 .L365-.LFB8057
	.uleb128 0
	.uleb128 .LEHB21-.LFB8057
	.uleb128 .LEHE21-.LEHB21
	.uleb128 .L366-.LFB8057
	.uleb128 0
	.uleb128 .LEHB22-.LFB8057
	.uleb128 .LEHE22-.LEHB22
	.uleb128 .L367-.LFB8057
	.uleb128 0
	.uleb128 .LEHB23-.LFB8057
	.uleb128 .LEHE23-.LEHB23
	.uleb128 0
	.uleb128 0
.LLSDACSE8057:
	.section	.text._ZN8nlohmann6detail20get_arithmetic_valueINS_10basic_jsonISt3mapSt6vectorNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEblmdSaNS_14adl_serializerEEEdLi0EEEvRKT_RT0_,"axG",@progbits,_ZN8nlohmann6detail20get_arithmetic_valueINS_10basic_jsonISt3mapSt6vectorNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEblmdSaNS_14adl_serializerEEEdLi0EEEvRKT_RT0_,comdat
	.size	_ZN8nlohmann6detail20get_arithmetic_valueINS_10basic_jsonISt3mapSt6vectorNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEblmdSaNS_14adl_serializerEEEdLi0EEEvRKT_RT0_, .-_ZN8nlohmann6detail20get_arithmetic_valueINS_10basic_jsonISt3mapSt6vectorNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEblmdSaNS_14adl_serializerEEEdLi0EEEvRKT_RT0_
	.section	.text._ZN8nlohmann10basic_jsonISt3mapSt6vectorNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEblmdSaNS_14adl_serializerEE6createIS2_ISA_SaISA_EEJN9__gnu_cxx17__normal_iteratorIPKiS2_IiSaIiEEEESK_EEEPT_DpOT0_,"axG",@progbits,_ZN8nlohmann10basic_jsonISt3mapSt6vectorNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEblmdSaNS_14adl_serializerEE6createIS2_ISA_SaISA_EEJN9__gnu_cxx17__normal_iteratorIPKiS2_IiSaIiEEEESK_EEEPT_DpOT0_,comdat
	.p2align 4,,15
	.weak	_ZN8nlohmann10basic_jsonISt3mapSt6vectorNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEblmdSaNS_14adl_serializerEE6createIS2_ISA_SaISA_EEJN9__gnu_cxx17__normal_iteratorIPKiS2_IiSaIiEEEESK_EEEPT_DpOT0_
	.type	_ZN8nlohmann10basic_jsonISt3mapSt6vectorNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEblmdSaNS_14adl_serializerEE6createIS2_ISA_SaISA_EEJN9__gnu_cxx17__normal_iteratorIPKiS2_IiSaIiEEEESK_EEEPT_DpOT0_, @function
_ZN8nlohmann10basic_jsonISt3mapSt6vectorNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEblmdSaNS_14adl_serializerEE6createIS2_ISA_SaISA_EEJN9__gnu_cxx17__normal_iteratorIPKiS2_IiSaIiEEEESK_EEEPT_DpOT0_:
.LFB8221:
	.cfi_startproc
	.cfi_personality 0x9b,DW.ref.__gxx_personality_v0
	.cfi_lsda 0x1b,.LLSDA8221
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
	movq	%rdi, %rbx
	movl	$24, %edi
	subq	$8, %rsp
	.cfi_def_cfa_offset 48
.LEHB24:
	call	_Znwm@PLT
.LEHE24:
	movq	(%r12), %r13
	movq	(%rbx), %rbx
	movq	%rax, %rbp
	movq	$0, (%rax)
	movq	$0, 8(%rax)
	movq	$0, 16(%rax)
	movq	%r13, %rax
	subq	%rbx, %rax
	sarq	$2, %rax
	testq	%rax, %rax
	movq	%rax, %r12
	je	.L380
	movabsq	$1152921504606846975, %rdx
	cmpq	%rdx, %rax
	ja	.L388
	salq	$4, %r12
	movq	%r12, %rdi
.LEHB25:
	call	_Znwm@PLT
.L374:
	addq	%rax, %r12
	cmpq	%rbx, %r13
	movq	%rax, 0(%rbp)
	movq	%r12, 16(%rbp)
	je	.L376
	leaq	4(%rbx), %rdi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	subq	%rdi, %r13
	movq	%r13, %r8
	addq	%r13, %rdi
	shrq	$2, %r8
	.p2align 4,,10
	.p2align 3
.L377:
	movslq	(%rdx), %rsi
	addq	$4, %rdx
	movb	$5, (%rcx)
	addq	$16, %rcx
	movq	%rsi, -8(%rcx)
	cmpq	%rdi, %rdx
	jne	.L377
	leaq	1(%r8), %rdx
	salq	$4, %rdx
	addq	%rdx, %rax
.L376:
	movq	%rax, 8(%rbp)
	addq	$8, %rsp
	.cfi_remember_state
	.cfi_def_cfa_offset 40
	movq	%rbp, %rax
	popq	%rbx
	.cfi_def_cfa_offset 32
	popq	%rbp
	.cfi_def_cfa_offset 24
	popq	%r12
	.cfi_def_cfa_offset 16
	popq	%r13
	.cfi_def_cfa_offset 8
	ret
	.p2align 4,,10
	.p2align 3
.L380:
	.cfi_restore_state
	xorl	%eax, %eax
	jmp	.L374
.L388:
	call	_ZSt17__throw_bad_allocv@PLT
.LEHE25:
.L381:
	movq	0(%rbp), %rdi
	movq	%rax, %rbx
	testq	%rdi, %rdi
	je	.L379
	call	_ZdlPv@PLT
.L379:
	movq	%rbp, %rdi
	call	_ZdlPv@PLT
	movq	%rbx, %rdi
.LEHB26:
	call	_Unwind_Resume@PLT
.LEHE26:
	.cfi_endproc
.LFE8221:
	.section	.gcc_except_table._ZN8nlohmann10basic_jsonISt3mapSt6vectorNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEblmdSaNS_14adl_serializerEE6createIS2_ISA_SaISA_EEJN9__gnu_cxx17__normal_iteratorIPKiS2_IiSaIiEEEESK_EEEPT_DpOT0_,"aG",@progbits,_ZN8nlohmann10basic_jsonISt3mapSt6vectorNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEblmdSaNS_14adl_serializerEE6createIS2_ISA_SaISA_EEJN9__gnu_cxx17__normal_iteratorIPKiS2_IiSaIiEEEESK_EEEPT_DpOT0_,comdat
.LLSDA8221:
	.byte	0xff
	.byte	0xff
	.byte	0x1
	.uleb128 .LLSDACSE8221-.LLSDACSB8221
.LLSDACSB8221:
	.uleb128 .LEHB24-.LFB8221
	.uleb128 .LEHE24-.LEHB24
	.uleb128 0
	.uleb128 0
	.uleb128 .LEHB25-.LFB8221
	.uleb128 .LEHE25-.LEHB25
	.uleb128 .L381-.LFB8221
	.uleb128 0
	.uleb128 .LEHB26-.LFB8221
	.uleb128 .LEHE26-.LEHB26
	.uleb128 0
	.uleb128 0
.LLSDACSE8221:
	.section	.text._ZN8nlohmann10basic_jsonISt3mapSt6vectorNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEblmdSaNS_14adl_serializerEE6createIS2_ISA_SaISA_EEJN9__gnu_cxx17__normal_iteratorIPKiS2_IiSaIiEEEESK_EEEPT_DpOT0_,"axG",@progbits,_ZN8nlohmann10basic_jsonISt3mapSt6vectorNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEblmdSaNS_14adl_serializerEE6createIS2_ISA_SaISA_EEJN9__gnu_cxx17__normal_iteratorIPKiS2_IiSaIiEEEESK_EEEPT_DpOT0_,comdat
	.size	_ZN8nlohmann10basic_jsonISt3mapSt6vectorNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEblmdSaNS_14adl_serializerEE6createIS2_ISA_SaISA_EEJN9__gnu_cxx17__normal_iteratorIPKiS2_IiSaIiEEEESK_EEEPT_DpOT0_, .-_ZN8nlohmann10basic_jsonISt3mapSt6vectorNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEblmdSaNS_14adl_serializerEE6createIS2_ISA_SaISA_EEJN9__gnu_cxx17__normal_iteratorIPKiS2_IiSaIiEEEESK_EEEPT_DpOT0_
	.section	.text._ZNSt8_Rb_treeINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESt4pairIKS5_N8nlohmann10basic_jsonISt3mapSt6vectorS5_blmdSaNS8_14adl_serializerEEEESt10_Select1stISE_ESt4lessIvESaISE_EE7_M_copyINSK_11_Alloc_nodeEEEPSt13_Rb_tree_nodeISE_EPKSO_PSt18_Rb_tree_node_baseRT_,"axG",@progbits,_ZNSt8_Rb_treeINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESt4pairIKS5_N8nlohmann10basic_jsonISt3mapSt6vectorS5_blmdSaNS8_14adl_serializerEEEESt10_Select1stISE_ESt4lessIvESaISE_EE7_M_copyINSK_11_Alloc_nodeEEEPSt13_Rb_tree_nodeISE_EPKSO_PSt18_Rb_tree_node_baseRT_,comdat
	.align 2
	.p2align 4,,15
	.weak	_ZNSt8_Rb_treeINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESt4pairIKS5_N8nlohmann10basic_jsonISt3mapSt6vectorS5_blmdSaNS8_14adl_serializerEEEESt10_Select1stISE_ESt4lessIvESaISE_EE7_M_copyINSK_11_Alloc_nodeEEEPSt13_Rb_tree_nodeISE_EPKSO_PSt18_Rb_tree_node_baseRT_
	.type	_ZNSt8_Rb_treeINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESt4pairIKS5_N8nlohmann10basic_jsonISt3mapSt6vectorS5_blmdSaNS8_14adl_serializerEEEESt10_Select1stISE_ESt4lessIvESaISE_EE7_M_copyINSK_11_Alloc_nodeEEEPSt13_Rb_tree_nodeISE_EPKSO_PSt18_Rb_tree_node_baseRT_, @function
_ZNSt8_Rb_treeINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESt4pairIKS5_N8nlohmann10basic_jsonISt3mapSt6vectorS5_blmdSaNS8_14adl_serializerEEEESt10_Select1stISE_ESt4lessIvESaISE_EE7_M_copyINSK_11_Alloc_nodeEEEPSt13_Rb_tree_nodeISE_EPKSO_PSt18_Rb_tree_node_baseRT_:
.LFB8822:
	.cfi_startproc
	.cfi_personality 0x9b,DW.ref.__gxx_personality_v0
	.cfi_lsda 0x1b,.LLSDA8822
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
	pushq	%rbp
	.cfi_def_cfa_offset 48
	.cfi_offset 6, -48
	pushq	%rbx
	.cfi_def_cfa_offset 56
	.cfi_offset 3, -56
	movq	%rsi, %rbx
	subq	$280, %rsp
	.cfi_def_cfa_offset 336
	movq	%rdi, 80(%rsp)
	movl	$80, %edi
	movq	%rdx, 8(%rsp)
	movq	%rcx, 88(%rsp)
	movq	%fs:40, %rax
	movq	%rax, 264(%rsp)
	xorl	%eax, %eax
.LEHB27:
	call	_Znwm@PLT
.LEHE27:
	movq	32(%rbx), %rsi
	movq	40(%rbx), %rdx
	movq	%rax, %r15
	leaq	32(%rax), %rdi
	leaq	48(%rax), %rax
	addq	%rsi, %rdx
	movq	%rax, 48(%rsp)
	movq	%rax, 32(%r15)
.LEHB28:
	call	_ZNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEE12_M_constructIPcEEvT_S7_St20forward_iterator_tag.isra.67
.LEHE28:
	movzbl	64(%rbx), %eax
	movq	$0, 72(%r15)
	cmpb	$7, %al
	movb	%al, 64(%r15)
	ja	.L390
	leaq	.L392(%rip), %rdx
	movslq	(%rdx,%rax,4), %rax
	addq	%rdx, %rax
	jmp	*%rax
	.section	.rodata._ZNSt8_Rb_treeINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESt4pairIKS5_N8nlohmann10basic_jsonISt3mapSt6vectorS5_blmdSaNS8_14adl_serializerEEEESt10_Select1stISE_ESt4lessIvESaISE_EE7_M_copyINSK_11_Alloc_nodeEEEPSt13_Rb_tree_nodeISE_EPKSO_PSt18_Rb_tree_node_baseRT_,"aG",@progbits,_ZNSt8_Rb_treeINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESt4pairIKS5_N8nlohmann10basic_jsonISt3mapSt6vectorS5_blmdSaNS8_14adl_serializerEEEESt10_Select1stISE_ESt4lessIvESaISE_EE7_M_copyINSK_11_Alloc_nodeEEEPSt13_Rb_tree_nodeISE_EPKSO_PSt18_Rb_tree_node_baseRT_,comdat
	.align 4
	.align 4
.L392:
	.long	.L390-.L392
	.long	.L391-.L392
	.long	.L393-.L392
	.long	.L394-.L392
	.long	.L395-.L392
	.long	.L397-.L392
	.long	.L397-.L392
	.long	.L398-.L392
	.section	.text._ZNSt8_Rb_treeINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESt4pairIKS5_N8nlohmann10basic_jsonISt3mapSt6vectorS5_blmdSaNS8_14adl_serializerEEEESt10_Select1stISE_ESt4lessIvESaISE_EE7_M_copyINSK_11_Alloc_nodeEEEPSt13_Rb_tree_nodeISE_EPKSO_PSt18_Rb_tree_node_baseRT_,"axG",@progbits,_ZNSt8_Rb_treeINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESt4pairIKS5_N8nlohmann10basic_jsonISt3mapSt6vectorS5_blmdSaNS8_14adl_serializerEEEESt10_Select1stISE_ESt4lessIvESaISE_EE7_M_copyINSK_11_Alloc_nodeEEEPSt13_Rb_tree_nodeISE_EPKSO_PSt18_Rb_tree_node_baseRT_,comdat
	.p2align 4,,10
	.p2align 3
.L394:
	movl	$32, %edi
	movq	72(%rbx), %r12
.LEHB29:
	call	_Znwm@PLT
.LEHE29:
	movq	%rax, %rbp
	leaq	16(%rax), %rax
	movq	%rbp, %rdi
	movq	%rax, 0(%rbp)
	movq	(%r12), %rsi
	movq	8(%r12), %rdx
	addq	%rsi, %rdx
.LEHB30:
	call	_ZNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEE12_M_constructIPcEEvT_S7_St20forward_iterator_tag.isra.67
.LEHE30:
.L833:
	movq	%rbp, 72(%r15)
.L390:
	movl	(%rbx), %eax
	movq	24(%rbx), %rsi
	movq	$0, 16(%r15)
	movq	$0, 24(%r15)
	movl	%eax, (%r15)
	movq	8(%rsp), %rax
	testq	%rsi, %rsi
	movq	%rax, 8(%r15)
	jne	.L844
.L519:
	movq	16(%rbx), %rbp
	testq	%rbp, %rbp
	je	.L389
	movq	%r15, 16(%rsp)
.L652:
	movl	$80, %edi
.LEHB31:
	call	_Znwm@PLT
.LEHE31:
	movq	%rax, %rbx
	leaq	32(%rax), %rdi
	leaq	48(%rax), %rax
	movq	%rax, 32(%rbx)
	movq	32(%rbp), %rsi
	movq	40(%rbp), %rdx
	movq	%rax, 56(%rsp)
	addq	%rsi, %rdx
.LEHB32:
	call	_ZNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEE12_M_constructIPcEEvT_S7_St20forward_iterator_tag.isra.67
.LEHE32:
	movzbl	64(%rbp), %eax
	movq	$0, 72(%rbx)
	cmpb	$7, %al
	movb	%al, 64(%rbx)
	ja	.L521
	leaq	.L523(%rip), %rdx
	movslq	(%rdx,%rax,4), %rax
	addq	%rdx, %rax
	jmp	*%rax
	.section	.rodata._ZNSt8_Rb_treeINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESt4pairIKS5_N8nlohmann10basic_jsonISt3mapSt6vectorS5_blmdSaNS8_14adl_serializerEEEESt10_Select1stISE_ESt4lessIvESaISE_EE7_M_copyINSK_11_Alloc_nodeEEEPSt13_Rb_tree_nodeISE_EPKSO_PSt18_Rb_tree_node_baseRT_,"aG",@progbits,_ZNSt8_Rb_treeINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESt4pairIKS5_N8nlohmann10basic_jsonISt3mapSt6vectorS5_blmdSaNS8_14adl_serializerEEEESt10_Select1stISE_ESt4lessIvESaISE_EE7_M_copyINSK_11_Alloc_nodeEEEPSt13_Rb_tree_nodeISE_EPKSO_PSt18_Rb_tree_node_baseRT_,comdat
	.align 4
	.align 4
.L523:
	.long	.L521-.L523
	.long	.L522-.L523
	.long	.L524-.L523
	.long	.L525-.L523
	.long	.L526-.L523
	.long	.L528-.L523
	.long	.L528-.L523
	.long	.L529-.L523
	.section	.text._ZNSt8_Rb_treeINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESt4pairIKS5_N8nlohmann10basic_jsonISt3mapSt6vectorS5_blmdSaNS8_14adl_serializerEEEESt10_Select1stISE_ESt4lessIvESaISE_EE7_M_copyINSK_11_Alloc_nodeEEEPSt13_Rb_tree_nodeISE_EPKSO_PSt18_Rb_tree_node_baseRT_,"axG",@progbits,_ZNSt8_Rb_treeINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESt4pairIKS5_N8nlohmann10basic_jsonISt3mapSt6vectorS5_blmdSaNS8_14adl_serializerEEEESt10_Select1stISE_ESt4lessIvESaISE_EE7_M_copyINSK_11_Alloc_nodeEEEPSt13_Rb_tree_nodeISE_EPKSO_PSt18_Rb_tree_node_baseRT_,comdat
	.p2align 4,,10
	.p2align 3
.L528:
	movq	72(%rbp), %rax
	movq	%rax, 72(%rbx)
.L521:
	movl	0(%rbp), %eax
	movq	$0, 16(%rbx)
	movq	$0, 24(%rbx)
	movl	%eax, (%rbx)
	movq	16(%rsp), %rax
	movq	%rbx, 16(%rax)
	movq	%rax, 8(%rbx)
	movq	24(%rbp), %rsi
	testq	%rsi, %rsi
	jne	.L845
.L651:
	movq	16(%rbp), %rbp
	movq	%rbx, 16(%rsp)
	testq	%rbp, %rbp
	jne	.L652
.L389:
	movq	264(%rsp), %rbx
	xorq	%fs:40, %rbx
	movq	%r15, %rax
	jne	.L846
	addq	$280, %rsp
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
	ret
	.p2align 4,,10
	.p2align 3
.L845:
	.cfi_restore_state
	movq	88(%rsp), %rcx
	movq	80(%rsp), %rdi
	movq	%rbx, %rdx
.LEHB33:
	call	_ZNSt8_Rb_treeINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESt4pairIKS5_N8nlohmann10basic_jsonISt3mapSt6vectorS5_blmdSaNS8_14adl_serializerEEEESt10_Select1stISE_ESt4lessIvESaISE_EE7_M_copyINSK_11_Alloc_nodeEEEPSt13_Rb_tree_nodeISE_EPKSO_PSt18_Rb_tree_node_baseRT_
.LEHE33:
	movq	%rax, 24(%rbx)
	jmp	.L651
	.p2align 4,,10
	.p2align 3
.L529:
	movsd	72(%rbp), %xmm0
	movsd	%xmm0, 72(%rbx)
	jmp	.L521
	.p2align 4,,10
	.p2align 3
.L526:
	movzbl	72(%rbp), %eax
	movb	%al, 72(%rbx)
	jmp	.L521
	.p2align 4,,10
	.p2align 3
.L525:
	movl	$32, %edi
	movq	72(%rbp), %r13
.LEHB34:
	call	_Znwm@PLT
.LEHE34:
	movq	%rax, %r12
	leaq	16(%rax), %rax
	movq	%r12, %rdi
	movq	%rax, (%r12)
	movq	0(%r13), %rsi
	movq	8(%r13), %rdx
	addq	%rsi, %rdx
.LEHB35:
	call	_ZNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEE12_M_constructIPcEEvT_S7_St20forward_iterator_tag.isra.67
.LEHE35:
.L843:
	movq	%r12, 72(%rbx)
	jmp	.L521
	.p2align 4,,10
	.p2align 3
.L524:
	movl	$24, %edi
	movq	72(%rbp), %r13
.LEHB36:
	call	_Znwm@PLT
.LEHE36:
	movq	8(%r13), %r12
	subq	0(%r13), %r12
	movq	%rax, %rsi
	movq	%rax, 120(%rsp)
	movq	$0, (%rsi)
	movq	$0, 8(%rsi)
	movq	$0, 16(%rsi)
	movq	%r12, %rax
	sarq	$4, %rax
	testq	%rax, %rax
	je	.L847
	movabsq	$1152921504606846975, %rdx
	cmpq	%rdx, %rax
	ja	.L848
	movq	%r12, %rdi
.LEHB37:
	call	_Znwm@PLT
.LEHE37:
	movq	%rax, 136(%rsp)
.L536:
	movq	136(%rsp), %rcx
	movq	120(%rsp), %rax
	addq	%rcx, %r12
	movq	%rcx, (%rax)
	movq	%rcx, 8(%rax)
	movq	%r12, 16(%rax)
	movq	8(%r13), %rsi
	movq	0(%r13), %rax
	movq	%rsi, 64(%rsp)
	cmpq	%rax, %rsi
	je	.L672
	movq	%rax, %r14
	movq	%rcx, 24(%rsp)
	.p2align 4,,10
	.p2align 3
.L640:
	movzbl	(%r14), %eax
	movq	24(%rsp), %rdi
	movq	$0, 8(%rdi)
	cmpb	$7, %al
	movb	%al, (%rdi)
	ja	.L539
	leaq	.L541(%rip), %rdx
	movslq	(%rdx,%rax,4), %rax
	addq	%rdx, %rax
	jmp	*%rax
	.section	.rodata._ZNSt8_Rb_treeINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESt4pairIKS5_N8nlohmann10basic_jsonISt3mapSt6vectorS5_blmdSaNS8_14adl_serializerEEEESt10_Select1stISE_ESt4lessIvESaISE_EE7_M_copyINSK_11_Alloc_nodeEEEPSt13_Rb_tree_nodeISE_EPKSO_PSt18_Rb_tree_node_baseRT_,"aG",@progbits,_ZNSt8_Rb_treeINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESt4pairIKS5_N8nlohmann10basic_jsonISt3mapSt6vectorS5_blmdSaNS8_14adl_serializerEEEESt10_Select1stISE_ESt4lessIvESaISE_EE7_M_copyINSK_11_Alloc_nodeEEEPSt13_Rb_tree_nodeISE_EPKSO_PSt18_Rb_tree_node_baseRT_,comdat
	.align 4
	.align 4
.L541:
	.long	.L539-.L541
	.long	.L540-.L541
	.long	.L542-.L541
	.long	.L543-.L541
	.long	.L544-.L541
	.long	.L545-.L541
	.long	.L546-.L541
	.long	.L547-.L541
	.section	.text._ZNSt8_Rb_treeINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESt4pairIKS5_N8nlohmann10basic_jsonISt3mapSt6vectorS5_blmdSaNS8_14adl_serializerEEEESt10_Select1stISE_ESt4lessIvESaISE_EE7_M_copyINSK_11_Alloc_nodeEEEPSt13_Rb_tree_nodeISE_EPKSO_PSt18_Rb_tree_node_baseRT_,"axG",@progbits,_ZNSt8_Rb_treeINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESt4pairIKS5_N8nlohmann10basic_jsonISt3mapSt6vectorS5_blmdSaNS8_14adl_serializerEEEESt10_Select1stISE_ESt4lessIvESaISE_EE7_M_copyINSK_11_Alloc_nodeEEEPSt13_Rb_tree_nodeISE_EPKSO_PSt18_Rb_tree_node_baseRT_,comdat
	.p2align 4,,10
	.p2align 3
.L522:
	movl	$48, %edi
	movq	72(%rbp), %r13
.LEHB38:
	call	_Znwm@PLT
.LEHE38:
	leaq	8(%rax), %rdx
	movq	$0, 16(%rax)
	movq	%rax, %r12
	movl	$0, 8(%rax)
	movq	$0, 40(%rax)
	movq	%rdx, 24(%rax)
	movq	%rdx, 32(%rax)
	movq	16(%r13), %rsi
	testq	%rsi, %rsi
	je	.L843
	leaq	224(%rsp), %rcx
	movq	%rax, %rdi
	movq	%rax, 224(%rsp)
.LEHB39:
	call	_ZNSt8_Rb_treeINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESt4pairIKS5_N8nlohmann10basic_jsonISt3mapSt6vectorS5_blmdSaNS8_14adl_serializerEEEESt10_Select1stISE_ESt4lessIvESaISE_EE7_M_copyINSK_11_Alloc_nodeEEEPSt13_Rb_tree_nodeISE_EPKSO_PSt18_Rb_tree_node_baseRT_
.LEHE39:
	movq	%rax, %rcx
	jmp	.L531
	.p2align 4,,10
	.p2align 3
.L670:
	movq	%rdx, %rcx
.L531:
	movq	16(%rcx), %rdx
	testq	%rdx, %rdx
	jne	.L670
	movq	%rcx, 24(%r12)
	movq	%rax, %rcx
	jmp	.L532
	.p2align 4,,10
	.p2align 3
.L671:
	movq	%rdx, %rcx
.L532:
	movq	24(%rcx), %rdx
	testq	%rdx, %rdx
	jne	.L671
	movq	40(%r13), %rdx
	movq	%rcx, 32(%r12)
	movq	%rax, 16(%r12)
	movq	%rdx, 40(%r12)
	jmp	.L843
	.p2align 4,,10
	.p2align 3
.L397:
	movq	72(%rbx), %rax
	movq	%rax, 72(%r15)
	jmp	.L390
	.p2align 4,,10
	.p2align 3
.L547:
	movq	24(%rsp), %rax
	movsd	8(%r14), %xmm0
	movsd	%xmm0, 8(%rax)
	.p2align 4,,10
	.p2align 3
.L539:
	addq	$16, 24(%rsp)
	addq	$16, %r14
	cmpq	%r14, 64(%rsp)
	jne	.L640
.L538:
	movq	120(%rsp), %rax
	movq	24(%rsp), %rcx
	movq	%rcx, 8(%rax)
	movq	%rax, 72(%rbx)
	jmp	.L521
	.p2align 4,,10
	.p2align 3
.L546:
	movq	8(%r14), %rax
	movq	24(%rsp), %rsi
	movq	%rax, 8(%rsi)
	jmp	.L539
	.p2align 4,,10
	.p2align 3
.L545:
	movq	8(%r14), %rax
	movq	24(%rsp), %rdi
	movq	%rax, 8(%rdi)
	jmp	.L539
	.p2align 4,,10
	.p2align 3
.L544:
	movzbl	8(%r14), %eax
	movq	24(%rsp), %rsi
	movb	%al, 8(%rsi)
	jmp	.L539
	.p2align 4,,10
	.p2align 3
.L543:
	movl	$32, %edi
	movq	8(%r14), %r13
.LEHB40:
	call	_Znwm@PLT
.LEHE40:
	movq	%rax, %r12
	leaq	16(%rax), %rax
	movq	%r12, %rdi
	movq	%rax, (%r12)
	movq	0(%r13), %rsi
	movq	8(%r13), %rdx
	addq	%rsi, %rdx
.LEHB41:
	call	_ZNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEE12_M_constructIPcEEvT_S7_St20forward_iterator_tag.isra.67
.LEHE41:
.L840:
	movq	24(%rsp), %rax
	movq	%r12, 8(%rax)
	jmp	.L539
	.p2align 4,,10
	.p2align 3
.L540:
	movl	$48, %edi
	movq	8(%r14), %r13
.LEHB42:
	call	_Znwm@PLT
.LEHE42:
	leaq	8(%rax), %rdx
	movq	$0, 16(%rax)
	movq	%rax, %r12
	movl	$0, 8(%rax)
	movq	$0, 40(%rax)
	movq	%rdx, 24(%rax)
	movq	%rdx, 32(%rax)
	movq	16(%r13), %rsi
	testq	%rsi, %rsi
	je	.L840
	leaq	232(%rsp), %rcx
	movq	%rax, %rdi
	movq	%rax, 232(%rsp)
.LEHB43:
	call	_ZNSt8_Rb_treeINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESt4pairIKS5_N8nlohmann10basic_jsonISt3mapSt6vectorS5_blmdSaNS8_14adl_serializerEEEESt10_Select1stISE_ESt4lessIvESaISE_EE7_M_copyINSK_11_Alloc_nodeEEEPSt13_Rb_tree_nodeISE_EPKSO_PSt18_Rb_tree_node_baseRT_
.LEHE43:
	movq	%rax, %rcx
	jmp	.L549
	.p2align 4,,10
	.p2align 3
.L673:
	movq	%rdx, %rcx
.L549:
	movq	16(%rcx), %rdx
	testq	%rdx, %rdx
	jne	.L673
	movq	%rcx, 24(%r12)
	movq	%rax, %rcx
	jmp	.L550
	.p2align 4,,10
	.p2align 3
.L674:
	movq	%rdx, %rcx
.L550:
	movq	24(%rcx), %rdx
	testq	%rdx, %rdx
	jne	.L674
	movq	40(%r13), %rdx
	movq	%rcx, 32(%r12)
	movq	%rax, 16(%r12)
	movq	%rdx, 40(%r12)
	jmp	.L840
	.p2align 4,,10
	.p2align 3
.L542:
	movl	$24, %edi
	movq	8(%r14), %r13
.LEHB44:
	call	_Znwm@PLT
.LEHE44:
	movq	8(%r13), %r12
	subq	0(%r13), %r12
	movq	%rax, %rdi
	movq	%rax, 144(%rsp)
	movq	$0, (%rdi)
	movq	$0, 8(%rdi)
	movq	$0, 16(%rdi)
	movq	%r12, %rax
	sarq	$4, %rax
	testq	%rax, %rax
	je	.L849
	movabsq	$1152921504606846975, %rdx
	cmpq	%rdx, %rax
	ja	.L850
	movq	%r12, %rdi
.LEHB45:
	call	_Znwm@PLT
.LEHE45:
	movq	%rax, 168(%rsp)
.L554:
	movq	168(%rsp), %rcx
	movq	144(%rsp), %rax
	addq	%rcx, %r12
	movq	%rcx, (%rax)
	movq	%rcx, 8(%rax)
	movq	%r12, 16(%rax)
	movq	8(%r13), %rdi
	movq	0(%r13), %rax
	movq	%rdi, 96(%rsp)
	cmpq	%rax, %rdi
	je	.L675
	movq	%rax, 8(%rsp)
	movq	%rcx, 40(%rsp)
	.p2align 4,,10
	.p2align 3
.L633:
	movq	8(%rsp), %rax
	movq	40(%rsp), %rcx
	movzbl	(%rax), %eax
	movq	$0, 8(%rcx)
	cmpb	$7, %al
	movb	%al, (%rcx)
	ja	.L557
	leaq	.L559(%rip), %rsi
	movslq	(%rsi,%rax,4), %rax
	addq	%rsi, %rax
	jmp	*%rax
	.section	.rodata._ZNSt8_Rb_treeINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESt4pairIKS5_N8nlohmann10basic_jsonISt3mapSt6vectorS5_blmdSaNS8_14adl_serializerEEEESt10_Select1stISE_ESt4lessIvESaISE_EE7_M_copyINSK_11_Alloc_nodeEEEPSt13_Rb_tree_nodeISE_EPKSO_PSt18_Rb_tree_node_baseRT_,"aG",@progbits,_ZNSt8_Rb_treeINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESt4pairIKS5_N8nlohmann10basic_jsonISt3mapSt6vectorS5_blmdSaNS8_14adl_serializerEEEESt10_Select1stISE_ESt4lessIvESaISE_EE7_M_copyINSK_11_Alloc_nodeEEEPSt13_Rb_tree_nodeISE_EPKSO_PSt18_Rb_tree_node_baseRT_,comdat
	.align 4
	.align 4
.L559:
	.long	.L557-.L559
	.long	.L558-.L559
	.long	.L560-.L559
	.long	.L561-.L559
	.long	.L562-.L559
	.long	.L563-.L559
	.long	.L564-.L559
	.long	.L565-.L559
	.section	.text._ZNSt8_Rb_treeINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESt4pairIKS5_N8nlohmann10basic_jsonISt3mapSt6vectorS5_blmdSaNS8_14adl_serializerEEEESt10_Select1stISE_ESt4lessIvESaISE_EE7_M_copyINSK_11_Alloc_nodeEEEPSt13_Rb_tree_nodeISE_EPKSO_PSt18_Rb_tree_node_baseRT_,"axG",@progbits,_ZNSt8_Rb_treeINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESt4pairIKS5_N8nlohmann10basic_jsonISt3mapSt6vectorS5_blmdSaNS8_14adl_serializerEEEESt10_Select1stISE_ESt4lessIvESaISE_EE7_M_copyINSK_11_Alloc_nodeEEEPSt13_Rb_tree_nodeISE_EPKSO_PSt18_Rb_tree_node_baseRT_,comdat
	.p2align 4,,10
	.p2align 3
.L844:
	movq	88(%rsp), %rcx
	movq	80(%rsp), %rdi
	movq	%r15, %rdx
.LEHB46:
	call	_ZNSt8_Rb_treeINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESt4pairIKS5_N8nlohmann10basic_jsonISt3mapSt6vectorS5_blmdSaNS8_14adl_serializerEEEESt10_Select1stISE_ESt4lessIvESaISE_EE7_M_copyINSK_11_Alloc_nodeEEEPSt13_Rb_tree_nodeISE_EPKSO_PSt18_Rb_tree_node_baseRT_
.LEHE46:
	movq	%rax, 24(%r15)
	jmp	.L519
	.p2align 4,,10
	.p2align 3
.L565:
	movq	8(%rsp), %rax
	movsd	8(%rax), %xmm0
	movq	40(%rsp), %rax
	movsd	%xmm0, 8(%rax)
	.p2align 4,,10
	.p2align 3
.L557:
	addq	$16, 8(%rsp)
	addq	$16, 40(%rsp)
	movq	8(%rsp), %rax
	cmpq	%rax, 96(%rsp)
	jne	.L633
.L556:
	movq	40(%rsp), %rdi
	movq	144(%rsp), %rax
	movq	%rdi, 8(%rax)
	movq	24(%rsp), %rdi
	movq	%rax, 8(%rdi)
	jmp	.L539
	.p2align 4,,10
	.p2align 3
.L564:
	movq	8(%rsp), %rax
	movq	40(%rsp), %rsi
	movq	8(%rax), %rax
	movq	%rax, 8(%rsi)
	jmp	.L557
	.p2align 4,,10
	.p2align 3
.L563:
	movq	8(%rsp), %rax
	movq	40(%rsp), %rdi
	movq	8(%rax), %rax
	movq	%rax, 8(%rdi)
	jmp	.L557
	.p2align 4,,10
	.p2align 3
.L562:
	movq	8(%rsp), %rax
	movq	40(%rsp), %rcx
	movzbl	8(%rax), %eax
	movb	%al, 8(%rcx)
	jmp	.L557
	.p2align 4,,10
	.p2align 3
.L561:
	movq	8(%rsp), %rax
	movl	$32, %edi
	movq	8(%rax), %r13
.LEHB47:
	call	_Znwm@PLT
.LEHE47:
	movq	%rax, %r12
	leaq	16(%rax), %rax
	movq	%r12, %rdi
	movq	%rax, (%r12)
	movq	0(%r13), %rsi
	movq	8(%r13), %rdx
	addq	%rsi, %rdx
.LEHB48:
	call	_ZNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEE12_M_constructIPcEEvT_S7_St20forward_iterator_tag.isra.67
.LEHE48:
.L838:
	movq	40(%rsp), %rax
	movq	%r12, 8(%rax)
	jmp	.L557
	.p2align 4,,10
	.p2align 3
.L558:
	movq	8(%rsp), %rax
	movl	$48, %edi
	movq	8(%rax), %r13
.LEHB49:
	call	_Znwm@PLT
.LEHE49:
	leaq	8(%rax), %rdx
	movq	$0, 16(%rax)
	movq	%rax, %r12
	movl	$0, 8(%rax)
	movq	$0, 40(%rax)
	movq	%rdx, 24(%rax)
	movq	%rdx, 32(%rax)
	movq	16(%r13), %rsi
	testq	%rsi, %rsi
	je	.L838
	leaq	240(%rsp), %rcx
	movq	%rax, %rdi
	movq	%rax, 240(%rsp)
.LEHB50:
	call	_ZNSt8_Rb_treeINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESt4pairIKS5_N8nlohmann10basic_jsonISt3mapSt6vectorS5_blmdSaNS8_14adl_serializerEEEESt10_Select1stISE_ESt4lessIvESaISE_EE7_M_copyINSK_11_Alloc_nodeEEEPSt13_Rb_tree_nodeISE_EPKSO_PSt18_Rb_tree_node_baseRT_
.LEHE50:
	movq	%rax, %rcx
	jmp	.L567
	.p2align 4,,10
	.p2align 3
.L676:
	movq	%rdx, %rcx
.L567:
	movq	16(%rcx), %rdx
	testq	%rdx, %rdx
	jne	.L676
	movq	%rcx, 24(%r12)
	movq	%rax, %rcx
	jmp	.L568
	.p2align 4,,10
	.p2align 3
.L677:
	movq	%rdx, %rcx
.L568:
	movq	24(%rcx), %rdx
	testq	%rdx, %rdx
	jne	.L677
	movq	40(%r13), %rdx
	movq	%rcx, 32(%r12)
	movq	%rax, 16(%r12)
	movq	%rdx, 40(%r12)
	jmp	.L838
	.p2align 4,,10
	.p2align 3
.L560:
	movq	8(%rsp), %rax
	movl	$24, %edi
	movq	8(%rax), %r13
.LEHB51:
	call	_Znwm@PLT
.LEHE51:
	movq	8(%r13), %r12
	subq	0(%r13), %r12
	movq	%rax, %rcx
	movq	%rax, 160(%rsp)
	movq	$0, (%rcx)
	movq	$0, 8(%rcx)
	movq	$0, 16(%rcx)
	movq	%r12, %rax
	sarq	$4, %rax
	testq	%rax, %rax
	je	.L851
	movabsq	$1152921504606846975, %rdx
	cmpq	%rdx, %rax
	ja	.L852
	movq	%r12, %rdi
.LEHB52:
	call	_Znwm@PLT
.LEHE52:
	movq	%rax, 176(%rsp)
.L572:
	movq	176(%rsp), %rdi
	movq	160(%rsp), %rax
	addq	%rdi, %r12
	movq	%rdi, (%rax)
	movq	%rdi, 8(%rax)
	movq	%r12, 16(%rax)
	movq	8(%r13), %rcx
	movq	0(%r13), %rax
	movq	%rcx, 104(%rsp)
	cmpq	%rax, %rcx
	je	.L678
	movq	%rax, 32(%rsp)
	movq	%rdi, 48(%rsp)
	.p2align 4,,10
	.p2align 3
.L626:
	movq	32(%rsp), %rax
	movq	48(%rsp), %rdi
	movzbl	(%rax), %eax
	movq	$0, 8(%rdi)
	cmpb	$7, %al
	movb	%al, (%rdi)
	ja	.L575
	leaq	.L577(%rip), %rcx
	movslq	(%rcx,%rax,4), %rax
	addq	%rcx, %rax
	jmp	*%rax
	.section	.rodata._ZNSt8_Rb_treeINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESt4pairIKS5_N8nlohmann10basic_jsonISt3mapSt6vectorS5_blmdSaNS8_14adl_serializerEEEESt10_Select1stISE_ESt4lessIvESaISE_EE7_M_copyINSK_11_Alloc_nodeEEEPSt13_Rb_tree_nodeISE_EPKSO_PSt18_Rb_tree_node_baseRT_,"aG",@progbits,_ZNSt8_Rb_treeINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESt4pairIKS5_N8nlohmann10basic_jsonISt3mapSt6vectorS5_blmdSaNS8_14adl_serializerEEEESt10_Select1stISE_ESt4lessIvESaISE_EE7_M_copyINSK_11_Alloc_nodeEEEPSt13_Rb_tree_nodeISE_EPKSO_PSt18_Rb_tree_node_baseRT_,comdat
	.align 4
	.align 4
.L577:
	.long	.L575-.L577
	.long	.L576-.L577
	.long	.L578-.L577
	.long	.L579-.L577
	.long	.L580-.L577
	.long	.L581-.L577
	.long	.L582-.L577
	.long	.L583-.L577
	.section	.text._ZNSt8_Rb_treeINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESt4pairIKS5_N8nlohmann10basic_jsonISt3mapSt6vectorS5_blmdSaNS8_14adl_serializerEEEESt10_Select1stISE_ESt4lessIvESaISE_EE7_M_copyINSK_11_Alloc_nodeEEEPSt13_Rb_tree_nodeISE_EPKSO_PSt18_Rb_tree_node_baseRT_,"axG",@progbits,_ZNSt8_Rb_treeINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESt4pairIKS5_N8nlohmann10basic_jsonISt3mapSt6vectorS5_blmdSaNS8_14adl_serializerEEEESt10_Select1stISE_ESt4lessIvESaISE_EE7_M_copyINSK_11_Alloc_nodeEEEPSt13_Rb_tree_nodeISE_EPKSO_PSt18_Rb_tree_node_baseRT_,comdat
	.p2align 4,,10
	.p2align 3
.L391:
	movl	$48, %edi
	movq	72(%rbx), %r12
.LEHB53:
	call	_Znwm@PLT
.LEHE53:
	leaq	8(%rax), %rdx
	movq	$0, 16(%rax)
	movq	%rax, %rbp
	movl	$0, 8(%rax)
	movq	$0, 40(%rax)
	movq	%rdx, 24(%rax)
	movq	%rdx, 32(%rax)
	movq	16(%r12), %rsi
	testq	%rsi, %rsi
	je	.L833
	leaq	224(%rsp), %rcx
	movq	%rax, %rdi
	movq	%rax, 224(%rsp)
.LEHB54:
	call	_ZNSt8_Rb_treeINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESt4pairIKS5_N8nlohmann10basic_jsonISt3mapSt6vectorS5_blmdSaNS8_14adl_serializerEEEESt10_Select1stISE_ESt4lessIvESaISE_EE7_M_copyINSK_11_Alloc_nodeEEEPSt13_Rb_tree_nodeISE_EPKSO_PSt18_Rb_tree_node_baseRT_
.LEHE54:
	movq	%rax, %rcx
	jmp	.L400
	.p2align 4,,10
	.p2align 3
.L655:
	movq	%rdx, %rcx
.L400:
	movq	16(%rcx), %rdx
	testq	%rdx, %rdx
	jne	.L655
	movq	%rcx, 24(%rbp)
	movq	%rax, %rcx
	jmp	.L401
	.p2align 4,,10
	.p2align 3
.L656:
	movq	%rdx, %rcx
.L401:
	movq	24(%rcx), %rdx
	testq	%rdx, %rdx
	jne	.L656
	movq	40(%r12), %rdx
	movq	%rcx, 32(%rbp)
	movq	%rax, 16(%rbp)
	movq	%rdx, 40(%rbp)
	jmp	.L833
	.p2align 4,,10
	.p2align 3
.L393:
	movl	$24, %edi
	movq	72(%rbx), %r12
.LEHB55:
	call	_Znwm@PLT
.LEHE55:
	movq	8(%r12), %rbp
	subq	(%r12), %rbp
	movq	%rax, %rcx
	movq	%rax, 112(%rsp)
	movq	$0, (%rcx)
	movq	$0, 8(%rcx)
	movq	$0, 16(%rcx)
	movq	%rbp, %rax
	sarq	$4, %rax
	testq	%rax, %rax
	je	.L853
	movabsq	$1152921504606846975, %rdx
	cmpq	%rdx, %rax
	ja	.L854
	movq	%rbp, %rdi
.LEHB56:
	call	_Znwm@PLT
.LEHE56:
	movq	%rax, 128(%rsp)
.L405:
	movq	128(%rsp), %r14
	movq	112(%rsp), %rax
	addq	%r14, %rbp
	movq	%r14, (%rax)
	movq	%r14, 8(%rax)
	movq	%rbp, 16(%rax)
	movq	8(%r12), %rax
	movq	(%r12), %rbp
	movq	%rax, 56(%rsp)
	cmpq	%rbp, %rax
	je	.L657
	.p2align 4,,10
	.p2align 3
.L509:
	movzbl	0(%rbp), %eax
	movq	$0, 8(%r14)
	cmpb	$7, %al
	movb	%al, (%r14)
	ja	.L408
	leaq	.L410(%rip), %rdx
	movslq	(%rdx,%rax,4), %rax
	addq	%rdx, %rax
	jmp	*%rax
	.section	.rodata._ZNSt8_Rb_treeINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESt4pairIKS5_N8nlohmann10basic_jsonISt3mapSt6vectorS5_blmdSaNS8_14adl_serializerEEEESt10_Select1stISE_ESt4lessIvESaISE_EE7_M_copyINSK_11_Alloc_nodeEEEPSt13_Rb_tree_nodeISE_EPKSO_PSt18_Rb_tree_node_baseRT_,"aG",@progbits,_ZNSt8_Rb_treeINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESt4pairIKS5_N8nlohmann10basic_jsonISt3mapSt6vectorS5_blmdSaNS8_14adl_serializerEEEESt10_Select1stISE_ESt4lessIvESaISE_EE7_M_copyINSK_11_Alloc_nodeEEEPSt13_Rb_tree_nodeISE_EPKSO_PSt18_Rb_tree_node_baseRT_,comdat
	.align 4
	.align 4
.L410:
	.long	.L408-.L410
	.long	.L409-.L410
	.long	.L411-.L410
	.long	.L412-.L410
	.long	.L413-.L410
	.long	.L415-.L410
	.long	.L415-.L410
	.long	.L416-.L410
	.section	.text._ZNSt8_Rb_treeINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESt4pairIKS5_N8nlohmann10basic_jsonISt3mapSt6vectorS5_blmdSaNS8_14adl_serializerEEEESt10_Select1stISE_ESt4lessIvESaISE_EE7_M_copyINSK_11_Alloc_nodeEEEPSt13_Rb_tree_nodeISE_EPKSO_PSt18_Rb_tree_node_baseRT_,"axG",@progbits,_ZNSt8_Rb_treeINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESt4pairIKS5_N8nlohmann10basic_jsonISt3mapSt6vectorS5_blmdSaNS8_14adl_serializerEEEESt10_Select1stISE_ESt4lessIvESaISE_EE7_M_copyINSK_11_Alloc_nodeEEEPSt13_Rb_tree_nodeISE_EPKSO_PSt18_Rb_tree_node_baseRT_,comdat
	.p2align 4,,10
	.p2align 3
.L415:
	movq	8(%rbp), %rax
	movq	%rax, 8(%r14)
.L408:
	addq	$16, %r14
	addq	$16, %rbp
	cmpq	%rbp, 56(%rsp)
	jne	.L509
.L407:
	movq	112(%rsp), %rax
	movq	%r14, 8(%rax)
	movq	%rax, 72(%r15)
	jmp	.L390
	.p2align 4,,10
	.p2align 3
.L395:
	movzbl	72(%rbx), %eax
	movb	%al, 72(%r15)
	jmp	.L390
	.p2align 4,,10
	.p2align 3
.L398:
	movsd	72(%rbx), %xmm0
	movsd	%xmm0, 72(%r15)
	jmp	.L390
	.p2align 4,,10
	.p2align 3
.L583:
	movq	32(%rsp), %rax
	movsd	8(%rax), %xmm0
	movq	48(%rsp), %rax
	movsd	%xmm0, 8(%rax)
	.p2align 4,,10
	.p2align 3
.L575:
	addq	$16, 32(%rsp)
	addq	$16, 48(%rsp)
	movq	32(%rsp), %rax
	cmpq	%rax, 104(%rsp)
	jne	.L626
.L574:
	movq	160(%rsp), %rax
	movq	48(%rsp), %rcx
	movq	40(%rsp), %rsi
	movq	%rcx, 8(%rax)
	movq	%rax, 8(%rsi)
	jmp	.L557
	.p2align 4,,10
	.p2align 3
.L582:
	movq	32(%rsp), %rax
	movq	48(%rsp), %rsi
	movq	8(%rax), %rax
	movq	%rax, 8(%rsi)
	jmp	.L575
	.p2align 4,,10
	.p2align 3
.L581:
	movq	32(%rsp), %rax
	movq	48(%rsp), %rcx
	movq	8(%rax), %rax
	movq	%rax, 8(%rcx)
	jmp	.L575
	.p2align 4,,10
	.p2align 3
.L580:
	movq	32(%rsp), %rax
	movq	48(%rsp), %rsi
	movzbl	8(%rax), %eax
	movb	%al, 8(%rsi)
	jmp	.L575
	.p2align 4,,10
	.p2align 3
.L579:
	movq	32(%rsp), %rax
	movl	$32, %edi
	movq	8(%rax), %r13
.LEHB57:
	call	_Znwm@PLT
.LEHE57:
	movq	%rax, %r12
	leaq	16(%rax), %rax
	movq	%r12, %rdi
	movq	%rax, (%r12)
	movq	0(%r13), %rsi
	movq	8(%r13), %rdx
	addq	%rsi, %rdx
.LEHB58:
	call	_ZNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEE12_M_constructIPcEEvT_S7_St20forward_iterator_tag.isra.67
.LEHE58:
.L836:
	movq	48(%rsp), %rax
	movq	%r12, 8(%rax)
	jmp	.L575
	.p2align 4,,10
	.p2align 3
.L576:
	movq	32(%rsp), %rax
	movl	$48, %edi
	movq	8(%rax), %r13
.LEHB59:
	call	_Znwm@PLT
.LEHE59:
	leaq	8(%rax), %rdx
	movq	$0, 16(%rax)
	movq	%rax, %r12
	movl	$0, 8(%rax)
	movq	$0, 40(%rax)
	movq	%rdx, 24(%rax)
	movq	%rdx, 32(%rax)
	movq	16(%r13), %rsi
	testq	%rsi, %rsi
	je	.L836
	leaq	248(%rsp), %rcx
	movq	%rax, %rdi
	movq	%rax, 248(%rsp)
.LEHB60:
	call	_ZNSt8_Rb_treeINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESt4pairIKS5_N8nlohmann10basic_jsonISt3mapSt6vectorS5_blmdSaNS8_14adl_serializerEEEESt10_Select1stISE_ESt4lessIvESaISE_EE7_M_copyINSK_11_Alloc_nodeEEEPSt13_Rb_tree_nodeISE_EPKSO_PSt18_Rb_tree_node_baseRT_
.LEHE60:
	movq	%rax, %rcx
	jmp	.L585
	.p2align 4,,10
	.p2align 3
.L679:
	movq	%rdx, %rcx
.L585:
	movq	16(%rcx), %rdx
	testq	%rdx, %rdx
	jne	.L679
	movq	%rcx, 24(%r12)
	movq	%rax, %rcx
	jmp	.L586
	.p2align 4,,10
	.p2align 3
.L680:
	movq	%rdx, %rcx
.L586:
	movq	24(%rcx), %rdx
	testq	%rdx, %rdx
	jne	.L680
	movq	40(%r13), %rdx
	movq	%rcx, 32(%r12)
	movq	%rax, 16(%r12)
	movq	%rdx, 40(%r12)
	jmp	.L836
	.p2align 4,,10
	.p2align 3
.L578:
	movq	32(%rsp), %rax
	movl	$24, %edi
	movq	8(%rax), %r13
.LEHB61:
	call	_Znwm@PLT
.LEHE61:
	movq	8(%r13), %r12
	subq	0(%r13), %r12
	movq	%rax, %rsi
	movq	%rax, 184(%rsp)
	movq	$0, (%rsi)
	movq	$0, 8(%rsi)
	movq	$0, 16(%rsi)
	movq	%r12, %rax
	sarq	$4, %rax
	testq	%rax, %rax
	je	.L855
	movabsq	$1152921504606846975, %rsi
	cmpq	%rsi, %rax
	ja	.L856
	movq	%r12, %rdi
.LEHB62:
	call	_Znwm@PLT
.LEHE62:
	movq	%rax, 200(%rsp)
.L590:
	movq	200(%rsp), %rax
	movq	184(%rsp), %rdi
	addq	%rax, %r12
	movq	%rax, (%rdi)
	movq	%rax, 8(%rdi)
	movq	%r12, 16(%rdi)
	movq	8(%r13), %rcx
	movq	0(%r13), %r12
	movq	%rcx, 128(%rsp)
	cmpq	%r12, %rcx
	je	.L681
	movq	%rax, 72(%rsp)
	leaq	256(%rsp), %rax
	movq	%rax, 216(%rsp)
	.p2align 4,,10
	.p2align 3
.L619:
	movzbl	(%r12), %eax
	movq	72(%rsp), %rsi
	movq	$0, 8(%rsi)
	cmpb	$7, %al
	movb	%al, (%rsi)
	ja	.L593
	leaq	.L595(%rip), %rdi
	movslq	(%rdi,%rax,4), %rax
	addq	%rdi, %rax
	jmp	*%rax
	.section	.rodata._ZNSt8_Rb_treeINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESt4pairIKS5_N8nlohmann10basic_jsonISt3mapSt6vectorS5_blmdSaNS8_14adl_serializerEEEESt10_Select1stISE_ESt4lessIvESaISE_EE7_M_copyINSK_11_Alloc_nodeEEEPSt13_Rb_tree_nodeISE_EPKSO_PSt18_Rb_tree_node_baseRT_,"aG",@progbits,_ZNSt8_Rb_treeINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESt4pairIKS5_N8nlohmann10basic_jsonISt3mapSt6vectorS5_blmdSaNS8_14adl_serializerEEEESt10_Select1stISE_ESt4lessIvESaISE_EE7_M_copyINSK_11_Alloc_nodeEEEPSt13_Rb_tree_nodeISE_EPKSO_PSt18_Rb_tree_node_baseRT_,comdat
	.align 4
	.align 4
.L595:
	.long	.L593-.L595
	.long	.L594-.L595
	.long	.L596-.L595
	.long	.L597-.L595
	.long	.L598-.L595
	.long	.L599-.L595
	.long	.L600-.L595
	.long	.L601-.L595
	.section	.text._ZNSt8_Rb_treeINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESt4pairIKS5_N8nlohmann10basic_jsonISt3mapSt6vectorS5_blmdSaNS8_14adl_serializerEEEESt10_Select1stISE_ESt4lessIvESaISE_EE7_M_copyINSK_11_Alloc_nodeEEEPSt13_Rb_tree_nodeISE_EPKSO_PSt18_Rb_tree_node_baseRT_,"axG",@progbits,_ZNSt8_Rb_treeINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESt4pairIKS5_N8nlohmann10basic_jsonISt3mapSt6vectorS5_blmdSaNS8_14adl_serializerEEEESt10_Select1stISE_ESt4lessIvESaISE_EE7_M_copyINSK_11_Alloc_nodeEEEPSt13_Rb_tree_nodeISE_EPKSO_PSt18_Rb_tree_node_baseRT_,comdat
	.p2align 4,,10
	.p2align 3
.L409:
	movl	$48, %edi
	movq	8(%rbp), %r13
.LEHB63:
	call	_Znwm@PLT
.LEHE63:
	leaq	8(%rax), %rdx
	movq	$0, 16(%rax)
	movq	%rax, %r12
	movl	$0, 8(%rax)
	movq	$0, 40(%rax)
	movq	%rdx, 24(%rax)
	movq	%rdx, 32(%rax)
	movq	16(%r13), %rsi
	testq	%rsi, %rsi
	je	.L830
	leaq	232(%rsp), %rcx
	movq	%rax, %rdi
	movq	%rax, 232(%rsp)
.LEHB64:
	call	_ZNSt8_Rb_treeINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESt4pairIKS5_N8nlohmann10basic_jsonISt3mapSt6vectorS5_blmdSaNS8_14adl_serializerEEEESt10_Select1stISE_ESt4lessIvESaISE_EE7_M_copyINSK_11_Alloc_nodeEEEPSt13_Rb_tree_nodeISE_EPKSO_PSt18_Rb_tree_node_baseRT_
.LEHE64:
	movq	%rax, %rcx
	jmp	.L418
	.p2align 4,,10
	.p2align 3
.L658:
	movq	%rdx, %rcx
.L418:
	movq	16(%rcx), %rdx
	testq	%rdx, %rdx
	jne	.L658
	movq	%rcx, 24(%r12)
	movq	%rax, %rcx
	jmp	.L419
	.p2align 4,,10
	.p2align 3
.L659:
	movq	%rdx, %rcx
.L419:
	movq	24(%rcx), %rdx
	testq	%rdx, %rdx
	jne	.L659
	movq	40(%r13), %rdx
	movq	%rcx, 32(%r12)
	movq	%rax, 16(%r12)
	movq	%rdx, 40(%r12)
	jmp	.L830
	.p2align 4,,10
	.p2align 3
.L416:
	movsd	8(%rbp), %xmm0
	movsd	%xmm0, 8(%r14)
	jmp	.L408
	.p2align 4,,10
	.p2align 3
.L413:
	movzbl	8(%rbp), %eax
	movb	%al, 8(%r14)
	jmp	.L408
	.p2align 4,,10
	.p2align 3
.L412:
	movl	$32, %edi
	movq	8(%rbp), %r13
.LEHB65:
	call	_Znwm@PLT
.LEHE65:
	movq	%rax, %r12
	leaq	16(%rax), %rax
	movq	%r12, %rdi
	movq	%rax, (%r12)
	movq	0(%r13), %rsi
	movq	8(%r13), %rdx
	addq	%rsi, %rdx
.LEHB66:
	call	_ZNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEE12_M_constructIPcEEvT_S7_St20forward_iterator_tag.isra.67
.LEHE66:
.L830:
	movq	%r12, 8(%r14)
	jmp	.L408
	.p2align 4,,10
	.p2align 3
.L411:
	movl	$24, %edi
	movq	8(%rbp), %r13
.LEHB67:
	call	_Znwm@PLT
.LEHE67:
	movq	8(%r13), %r12
	subq	0(%r13), %r12
	movq	%rax, %rsi
	movq	%rax, 136(%rsp)
	movq	$0, (%rsi)
	movq	$0, 8(%rsi)
	movq	$0, 16(%rsi)
	movq	%r12, %rax
	sarq	$4, %rax
	testq	%rax, %rax
	je	.L857
	movabsq	$1152921504606846975, %rdx
	cmpq	%rdx, %rax
	ja	.L858
	movq	%r12, %rdi
.LEHB68:
	call	_Znwm@PLT
.LEHE68:
	movq	%rax, 160(%rsp)
.L423:
	movq	160(%rsp), %rcx
	movq	136(%rsp), %rax
	addq	%rcx, %r12
	movq	%rcx, (%rax)
	movq	%rcx, 8(%rax)
	movq	%r12, 16(%rax)
	movq	8(%r13), %rdi
	movq	0(%r13), %rax
	movq	%rdi, 72(%rsp)
	cmpq	%rax, %rdi
	je	.L660
	movq	%rax, %r13
	movq	%rcx, 24(%rsp)
	.p2align 4,,10
	.p2align 3
.L502:
	movzbl	0(%r13), %eax
	movq	24(%rsp), %rdi
	movq	$0, 8(%rdi)
	cmpb	$7, %al
	movb	%al, (%rdi)
	ja	.L426
	leaq	.L428(%rip), %rsi
	movslq	(%rsi,%rax,4), %rax
	addq	%rsi, %rax
	jmp	*%rax
	.section	.rodata._ZNSt8_Rb_treeINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESt4pairIKS5_N8nlohmann10basic_jsonISt3mapSt6vectorS5_blmdSaNS8_14adl_serializerEEEESt10_Select1stISE_ESt4lessIvESaISE_EE7_M_copyINSK_11_Alloc_nodeEEEPSt13_Rb_tree_nodeISE_EPKSO_PSt18_Rb_tree_node_baseRT_,"aG",@progbits,_ZNSt8_Rb_treeINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESt4pairIKS5_N8nlohmann10basic_jsonISt3mapSt6vectorS5_blmdSaNS8_14adl_serializerEEEESt10_Select1stISE_ESt4lessIvESaISE_EE7_M_copyINSK_11_Alloc_nodeEEEPSt13_Rb_tree_nodeISE_EPKSO_PSt18_Rb_tree_node_baseRT_,comdat
	.align 4
	.align 4
.L428:
	.long	.L426-.L428
	.long	.L427-.L428
	.long	.L429-.L428
	.long	.L430-.L428
	.long	.L431-.L428
	.long	.L432-.L428
	.long	.L433-.L428
	.long	.L434-.L428
	.section	.text._ZNSt8_Rb_treeINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESt4pairIKS5_N8nlohmann10basic_jsonISt3mapSt6vectorS5_blmdSaNS8_14adl_serializerEEEESt10_Select1stISE_ESt4lessIvESaISE_EE7_M_copyINSK_11_Alloc_nodeEEEPSt13_Rb_tree_nodeISE_EPKSO_PSt18_Rb_tree_node_baseRT_,"axG",@progbits,_ZNSt8_Rb_treeINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESt4pairIKS5_N8nlohmann10basic_jsonISt3mapSt6vectorS5_blmdSaNS8_14adl_serializerEEEESt10_Select1stISE_ESt4lessIvESaISE_EE7_M_copyINSK_11_Alloc_nodeEEEPSt13_Rb_tree_nodeISE_EPKSO_PSt18_Rb_tree_node_baseRT_,comdat
	.p2align 4,,10
	.p2align 3
.L601:
	movq	72(%rsp), %rax
	movsd	8(%r12), %xmm0
	movsd	%xmm0, 8(%rax)
	.p2align 4,,10
	.p2align 3
.L593:
	addq	$16, 72(%rsp)
	addq	$16, %r12
	cmpq	%r12, 128(%rsp)
	jne	.L619
.L592:
	movq	72(%rsp), %rcx
	movq	184(%rsp), %rax
	movq	%rcx, 8(%rax)
	movq	48(%rsp), %rcx
	movq	%rax, 8(%rcx)
	jmp	.L575
	.p2align 4,,10
	.p2align 3
.L600:
	movq	8(%r12), %rax
	movq	72(%rsp), %rcx
	movq	%rax, 8(%rcx)
	jmp	.L593
	.p2align 4,,10
	.p2align 3
.L599:
	movq	8(%r12), %rax
	movq	72(%rsp), %rsi
	movq	%rax, 8(%rsi)
	jmp	.L593
	.p2align 4,,10
	.p2align 3
.L598:
	movzbl	8(%r12), %eax
	movq	72(%rsp), %rcx
	movb	%al, 8(%rcx)
	jmp	.L593
	.p2align 4,,10
	.p2align 3
.L597:
	movq	8(%r12), %rax
	movl	$32, %edi
	movq	%rax, 112(%rsp)
.LEHB69:
	call	_Znwm@PLT
.LEHE69:
	movq	%rax, %r13
	leaq	16(%rax), %rax
	movq	%r13, %rdi
	movq	%rax, 0(%r13)
	movq	112(%rsp), %rax
	movq	(%rax), %rsi
	movq	8(%rax), %rdx
	addq	%rsi, %rdx
.LEHB70:
	call	_ZNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEE12_M_constructIPcEEvT_S7_St20forward_iterator_tag.isra.67
.LEHE70:
.L834:
	movq	72(%rsp), %rax
	movq	%r13, 8(%rax)
	jmp	.L593
	.p2align 4,,10
	.p2align 3
.L594:
	movq	8(%r12), %rax
	movl	$48, %edi
	movq	%rax, 112(%rsp)
.LEHB71:
	call	_Znwm@PLT
.LEHE71:
	leaq	8(%rax), %rdx
	movq	$0, 16(%rax)
	movq	%rax, %r13
	movl	$0, 8(%rax)
	movq	$0, 40(%rax)
	movq	%rdx, 24(%rax)
	movq	%rdx, 32(%rax)
	movq	112(%rsp), %rax
	movq	16(%rax), %rsi
	testq	%rsi, %rsi
	je	.L834
	movq	216(%rsp), %rcx
	movq	%r13, %rdi
	movq	%r13, 256(%rsp)
.LEHB72:
	call	_ZNSt8_Rb_treeINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESt4pairIKS5_N8nlohmann10basic_jsonISt3mapSt6vectorS5_blmdSaNS8_14adl_serializerEEEESt10_Select1stISE_ESt4lessIvESaISE_EE7_M_copyINSK_11_Alloc_nodeEEEPSt13_Rb_tree_nodeISE_EPKSO_PSt18_Rb_tree_node_baseRT_
.LEHE72:
	movq	%rax, %rcx
	jmp	.L603
	.p2align 4,,10
	.p2align 3
.L682:
	movq	%rdx, %rcx
.L603:
	movq	16(%rcx), %rdx
	testq	%rdx, %rdx
	jne	.L682
	movq	%rcx, 24(%r13)
	movq	%rax, %rcx
	jmp	.L604
	.p2align 4,,10
	.p2align 3
.L683:
	movq	%rdx, %rcx
.L604:
	movq	24(%rcx), %rdx
	testq	%rdx, %rdx
	jne	.L683
	movq	112(%rsp), %rdi
	movq	%rcx, 32(%r13)
	movq	40(%rdi), %rdx
	movq	%rax, 16(%r13)
	movq	%rdx, 40(%r13)
	jmp	.L834
	.p2align 4,,10
	.p2align 3
.L596:
	movl	$24, %edi
	movq	8(%r12), %r13
.LEHB73:
	call	_Znwm@PLT
.LEHE73:
	movq	%rax, %rsi
	movq	%rax, 192(%rsp)
	movq	8(%r13), %rax
	subq	0(%r13), %rax
	movq	$0, (%rsi)
	movq	$0, 8(%rsi)
	movq	$0, 16(%rsi)
	movq	%rax, 112(%rsp)
	sarq	$4, %rax
	testq	%rax, %rax
	je	.L859
	movabsq	$1152921504606846975, %rcx
	cmpq	%rcx, %rax
	ja	.L860
	movq	112(%rsp), %rdi
.LEHB74:
	call	_Znwm@PLT
.LEHE74:
	movq	%rax, 208(%rsp)
.L608:
	movq	208(%rsp), %rdi
	movq	112(%rsp), %rax
	movq	192(%rsp), %rcx
	addq	%rdi, %rax
	movq	%rdi, (%rcx)
	movq	%rdi, 8(%rcx)
	movq	%rax, 16(%rcx)
	movq	8(%r13), %rax
	movq	0(%r13), %r13
	movq	%rax, 152(%rsp)
	cmpq	%r13, %rax
	je	.L684
	movq	%rdi, 112(%rsp)
	.p2align 4,,10
	.p2align 3
.L611:
	movq	112(%rsp), %rdi
	movq	%r13, %rsi
.LEHB75:
	call	_ZN8nlohmann10basic_jsonISt3mapSt6vectorNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEblmdSaNS_14adl_serializerEEC1ERKSA_
.LEHE75:
	addq	$16, 112(%rsp)
	addq	$16, %r13
	cmpq	%r13, 152(%rsp)
	jne	.L611
.L610:
	movq	112(%rsp), %rsi
	movq	192(%rsp), %rax
	movq	%rsi, 8(%rax)
	movq	72(%rsp), %rsi
	movq	%rax, 8(%rsi)
	jmp	.L593
	.p2align 4,,10
	.p2align 3
.L434:
	movq	24(%rsp), %rax
	movsd	8(%r13), %xmm0
	movsd	%xmm0, 8(%rax)
	.p2align 4,,10
	.p2align 3
.L426:
	addq	$16, 24(%rsp)
	addq	$16, %r13
	cmpq	%r13, 72(%rsp)
	jne	.L502
.L425:
	movq	136(%rsp), %rax
	movq	24(%rsp), %rsi
	movq	%rsi, 8(%rax)
	movq	%rax, 8(%r14)
	jmp	.L408
	.p2align 4,,10
	.p2align 3
.L433:
	movq	8(%r13), %rax
	movq	24(%rsp), %rcx
	movq	%rax, 8(%rcx)
	jmp	.L426
	.p2align 4,,10
	.p2align 3
.L432:
	movq	8(%r13), %rax
	movq	24(%rsp), %rdi
	movq	%rax, 8(%rdi)
	jmp	.L426
	.p2align 4,,10
	.p2align 3
.L431:
	movzbl	8(%r13), %eax
	movq	24(%rsp), %rcx
	movb	%al, 8(%rcx)
	jmp	.L426
	.p2align 4,,10
	.p2align 3
.L430:
	movq	8(%r13), %rax
	movl	$32, %edi
	movq	%rax, 16(%rsp)
.LEHB76:
	call	_Znwm@PLT
.LEHE76:
	movq	%rax, %r12
	leaq	16(%rax), %rax
	movq	%r12, %rdi
	movq	%rax, (%r12)
	movq	16(%rsp), %rax
	movq	(%rax), %rsi
	movq	8(%rax), %rdx
	addq	%rsi, %rdx
.LEHB77:
	call	_ZNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEE12_M_constructIPcEEvT_S7_St20forward_iterator_tag.isra.67
.LEHE77:
.L828:
	movq	24(%rsp), %rax
	movq	%r12, 8(%rax)
	jmp	.L426
	.p2align 4,,10
	.p2align 3
.L427:
	movq	8(%r13), %rax
	movl	$48, %edi
	movq	%rax, 16(%rsp)
.LEHB78:
	call	_Znwm@PLT
.LEHE78:
	leaq	8(%rax), %rdx
	movq	$0, 16(%rax)
	movq	%rax, %r12
	movl	$0, 8(%rax)
	movq	$0, 40(%rax)
	movq	%rdx, 24(%rax)
	movq	%rdx, 32(%rax)
	movq	16(%rsp), %rax
	movq	16(%rax), %rsi
	testq	%rsi, %rsi
	je	.L828
	leaq	240(%rsp), %rcx
	movq	%r12, %rdi
	movq	%r12, 240(%rsp)
.LEHB79:
	call	_ZNSt8_Rb_treeINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESt4pairIKS5_N8nlohmann10basic_jsonISt3mapSt6vectorS5_blmdSaNS8_14adl_serializerEEEESt10_Select1stISE_ESt4lessIvESaISE_EE7_M_copyINSK_11_Alloc_nodeEEEPSt13_Rb_tree_nodeISE_EPKSO_PSt18_Rb_tree_node_baseRT_
.LEHE79:
	movq	%rax, %rcx
	jmp	.L436
	.p2align 4,,10
	.p2align 3
.L661:
	movq	%rdx, %rcx
.L436:
	movq	16(%rcx), %rdx
	testq	%rdx, %rdx
	jne	.L661
	movq	%rcx, 24(%r12)
	movq	%rax, %rcx
	jmp	.L437
	.p2align 4,,10
	.p2align 3
.L662:
	movq	%rdx, %rcx
.L437:
	movq	24(%rcx), %rdx
	testq	%rdx, %rdx
	jne	.L662
	movq	%rcx, 32(%r12)
	movq	16(%rsp), %rcx
	movq	40(%rcx), %rdx
	movq	%rax, 16(%r12)
	movq	%rdx, 40(%r12)
	jmp	.L828
	.p2align 4,,10
	.p2align 3
.L429:
	movq	8(%r13), %rax
	movl	$24, %edi
	movq	%rax, %r12
	movq	%rax, 16(%rsp)
.LEHB80:
	call	_Znwm@PLT
.LEHE80:
	movq	%rax, %rdi
	movq	%rax, 152(%rsp)
	movq	%r12, %rax
	movq	8(%r12), %r12
	subq	(%rax), %r12
	movq	$0, (%rdi)
	movq	$0, 8(%rdi)
	movq	$0, 16(%rdi)
	movq	%r12, %rax
	sarq	$4, %rax
	testq	%rax, %rax
	je	.L861
	movabsq	$1152921504606846975, %rdx
	cmpq	%rdx, %rax
	ja	.L862
	movq	%r12, %rdi
.LEHB81:
	call	_Znwm@PLT
.LEHE81:
	movq	%rax, 176(%rsp)
.L441:
	movq	176(%rsp), %rsi
	movq	152(%rsp), %rax
	addq	%rsi, %r12
	movq	%rsi, (%rax)
	movq	%rsi, 8(%rax)
	movq	%r12, 16(%rax)
	movq	16(%rsp), %rax
	movq	8(%rax), %rdi
	movq	(%rax), %rax
	cmpq	%rax, %rdi
	movq	%rdi, 96(%rsp)
	je	.L663
	movq	%rax, 16(%rsp)
	movq	%rsi, 40(%rsp)
	.p2align 4,,10
	.p2align 3
.L495:
	movq	16(%rsp), %rax
	movq	40(%rsp), %rsi
	movzbl	(%rax), %eax
	movq	$0, 8(%rsi)
	cmpb	$7, %al
	movb	%al, (%rsi)
	ja	.L444
	leaq	.L446(%rip), %rdi
	movslq	(%rdi,%rax,4), %rax
	addq	%rdi, %rax
	jmp	*%rax
	.section	.rodata._ZNSt8_Rb_treeINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESt4pairIKS5_N8nlohmann10basic_jsonISt3mapSt6vectorS5_blmdSaNS8_14adl_serializerEEEESt10_Select1stISE_ESt4lessIvESaISE_EE7_M_copyINSK_11_Alloc_nodeEEEPSt13_Rb_tree_nodeISE_EPKSO_PSt18_Rb_tree_node_baseRT_,"aG",@progbits,_ZNSt8_Rb_treeINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESt4pairIKS5_N8nlohmann10basic_jsonISt3mapSt6vectorS5_blmdSaNS8_14adl_serializerEEEESt10_Select1stISE_ESt4lessIvESaISE_EE7_M_copyINSK_11_Alloc_nodeEEEPSt13_Rb_tree_nodeISE_EPKSO_PSt18_Rb_tree_node_baseRT_,comdat
	.align 4
	.align 4
.L446:
	.long	.L444-.L446
	.long	.L445-.L446
	.long	.L447-.L446
	.long	.L448-.L446
	.long	.L449-.L446
	.long	.L450-.L446
	.long	.L451-.L446
	.long	.L452-.L446
	.section	.text._ZNSt8_Rb_treeINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESt4pairIKS5_N8nlohmann10basic_jsonISt3mapSt6vectorS5_blmdSaNS8_14adl_serializerEEEESt10_Select1stISE_ESt4lessIvESaISE_EE7_M_copyINSK_11_Alloc_nodeEEEPSt13_Rb_tree_nodeISE_EPKSO_PSt18_Rb_tree_node_baseRT_,"axG",@progbits,_ZNSt8_Rb_treeINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESt4pairIKS5_N8nlohmann10basic_jsonISt3mapSt6vectorS5_blmdSaNS8_14adl_serializerEEEESt10_Select1stISE_ESt4lessIvESaISE_EE7_M_copyINSK_11_Alloc_nodeEEEPSt13_Rb_tree_nodeISE_EPKSO_PSt18_Rb_tree_node_baseRT_,comdat
	.p2align 4,,10
	.p2align 3
.L448:
	movq	16(%rsp), %rax
	movl	$32, %edi
	movq	8(%rax), %rax
	movq	%rax, 32(%rsp)
.LEHB82:
	call	_Znwm@PLT
.LEHE82:
	movq	%rax, %r12
	leaq	16(%rax), %rax
	movq	%r12, %rdi
	movq	%rax, (%r12)
	movq	32(%rsp), %rax
	movq	(%rax), %rsi
	movq	8(%rax), %rdx
	addq	%rsi, %rdx
.LEHB83:
	call	_ZNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEE12_M_constructIPcEEvT_S7_St20forward_iterator_tag.isra.67
.LEHE83:
.L826:
	movq	40(%rsp), %rax
	movq	%r12, 8(%rax)
.L444:
	addq	$16, 16(%rsp)
	addq	$16, 40(%rsp)
	movq	16(%rsp), %rax
	cmpq	%rax, 96(%rsp)
	jne	.L495
.L443:
	movq	40(%rsp), %rcx
	movq	152(%rsp), %rax
	movq	%rcx, 8(%rax)
	movq	24(%rsp), %rcx
	movq	%rax, 8(%rcx)
	jmp	.L426
	.p2align 4,,10
	.p2align 3
.L452:
	movq	16(%rsp), %rax
	movsd	8(%rax), %xmm0
	movq	40(%rsp), %rax
	movsd	%xmm0, 8(%rax)
	jmp	.L444
	.p2align 4,,10
	.p2align 3
.L451:
	movq	16(%rsp), %rax
	movq	40(%rsp), %rcx
	movq	8(%rax), %rax
	movq	%rax, 8(%rcx)
	jmp	.L444
	.p2align 4,,10
	.p2align 3
.L450:
	movq	16(%rsp), %rax
	movq	40(%rsp), %rdi
	movq	8(%rax), %rax
	movq	%rax, 8(%rdi)
	jmp	.L444
	.p2align 4,,10
	.p2align 3
.L449:
	movq	16(%rsp), %rax
	movq	40(%rsp), %rcx
	movzbl	8(%rax), %eax
	movb	%al, 8(%rcx)
	jmp	.L444
	.p2align 4,,10
	.p2align 3
.L447:
	movq	16(%rsp), %rax
	movl	$24, %edi
	movq	8(%rax), %rax
	movq	%rax, %r12
	movq	%rax, 32(%rsp)
.LEHB84:
	call	_Znwm@PLT
.LEHE84:
	movq	%rax, %rdi
	movq	%rax, 168(%rsp)
	movq	%r12, %rax
	movq	8(%r12), %r12
	subq	(%rax), %r12
	movq	$0, (%rdi)
	movq	$0, 8(%rdi)
	movq	$0, 16(%rdi)
	movq	%r12, %rax
	sarq	$4, %rax
	testq	%rax, %rax
	je	.L863
	movabsq	$1152921504606846975, %rdi
	cmpq	%rdi, %rax
	ja	.L864
	movq	%r12, %rdi
.LEHB85:
	call	_Znwm@PLT
.LEHE85:
	movq	%rax, 184(%rsp)
.L459:
	movq	184(%rsp), %rcx
	movq	168(%rsp), %rax
	addq	%rcx, %r12
	movq	%rcx, (%rax)
	movq	%rcx, 8(%rax)
	movq	%r12, 16(%rax)
	movq	32(%rsp), %rax
	movq	8(%rax), %rsi
	movq	(%rax), %rax
	cmpq	%rax, %rsi
	movq	%rsi, 120(%rsp)
	je	.L666
	movq	%rax, 32(%rsp)
	leaq	256(%rsp), %rax
	movq	%rcx, 64(%rsp)
	movq	%rax, 208(%rsp)
	.p2align 4,,10
	.p2align 3
.L488:
	movq	32(%rsp), %rax
	movq	64(%rsp), %rcx
	movzbl	(%rax), %eax
	movq	$0, 8(%rcx)
	cmpb	$7, %al
	movb	%al, (%rcx)
	ja	.L462
	leaq	.L464(%rip), %rsi
	movslq	(%rsi,%rax,4), %rax
	addq	%rsi, %rax
	jmp	*%rax
	.section	.rodata._ZNSt8_Rb_treeINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESt4pairIKS5_N8nlohmann10basic_jsonISt3mapSt6vectorS5_blmdSaNS8_14adl_serializerEEEESt10_Select1stISE_ESt4lessIvESaISE_EE7_M_copyINSK_11_Alloc_nodeEEEPSt13_Rb_tree_nodeISE_EPKSO_PSt18_Rb_tree_node_baseRT_,"aG",@progbits,_ZNSt8_Rb_treeINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESt4pairIKS5_N8nlohmann10basic_jsonISt3mapSt6vectorS5_blmdSaNS8_14adl_serializerEEEESt10_Select1stISE_ESt4lessIvESaISE_EE7_M_copyINSK_11_Alloc_nodeEEEPSt13_Rb_tree_nodeISE_EPKSO_PSt18_Rb_tree_node_baseRT_,comdat
	.align 4
	.align 4
.L464:
	.long	.L462-.L464
	.long	.L463-.L464
	.long	.L465-.L464
	.long	.L466-.L464
	.long	.L467-.L464
	.long	.L468-.L464
	.long	.L469-.L464
	.long	.L470-.L464
	.section	.text._ZNSt8_Rb_treeINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESt4pairIKS5_N8nlohmann10basic_jsonISt3mapSt6vectorS5_blmdSaNS8_14adl_serializerEEEESt10_Select1stISE_ESt4lessIvESaISE_EE7_M_copyINSK_11_Alloc_nodeEEEPSt13_Rb_tree_nodeISE_EPKSO_PSt18_Rb_tree_node_baseRT_,"axG",@progbits,_ZNSt8_Rb_treeINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESt4pairIKS5_N8nlohmann10basic_jsonISt3mapSt6vectorS5_blmdSaNS8_14adl_serializerEEEESt10_Select1stISE_ESt4lessIvESaISE_EE7_M_copyINSK_11_Alloc_nodeEEEPSt13_Rb_tree_nodeISE_EPKSO_PSt18_Rb_tree_node_baseRT_,comdat
	.p2align 4,,10
	.p2align 3
.L445:
	movq	16(%rsp), %rax
	movl	$48, %edi
	movq	8(%rax), %rax
	movq	%rax, 32(%rsp)
.LEHB86:
	call	_Znwm@PLT
.LEHE86:
	leaq	8(%rax), %rdx
	movq	$0, 16(%rax)
	movq	%rax, %r12
	movl	$0, 8(%rax)
	movq	$0, 40(%rax)
	movq	%rdx, 24(%rax)
	movq	%rdx, 32(%rax)
	movq	32(%rsp), %rax
	movq	16(%rax), %rsi
	testq	%rsi, %rsi
	je	.L826
	leaq	248(%rsp), %rcx
	movq	%r12, %rdi
	movq	%r12, 248(%rsp)
.LEHB87:
	call	_ZNSt8_Rb_treeINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESt4pairIKS5_N8nlohmann10basic_jsonISt3mapSt6vectorS5_blmdSaNS8_14adl_serializerEEEESt10_Select1stISE_ESt4lessIvESaISE_EE7_M_copyINSK_11_Alloc_nodeEEEPSt13_Rb_tree_nodeISE_EPKSO_PSt18_Rb_tree_node_baseRT_
.LEHE87:
	movq	%rax, %rcx
	jmp	.L454
	.p2align 4,,10
	.p2align 3
.L664:
	movq	%rdx, %rcx
.L454:
	movq	16(%rcx), %rdx
	testq	%rdx, %rdx
	jne	.L664
	movq	%rcx, 24(%r12)
	movq	%rax, %rcx
	jmp	.L455
	.p2align 4,,10
	.p2align 3
.L665:
	movq	%rdx, %rcx
.L455:
	movq	24(%rcx), %rdx
	testq	%rdx, %rdx
	jne	.L665
	movq	32(%rsp), %rdi
	movq	%rcx, 32(%r12)
	movq	40(%rdi), %rdx
	movq	%rax, 16(%r12)
	movq	%rdx, 40(%r12)
	jmp	.L826
	.p2align 4,,10
	.p2align 3
.L675:
	movq	168(%rsp), %rax
	movq	%rax, 40(%rsp)
	jmp	.L556
	.p2align 4,,10
	.p2align 3
.L470:
	movq	32(%rsp), %rax
	movsd	8(%rax), %xmm0
	movq	64(%rsp), %rax
	movsd	%xmm0, 8(%rax)
	.p2align 4,,10
	.p2align 3
.L462:
	addq	$16, 32(%rsp)
	addq	$16, 64(%rsp)
	movq	32(%rsp), %rax
	cmpq	%rax, 120(%rsp)
	jne	.L488
.L461:
	movq	168(%rsp), %rax
	movq	64(%rsp), %rcx
	movq	40(%rsp), %rdi
	movq	%rcx, 8(%rax)
	movq	%rax, 8(%rdi)
	jmp	.L444
	.p2align 4,,10
	.p2align 3
.L469:
	movq	32(%rsp), %rax
	movq	64(%rsp), %rcx
	movq	8(%rax), %rax
	movq	%rax, 8(%rcx)
	jmp	.L462
	.p2align 4,,10
	.p2align 3
.L468:
	movq	32(%rsp), %rax
	movq	64(%rsp), %rsi
	movq	8(%rax), %rax
	movq	%rax, 8(%rsi)
	jmp	.L462
	.p2align 4,,10
	.p2align 3
.L467:
	movq	32(%rsp), %rax
	movq	64(%rsp), %rdi
	movzbl	8(%rax), %eax
	movb	%al, 8(%rdi)
	jmp	.L462
	.p2align 4,,10
	.p2align 3
.L466:
	movq	32(%rsp), %rax
	movl	$32, %edi
	movq	8(%rax), %rax
	movq	%rax, 104(%rsp)
.LEHB88:
	call	_Znwm@PLT
.LEHE88:
	movq	%rax, %r12
	leaq	16(%rax), %rax
	movq	%r12, %rdi
	movq	%rax, (%r12)
	movq	104(%rsp), %rax
	movq	(%rax), %rsi
	movq	8(%rax), %rdx
	addq	%rsi, %rdx
.LEHB89:
	call	_ZNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEE12_M_constructIPcEEvT_S7_St20forward_iterator_tag.isra.67
.LEHE89:
.L824:
	movq	64(%rsp), %rax
	movq	%r12, 8(%rax)
	jmp	.L462
	.p2align 4,,10
	.p2align 3
.L463:
	movq	32(%rsp), %rax
	movl	$48, %edi
	movq	8(%rax), %rax
	movq	%rax, 104(%rsp)
.LEHB90:
	call	_Znwm@PLT
.LEHE90:
	leaq	8(%rax), %rdx
	movq	$0, 16(%rax)
	movq	%rax, %r12
	movl	$0, 8(%rax)
	movq	$0, 40(%rax)
	movq	%rdx, 24(%rax)
	movq	%rdx, 32(%rax)
	movq	104(%rsp), %rax
	movq	16(%rax), %rsi
	testq	%rsi, %rsi
	je	.L824
	movq	208(%rsp), %rcx
	movq	%r12, %rdi
	movq	%r12, 256(%rsp)
.LEHB91:
	call	_ZNSt8_Rb_treeINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESt4pairIKS5_N8nlohmann10basic_jsonISt3mapSt6vectorS5_blmdSaNS8_14adl_serializerEEEESt10_Select1stISE_ESt4lessIvESaISE_EE7_M_copyINSK_11_Alloc_nodeEEEPSt13_Rb_tree_nodeISE_EPKSO_PSt18_Rb_tree_node_baseRT_
.LEHE91:
	movq	%rax, %rcx
	jmp	.L472
	.p2align 4,,10
	.p2align 3
.L667:
	movq	%rdx, %rcx
.L472:
	movq	16(%rcx), %rdx
	testq	%rdx, %rdx
	jne	.L667
	movq	%rcx, 24(%r12)
	movq	%rax, %rcx
	jmp	.L473
	.p2align 4,,10
	.p2align 3
.L668:
	movq	%rdx, %rcx
.L473:
	movq	24(%rcx), %rdx
	testq	%rdx, %rdx
	jne	.L668
	movq	104(%rsp), %rsi
	movq	%rcx, 32(%r12)
	movq	40(%rsi), %rdx
	movq	%rax, 16(%r12)
	movq	%rdx, 40(%r12)
	jmp	.L824
	.p2align 4,,10
	.p2align 3
.L465:
	movq	32(%rsp), %rax
	movl	$24, %edi
	movq	8(%rax), %r12
.LEHB92:
	call	_Znwm@PLT
.LEHE92:
	movq	%rax, %rsi
	movq	%rax, 192(%rsp)
	movq	8(%r12), %rax
	subq	(%r12), %rax
	movq	$0, (%rsi)
	movq	$0, 8(%rsi)
	movq	$0, 16(%rsi)
	movq	%rax, 104(%rsp)
	sarq	$4, %rax
	testq	%rax, %rax
	je	.L865
	movabsq	$1152921504606846975, %rdi
	cmpq	%rdi, %rax
	ja	.L866
	movq	104(%rsp), %rdi
.LEHB93:
	call	_Znwm@PLT
.LEHE93:
	movq	%rax, 200(%rsp)
.L477:
	movq	200(%rsp), %rdi
	movq	104(%rsp), %rax
	movq	192(%rsp), %rsi
	addq	%rdi, %rax
	movq	%rdi, (%rsi)
	movq	%rdi, 8(%rsi)
	movq	%rax, 16(%rsi)
	movq	8(%r12), %rax
	movq	(%r12), %r12
	movq	%rax, 144(%rsp)
	cmpq	%r12, %rax
	je	.L669
	movq	%rdi, 104(%rsp)
	.p2align 4,,10
	.p2align 3
.L480:
	movq	104(%rsp), %rdi
	movq	%r12, %rsi
.LEHB94:
	call	_ZN8nlohmann10basic_jsonISt3mapSt6vectorNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEblmdSaNS_14adl_serializerEEC1ERKSA_
.LEHE94:
	addq	$16, 104(%rsp)
	addq	$16, %r12
	cmpq	%r12, 144(%rsp)
	jne	.L480
.L479:
	movq	192(%rsp), %rax
	movq	104(%rsp), %rsi
	movq	64(%rsp), %rcx
	movq	%rsi, 8(%rax)
	movq	%rax, 8(%rcx)
	jmp	.L462
	.p2align 4,,10
	.p2align 3
.L672:
	movq	136(%rsp), %rax
	movq	%rax, 24(%rsp)
	jmp	.L538
	.p2align 4,,10
	.p2align 3
.L847:
	movq	$0, 136(%rsp)
	jmp	.L536
.L678:
	movq	176(%rsp), %rax
	movq	%rax, 48(%rsp)
	jmp	.L574
.L849:
	movq	$0, 168(%rsp)
	jmp	.L554
.L657:
	movq	128(%rsp), %r14
	jmp	.L407
.L681:
	movq	200(%rsp), %rax
	movq	%rax, 72(%rsp)
	jmp	.L592
.L851:
	movq	$0, 176(%rsp)
	jmp	.L572
.L660:
	movq	160(%rsp), %rax
	movq	%rax, 24(%rsp)
	jmp	.L425
.L684:
	movq	208(%rsp), %rax
	movq	%rax, 112(%rsp)
	jmp	.L610
.L853:
	movq	$0, 128(%rsp)
	jmp	.L405
.L855:
	movq	$0, 200(%rsp)
	jmp	.L590
.L663:
	movq	176(%rsp), %rax
	movq	%rax, 40(%rsp)
	jmp	.L443
.L857:
	movq	$0, 160(%rsp)
	jmp	.L423
.L859:
	movq	$0, 208(%rsp)
	jmp	.L608
.L666:
	movq	184(%rsp), %rax
	movq	%rax, 64(%rsp)
	jmp	.L461
.L861:
	movq	$0, 176(%rsp)
	jmp	.L441
.L669:
	movq	200(%rsp), %rax
	movq	%rax, 104(%rsp)
	jmp	.L479
.L863:
	movq	$0, 184(%rsp)
	jmp	.L459
.L865:
	movq	$0, 200(%rsp)
	jmp	.L477
.L846:
	call	__stack_chk_fail@PLT
.L691:
	movq	%rax, %rbx
.L514:
	movq	112(%rsp), %rdi
	call	_ZdlPv@PLT
.L403:
	movq	32(%r15), %rdi
	cmpq	%rdi, 48(%rsp)
	je	.L516
	call	_ZdlPv@PLT
.L516:
	movq	%rbx, %rax
.L517:
	movq	%rax, %rdi
	call	__cxa_begin_catch@PLT
	movq	%r15, %rdi
	call	_ZdlPv@PLT
.LEHB95:
	call	__cxa_rethrow@PLT
.LEHE95:
.L729:
	movq	%rax, %rbp
.L588:
	movq	%rbp, %rdi
	call	__cxa_begin_catch@PLT
	movq	176(%rsp), %rbp
.L628:
	cmpq	%rbp, 48(%rsp)
	je	.L627
	movzbl	0(%rbp), %esi
	leaq	8(%rbp), %rdi
	addq	$16, %rbp
	call	_ZN8nlohmann10basic_jsonISt3mapSt6vectorNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEblmdSaNS_14adl_serializerEE10json_value7destroyENS_6detail7value_tE
	jmp	.L628
.L714:
.L832:
	movq	%rbp, %rdi
	movq	%rax, %rbx
	call	_ZdlPv@PLT
	jmp	.L403
.L688:
	movq	%rax, %rbx
	call	__cxa_end_catch@PLT
	movq	%rbx, %rdi
.LEHB96:
	call	_Unwind_Resume@PLT
.LEHE96:
.L627:
.LEHB97:
	call	__cxa_rethrow@PLT
.LEHE97:
.L854:
.LEHB98:
	call	_ZSt17__throw_bad_allocv@PLT
.LEHE98:
.L730:
.L837:
	movq	%r12, %rdi
	movq	%rax, %rbp
	call	_ZdlPv@PLT
	jmp	.L588
.L739:
	jmp	.L837
.L856:
.LEHB99:
	call	_ZSt17__throw_bad_allocv@PLT
.LEHE99:
.L728:
	movq	%rax, %rbp
	call	__cxa_end_catch@PLT
	movq	160(%rsp), %rax
	movq	(%rax), %rdi
	testq	%rdi, %rdi
	je	.L631
	call	_ZdlPv@PLT
.L631:
	movq	160(%rsp), %rdi
	call	_ZdlPv@PLT
.L570:
	movq	%rbp, %rdi
	call	__cxa_begin_catch@PLT
	movq	168(%rsp), %rbp
.L635:
	cmpq	40(%rsp), %rbp
	je	.L634
	movzbl	0(%rbp), %esi
	leaq	8(%rbp), %rdi
	addq	$16, %rbp
	call	_ZN8nlohmann10basic_jsonISt3mapSt6vectorNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEblmdSaNS_14adl_serializerEE10json_value7destroyENS_6detail7value_tE
	jmp	.L635
.L864:
.LEHB100:
	call	_ZSt17__throw_bad_allocv@PLT
.LEHE100:
.L742:
.L842:
	movq	%r12, %rdi
	movq	%rax, %rbp
	call	_ZdlPv@PLT
.L534:
	movq	32(%rbx), %rdi
	cmpq	%rdi, 56(%rsp)
	je	.L647
	call	_ZdlPv@PLT
.L647:
	movq	%rbp, %rax
.L648:
	movq	%rax, %rdi
	call	__cxa_begin_catch@PLT
	movq	%rbx, %rdi
	call	_ZdlPv@PLT
.LEHB101:
	call	__cxa_rethrow@PLT
.LEHE101:
.L719:
	movq	%rax, %rbp
.L645:
	movq	120(%rsp), %rdi
	call	_ZdlPv@PLT
	jmp	.L534
.L716:
	movq	%rax, %rbx
	call	__cxa_end_catch@PLT
	movq	%rbx, %rax
.L650:
	movq	%rax, %rdi
	call	__cxa_begin_catch@PLT
	movq	80(%rsp), %rdi
	movq	%r15, %rsi
	call	_ZNSt8_Rb_treeINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESt4pairIKS5_N8nlohmann10basic_jsonISt3mapSt6vectorS5_blmdSaNS8_14adl_serializerEEEESt10_Select1stISE_ESt4lessIvESaISE_EE8_M_eraseEPSt13_Rb_tree_nodeISE_E
.LEHB102:
	call	__cxa_rethrow@PLT
.LEHE102:
.L741:
.L841:
	movq	%r12, %rdi
	movq	%rax, %rbp
	call	_ZdlPv@PLT
.L552:
	movq	%rbp, %rdi
	call	__cxa_begin_catch@PLT
	movq	136(%rsp), %rbp
.L642:
	cmpq	24(%rsp), %rbp
	je	.L641
	movzbl	0(%rbp), %esi
	leaq	8(%rbp), %rdi
	addq	$16, %rbp
	call	_ZN8nlohmann10basic_jsonISt3mapSt6vectorNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEblmdSaNS_14adl_serializerEE10json_value7destroyENS_6detail7value_tE
	jmp	.L642
.L721:
	movq	%rax, %rbp
	jmp	.L552
.L686:
	movq	%rax, %rbx
	call	__cxa_end_catch@PLT
	movq	%rbx, %rdi
.LEHB103:
	call	_Unwind_Resume@PLT
.LEHE103:
.L641:
.LEHB104:
	call	__cxa_rethrow@PLT
.LEHE104:
.L723:
	movq	%rax, %rbp
.L638:
	movq	144(%rsp), %rdi
	call	_ZdlPv@PLT
	jmp	.L552
.L848:
.LEHB105:
	call	_ZSt17__throw_bad_allocv@PLT
.LEHE105:
.L687:
	jmp	.L517
.L850:
.LEHB106:
	call	_ZSt17__throw_bad_allocv@PLT
.LEHE106:
.L720:
	movq	%rax, %rbp
	call	__cxa_end_catch@PLT
	movq	120(%rsp), %rax
	movq	(%rax), %rdi
	testq	%rdi, %rdi
	je	.L645
	call	_ZdlPv@PLT
	jmp	.L645
.L733:
	movq	%rax, %rbp
.L606:
	movq	%rbp, %rdi
	call	__cxa_begin_catch@PLT
	movq	200(%rsp), %rbp
.L621:
	cmpq	72(%rsp), %rbp
	je	.L620
	movzbl	0(%rbp), %esi
	leaq	8(%rbp), %rdi
	addq	$16, %rbp
	call	_ZN8nlohmann10basic_jsonISt3mapSt6vectorNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEblmdSaNS_14adl_serializerEE10json_value7destroyENS_6detail7value_tE
	jmp	.L621
.L735:
	movq	%rax, %rbp
.L617:
	movq	192(%rsp), %rdi
	call	_ZdlPv@PLT
	jmp	.L606
.L620:
.LEHB107:
	call	__cxa_rethrow@PLT
.LEHE107:
.L860:
.LEHB108:
	call	_ZSt17__throw_bad_allocv@PLT
.LEHE108:
.L703:
	movq	%rax, %rbx
.L493:
	movq	168(%rsp), %rdi
	call	_ZdlPv@PLT
.L457:
	movq	%rbx, %rdi
	call	__cxa_begin_catch@PLT
	movq	176(%rsp), %rbx
.L497:
	cmpq	40(%rsp), %rbx
	je	.L496
	movzbl	(%rbx), %esi
	leaq	8(%rbx), %rdi
	addq	$16, %rbx
	call	_ZN8nlohmann10basic_jsonISt3mapSt6vectorNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEblmdSaNS_14adl_serializerEE10json_value7destroyENS_6detail7value_tE
	jmp	.L497
.L732:
	movq	%rax, %rbp
	call	__cxa_end_catch@PLT
	movq	184(%rsp), %rax
	movq	(%rax), %rdi
	testq	%rdi, %rdi
	je	.L624
	call	_ZdlPv@PLT
.L624:
	movq	184(%rsp), %rdi
	call	_ZdlPv@PLT
	jmp	.L588
.L496:
.LEHB109:
	call	__cxa_rethrow@PLT
.LEHE109:
.L862:
.LEHB110:
	call	_ZSt17__throw_bad_allocv@PLT
.LEHE110:
.L866:
.LEHB111:
	call	_ZSt17__throw_bad_allocv@PLT
.LEHE111:
.L634:
.LEHB112:
	call	__cxa_rethrow@PLT
.LEHE112:
.L707:
	movq	%rax, %rbx
.L486:
	movq	192(%rsp), %rdi
	call	_ZdlPv@PLT
.L475:
	movq	%rbx, %rdi
	call	__cxa_begin_catch@PLT
	movq	184(%rsp), %rbx
.L490:
	cmpq	%rbx, 64(%rsp)
	je	.L489
	movzbl	(%rbx), %esi
	leaq	8(%rbx), %rdi
	addq	$16, %rbx
	call	_ZN8nlohmann10basic_jsonISt3mapSt6vectorNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEblmdSaNS_14adl_serializerEE10json_value7destroyENS_6detail7value_tE
	jmp	.L490
.L700:
	movq	%rax, %rbx
	call	__cxa_end_catch@PLT
	movq	152(%rsp), %rax
	movq	(%rax), %rdi
	testq	%rdi, %rdi
	je	.L500
	call	_ZdlPv@PLT
.L500:
	movq	152(%rsp), %rdi
	call	_ZdlPv@PLT
.L439:
	movq	%rbx, %rdi
	call	__cxa_begin_catch@PLT
	movq	160(%rsp), %rbx
.L504:
	cmpq	24(%rsp), %rbx
	je	.L503
	movzbl	(%rbx), %esi
	leaq	8(%rbx), %rdi
	addq	$16, %rbx
	call	_ZN8nlohmann10basic_jsonISt3mapSt6vectorNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEblmdSaNS_14adl_serializerEE10json_value7destroyENS_6detail7value_tE
	jmp	.L504
.L724:
	movq	%rax, %rbp
	call	__cxa_end_catch@PLT
	movq	144(%rsp), %rax
	movq	(%rax), %rdi
	testq	%rdi, %rdi
	je	.L638
	call	_ZdlPv@PLT
	jmp	.L638
.L489:
.LEHB113:
	call	__cxa_rethrow@PLT
.LEHE113:
.L503:
.LEHB114:
	call	__cxa_rethrow@PLT
.LEHE114:
.L704:
	movq	%rax, %rbx
	call	__cxa_end_catch@PLT
	movq	168(%rsp), %rax
	movq	(%rax), %rdi
	testq	%rdi, %rdi
	je	.L493
	call	_ZdlPv@PLT
	jmp	.L493
.L696:
	movq	%rax, %rbx
	call	__cxa_end_catch@PLT
	movq	136(%rsp), %rax
	movq	(%rax), %rdi
	testq	%rdi, %rdi
	je	.L507
	call	_ZdlPv@PLT
.L507:
	movq	136(%rsp), %rdi
	call	_ZdlPv@PLT
.L421:
	movq	%rbx, %rdi
	call	__cxa_begin_catch@PLT
	movq	128(%rsp), %rbx
.L511:
	cmpq	%r14, %rbx
	je	.L510
	movzbl	(%rbx), %esi
	leaq	8(%rbx), %rdi
	addq	$16, %rbx
	call	_ZN8nlohmann10basic_jsonISt3mapSt6vectorNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEblmdSaNS_14adl_serializerEE10json_value7destroyENS_6detail7value_tE
	jmp	.L511
.L698:
.L829:
	movq	%r12, %rdi
	movq	%rax, %rbx
	call	_ZdlPv@PLT
	jmp	.L439
.L734:
.L835:
	movq	%r13, %rdi
	movq	%rax, %rbp
	call	_ZdlPv@PLT
	jmp	.L606
.L738:
	jmp	.L835
.L690:
	jmp	.L832
.L689:
	movq	%rax, %rbx
	jmp	.L403
.L706:
.L825:
	movq	%r12, %rdi
	movq	%rax, %rbx
	call	_ZdlPv@PLT
	jmp	.L475
.L722:
	jmp	.L841
.L727:
	movq	%rax, %rbp
	jmp	.L631
.L852:
.LEHB115:
	call	_ZSt17__throw_bad_allocv@PLT
.LEHE115:
.L731:
	movq	%rax, %rbp
	jmp	.L624
.L726:
.L839:
	movq	%r12, %rdi
	movq	%rax, %rbp
	call	_ZdlPv@PLT
	jmp	.L570
.L740:
	jmp	.L839
.L725:
	movq	%rax, %rbp
	jmp	.L570
.L718:
	jmp	.L842
.L715:
	jmp	.L648
.L685:
	jmp	.L650
.L717:
	movq	%rax, %rbp
	jmp	.L534
.L699:
	movq	%rax, %rbx
	jmp	.L500
.L713:
.L831:
	movq	%r12, %rdi
	movq	%rax, %rbx
	call	_ZdlPv@PLT
	jmp	.L421
.L695:
	movq	%rax, %rbx
	jmp	.L507
.L858:
.LEHB116:
	call	_ZSt17__throw_bad_allocv@PLT
.LEHE116:
.L710:
	jmp	.L825
.L705:
	movq	%rax, %rbx
	jmp	.L475
.L694:
	jmp	.L831
.L693:
	movq	%rax, %rbx
	jmp	.L421
.L709:
	movq	%rax, %rdi
	call	__cxa_begin_catch@PLT
	movq	200(%rsp), %rbx
.L483:
	cmpq	%rbx, 104(%rsp)
	je	.L482
	movzbl	(%rbx), %esi
	leaq	8(%rbx), %rdi
	addq	$16, %rbx
	call	_ZN8nlohmann10basic_jsonISt3mapSt6vectorNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEblmdSaNS_14adl_serializerEE10json_value7destroyENS_6detail7value_tE
	jmp	.L483
.L702:
.L827:
	movq	%r12, %rdi
	movq	%rax, %rbx
	call	_ZdlPv@PLT
	jmp	.L457
.L482:
.LEHB117:
	call	__cxa_rethrow@PLT
.LEHE117:
.L697:
	movq	%rax, %rbx
	jmp	.L439
.L737:
	movq	%rax, %rdi
	call	__cxa_begin_catch@PLT
	movq	208(%rsp), %rbp
.L614:
	cmpq	%rbp, 112(%rsp)
	je	.L613
	movzbl	0(%rbp), %esi
	leaq	8(%rbp), %rdi
	addq	$16, %rbp
	call	_ZN8nlohmann10basic_jsonISt3mapSt6vectorNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEblmdSaNS_14adl_serializerEE10json_value7destroyENS_6detail7value_tE
	jmp	.L614
.L708:
	movq	%rax, %rbx
	call	__cxa_end_catch@PLT
	movq	192(%rsp), %rax
	movq	(%rax), %rdi
	testq	%rdi, %rdi
	je	.L486
	call	_ZdlPv@PLT
	jmp	.L486
.L613:
.LEHB118:
	call	__cxa_rethrow@PLT
.LEHE118:
.L711:
	jmp	.L827
.L712:
	jmp	.L829
.L510:
.LEHB119:
	call	__cxa_rethrow@PLT
.LEHE119:
.L701:
	movq	%rax, %rbx
	jmp	.L457
.L736:
	movq	%rax, %rbp
	call	__cxa_end_catch@PLT
	movq	192(%rsp), %rax
	movq	(%rax), %rdi
	testq	%rdi, %rdi
	je	.L617
	call	_ZdlPv@PLT
	jmp	.L617
.L692:
	movq	%rax, %rbx
	call	__cxa_end_catch@PLT
	movq	112(%rsp), %rax
	movq	(%rax), %rdi
	testq	%rdi, %rdi
	je	.L514
	call	_ZdlPv@PLT
	jmp	.L514
	.cfi_endproc
.LFE8822:
	.section	.gcc_except_table._ZNSt8_Rb_treeINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESt4pairIKS5_N8nlohmann10basic_jsonISt3mapSt6vectorS5_blmdSaNS8_14adl_serializerEEEESt10_Select1stISE_ESt4lessIvESaISE_EE7_M_copyINSK_11_Alloc_nodeEEEPSt13_Rb_tree_nodeISE_EPKSO_PSt18_Rb_tree_node_baseRT_,"aG",@progbits,_ZNSt8_Rb_treeINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESt4pairIKS5_N8nlohmann10basic_jsonISt3mapSt6vectorS5_blmdSaNS8_14adl_serializerEEEESt10_Select1stISE_ESt4lessIvESaISE_EE7_M_copyINSK_11_Alloc_nodeEEEPSt13_Rb_tree_nodeISE_EPKSO_PSt18_Rb_tree_node_baseRT_,comdat
	.align 4
.LLSDA8822:
	.byte	0xff
	.byte	0x9b
	.uleb128 .LLSDATT8822-.LLSDATTD8822
.LLSDATTD8822:
	.byte	0x1
	.uleb128 .LLSDACSE8822-.LLSDACSB8822
.LLSDACSB8822:
	.uleb128 .LEHB27-.LFB8822
	.uleb128 .LEHE27-.LEHB27
	.uleb128 0
	.uleb128 0
	.uleb128 .LEHB28-.LFB8822
	.uleb128 .LEHE28-.LEHB28
	.uleb128 .L687-.LFB8822
	.uleb128 0x1
	.uleb128 .LEHB29-.LFB8822
	.uleb128 .LEHE29-.LEHB29
	.uleb128 .L689-.LFB8822
	.uleb128 0x3
	.uleb128 .LEHB30-.LFB8822
	.uleb128 .LEHE30-.LEHB30
	.uleb128 .L714-.LFB8822
	.uleb128 0x3
	.uleb128 .LEHB31-.LFB8822
	.uleb128 .LEHE31-.LEHB31
	.uleb128 .L685-.LFB8822
	.uleb128 0x1
	.uleb128 .LEHB32-.LFB8822
	.uleb128 .LEHE32-.LEHB32
	.uleb128 .L715-.LFB8822
	.uleb128 0x1
	.uleb128 .LEHB33-.LFB8822
	.uleb128 .LEHE33-.LEHB33
	.uleb128 .L685-.LFB8822
	.uleb128 0x1
	.uleb128 .LEHB34-.LFB8822
	.uleb128 .LEHE34-.LEHB34
	.uleb128 .L717-.LFB8822
	.uleb128 0x3
	.uleb128 .LEHB35-.LFB8822
	.uleb128 .LEHE35-.LEHB35
	.uleb128 .L742-.LFB8822
	.uleb128 0x3
	.uleb128 .LEHB36-.LFB8822
	.uleb128 .LEHE36-.LEHB36
	.uleb128 .L717-.LFB8822
	.uleb128 0x3
	.uleb128 .LEHB37-.LFB8822
	.uleb128 .LEHE37-.LEHB37
	.uleb128 .L719-.LFB8822
	.uleb128 0x3
	.uleb128 .LEHB38-.LFB8822
	.uleb128 .LEHE38-.LEHB38
	.uleb128 .L717-.LFB8822
	.uleb128 0x3
	.uleb128 .LEHB39-.LFB8822
	.uleb128 .LEHE39-.LEHB39
	.uleb128 .L718-.LFB8822
	.uleb128 0x3
	.uleb128 .LEHB40-.LFB8822
	.uleb128 .LEHE40-.LEHB40
	.uleb128 .L721-.LFB8822
	.uleb128 0x1
	.uleb128 .LEHB41-.LFB8822
	.uleb128 .LEHE41-.LEHB41
	.uleb128 .L741-.LFB8822
	.uleb128 0x1
	.uleb128 .LEHB42-.LFB8822
	.uleb128 .LEHE42-.LEHB42
	.uleb128 .L721-.LFB8822
	.uleb128 0x1
	.uleb128 .LEHB43-.LFB8822
	.uleb128 .LEHE43-.LEHB43
	.uleb128 .L722-.LFB8822
	.uleb128 0x1
	.uleb128 .LEHB44-.LFB8822
	.uleb128 .LEHE44-.LEHB44
	.uleb128 .L721-.LFB8822
	.uleb128 0x1
	.uleb128 .LEHB45-.LFB8822
	.uleb128 .LEHE45-.LEHB45
	.uleb128 .L723-.LFB8822
	.uleb128 0x1
	.uleb128 .LEHB46-.LFB8822
	.uleb128 .LEHE46-.LEHB46
	.uleb128 .L685-.LFB8822
	.uleb128 0x1
	.uleb128 .LEHB47-.LFB8822
	.uleb128 .LEHE47-.LEHB47
	.uleb128 .L725-.LFB8822
	.uleb128 0x1
	.uleb128 .LEHB48-.LFB8822
	.uleb128 .LEHE48-.LEHB48
	.uleb128 .L740-.LFB8822
	.uleb128 0x1
	.uleb128 .LEHB49-.LFB8822
	.uleb128 .LEHE49-.LEHB49
	.uleb128 .L725-.LFB8822
	.uleb128 0x1
	.uleb128 .LEHB50-.LFB8822
	.uleb128 .LEHE50-.LEHB50
	.uleb128 .L726-.LFB8822
	.uleb128 0x1
	.uleb128 .LEHB51-.LFB8822
	.uleb128 .LEHE51-.LEHB51
	.uleb128 .L725-.LFB8822
	.uleb128 0x1
	.uleb128 .LEHB52-.LFB8822
	.uleb128 .LEHE52-.LEHB52
	.uleb128 .L727-.LFB8822
	.uleb128 0x1
	.uleb128 .LEHB53-.LFB8822
	.uleb128 .LEHE53-.LEHB53
	.uleb128 .L689-.LFB8822
	.uleb128 0x3
	.uleb128 .LEHB54-.LFB8822
	.uleb128 .LEHE54-.LEHB54
	.uleb128 .L690-.LFB8822
	.uleb128 0x3
	.uleb128 .LEHB55-.LFB8822
	.uleb128 .LEHE55-.LEHB55
	.uleb128 .L689-.LFB8822
	.uleb128 0x3
	.uleb128 .LEHB56-.LFB8822
	.uleb128 .LEHE56-.LEHB56
	.uleb128 .L691-.LFB8822
	.uleb128 0x3
	.uleb128 .LEHB57-.LFB8822
	.uleb128 .LEHE57-.LEHB57
	.uleb128 .L729-.LFB8822
	.uleb128 0x1
	.uleb128 .LEHB58-.LFB8822
	.uleb128 .LEHE58-.LEHB58
	.uleb128 .L739-.LFB8822
	.uleb128 0x1
	.uleb128 .LEHB59-.LFB8822
	.uleb128 .LEHE59-.LEHB59
	.uleb128 .L729-.LFB8822
	.uleb128 0x1
	.uleb128 .LEHB60-.LFB8822
	.uleb128 .LEHE60-.LEHB60
	.uleb128 .L730-.LFB8822
	.uleb128 0x1
	.uleb128 .LEHB61-.LFB8822
	.uleb128 .LEHE61-.LEHB61
	.uleb128 .L729-.LFB8822
	.uleb128 0x1
	.uleb128 .LEHB62-.LFB8822
	.uleb128 .LEHE62-.LEHB62
	.uleb128 .L731-.LFB8822
	.uleb128 0x1
	.uleb128 .LEHB63-.LFB8822
	.uleb128 .LEHE63-.LEHB63
	.uleb128 .L693-.LFB8822
	.uleb128 0x1
	.uleb128 .LEHB64-.LFB8822
	.uleb128 .LEHE64-.LEHB64
	.uleb128 .L694-.LFB8822
	.uleb128 0x1
	.uleb128 .LEHB65-.LFB8822
	.uleb128 .LEHE65-.LEHB65
	.uleb128 .L693-.LFB8822
	.uleb128 0x1
	.uleb128 .LEHB66-.LFB8822
	.uleb128 .LEHE66-.LEHB66
	.uleb128 .L713-.LFB8822
	.uleb128 0x1
	.uleb128 .LEHB67-.LFB8822
	.uleb128 .LEHE67-.LEHB67
	.uleb128 .L693-.LFB8822
	.uleb128 0x1
	.uleb128 .LEHB68-.LFB8822
	.uleb128 .LEHE68-.LEHB68
	.uleb128 .L695-.LFB8822
	.uleb128 0x1
	.uleb128 .LEHB69-.LFB8822
	.uleb128 .LEHE69-.LEHB69
	.uleb128 .L733-.LFB8822
	.uleb128 0x1
	.uleb128 .LEHB70-.LFB8822
	.uleb128 .LEHE70-.LEHB70
	.uleb128 .L738-.LFB8822
	.uleb128 0x1
	.uleb128 .LEHB71-.LFB8822
	.uleb128 .LEHE71-.LEHB71
	.uleb128 .L733-.LFB8822
	.uleb128 0x1
	.uleb128 .LEHB72-.LFB8822
	.uleb128 .LEHE72-.LEHB72
	.uleb128 .L734-.LFB8822
	.uleb128 0x1
	.uleb128 .LEHB73-.LFB8822
	.uleb128 .LEHE73-.LEHB73
	.uleb128 .L733-.LFB8822
	.uleb128 0x1
	.uleb128 .LEHB74-.LFB8822
	.uleb128 .LEHE74-.LEHB74
	.uleb128 .L735-.LFB8822
	.uleb128 0x1
	.uleb128 .LEHB75-.LFB8822
	.uleb128 .LEHE75-.LEHB75
	.uleb128 .L737-.LFB8822
	.uleb128 0x1
	.uleb128 .LEHB76-.LFB8822
	.uleb128 .LEHE76-.LEHB76
	.uleb128 .L697-.LFB8822
	.uleb128 0x1
	.uleb128 .LEHB77-.LFB8822
	.uleb128 .LEHE77-.LEHB77
	.uleb128 .L712-.LFB8822
	.uleb128 0x1
	.uleb128 .LEHB78-.LFB8822
	.uleb128 .LEHE78-.LEHB78
	.uleb128 .L697-.LFB8822
	.uleb128 0x1
	.uleb128 .LEHB79-.LFB8822
	.uleb128 .LEHE79-.LEHB79
	.uleb128 .L698-.LFB8822
	.uleb128 0x1
	.uleb128 .LEHB80-.LFB8822
	.uleb128 .LEHE80-.LEHB80
	.uleb128 .L697-.LFB8822
	.uleb128 0x1
	.uleb128 .LEHB81-.LFB8822
	.uleb128 .LEHE81-.LEHB81
	.uleb128 .L699-.LFB8822
	.uleb128 0x1
	.uleb128 .LEHB82-.LFB8822
	.uleb128 .LEHE82-.LEHB82
	.uleb128 .L701-.LFB8822
	.uleb128 0x1
	.uleb128 .LEHB83-.LFB8822
	.uleb128 .LEHE83-.LEHB83
	.uleb128 .L711-.LFB8822
	.uleb128 0x1
	.uleb128 .LEHB84-.LFB8822
	.uleb128 .LEHE84-.LEHB84
	.uleb128 .L701-.LFB8822
	.uleb128 0x1
	.uleb128 .LEHB85-.LFB8822
	.uleb128 .LEHE85-.LEHB85
	.uleb128 .L703-.LFB8822
	.uleb128 0x1
	.uleb128 .LEHB86-.LFB8822
	.uleb128 .LEHE86-.LEHB86
	.uleb128 .L701-.LFB8822
	.uleb128 0x1
	.uleb128 .LEHB87-.LFB8822
	.uleb128 .LEHE87-.LEHB87
	.uleb128 .L702-.LFB8822
	.uleb128 0x1
	.uleb128 .LEHB88-.LFB8822
	.uleb128 .LEHE88-.LEHB88
	.uleb128 .L705-.LFB8822
	.uleb128 0x1
	.uleb128 .LEHB89-.LFB8822
	.uleb128 .LEHE89-.LEHB89
	.uleb128 .L710-.LFB8822
	.uleb128 0x1
	.uleb128 .LEHB90-.LFB8822
	.uleb128 .LEHE90-.LEHB90
	.uleb128 .L705-.LFB8822
	.uleb128 0x1
	.uleb128 .LEHB91-.LFB8822
	.uleb128 .LEHE91-.LEHB91
	.uleb128 .L706-.LFB8822
	.uleb128 0x1
	.uleb128 .LEHB92-.LFB8822
	.uleb128 .LEHE92-.LEHB92
	.uleb128 .L705-.LFB8822
	.uleb128 0x1
	.uleb128 .LEHB93-.LFB8822
	.uleb128 .LEHE93-.LEHB93
	.uleb128 .L707-.LFB8822
	.uleb128 0x1
	.uleb128 .LEHB94-.LFB8822
	.uleb128 .LEHE94-.LEHB94
	.uleb128 .L709-.LFB8822
	.uleb128 0x1
	.uleb128 .LEHB95-.LFB8822
	.uleb128 .LEHE95-.LEHB95
	.uleb128 .L688-.LFB8822
	.uleb128 0
	.uleb128 .LEHB96-.LFB8822
	.uleb128 .LEHE96-.LEHB96
	.uleb128 0
	.uleb128 0
	.uleb128 .LEHB97-.LFB8822
	.uleb128 .LEHE97-.LEHB97
	.uleb128 .L728-.LFB8822
	.uleb128 0x1
	.uleb128 .LEHB98-.LFB8822
	.uleb128 .LEHE98-.LEHB98
	.uleb128 .L691-.LFB8822
	.uleb128 0x3
	.uleb128 .LEHB99-.LFB8822
	.uleb128 .LEHE99-.LEHB99
	.uleb128 .L731-.LFB8822
	.uleb128 0x1
	.uleb128 .LEHB100-.LFB8822
	.uleb128 .LEHE100-.LEHB100
	.uleb128 .L703-.LFB8822
	.uleb128 0x1
	.uleb128 .LEHB101-.LFB8822
	.uleb128 .LEHE101-.LEHB101
	.uleb128 .L716-.LFB8822
	.uleb128 0x3
	.uleb128 .LEHB102-.LFB8822
	.uleb128 .LEHE102-.LEHB102
	.uleb128 .L686-.LFB8822
	.uleb128 0
	.uleb128 .LEHB103-.LFB8822
	.uleb128 .LEHE103-.LEHB103
	.uleb128 0
	.uleb128 0
	.uleb128 .LEHB104-.LFB8822
	.uleb128 .LEHE104-.LEHB104
	.uleb128 .L720-.LFB8822
	.uleb128 0x3
	.uleb128 .LEHB105-.LFB8822
	.uleb128 .LEHE105-.LEHB105
	.uleb128 .L719-.LFB8822
	.uleb128 0x3
	.uleb128 .LEHB106-.LFB8822
	.uleb128 .LEHE106-.LEHB106
	.uleb128 .L723-.LFB8822
	.uleb128 0x1
	.uleb128 .LEHB107-.LFB8822
	.uleb128 .LEHE107-.LEHB107
	.uleb128 .L732-.LFB8822
	.uleb128 0x1
	.uleb128 .LEHB108-.LFB8822
	.uleb128 .LEHE108-.LEHB108
	.uleb128 .L735-.LFB8822
	.uleb128 0x1
	.uleb128 .LEHB109-.LFB8822
	.uleb128 .LEHE109-.LEHB109
	.uleb128 .L700-.LFB8822
	.uleb128 0x1
	.uleb128 .LEHB110-.LFB8822
	.uleb128 .LEHE110-.LEHB110
	.uleb128 .L699-.LFB8822
	.uleb128 0x1
	.uleb128 .LEHB111-.LFB8822
	.uleb128 .LEHE111-.LEHB111
	.uleb128 .L707-.LFB8822
	.uleb128 0x1
	.uleb128 .LEHB112-.LFB8822
	.uleb128 .LEHE112-.LEHB112
	.uleb128 .L724-.LFB8822
	.uleb128 0x1
	.uleb128 .LEHB113-.LFB8822
	.uleb128 .LEHE113-.LEHB113
	.uleb128 .L704-.LFB8822
	.uleb128 0x1
	.uleb128 .LEHB114-.LFB8822
	.uleb128 .LEHE114-.LEHB114
	.uleb128 .L696-.LFB8822
	.uleb128 0x1
	.uleb128 .LEHB115-.LFB8822
	.uleb128 .LEHE115-.LEHB115
	.uleb128 .L727-.LFB8822
	.uleb128 0x1
	.uleb128 .LEHB116-.LFB8822
	.uleb128 .LEHE116-.LEHB116
	.uleb128 .L695-.LFB8822
	.uleb128 0x1
	.uleb128 .LEHB117-.LFB8822
	.uleb128 .LEHE117-.LEHB117
	.uleb128 .L708-.LFB8822
	.uleb128 0x1
	.uleb128 .LEHB118-.LFB8822
	.uleb128 .LEHE118-.LEHB118
	.uleb128 .L736-.LFB8822
	.uleb128 0x1
	.uleb128 .LEHB119-.LFB8822
	.uleb128 .LEHE119-.LEHB119
	.uleb128 .L692-.LFB8822
	.uleb128 0x3
.LLSDACSE8822:
	.byte	0x1
	.byte	0
	.byte	0
	.byte	0x7d
	.align 4
	.long	0

.LLSDATT8822:
	.section	.text._ZNSt8_Rb_treeINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESt4pairIKS5_N8nlohmann10basic_jsonISt3mapSt6vectorS5_blmdSaNS8_14adl_serializerEEEESt10_Select1stISE_ESt4lessIvESaISE_EE7_M_copyINSK_11_Alloc_nodeEEEPSt13_Rb_tree_nodeISE_EPKSO_PSt18_Rb_tree_node_baseRT_,"axG",@progbits,_ZNSt8_Rb_treeINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESt4pairIKS5_N8nlohmann10basic_jsonISt3mapSt6vectorS5_blmdSaNS8_14adl_serializerEEEESt10_Select1stISE_ESt4lessIvESaISE_EE7_M_copyINSK_11_Alloc_nodeEEEPSt13_Rb_tree_nodeISE_EPKSO_PSt18_Rb_tree_node_baseRT_,comdat
	.size	_ZNSt8_Rb_treeINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESt4pairIKS5_N8nlohmann10basic_jsonISt3mapSt6vectorS5_blmdSaNS8_14adl_serializerEEEESt10_Select1stISE_ESt4lessIvESaISE_EE7_M_copyINSK_11_Alloc_nodeEEEPSt13_Rb_tree_nodeISE_EPKSO_PSt18_Rb_tree_node_baseRT_, .-_ZNSt8_Rb_treeINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESt4pairIKS5_N8nlohmann10basic_jsonISt3mapSt6vectorS5_blmdSaNS8_14adl_serializerEEEESt10_Select1stISE_ESt4lessIvESaISE_EE7_M_copyINSK_11_Alloc_nodeEEEPSt13_Rb_tree_nodeISE_EPKSO_PSt18_Rb_tree_node_baseRT_
	.section	.text._ZN8nlohmann10basic_jsonISt3mapSt6vectorNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEblmdSaNS_14adl_serializerEEC2ERKSA_,"axG",@progbits,_ZN8nlohmann10basic_jsonISt3mapSt6vectorNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEblmdSaNS_14adl_serializerEEC5ERKSA_,comdat
	.align 2
	.p2align 4,,15
	.weak	_ZN8nlohmann10basic_jsonISt3mapSt6vectorNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEblmdSaNS_14adl_serializerEEC2ERKSA_
	.type	_ZN8nlohmann10basic_jsonISt3mapSt6vectorNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEblmdSaNS_14adl_serializerEEC2ERKSA_, @function
_ZN8nlohmann10basic_jsonISt3mapSt6vectorNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEblmdSaNS_14adl_serializerEEC2ERKSA_:
.LFB8073:
	.cfi_startproc
	.cfi_personality 0x9b,DW.ref.__gxx_personality_v0
	.cfi_lsda 0x1b,.LLSDA8073
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
	pushq	%rbp
	.cfi_def_cfa_offset 48
	.cfi_offset 6, -48
	pushq	%rbx
	.cfi_def_cfa_offset 56
	.cfi_offset 3, -56
	subq	$88, %rsp
	.cfi_def_cfa_offset 144
	movq	%fs:40, %rax
	movq	%rax, 72(%rsp)
	xorl	%eax, %eax
	movzbl	(%rsi), %eax
	movq	$0, 8(%rdi)
	cmpb	$7, %al
	movb	%al, (%rdi)
	ja	.L867
	leaq	.L870(%rip), %rdx
	movq	%rdi, %rbp
	movslq	(%rdx,%rax,4), %rax
	addq	%rdx, %rax
	jmp	*%rax
	.section	.rodata._ZN8nlohmann10basic_jsonISt3mapSt6vectorNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEblmdSaNS_14adl_serializerEEC2ERKSA_,"aG",@progbits,_ZN8nlohmann10basic_jsonISt3mapSt6vectorNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEblmdSaNS_14adl_serializerEEC5ERKSA_,comdat
	.align 4
	.align 4
.L870:
	.long	.L867-.L870
	.long	.L869-.L870
	.long	.L871-.L870
	.long	.L872-.L870
	.long	.L873-.L870
	.long	.L875-.L870
	.long	.L875-.L870
	.long	.L876-.L870
	.section	.text._ZN8nlohmann10basic_jsonISt3mapSt6vectorNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEblmdSaNS_14adl_serializerEEC2ERKSA_,"axG",@progbits,_ZN8nlohmann10basic_jsonISt3mapSt6vectorNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEblmdSaNS_14adl_serializerEEC5ERKSA_,comdat
	.p2align 4,,10
	.p2align 3
.L892:
	movq	8(%rbx), %rax
	movq	%rax, 8(%r12)
.L885:
	addq	$16, %r12
	addq	$16, %rbx
	cmpq	%rbx, 8(%rsp)
	jne	.L911
.L884:
	movq	24(%rsp), %rax
	movq	%r12, 8(%rax)
	movq	%rax, 8(%rbp)
	.p2align 4,,10
	.p2align 3
.L867:
	movq	72(%rsp), %rax
	xorq	%fs:40, %rax
	jne	.L954
	addq	$88, %rsp
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
	ret
	.p2align 4,,10
	.p2align 3
.L875:
	.cfi_restore_state
	movq	8(%rsi), %rax
	movq	%rax, 8(%rdi)
	jmp	.L867
	.p2align 4,,10
	.p2align 3
.L876:
	movsd	8(%rsi), %xmm0
	movsd	%xmm0, 8(%rdi)
	jmp	.L867
	.p2align 4,,10
	.p2align 3
.L869:
	movq	8(%rsi), %r12
	movl	$48, %edi
.LEHB120:
	call	_Znwm@PLT
.LEHE120:
	leaq	8(%rax), %rdx
	movq	$0, 16(%rax)
	movq	%rax, %rbx
	movl	$0, 8(%rax)
	movq	$0, 40(%rax)
	movq	%rdx, 24(%rax)
	movq	%rdx, 32(%rax)
	movq	16(%r12), %rsi
	testq	%rsi, %rsi
	je	.L952
	leaq	64(%rsp), %rcx
	movq	%rax, %rdi
	movq	%rax, 64(%rsp)
.LEHB121:
	call	_ZNSt8_Rb_treeINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESt4pairIKS5_N8nlohmann10basic_jsonISt3mapSt6vectorS5_blmdSaNS8_14adl_serializerEEEESt10_Select1stISE_ESt4lessIvESaISE_EE7_M_copyINSK_11_Alloc_nodeEEEPSt13_Rb_tree_nodeISE_EPKSO_PSt18_Rb_tree_node_baseRT_
.LEHE121:
	movq	%rax, %rcx
	jmp	.L878
	.p2align 4,,10
	.p2align 3
.L919:
	movq	%rdx, %rcx
.L878:
	movq	16(%rcx), %rdx
	testq	%rdx, %rdx
	jne	.L919
	movq	%rcx, 24(%rbx)
	movq	%rax, %rcx
	jmp	.L879
	.p2align 4,,10
	.p2align 3
.L920:
	movq	%rdx, %rcx
.L879:
	movq	24(%rcx), %rdx
	testq	%rdx, %rdx
	jne	.L920
	movq	40(%r12), %rdx
	movq	%rcx, 32(%rbx)
	movq	%rax, 16(%rbx)
	movq	%rdx, 40(%rbx)
	jmp	.L952
	.p2align 4,,10
	.p2align 3
.L871:
	movq	8(%rsi), %r12
	movl	$24, %edi
.LEHB122:
	call	_Znwm@PLT
.LEHE122:
	movq	%rax, %rdi
	movq	%rax, 24(%rsp)
	movq	8(%r12), %rbx
	subq	(%r12), %rbx
	movq	$0, (%rdi)
	movq	$0, 8(%rdi)
	movq	$0, 16(%rdi)
	movq	%rbx, %rax
	sarq	$4, %rax
	testq	%rax, %rax
	je	.L955
	movabsq	$1152921504606846975, %rdx
	cmpq	%rdx, %rax
	ja	.L956
	movq	%rbx, %rdi
.LEHB123:
	call	_Znwm@PLT
.LEHE123:
	movq	%rax, 32(%rsp)
.L882:
	movq	32(%rsp), %rax
	movq	24(%rsp), %rdi
	addq	%rax, %rbx
	movq	%rax, (%rdi)
	movq	%rax, 8(%rdi)
	movq	%rbx, 16(%rdi)
	movq	8(%r12), %rsi
	movq	(%r12), %rbx
	movq	%rsi, 8(%rsp)
	cmpq	%rbx, %rsi
	je	.L921
	movq	%rax, %r12
	leaq	64(%rsp), %rax
	leaq	.L887(%rip), %r15
	movq	%rax, 56(%rsp)
	.p2align 4,,10
	.p2align 3
.L911:
	movzbl	(%rbx), %eax
	movq	$0, 8(%r12)
	cmpb	$7, %al
	movb	%al, (%r12)
	ja	.L885
	movslq	(%r15,%rax,4), %rax
	addq	%r15, %rax
	jmp	*%rax
	.section	.rodata._ZN8nlohmann10basic_jsonISt3mapSt6vectorNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEblmdSaNS_14adl_serializerEEC2ERKSA_,"aG",@progbits,_ZN8nlohmann10basic_jsonISt3mapSt6vectorNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEblmdSaNS_14adl_serializerEEC5ERKSA_,comdat
	.align 4
	.align 4
.L887:
	.long	.L885-.L887
	.long	.L886-.L887
	.long	.L888-.L887
	.long	.L889-.L887
	.long	.L890-.L887
	.long	.L892-.L887
	.long	.L892-.L887
	.long	.L893-.L887
	.section	.text._ZN8nlohmann10basic_jsonISt3mapSt6vectorNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEblmdSaNS_14adl_serializerEEC2ERKSA_,"axG",@progbits,_ZN8nlohmann10basic_jsonISt3mapSt6vectorNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEblmdSaNS_14adl_serializerEEC5ERKSA_,comdat
	.p2align 4,,10
	.p2align 3
.L872:
	movq	8(%rsi), %r12
	movl	$32, %edi
.LEHB124:
	call	_Znwm@PLT
.LEHE124:
	movq	%rax, %rbx
	leaq	16(%rax), %rax
	movq	%rbx, %rdi
	movq	%rax, (%rbx)
	movq	(%r12), %rsi
	movq	8(%r12), %rdx
	addq	%rsi, %rdx
.LEHB125:
	call	_ZNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEE12_M_constructIPcEEvT_S7_St20forward_iterator_tag.isra.67
.LEHE125:
.L952:
	movq	%rbx, 8(%rbp)
	jmp	.L867
	.p2align 4,,10
	.p2align 3
.L873:
	movzbl	8(%rsi), %eax
	movb	%al, 8(%rdi)
	jmp	.L867
	.p2align 4,,10
	.p2align 3
.L890:
	movzbl	8(%rbx), %eax
	movb	%al, 8(%r12)
	jmp	.L885
	.p2align 4,,10
	.p2align 3
.L889:
	movl	$32, %edi
	movq	8(%rbx), %r14
.LEHB126:
	call	_Znwm@PLT
.LEHE126:
	movq	%rax, %r13
	leaq	16(%rax), %rax
	movq	%r13, %rdi
	movq	%rax, 0(%r13)
	movq	(%r14), %rsi
	movq	8(%r14), %rdx
	addq	%rsi, %rdx
.LEHB127:
	call	_ZNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEE12_M_constructIPcEEvT_S7_St20forward_iterator_tag.isra.67
.LEHE127:
.L950:
	movq	%r13, 8(%r12)
	jmp	.L885
	.p2align 4,,10
	.p2align 3
.L893:
	movsd	8(%rbx), %xmm0
	movsd	%xmm0, 8(%r12)
	jmp	.L885
	.p2align 4,,10
	.p2align 3
.L888:
	movl	$24, %edi
	movq	8(%rbx), %r13
.LEHB128:
	call	_Znwm@PLT
.LEHE128:
	movq	8(%r13), %r14
	subq	0(%r13), %r14
	movq	%rax, %rcx
	movq	%rax, 40(%rsp)
	movq	$0, (%rcx)
	movq	$0, 8(%rcx)
	movq	$0, 16(%rcx)
	movq	%r14, %rax
	sarq	$4, %rax
	testq	%rax, %rax
	je	.L957
	movabsq	$1152921504606846975, %rdi
	cmpq	%rdi, %rax
	ja	.L958
	movq	%r14, %rdi
.LEHB129:
	call	_Znwm@PLT
.LEHE129:
	movq	%rax, 48(%rsp)
.L900:
	movq	48(%rsp), %rax
	movq	40(%rsp), %rcx
	addq	%rax, %r14
	movq	%rax, (%rcx)
	movq	%rax, 8(%rcx)
	movq	%r14, 16(%rcx)
	movq	8(%r13), %rdi
	movq	0(%r13), %r13
	movq	%rdi, 16(%rsp)
	cmpq	%r13, %rdi
	je	.L924
	movq	%rax, %r14
	.p2align 4,,10
	.p2align 3
.L903:
	movq	%r13, %rsi
	movq	%r14, %rdi
.LEHB130:
	call	_ZN8nlohmann10basic_jsonISt3mapSt6vectorNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEblmdSaNS_14adl_serializerEEC1ERKSA_
.LEHE130:
	addq	$16, %r14
	addq	$16, %r13
	cmpq	%r13, 16(%rsp)
	jne	.L903
.L902:
	movq	40(%rsp), %rax
	movq	%r14, 8(%rax)
	movq	%rax, 8(%r12)
	jmp	.L885
	.p2align 4,,10
	.p2align 3
.L886:
	movl	$48, %edi
	movq	8(%rbx), %r14
.LEHB131:
	call	_Znwm@PLT
.LEHE131:
	leaq	8(%rax), %rdx
	movq	$0, 16(%rax)
	movq	%rax, %r13
	movl	$0, 8(%rax)
	movq	$0, 40(%rax)
	movq	%rdx, 24(%rax)
	movq	%rdx, 32(%rax)
	movq	16(%r14), %rsi
	testq	%rsi, %rsi
	je	.L950
	movq	56(%rsp), %rcx
	movq	%rax, %rdi
	movq	%rax, 64(%rsp)
.LEHB132:
	call	_ZNSt8_Rb_treeINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESt4pairIKS5_N8nlohmann10basic_jsonISt3mapSt6vectorS5_blmdSaNS8_14adl_serializerEEEESt10_Select1stISE_ESt4lessIvESaISE_EE7_M_copyINSK_11_Alloc_nodeEEEPSt13_Rb_tree_nodeISE_EPKSO_PSt18_Rb_tree_node_baseRT_
.LEHE132:
	movq	%rax, %rcx
	jmp	.L895
	.p2align 4,,10
	.p2align 3
.L922:
	movq	%rdx, %rcx
.L895:
	movq	16(%rcx), %rdx
	testq	%rdx, %rdx
	jne	.L922
	movq	%rcx, 24(%r13)
	movq	%rax, %rcx
	jmp	.L896
	.p2align 4,,10
	.p2align 3
.L923:
	movq	%rdx, %rcx
.L896:
	movq	24(%rcx), %rdx
	testq	%rdx, %rdx
	jne	.L923
	movq	40(%r14), %rdx
	movq	%rcx, 32(%r13)
	movq	%rax, 16(%r13)
	movq	%rdx, 40(%r13)
	jmp	.L950
	.p2align 4,,10
	.p2align 3
.L921:
	movq	32(%rsp), %r12
	jmp	.L884
	.p2align 4,,10
	.p2align 3
.L924:
	movq	48(%rsp), %r14
	jmp	.L902
	.p2align 4,,10
	.p2align 3
.L955:
	movq	$0, 32(%rsp)
	jmp	.L882
.L957:
	movq	$0, 48(%rsp)
	jmp	.L900
.L954:
	call	__stack_chk_fail@PLT
.L928:
	movq	%rax, %rbx
.L898:
	movq	%rbx, %rdi
	call	__cxa_begin_catch@PLT
	movq	32(%rsp), %rbx
.L913:
	cmpq	%rbx, %r12
	je	.L912
	movzbl	(%rbx), %esi
	leaq	8(%rbx), %rdi
	addq	$16, %rbx
	call	_ZN8nlohmann10basic_jsonISt3mapSt6vectorNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEblmdSaNS_14adl_serializerEE10json_value7destroyENS_6detail7value_tE
	jmp	.L913
.L934:
.L953:
	movq	%rax, %rbp
	movq	%rbx, %rdi
	call	_ZdlPv@PLT
	movq	%rbp, %rdi
.LEHB133:
	call	_Unwind_Resume@PLT
.LEHE133:
.L912:
.LEHB134:
	call	__cxa_rethrow@PLT
.LEHE134:
.L933:
.L951:
	movq	%r13, %rdi
	movq	%rax, %rbx
	call	_ZdlPv@PLT
	jmp	.L898
.L926:
	movq	%rax, %rbx
.L916:
	movq	24(%rsp), %rdi
	call	_ZdlPv@PLT
	movq	%rbx, %rdi
.LEHB135:
	call	_Unwind_Resume@PLT
.LEHE135:
.L956:
.LEHB136:
	call	_ZSt17__throw_bad_allocv@PLT
.LEHE136:
.L930:
	movq	%rax, %rbx
.L909:
	movq	40(%rsp), %rdi
	call	_ZdlPv@PLT
	jmp	.L898
.L958:
.LEHB137:
	call	_ZSt17__throw_bad_allocv@PLT
.LEHE137:
.L932:
	movq	%rax, %rdi
	call	__cxa_begin_catch@PLT
	movq	48(%rsp), %rbx
.L906:
	cmpq	%rbx, %r14
	je	.L905
	movzbl	(%rbx), %esi
	leaq	8(%rbx), %rdi
	addq	$16, %rbx
	call	_ZN8nlohmann10basic_jsonISt3mapSt6vectorNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEblmdSaNS_14adl_serializerEE10json_value7destroyENS_6detail7value_tE
	jmp	.L906
.L925:
	jmp	.L953
.L905:
.LEHB138:
	call	__cxa_rethrow@PLT
.LEHE138:
.L929:
	jmp	.L951
.L927:
	movq	%rax, %rbx
	call	__cxa_end_catch@PLT
	movq	24(%rsp), %rax
	movq	(%rax), %rdi
	testq	%rdi, %rdi
	je	.L916
	call	_ZdlPv@PLT
	jmp	.L916
.L931:
	movq	%rax, %rbx
	call	__cxa_end_catch@PLT
	movq	40(%rsp), %rax
	movq	(%rax), %rdi
	testq	%rdi, %rdi
	je	.L909
	call	_ZdlPv@PLT
	jmp	.L909
	.cfi_endproc
.LFE8073:
	.section	.gcc_except_table._ZN8nlohmann10basic_jsonISt3mapSt6vectorNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEblmdSaNS_14adl_serializerEEC2ERKSA_,"aG",@progbits,_ZN8nlohmann10basic_jsonISt3mapSt6vectorNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEblmdSaNS_14adl_serializerEEC5ERKSA_,comdat
	.align 4
.LLSDA8073:
	.byte	0xff
	.byte	0x9b
	.uleb128 .LLSDATT8073-.LLSDATTD8073
.LLSDATTD8073:
	.byte	0x1
	.uleb128 .LLSDACSE8073-.LLSDACSB8073
.LLSDACSB8073:
	.uleb128 .LEHB120-.LFB8073
	.uleb128 .LEHE120-.LEHB120
	.uleb128 0
	.uleb128 0
	.uleb128 .LEHB121-.LFB8073
	.uleb128 .LEHE121-.LEHB121
	.uleb128 .L925-.LFB8073
	.uleb128 0
	.uleb128 .LEHB122-.LFB8073
	.uleb128 .LEHE122-.LEHB122
	.uleb128 0
	.uleb128 0
	.uleb128 .LEHB123-.LFB8073
	.uleb128 .LEHE123-.LEHB123
	.uleb128 .L926-.LFB8073
	.uleb128 0
	.uleb128 .LEHB124-.LFB8073
	.uleb128 .LEHE124-.LEHB124
	.uleb128 0
	.uleb128 0
	.uleb128 .LEHB125-.LFB8073
	.uleb128 .LEHE125-.LEHB125
	.uleb128 .L934-.LFB8073
	.uleb128 0
	.uleb128 .LEHB126-.LFB8073
	.uleb128 .LEHE126-.LEHB126
	.uleb128 .L928-.LFB8073
	.uleb128 0x1
	.uleb128 .LEHB127-.LFB8073
	.uleb128 .LEHE127-.LEHB127
	.uleb128 .L933-.LFB8073
	.uleb128 0x1
	.uleb128 .LEHB128-.LFB8073
	.uleb128 .LEHE128-.LEHB128
	.uleb128 .L928-.LFB8073
	.uleb128 0x1
	.uleb128 .LEHB129-.LFB8073
	.uleb128 .LEHE129-.LEHB129
	.uleb128 .L930-.LFB8073
	.uleb128 0x1
	.uleb128 .LEHB130-.LFB8073
	.uleb128 .LEHE130-.LEHB130
	.uleb128 .L932-.LFB8073
	.uleb128 0x1
	.uleb128 .LEHB131-.LFB8073
	.uleb128 .LEHE131-.LEHB131
	.uleb128 .L928-.LFB8073
	.uleb128 0x1
	.uleb128 .LEHB132-.LFB8073
	.uleb128 .LEHE132-.LEHB132
	.uleb128 .L929-.LFB8073
	.uleb128 0x1
	.uleb128 .LEHB133-.LFB8073
	.uleb128 .LEHE133-.LEHB133
	.uleb128 0
	.uleb128 0
	.uleb128 .LEHB134-.LFB8073
	.uleb128 .LEHE134-.LEHB134
	.uleb128 .L927-.LFB8073
	.uleb128 0
	.uleb128 .LEHB135-.LFB8073
	.uleb128 .LEHE135-.LEHB135
	.uleb128 0
	.uleb128 0
	.uleb128 .LEHB136-.LFB8073
	.uleb128 .LEHE136-.LEHB136
	.uleb128 .L926-.LFB8073
	.uleb128 0
	.uleb128 .LEHB137-.LFB8073
	.uleb128 .LEHE137-.LEHB137
	.uleb128 .L930-.LFB8073
	.uleb128 0x1
	.uleb128 .LEHB138-.LFB8073
	.uleb128 .LEHE138-.LEHB138
	.uleb128 .L931-.LFB8073
	.uleb128 0x1
.LLSDACSE8073:
	.byte	0x1
	.byte	0
	.align 4
	.long	0

.LLSDATT8073:
	.section	.text._ZN8nlohmann10basic_jsonISt3mapSt6vectorNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEblmdSaNS_14adl_serializerEEC2ERKSA_,"axG",@progbits,_ZN8nlohmann10basic_jsonISt3mapSt6vectorNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEblmdSaNS_14adl_serializerEEC5ERKSA_,comdat
	.size	_ZN8nlohmann10basic_jsonISt3mapSt6vectorNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEblmdSaNS_14adl_serializerEEC2ERKSA_, .-_ZN8nlohmann10basic_jsonISt3mapSt6vectorNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEblmdSaNS_14adl_serializerEEC2ERKSA_
	.weak	_ZN8nlohmann10basic_jsonISt3mapSt6vectorNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEblmdSaNS_14adl_serializerEEC1ERKSA_
	.set	_ZN8nlohmann10basic_jsonISt3mapSt6vectorNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEblmdSaNS_14adl_serializerEEC1ERKSA_,_ZN8nlohmann10basic_jsonISt3mapSt6vectorNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEblmdSaNS_14adl_serializerEEC2ERKSA_
	.section	.rodata._ZNSt6vectorIN8nlohmann10basic_jsonISt3mapS_NSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEblmdSaNS0_14adl_serializerEEESaISA_EE14_M_fill_insertEN9__gnu_cxx17__normal_iteratorIPSA_SC_EEmRKSA_.str1.1,"aMS",@progbits,1
.LC19:
	.string	"vector::_M_fill_insert"
	.section	.text._ZNSt6vectorIN8nlohmann10basic_jsonISt3mapS_NSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEblmdSaNS0_14adl_serializerEEESaISA_EE14_M_fill_insertEN9__gnu_cxx17__normal_iteratorIPSA_SC_EEmRKSA_,"axG",@progbits,_ZNSt6vectorIN8nlohmann10basic_jsonISt3mapS_NSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEblmdSaNS0_14adl_serializerEEESaISA_EE14_M_fill_insertEN9__gnu_cxx17__normal_iteratorIPSA_SC_EEmRKSA_,comdat
	.align 2
	.p2align 4,,15
	.weak	_ZNSt6vectorIN8nlohmann10basic_jsonISt3mapS_NSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEblmdSaNS0_14adl_serializerEEESaISA_EE14_M_fill_insertEN9__gnu_cxx17__normal_iteratorIPSA_SC_EEmRKSA_
	.type	_ZNSt6vectorIN8nlohmann10basic_jsonISt3mapS_NSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEblmdSaNS0_14adl_serializerEEESaISA_EE14_M_fill_insertEN9__gnu_cxx17__normal_iteratorIPSA_SC_EEmRKSA_, @function
_ZNSt6vectorIN8nlohmann10basic_jsonISt3mapS_NSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEblmdSaNS0_14adl_serializerEEESaISA_EE14_M_fill_insertEN9__gnu_cxx17__normal_iteratorIPSA_SC_EEmRKSA_:
.LFB7327:
	.cfi_startproc
	.cfi_personality 0x9b,DW.ref.__gxx_personality_v0
	.cfi_lsda 0x1b,.LLSDA7327
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
	pushq	%rbp
	.cfi_def_cfa_offset 48
	.cfi_offset 6, -48
	pushq	%rbx
	.cfi_def_cfa_offset 56
	.cfi_offset 3, -56
	subq	$408, %rsp
	.cfi_def_cfa_offset 464
	movq	%fs:40, %rax
	movq	%rax, 392(%rsp)
	xorl	%eax, %eax
	testq	%rdx, %rdx
	movq	%rdi, 16(%rsp)
	movq	%rsi, 8(%rsp)
	movq	%rdx, 40(%rsp)
	je	.L959
	movq	16(%rdi), %rax
	movq	8(%rdi), %r12
	movq	%rcx, %rbp
	movq	%rsi, %rbx
	movq	%rax, (%rsp)
	subq	%r12, %rax
	sarq	$4, %rax
	cmpq	%rdx, %rax
	jb	.L961
	movzbl	0(%rbp), %eax
	movq	%rdi, 368(%rsp)
	movq	$0, 384(%rsp)
	cmpb	$7, %al
	movb	%al, 376(%rsp)
	ja	.L962
	leaq	.L964(%rip), %rdx
	movslq	(%rdx,%rax,4), %rax
	addq	%rdx, %rax
	jmp	*%rax
	.section	.rodata._ZNSt6vectorIN8nlohmann10basic_jsonISt3mapS_NSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEblmdSaNS0_14adl_serializerEEESaISA_EE14_M_fill_insertEN9__gnu_cxx17__normal_iteratorIPSA_SC_EEmRKSA_,"aG",@progbits,_ZNSt6vectorIN8nlohmann10basic_jsonISt3mapS_NSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEblmdSaNS0_14adl_serializerEEESaISA_EE14_M_fill_insertEN9__gnu_cxx17__normal_iteratorIPSA_SC_EEmRKSA_,comdat
	.align 4
	.align 4
.L964:
	.long	.L962-.L964
	.long	.L963-.L964
	.long	.L965-.L964
	.long	.L966-.L964
	.long	.L967-.L964
	.long	.L969-.L964
	.long	.L969-.L964
	.long	.L970-.L964
	.section	.text._ZNSt6vectorIN8nlohmann10basic_jsonISt3mapS_NSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEblmdSaNS0_14adl_serializerEEESaISA_EE14_M_fill_insertEN9__gnu_cxx17__normal_iteratorIPSA_SC_EEmRKSA_,"axG",@progbits,_ZNSt6vectorIN8nlohmann10basic_jsonISt3mapS_NSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEblmdSaNS0_14adl_serializerEEESaISA_EE14_M_fill_insertEN9__gnu_cxx17__normal_iteratorIPSA_SC_EEmRKSA_,comdat
	.p2align 4,,10
	.p2align 3
.L1229:
	movq	384(%rsp), %rax
	movq	%rax, 360(%rsp)
.L1222:
	movzbl	(%rbx), %eax
	movq	8(%rbx), %r14
	movq	360(%rsp), %rdx
	movb	%bpl, (%rbx)
	movq	%r14, 360(%rsp)
	cmpb	$2, %al
	movq	%rdx, 8(%rbx)
	je	.L1298
	cmpb	$3, %al
	je	.L1299
	cmpb	$1, %al
	jne	.L1297
	movq	16(%r14), %rsi
	movq	%r14, %rdi
	call	_ZNSt8_Rb_treeINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESt4pairIKS5_N8nlohmann10basic_jsonISt3mapSt6vectorS5_blmdSaNS8_14adl_serializerEEEESt10_Select1stISE_ESt4lessIvESaISE_EE8_M_eraseEPSt13_Rb_tree_nodeISE_E
	movq	%r14, %rdi
	call	_ZdlPv@PLT
.L1297:
	addq	$16, %rbx
	cmpq	%rbx, %r12
	jne	.L1305
.L1001:
	movzbl	376(%rsp), %esi
	leaq	384(%rsp), %rdi
	call	_ZN8nlohmann10basic_jsonISt3mapSt6vectorNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEblmdSaNS_14adl_serializerEE10json_value7destroyENS_6detail7value_tE
.L959:
	movq	392(%rsp), %rax
	xorq	%fs:40, %rax
	jne	.L1801
	addq	$408, %rsp
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
	ret
	.p2align 4,,10
	.p2align 3
.L961:
	.cfi_restore_state
	movq	16(%rsp), %rax
	movq	(%rax), %rdx
	movabsq	$1152921504606846975, %rax
	movq	%rax, %rcx
	subq	%rdx, %r12
	sarq	$4, %r12
	subq	%r12, %rcx
	cmpq	%rcx, 40(%rsp)
	ja	.L1802
	movq	40(%rsp), %rbx
	cmpq	%r12, %rbx
	movq	%rbx, %rcx
	movq	8(%rsp), %rbx
	cmovb	%r12, %rcx
	addq	%rcx, %r12
	setc	%cl
	subq	%rdx, %rbx
	movq	%r12, 96(%rsp)
	movzbl	%cl, %ecx
	movq	%rbx, 128(%rsp)
	testq	%rcx, %rcx
	jne	.L1502
	cmpq	%rax, %r12
	ja	.L1502
	testq	%r12, %r12
	movq	%r12, %rax
	je	.L1503
	salq	$4, %rax
	movq	%rax, %rdi
.L1309:
.LEHB139:
	call	_Znwm@PLT
	movq	%rax, 64(%rsp)
.L1310:
	movq	64(%rsp), %rax
	addq	128(%rsp), %rax
	movq	%rax, 184(%rsp)
	movq	%rax, %rbx
	movq	40(%rsp), %rax
	movq	%rax, 48(%rsp)
	.p2align 4,,10
	.p2align 3
.L1437:
	movzbl	0(%rbp), %eax
	movq	$0, 8(%rbx)
	cmpb	$7, %al
	movb	%al, (%rbx)
	ja	.L1311
	leaq	.L1313(%rip), %rdx
	movslq	(%rdx,%rax,4), %rax
	addq	%rdx, %rax
	jmp	*%rax
	.section	.rodata._ZNSt6vectorIN8nlohmann10basic_jsonISt3mapS_NSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEblmdSaNS0_14adl_serializerEEESaISA_EE14_M_fill_insertEN9__gnu_cxx17__normal_iteratorIPSA_SC_EEmRKSA_,"aG",@progbits,_ZNSt6vectorIN8nlohmann10basic_jsonISt3mapS_NSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEblmdSaNS0_14adl_serializerEEESaISA_EE14_M_fill_insertEN9__gnu_cxx17__normal_iteratorIPSA_SC_EEmRKSA_,comdat
	.align 4
	.align 4
.L1313:
	.long	.L1311-.L1313
	.long	.L1312-.L1313
	.long	.L1314-.L1313
	.long	.L1315-.L1313
	.long	.L1316-.L1313
	.long	.L1318-.L1313
	.long	.L1318-.L1313
	.long	.L1319-.L1313
	.section	.text._ZNSt6vectorIN8nlohmann10basic_jsonISt3mapS_NSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEblmdSaNS0_14adl_serializerEEESaISA_EE14_M_fill_insertEN9__gnu_cxx17__normal_iteratorIPSA_SC_EEmRKSA_,"axG",@progbits,_ZNSt6vectorIN8nlohmann10basic_jsonISt3mapS_NSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEblmdSaNS0_14adl_serializerEEESaISA_EE14_M_fill_insertEN9__gnu_cxx17__normal_iteratorIPSA_SC_EEmRKSA_,comdat
	.p2align 4,,10
	.p2align 3
.L966:
	movq	8(%rbp), %r12
	movl	$32, %edi
	call	_Znwm@PLT
.LEHE139:
	movq	%rax, %rbp
	leaq	16(%rax), %rax
	movq	%rbp, %rdi
	movq	%rax, 0(%rbp)
	movq	(%r12), %rsi
	movq	8(%r12), %rdx
	addq	%rsi, %rdx
.LEHB140:
	call	_ZNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEE12_M_constructIPcEEvT_S7_St20forward_iterator_tag.isra.67
.LEHE140:
.L1766:
	movq	16(%rsp), %rax
	movq	%rbp, 384(%rsp)
	movq	8(%rax), %r12
.L962:
	movq	%r12, %rax
	subq	8(%rsp), %rax
	movq	40(%rsp), %rsi
	movq	%rax, 56(%rsp)
	sarq	$4, %rax
	cmpq	%rax, %rsi
	jnb	.L987
	salq	$4, %rsi
	movq	%r12, %r14
	subq	%rsi, %r14
	movq	%rsi, (%rsp)
	cmpq	%r12, %r14
	je	.L988
	movq	%r14, %rax
	movq	%r12, %rdi
	movq	%r12, %rcx
	.p2align 4,,10
	.p2align 3
.L989:
	movzbl	(%rax), %edx
	addq	$16, %rax
	addq	$16, %rcx
	movb	%dl, -16(%rcx)
	movq	-8(%rax), %rdx
	movq	%rdx, -8(%rcx)
	movb	$0, -16(%rax)
	movq	$0, -8(%rax)
	cmpq	%rax, %rdi
	jne	.L989
.L988:
	movq	(%rsp), %rax
	movq	16(%rsp), %rdi
	addq	%r12, %rax
	movq	%rax, 8(%rdi)
	movq	%r14, %rax
	subq	8(%rsp), %rax
	movq	%rax, %r13
	sarq	$4, %r13
	testq	%rax, %rax
	jle	.L990
	movq	%rbx, 16(%rsp)
	movq	%r12, %rbx
	jmp	.L999
	.p2align 4,,10
	.p2align 3
.L1804:
	cmpb	$3, %dl
	je	.L993
	cmpb	$1, %dl
	jne	.L991
	movq	16(%r15), %rsi
	movq	%r15, %rdi
	call	_ZNSt8_Rb_treeINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESt4pairIKS5_N8nlohmann10basic_jsonISt3mapSt6vectorS5_blmdSaNS8_14adl_serializerEEEESt10_Select1stISE_ESt4lessIvESaISE_EE8_M_eraseEPSt13_Rb_tree_nodeISE_E
	movq	%r15, %rdi
	call	_ZdlPv@PLT
.L991:
	subq	$1, %r13
	je	.L1803
.L999:
	subq	$16, %r14
	subq	$16, %rbx
	movzbl	(%r14), %esi
	movb	$0, (%r14)
	movzbl	(%rbx), %edx
	movq	8(%r14), %rcx
	movq	$0, 8(%r14)
	movq	8(%rbx), %r15
	movb	%sil, (%rbx)
	cmpb	$2, %dl
	movq	%rcx, 8(%rbx)
	jne	.L1804
	movq	8(%r15), %r12
	movq	(%r15), %rbp
	cmpq	%rbp, %r12
	je	.L995
	.p2align 4,,10
	.p2align 3
.L996:
	movzbl	0(%rbp), %esi
	leaq	8(%rbp), %rdi
	addq	$16, %rbp
	call	_ZN8nlohmann10basic_jsonISt3mapSt6vectorNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEblmdSaNS_14adl_serializerEE10json_value7destroyENS_6detail7value_tE
	cmpq	%rbp, %r12
	jne	.L996
	movq	(%r15), %rbp
.L995:
	testq	%rbp, %rbp
	je	.L998
	movq	%rbp, %rdi
	call	_ZdlPv@PLT
.L998:
	movq	%r15, %rdi
	call	_ZdlPv@PLT
	subq	$1, %r13
	jne	.L999
.L1803:
	movq	16(%rsp), %rbx
.L990:
	movq	8(%rsp), %rdi
	movq	(%rsp), %rax
	addq	%rdi, %rax
	cmpq	%rax, %rdi
	movq	%rax, 24(%rsp)
	je	.L1001
	.p2align 4,,10
	.p2align 3
.L1000:
	movzbl	376(%rsp), %ebp
	movq	$0, 344(%rsp)
	cmpb	$7, %bpl
	ja	.L1002
	leaq	.L1004(%rip), %rcx
	movzbl	%bpl, %edx
	movslq	(%rcx,%rdx,4), %rax
	addq	%rcx, %rax
	jmp	*%rax
	.section	.rodata._ZNSt6vectorIN8nlohmann10basic_jsonISt3mapS_NSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEblmdSaNS0_14adl_serializerEEESaISA_EE14_M_fill_insertEN9__gnu_cxx17__normal_iteratorIPSA_SC_EEmRKSA_,"aG",@progbits,_ZNSt6vectorIN8nlohmann10basic_jsonISt3mapS_NSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEblmdSaNS0_14adl_serializerEEESaISA_EE14_M_fill_insertEN9__gnu_cxx17__normal_iteratorIPSA_SC_EEmRKSA_,comdat
	.align 4
	.align 4
.L1004:
	.long	.L1002-.L1004
	.long	.L1003-.L1004
	.long	.L1005-.L1004
	.long	.L1006-.L1004
	.long	.L1007-.L1004
	.long	.L1009-.L1004
	.long	.L1009-.L1004
	.long	.L1010-.L1004
	.section	.text._ZNSt6vectorIN8nlohmann10basic_jsonISt3mapS_NSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEblmdSaNS0_14adl_serializerEEESaISA_EE14_M_fill_insertEN9__gnu_cxx17__normal_iteratorIPSA_SC_EEmRKSA_,"axG",@progbits,_ZNSt6vectorIN8nlohmann10basic_jsonISt3mapS_NSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEblmdSaNS0_14adl_serializerEEESaISA_EE14_M_fill_insertEN9__gnu_cxx17__normal_iteratorIPSA_SC_EEmRKSA_,comdat
	.p2align 4,,10
	.p2align 3
.L1318:
	movq	8(%rbp), %rax
	movq	%rax, 8(%rbx)
.L1311:
	addq	$16, %rbx
	subq	$1, 48(%rsp)
	jne	.L1437
	movq	16(%rsp), %rax
	movq	64(%rsp), %rbp
	movq	(%rax), %r15
	cmpq	%r15, 8(%rsp)
	je	.L1439
	movq	8(%rsp), %rdx
	leaq	16(%r15), %rsi
	movq	%r15, %rax
	subq	%rsi, %rdx
	movq	%rdx, %rbp
	addq	%rdx, %rsi
	movq	64(%rsp), %rdx
	shrq	$4, %rbp
	.p2align 4,,10
	.p2align 3
.L1445:
	movzbl	(%rax), %ecx
	addq	$16, %rax
	addq	$16, %rdx
	movb	%cl, -16(%rdx)
	movq	-8(%rax), %rcx
	movq	%rcx, -8(%rdx)
	movb	$0, -16(%rax)
	movq	$0, -8(%rax)
	cmpq	%rax, %rsi
	jne	.L1445
	addq	$1, %rbp
	salq	$4, %rbp
	addq	64(%rsp), %rbp
.L1439:
	movq	40(%rsp), %rax
	salq	$4, %rax
	addq	%rax, %rbp
	movq	16(%rsp), %rax
	movq	8(%rax), %rbx
	cmpq	%rbx, 8(%rsp)
	je	.L1450
	movq	8(%rsp), %rax
	movq	%rbx, %rdx
	leaq	16(%rax), %rsi
	subq	%rsi, %rdx
	movq	%rdx, %rdi
	addq	%rdx, %rsi
	movq	%rbp, %rdx
	shrq	$4, %rdi
	.p2align 4,,10
	.p2align 3
.L1449:
	movzbl	(%rax), %ecx
	movb	$0, (%rax)
	addq	$16, %rax
	addq	$16, %rdx
	movb	%cl, -16(%rdx)
	movq	-8(%rax), %rcx
	movq	$0, -8(%rax)
	movq	%rcx, -8(%rdx)
	cmpq	%rax, %rsi
	jne	.L1449
	leaq	1(%rdi), %rax
	salq	$4, %rax
	addq	%rax, %rbp
.L1450:
	cmpq	%rbx, %r15
	jne	.L1448
	jmp	.L1447
	.p2align 4,,10
	.p2align 3
.L1806:
	cmpb	$3, %al
	je	.L1453
	cmpb	$1, %al
	jne	.L1451
	movq	8(%r15), %rdi
	movq	16(%rdi), %rsi
	call	_ZNSt8_Rb_treeINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESt4pairIKS5_N8nlohmann10basic_jsonISt3mapSt6vectorS5_blmdSaNS8_14adl_serializerEEEESt10_Select1stISE_ESt4lessIvESaISE_EE8_M_eraseEPSt13_Rb_tree_nodeISE_E
	movq	8(%r15), %rdi
	call	_ZdlPv@PLT
.L1451:
	addq	$16, %r15
	cmpq	%r15, %rbx
	je	.L1805
.L1448:
	movzbl	(%r15), %eax
	cmpb	$2, %al
	jne	.L1806
	movq	8(%r15), %r14
	movq	8(%r14), %r13
	movq	(%r14), %r12
	cmpq	%r12, %r13
	je	.L1455
	.p2align 4,,10
	.p2align 3
.L1456:
	movzbl	(%r12), %esi
	leaq	8(%r12), %rdi
	addq	$16, %r12
	call	_ZN8nlohmann10basic_jsonISt3mapSt6vectorNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEblmdSaNS_14adl_serializerEE10json_value7destroyENS_6detail7value_tE
	cmpq	%r12, %r13
	jne	.L1456
	movq	(%r14), %r12
.L1455:
	testq	%r12, %r12
	je	.L1797
	movq	%r12, %rdi
	call	_ZdlPv@PLT
.L1797:
	movq	8(%r15), %rdi
.L1458:
	addq	$16, %r15
	call	_ZdlPv@PLT
	cmpq	%r15, %rbx
	jne	.L1448
.L1805:
	movq	16(%rsp), %rax
	movq	(%rax), %rbx
.L1447:
	testq	%rbx, %rbx
	je	.L1459
	movq	%rbx, %rdi
	call	_ZdlPv@PLT
.L1459:
	movq	96(%rsp), %rax
	movq	64(%rsp), %rdi
	movq	16(%rsp), %rbx
	salq	$4, %rax
	addq	%rdi, %rax
	movq	%rdi, (%rbx)
	movq	%rbp, 8(%rbx)
	movq	%rax, 16(%rbx)
	jmp	.L959
	.p2align 4,,10
	.p2align 3
.L1319:
	movsd	8(%rbp), %xmm0
	movsd	%xmm0, 8(%rbx)
	jmp	.L1311
	.p2align 4,,10
	.p2align 3
.L1316:
	movzbl	8(%rbp), %eax
	movb	%al, 8(%rbx)
	jmp	.L1311
	.p2align 4,,10
	.p2align 3
.L1315:
	movl	$32, %edi
	movq	8(%rbp), %r13
.LEHB141:
	call	_Znwm@PLT
.LEHE141:
	movq	%rax, %r12
	leaq	16(%rax), %rax
	movq	%r12, %rdi
	movq	%rax, (%r12)
	movq	0(%r13), %rsi
	movq	8(%r13), %rdx
	addq	%rsi, %rdx
.LEHB142:
	call	_ZNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEE12_M_constructIPcEEvT_S7_St20forward_iterator_tag.isra.67
.LEHE142:
.L1795:
	movq	%r12, 8(%rbx)
	jmp	.L1311
	.p2align 4,,10
	.p2align 3
.L1314:
	movl	$24, %edi
	movq	8(%rbp), %r13
.LEHB143:
	call	_Znwm@PLT
.LEHE143:
	movq	8(%r13), %r12
	subq	0(%r13), %r12
	movq	%rax, %rcx
	movq	%rax, 120(%rsp)
	movq	$0, (%rcx)
	movq	$0, 8(%rcx)
	movq	$0, 16(%rcx)
	movq	%r12, %rax
	sarq	$4, %rax
	testq	%rax, %rax
	je	.L1807
	movabsq	$1152921504606846975, %rdx
	cmpq	%rdx, %rax
	ja	.L1808
	movq	%r12, %rdi
.LEHB144:
	call	_Znwm@PLT
.LEHE144:
	movq	%rax, 144(%rsp)
.L1326:
	movq	144(%rsp), %r15
	movq	120(%rsp), %rax
	addq	%r15, %r12
	movq	%r15, (%rax)
	movq	%r15, 8(%rax)
	movq	%r12, 16(%rax)
	movq	8(%r13), %rsi
	movq	0(%r13), %rax
	movq	%rsi, 72(%rsp)
	cmpq	%rax, %rsi
	je	.L1506
	movq	%rax, %r14
	.p2align 4,,10
	.p2align 3
.L1430:
	movzbl	(%r14), %eax
	movq	$0, 8(%r15)
	cmpb	$7, %al
	movb	%al, (%r15)
	ja	.L1329
	leaq	.L1331(%rip), %rdx
	movslq	(%rdx,%rax,4), %rax
	addq	%rdx, %rax
	jmp	*%rax
	.section	.rodata._ZNSt6vectorIN8nlohmann10basic_jsonISt3mapS_NSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEblmdSaNS0_14adl_serializerEEESaISA_EE14_M_fill_insertEN9__gnu_cxx17__normal_iteratorIPSA_SC_EEmRKSA_,"aG",@progbits,_ZNSt6vectorIN8nlohmann10basic_jsonISt3mapS_NSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEblmdSaNS0_14adl_serializerEEESaISA_EE14_M_fill_insertEN9__gnu_cxx17__normal_iteratorIPSA_SC_EEmRKSA_,comdat
	.align 4
	.align 4
.L1331:
	.long	.L1329-.L1331
	.long	.L1330-.L1331
	.long	.L1332-.L1331
	.long	.L1333-.L1331
	.long	.L1334-.L1331
	.long	.L1336-.L1331
	.long	.L1336-.L1331
	.long	.L1337-.L1331
	.section	.text._ZNSt6vectorIN8nlohmann10basic_jsonISt3mapS_NSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEblmdSaNS0_14adl_serializerEEESaISA_EE14_M_fill_insertEN9__gnu_cxx17__normal_iteratorIPSA_SC_EEmRKSA_,"axG",@progbits,_ZNSt6vectorIN8nlohmann10basic_jsonISt3mapS_NSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEblmdSaNS0_14adl_serializerEEESaISA_EE14_M_fill_insertEN9__gnu_cxx17__normal_iteratorIPSA_SC_EEmRKSA_,comdat
	.p2align 4,,10
	.p2align 3
.L1336:
	movq	8(%r14), %rax
	movq	%rax, 8(%r15)
.L1329:
	addq	$16, %r15
	addq	$16, %r14
	cmpq	%r14, 72(%rsp)
	jne	.L1430
.L1328:
	movq	120(%rsp), %rax
	movq	%r15, 8(%rax)
	movq	%rax, 8(%rbx)
	jmp	.L1311
	.p2align 4,,10
	.p2align 3
.L1312:
	movl	$48, %edi
	movq	8(%rbp), %r13
.LEHB145:
	call	_Znwm@PLT
.LEHE145:
	leaq	8(%rax), %rdx
	movq	$0, 16(%rax)
	movq	%rax, %r12
	movl	$0, 8(%rax)
	movq	$0, 40(%rax)
	movq	%rdx, 24(%rax)
	movq	%rdx, 32(%rax)
	movq	16(%r13), %rsi
	testq	%rsi, %rsi
	je	.L1795
	leaq	312(%rsp), %rcx
	movq	%rax, %rdi
	movq	%rax, 312(%rsp)
.LEHB146:
	call	_ZNSt8_Rb_treeINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESt4pairIKS5_N8nlohmann10basic_jsonISt3mapSt6vectorS5_blmdSaNS8_14adl_serializerEEEESt10_Select1stISE_ESt4lessIvESaISE_EE7_M_copyINSK_11_Alloc_nodeEEEPSt13_Rb_tree_nodeISE_EPKSO_PSt18_Rb_tree_node_baseRT_
.LEHE146:
	movq	%rax, %rcx
	jmp	.L1321
	.p2align 4,,10
	.p2align 3
.L1504:
	movq	%rdx, %rcx
.L1321:
	movq	16(%rcx), %rdx
	testq	%rdx, %rdx
	jne	.L1504
	movq	%rcx, 24(%r12)
	movq	%rax, %rcx
	jmp	.L1322
	.p2align 4,,10
	.p2align 3
.L1505:
	movq	%rdx, %rcx
.L1322:
	movq	24(%rcx), %rdx
	testq	%rdx, %rdx
	jne	.L1505
	movq	40(%r13), %rdx
	movq	%rcx, 32(%r12)
	movq	%rax, 16(%r12)
	movq	%rdx, 40(%r12)
	jmp	.L1795
	.p2align 4,,10
	.p2align 3
.L1337:
	movsd	8(%r14), %xmm0
	movsd	%xmm0, 8(%r15)
	jmp	.L1329
	.p2align 4,,10
	.p2align 3
.L1334:
	movzbl	8(%r14), %eax
	movb	%al, 8(%r15)
	jmp	.L1329
	.p2align 4,,10
	.p2align 3
.L1333:
	movl	$32, %edi
	movq	8(%r14), %r13
.LEHB147:
	call	_Znwm@PLT
.LEHE147:
	movq	%rax, %r12
	leaq	16(%rax), %rax
	movq	%r12, %rdi
	movq	%rax, (%r12)
	movq	0(%r13), %rsi
	movq	8(%r13), %rdx
	addq	%rsi, %rdx
.LEHB148:
	call	_ZNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEE12_M_constructIPcEEvT_S7_St20forward_iterator_tag.isra.67
.LEHE148:
.L1793:
	movq	%r12, 8(%r15)
	jmp	.L1329
	.p2align 4,,10
	.p2align 3
.L1332:
	movl	$24, %edi
	movq	8(%r14), %r13
.LEHB149:
	call	_Znwm@PLT
.LEHE149:
	movq	8(%r13), %r12
	subq	0(%r13), %r12
	movq	%rax, %rsi
	movq	%rax, 152(%rsp)
	movq	$0, (%rsi)
	movq	$0, 8(%rsi)
	movq	$0, 16(%rsi)
	movq	%r12, %rax
	sarq	$4, %rax
	testq	%rax, %rax
	je	.L1809
	movabsq	$1152921504606846975, %rdx
	cmpq	%rdx, %rax
	ja	.L1810
	movq	%r12, %rdi
.LEHB150:
	call	_Znwm@PLT
.LEHE150:
	movq	%rax, 168(%rsp)
.L1344:
	movq	168(%rsp), %rdi
	movq	152(%rsp), %rax
	addq	%rdi, %r12
	movq	%rdi, (%rax)
	movq	%rdi, 8(%rax)
	movq	%r12, 16(%rax)
	movq	8(%r13), %rsi
	movq	0(%r13), %rax
	movq	%rsi, 88(%rsp)
	cmpq	%rax, %rsi
	je	.L1509
	movq	%rax, (%rsp)
	movq	%rdi, 32(%rsp)
	.p2align 4,,10
	.p2align 3
.L1423:
	movq	(%rsp), %rax
	movq	32(%rsp), %rsi
	movzbl	(%rax), %eax
	movq	$0, 8(%rsi)
	cmpb	$7, %al
	movb	%al, (%rsi)
	ja	.L1347
	leaq	.L1349(%rip), %rsi
	movslq	(%rsi,%rax,4), %rax
	addq	%rsi, %rax
	jmp	*%rax
	.section	.rodata._ZNSt6vectorIN8nlohmann10basic_jsonISt3mapS_NSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEblmdSaNS0_14adl_serializerEEESaISA_EE14_M_fill_insertEN9__gnu_cxx17__normal_iteratorIPSA_SC_EEmRKSA_,"aG",@progbits,_ZNSt6vectorIN8nlohmann10basic_jsonISt3mapS_NSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEblmdSaNS0_14adl_serializerEEESaISA_EE14_M_fill_insertEN9__gnu_cxx17__normal_iteratorIPSA_SC_EEmRKSA_,comdat
	.align 4
	.align 4
.L1349:
	.long	.L1347-.L1349
	.long	.L1348-.L1349
	.long	.L1350-.L1349
	.long	.L1351-.L1349
	.long	.L1352-.L1349
	.long	.L1353-.L1349
	.long	.L1354-.L1349
	.long	.L1355-.L1349
	.section	.text._ZNSt6vectorIN8nlohmann10basic_jsonISt3mapS_NSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEblmdSaNS0_14adl_serializerEEESaISA_EE14_M_fill_insertEN9__gnu_cxx17__normal_iteratorIPSA_SC_EEmRKSA_,"axG",@progbits,_ZNSt6vectorIN8nlohmann10basic_jsonISt3mapS_NSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEblmdSaNS0_14adl_serializerEEESaISA_EE14_M_fill_insertEN9__gnu_cxx17__normal_iteratorIPSA_SC_EEmRKSA_,comdat
	.p2align 4,,10
	.p2align 3
.L1330:
	movl	$48, %edi
	movq	8(%r14), %r13
.LEHB151:
	call	_Znwm@PLT
.LEHE151:
	leaq	8(%rax), %rdx
	movq	$0, 16(%rax)
	movq	%rax, %r12
	movl	$0, 8(%rax)
	movq	$0, 40(%rax)
	movq	%rdx, 24(%rax)
	movq	%rdx, 32(%rax)
	movq	16(%r13), %rsi
	testq	%rsi, %rsi
	je	.L1793
	leaq	320(%rsp), %rcx
	movq	%rax, %rdi
	movq	%rax, 320(%rsp)
.LEHB152:
	call	_ZNSt8_Rb_treeINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESt4pairIKS5_N8nlohmann10basic_jsonISt3mapSt6vectorS5_blmdSaNS8_14adl_serializerEEEESt10_Select1stISE_ESt4lessIvESaISE_EE7_M_copyINSK_11_Alloc_nodeEEEPSt13_Rb_tree_nodeISE_EPKSO_PSt18_Rb_tree_node_baseRT_
.LEHE152:
	movq	%rax, %rcx
	jmp	.L1339
	.p2align 4,,10
	.p2align 3
.L1507:
	movq	%rdx, %rcx
.L1339:
	movq	16(%rcx), %rdx
	testq	%rdx, %rdx
	jne	.L1507
	movq	%rcx, 24(%r12)
	movq	%rax, %rcx
	jmp	.L1340
	.p2align 4,,10
	.p2align 3
.L1508:
	movq	%rdx, %rcx
.L1340:
	movq	24(%rcx), %rdx
	testq	%rdx, %rdx
	jne	.L1508
	movq	40(%r13), %rdx
	movq	%rcx, 32(%r12)
	movq	%rax, 16(%r12)
	movq	%rdx, 40(%r12)
	jmp	.L1793
	.p2align 4,,10
	.p2align 3
.L1355:
	movq	(%rsp), %rax
	movsd	8(%rax), %xmm0
	movq	32(%rsp), %rax
	movsd	%xmm0, 8(%rax)
	.p2align 4,,10
	.p2align 3
.L1347:
	addq	$16, (%rsp)
	addq	$16, 32(%rsp)
	movq	(%rsp), %rax
	cmpq	%rax, 88(%rsp)
	jne	.L1423
.L1346:
	movq	152(%rsp), %rax
	movq	32(%rsp), %rdi
	movq	%rdi, 8(%rax)
	movq	%rax, 8(%r15)
	jmp	.L1329
	.p2align 4,,10
	.p2align 3
.L1354:
	movq	(%rsp), %rax
	movq	32(%rsp), %rsi
	movq	8(%rax), %rax
	movq	%rax, 8(%rsi)
	jmp	.L1347
	.p2align 4,,10
	.p2align 3
.L1353:
	movq	(%rsp), %rax
	movq	32(%rsp), %rcx
	movq	8(%rax), %rax
	movq	%rax, 8(%rcx)
	jmp	.L1347
	.p2align 4,,10
	.p2align 3
.L1352:
	movq	(%rsp), %rax
	movq	32(%rsp), %rdi
	movzbl	8(%rax), %eax
	movb	%al, 8(%rdi)
	jmp	.L1347
	.p2align 4,,10
	.p2align 3
.L1351:
	movq	(%rsp), %rax
	movl	$32, %edi
	movq	8(%rax), %r13
.LEHB153:
	call	_Znwm@PLT
.LEHE153:
	movq	%rax, %r12
	leaq	16(%rax), %rax
	movq	%r12, %rdi
	movq	%rax, (%r12)
	movq	0(%r13), %rsi
	movq	8(%r13), %rdx
	addq	%rsi, %rdx
.LEHB154:
	call	_ZNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEE12_M_constructIPcEEvT_S7_St20forward_iterator_tag.isra.67
.LEHE154:
.L1791:
	movq	32(%rsp), %rax
	movq	%r12, 8(%rax)
	jmp	.L1347
	.p2align 4,,10
	.p2align 3
.L1348:
	movq	(%rsp), %rax
	movl	$48, %edi
	movq	8(%rax), %r13
.LEHB155:
	call	_Znwm@PLT
.LEHE155:
	leaq	8(%rax), %rdx
	movq	$0, 16(%rax)
	movq	%rax, %r12
	movl	$0, 8(%rax)
	movq	$0, 40(%rax)
	movq	%rdx, 24(%rax)
	movq	%rdx, 32(%rax)
	movq	16(%r13), %rsi
	testq	%rsi, %rsi
	je	.L1791
	leaq	328(%rsp), %rcx
	movq	%rax, %rdi
	movq	%rax, 328(%rsp)
.LEHB156:
	call	_ZNSt8_Rb_treeINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESt4pairIKS5_N8nlohmann10basic_jsonISt3mapSt6vectorS5_blmdSaNS8_14adl_serializerEEEESt10_Select1stISE_ESt4lessIvESaISE_EE7_M_copyINSK_11_Alloc_nodeEEEPSt13_Rb_tree_nodeISE_EPKSO_PSt18_Rb_tree_node_baseRT_
.LEHE156:
	movq	%rax, %rcx
	jmp	.L1357
	.p2align 4,,10
	.p2align 3
.L1510:
	movq	%rdx, %rcx
.L1357:
	movq	16(%rcx), %rdx
	testq	%rdx, %rdx
	jne	.L1510
	movq	%rcx, 24(%r12)
	movq	%rax, %rcx
	jmp	.L1358
	.p2align 4,,10
	.p2align 3
.L1511:
	movq	%rdx, %rcx
.L1358:
	movq	24(%rcx), %rdx
	testq	%rdx, %rdx
	jne	.L1511
	movq	40(%r13), %rdx
	movq	%rcx, 32(%r12)
	movq	%rax, 16(%r12)
	movq	%rdx, 40(%r12)
	jmp	.L1791
	.p2align 4,,10
	.p2align 3
.L1350:
	movq	(%rsp), %rax
	movl	$24, %edi
	movq	8(%rax), %r13
.LEHB157:
	call	_Znwm@PLT
.LEHE157:
	movq	8(%r13), %r12
	subq	0(%r13), %r12
	movq	%rax, %rcx
	movq	%rax, 176(%rsp)
	movq	$0, (%rcx)
	movq	$0, 8(%rcx)
	movq	$0, 16(%rcx)
	movq	%r12, %rax
	sarq	$4, %rax
	testq	%rax, %rax
	je	.L1811
	movabsq	$1152921504606846975, %rdx
	cmpq	%rdx, %rax
	ja	.L1812
	movq	%r12, %rdi
.LEHB158:
	call	_Znwm@PLT
.LEHE158:
	movq	%rax, 192(%rsp)
.L1362:
	movq	192(%rsp), %rsi
	movq	176(%rsp), %rax
	addq	%rsi, %r12
	movq	%rsi, (%rax)
	movq	%rsi, 8(%rax)
	movq	%r12, 16(%rax)
	movq	8(%r13), %rcx
	movq	0(%r13), %rax
	movq	%rcx, 104(%rsp)
	cmpq	%rax, %rcx
	je	.L1512
	movq	%rax, 24(%rsp)
	movq	%rsi, 56(%rsp)
	.p2align 4,,10
	.p2align 3
.L1416:
	movq	24(%rsp), %rax
	movq	56(%rsp), %rdi
	movzbl	(%rax), %eax
	movq	$0, 8(%rdi)
	cmpb	$7, %al
	movb	%al, (%rdi)
	ja	.L1365
	leaq	.L1367(%rip), %rdi
	movslq	(%rdi,%rax,4), %rax
	addq	%rdi, %rax
	jmp	*%rax
	.section	.rodata._ZNSt6vectorIN8nlohmann10basic_jsonISt3mapS_NSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEblmdSaNS0_14adl_serializerEEESaISA_EE14_M_fill_insertEN9__gnu_cxx17__normal_iteratorIPSA_SC_EEmRKSA_,"aG",@progbits,_ZNSt6vectorIN8nlohmann10basic_jsonISt3mapS_NSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEblmdSaNS0_14adl_serializerEEESaISA_EE14_M_fill_insertEN9__gnu_cxx17__normal_iteratorIPSA_SC_EEmRKSA_,comdat
	.align 4
	.align 4
.L1367:
	.long	.L1365-.L1367
	.long	.L1366-.L1367
	.long	.L1368-.L1367
	.long	.L1369-.L1367
	.long	.L1370-.L1367
	.long	.L1371-.L1367
	.long	.L1372-.L1367
	.long	.L1373-.L1367
	.section	.text._ZNSt6vectorIN8nlohmann10basic_jsonISt3mapS_NSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEblmdSaNS0_14adl_serializerEEESaISA_EE14_M_fill_insertEN9__gnu_cxx17__normal_iteratorIPSA_SC_EEmRKSA_,"axG",@progbits,_ZNSt6vectorIN8nlohmann10basic_jsonISt3mapS_NSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEblmdSaNS0_14adl_serializerEEESaISA_EE14_M_fill_insertEN9__gnu_cxx17__normal_iteratorIPSA_SC_EEmRKSA_,comdat
	.p2align 4,,10
	.p2align 3
.L987:
	movq	40(%rsp), %rdi
	movq	%r12, %rbp
	subq	%rax, %rdi
	movq	%rdi, 32(%rsp)
	je	.L1137
	.p2align 4,,10
	.p2align 3
.L1214:
	movzbl	376(%rsp), %eax
	movq	$0, 8(%rbp)
	cmpb	$7, %al
	movb	%al, 0(%rbp)
	ja	.L1138
	leaq	.L1140(%rip), %rdi
	movslq	(%rdi,%rax,4), %rax
	addq	%rdi, %rax
	jmp	*%rax
	.section	.rodata._ZNSt6vectorIN8nlohmann10basic_jsonISt3mapS_NSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEblmdSaNS0_14adl_serializerEEESaISA_EE14_M_fill_insertEN9__gnu_cxx17__normal_iteratorIPSA_SC_EEmRKSA_,"aG",@progbits,_ZNSt6vectorIN8nlohmann10basic_jsonISt3mapS_NSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEblmdSaNS0_14adl_serializerEEESaISA_EE14_M_fill_insertEN9__gnu_cxx17__normal_iteratorIPSA_SC_EEmRKSA_,comdat
	.align 4
	.align 4
.L1140:
	.long	.L1138-.L1140
	.long	.L1139-.L1140
	.long	.L1141-.L1140
	.long	.L1142-.L1140
	.long	.L1143-.L1140
	.long	.L1145-.L1140
	.long	.L1145-.L1140
	.long	.L1146-.L1140
	.section	.text._ZNSt6vectorIN8nlohmann10basic_jsonISt3mapS_NSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEblmdSaNS0_14adl_serializerEEESaISA_EE14_M_fill_insertEN9__gnu_cxx17__normal_iteratorIPSA_SC_EEmRKSA_,"axG",@progbits,_ZNSt6vectorIN8nlohmann10basic_jsonISt3mapS_NSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEblmdSaNS0_14adl_serializerEEESaISA_EE14_M_fill_insertEN9__gnu_cxx17__normal_iteratorIPSA_SC_EEmRKSA_,comdat
	.p2align 4,,10
	.p2align 3
.L1145:
	movq	384(%rsp), %rax
	movq	%rax, 8(%rbp)
.L1138:
	addq	$16, %rbp
	subq	$1, 32(%rsp)
	jne	.L1214
.L1137:
	movq	8(%rsp), %rdi
	movq	56(%rsp), %rcx
	movq	%r12, %rsi
	movq	16(%rsp), %rax
	addq	%rbp, %rcx
	cmpq	%r12, %rdi
	movq	%rbp, 8(%rax)
	movq	%rdi, %rax
	je	.L1813
	.p2align 4,,10
	.p2align 3
.L1220:
	movzbl	(%rax), %edx
	addq	$16, %rax
	addq	$16, %rbp
	movb	%dl, -16(%rbp)
	movq	-8(%rax), %rdx
	movq	%rdx, -8(%rbp)
	movb	$0, -16(%rax)
	movq	$0, -8(%rax)
	cmpq	%rsi, %rax
	jne	.L1220
	movq	16(%rsp), %rax
	movq	%rcx, 8(%rax)
	.p2align 4,,10
	.p2align 3
.L1305:
	movzbl	376(%rsp), %ebp
	movq	$0, 360(%rsp)
	cmpb	$7, %bpl
	ja	.L1222
	leaq	.L1224(%rip), %rsi
	movzbl	%bpl, %eax
	movslq	(%rsi,%rax,4), %rax
	addq	%rsi, %rax
	jmp	*%rax
	.section	.rodata._ZNSt6vectorIN8nlohmann10basic_jsonISt3mapS_NSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEblmdSaNS0_14adl_serializerEEESaISA_EE14_M_fill_insertEN9__gnu_cxx17__normal_iteratorIPSA_SC_EEmRKSA_,"aG",@progbits,_ZNSt6vectorIN8nlohmann10basic_jsonISt3mapS_NSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEblmdSaNS0_14adl_serializerEEESaISA_EE14_M_fill_insertEN9__gnu_cxx17__normal_iteratorIPSA_SC_EEmRKSA_,comdat
	.align 4
	.align 4
.L1224:
	.long	.L1222-.L1224
	.long	.L1223-.L1224
	.long	.L1225-.L1224
	.long	.L1226-.L1224
	.long	.L1227-.L1224
	.long	.L1229-.L1224
	.long	.L1229-.L1224
	.long	.L1230-.L1224
	.section	.text._ZNSt6vectorIN8nlohmann10basic_jsonISt3mapS_NSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEblmdSaNS0_14adl_serializerEEESaISA_EE14_M_fill_insertEN9__gnu_cxx17__normal_iteratorIPSA_SC_EEmRKSA_,"axG",@progbits,_ZNSt6vectorIN8nlohmann10basic_jsonISt3mapS_NSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEblmdSaNS0_14adl_serializerEEESaISA_EE14_M_fill_insertEN9__gnu_cxx17__normal_iteratorIPSA_SC_EEmRKSA_,comdat
	.p2align 4,,10
	.p2align 3
.L1502:
	movq	$-16, %rdi
	movq	%rax, 96(%rsp)
	jmp	.L1309
	.p2align 4,,10
	.p2align 3
.L1373:
	movq	24(%rsp), %rax
	movsd	8(%rax), %xmm0
	movq	56(%rsp), %rax
	movsd	%xmm0, 8(%rax)
	.p2align 4,,10
	.p2align 3
.L1365:
	addq	$16, 24(%rsp)
	addq	$16, 56(%rsp)
	movq	24(%rsp), %rax
	cmpq	%rax, 104(%rsp)
	jne	.L1416
.L1364:
	movq	176(%rsp), %rax
	movq	56(%rsp), %rdi
	movq	32(%rsp), %rcx
	movq	%rdi, 8(%rax)
	movq	%rax, 8(%rcx)
	jmp	.L1347
	.p2align 4,,10
	.p2align 3
.L1372:
	movq	24(%rsp), %rax
	movq	56(%rsp), %rdi
	movq	8(%rax), %rax
	movq	%rax, 8(%rdi)
	jmp	.L1365
	.p2align 4,,10
	.p2align 3
.L1371:
	movq	24(%rsp), %rax
	movq	56(%rsp), %rsi
	movq	8(%rax), %rax
	movq	%rax, 8(%rsi)
	jmp	.L1365
	.p2align 4,,10
	.p2align 3
.L1370:
	movq	24(%rsp), %rax
	movq	56(%rsp), %rcx
	movzbl	8(%rax), %eax
	movb	%al, 8(%rcx)
	jmp	.L1365
	.p2align 4,,10
	.p2align 3
.L1369:
	movq	24(%rsp), %rax
	movl	$32, %edi
	movq	8(%rax), %r13
.LEHB159:
	call	_Znwm@PLT
.LEHE159:
	movq	%rax, %r12
	leaq	16(%rax), %rax
	movq	%r12, %rdi
	movq	%rax, (%r12)
	movq	0(%r13), %rsi
	movq	8(%r13), %rdx
	addq	%rsi, %rdx
.LEHB160:
	call	_ZNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEE12_M_constructIPcEEvT_S7_St20forward_iterator_tag.isra.67
.LEHE160:
.L1789:
	movq	56(%rsp), %rax
	movq	%r12, 8(%rax)
	jmp	.L1365
	.p2align 4,,10
	.p2align 3
.L1366:
	movq	24(%rsp), %rax
	movl	$48, %edi
	movq	8(%rax), %r13
.LEHB161:
	call	_Znwm@PLT
.LEHE161:
	leaq	8(%rax), %rdx
	movq	$0, 16(%rax)
	movq	%rax, %r12
	movl	$0, 8(%rax)
	movq	$0, 40(%rax)
	movq	%rdx, 24(%rax)
	movq	%rdx, 32(%rax)
	movq	16(%r13), %rsi
	testq	%rsi, %rsi
	je	.L1789
	leaq	336(%rsp), %rcx
	movq	%rax, %rdi
	movq	%rax, 336(%rsp)
.LEHB162:
	call	_ZNSt8_Rb_treeINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESt4pairIKS5_N8nlohmann10basic_jsonISt3mapSt6vectorS5_blmdSaNS8_14adl_serializerEEEESt10_Select1stISE_ESt4lessIvESaISE_EE7_M_copyINSK_11_Alloc_nodeEEEPSt13_Rb_tree_nodeISE_EPKSO_PSt18_Rb_tree_node_baseRT_
.LEHE162:
	movq	%rax, %rcx
	jmp	.L1375
	.p2align 4,,10
	.p2align 3
.L1513:
	movq	%rdx, %rcx
.L1375:
	movq	16(%rcx), %rdx
	testq	%rdx, %rdx
	jne	.L1513
	movq	%rcx, 24(%r12)
	movq	%rax, %rcx
	jmp	.L1376
	.p2align 4,,10
	.p2align 3
.L1514:
	movq	%rdx, %rcx
.L1376:
	movq	24(%rcx), %rdx
	testq	%rdx, %rdx
	jne	.L1514
	movq	40(%r13), %rdx
	movq	%rcx, 32(%r12)
	movq	%rax, 16(%r12)
	movq	%rdx, 40(%r12)
	jmp	.L1789
	.p2align 4,,10
	.p2align 3
.L1368:
	movq	24(%rsp), %rax
	movl	$24, %edi
	movq	8(%rax), %r13
.LEHB163:
	call	_Znwm@PLT
.LEHE163:
	movq	8(%r13), %r12
	subq	0(%r13), %r12
	movq	%rax, %rdi
	movq	%rax, 200(%rsp)
	movq	$0, (%rdi)
	movq	$0, 8(%rdi)
	movq	$0, 16(%rdi)
	movq	%r12, %rax
	sarq	$4, %rax
	testq	%rax, %rax
	je	.L1814
	movabsq	$1152921504606846975, %rdi
	cmpq	%rdi, %rax
	ja	.L1815
	movq	%r12, %rdi
.LEHB164:
	call	_Znwm@PLT
.LEHE164:
	movq	%rax, 208(%rsp)
.L1380:
	movq	208(%rsp), %rax
	movq	200(%rsp), %rcx
	addq	%rax, %r12
	movq	%rax, (%rcx)
	movq	%rax, 8(%rcx)
	movq	%r12, 16(%rcx)
	movq	8(%r13), %rcx
	movq	0(%r13), %r12
	movq	%rcx, 136(%rsp)
	cmpq	%r12, %rcx
	je	.L1515
	movq	%rax, 80(%rsp)
	leaq	352(%rsp), %rax
	movq	%rax, 232(%rsp)
	.p2align 4,,10
	.p2align 3
.L1409:
	movzbl	(%r12), %eax
	movq	80(%rsp), %rcx
	movq	$0, 8(%rcx)
	cmpb	$7, %al
	movb	%al, (%rcx)
	ja	.L1383
	leaq	.L1385(%rip), %rsi
	movslq	(%rsi,%rax,4), %rax
	addq	%rsi, %rax
	jmp	*%rax
	.section	.rodata._ZNSt6vectorIN8nlohmann10basic_jsonISt3mapS_NSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEblmdSaNS0_14adl_serializerEEESaISA_EE14_M_fill_insertEN9__gnu_cxx17__normal_iteratorIPSA_SC_EEmRKSA_,"aG",@progbits,_ZNSt6vectorIN8nlohmann10basic_jsonISt3mapS_NSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEblmdSaNS0_14adl_serializerEEESaISA_EE14_M_fill_insertEN9__gnu_cxx17__normal_iteratorIPSA_SC_EEmRKSA_,comdat
	.align 4
	.align 4
.L1385:
	.long	.L1383-.L1385
	.long	.L1384-.L1385
	.long	.L1386-.L1385
	.long	.L1387-.L1385
	.long	.L1388-.L1385
	.long	.L1389-.L1385
	.long	.L1390-.L1385
	.long	.L1391-.L1385
	.section	.text._ZNSt6vectorIN8nlohmann10basic_jsonISt3mapS_NSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEblmdSaNS0_14adl_serializerEEESaISA_EE14_M_fill_insertEN9__gnu_cxx17__normal_iteratorIPSA_SC_EEmRKSA_,"axG",@progbits,_ZNSt6vectorIN8nlohmann10basic_jsonISt3mapS_NSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEblmdSaNS0_14adl_serializerEEESaISA_EE14_M_fill_insertEN9__gnu_cxx17__normal_iteratorIPSA_SC_EEmRKSA_,comdat
	.p2align 4,,10
	.p2align 3
.L969:
	movq	8(%rbp), %rax
	movq	%rax, 384(%rsp)
	jmp	.L962
	.p2align 4,,10
	.p2align 3
.L1230:
	movsd	384(%rsp), %xmm0
	movsd	%xmm0, 360(%rsp)
	jmp	.L1222
	.p2align 4,,10
	.p2align 3
.L1227:
	movzbl	384(%rsp), %eax
	movb	%al, 360(%rsp)
	jmp	.L1222
	.p2align 4,,10
	.p2align 3
.L1139:
	movl	$48, %edi
	movq	384(%rsp), %r14
.LEHB165:
	call	_Znwm@PLT
.LEHE165:
	leaq	8(%rax), %rdx
	movq	$0, 16(%rax)
	movq	%rax, %r13
	movl	$0, 8(%rax)
	movq	$0, 40(%rax)
	movq	%rdx, 24(%rax)
	movq	%rdx, 32(%rax)
	movq	16(%r14), %rsi
	testq	%rsi, %rsi
	je	.L1780
	leaq	288(%rsp), %rcx
	movq	%rax, %rdi
	movq	%rax, 288(%rsp)
.LEHB166:
	call	_ZNSt8_Rb_treeINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESt4pairIKS5_N8nlohmann10basic_jsonISt3mapSt6vectorS5_blmdSaNS8_14adl_serializerEEEESt10_Select1stISE_ESt4lessIvESaISE_EE7_M_copyINSK_11_Alloc_nodeEEEPSt13_Rb_tree_nodeISE_EPKSO_PSt18_Rb_tree_node_baseRT_
.LEHE166:
	movq	%rax, %rcx
	jmp	.L1148
	.p2align 4,,10
	.p2align 3
.L1483:
	movq	%rdx, %rcx
.L1148:
	movq	16(%rcx), %rdx
	testq	%rdx, %rdx
	jne	.L1483
	movq	%rcx, 24(%r13)
	movq	%rax, %rcx
	jmp	.L1149
	.p2align 4,,10
	.p2align 3
.L1484:
	movq	%rdx, %rcx
.L1149:
	movq	24(%rcx), %rdx
	testq	%rdx, %rdx
	jne	.L1484
	movq	40(%r14), %rdx
	movq	%rcx, 32(%r13)
	movq	%rax, 16(%r13)
	movq	%rdx, 40(%r13)
	jmp	.L1780
	.p2align 4,,10
	.p2align 3
.L1226:
	movl	$32, %edi
	movq	384(%rsp), %r14
.LEHB167:
	call	_Znwm@PLT
.LEHE167:
	movq	%rax, %r13
	leaq	16(%rax), %rax
	movq	%r13, %rdi
	movq	%rax, 0(%r13)
	movq	(%r14), %rsi
	movq	8(%r14), %rdx
	addq	%rsi, %rdx
.LEHB168:
	call	_ZNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEE12_M_constructIPcEEvT_S7_St20forward_iterator_tag.isra.67
.LEHE168:
.L1786:
	movq	%r13, 360(%rsp)
	jmp	.L1222
	.p2align 4,,10
	.p2align 3
.L1223:
	movl	$48, %edi
	movq	384(%rsp), %r14
.LEHB169:
	call	_Znwm@PLT
.LEHE169:
	leaq	8(%rax), %rdx
	movq	$0, 16(%rax)
	movq	%rax, %r13
	movl	$0, 8(%rax)
	movq	$0, 40(%rax)
	movq	%rdx, 24(%rax)
	movq	%rdx, 32(%rax)
	movq	16(%r14), %rsi
	testq	%rsi, %rsi
	je	.L1786
	leaq	312(%rsp), %rcx
	movq	%rax, %rdi
	movq	%rax, 312(%rsp)
.LEHB170:
	call	_ZNSt8_Rb_treeINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESt4pairIKS5_N8nlohmann10basic_jsonISt3mapSt6vectorS5_blmdSaNS8_14adl_serializerEEEESt10_Select1stISE_ESt4lessIvESaISE_EE7_M_copyINSK_11_Alloc_nodeEEEPSt13_Rb_tree_nodeISE_EPKSO_PSt18_Rb_tree_node_baseRT_
.LEHE170:
	movq	%rax, %rcx
	jmp	.L1232
	.p2align 4,,10
	.p2align 3
.L1492:
	movq	%rdx, %rcx
.L1232:
	movq	16(%rcx), %rdx
	testq	%rdx, %rdx
	jne	.L1492
	movq	%rcx, 24(%r13)
	movq	%rax, %rcx
	jmp	.L1233
	.p2align 4,,10
	.p2align 3
.L1493:
	movq	%rdx, %rcx
.L1233:
	movq	24(%rcx), %rdx
	testq	%rdx, %rdx
	jne	.L1493
	movq	40(%r14), %rdx
	movq	%rcx, 32(%r13)
	movq	%rax, 16(%r13)
	movq	%rdx, 40(%r13)
	jmp	.L1786
	.p2align 4,,10
	.p2align 3
.L1225:
	movl	$24, %edi
	movq	384(%rsp), %r14
.LEHB171:
	call	_Znwm@PLT
.LEHE171:
	movq	8(%r14), %r13
	subq	(%r14), %r13
	movq	%rax, %rsi
	movq	%rax, 48(%rsp)
	movq	$0, (%rsi)
	movq	$0, 8(%rsi)
	movq	$0, 16(%rsi)
	movq	%r13, %rax
	sarq	$4, %rax
	testq	%rax, %rax
	je	.L1816
	movabsq	$1152921504606846975, %rdx
	cmpq	%rdx, %rax
	ja	.L1817
	movq	%r13, %rdi
.LEHB172:
	call	_Znwm@PLT
.LEHE172:
	movq	%rax, 64(%rsp)
.L1236:
	movq	64(%rsp), %r15
	movq	48(%rsp), %rax
	addq	%r15, %r13
	movq	%r15, (%rax)
	movq	%r15, 8(%rax)
	movq	%r13, 16(%rax)
	movq	8(%r14), %rcx
	movq	(%r14), %rax
	movq	%rcx, 16(%rsp)
	cmpq	%rax, %rcx
	je	.L1494
	movq	%rax, %r14
	.p2align 4,,10
	.p2align 3
.L1290:
	movzbl	(%r14), %eax
	movq	$0, 8(%r15)
	cmpb	$7, %al
	movb	%al, (%r15)
	ja	.L1239
	leaq	.L1241(%rip), %rsi
	movslq	(%rsi,%rax,4), %rax
	addq	%rsi, %rax
	jmp	*%rax
	.section	.rodata._ZNSt6vectorIN8nlohmann10basic_jsonISt3mapS_NSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEblmdSaNS0_14adl_serializerEEESaISA_EE14_M_fill_insertEN9__gnu_cxx17__normal_iteratorIPSA_SC_EEmRKSA_,"aG",@progbits,_ZNSt6vectorIN8nlohmann10basic_jsonISt3mapS_NSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEblmdSaNS0_14adl_serializerEEESaISA_EE14_M_fill_insertEN9__gnu_cxx17__normal_iteratorIPSA_SC_EEmRKSA_,comdat
	.align 4
	.align 4
.L1241:
	.long	.L1239-.L1241
	.long	.L1240-.L1241
	.long	.L1242-.L1241
	.long	.L1243-.L1241
	.long	.L1244-.L1241
	.long	.L1246-.L1241
	.long	.L1246-.L1241
	.long	.L1247-.L1241
	.section	.text._ZNSt6vectorIN8nlohmann10basic_jsonISt3mapS_NSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEblmdSaNS0_14adl_serializerEEESaISA_EE14_M_fill_insertEN9__gnu_cxx17__normal_iteratorIPSA_SC_EEmRKSA_,"axG",@progbits,_ZNSt6vectorIN8nlohmann10basic_jsonISt3mapS_NSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEblmdSaNS0_14adl_serializerEEESaISA_EE14_M_fill_insertEN9__gnu_cxx17__normal_iteratorIPSA_SC_EEmRKSA_,comdat
	.p2align 4,,10
	.p2align 3
.L1246:
	movq	8(%r14), %rax
	movq	%rax, 8(%r15)
.L1239:
	addq	$16, %r15
	addq	$16, %r14
	cmpq	%r14, 16(%rsp)
	jne	.L1290
.L1238:
	movq	48(%rsp), %rax
	movq	%r15, 8(%rax)
	movq	%rax, 360(%rsp)
	jmp	.L1222
	.p2align 4,,10
	.p2align 3
.L1146:
	movsd	384(%rsp), %xmm0
	movsd	%xmm0, 8(%rbp)
	jmp	.L1138
	.p2align 4,,10
	.p2align 3
.L1143:
	movzbl	384(%rsp), %eax
	movb	%al, 8(%rbp)
	jmp	.L1138
	.p2align 4,,10
	.p2align 3
.L1142:
	movl	$32, %edi
	movq	384(%rsp), %r14
.LEHB173:
	call	_Znwm@PLT
.LEHE173:
	movq	%rax, %r13
	leaq	16(%rax), %rax
	movq	%r13, %rdi
	movq	%rax, 0(%r13)
	movq	(%r14), %rsi
	movq	8(%r14), %rdx
	addq	%rsi, %rdx
.LEHB174:
	call	_ZNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEE12_M_constructIPcEEvT_S7_St20forward_iterator_tag.isra.67
.LEHE174:
.L1780:
	movq	%r13, 8(%rbp)
	jmp	.L1138
	.p2align 4,,10
	.p2align 3
.L1141:
	movl	$24, %edi
	movq	384(%rsp), %r14
.LEHB175:
	call	_Znwm@PLT
.LEHE175:
	movq	8(%r14), %r13
	subq	(%r14), %r13
	movq	%rax, %rdi
	movq	%rax, 80(%rsp)
	movq	$0, (%rdi)
	movq	$0, 8(%rdi)
	movq	$0, 16(%rdi)
	movq	%r13, %rax
	sarq	$4, %rax
	testq	%rax, %rax
	je	.L1818
	movabsq	$1152921504606846975, %rdx
	cmpq	%rdx, %rax
	ja	.L1819
	movq	%r13, %rdi
.LEHB176:
	call	_Znwm@PLT
.LEHE176:
	movq	%rax, 96(%rsp)
.L1153:
	movq	96(%rsp), %r15
	movq	80(%rsp), %rax
	addq	%r15, %r13
	movq	%r15, (%rax)
	movq	%r15, 8(%rax)
	movq	%r13, 16(%rax)
	movq	8(%r14), %rdi
	movq	(%r14), %rax
	movq	%rdi, 40(%rsp)
	cmpq	%rax, %rdi
	je	.L1485
	movq	%rax, %r14
	.p2align 4,,10
	.p2align 3
.L1207:
	movzbl	(%r14), %eax
	movq	$0, 8(%r15)
	cmpb	$7, %al
	movb	%al, (%r15)
	ja	.L1156
	leaq	.L1158(%rip), %rdi
	movslq	(%rdi,%rax,4), %rax
	addq	%rdi, %rax
	jmp	*%rax
	.section	.rodata._ZNSt6vectorIN8nlohmann10basic_jsonISt3mapS_NSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEblmdSaNS0_14adl_serializerEEESaISA_EE14_M_fill_insertEN9__gnu_cxx17__normal_iteratorIPSA_SC_EEmRKSA_,"aG",@progbits,_ZNSt6vectorIN8nlohmann10basic_jsonISt3mapS_NSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEblmdSaNS0_14adl_serializerEEESaISA_EE14_M_fill_insertEN9__gnu_cxx17__normal_iteratorIPSA_SC_EEmRKSA_,comdat
	.align 4
	.align 4
.L1158:
	.long	.L1156-.L1158
	.long	.L1157-.L1158
	.long	.L1159-.L1158
	.long	.L1160-.L1158
	.long	.L1161-.L1158
	.long	.L1163-.L1158
	.long	.L1163-.L1158
	.long	.L1164-.L1158
	.section	.text._ZNSt6vectorIN8nlohmann10basic_jsonISt3mapS_NSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEblmdSaNS0_14adl_serializerEEESaISA_EE14_M_fill_insertEN9__gnu_cxx17__normal_iteratorIPSA_SC_EEmRKSA_,"axG",@progbits,_ZNSt6vectorIN8nlohmann10basic_jsonISt3mapS_NSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEblmdSaNS0_14adl_serializerEEESaISA_EE14_M_fill_insertEN9__gnu_cxx17__normal_iteratorIPSA_SC_EEmRKSA_,comdat
	.p2align 4,,10
	.p2align 3
.L1163:
	movq	8(%r14), %rax
	movq	%rax, 8(%r15)
.L1156:
	addq	$16, %r15
	addq	$16, %r14
	cmpq	%r14, 40(%rsp)
	jne	.L1207
.L1155:
	movq	80(%rsp), %rax
	movq	%r15, 8(%rax)
	movq	%rax, 8(%rbp)
	jmp	.L1138
	.p2align 4,,10
	.p2align 3
.L1387:
	movq	8(%r12), %rax
	movl	$32, %edi
	movq	%rax, 112(%rsp)
.LEHB177:
	call	_Znwm@PLT
.LEHE177:
	movq	%rax, %r13
	leaq	16(%rax), %rax
	movq	%r13, %rdi
	movq	%rax, 0(%r13)
	movq	112(%rsp), %rax
	movq	(%rax), %rsi
	movq	8(%rax), %rdx
	addq	%rsi, %rdx
.LEHB178:
	call	_ZNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEE12_M_constructIPcEEvT_S7_St20forward_iterator_tag.isra.67
.LEHE178:
.L1787:
	movq	80(%rsp), %rax
	movq	%r13, 8(%rax)
.L1383:
	addq	$16, 80(%rsp)
	addq	$16, %r12
	cmpq	%r12, 136(%rsp)
	jne	.L1409
.L1382:
	movq	200(%rsp), %rax
	movq	80(%rsp), %rcx
	movq	56(%rsp), %rdi
	movq	%rcx, 8(%rax)
	movq	%rax, 8(%rdi)
	jmp	.L1365
	.p2align 4,,10
	.p2align 3
.L1453:
	movq	8(%r15), %rdi
	movq	(%rdi), %rax
	leaq	16(%rdi), %rdx
	cmpq	%rdx, %rax
	je	.L1458
	movq	%rax, %rdi
	call	_ZdlPv@PLT
	jmp	.L1797
	.p2align 4,,10
	.p2align 3
.L993:
	movq	(%r15), %rdi
	leaq	16(%r15), %rdx
	cmpq	%rdx, %rdi
	je	.L998
	call	_ZdlPv@PLT
	jmp	.L998
	.p2align 4,,10
	.p2align 3
.L1009:
	movq	384(%rsp), %rax
	movq	%rax, 344(%rsp)
.L1002:
	movzbl	(%rbx), %eax
	movq	8(%rbx), %r12
	movq	344(%rsp), %rdx
	movb	%bpl, (%rbx)
	movq	%r12, 344(%rsp)
	cmpb	$2, %al
	movq	%rdx, 8(%rbx)
	je	.L1129
	cmpb	$3, %al
	je	.L1130
	cmpb	$1, %al
	jne	.L1128
	movq	16(%r12), %rsi
	movq	%r12, %rdi
	call	_ZNSt8_Rb_treeINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESt4pairIKS5_N8nlohmann10basic_jsonISt3mapSt6vectorS5_blmdSaNS8_14adl_serializerEEEESt10_Select1stISE_ESt4lessIvESaISE_EE8_M_eraseEPSt13_Rb_tree_nodeISE_E
	movq	%r12, %rdi
	call	_ZdlPv@PLT
.L1128:
	addq	$16, %rbx
	cmpq	%rbx, 24(%rsp)
	jne	.L1000
	jmp	.L1001
	.p2align 4,,10
	.p2align 3
.L1005:
	movl	$24, %edi
	movq	384(%rsp), %r12
.LEHB179:
	call	_Znwm@PLT
.LEHE179:
	movq	8(%r12), %r13
	subq	(%r12), %r13
	movq	%rax, %rcx
	movq	%rax, 80(%rsp)
	movq	$0, (%rcx)
	movq	$0, 8(%rcx)
	movq	$0, 16(%rcx)
	movq	%r13, %rax
	sarq	$4, %rax
	testq	%rax, %rax
	je	.L1820
	movabsq	$1152921504606846975, %rdx
	cmpq	%rdx, %rax
	ja	.L1821
	movq	%r13, %rdi
.LEHB180:
	call	_Znwm@PLT
.LEHE180:
	movq	%rax, 96(%rsp)
.L1017:
	movq	96(%rsp), %r15
	movq	80(%rsp), %rax
	addq	%r15, %r13
	movq	%r15, (%rax)
	movq	%r15, 8(%rax)
	movq	%r13, 16(%rax)
	movq	8(%r12), %rax
	movq	(%r12), %r12
	movq	%rax, 40(%rsp)
	cmpq	%r12, %rax
	je	.L1469
	.p2align 4,,10
	.p2align 3
.L1121:
	movzbl	(%r12), %eax
	movq	$0, 8(%r15)
	cmpb	$7, %al
	movb	%al, (%r15)
	ja	.L1020
	leaq	.L1022(%rip), %rdx
	movslq	(%rdx,%rax,4), %rax
	addq	%rdx, %rax
	jmp	*%rax
	.section	.rodata._ZNSt6vectorIN8nlohmann10basic_jsonISt3mapS_NSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEblmdSaNS0_14adl_serializerEEESaISA_EE14_M_fill_insertEN9__gnu_cxx17__normal_iteratorIPSA_SC_EEmRKSA_,"aG",@progbits,_ZNSt6vectorIN8nlohmann10basic_jsonISt3mapS_NSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEblmdSaNS0_14adl_serializerEEESaISA_EE14_M_fill_insertEN9__gnu_cxx17__normal_iteratorIPSA_SC_EEmRKSA_,comdat
	.align 4
	.align 4
.L1022:
	.long	.L1020-.L1022
	.long	.L1021-.L1022
	.long	.L1023-.L1022
	.long	.L1024-.L1022
	.long	.L1025-.L1022
	.long	.L1027-.L1022
	.long	.L1027-.L1022
	.long	.L1028-.L1022
	.section	.text._ZNSt6vectorIN8nlohmann10basic_jsonISt3mapS_NSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEblmdSaNS0_14adl_serializerEEESaISA_EE14_M_fill_insertEN9__gnu_cxx17__normal_iteratorIPSA_SC_EEmRKSA_,"axG",@progbits,_ZNSt6vectorIN8nlohmann10basic_jsonISt3mapS_NSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEblmdSaNS0_14adl_serializerEEESaISA_EE14_M_fill_insertEN9__gnu_cxx17__normal_iteratorIPSA_SC_EEmRKSA_,comdat
	.p2align 4,,10
	.p2align 3
.L1027:
	movq	8(%r12), %rax
	movq	%rax, 8(%r15)
.L1020:
	addq	$16, %r15
	addq	$16, %r12
	cmpq	%r12, 40(%rsp)
	jne	.L1121
.L1019:
	movq	80(%rsp), %rax
	movq	%r15, 8(%rax)
	movq	%rax, 344(%rsp)
	jmp	.L1002
	.p2align 4,,10
	.p2align 3
.L1003:
	movl	$48, %edi
	movq	384(%rsp), %r13
.LEHB181:
	call	_Znwm@PLT
.LEHE181:
	leaq	8(%rax), %rdx
	movq	$0, 16(%rax)
	movq	%rax, %r12
	movl	$0, 8(%rax)
	movq	$0, 40(%rax)
	movq	%rdx, 24(%rax)
	movq	%rdx, 32(%rax)
	movq	16(%r13), %rsi
	testq	%rsi, %rsi
	je	.L1775
	leaq	248(%rsp), %rcx
	movq	%rax, %rdi
	movq	%rax, 248(%rsp)
.LEHB182:
	call	_ZNSt8_Rb_treeINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESt4pairIKS5_N8nlohmann10basic_jsonISt3mapSt6vectorS5_blmdSaNS8_14adl_serializerEEEESt10_Select1stISE_ESt4lessIvESaISE_EE7_M_copyINSK_11_Alloc_nodeEEEPSt13_Rb_tree_nodeISE_EPKSO_PSt18_Rb_tree_node_baseRT_
.LEHE182:
	movq	%rax, %rcx
	jmp	.L1012
	.p2align 4,,10
	.p2align 3
.L1467:
	movq	%rdx, %rcx
.L1012:
	movq	16(%rcx), %rdx
	testq	%rdx, %rdx
	jne	.L1467
	movq	%rcx, 24(%r12)
	movq	%rax, %rcx
	jmp	.L1013
	.p2align 4,,10
	.p2align 3
.L1468:
	movq	%rdx, %rcx
.L1013:
	movq	24(%rcx), %rdx
	testq	%rdx, %rdx
	jne	.L1468
	movq	40(%r13), %rdx
	movq	%rcx, 32(%r12)
	movq	%rax, 16(%r12)
	movq	%rdx, 40(%r12)
	jmp	.L1775
	.p2align 4,,10
	.p2align 3
.L1007:
	movzbl	384(%rsp), %eax
	movb	%al, 344(%rsp)
	jmp	.L1002
	.p2align 4,,10
	.p2align 3
.L1006:
	movl	$32, %edi
	movq	384(%rsp), %r13
.LEHB183:
	call	_Znwm@PLT
.LEHE183:
	movq	%rax, %r12
	leaq	16(%rax), %rax
	movq	%r12, %rdi
	movq	%rax, (%r12)
	movq	0(%r13), %rsi
	movq	8(%r13), %rdx
	addq	%rsi, %rdx
.LEHB184:
	call	_ZNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEE12_M_constructIPcEEvT_S7_St20forward_iterator_tag.isra.67
.LEHE184:
.L1775:
	movq	%r12, 344(%rsp)
	jmp	.L1002
	.p2align 4,,10
	.p2align 3
.L1010:
	movsd	384(%rsp), %xmm0
	movsd	%xmm0, 344(%rsp)
	jmp	.L1002
	.p2align 4,,10
	.p2align 3
.L1028:
	movsd	8(%r12), %xmm0
	movsd	%xmm0, 8(%r15)
	jmp	.L1020
	.p2align 4,,10
	.p2align 3
.L1025:
	movzbl	8(%r12), %eax
	movb	%al, 8(%r15)
	jmp	.L1020
	.p2align 4,,10
	.p2align 3
.L1024:
	movl	$32, %edi
	movq	8(%r12), %r14
.LEHB185:
	call	_Znwm@PLT
.LEHE185:
	movq	%rax, %r13
	leaq	16(%rax), %rax
	movq	%r13, %rdi
	movq	%rax, 0(%r13)
	movq	(%r14), %rsi
	movq	8(%r14), %rdx
	addq	%rsi, %rdx
.LEHB186:
	call	_ZNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEE12_M_constructIPcEEvT_S7_St20forward_iterator_tag.isra.67
.LEHE186:
.L1773:
	movq	%r13, 8(%r15)
	jmp	.L1020
	.p2align 4,,10
	.p2align 3
.L1023:
	movl	$24, %edi
	movq	8(%r12), %r14
.LEHB187:
	call	_Znwm@PLT
.LEHE187:
	movq	8(%r14), %r13
	subq	(%r14), %r13
	movq	%rax, %rdi
	movq	%rax, 104(%rsp)
	movq	$0, (%rdi)
	movq	$0, 8(%rdi)
	movq	$0, 16(%rdi)
	movq	%r13, %rax
	sarq	$4, %rax
	testq	%rax, %rax
	je	.L1822
	movabsq	$1152921504606846975, %rdx
	cmpq	%rdx, %rax
	ja	.L1823
	movq	%r13, %rdi
.LEHB188:
	call	_Znwm@PLT
.LEHE188:
	movq	%rax, 128(%rsp)
.L1035:
	movq	128(%rsp), %rcx
	movq	104(%rsp), %rax
	addq	%rcx, %r13
	movq	%rcx, (%rax)
	movq	%rcx, 8(%rax)
	movq	%r13, 16(%rax)
	movq	8(%r14), %rsi
	movq	(%r14), %rax
	movq	%rsi, 56(%rsp)
	cmpq	%rax, %rsi
	je	.L1472
	movq	%rax, %r14
	movq	%rcx, 8(%rsp)
	.p2align 4,,10
	.p2align 3
.L1114:
	movzbl	(%r14), %eax
	movq	8(%rsp), %rsi
	movq	$0, 8(%rsi)
	cmpb	$7, %al
	movb	%al, (%rsi)
	ja	.L1038
	leaq	.L1040(%rip), %rsi
	movslq	(%rsi,%rax,4), %rax
	addq	%rsi, %rax
	jmp	*%rax
	.section	.rodata._ZNSt6vectorIN8nlohmann10basic_jsonISt3mapS_NSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEblmdSaNS0_14adl_serializerEEESaISA_EE14_M_fill_insertEN9__gnu_cxx17__normal_iteratorIPSA_SC_EEmRKSA_,"aG",@progbits,_ZNSt6vectorIN8nlohmann10basic_jsonISt3mapS_NSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEblmdSaNS0_14adl_serializerEEESaISA_EE14_M_fill_insertEN9__gnu_cxx17__normal_iteratorIPSA_SC_EEmRKSA_,comdat
	.align 4
	.align 4
.L1040:
	.long	.L1038-.L1040
	.long	.L1039-.L1040
	.long	.L1041-.L1040
	.long	.L1042-.L1040
	.long	.L1043-.L1040
	.long	.L1044-.L1040
	.long	.L1045-.L1040
	.long	.L1046-.L1040
	.section	.text._ZNSt6vectorIN8nlohmann10basic_jsonISt3mapS_NSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEblmdSaNS0_14adl_serializerEEESaISA_EE14_M_fill_insertEN9__gnu_cxx17__normal_iteratorIPSA_SC_EEmRKSA_,"axG",@progbits,_ZNSt6vectorIN8nlohmann10basic_jsonISt3mapS_NSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEblmdSaNS0_14adl_serializerEEESaISA_EE14_M_fill_insertEN9__gnu_cxx17__normal_iteratorIPSA_SC_EEmRKSA_,comdat
	.p2align 4,,10
	.p2align 3
.L1021:
	movl	$48, %edi
	movq	8(%r12), %r14
.LEHB189:
	call	_Znwm@PLT
.LEHE189:
	leaq	8(%rax), %rdx
	movq	$0, 16(%rax)
	movq	%rax, %r13
	movl	$0, 8(%rax)
	movq	$0, 40(%rax)
	movq	%rdx, 24(%rax)
	movq	%rdx, 32(%rax)
	movq	16(%r14), %rsi
	testq	%rsi, %rsi
	je	.L1773
	leaq	256(%rsp), %rcx
	movq	%rax, %rdi
	movq	%rax, 256(%rsp)
.LEHB190:
	call	_ZNSt8_Rb_treeINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESt4pairIKS5_N8nlohmann10basic_jsonISt3mapSt6vectorS5_blmdSaNS8_14adl_serializerEEEESt10_Select1stISE_ESt4lessIvESaISE_EE7_M_copyINSK_11_Alloc_nodeEEEPSt13_Rb_tree_nodeISE_EPKSO_PSt18_Rb_tree_node_baseRT_
.LEHE190:
	movq	%rax, %rcx
	jmp	.L1030
	.p2align 4,,10
	.p2align 3
.L1470:
	movq	%rdx, %rcx
.L1030:
	movq	16(%rcx), %rdx
	testq	%rdx, %rdx
	jne	.L1470
	movq	%rcx, 24(%r13)
	movq	%rax, %rcx
	jmp	.L1031
	.p2align 4,,10
	.p2align 3
.L1471:
	movq	%rdx, %rcx
.L1031:
	movq	24(%rcx), %rdx
	testq	%rdx, %rdx
	jne	.L1471
	movq	40(%r14), %rdx
	movq	%rcx, 32(%r13)
	movq	%rax, 16(%r13)
	movq	%rdx, 40(%r13)
	jmp	.L1773
	.p2align 4,,10
	.p2align 3
.L1046:
	movq	8(%rsp), %rax
	movsd	8(%r14), %xmm0
	movsd	%xmm0, 8(%rax)
	.p2align 4,,10
	.p2align 3
.L1038:
	addq	$16, 8(%rsp)
	addq	$16, %r14
	cmpq	%r14, 56(%rsp)
	jne	.L1114
.L1037:
	movq	104(%rsp), %rax
	movq	8(%rsp), %rdi
	movq	%rdi, 8(%rax)
	movq	%rax, 8(%r15)
	jmp	.L1020
	.p2align 4,,10
	.p2align 3
.L1045:
	movq	8(%r14), %rax
	movq	8(%rsp), %rsi
	movq	%rax, 8(%rsi)
	jmp	.L1038
	.p2align 4,,10
	.p2align 3
.L1044:
	movq	8(%r14), %rax
	movq	8(%rsp), %rdi
	movq	%rax, 8(%rdi)
	jmp	.L1038
	.p2align 4,,10
	.p2align 3
.L1043:
	movzbl	8(%r14), %eax
	movq	8(%rsp), %rsi
	movb	%al, 8(%rsi)
	jmp	.L1038
	.p2align 4,,10
	.p2align 3
.L1042:
	movq	8(%r14), %rax
	movl	$32, %edi
	movq	%rax, (%rsp)
.LEHB191:
	call	_Znwm@PLT
.LEHE191:
	movq	%rax, %r13
	leaq	16(%rax), %rax
	movq	%r13, %rdi
	movq	%rax, 0(%r13)
	movq	(%rsp), %rax
	movq	(%rax), %rsi
	movq	8(%rax), %rdx
	addq	%rsi, %rdx
.LEHB192:
	call	_ZNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEE12_M_constructIPcEEvT_S7_St20forward_iterator_tag.isra.67
.LEHE192:
.L1771:
	movq	8(%rsp), %rax
	movq	%r13, 8(%rax)
	jmp	.L1038
	.p2align 4,,10
	.p2align 3
.L1039:
	movq	8(%r14), %rax
	movl	$48, %edi
	movq	%rax, (%rsp)
.LEHB193:
	call	_Znwm@PLT
.LEHE193:
	leaq	8(%rax), %rdx
	movq	$0, 16(%rax)
	movq	%rax, %r13
	movl	$0, 8(%rax)
	movq	$0, 40(%rax)
	movq	%rdx, 24(%rax)
	movq	%rdx, 32(%rax)
	movq	(%rsp), %rax
	movq	16(%rax), %rsi
	testq	%rsi, %rsi
	je	.L1771
	leaq	264(%rsp), %rcx
	movq	%r13, %rdi
	movq	%r13, 264(%rsp)
.LEHB194:
	call	_ZNSt8_Rb_treeINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESt4pairIKS5_N8nlohmann10basic_jsonISt3mapSt6vectorS5_blmdSaNS8_14adl_serializerEEEESt10_Select1stISE_ESt4lessIvESaISE_EE7_M_copyINSK_11_Alloc_nodeEEEPSt13_Rb_tree_nodeISE_EPKSO_PSt18_Rb_tree_node_baseRT_
.LEHE194:
	movq	%rax, %rcx
	jmp	.L1048
	.p2align 4,,10
	.p2align 3
.L1473:
	movq	%rdx, %rcx
.L1048:
	movq	16(%rcx), %rdx
	testq	%rdx, %rdx
	jne	.L1473
	movq	%rcx, 24(%r13)
	movq	%rax, %rcx
	jmp	.L1049
	.p2align 4,,10
	.p2align 3
.L1474:
	movq	%rdx, %rcx
.L1049:
	movq	24(%rcx), %rdx
	testq	%rdx, %rdx
	jne	.L1474
	movq	(%rsp), %rdi
	movq	%rcx, 32(%r13)
	movq	40(%rdi), %rdx
	movq	%rax, 16(%r13)
	movq	%rdx, 40(%r13)
	jmp	.L1771
	.p2align 4,,10
	.p2align 3
.L1041:
	movq	8(%r14), %rax
	movl	$24, %edi
	movq	%rax, %r13
	movq	%rax, (%rsp)
.LEHB195:
	call	_Znwm@PLT
.LEHE195:
	movq	%rax, %rsi
	movq	%rax, 120(%rsp)
	movq	%r13, %rax
	movq	8(%r13), %r13
	subq	(%rax), %r13
	movq	$0, (%rsi)
	movq	$0, 8(%rsi)
	movq	$0, 16(%rsi)
	movq	%r13, %rax
	sarq	$4, %rax
	testq	%rax, %rax
	je	.L1824
	movabsq	$1152921504606846975, %rdx
	cmpq	%rdx, %rax
	ja	.L1825
	movq	%r13, %rdi
.LEHB196:
	call	_Znwm@PLT
.LEHE196:
	movq	%rax, 136(%rsp)
.L1053:
	movq	136(%rsp), %rcx
	movq	120(%rsp), %rax
	addq	%rcx, %r13
	movq	%rcx, (%rax)
	movq	%rcx, 8(%rax)
	movq	%r13, 16(%rax)
	movq	(%rsp), %rax
	movq	8(%rax), %rsi
	movq	(%rax), %rax
	cmpq	%rax, %rsi
	movq	%rsi, 64(%rsp)
	je	.L1475
	movq	%rax, (%rsp)
	movq	%rcx, 32(%rsp)
	.p2align 4,,10
	.p2align 3
.L1107:
	movq	(%rsp), %rax
	movq	32(%rsp), %rdi
	movzbl	(%rax), %eax
	movq	$0, 8(%rdi)
	cmpb	$7, %al
	movb	%al, (%rdi)
	ja	.L1056
	leaq	.L1058(%rip), %rcx
	movslq	(%rcx,%rax,4), %rax
	addq	%rcx, %rax
	jmp	*%rax
	.section	.rodata._ZNSt6vectorIN8nlohmann10basic_jsonISt3mapS_NSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEblmdSaNS0_14adl_serializerEEESaISA_EE14_M_fill_insertEN9__gnu_cxx17__normal_iteratorIPSA_SC_EEmRKSA_,"aG",@progbits,_ZNSt6vectorIN8nlohmann10basic_jsonISt3mapS_NSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEblmdSaNS0_14adl_serializerEEESaISA_EE14_M_fill_insertEN9__gnu_cxx17__normal_iteratorIPSA_SC_EEmRKSA_,comdat
	.align 4
	.align 4
.L1058:
	.long	.L1056-.L1058
	.long	.L1057-.L1058
	.long	.L1059-.L1058
	.long	.L1060-.L1058
	.long	.L1061-.L1058
	.long	.L1062-.L1058
	.long	.L1063-.L1058
	.long	.L1064-.L1058
	.section	.text._ZNSt6vectorIN8nlohmann10basic_jsonISt3mapS_NSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEblmdSaNS0_14adl_serializerEEESaISA_EE14_M_fill_insertEN9__gnu_cxx17__normal_iteratorIPSA_SC_EEmRKSA_,"axG",@progbits,_ZNSt6vectorIN8nlohmann10basic_jsonISt3mapS_NSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEblmdSaNS0_14adl_serializerEEESaISA_EE14_M_fill_insertEN9__gnu_cxx17__normal_iteratorIPSA_SC_EEmRKSA_,comdat
	.p2align 4,,10
	.p2align 3
.L1078:
	movq	16(%rsp), %rax
	movl	$32, %edi
	movq	8(%rax), %rax
	movq	%rax, 72(%rsp)
.LEHB197:
	call	_Znwm@PLT
.LEHE197:
	movq	%rax, %r13
	leaq	16(%rax), %rax
	movq	%r13, %rdi
	movq	%rax, 0(%r13)
	movq	72(%rsp), %rax
	movq	(%rax), %rsi
	movq	8(%rax), %rdx
	addq	%rsi, %rdx
.LEHB198:
	call	_ZNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEE12_M_constructIPcEEvT_S7_St20forward_iterator_tag.isra.67
.LEHE198:
.L1767:
	movq	48(%rsp), %rax
	movq	%r13, 8(%rax)
.L1074:
	addq	$16, 16(%rsp)
	addq	$16, 48(%rsp)
	movq	16(%rsp), %rax
	cmpq	%rax, 88(%rsp)
	jne	.L1100
.L1073:
	movq	144(%rsp), %rax
	movq	48(%rsp), %rdi
	movq	32(%rsp), %rsi
	movq	%rdi, 8(%rax)
	movq	%rax, 8(%rsi)
	.p2align 4,,10
	.p2align 3
.L1056:
	addq	$16, (%rsp)
	addq	$16, 32(%rsp)
	movq	(%rsp), %rax
	cmpq	%rax, 64(%rsp)
	jne	.L1107
.L1055:
	movq	32(%rsp), %rdi
	movq	120(%rsp), %rax
	movq	%rdi, 8(%rax)
	movq	8(%rsp), %rdi
	movq	%rax, 8(%rdi)
	jmp	.L1038
	.p2align 4,,10
	.p2align 3
.L1130:
	movq	(%r12), %rdi
	leaq	16(%r12), %rax
	cmpq	%rax, %rdi
	je	.L1135
	call	_ZdlPv@PLT
.L1135:
	movq	%r12, %rdi
	call	_ZdlPv@PLT
	jmp	.L1128
	.p2align 4,,10
	.p2align 3
.L1129:
	movq	8(%r12), %r13
	movq	(%r12), %rbp
	cmpq	%rbp, %r13
	je	.L1132
	.p2align 4,,10
	.p2align 3
.L1133:
	movzbl	0(%rbp), %esi
	leaq	8(%rbp), %rdi
	addq	$16, %rbp
	call	_ZN8nlohmann10basic_jsonISt3mapSt6vectorNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEblmdSaNS_14adl_serializerEE10json_value7destroyENS_6detail7value_tE
	cmpq	%rbp, %r13
	jne	.L1133
	movq	(%r12), %rbp
.L1132:
	testq	%rbp, %rbp
	je	.L1135
	movq	%rbp, %rdi
	call	_ZdlPv@PLT
	jmp	.L1135
	.p2align 4,,10
	.p2align 3
.L1299:
	movq	(%r14), %rdi
	leaq	16(%r14), %rax
	cmpq	%rax, %rdi
	je	.L1304
	call	_ZdlPv@PLT
.L1304:
	movq	%r14, %rdi
	call	_ZdlPv@PLT
	jmp	.L1297
	.p2align 4,,10
	.p2align 3
.L1298:
	movq	8(%r14), %r13
	movq	(%r14), %rbp
	cmpq	%rbp, %r13
	je	.L1301
	.p2align 4,,10
	.p2align 3
.L1302:
	movzbl	0(%rbp), %esi
	leaq	8(%rbp), %rdi
	addq	$16, %rbp
	call	_ZN8nlohmann10basic_jsonISt3mapSt6vectorNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEblmdSaNS_14adl_serializerEE10json_value7destroyENS_6detail7value_tE
	cmpq	%rbp, %r13
	jne	.L1302
	movq	(%r14), %rbp
.L1301:
	testq	%rbp, %rbp
	je	.L1304
	movq	%rbp, %rdi
	call	_ZdlPv@PLT
	jmp	.L1304
	.p2align 4,,10
	.p2align 3
.L1391:
	movq	80(%rsp), %rax
	movsd	8(%r12), %xmm0
	movsd	%xmm0, 8(%rax)
	jmp	.L1383
	.p2align 4,,10
	.p2align 3
.L1390:
	movq	8(%r12), %rax
	movq	80(%rsp), %rcx
	movq	%rax, 8(%rcx)
	jmp	.L1383
	.p2align 4,,10
	.p2align 3
.L1389:
	movq	8(%r12), %rax
	movq	80(%rsp), %rsi
	movq	%rax, 8(%rsi)
	jmp	.L1383
	.p2align 4,,10
	.p2align 3
.L1388:
	movzbl	8(%r12), %eax
	movq	80(%rsp), %rdi
	movb	%al, 8(%rdi)
	jmp	.L1383
	.p2align 4,,10
	.p2align 3
.L1386:
	movl	$24, %edi
	movq	8(%r12), %r13
.LEHB199:
	call	_Znwm@PLT
.LEHE199:
	movq	%rax, %rsi
	movq	%rax, 216(%rsp)
	movq	8(%r13), %rax
	subq	0(%r13), %rax
	movq	$0, (%rsi)
	movq	$0, 8(%rsi)
	movq	$0, 16(%rsi)
	movq	%rax, 112(%rsp)
	sarq	$4, %rax
	testq	%rax, %rax
	je	.L1826
	movabsq	$1152921504606846975, %rsi
	cmpq	%rsi, %rax
	ja	.L1827
	movq	112(%rsp), %rdi
.LEHB200:
	call	_Znwm@PLT
.LEHE200:
	movq	%rax, 224(%rsp)
.L1398:
	movq	224(%rsp), %rdi
	movq	112(%rsp), %rax
	movq	216(%rsp), %rsi
	addq	%rdi, %rax
	movq	%rdi, (%rsi)
	movq	%rdi, 8(%rsi)
	movq	%rax, 16(%rsi)
	movq	8(%r13), %rax
	movq	0(%r13), %r13
	movq	%rax, 160(%rsp)
	cmpq	%r13, %rax
	je	.L1518
	movq	%rdi, 112(%rsp)
	.p2align 4,,10
	.p2align 3
.L1401:
	movq	112(%rsp), %rdi
	movq	%r13, %rsi
.LEHB201:
	call	_ZN8nlohmann10basic_jsonISt3mapSt6vectorNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEblmdSaNS_14adl_serializerEEC1ERKSA_
.LEHE201:
	addq	$16, 112(%rsp)
	addq	$16, %r13
	cmpq	%r13, 160(%rsp)
	jne	.L1401
.L1400:
	movq	216(%rsp), %rax
	movq	112(%rsp), %rsi
	movq	80(%rsp), %rcx
	movq	%rsi, 8(%rax)
	movq	%rax, 8(%rcx)
	jmp	.L1383
	.p2align 4,,10
	.p2align 3
.L1384:
	movq	8(%r12), %rax
	movl	$48, %edi
	movq	%rax, 112(%rsp)
.LEHB202:
	call	_Znwm@PLT
.LEHE202:
	leaq	8(%rax), %rdx
	movq	$0, 16(%rax)
	movq	%rax, %r13
	movl	$0, 8(%rax)
	movq	$0, 40(%rax)
	movq	%rdx, 24(%rax)
	movq	%rdx, 32(%rax)
	movq	112(%rsp), %rax
	movq	16(%rax), %rsi
	testq	%rsi, %rsi
	je	.L1787
	movq	232(%rsp), %rcx
	movq	%r13, %rdi
	movq	%r13, 352(%rsp)
.LEHB203:
	call	_ZNSt8_Rb_treeINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESt4pairIKS5_N8nlohmann10basic_jsonISt3mapSt6vectorS5_blmdSaNS8_14adl_serializerEEEESt10_Select1stISE_ESt4lessIvESaISE_EE7_M_copyINSK_11_Alloc_nodeEEEPSt13_Rb_tree_nodeISE_EPKSO_PSt18_Rb_tree_node_baseRT_
.LEHE203:
	movq	%rax, %rcx
	jmp	.L1393
	.p2align 4,,10
	.p2align 3
.L1516:
	movq	%rdx, %rcx
.L1393:
	movq	16(%rcx), %rdx
	testq	%rdx, %rdx
	jne	.L1516
	movq	%rcx, 24(%r13)
	movq	%rax, %rcx
	jmp	.L1394
	.p2align 4,,10
	.p2align 3
.L1517:
	movq	%rdx, %rcx
.L1394:
	movq	24(%rcx), %rdx
	testq	%rdx, %rdx
	jne	.L1517
	movq	%rcx, 32(%r13)
	movq	112(%rsp), %rcx
	movq	40(%rcx), %rdx
	movq	%rax, 16(%r13)
	movq	%rdx, 40(%r13)
	jmp	.L1787
	.p2align 4,,10
	.p2align 3
.L1064:
	movq	(%rsp), %rax
	movsd	8(%rax), %xmm0
	movq	32(%rsp), %rax
	movsd	%xmm0, 8(%rax)
	jmp	.L1056
	.p2align 4,,10
	.p2align 3
.L1063:
	movq	(%rsp), %rax
	movq	32(%rsp), %rsi
	movq	8(%rax), %rax
	movq	%rax, 8(%rsi)
	jmp	.L1056
	.p2align 4,,10
	.p2align 3
.L1060:
	movq	(%rsp), %rax
	movl	$32, %edi
	movq	8(%rax), %rax
	movq	%rax, 16(%rsp)
.LEHB204:
	call	_Znwm@PLT
.LEHE204:
	movq	%rax, %r13
	leaq	16(%rax), %rax
	movq	%r13, %rdi
	movq	%rax, 0(%r13)
	movq	16(%rsp), %rax
	movq	(%rax), %rsi
	movq	8(%rax), %rdx
	addq	%rsi, %rdx
.LEHB205:
	call	_ZNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEE12_M_constructIPcEEvT_S7_St20forward_iterator_tag.isra.67
.LEHE205:
.L1769:
	movq	32(%rsp), %rax
	movq	%r13, 8(%rax)
	jmp	.L1056
	.p2align 4,,10
	.p2align 3
.L1059:
	movq	(%rsp), %rax
	movl	$24, %edi
	movq	8(%rax), %r13
.LEHB206:
	call	_Znwm@PLT
.LEHE206:
	movq	%rax, %rcx
	movq	%rax, 144(%rsp)
	movq	8(%r13), %rax
	subq	0(%r13), %rax
	movq	$0, (%rcx)
	movq	$0, 8(%rcx)
	movq	$0, 16(%rcx)
	movq	%rax, 16(%rsp)
	sarq	$4, %rax
	testq	%rax, %rax
	je	.L1828
	movabsq	$1152921504606846975, %rdi
	cmpq	%rdi, %rax
	ja	.L1829
	movq	16(%rsp), %rdi
.LEHB207:
	call	_Znwm@PLT
.LEHE207:
	movq	%rax, 160(%rsp)
.L1071:
	movq	160(%rsp), %rcx
	movq	16(%rsp), %rax
	movq	144(%rsp), %rdi
	addq	%rcx, %rax
	movq	%rcx, (%rdi)
	movq	%rcx, 8(%rdi)
	movq	%rax, 16(%rdi)
	movq	8(%r13), %rsi
	movq	0(%r13), %rax
	movq	%rsi, 88(%rsp)
	cmpq	%rax, %rsi
	je	.L1478
	movq	%rax, 16(%rsp)
	leaq	280(%rsp), %rax
	movq	%rcx, 48(%rsp)
	movq	%rax, 176(%rsp)
	.p2align 4,,10
	.p2align 3
.L1100:
	movq	16(%rsp), %rax
	movq	48(%rsp), %rsi
	movzbl	(%rax), %eax
	movq	$0, 8(%rsi)
	cmpb	$7, %al
	movb	%al, (%rsi)
	ja	.L1074
	leaq	.L1076(%rip), %rdi
	movslq	(%rdi,%rax,4), %rax
	addq	%rdi, %rax
	jmp	*%rax
	.section	.rodata._ZNSt6vectorIN8nlohmann10basic_jsonISt3mapS_NSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEblmdSaNS0_14adl_serializerEEESaISA_EE14_M_fill_insertEN9__gnu_cxx17__normal_iteratorIPSA_SC_EEmRKSA_,"aG",@progbits,_ZNSt6vectorIN8nlohmann10basic_jsonISt3mapS_NSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEblmdSaNS0_14adl_serializerEEESaISA_EE14_M_fill_insertEN9__gnu_cxx17__normal_iteratorIPSA_SC_EEmRKSA_,comdat
	.align 4
	.align 4
.L1076:
	.long	.L1074-.L1076
	.long	.L1075-.L1076
	.long	.L1077-.L1076
	.long	.L1078-.L1076
	.long	.L1079-.L1076
	.long	.L1080-.L1076
	.long	.L1081-.L1076
	.long	.L1082-.L1076
	.section	.text._ZNSt6vectorIN8nlohmann10basic_jsonISt3mapS_NSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEblmdSaNS0_14adl_serializerEEESaISA_EE14_M_fill_insertEN9__gnu_cxx17__normal_iteratorIPSA_SC_EEmRKSA_,"axG",@progbits,_ZNSt6vectorIN8nlohmann10basic_jsonISt3mapS_NSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEblmdSaNS0_14adl_serializerEEESaISA_EE14_M_fill_insertEN9__gnu_cxx17__normal_iteratorIPSA_SC_EEmRKSA_,comdat
	.p2align 4,,10
	.p2align 3
.L1057:
	movq	(%rsp), %rax
	movl	$48, %edi
	movq	8(%rax), %rax
	movq	%rax, 16(%rsp)
.LEHB208:
	call	_Znwm@PLT
.LEHE208:
	leaq	8(%rax), %rdx
	movq	$0, 16(%rax)
	movq	%rax, %r13
	movl	$0, 8(%rax)
	movq	$0, 40(%rax)
	movq	%rdx, 24(%rax)
	movq	%rdx, 32(%rax)
	movq	16(%rsp), %rax
	movq	16(%rax), %rsi
	testq	%rsi, %rsi
	je	.L1769
	leaq	272(%rsp), %rcx
	movq	%r13, %rdi
	movq	%r13, 272(%rsp)
.LEHB209:
	call	_ZNSt8_Rb_treeINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESt4pairIKS5_N8nlohmann10basic_jsonISt3mapSt6vectorS5_blmdSaNS8_14adl_serializerEEEESt10_Select1stISE_ESt4lessIvESaISE_EE7_M_copyINSK_11_Alloc_nodeEEEPSt13_Rb_tree_nodeISE_EPKSO_PSt18_Rb_tree_node_baseRT_
.LEHE209:
	movq	%rax, %rcx
	jmp	.L1066
	.p2align 4,,10
	.p2align 3
.L1476:
	movq	%rdx, %rcx
.L1066:
	movq	16(%rcx), %rdx
	testq	%rdx, %rdx
	jne	.L1476
	movq	%rcx, 24(%r13)
	movq	%rax, %rcx
	jmp	.L1067
	.p2align 4,,10
	.p2align 3
.L1477:
	movq	%rdx, %rcx
.L1067:
	movq	24(%rcx), %rdx
	testq	%rdx, %rdx
	jne	.L1477
	movq	16(%rsp), %rdi
	movq	%rcx, 32(%r13)
	movq	40(%rdi), %rdx
	movq	%rax, 16(%r13)
	movq	%rdx, 40(%r13)
	jmp	.L1769
	.p2align 4,,10
	.p2align 3
.L1062:
	movq	(%rsp), %rax
	movq	32(%rsp), %rcx
	movq	8(%rax), %rax
	movq	%rax, 8(%rcx)
	jmp	.L1056
	.p2align 4,,10
	.p2align 3
.L1061:
	movq	(%rsp), %rax
	movq	32(%rsp), %rdi
	movzbl	8(%rax), %eax
	movb	%al, 8(%rdi)
	jmp	.L1056
	.p2align 4,,10
	.p2align 3
.L1164:
	movsd	8(%r14), %xmm0
	movsd	%xmm0, 8(%r15)
	jmp	.L1156
	.p2align 4,,10
	.p2align 3
.L1161:
	movzbl	8(%r14), %eax
	movb	%al, 8(%r15)
	jmp	.L1156
	.p2align 4,,10
	.p2align 3
.L1160:
	movq	8(%r14), %rax
	movl	$32, %edi
	movq	%rax, (%rsp)
.LEHB210:
	call	_Znwm@PLT
.LEHE210:
	movq	%rax, %r13
	leaq	16(%rax), %rax
	movq	%r13, %rdi
	movq	%rax, 0(%r13)
	movq	(%rsp), %rax
	movq	(%rax), %rsi
	movq	8(%rax), %rdx
	addq	%rsi, %rdx
.LEHB211:
	call	_ZNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEE12_M_constructIPcEEvT_S7_St20forward_iterator_tag.isra.67
.LEHE211:
.L1778:
	movq	%r13, 8(%r15)
	jmp	.L1156
	.p2align 4,,10
	.p2align 3
.L1159:
	movl	$24, %edi
	movq	8(%r14), %r13
.LEHB212:
	call	_Znwm@PLT
.LEHE212:
	movq	%rax, %rsi
	movq	%rax, 88(%rsp)
	movq	8(%r13), %rax
	subq	0(%r13), %rax
	movq	$0, (%rsi)
	movq	$0, 8(%rsi)
	movq	$0, 16(%rsi)
	movq	%rax, (%rsp)
	sarq	$4, %rax
	testq	%rax, %rax
	je	.L1830
	movabsq	$1152921504606846975, %rcx
	cmpq	%rcx, %rax
	ja	.L1831
	movq	(%rsp), %rdi
.LEHB213:
	call	_Znwm@PLT
.LEHE213:
	movq	%rax, 112(%rsp)
.L1171:
	movq	112(%rsp), %rsi
	movq	(%rsp), %rax
	movq	88(%rsp), %rcx
	addq	%rsi, %rax
	movq	%rsi, (%rcx)
	movq	%rsi, 8(%rcx)
	movq	%rax, 16(%rcx)
	movq	8(%r13), %rcx
	movq	0(%r13), %rax
	movq	%rcx, 64(%rsp)
	cmpq	%rax, %rcx
	je	.L1488
	movq	%rax, (%rsp)
	leaq	304(%rsp), %rax
	movq	%rsi, 24(%rsp)
	movq	%rax, 128(%rsp)
	.p2align 4,,10
	.p2align 3
.L1200:
	movq	(%rsp), %rax
	movq	24(%rsp), %rdi
	movzbl	(%rax), %eax
	movq	$0, 8(%rdi)
	cmpb	$7, %al
	movb	%al, (%rdi)
	ja	.L1174
	leaq	.L1176(%rip), %rsi
	movslq	(%rsi,%rax,4), %rax
	addq	%rsi, %rax
	jmp	*%rax
	.section	.rodata._ZNSt6vectorIN8nlohmann10basic_jsonISt3mapS_NSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEblmdSaNS0_14adl_serializerEEESaISA_EE14_M_fill_insertEN9__gnu_cxx17__normal_iteratorIPSA_SC_EEmRKSA_,"aG",@progbits,_ZNSt6vectorIN8nlohmann10basic_jsonISt3mapS_NSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEblmdSaNS0_14adl_serializerEEESaISA_EE14_M_fill_insertEN9__gnu_cxx17__normal_iteratorIPSA_SC_EEmRKSA_,comdat
	.align 4
	.align 4
.L1176:
	.long	.L1174-.L1176
	.long	.L1175-.L1176
	.long	.L1177-.L1176
	.long	.L1178-.L1176
	.long	.L1179-.L1176
	.long	.L1180-.L1176
	.long	.L1181-.L1176
	.long	.L1182-.L1176
	.section	.text._ZNSt6vectorIN8nlohmann10basic_jsonISt3mapS_NSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEblmdSaNS0_14adl_serializerEEESaISA_EE14_M_fill_insertEN9__gnu_cxx17__normal_iteratorIPSA_SC_EEmRKSA_,"axG",@progbits,_ZNSt6vectorIN8nlohmann10basic_jsonISt3mapS_NSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEblmdSaNS0_14adl_serializerEEESaISA_EE14_M_fill_insertEN9__gnu_cxx17__normal_iteratorIPSA_SC_EEmRKSA_,comdat
	.p2align 4,,10
	.p2align 3
.L1247:
	movsd	8(%r14), %xmm0
	movsd	%xmm0, 8(%r15)
	jmp	.L1239
	.p2align 4,,10
	.p2align 3
.L1244:
	movzbl	8(%r14), %eax
	movb	%al, 8(%r15)
	jmp	.L1239
	.p2align 4,,10
	.p2align 3
.L1157:
	movq	8(%r14), %rax
	movl	$48, %edi
	movq	%rax, (%rsp)
.LEHB214:
	call	_Znwm@PLT
.LEHE214:
	leaq	8(%rax), %rdx
	movq	$0, 16(%rax)
	movq	%rax, %r13
	movl	$0, 8(%rax)
	movq	$0, 40(%rax)
	movq	%rdx, 24(%rax)
	movq	%rdx, 32(%rax)
	movq	(%rsp), %rax
	movq	16(%rax), %rsi
	testq	%rsi, %rsi
	je	.L1778
	leaq	296(%rsp), %rcx
	movq	%r13, %rdi
	movq	%r13, 296(%rsp)
.LEHB215:
	call	_ZNSt8_Rb_treeINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESt4pairIKS5_N8nlohmann10basic_jsonISt3mapSt6vectorS5_blmdSaNS8_14adl_serializerEEEESt10_Select1stISE_ESt4lessIvESaISE_EE7_M_copyINSK_11_Alloc_nodeEEEPSt13_Rb_tree_nodeISE_EPKSO_PSt18_Rb_tree_node_baseRT_
.LEHE215:
	movq	%rax, %rcx
	jmp	.L1166
	.p2align 4,,10
	.p2align 3
.L1486:
	movq	%rdx, %rcx
.L1166:
	movq	16(%rcx), %rdx
	testq	%rdx, %rdx
	jne	.L1486
	movq	%rcx, 24(%r13)
	movq	%rax, %rcx
	jmp	.L1167
	.p2align 4,,10
	.p2align 3
.L1487:
	movq	%rdx, %rcx
.L1167:
	movq	24(%rcx), %rdx
	testq	%rdx, %rdx
	jne	.L1487
	movq	(%rsp), %rsi
	movq	%rcx, 32(%r13)
	movq	40(%rsi), %rdx
	movq	%rax, 16(%r13)
	movq	%rdx, 40(%r13)
	jmp	.L1778
	.p2align 4,,10
	.p2align 3
.L1243:
	movq	8(%r14), %rax
	movl	$32, %edi
	movq	%rax, (%rsp)
.LEHB216:
	call	_Znwm@PLT
.LEHE216:
	movq	%rax, %r13
	leaq	16(%rax), %rax
	movq	%r13, %rdi
	movq	%rax, 0(%r13)
	movq	(%rsp), %rax
	movq	(%rax), %rsi
	movq	8(%rax), %rdx
	addq	%rsi, %rdx
.LEHB217:
	call	_ZNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEE12_M_constructIPcEEvT_S7_St20forward_iterator_tag.isra.67
.LEHE217:
.L1784:
	movq	%r13, 8(%r15)
	jmp	.L1239
	.p2align 4,,10
	.p2align 3
.L1242:
	movl	$24, %edi
	movq	8(%r14), %r13
.LEHB218:
	call	_Znwm@PLT
.LEHE218:
	movq	%rax, %rdi
	movq	%rax, 56(%rsp)
	movq	8(%r13), %rax
	subq	0(%r13), %rax
	movq	$0, (%rdi)
	movq	$0, 8(%rdi)
	movq	$0, 16(%rdi)
	movq	%rax, (%rsp)
	sarq	$4, %rax
	testq	%rax, %rax
	je	.L1832
	movabsq	$1152921504606846975, %rsi
	cmpq	%rsi, %rax
	ja	.L1833
	movq	(%rsp), %rdi
.LEHB219:
	call	_Znwm@PLT
.LEHE219:
	movq	%rax, 80(%rsp)
.L1254:
	movq	80(%rsp), %rdi
	movq	(%rsp), %rax
	movq	56(%rsp), %rsi
	addq	%rdi, %rax
	movq	%rdi, (%rsi)
	movq	%rdi, 8(%rsi)
	movq	%rax, 16(%rsi)
	movq	8(%r13), %rcx
	movq	0(%r13), %rax
	movq	%rcx, 32(%rsp)
	cmpq	%rax, %rcx
	je	.L1497
	movq	%rax, (%rsp)
	leaq	328(%rsp), %rax
	movq	%rdi, 8(%rsp)
	movq	%rax, 96(%rsp)
	.p2align 4,,10
	.p2align 3
.L1283:
	movq	(%rsp), %rax
	movq	8(%rsp), %rcx
	movzbl	(%rax), %eax
	movq	$0, 8(%rcx)
	cmpb	$7, %al
	movb	%al, (%rcx)
	ja	.L1257
	leaq	.L1259(%rip), %rcx
	movslq	(%rcx,%rax,4), %rax
	addq	%rcx, %rax
	jmp	*%rax
	.section	.rodata._ZNSt6vectorIN8nlohmann10basic_jsonISt3mapS_NSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEblmdSaNS0_14adl_serializerEEESaISA_EE14_M_fill_insertEN9__gnu_cxx17__normal_iteratorIPSA_SC_EEmRKSA_,"aG",@progbits,_ZNSt6vectorIN8nlohmann10basic_jsonISt3mapS_NSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEblmdSaNS0_14adl_serializerEEESaISA_EE14_M_fill_insertEN9__gnu_cxx17__normal_iteratorIPSA_SC_EEmRKSA_,comdat
	.align 4
	.align 4
.L1259:
	.long	.L1257-.L1259
	.long	.L1258-.L1259
	.long	.L1260-.L1259
	.long	.L1261-.L1259
	.long	.L1262-.L1259
	.long	.L1263-.L1259
	.long	.L1264-.L1259
	.long	.L1265-.L1259
	.section	.text._ZNSt6vectorIN8nlohmann10basic_jsonISt3mapS_NSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEblmdSaNS0_14adl_serializerEEESaISA_EE14_M_fill_insertEN9__gnu_cxx17__normal_iteratorIPSA_SC_EEmRKSA_,"axG",@progbits,_ZNSt6vectorIN8nlohmann10basic_jsonISt3mapS_NSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEblmdSaNS0_14adl_serializerEEESaISA_EE14_M_fill_insertEN9__gnu_cxx17__normal_iteratorIPSA_SC_EEmRKSA_,comdat
	.p2align 4,,10
	.p2align 3
.L1240:
	movq	8(%r14), %rax
	movl	$48, %edi
	movq	%rax, (%rsp)
.LEHB220:
	call	_Znwm@PLT
.LEHE220:
	leaq	8(%rax), %rdx
	movq	$0, 16(%rax)
	movq	%rax, %r13
	movl	$0, 8(%rax)
	movq	$0, 40(%rax)
	movq	%rdx, 24(%rax)
	movq	%rdx, 32(%rax)
	movq	(%rsp), %rax
	movq	16(%rax), %rsi
	testq	%rsi, %rsi
	je	.L1784
	leaq	320(%rsp), %rcx
	movq	%r13, %rdi
	movq	%r13, 320(%rsp)
.LEHB221:
	call	_ZNSt8_Rb_treeINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESt4pairIKS5_N8nlohmann10basic_jsonISt3mapSt6vectorS5_blmdSaNS8_14adl_serializerEEEESt10_Select1stISE_ESt4lessIvESaISE_EE7_M_copyINSK_11_Alloc_nodeEEEPSt13_Rb_tree_nodeISE_EPKSO_PSt18_Rb_tree_node_baseRT_
.LEHE221:
	movq	%rax, %rcx
	jmp	.L1249
	.p2align 4,,10
	.p2align 3
.L1495:
	movq	%rdx, %rcx
.L1249:
	movq	16(%rcx), %rdx
	testq	%rdx, %rdx
	jne	.L1495
	movq	%rcx, 24(%r13)
	movq	%rax, %rcx
	jmp	.L1250
	.p2align 4,,10
	.p2align 3
.L1496:
	movq	%rdx, %rcx
.L1250:
	movq	24(%rcx), %rdx
	testq	%rdx, %rdx
	jne	.L1496
	movq	(%rsp), %rdi
	movq	%rcx, 32(%r13)
	movq	40(%rdi), %rdx
	movq	%rax, 16(%r13)
	movq	%rdx, 40(%r13)
	jmp	.L1784
	.p2align 4,,10
	.p2align 3
.L963:
	movq	8(%rbp), %r12
	movl	$48, %edi
.LEHB222:
	call	_Znwm@PLT
.LEHE222:
	leaq	8(%rax), %rdx
	movq	$0, 16(%rax)
	movq	%rax, %rbp
	movl	$0, 8(%rax)
	movq	$0, 40(%rax)
	movq	%rdx, 24(%rax)
	movq	%rdx, 32(%rax)
	movq	16(%r12), %rsi
	testq	%rsi, %rsi
	je	.L1766
	leaq	352(%rsp), %rcx
	movq	%rax, %rdi
	movq	%rax, 352(%rsp)
.LEHB223:
	call	_ZNSt8_Rb_treeINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESt4pairIKS5_N8nlohmann10basic_jsonISt3mapSt6vectorS5_blmdSaNS8_14adl_serializerEEEESt10_Select1stISE_ESt4lessIvESaISE_EE7_M_copyINSK_11_Alloc_nodeEEEPSt13_Rb_tree_nodeISE_EPKSO_PSt18_Rb_tree_node_baseRT_
.LEHE223:
	movq	%rax, %rcx
	jmp	.L972
	.p2align 4,,10
	.p2align 3
.L1464:
	movq	%rdx, %rcx
.L972:
	movq	16(%rcx), %rdx
	testq	%rdx, %rdx
	jne	.L1464
	movq	%rcx, 24(%rbp)
	movq	%rax, %rcx
	jmp	.L973
	.p2align 4,,10
	.p2align 3
.L1465:
	movq	%rdx, %rcx
.L973:
	movq	24(%rcx), %rdx
	testq	%rdx, %rdx
	jne	.L1465
	movq	40(%r12), %rdx
	movq	%rcx, 32(%rbp)
	movq	%rax, 16(%rbp)
	movq	%rdx, 40(%rbp)
	jmp	.L1766
	.p2align 4,,10
	.p2align 3
.L965:
	movq	8(%rbp), %rbp
	movl	$24, %edi
	xorl	%r15d, %r15d
.LEHB224:
	call	_Znwm@PLT
.LEHE224:
	movq	%rax, %r13
	movq	8(%rbp), %r12
	subq	0(%rbp), %r12
	movq	$0, 0(%r13)
	movq	$0, 8(%r13)
	movq	$0, 16(%r13)
	movq	%r12, %rax
	sarq	$4, %rax
	testq	%rax, %rax
	je	.L976
	movabsq	$1152921504606846975, %rdx
	cmpq	%rdx, %rax
	ja	.L1834
	movq	%r12, %rdi
.LEHB225:
	call	_Znwm@PLT
.LEHE225:
	movq	%rax, %r15
.L976:
	addq	%r15, %r12
	movq	%r15, 0(%r13)
	movq	%r15, 8(%r13)
	movq	%r12, 16(%r13)
	movq	8(%rbp), %r14
	movq	%r15, %r12
	movq	0(%rbp), %rbp
	cmpq	%rbp, %r14
	je	.L978
	.p2align 4,,10
	.p2align 3
.L979:
	movq	%rbp, %rsi
	movq	%r12, %rdi
.LEHB226:
	call	_ZN8nlohmann10basic_jsonISt3mapSt6vectorNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEblmdSaNS_14adl_serializerEEC1ERKSA_
.LEHE226:
	addq	$16, %rbp
	addq	$16, %r12
	cmpq	%rbp, %r14
	jne	.L979
.L978:
	movq	16(%rsp), %rax
	movq	%r12, 8(%r13)
	movq	%r13, 384(%rsp)
	movq	8(%rax), %r12
	jmp	.L962
	.p2align 4,,10
	.p2align 3
.L967:
	movzbl	8(%rbp), %eax
	movb	%al, 384(%rsp)
	jmp	.L962
	.p2align 4,,10
	.p2align 3
.L970:
	movsd	8(%rbp), %xmm0
	movsd	%xmm0, 384(%rsp)
	jmp	.L962
	.p2align 4,,10
	.p2align 3
.L1075:
	movq	16(%rsp), %rax
	movl	$48, %edi
	movq	8(%rax), %rax
	movq	%rax, 72(%rsp)
.LEHB227:
	call	_Znwm@PLT
.LEHE227:
	leaq	8(%rax), %rdx
	movq	$0, 16(%rax)
	movq	%rax, %r13
	movl	$0, 8(%rax)
	movq	$0, 40(%rax)
	movq	%rdx, 24(%rax)
	movq	%rdx, 32(%rax)
	movq	72(%rsp), %rax
	movq	16(%rax), %rsi
	testq	%rsi, %rsi
	je	.L1767
	movq	176(%rsp), %rcx
	movq	%r13, %rdi
	movq	%r13, 280(%rsp)
.LEHB228:
	call	_ZNSt8_Rb_treeINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESt4pairIKS5_N8nlohmann10basic_jsonISt3mapSt6vectorS5_blmdSaNS8_14adl_serializerEEEESt10_Select1stISE_ESt4lessIvESaISE_EE7_M_copyINSK_11_Alloc_nodeEEEPSt13_Rb_tree_nodeISE_EPKSO_PSt18_Rb_tree_node_baseRT_
.LEHE228:
	movq	%rax, %rcx
	jmp	.L1084
	.p2align 4,,10
	.p2align 3
.L1479:
	movq	%rdx, %rcx
.L1084:
	movq	16(%rcx), %rdx
	testq	%rdx, %rdx
	jne	.L1479
	movq	%rcx, 24(%r13)
	movq	%rax, %rcx
	jmp	.L1085
	.p2align 4,,10
	.p2align 3
.L1480:
	movq	%rdx, %rcx
.L1085:
	movq	24(%rcx), %rdx
	testq	%rdx, %rdx
	jne	.L1480
	movq	72(%rsp), %rsi
	movq	%rcx, 32(%r13)
	movq	40(%rsi), %rdx
	movq	%rax, 16(%r13)
	movq	%rdx, 40(%r13)
	jmp	.L1767
	.p2align 4,,10
	.p2align 3
.L1077:
	movq	16(%rsp), %rax
	movl	$24, %edi
	movq	8(%rax), %r13
.LEHB229:
	call	_Znwm@PLT
.LEHE229:
	movq	%rax, %rcx
	movq	%rax, 152(%rsp)
	movq	8(%r13), %rax
	subq	0(%r13), %rax
	movq	$0, (%rcx)
	movq	$0, 8(%rcx)
	movq	$0, 16(%rcx)
	movq	%rax, 72(%rsp)
	sarq	$4, %rax
	testq	%rax, %rax
	je	.L1835
	movabsq	$1152921504606846975, %rdi
	cmpq	%rdi, %rax
	ja	.L1836
	movq	72(%rsp), %rdi
.LEHB230:
	call	_Znwm@PLT
.LEHE230:
	movq	%rax, 168(%rsp)
.L1089:
	movq	168(%rsp), %rcx
	movq	72(%rsp), %rax
	movq	152(%rsp), %rdi
	addq	%rcx, %rax
	movq	%rcx, (%rdi)
	movq	%rcx, 8(%rdi)
	movq	%rax, 16(%rdi)
	movq	8(%r13), %rax
	movq	0(%r13), %r13
	movq	%rax, 112(%rsp)
	cmpq	%r13, %rax
	je	.L1481
	movq	%rcx, 72(%rsp)
	.p2align 4,,10
	.p2align 3
.L1092:
	movq	72(%rsp), %rdi
	movq	%r13, %rsi
.LEHB231:
	call	_ZN8nlohmann10basic_jsonISt3mapSt6vectorNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEblmdSaNS_14adl_serializerEEC1ERKSA_
.LEHE231:
	addq	$16, 72(%rsp)
	addq	$16, %r13
	cmpq	%r13, 112(%rsp)
	jne	.L1092
.L1091:
	movq	152(%rsp), %rax
	movq	72(%rsp), %rcx
	movq	48(%rsp), %rsi
	movq	%rcx, 8(%rax)
	movq	%rax, 8(%rsi)
	jmp	.L1074
	.p2align 4,,10
	.p2align 3
.L1082:
	movq	16(%rsp), %rax
	movsd	8(%rax), %xmm0
	movq	48(%rsp), %rax
	movsd	%xmm0, 8(%rax)
	jmp	.L1074
	.p2align 4,,10
	.p2align 3
.L1081:
	movq	16(%rsp), %rax
	movq	48(%rsp), %rsi
	movq	8(%rax), %rax
	movq	%rax, 8(%rsi)
	jmp	.L1074
	.p2align 4,,10
	.p2align 3
.L1080:
	movq	16(%rsp), %rax
	movq	48(%rsp), %rdi
	movq	8(%rax), %rax
	movq	%rax, 8(%rdi)
	jmp	.L1074
	.p2align 4,,10
	.p2align 3
.L1079:
	movq	16(%rsp), %rax
	movq	48(%rsp), %rcx
	movzbl	8(%rax), %eax
	movb	%al, 8(%rcx)
	jmp	.L1074
	.p2align 4,,10
	.p2align 3
.L1178:
	movq	(%rsp), %rax
	movl	$32, %edi
	movq	8(%rax), %rax
	movq	%rax, 48(%rsp)
.LEHB232:
	call	_Znwm@PLT
.LEHE232:
	movq	%rax, %r13
	leaq	16(%rax), %rax
	movq	%r13, %rdi
	movq	%rax, 0(%r13)
	movq	48(%rsp), %rax
	movq	(%rax), %rsi
	movq	8(%rax), %rdx
	addq	%rsi, %rdx
.LEHB233:
	call	_ZNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEE12_M_constructIPcEEvT_S7_St20forward_iterator_tag.isra.67
.LEHE233:
.L1776:
	movq	24(%rsp), %rax
	movq	%r13, 8(%rax)
.L1174:
	addq	$16, (%rsp)
	addq	$16, 24(%rsp)
	movq	(%rsp), %rax
	cmpq	%rax, 64(%rsp)
	jne	.L1200
.L1173:
	movq	88(%rsp), %rax
	movq	24(%rsp), %rdi
	movq	%rdi, 8(%rax)
	movq	%rax, 8(%r15)
	jmp	.L1156
	.p2align 4,,10
	.p2align 3
.L1177:
	movq	(%rsp), %rax
	movl	$24, %edi
	movq	8(%rax), %r13
.LEHB234:
	call	_Znwm@PLT
.LEHE234:
	movq	%rax, %rdi
	movq	%rax, 104(%rsp)
	movq	8(%r13), %rax
	subq	0(%r13), %rax
	movq	$0, (%rdi)
	movq	$0, 8(%rdi)
	movq	$0, 16(%rdi)
	movq	%rax, 48(%rsp)
	sarq	$4, %rax
	testq	%rax, %rax
	je	.L1837
	movabsq	$1152921504606846975, %rsi
	cmpq	%rsi, %rax
	ja	.L1838
	movq	48(%rsp), %rdi
.LEHB235:
	call	_Znwm@PLT
.LEHE235:
	movq	%rax, 120(%rsp)
.L1189:
	movq	120(%rsp), %rdi
	movq	48(%rsp), %rax
	movq	104(%rsp), %rsi
	addq	%rdi, %rax
	movq	%rdi, (%rsi)
	movq	%rdi, 8(%rsi)
	movq	%rax, 16(%rsi)
	movq	8(%r13), %rax
	movq	0(%r13), %r13
	movq	%rax, 72(%rsp)
	cmpq	%r13, %rax
	je	.L1491
	movq	%rdi, 48(%rsp)
	.p2align 4,,10
	.p2align 3
.L1192:
	movq	48(%rsp), %rdi
	movq	%r13, %rsi
.LEHB236:
	call	_ZN8nlohmann10basic_jsonISt3mapSt6vectorNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEblmdSaNS_14adl_serializerEEC1ERKSA_
.LEHE236:
	addq	$16, 48(%rsp)
	addq	$16, %r13
	cmpq	%r13, 72(%rsp)
	jne	.L1192
.L1191:
	movq	104(%rsp), %rax
	movq	48(%rsp), %rcx
	movq	24(%rsp), %rdi
	movq	%rcx, 8(%rax)
	movq	%rax, 8(%rdi)
	jmp	.L1174
	.p2align 4,,10
	.p2align 3
.L1175:
	movq	(%rsp), %rax
	movl	$48, %edi
	movq	8(%rax), %rax
	movq	%rax, 48(%rsp)
.LEHB237:
	call	_Znwm@PLT
.LEHE237:
	leaq	8(%rax), %rdx
	movq	$0, 16(%rax)
	movq	%rax, %r13
	movl	$0, 8(%rax)
	movq	$0, 40(%rax)
	movq	%rdx, 24(%rax)
	movq	%rdx, 32(%rax)
	movq	48(%rsp), %rax
	movq	16(%rax), %rsi
	testq	%rsi, %rsi
	je	.L1776
	movq	128(%rsp), %rcx
	movq	%r13, %rdi
	movq	%r13, 304(%rsp)
.LEHB238:
	call	_ZNSt8_Rb_treeINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESt4pairIKS5_N8nlohmann10basic_jsonISt3mapSt6vectorS5_blmdSaNS8_14adl_serializerEEEESt10_Select1stISE_ESt4lessIvESaISE_EE7_M_copyINSK_11_Alloc_nodeEEEPSt13_Rb_tree_nodeISE_EPKSO_PSt18_Rb_tree_node_baseRT_
.LEHE238:
	movq	%rax, %rcx
	jmp	.L1184
	.p2align 4,,10
	.p2align 3
.L1489:
	movq	%rdx, %rcx
.L1184:
	movq	16(%rcx), %rdx
	testq	%rdx, %rdx
	jne	.L1489
	movq	%rcx, 24(%r13)
	movq	%rax, %rcx
	jmp	.L1185
	.p2align 4,,10
	.p2align 3
.L1490:
	movq	%rdx, %rcx
.L1185:
	movq	24(%rcx), %rdx
	testq	%rdx, %rdx
	jne	.L1490
	movq	48(%rsp), %rdi
	movq	%rcx, 32(%r13)
	movq	40(%rdi), %rdx
	movq	%rax, 16(%r13)
	movq	%rdx, 40(%r13)
	jmp	.L1776
	.p2align 4,,10
	.p2align 3
.L1182:
	movq	(%rsp), %rax
	movsd	8(%rax), %xmm0
	movq	24(%rsp), %rax
	movsd	%xmm0, 8(%rax)
	jmp	.L1174
	.p2align 4,,10
	.p2align 3
.L1181:
	movq	(%rsp), %rax
	movq	24(%rsp), %rdi
	movq	8(%rax), %rax
	movq	%rax, 8(%rdi)
	jmp	.L1174
	.p2align 4,,10
	.p2align 3
.L1261:
	movq	(%rsp), %rax
	movl	$32, %edi
	movq	8(%rax), %rax
	movq	%rax, 24(%rsp)
.LEHB239:
	call	_Znwm@PLT
.LEHE239:
	movq	%rax, %r13
	leaq	16(%rax), %rax
	movq	%r13, %rdi
	movq	%rax, 0(%r13)
	movq	24(%rsp), %rax
	movq	(%rax), %rsi
	movq	8(%rax), %rdx
	addq	%rsi, %rdx
.LEHB240:
	call	_ZNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEE12_M_constructIPcEEvT_S7_St20forward_iterator_tag.isra.67
.LEHE240:
.L1782:
	movq	8(%rsp), %rax
	movq	%r13, 8(%rax)
.L1257:
	addq	$16, (%rsp)
	addq	$16, 8(%rsp)
	movq	(%rsp), %rax
	cmpq	%rax, 32(%rsp)
	jne	.L1283
.L1256:
	movq	56(%rsp), %rax
	movq	8(%rsp), %rcx
	movq	%rcx, 8(%rax)
	movq	%rax, 8(%r15)
	jmp	.L1239
	.p2align 4,,10
	.p2align 3
.L1260:
	movq	(%rsp), %rax
	movl	$24, %edi
	movq	8(%rax), %r13
.LEHB241:
	call	_Znwm@PLT
.LEHE241:
	movq	%rax, %rsi
	movq	%rax, 72(%rsp)
	movq	8(%r13), %rax
	subq	0(%r13), %rax
	movq	$0, (%rsi)
	movq	$0, 8(%rsi)
	movq	$0, 16(%rsi)
	movq	%rax, 24(%rsp)
	sarq	$4, %rax
	testq	%rax, %rax
	je	.L1839
	movabsq	$1152921504606846975, %rcx
	cmpq	%rcx, %rax
	ja	.L1840
	movq	24(%rsp), %rdi
.LEHB242:
	call	_Znwm@PLT
.LEHE242:
	movq	%rax, 88(%rsp)
.L1272:
	movq	88(%rsp), %rsi
	movq	24(%rsp), %rax
	movq	72(%rsp), %rcx
	addq	%rsi, %rax
	movq	%rsi, (%rcx)
	movq	%rsi, 8(%rcx)
	movq	%rax, 16(%rcx)
	movq	8(%r13), %rax
	movq	0(%r13), %r13
	movq	%rax, 40(%rsp)
	cmpq	%r13, %rax
	je	.L1500
	movq	%rsi, 24(%rsp)
	.p2align 4,,10
	.p2align 3
.L1275:
	movq	24(%rsp), %rdi
	movq	%r13, %rsi
.LEHB243:
	call	_ZN8nlohmann10basic_jsonISt3mapSt6vectorNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEblmdSaNS_14adl_serializerEEC1ERKSA_
.LEHE243:
	addq	$16, 24(%rsp)
	addq	$16, %r13
	cmpq	%r13, 40(%rsp)
	jne	.L1275
.L1274:
	movq	72(%rsp), %rax
	movq	24(%rsp), %rsi
	movq	8(%rsp), %rcx
	movq	%rsi, 8(%rax)
	movq	%rax, 8(%rcx)
	jmp	.L1257
	.p2align 4,,10
	.p2align 3
.L1258:
	movq	(%rsp), %rax
	movl	$48, %edi
	movq	8(%rax), %rax
	movq	%rax, 24(%rsp)
.LEHB244:
	call	_Znwm@PLT
.LEHE244:
	leaq	8(%rax), %rdx
	movq	$0, 16(%rax)
	movq	%rax, %r13
	movl	$0, 8(%rax)
	movq	$0, 40(%rax)
	movq	%rdx, 24(%rax)
	movq	%rdx, 32(%rax)
	movq	24(%rsp), %rax
	movq	16(%rax), %rsi
	testq	%rsi, %rsi
	je	.L1782
	movq	96(%rsp), %rcx
	movq	%r13, %rdi
	movq	%r13, 328(%rsp)
.LEHB245:
	call	_ZNSt8_Rb_treeINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESt4pairIKS5_N8nlohmann10basic_jsonISt3mapSt6vectorS5_blmdSaNS8_14adl_serializerEEEESt10_Select1stISE_ESt4lessIvESaISE_EE7_M_copyINSK_11_Alloc_nodeEEEPSt13_Rb_tree_nodeISE_EPKSO_PSt18_Rb_tree_node_baseRT_
.LEHE245:
	movq	%rax, %rcx
	jmp	.L1267
	.p2align 4,,10
	.p2align 3
.L1498:
	movq	%rdx, %rcx
.L1267:
	movq	16(%rcx), %rdx
	testq	%rdx, %rdx
	jne	.L1498
	movq	%rcx, 24(%r13)
	movq	%rax, %rcx
	jmp	.L1268
	.p2align 4,,10
	.p2align 3
.L1499:
	movq	%rdx, %rcx
.L1268:
	movq	24(%rcx), %rdx
	testq	%rdx, %rdx
	jne	.L1499
	movq	%rcx, 32(%r13)
	movq	24(%rsp), %rcx
	movq	40(%rcx), %rdx
	movq	%rax, 16(%r13)
	movq	%rdx, 40(%r13)
	jmp	.L1782
	.p2align 4,,10
	.p2align 3
.L1265:
	movq	(%rsp), %rax
	movsd	8(%rax), %xmm0
	movq	8(%rsp), %rax
	movsd	%xmm0, 8(%rax)
	jmp	.L1257
	.p2align 4,,10
	.p2align 3
.L1264:
	movq	(%rsp), %rax
	movq	8(%rsp), %rcx
	movq	8(%rax), %rax
	movq	%rax, 8(%rcx)
	jmp	.L1257
	.p2align 4,,10
	.p2align 3
.L1180:
	movq	(%rsp), %rax
	movq	24(%rsp), %rcx
	movq	8(%rax), %rax
	movq	%rax, 8(%rcx)
	jmp	.L1174
	.p2align 4,,10
	.p2align 3
.L1179:
	movq	(%rsp), %rax
	movq	24(%rsp), %rsi
	movzbl	8(%rax), %eax
	movb	%al, 8(%rsi)
	jmp	.L1174
	.p2align 4,,10
	.p2align 3
.L1263:
	movq	(%rsp), %rax
	movq	8(%rsp), %rsi
	movq	8(%rax), %rax
	movq	%rax, 8(%rsi)
	jmp	.L1257
	.p2align 4,,10
	.p2align 3
.L1262:
	movq	(%rsp), %rax
	movq	8(%rsp), %rdi
	movzbl	8(%rax), %eax
	movb	%al, 8(%rdi)
	jmp	.L1257
	.p2align 4,,10
	.p2align 3
.L1506:
	movq	144(%rsp), %r15
	jmp	.L1328
.L1503:
	movq	$0, 64(%rsp)
	jmp	.L1310
.L1509:
	movq	168(%rsp), %rax
	movq	%rax, 32(%rsp)
	jmp	.L1346
.L1469:
	movq	96(%rsp), %r15
	jmp	.L1019
.L1807:
	movq	$0, 144(%rsp)
	jmp	.L1326
.L1512:
	movq	192(%rsp), %rax
	movq	%rax, 56(%rsp)
	jmp	.L1364
.L1813:
	movq	16(%rsp), %rax
	movq	%rcx, 8(%rax)
	jmp	.L1001
.L1472:
	movq	128(%rsp), %rax
	movq	%rax, 8(%rsp)
	jmp	.L1037
.L1809:
	movq	$0, 168(%rsp)
	jmp	.L1344
.L1820:
	movq	$0, 96(%rsp)
	jmp	.L1017
.L1515:
	movq	208(%rsp), %rax
	movq	%rax, 80(%rsp)
	jmp	.L1382
.L1475:
	movq	136(%rsp), %rax
	movq	%rax, 32(%rsp)
	jmp	.L1055
.L1485:
	movq	96(%rsp), %r15
	jmp	.L1155
.L1494:
	movq	64(%rsp), %r15
	jmp	.L1238
.L1811:
	movq	$0, 192(%rsp)
	jmp	.L1362
.L1822:
	movq	$0, 128(%rsp)
	jmp	.L1035
.L1518:
	movq	224(%rsp), %rax
	movq	%rax, 112(%rsp)
	jmp	.L1400
.L1478:
	movq	160(%rsp), %rax
	movq	%rax, 48(%rsp)
	jmp	.L1073
.L1488:
	movq	112(%rsp), %rax
	movq	%rax, 24(%rsp)
	jmp	.L1173
.L1497:
	movq	80(%rsp), %rax
	movq	%rax, 8(%rsp)
	jmp	.L1256
.L1814:
	movq	$0, 208(%rsp)
	jmp	.L1380
.L1824:
	movq	$0, 136(%rsp)
	jmp	.L1053
.L1816:
	movq	$0, 64(%rsp)
	jmp	.L1236
.L1818:
	movq	$0, 96(%rsp)
	jmp	.L1153
.L1481:
	movq	168(%rsp), %rax
	movq	%rax, 72(%rsp)
	jmp	.L1091
.L1500:
	movq	88(%rsp), %rax
	movq	%rax, 24(%rsp)
	jmp	.L1274
.L1491:
	movq	120(%rsp), %rax
	movq	%rax, 48(%rsp)
	jmp	.L1191
.L1826:
	movq	$0, 224(%rsp)
	jmp	.L1398
.L1830:
	movq	$0, 112(%rsp)
	jmp	.L1171
.L1832:
	movq	$0, 80(%rsp)
	jmp	.L1254
.L1828:
	movq	$0, 160(%rsp)
	jmp	.L1071
.L1839:
	movq	$0, 88(%rsp)
	jmp	.L1272
.L1835:
	movq	$0, 168(%rsp)
	jmp	.L1089
.L1837:
	movq	$0, 120(%rsp)
	jmp	.L1189
.L1801:
	call	__stack_chk_fail@PLT
.L1802:
	leaq	.LC19(%rip), %rdi
.LEHB246:
	call	_ZSt20__throw_length_errorPKc@PLT
.LEHE246:
.L1562:
	movq	%rax, %rbx
.L1198:
	movq	104(%rsp), %rdi
	call	_ZdlPv@PLT
.L1187:
	movq	%rbx, %rdi
	call	__cxa_begin_catch@PLT
	movq	112(%rsp), %rbx
.L1202:
	cmpq	24(%rsp), %rbx
	je	.L1201
	movzbl	(%rbx), %esi
	leaq	8(%rbx), %rdi
	addq	$16, %rbx
	call	_ZN8nlohmann10basic_jsonISt3mapSt6vectorNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEblmdSaNS_14adl_serializerEE10json_value7destroyENS_6detail7value_tE
	jmp	.L1202
.L1827:
.LEHB247:
	call	_ZSt17__throw_bad_allocv@PLT
.LEHE247:
.L1201:
.LEHB248:
	call	__cxa_rethrow@PLT
.LEHE248:
.L1605:
.L1788:
	movq	%r13, %rdi
	movq	%rax, %rbp
	call	_ZdlPv@PLT
.L1396:
	movq	%rbp, %rdi
	call	__cxa_begin_catch@PLT
	movq	208(%rsp), %rbp
.L1411:
	cmpq	80(%rsp), %rbp
	je	.L1410
	movzbl	0(%rbp), %esi
	leaq	8(%rbp), %rdi
	addq	$16, %rbp
	call	_ZN8nlohmann10basic_jsonISt3mapSt6vectorNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEblmdSaNS_14adl_serializerEE10json_value7destroyENS_6detail7value_tE
	jmp	.L1411
.L1600:
	movq	%rax, %rbp
	jmp	.L1396
.L1559:
	movq	%rax, %rbx
	call	__cxa_end_catch@PLT
	movq	88(%rsp), %rax
	movq	(%rax), %rdi
	testq	%rdi, %rdi
	je	.L1205
	call	_ZdlPv@PLT
.L1205:
	movq	88(%rsp), %rdi
	call	_ZdlPv@PLT
.L1169:
	movq	%rbx, %rdi
	call	__cxa_begin_catch@PLT
	movq	96(%rsp), %rbx
.L1209:
	cmpq	%r15, %rbx
	je	.L1208
	movzbl	(%rbx), %esi
	leaq	8(%rbx), %rdi
	addq	$16, %rbx
	call	_ZN8nlohmann10basic_jsonISt3mapSt6vectorNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEblmdSaNS_14adl_serializerEE10json_value7destroyENS_6detail7value_tE
	jmp	.L1209
.L1410:
.LEHB249:
	call	__cxa_rethrow@PLT
.LEHE249:
.L1208:
.LEHB250:
	call	__cxa_rethrow@PLT
.LEHE250:
.L1599:
	movq	%rax, %rbp
	call	__cxa_end_catch@PLT
	movq	200(%rsp), %rax
	movq	(%rax), %rdi
	testq	%rdi, %rdi
	je	.L1414
	call	_ZdlPv@PLT
.L1414:
	movq	200(%rsp), %rdi
	call	_ZdlPv@PLT
.L1378:
	movq	%rbp, %rdi
	call	__cxa_begin_catch@PLT
	movq	192(%rsp), %rbp
.L1418:
	cmpq	56(%rsp), %rbp
	je	.L1417
	movzbl	0(%rbp), %esi
	leaq	8(%rbp), %rdi
	addq	$16, %rbp
	call	_ZN8nlohmann10basic_jsonISt3mapSt6vectorNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEblmdSaNS_14adl_serializerEE10json_value7destroyENS_6detail7value_tE
	jmp	.L1418
.L1555:
	movq	%rax, %rbx
	call	__cxa_end_catch@PLT
	movq	80(%rsp), %rax
	movq	(%rax), %rdi
	testq	%rdi, %rdi
	je	.L1212
	call	_ZdlPv@PLT
.L1212:
	movq	80(%rsp), %rdi
	call	_ZdlPv@PLT
.L1151:
	movq	%rbx, %rdi
	call	__cxa_begin_catch@PLT
.L1218:
	cmpq	%rbp, %r12
	je	.L1217
	movzbl	(%r12), %esi
	leaq	8(%r12), %rdi
	addq	$16, %r12
	call	_ZN8nlohmann10basic_jsonISt3mapSt6vectorNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEblmdSaNS_14adl_serializerEE10json_value7destroyENS_6detail7value_tE
	jmp	.L1218
.L1417:
.LEHB251:
	call	__cxa_rethrow@PLT
.LEHE251:
.L1217:
.LEHB252:
	call	__cxa_rethrow@PLT
.LEHE252:
.L1595:
	movq	%rax, %rbp
	call	__cxa_end_catch@PLT
	movq	176(%rsp), %rax
	movq	(%rax), %rdi
	testq	%rdi, %rdi
	je	.L1421
	call	_ZdlPv@PLT
.L1421:
	movq	176(%rsp), %rdi
	call	_ZdlPv@PLT
.L1360:
	movq	%rbp, %rdi
	call	__cxa_begin_catch@PLT
	movq	168(%rsp), %rbp
.L1425:
	cmpq	32(%rsp), %rbp
	je	.L1424
	movzbl	0(%rbp), %esi
	leaq	8(%rbp), %rdi
	addq	$16, %rbp
	call	_ZN8nlohmann10basic_jsonISt3mapSt6vectorNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEblmdSaNS_14adl_serializerEE10json_value7destroyENS_6detail7value_tE
	jmp	.L1425
.L1551:
	movq	%rax, %rbx
	call	__cxa_end_catch@PLT
.L1015:
	movzbl	376(%rsp), %esi
	leaq	384(%rsp), %rdi
	call	_ZN8nlohmann10basic_jsonISt3mapSt6vectorNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEblmdSaNS_14adl_serializerEE10json_value7destroyENS_6detail7value_tE
	movq	%rbx, %rdi
.LEHB253:
	call	_Unwind_Resume@PLT
.LEHE253:
.L1424:
.LEHB254:
	call	__cxa_rethrow@PLT
.LEHE254:
.L1547:
.L1770:
	movq	%r13, %rdi
	movq	%rax, %rbx
	call	_ZdlPv@PLT
.L1069:
	movq	%rbx, %rdi
	call	__cxa_begin_catch@PLT
	movq	136(%rsp), %rbx
.L1109:
	cmpq	32(%rsp), %rbx
	je	.L1108
	movzbl	(%rbx), %esi
	leaq	8(%rbx), %rdi
	addq	$16, %rbx
	call	_ZN8nlohmann10basic_jsonISt3mapSt6vectorNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEblmdSaNS_14adl_serializerEE10json_value7destroyENS_6detail7value_tE
	jmp	.L1109
.L1566:
.L1779:
	movq	%r13, %rdi
	movq	%rax, %rbx
	call	_ZdlPv@PLT
	jmp	.L1169
.L1537:
	movq	%rax, %rbx
	jmp	.L1069
.L1591:
	movq	%rax, %rbp
	call	__cxa_end_catch@PLT
	movq	152(%rsp), %rax
	movq	(%rax), %rdi
	testq	%rdi, %rdi
	je	.L1428
	call	_ZdlPv@PLT
.L1428:
	movq	152(%rsp), %rdi
	call	_ZdlPv@PLT
.L1342:
	movq	%rbp, %rdi
	call	__cxa_begin_catch@PLT
	movq	144(%rsp), %rbp
.L1432:
	cmpq	%r15, %rbp
	je	.L1431
	movzbl	0(%rbp), %esi
	leaq	8(%rbp), %rdi
	addq	$16, %rbp
	call	_ZN8nlohmann10basic_jsonISt3mapSt6vectorNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEblmdSaNS_14adl_serializerEE10json_value7destroyENS_6detail7value_tE
	jmp	.L1432
.L1556:
	movq	%rax, %rbx
	jmp	.L1169
.L1108:
.LEHB255:
	call	__cxa_rethrow@PLT
.LEHE255:
.L1431:
.LEHB256:
	call	__cxa_rethrow@PLT
.LEHE256:
.L1536:
	movq	%rax, %rbx
	call	__cxa_end_catch@PLT
	movq	120(%rsp), %rax
	movq	(%rax), %rdi
	testq	%rdi, %rdi
	je	.L1112
	call	_ZdlPv@PLT
.L1112:
	movq	120(%rsp), %rdi
	call	_ZdlPv@PLT
.L1051:
	movq	%rbx, %rdi
	call	__cxa_begin_catch@PLT
	movq	128(%rsp), %rbx
.L1116:
	cmpq	8(%rsp), %rbx
	je	.L1115
	movzbl	(%rbx), %esi
	leaq	8(%rbx), %rdi
	addq	$16, %rbx
	call	_ZN8nlohmann10basic_jsonISt3mapSt6vectorNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEblmdSaNS_14adl_serializerEE10json_value7destroyENS_6detail7value_tE
	jmp	.L1116
.L1587:
	movq	%rax, %rbp
	call	__cxa_end_catch@PLT
	movq	120(%rsp), %rax
	movq	(%rax), %rdi
	testq	%rdi, %rdi
	je	.L1435
	call	_ZdlPv@PLT
.L1435:
	movq	120(%rsp), %rdi
	call	_ZdlPv@PLT
.L1324:
	movq	%rbp, %rdi
	call	__cxa_begin_catch@PLT
	movq	184(%rsp), %rbp
.L1441:
	cmpq	%rbx, %rbp
	je	.L1440
	movzbl	0(%rbp), %esi
	leaq	8(%rbp), %rdi
	addq	$16, %rbp
	call	_ZN8nlohmann10basic_jsonISt3mapSt6vectorNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEblmdSaNS_14adl_serializerEE10json_value7destroyENS_6detail7value_tE
	jmp	.L1441
.L1115:
.LEHB257:
	call	__cxa_rethrow@PLT
.LEHE257:
.L1440:
.LEHB258:
	call	__cxa_rethrow@PLT
.LEHE258:
.L1532:
	movq	%rax, %rbx
	call	__cxa_end_catch@PLT
	movq	104(%rsp), %rax
	movq	(%rax), %rdi
	testq	%rdi, %rdi
	je	.L1119
	call	_ZdlPv@PLT
.L1119:
	movq	104(%rsp), %rdi
	call	_ZdlPv@PLT
.L1033:
	movq	%rbx, %rdi
	call	__cxa_begin_catch@PLT
	movq	96(%rsp), %rbx
.L1123:
	cmpq	%r15, %rbx
	je	.L1122
	movzbl	(%rbx), %esi
	leaq	8(%rbx), %rdi
	addq	$16, %rbx
	call	_ZN8nlohmann10basic_jsonISt3mapSt6vectorNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEblmdSaNS_14adl_serializerEE10json_value7destroyENS_6detail7value_tE
	jmp	.L1123
.L1583:
	movq	%rax, %rbx
	call	__cxa_end_catch@PLT
	movq	%rbx, %rdi
	call	__cxa_begin_catch@PLT
	cmpq	$0, 64(%rsp)
	je	.L1841
	movq	64(%rsp), %rdi
	call	_ZdlPv@PLT
.L1460:
.LEHB259:
	call	__cxa_rethrow@PLT
.LEHE259:
.L1122:
.LEHB260:
	call	__cxa_rethrow@PLT
.LEHE260:
.L1520:
	movq	%rax, %rbx
	call	__cxa_end_catch@PLT
	movq	%rbx, %rdi
.LEHB261:
	call	_Unwind_Resume@PLT
.LEHE261:
.L1841:
	movq	128(%rsp), %rbx
	movq	184(%rsp), %rbp
	sarq	$4, %rbx
	addq	40(%rsp), %rbx
	salq	$4, %rbx
.L1461:
	cmpq	%rbp, %rbx
	je	.L1460
	movzbl	0(%rbp), %esi
	leaq	8(%rbp), %rdi
	addq	$16, %rbp
	call	_ZN8nlohmann10basic_jsonISt3mapSt6vectorNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEblmdSaNS_14adl_serializerEE10json_value7destroyENS_6detail7value_tE
	jmp	.L1461
.L1528:
	movq	%rax, %rbx
	call	__cxa_end_catch@PLT
	movq	80(%rsp), %rax
	movq	(%rax), %rdi
	testq	%rdi, %rdi
	je	.L1126
	call	_ZdlPv@PLT
.L1126:
	movq	80(%rsp), %rdi
	call	_ZdlPv@PLT
	jmp	.L1015
.L1836:
.LEHB262:
	call	_ZSt17__throw_bad_allocv@PLT
.LEHE262:
.L1838:
.LEHB263:
	call	_ZSt17__throw_bad_allocv@PLT
.LEHE263:
.L1585:
.L1796:
	movq	%r12, %rdi
	movq	%rax, %rbp
	call	_ZdlPv@PLT
	jmp	.L1324
.L1584:
	movq	%rax, %rbp
	jmp	.L1324
.L1829:
.LEHB264:
	call	_ZSt17__throw_bad_allocv@PLT
.LEHE264:
.L1840:
.LEHB265:
	call	_ZSt17__throw_bad_allocv@PLT
.LEHE265:
.L1543:
	movq	%rax, %rbx
.L1098:
	movq	152(%rsp), %rdi
	call	_ZdlPv@PLT
.L1087:
	movq	%rbx, %rdi
	call	__cxa_begin_catch@PLT
	movq	160(%rsp), %rbx
.L1102:
	cmpq	48(%rsp), %rbx
	je	.L1101
	movzbl	(%rbx), %esi
	leaq	8(%rbx), %rdi
	addq	$16, %rbx
	call	_ZN8nlohmann10basic_jsonISt3mapSt6vectorNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEblmdSaNS_14adl_serializerEE10json_value7destroyENS_6detail7value_tE
	jmp	.L1102
.L1577:
	movq	%rax, %rbx
.L1281:
	movq	72(%rsp), %rdi
	call	_ZdlPv@PLT
.L1270:
	movq	%rbx, %rdi
	call	__cxa_begin_catch@PLT
	movq	80(%rsp), %rbx
.L1285:
	cmpq	8(%rsp), %rbx
	je	.L1284
	movzbl	(%rbx), %esi
	leaq	8(%rbx), %rdi
	addq	$16, %rbx
	call	_ZN8nlohmann10basic_jsonISt3mapSt6vectorNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEblmdSaNS_14adl_serializerEE10json_value7destroyENS_6detail7value_tE
	jmp	.L1285
.L1101:
.LEHB266:
	call	__cxa_rethrow@PLT
.LEHE266:
.L1284:
.LEHB267:
	call	__cxa_rethrow@PLT
.LEHE267:
.L1540:
	movq	%rax, %rbx
	call	__cxa_end_catch@PLT
	movq	144(%rsp), %rax
	movq	(%rax), %rdi
	testq	%rdi, %rdi
	je	.L1105
	call	_ZdlPv@PLT
.L1105:
	movq	144(%rsp), %rdi
	call	_ZdlPv@PLT
	jmp	.L1069
.L1574:
	movq	%rax, %rbx
	call	__cxa_end_catch@PLT
	movq	56(%rsp), %rax
	movq	(%rax), %rdi
	testq	%rdi, %rdi
	je	.L1288
	call	_ZdlPv@PLT
.L1288:
	movq	56(%rsp), %rdi
	call	_ZdlPv@PLT
.L1252:
	movq	%rbx, %rdi
	call	__cxa_begin_catch@PLT
	movq	64(%rsp), %rbx
.L1292:
	cmpq	%r15, %rbx
	je	.L1291
	movzbl	(%rbx), %esi
	leaq	8(%rbx), %rdi
	addq	$16, %rbx
	call	_ZN8nlohmann10basic_jsonISt3mapSt6vectorNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEblmdSaNS_14adl_serializerEE10json_value7destroyENS_6detail7value_tE
	jmp	.L1292
.L1554:
	movq	%rax, %rbx
	jmp	.L1212
.L1593:
.L1792:
	movq	%r12, %rdi
	movq	%rax, %rbp
	call	_ZdlPv@PLT
	jmp	.L1360
.L1607:
	jmp	.L1792
.L1592:
	movq	%rax, %rbp
	jmp	.L1360
.L1589:
.L1794:
	movq	%r12, %rdi
	movq	%rax, %rbp
	call	_ZdlPv@PLT
	jmp	.L1342
.L1609:
	jmp	.L1796
.L1594:
	movq	%rax, %rbp
	jmp	.L1421
.L1819:
.LEHB268:
	call	_ZSt17__throw_bad_allocv@PLT
.LEHE268:
.L1291:
.LEHB269:
	call	__cxa_rethrow@PLT
.LEHE269:
.L1586:
	movq	%rax, %rbp
	jmp	.L1435
.L1557:
	jmp	.L1779
.L1572:
.L1785:
	movq	%r13, %rdi
	movq	%rax, %rbx
	call	_ZdlPv@PLT
	jmp	.L1252
.L1602:
	movq	%rax, %rbp
.L1407:
	movq	216(%rsp), %rdi
	call	_ZdlPv@PLT
	jmp	.L1396
.L1539:
	movq	%rax, %rbx
	jmp	.L1105
.L1525:
.L1800:
	movq	%rax, %rbx
	movq	%rbp, %rdi
	call	_ZdlPv@PLT
	movq	%rbx, %rdi
.LEHB270:
	call	_Unwind_Resume@PLT
.L1521:
	jmp	.L1800
.L1522:
	movq	%rax, %rbx
.L985:
	movq	%r13, %rdi
	call	_ZdlPv@PLT
	movq	%rbx, %rdi
	call	_Unwind_Resume@PLT
.LEHE270:
.L1834:
.LEHB271:
	call	_ZSt17__throw_bad_allocv@PLT
.LEHE271:
.L1810:
.LEHB272:
	call	_ZSt17__throw_bad_allocv@PLT
.LEHE272:
.L1542:
.L1768:
	movq	%r13, %rdi
	movq	%rax, %rbx
	call	_ZdlPv@PLT
	jmp	.L1087
.L1546:
	jmp	.L1768
.L1541:
	movq	%rax, %rbx
	jmp	.L1087
.L1579:
	movq	%rax, %rdi
	call	__cxa_begin_catch@PLT
	movq	88(%rsp), %rbx
.L1278:
	cmpq	24(%rsp), %rbx
	je	.L1277
	movzbl	(%rbx), %esi
	leaq	8(%rbx), %rdi
	addq	$16, %rbx
	call	_ZN8nlohmann10basic_jsonISt3mapSt6vectorNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEblmdSaNS_14adl_serializerEE10json_value7destroyENS_6detail7value_tE
	jmp	.L1278
.L1538:
	jmp	.L1770
.L1277:
.LEHB273:
	call	__cxa_rethrow@PLT
.LEHE273:
.L1524:
	movq	%rax, %rdi
	call	__cxa_begin_catch@PLT
.L982:
	cmpq	%r12, %r15
	je	.L981
	movzbl	(%r15), %esi
	leaq	8(%r15), %rdi
	addq	$16, %r15
	call	_ZN8nlohmann10basic_jsonISt3mapSt6vectorNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEblmdSaNS_14adl_serializerEE10json_value7destroyENS_6detail7value_tE
	jmp	.L982
.L1601:
	jmp	.L1788
.L1578:
	movq	%rax, %rbx
	call	__cxa_end_catch@PLT
	movq	72(%rsp), %rax
	movq	(%rax), %rdi
	testq	%rdi, %rdi
	je	.L1281
	call	_ZdlPv@PLT
	jmp	.L1281
.L981:
.LEHB274:
	call	__cxa_rethrow@PLT
.LEHE274:
.L1815:
.LEHB275:
	call	_ZSt17__throw_bad_allocv@PLT
.LEHE275:
.L1812:
.LEHB276:
	call	_ZSt17__throw_bad_allocv@PLT
.LEHE276:
.L1549:
.L1774:
	movq	%r13, %rdi
	movq	%rax, %rbx
	call	_ZdlPv@PLT
	jmp	.L1033
.L1531:
	movq	%rax, %rbx
	jmp	.L1119
.L1823:
.LEHB277:
	call	_ZSt17__throw_bad_allocv@PLT
.LEHE277:
.L1590:
	movq	%rax, %rbp
	jmp	.L1428
.L1526:
.L1798:
	movq	%r12, %rdi
	movq	%rax, %rbx
	call	_ZdlPv@PLT
	jmp	.L1015
.L1529:
	movq	%rax, %rbx
	jmp	.L1033
.L1550:
	jmp	.L1798
.L1596:
	movq	%rax, %rbp
	jmp	.L1378
.L1530:
	jmp	.L1774
.L1548:
.L1772:
	movq	%r13, %rdi
	movq	%rax, %rbx
	call	_ZdlPv@PLT
	jmp	.L1051
.L1533:
	movq	%rax, %rbx
	jmp	.L1051
.L1535:
	movq	%rax, %rbx
	jmp	.L1112
.L1825:
.LEHB278:
	call	_ZSt17__throw_bad_allocv@PLT
.LEHE278:
.L1534:
	jmp	.L1772
.L1523:
	movq	%rax, %rbx
	call	__cxa_end_catch@PLT
	movq	0(%r13), %rdi
	testq	%rdi, %rdi
	je	.L985
	call	_ZdlPv@PLT
	jmp	.L985
.L1831:
.LEHB279:
	call	_ZSt17__throw_bad_allocv@PLT
.LEHE279:
.L1833:
.LEHB280:
	call	_ZSt17__throw_bad_allocv@PLT
.LEHE280:
.L1558:
	movq	%rax, %rbx
	jmp	.L1205
.L1573:
	movq	%rax, %rbx
	jmp	.L1288
.L1527:
	movq	%rax, %rbx
	jmp	.L1126
.L1821:
.LEHB281:
	call	_ZSt17__throw_bad_allocv@PLT
.LEHE281:
.L1580:
.L1783:
	movq	%r13, %rdi
	movq	%rax, %rbx
	call	_ZdlPv@PLT
	jmp	.L1270
.L1575:
	movq	%rax, %rbx
	jmp	.L1270
.L1608:
	jmp	.L1794
.L1588:
	movq	%rax, %rbp
	jmp	.L1342
.L1569:
	movq	%rax, %rbx
.L1295:
	movq	48(%rsp), %rdi
	call	_ZdlPv@PLT
	jmp	.L1015
.L1817:
.LEHB282:
	call	_ZSt17__throw_bad_allocv@PLT
.LEHE282:
.L1553:
.L1781:
	movq	%r13, %rdi
	movq	%rax, %rbx
	call	_ZdlPv@PLT
	jmp	.L1151
.L1552:
	movq	%rax, %rbx
	jmp	.L1151
.L1582:
.L1799:
	movq	%r13, %rdi
	movq	%rax, %rbx
	call	_ZdlPv@PLT
	jmp	.L1015
.L1519:
	movq	%rax, %rbx
	jmp	.L1015
.L1568:
	jmp	.L1799
.L1567:
	jmp	.L1781
.L1581:
	jmp	.L1785
.L1571:
	movq	%rax, %rbx
	jmp	.L1252
.L1598:
	movq	%rax, %rbp
	jmp	.L1414
.L1604:
	movq	%rax, %rdi
	call	__cxa_begin_catch@PLT
	movq	224(%rsp), %rbp
.L1404:
	cmpq	112(%rsp), %rbp
	je	.L1403
	movzbl	0(%rbp), %esi
	leaq	8(%rbp), %rdi
	addq	$16, %rbp
	call	_ZN8nlohmann10basic_jsonISt3mapSt6vectorNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEblmdSaNS_14adl_serializerEE10json_value7destroyENS_6detail7value_tE
	jmp	.L1404
.L1565:
.L1777:
	movq	%r13, %rdi
	movq	%rax, %rbx
	call	_ZdlPv@PLT
	jmp	.L1187
.L1560:
	movq	%rax, %rbx
	jmp	.L1187
.L1403:
.LEHB283:
	call	__cxa_rethrow@PLT
.LEHE283:
.L1561:
	jmp	.L1777
.L1597:
.L1790:
	movq	%r12, %rdi
	movq	%rax, %rbp
	call	_ZdlPv@PLT
	jmp	.L1378
.L1606:
	jmp	.L1790
.L1808:
.LEHB284:
	call	_ZSt17__throw_bad_allocv@PLT
.LEHE284:
.L1570:
	movq	%rax, %rbx
	call	__cxa_end_catch@PLT
	movq	48(%rsp), %rax
	movq	(%rax), %rdi
	testq	%rdi, %rdi
	je	.L1295
	call	_ZdlPv@PLT
	jmp	.L1295
.L1545:
	movq	%rax, %rdi
	call	__cxa_begin_catch@PLT
	movq	168(%rsp), %rbx
.L1095:
	cmpq	72(%rsp), %rbx
	je	.L1094
	movzbl	(%rbx), %esi
	leaq	8(%rbx), %rdi
	addq	$16, %rbx
	call	_ZN8nlohmann10basic_jsonISt3mapSt6vectorNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEblmdSaNS_14adl_serializerEE10json_value7destroyENS_6detail7value_tE
	jmp	.L1095
.L1564:
	movq	%rax, %rdi
	call	__cxa_begin_catch@PLT
	movq	120(%rsp), %rbx
.L1195:
	cmpq	48(%rsp), %rbx
	je	.L1194
	movzbl	(%rbx), %esi
	leaq	8(%rbx), %rdi
	addq	$16, %rbx
	call	_ZN8nlohmann10basic_jsonISt3mapSt6vectorNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEblmdSaNS_14adl_serializerEE10json_value7destroyENS_6detail7value_tE
	jmp	.L1195
.L1094:
.LEHB285:
	call	__cxa_rethrow@PLT
.LEHE285:
.L1194:
.LEHB286:
	call	__cxa_rethrow@PLT
.LEHE286:
.L1544:
	movq	%rax, %rbx
	call	__cxa_end_catch@PLT
	movq	152(%rsp), %rax
	movq	(%rax), %rdi
	testq	%rdi, %rdi
	je	.L1098
	call	_ZdlPv@PLT
	jmp	.L1098
.L1563:
	movq	%rax, %rbx
	call	__cxa_end_catch@PLT
	movq	104(%rsp), %rax
	movq	(%rax), %rdi
	testq	%rdi, %rdi
	je	.L1198
	call	_ZdlPv@PLT
	jmp	.L1198
.L1576:
	jmp	.L1783
.L1603:
	movq	%rax, %rbp
	call	__cxa_end_catch@PLT
	movq	216(%rsp), %rax
	movq	(%rax), %rdi
	testq	%rdi, %rdi
	je	.L1407
	call	_ZdlPv@PLT
	jmp	.L1407
	.cfi_endproc
.LFE7327:
	.section	.gcc_except_table._ZNSt6vectorIN8nlohmann10basic_jsonISt3mapS_NSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEblmdSaNS0_14adl_serializerEEESaISA_EE14_M_fill_insertEN9__gnu_cxx17__normal_iteratorIPSA_SC_EEmRKSA_,"aG",@progbits,_ZNSt6vectorIN8nlohmann10basic_jsonISt3mapS_NSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEblmdSaNS0_14adl_serializerEEESaISA_EE14_M_fill_insertEN9__gnu_cxx17__normal_iteratorIPSA_SC_EEmRKSA_,comdat
	.align 4
.LLSDA7327:
	.byte	0xff
	.byte	0x9b
	.uleb128 .LLSDATT7327-.LLSDATTD7327
.LLSDATTD7327:
	.byte	0x1
	.uleb128 .LLSDACSE7327-.LLSDACSB7327
.LLSDACSB7327:
	.uleb128 .LEHB139-.LFB7327
	.uleb128 .LEHE139-.LEHB139
	.uleb128 0
	.uleb128 0
	.uleb128 .LEHB140-.LFB7327
	.uleb128 .LEHE140-.LEHB140
	.uleb128 .L1525-.LFB7327
	.uleb128 0
	.uleb128 .LEHB141-.LFB7327
	.uleb128 .LEHE141-.LEHB141
	.uleb128 .L1584-.LFB7327
	.uleb128 0x1
	.uleb128 .LEHB142-.LFB7327
	.uleb128 .LEHE142-.LEHB142
	.uleb128 .L1609-.LFB7327
	.uleb128 0x3
	.uleb128 .LEHB143-.LFB7327
	.uleb128 .LEHE143-.LEHB143
	.uleb128 .L1584-.LFB7327
	.uleb128 0x1
	.uleb128 .LEHB144-.LFB7327
	.uleb128 .LEHE144-.LEHB144
	.uleb128 .L1586-.LFB7327
	.uleb128 0x3
	.uleb128 .LEHB145-.LFB7327
	.uleb128 .LEHE145-.LEHB145
	.uleb128 .L1584-.LFB7327
	.uleb128 0x1
	.uleb128 .LEHB146-.LFB7327
	.uleb128 .LEHE146-.LEHB146
	.uleb128 .L1585-.LFB7327
	.uleb128 0x3
	.uleb128 .LEHB147-.LFB7327
	.uleb128 .LEHE147-.LEHB147
	.uleb128 .L1588-.LFB7327
	.uleb128 0x1
	.uleb128 .LEHB148-.LFB7327
	.uleb128 .LEHE148-.LEHB148
	.uleb128 .L1608-.LFB7327
	.uleb128 0x1
	.uleb128 .LEHB149-.LFB7327
	.uleb128 .LEHE149-.LEHB149
	.uleb128 .L1588-.LFB7327
	.uleb128 0x1
	.uleb128 .LEHB150-.LFB7327
	.uleb128 .LEHE150-.LEHB150
	.uleb128 .L1590-.LFB7327
	.uleb128 0x1
	.uleb128 .LEHB151-.LFB7327
	.uleb128 .LEHE151-.LEHB151
	.uleb128 .L1588-.LFB7327
	.uleb128 0x1
	.uleb128 .LEHB152-.LFB7327
	.uleb128 .LEHE152-.LEHB152
	.uleb128 .L1589-.LFB7327
	.uleb128 0x1
	.uleb128 .LEHB153-.LFB7327
	.uleb128 .LEHE153-.LEHB153
	.uleb128 .L1592-.LFB7327
	.uleb128 0x1
	.uleb128 .LEHB154-.LFB7327
	.uleb128 .LEHE154-.LEHB154
	.uleb128 .L1607-.LFB7327
	.uleb128 0x1
	.uleb128 .LEHB155-.LFB7327
	.uleb128 .LEHE155-.LEHB155
	.uleb128 .L1592-.LFB7327
	.uleb128 0x1
	.uleb128 .LEHB156-.LFB7327
	.uleb128 .LEHE156-.LEHB156
	.uleb128 .L1593-.LFB7327
	.uleb128 0x1
	.uleb128 .LEHB157-.LFB7327
	.uleb128 .LEHE157-.LEHB157
	.uleb128 .L1592-.LFB7327
	.uleb128 0x1
	.uleb128 .LEHB158-.LFB7327
	.uleb128 .LEHE158-.LEHB158
	.uleb128 .L1594-.LFB7327
	.uleb128 0x1
	.uleb128 .LEHB159-.LFB7327
	.uleb128 .LEHE159-.LEHB159
	.uleb128 .L1596-.LFB7327
	.uleb128 0x1
	.uleb128 .LEHB160-.LFB7327
	.uleb128 .LEHE160-.LEHB160
	.uleb128 .L1606-.LFB7327
	.uleb128 0x1
	.uleb128 .LEHB161-.LFB7327
	.uleb128 .LEHE161-.LEHB161
	.uleb128 .L1596-.LFB7327
	.uleb128 0x1
	.uleb128 .LEHB162-.LFB7327
	.uleb128 .LEHE162-.LEHB162
	.uleb128 .L1597-.LFB7327
	.uleb128 0x1
	.uleb128 .LEHB163-.LFB7327
	.uleb128 .LEHE163-.LEHB163
	.uleb128 .L1596-.LFB7327
	.uleb128 0x1
	.uleb128 .LEHB164-.LFB7327
	.uleb128 .LEHE164-.LEHB164
	.uleb128 .L1598-.LFB7327
	.uleb128 0x1
	.uleb128 .LEHB165-.LFB7327
	.uleb128 .LEHE165-.LEHB165
	.uleb128 .L1552-.LFB7327
	.uleb128 0x1
	.uleb128 .LEHB166-.LFB7327
	.uleb128 .LEHE166-.LEHB166
	.uleb128 .L1553-.LFB7327
	.uleb128 0x1
	.uleb128 .LEHB167-.LFB7327
	.uleb128 .LEHE167-.LEHB167
	.uleb128 .L1519-.LFB7327
	.uleb128 0
	.uleb128 .LEHB168-.LFB7327
	.uleb128 .LEHE168-.LEHB168
	.uleb128 .L1582-.LFB7327
	.uleb128 0
	.uleb128 .LEHB169-.LFB7327
	.uleb128 .LEHE169-.LEHB169
	.uleb128 .L1519-.LFB7327
	.uleb128 0
	.uleb128 .LEHB170-.LFB7327
	.uleb128 .LEHE170-.LEHB170
	.uleb128 .L1568-.LFB7327
	.uleb128 0
	.uleb128 .LEHB171-.LFB7327
	.uleb128 .LEHE171-.LEHB171
	.uleb128 .L1519-.LFB7327
	.uleb128 0
	.uleb128 .LEHB172-.LFB7327
	.uleb128 .LEHE172-.LEHB172
	.uleb128 .L1569-.LFB7327
	.uleb128 0
	.uleb128 .LEHB173-.LFB7327
	.uleb128 .LEHE173-.LEHB173
	.uleb128 .L1552-.LFB7327
	.uleb128 0x1
	.uleb128 .LEHB174-.LFB7327
	.uleb128 .LEHE174-.LEHB174
	.uleb128 .L1567-.LFB7327
	.uleb128 0x1
	.uleb128 .LEHB175-.LFB7327
	.uleb128 .LEHE175-.LEHB175
	.uleb128 .L1552-.LFB7327
	.uleb128 0x1
	.uleb128 .LEHB176-.LFB7327
	.uleb128 .LEHE176-.LEHB176
	.uleb128 .L1554-.LFB7327
	.uleb128 0x1
	.uleb128 .LEHB177-.LFB7327
	.uleb128 .LEHE177-.LEHB177
	.uleb128 .L1600-.LFB7327
	.uleb128 0x1
	.uleb128 .LEHB178-.LFB7327
	.uleb128 .LEHE178-.LEHB178
	.uleb128 .L1605-.LFB7327
	.uleb128 0x1
	.uleb128 .LEHB179-.LFB7327
	.uleb128 .LEHE179-.LEHB179
	.uleb128 .L1519-.LFB7327
	.uleb128 0
	.uleb128 .LEHB180-.LFB7327
	.uleb128 .LEHE180-.LEHB180
	.uleb128 .L1527-.LFB7327
	.uleb128 0
	.uleb128 .LEHB181-.LFB7327
	.uleb128 .LEHE181-.LEHB181
	.uleb128 .L1519-.LFB7327
	.uleb128 0
	.uleb128 .LEHB182-.LFB7327
	.uleb128 .LEHE182-.LEHB182
	.uleb128 .L1526-.LFB7327
	.uleb128 0
	.uleb128 .LEHB183-.LFB7327
	.uleb128 .LEHE183-.LEHB183
	.uleb128 .L1519-.LFB7327
	.uleb128 0
	.uleb128 .LEHB184-.LFB7327
	.uleb128 .LEHE184-.LEHB184
	.uleb128 .L1550-.LFB7327
	.uleb128 0
	.uleb128 .LEHB185-.LFB7327
	.uleb128 .LEHE185-.LEHB185
	.uleb128 .L1529-.LFB7327
	.uleb128 0x1
	.uleb128 .LEHB186-.LFB7327
	.uleb128 .LEHE186-.LEHB186
	.uleb128 .L1549-.LFB7327
	.uleb128 0x1
	.uleb128 .LEHB187-.LFB7327
	.uleb128 .LEHE187-.LEHB187
	.uleb128 .L1529-.LFB7327
	.uleb128 0x1
	.uleb128 .LEHB188-.LFB7327
	.uleb128 .LEHE188-.LEHB188
	.uleb128 .L1531-.LFB7327
	.uleb128 0x1
	.uleb128 .LEHB189-.LFB7327
	.uleb128 .LEHE189-.LEHB189
	.uleb128 .L1529-.LFB7327
	.uleb128 0x1
	.uleb128 .LEHB190-.LFB7327
	.uleb128 .LEHE190-.LEHB190
	.uleb128 .L1530-.LFB7327
	.uleb128 0x1
	.uleb128 .LEHB191-.LFB7327
	.uleb128 .LEHE191-.LEHB191
	.uleb128 .L1533-.LFB7327
	.uleb128 0x1
	.uleb128 .LEHB192-.LFB7327
	.uleb128 .LEHE192-.LEHB192
	.uleb128 .L1548-.LFB7327
	.uleb128 0x1
	.uleb128 .LEHB193-.LFB7327
	.uleb128 .LEHE193-.LEHB193
	.uleb128 .L1533-.LFB7327
	.uleb128 0x1
	.uleb128 .LEHB194-.LFB7327
	.uleb128 .LEHE194-.LEHB194
	.uleb128 .L1534-.LFB7327
	.uleb128 0x1
	.uleb128 .LEHB195-.LFB7327
	.uleb128 .LEHE195-.LEHB195
	.uleb128 .L1533-.LFB7327
	.uleb128 0x1
	.uleb128 .LEHB196-.LFB7327
	.uleb128 .LEHE196-.LEHB196
	.uleb128 .L1535-.LFB7327
	.uleb128 0x1
	.uleb128 .LEHB197-.LFB7327
	.uleb128 .LEHE197-.LEHB197
	.uleb128 .L1541-.LFB7327
	.uleb128 0x1
	.uleb128 .LEHB198-.LFB7327
	.uleb128 .LEHE198-.LEHB198
	.uleb128 .L1546-.LFB7327
	.uleb128 0x1
	.uleb128 .LEHB199-.LFB7327
	.uleb128 .LEHE199-.LEHB199
	.uleb128 .L1600-.LFB7327
	.uleb128 0x1
	.uleb128 .LEHB200-.LFB7327
	.uleb128 .LEHE200-.LEHB200
	.uleb128 .L1602-.LFB7327
	.uleb128 0x1
	.uleb128 .LEHB201-.LFB7327
	.uleb128 .LEHE201-.LEHB201
	.uleb128 .L1604-.LFB7327
	.uleb128 0x1
	.uleb128 .LEHB202-.LFB7327
	.uleb128 .LEHE202-.LEHB202
	.uleb128 .L1600-.LFB7327
	.uleb128 0x1
	.uleb128 .LEHB203-.LFB7327
	.uleb128 .LEHE203-.LEHB203
	.uleb128 .L1601-.LFB7327
	.uleb128 0x1
	.uleb128 .LEHB204-.LFB7327
	.uleb128 .LEHE204-.LEHB204
	.uleb128 .L1537-.LFB7327
	.uleb128 0x1
	.uleb128 .LEHB205-.LFB7327
	.uleb128 .LEHE205-.LEHB205
	.uleb128 .L1547-.LFB7327
	.uleb128 0x1
	.uleb128 .LEHB206-.LFB7327
	.uleb128 .LEHE206-.LEHB206
	.uleb128 .L1537-.LFB7327
	.uleb128 0x1
	.uleb128 .LEHB207-.LFB7327
	.uleb128 .LEHE207-.LEHB207
	.uleb128 .L1539-.LFB7327
	.uleb128 0x1
	.uleb128 .LEHB208-.LFB7327
	.uleb128 .LEHE208-.LEHB208
	.uleb128 .L1537-.LFB7327
	.uleb128 0x1
	.uleb128 .LEHB209-.LFB7327
	.uleb128 .LEHE209-.LEHB209
	.uleb128 .L1538-.LFB7327
	.uleb128 0x1
	.uleb128 .LEHB210-.LFB7327
	.uleb128 .LEHE210-.LEHB210
	.uleb128 .L1556-.LFB7327
	.uleb128 0x1
	.uleb128 .LEHB211-.LFB7327
	.uleb128 .LEHE211-.LEHB211
	.uleb128 .L1566-.LFB7327
	.uleb128 0x1
	.uleb128 .LEHB212-.LFB7327
	.uleb128 .LEHE212-.LEHB212
	.uleb128 .L1556-.LFB7327
	.uleb128 0x1
	.uleb128 .LEHB213-.LFB7327
	.uleb128 .LEHE213-.LEHB213
	.uleb128 .L1558-.LFB7327
	.uleb128 0x1
	.uleb128 .LEHB214-.LFB7327
	.uleb128 .LEHE214-.LEHB214
	.uleb128 .L1556-.LFB7327
	.uleb128 0x1
	.uleb128 .LEHB215-.LFB7327
	.uleb128 .LEHE215-.LEHB215
	.uleb128 .L1557-.LFB7327
	.uleb128 0x1
	.uleb128 .LEHB216-.LFB7327
	.uleb128 .LEHE216-.LEHB216
	.uleb128 .L1571-.LFB7327
	.uleb128 0x1
	.uleb128 .LEHB217-.LFB7327
	.uleb128 .LEHE217-.LEHB217
	.uleb128 .L1581-.LFB7327
	.uleb128 0x1
	.uleb128 .LEHB218-.LFB7327
	.uleb128 .LEHE218-.LEHB218
	.uleb128 .L1571-.LFB7327
	.uleb128 0x1
	.uleb128 .LEHB219-.LFB7327
	.uleb128 .LEHE219-.LEHB219
	.uleb128 .L1573-.LFB7327
	.uleb128 0x1
	.uleb128 .LEHB220-.LFB7327
	.uleb128 .LEHE220-.LEHB220
	.uleb128 .L1571-.LFB7327
	.uleb128 0x1
	.uleb128 .LEHB221-.LFB7327
	.uleb128 .LEHE221-.LEHB221
	.uleb128 .L1572-.LFB7327
	.uleb128 0x1
	.uleb128 .LEHB222-.LFB7327
	.uleb128 .LEHE222-.LEHB222
	.uleb128 0
	.uleb128 0
	.uleb128 .LEHB223-.LFB7327
	.uleb128 .LEHE223-.LEHB223
	.uleb128 .L1521-.LFB7327
	.uleb128 0
	.uleb128 .LEHB224-.LFB7327
	.uleb128 .LEHE224-.LEHB224
	.uleb128 0
	.uleb128 0
	.uleb128 .LEHB225-.LFB7327
	.uleb128 .LEHE225-.LEHB225
	.uleb128 .L1522-.LFB7327
	.uleb128 0
	.uleb128 .LEHB226-.LFB7327
	.uleb128 .LEHE226-.LEHB226
	.uleb128 .L1524-.LFB7327
	.uleb128 0x1
	.uleb128 .LEHB227-.LFB7327
	.uleb128 .LEHE227-.LEHB227
	.uleb128 .L1541-.LFB7327
	.uleb128 0x1
	.uleb128 .LEHB228-.LFB7327
	.uleb128 .LEHE228-.LEHB228
	.uleb128 .L1542-.LFB7327
	.uleb128 0x1
	.uleb128 .LEHB229-.LFB7327
	.uleb128 .LEHE229-.LEHB229
	.uleb128 .L1541-.LFB7327
	.uleb128 0x1
	.uleb128 .LEHB230-.LFB7327
	.uleb128 .LEHE230-.LEHB230
	.uleb128 .L1543-.LFB7327
	.uleb128 0x1
	.uleb128 .LEHB231-.LFB7327
	.uleb128 .LEHE231-.LEHB231
	.uleb128 .L1545-.LFB7327
	.uleb128 0x1
	.uleb128 .LEHB232-.LFB7327
	.uleb128 .LEHE232-.LEHB232
	.uleb128 .L1560-.LFB7327
	.uleb128 0x1
	.uleb128 .LEHB233-.LFB7327
	.uleb128 .LEHE233-.LEHB233
	.uleb128 .L1565-.LFB7327
	.uleb128 0x1
	.uleb128 .LEHB234-.LFB7327
	.uleb128 .LEHE234-.LEHB234
	.uleb128 .L1560-.LFB7327
	.uleb128 0x1
	.uleb128 .LEHB235-.LFB7327
	.uleb128 .LEHE235-.LEHB235
	.uleb128 .L1562-.LFB7327
	.uleb128 0x1
	.uleb128 .LEHB236-.LFB7327
	.uleb128 .LEHE236-.LEHB236
	.uleb128 .L1564-.LFB7327
	.uleb128 0x1
	.uleb128 .LEHB237-.LFB7327
	.uleb128 .LEHE237-.LEHB237
	.uleb128 .L1560-.LFB7327
	.uleb128 0x1
	.uleb128 .LEHB238-.LFB7327
	.uleb128 .LEHE238-.LEHB238
	.uleb128 .L1561-.LFB7327
	.uleb128 0x1
	.uleb128 .LEHB239-.LFB7327
	.uleb128 .LEHE239-.LEHB239
	.uleb128 .L1575-.LFB7327
	.uleb128 0x1
	.uleb128 .LEHB240-.LFB7327
	.uleb128 .LEHE240-.LEHB240
	.uleb128 .L1580-.LFB7327
	.uleb128 0x1
	.uleb128 .LEHB241-.LFB7327
	.uleb128 .LEHE241-.LEHB241
	.uleb128 .L1575-.LFB7327
	.uleb128 0x1
	.uleb128 .LEHB242-.LFB7327
	.uleb128 .LEHE242-.LEHB242
	.uleb128 .L1577-.LFB7327
	.uleb128 0x1
	.uleb128 .LEHB243-.LFB7327
	.uleb128 .LEHE243-.LEHB243
	.uleb128 .L1579-.LFB7327
	.uleb128 0x1
	.uleb128 .LEHB244-.LFB7327
	.uleb128 .LEHE244-.LEHB244
	.uleb128 .L1575-.LFB7327
	.uleb128 0x1
	.uleb128 .LEHB245-.LFB7327
	.uleb128 .LEHE245-.LEHB245
	.uleb128 .L1576-.LFB7327
	.uleb128 0x1
	.uleb128 .LEHB246-.LFB7327
	.uleb128 .LEHE246-.LEHB246
	.uleb128 0
	.uleb128 0
	.uleb128 .LEHB247-.LFB7327
	.uleb128 .LEHE247-.LEHB247
	.uleb128 .L1602-.LFB7327
	.uleb128 0x1
	.uleb128 .LEHB248-.LFB7327
	.uleb128 .LEHE248-.LEHB248
	.uleb128 .L1559-.LFB7327
	.uleb128 0x1
	.uleb128 .LEHB249-.LFB7327
	.uleb128 .LEHE249-.LEHB249
	.uleb128 .L1599-.LFB7327
	.uleb128 0x1
	.uleb128 .LEHB250-.LFB7327
	.uleb128 .LEHE250-.LEHB250
	.uleb128 .L1555-.LFB7327
	.uleb128 0x1
	.uleb128 .LEHB251-.LFB7327
	.uleb128 .LEHE251-.LEHB251
	.uleb128 .L1595-.LFB7327
	.uleb128 0x1
	.uleb128 .LEHB252-.LFB7327
	.uleb128 .LEHE252-.LEHB252
	.uleb128 .L1551-.LFB7327
	.uleb128 0
	.uleb128 .LEHB253-.LFB7327
	.uleb128 .LEHE253-.LEHB253
	.uleb128 0
	.uleb128 0
	.uleb128 .LEHB254-.LFB7327
	.uleb128 .LEHE254-.LEHB254
	.uleb128 .L1591-.LFB7327
	.uleb128 0x1
	.uleb128 .LEHB255-.LFB7327
	.uleb128 .LEHE255-.LEHB255
	.uleb128 .L1536-.LFB7327
	.uleb128 0x1
	.uleb128 .LEHB256-.LFB7327
	.uleb128 .LEHE256-.LEHB256
	.uleb128 .L1587-.LFB7327
	.uleb128 0x3
	.uleb128 .LEHB257-.LFB7327
	.uleb128 .LEHE257-.LEHB257
	.uleb128 .L1532-.LFB7327
	.uleb128 0x1
	.uleb128 .LEHB258-.LFB7327
	.uleb128 .LEHE258-.LEHB258
	.uleb128 .L1583-.LFB7327
	.uleb128 0x3
	.uleb128 .LEHB259-.LFB7327
	.uleb128 .LEHE259-.LEHB259
	.uleb128 .L1520-.LFB7327
	.uleb128 0
	.uleb128 .LEHB260-.LFB7327
	.uleb128 .LEHE260-.LEHB260
	.uleb128 .L1528-.LFB7327
	.uleb128 0
	.uleb128 .LEHB261-.LFB7327
	.uleb128 .LEHE261-.LEHB261
	.uleb128 0
	.uleb128 0
	.uleb128 .LEHB262-.LFB7327
	.uleb128 .LEHE262-.LEHB262
	.uleb128 .L1543-.LFB7327
	.uleb128 0x1
	.uleb128 .LEHB263-.LFB7327
	.uleb128 .LEHE263-.LEHB263
	.uleb128 .L1562-.LFB7327
	.uleb128 0x1
	.uleb128 .LEHB264-.LFB7327
	.uleb128 .LEHE264-.LEHB264
	.uleb128 .L1539-.LFB7327
	.uleb128 0x1
	.uleb128 .LEHB265-.LFB7327
	.uleb128 .LEHE265-.LEHB265
	.uleb128 .L1577-.LFB7327
	.uleb128 0x1
	.uleb128 .LEHB266-.LFB7327
	.uleb128 .LEHE266-.LEHB266
	.uleb128 .L1540-.LFB7327
	.uleb128 0x1
	.uleb128 .LEHB267-.LFB7327
	.uleb128 .LEHE267-.LEHB267
	.uleb128 .L1574-.LFB7327
	.uleb128 0x1
	.uleb128 .LEHB268-.LFB7327
	.uleb128 .LEHE268-.LEHB268
	.uleb128 .L1554-.LFB7327
	.uleb128 0x1
	.uleb128 .LEHB269-.LFB7327
	.uleb128 .LEHE269-.LEHB269
	.uleb128 .L1570-.LFB7327
	.uleb128 0
	.uleb128 .LEHB270-.LFB7327
	.uleb128 .LEHE270-.LEHB270
	.uleb128 0
	.uleb128 0
	.uleb128 .LEHB271-.LFB7327
	.uleb128 .LEHE271-.LEHB271
	.uleb128 .L1522-.LFB7327
	.uleb128 0
	.uleb128 .LEHB272-.LFB7327
	.uleb128 .LEHE272-.LEHB272
	.uleb128 .L1590-.LFB7327
	.uleb128 0x1
	.uleb128 .LEHB273-.LFB7327
	.uleb128 .LEHE273-.LEHB273
	.uleb128 .L1578-.LFB7327
	.uleb128 0x1
	.uleb128 .LEHB274-.LFB7327
	.uleb128 .LEHE274-.LEHB274
	.uleb128 .L1523-.LFB7327
	.uleb128 0
	.uleb128 .LEHB275-.LFB7327
	.uleb128 .LEHE275-.LEHB275
	.uleb128 .L1598-.LFB7327
	.uleb128 0x1
	.uleb128 .LEHB276-.LFB7327
	.uleb128 .LEHE276-.LEHB276
	.uleb128 .L1594-.LFB7327
	.uleb128 0x1
	.uleb128 .LEHB277-.LFB7327
	.uleb128 .LEHE277-.LEHB277
	.uleb128 .L1531-.LFB7327
	.uleb128 0x1
	.uleb128 .LEHB278-.LFB7327
	.uleb128 .LEHE278-.LEHB278
	.uleb128 .L1535-.LFB7327
	.uleb128 0x1
	.uleb128 .LEHB279-.LFB7327
	.uleb128 .LEHE279-.LEHB279
	.uleb128 .L1558-.LFB7327
	.uleb128 0x1
	.uleb128 .LEHB280-.LFB7327
	.uleb128 .LEHE280-.LEHB280
	.uleb128 .L1573-.LFB7327
	.uleb128 0x1
	.uleb128 .LEHB281-.LFB7327
	.uleb128 .LEHE281-.LEHB281
	.uleb128 .L1527-.LFB7327
	.uleb128 0
	.uleb128 .LEHB282-.LFB7327
	.uleb128 .LEHE282-.LEHB282
	.uleb128 .L1569-.LFB7327
	.uleb128 0
	.uleb128 .LEHB283-.LFB7327
	.uleb128 .LEHE283-.LEHB283
	.uleb128 .L1603-.LFB7327
	.uleb128 0x1
	.uleb128 .LEHB284-.LFB7327
	.uleb128 .LEHE284-.LEHB284
	.uleb128 .L1586-.LFB7327
	.uleb128 0x3
	.uleb128 .LEHB285-.LFB7327
	.uleb128 .LEHE285-.LEHB285
	.uleb128 .L1544-.LFB7327
	.uleb128 0x1
	.uleb128 .LEHB286-.LFB7327
	.uleb128 .LEHE286-.LEHB286
	.uleb128 .L1563-.LFB7327
	.uleb128 0x1
.LLSDACSE7327:
	.byte	0x1
	.byte	0
	.byte	0
	.byte	0x7d
	.align 4
	.long	0

.LLSDATT7327:
	.section	.text._ZNSt6vectorIN8nlohmann10basic_jsonISt3mapS_NSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEblmdSaNS0_14adl_serializerEEESaISA_EE14_M_fill_insertEN9__gnu_cxx17__normal_iteratorIPSA_SC_EEmRKSA_,"axG",@progbits,_ZNSt6vectorIN8nlohmann10basic_jsonISt3mapS_NSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEblmdSaNS0_14adl_serializerEEESaISA_EE14_M_fill_insertEN9__gnu_cxx17__normal_iteratorIPSA_SC_EEmRKSA_,comdat
	.size	_ZNSt6vectorIN8nlohmann10basic_jsonISt3mapS_NSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEblmdSaNS0_14adl_serializerEEESaISA_EE14_M_fill_insertEN9__gnu_cxx17__normal_iteratorIPSA_SC_EEmRKSA_, .-_ZNSt6vectorIN8nlohmann10basic_jsonISt3mapS_NSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEblmdSaNS0_14adl_serializerEEESaISA_EE14_M_fill_insertEN9__gnu_cxx17__normal_iteratorIPSA_SC_EEmRKSA_
	.section	.rodata._ZN8nlohmann10basic_jsonISt3mapSt6vectorNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEblmdSaNS_14adl_serializerEEixEm.str1.1,"aMS",@progbits,1
.LC20:
	.string	"cannot use operator[] with "
	.section	.text._ZN8nlohmann10basic_jsonISt3mapSt6vectorNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEblmdSaNS_14adl_serializerEEixEm,"axG",@progbits,_ZN8nlohmann10basic_jsonISt3mapSt6vectorNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEblmdSaNS_14adl_serializerEEixEm,comdat
	.align 2
	.p2align 4,,15
	.weak	_ZN8nlohmann10basic_jsonISt3mapSt6vectorNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEblmdSaNS_14adl_serializerEEixEm
	.type	_ZN8nlohmann10basic_jsonISt3mapSt6vectorNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEblmdSaNS_14adl_serializerEEixEm, @function
_ZN8nlohmann10basic_jsonISt3mapSt6vectorNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEblmdSaNS_14adl_serializerEEixEm:
.LFB6834:
	.cfi_startproc
	.cfi_personality 0x9b,DW.ref.__gxx_personality_v0
	.cfi_lsda 0x1b,.LLSDA6834
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
	pushq	%rbp
	.cfi_def_cfa_offset 48
	.cfi_offset 6, -48
	pushq	%rbx
	.cfi_def_cfa_offset 56
	.cfi_offset 3, -56
	movq	%rdi, %rbp
	movq	%rsi, %rbx
	subq	$328, %rsp
	.cfi_def_cfa_offset 384
	movq	%fs:40, %rax
	movq	%rax, 312(%rsp)
	xorl	%eax, %eax
	movzbl	(%rdi), %eax
	testb	%al, %al
	je	.L1951
	cmpb	$2, %al
	jne	.L1844
.L1954:
	movq	8(%rbp), %r12
	movq	(%r12), %rax
	movq	8(%r12), %rdx
	subq	%rax, %rdx
	sarq	$4, %rdx
	cmpq	%rdx, %rbx
	jnb	.L1952
.L1845:
	salq	$4, %rbx
	addq	%rbx, %rax
	movq	312(%rsp), %rcx
	xorq	%fs:40, %rcx
	jne	.L1953
	addq	$328, %rsp
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
	ret
	.p2align 4,,10
	.p2align 3
.L1951:
	.cfi_restore_state
	movb	$2, (%rdi)
	movl	$24, %edi
.LEHB287:
	call	_Znwm@PLT
.LEHE287:
	movq	$0, (%rax)
	movq	$0, 8(%rax)
	movq	$0, 16(%rax)
	movq	%rax, 8(%rbp)
	movzbl	0(%rbp), %eax
	cmpb	$2, %al
	je	.L1954
.L1844:
	movl	$32, %edi
	call	__cxa_allocate_exception@PLT
	cmpb	$8, 0(%rbp)
	movq	%rax, %r15
	ja	.L1854
	movzbl	0(%rbp), %eax
	leaq	.L1856(%rip), %rdx
	movslq	(%rdx,%rax,4), %rax
	addq	%rdx, %rax
	jmp	*%rax
	.section	.rodata._ZN8nlohmann10basic_jsonISt3mapSt6vectorNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEblmdSaNS_14adl_serializerEEixEm,"aG",@progbits,_ZN8nlohmann10basic_jsonISt3mapSt6vectorNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEblmdSaNS_14adl_serializerEEixEm,comdat
	.align 4
	.align 4
.L1856:
	.long	.L1855-.L1856
	.long	.L1857-.L1856
	.long	.L1920-.L1856
	.long	.L1859-.L1856
	.long	.L1860-.L1856
	.long	.L1854-.L1856
	.long	.L1854-.L1856
	.long	.L1854-.L1856
	.long	.L1861-.L1856
	.section	.text._ZN8nlohmann10basic_jsonISt3mapSt6vectorNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEblmdSaNS_14adl_serializerEEixEm,"axG",@progbits,_ZN8nlohmann10basic_jsonISt3mapSt6vectorNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEblmdSaNS_14adl_serializerEEixEm,comdat
	.p2align 4,,10
	.p2align 3
.L1952:
	leaq	32(%rsp), %r13
	xorl	%esi, %esi
	movb	$0, 32(%rsp)
	leaq	8(%r13), %rdi
	call	_ZN8nlohmann10basic_jsonISt3mapSt6vectorNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEblmdSaNS_14adl_serializerEE10json_valueC1ENS_6detail7value_tE
	movq	8(%rbp), %rax
	leaq	1(%rbx), %rdx
	movq	%r12, %rdi
	movq	8(%rax), %rsi
	movq	%rsi, %rcx
	subq	(%rax), %rcx
	movq	%rcx, %rax
	movq	%r13, %rcx
	sarq	$4, %rax
	subq	%rax, %rdx
.LEHB288:
	call	_ZNSt6vectorIN8nlohmann10basic_jsonISt3mapS_NSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEblmdSaNS0_14adl_serializerEEESaISA_EE14_M_fill_insertEN9__gnu_cxx17__normal_iteratorIPSA_SC_EEmRKSA_
.LEHE288:
	movzbl	32(%rsp), %eax
	cmpb	$2, %al
	je	.L1847
	cmpb	$3, %al
	je	.L1848
	cmpb	$1, %al
	jne	.L1846
	movq	40(%rsp), %rdi
	movq	16(%rdi), %rsi
	call	_ZNSt8_Rb_treeINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESt4pairIKS5_N8nlohmann10basic_jsonISt3mapSt6vectorS5_blmdSaNS8_14adl_serializerEEEESt10_Select1stISE_ESt4lessIvESaISE_EE8_M_eraseEPSt13_Rb_tree_nodeISE_E
	movq	40(%rsp), %rdi
	call	_ZdlPv@PLT
.L1846:
	movq	8(%rbp), %rax
	movq	(%rax), %rax
	jmp	.L1845
	.p2align 4,,10
	.p2align 3
.L1848:
	movq	40(%rsp), %rdi
	movq	(%rdi), %rax
	leaq	16(%rdi), %rdx
	cmpq	%rdx, %rax
	je	.L1853
	movq	%rax, %rdi
	call	_ZdlPv@PLT
.L1946:
	movq	40(%rsp), %rdi
.L1853:
	call	_ZdlPv@PLT
	jmp	.L1846
	.p2align 4,,10
	.p2align 3
.L1847:
	movq	40(%rsp), %r14
	movq	8(%r14), %r13
	movq	(%r14), %r12
	cmpq	%r12, %r13
	je	.L1850
	.p2align 4,,10
	.p2align 3
.L1851:
	movzbl	(%r12), %esi
	leaq	8(%r12), %rdi
	addq	$16, %r12
	call	_ZN8nlohmann10basic_jsonISt3mapSt6vectorNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEblmdSaNS_14adl_serializerEE10json_value7destroyENS_6detail7value_tE
	cmpq	%r12, %r13
	jne	.L1851
	movq	(%r14), %r12
.L1850:
	testq	%r12, %r12
	je	.L1946
	movq	%r12, %rdi
	call	_ZdlPv@PLT
	jmp	.L1946
	.p2align 4,,10
	.p2align 3
.L1920:
	leaq	.LC13(%rip), %rbx
	.p2align 4,,10
	.p2align 3
.L1858:
	leaq	48(%rsp), %r12
	movq	%rbx, %rdi
	leaq	16(%r12), %rax
	movq	%rax, 48(%rsp)
	call	strlen@PLT
.L1918:
	leaq	(%rbx,%rax), %rdx
	movq	%rbx, %rsi
	movq	%r12, %rdi
.LEHB289:
	call	_ZNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEE12_M_constructIPKcEEvT_S8_St20forward_iterator_tag.isra.71
.LEHE289:
	leaq	.LC20(%rip), %rcx
	movl	$27, %r8d
	xorl	%edx, %edx
	xorl	%esi, %esi
	movq	%r12, %rdi
.LEHB290:
	call	_ZNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEE10_M_replaceEmmPKcm
.LEHE290:
	leaq	96(%rsp), %rdx
	leaq	80(%rsp), %rsi
	movq	%rdx, 80(%rsp)
	movq	(%rax), %rcx
	leaq	16(%rax), %rdx
	movq	%rsi, 24(%rsp)
	cmpq	%rdx, %rcx
	je	.L1955
	movq	%rcx, 80(%rsp)
	movq	16(%rax), %rcx
	movq	%rcx, 96(%rsp)
.L1863:
	movq	8(%rax), %rcx
	leaq	144(%rsp), %r14
	movb	$0, 16(%rax)
	movq	%r14, %rdi
	movq	%rcx, 88(%rsp)
	movq	%rdx, (%rax)
	leaq	10+.LC3(%rip), %rdx
	movq	$0, 8(%rax)
	leaq	16(%r14), %rax
	leaq	-10(%rdx), %rsi
	movq	%rax, 144(%rsp)
.LEHB291:
	call	_ZNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEE12_M_constructIPKcEEvT_S8_St20forward_iterator_tag.isra.71
.LEHE291:
	leaq	240(%rsp), %rax
	movq	vsnprintf@GOTPCREL(%rip), %rsi
	leaq	.LC4(%rip), %rcx
	movl	$305, %r8d
	movl	$16, %edx
	movq	%rax, 16(%rsp)
	movq	%rax, %rdi
	xorl	%eax, %eax
.LEHB292:
	call	_ZN9__gnu_cxx12__to_xstringINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEcEET_PFiPT0_mPKS8_P13__va_list_tagEmSB_z
.LEHE292:
	leaq	176(%rsp), %rbx
	movq	$0, 184(%rsp)
	movb	$0, 192(%rsp)
	leaq	16(%rbx), %rax
	movq	%rax, 176(%rsp)
	movq	152(%rsp), %rax
	addq	$16, %rax
	cmpq	$15, %rax
	movq	%rax, 32(%rsp)
	ja	.L1956
.L1864:
	leaq	.LC6(%rip), %rsi
	movl	$16, %edx
	movq	%rbx, %rdi
.LEHB293:
	call	_ZNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEE9_M_appendEPKcm
	movq	152(%rsp), %rdx
	movq	144(%rsp), %rsi
	movq	%rbx, %rdi
	call	_ZNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEE9_M_appendEPKcm
.LEHE293:
	movabsq	$9223372036854775807, %rax
	cmpq	%rax, 184(%rsp)
	je	.L1957
	leaq	.LC7(%rip), %rsi
	movl	$1, %edx
	movq	%rbx, %rdi
.LEHB294:
	call	_ZNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEE9_M_appendEPKcm
.LEHE294:
	leaq	208(%rsp), %r13
	leaq	16(%r13), %rdx
	movq	%rdx, 208(%rsp)
	movq	(%rax), %rcx
	leaq	16(%rax), %rdx
	cmpq	%rdx, %rcx
	je	.L1958
	movq	%rcx, 208(%rsp)
	movq	16(%rax), %rcx
	movq	%rcx, 224(%rsp)
.L1874:
	movq	8(%rax), %rcx
	leaq	16(%r13), %rsi
	movb	$0, 16(%rax)
	movq	%rcx, 216(%rsp)
	movq	%rdx, (%rax)
	movq	208(%rsp), %rcx
	movq	$0, 8(%rax)
	movl	$15, %eax
	movq	216(%rsp), %r8
	movq	248(%rsp), %rdx
	cmpq	%rsi, %rcx
	leaq	(%r8,%rdx), %rdi
	je	.L1875
	movq	224(%rsp), %rax
.L1875:
	cmpq	%rax, %rdi
	movq	240(%rsp), %rsi
	jbe	.L1876
	movq	16(%rsp), %rax
	leaq	16(%rax), %r9
	movl	$15, %eax
	cmpq	%r9, %rsi
	je	.L1877
	movq	256(%rsp), %rax
.L1877:
	cmpq	%rax, %rdi
	jbe	.L1959
.L1876:
	movq	%r13, %rdi
.LEHB295:
	call	_ZNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEE9_M_appendEPKcm
.LEHE295:
.L1878:
	leaq	272(%rsp), %rbp
	leaq	16(%rbp), %rdx
	movq	%rdx, 272(%rsp)
	movq	(%rax), %rcx
	leaq	16(%rax), %rdx
	cmpq	%rdx, %rcx
	je	.L1960
	movq	%rcx, 272(%rsp)
	movq	16(%rax), %rcx
	movq	%rcx, 288(%rsp)
.L1880:
	movq	8(%rax), %rcx
	movq	%rcx, 280(%rsp)
	movq	%rdx, (%rax)
	movq	$0, 8(%rax)
	movb	$0, 16(%rax)
	movabsq	$9223372036854775807, %rax
	subq	280(%rsp), %rax
	cmpq	$1, %rax
	jbe	.L1961
	leaq	.LC8(%rip), %rsi
	movl	$2, %edx
	movq	%rbp, %rdi
.LEHB296:
	call	_ZNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEE9_M_appendEPKcm
.LEHE296:
	leaq	112(%rsp), %rcx
	leaq	128(%rsp), %rdx
	movq	%rcx, 8(%rsp)
	movq	%rdx, 112(%rsp)
	leaq	16(%rax), %rdx
	movq	(%rax), %rcx
	cmpq	%rdx, %rcx
	je	.L1962
	movq	%rcx, 112(%rsp)
	movq	16(%rax), %rcx
	movq	%rcx, 128(%rsp)
.L1883:
	movq	8(%rax), %rcx
	movb	$0, 16(%rax)
	movq	%rcx, 120(%rsp)
	movq	%rdx, (%rax)
	movq	$0, 8(%rax)
	movq	272(%rsp), %rdi
	leaq	16(%rbp), %rax
	cmpq	%rax, %rdi
	je	.L1884
	call	_ZdlPv@PLT
.L1884:
	movq	208(%rsp), %rdi
	addq	$16, %r13
	cmpq	%r13, %rdi
	je	.L1885
	call	_ZdlPv@PLT
.L1885:
	movq	176(%rsp), %rdi
	addq	$16, %rbx
	cmpq	%rbx, %rdi
	je	.L1886
	call	_ZdlPv@PLT
.L1886:
	movq	16(%rsp), %rax
	movq	240(%rsp), %rdi
	addq	$16, %rax
	cmpq	%rax, %rdi
	je	.L1887
	call	_ZdlPv@PLT
.L1887:
	movq	88(%rsp), %rdx
	movq	80(%rsp), %rsi
	movq	8(%rsp), %rdi
.LEHB297:
	call	_ZNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEE9_M_appendEPKcm
.LEHE297:
	leaq	16(%rbp), %rdx
	movq	%rdx, 272(%rsp)
	movq	(%rax), %rcx
	leaq	16(%rax), %rdx
	cmpq	%rdx, %rcx
	je	.L1963
	movq	%rcx, 272(%rsp)
	movq	16(%rax), %rcx
	movq	%rcx, 288(%rsp)
.L1898:
	movq	8(%rax), %rcx
	movq	%rcx, 280(%rsp)
	movq	%rdx, (%rax)
	movq	$0, 8(%rax)
	movb	$0, 16(%rax)
	movq	8(%rsp), %rax
	movq	112(%rsp), %rdi
	addq	$16, %rax
	cmpq	%rax, %rdi
	je	.L1899
	call	_ZdlPv@PLT
.L1899:
	movq	144(%rsp), %rdi
	addq	$16, %r14
	cmpq	%r14, %rdi
	je	.L1900
	call	_ZdlPv@PLT
.L1900:
	movq	272(%rsp), %rsi
	leaq	16+_ZTVN8nlohmann6detail9exceptionE(%rip), %rax
	leaq	16(%r15), %rdi
	movl	$305, 8(%r15)
	movq	%rax, (%r15)
.LEHB298:
	call	_ZNSt13runtime_errorC1EPKc@PLT
.LEHE298:
	movq	272(%rsp), %rdi
	leaq	16+_ZTVN8nlohmann6detail10type_errorE(%rip), %rax
	addq	$16, %rbp
	movq	%rax, (%r15)
	cmpq	%rbp, %rdi
	je	.L1905
	call	_ZdlPv@PLT
.L1905:
	movq	24(%rsp), %rax
	movq	80(%rsp), %rdi
	addq	$16, %rax
	cmpq	%rax, %rdi
	je	.L1907
	call	_ZdlPv@PLT
.L1907:
	movq	48(%rsp), %rdi
	addq	$16, %r12
	cmpq	%r12, %rdi
	je	.L1912
	call	_ZdlPv@PLT
.L1912:
	leaq	_ZN8nlohmann6detail10type_errorD1Ev(%rip), %rdx
	leaq	_ZTIN8nlohmann6detail10type_errorE(%rip), %rsi
	movq	%r15, %rdi
.LEHB299:
	call	__cxa_throw@PLT
.LEHE299:
	.p2align 4,,10
	.p2align 3
.L1859:
	leaq	.LC14(%rip), %rbx
	jmp	.L1858
	.p2align 4,,10
	.p2align 3
.L1857:
	leaq	.LC12(%rip), %rbx
	jmp	.L1858
	.p2align 4,,10
	.p2align 3
.L1861:
	leaq	.LC16(%rip), %rbx
	jmp	.L1858
	.p2align 4,,10
	.p2align 3
.L1860:
	leaq	.LC15(%rip), %rbx
	jmp	.L1858
	.p2align 4,,10
	.p2align 3
.L1855:
	leaq	48(%rsp), %r12
	leaq	.LC11(%rip), %rbx
	leaq	16(%r12), %rax
	movq	%rax, 48(%rsp)
	movl	$4, %eax
	jmp	.L1918
	.p2align 4,,10
	.p2align 3
.L1854:
	leaq	.LC17(%rip), %rbx
	jmp	.L1858
	.p2align 4,,10
	.p2align 3
.L1956:
	leaq	32(%rsp), %rdi
	movl	$15, %esi
.LEHB300:
	call	_ZNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEE9_M_createERmm.isra.66
	movq	%rax, %rbp
	movq	184(%rsp), %rax
	movq	176(%rsp), %rdi
	leaq	1(%rax), %rdx
	cmpq	$1, %rdx
	je	.L1964
	testq	%rdx, %rdx
	jne	.L1965
.L1866:
	leaq	16(%rbx), %rax
	cmpq	%rax, %rdi
	je	.L1867
	call	_ZdlPv@PLT
.L1867:
	movq	32(%rsp), %rax
	movq	%rbp, 176(%rsp)
	movq	%rax, 192(%rsp)
	movabsq	$9223372036854775807, %rax
	subq	184(%rsp), %rax
	cmpq	$15, %rax
	ja	.L1864
	leaq	.LC5(%rip), %rdi
	call	_ZSt20__throw_length_errorPKc@PLT
.LEHE300:
	.p2align 4,,10
	.p2align 3
.L1963:
	movdqu	16(%rax), %xmm0
	movaps	%xmm0, 288(%rsp)
	jmp	.L1898
	.p2align 4,,10
	.p2align 3
.L1955:
	movdqu	16(%rax), %xmm0
	movaps	%xmm0, 96(%rsp)
	jmp	.L1863
	.p2align 4,,10
	.p2align 3
.L1962:
	movdqu	16(%rax), %xmm0
	movaps	%xmm0, 128(%rsp)
	jmp	.L1883
	.p2align 4,,10
	.p2align 3
.L1960:
	movdqu	16(%rax), %xmm0
	movaps	%xmm0, 288(%rsp)
	jmp	.L1880
	.p2align 4,,10
	.p2align 3
.L1958:
	movdqu	16(%rax), %xmm0
	movaps	%xmm0, 224(%rsp)
	jmp	.L1874
	.p2align 4,,10
	.p2align 3
.L1959:
	movq	16(%rsp), %rdi
	xorl	%edx, %edx
	xorl	%esi, %esi
.LEHB301:
	call	_ZNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEE10_M_replaceEmmPKcm
.LEHE301:
	jmp	.L1878
	.p2align 4,,10
	.p2align 3
.L1964:
	movzbl	(%rdi), %eax
	movb	%al, 0(%rbp)
	movq	176(%rsp), %rdi
	jmp	.L1866
	.p2align 4,,10
	.p2align 3
.L1965:
	movq	%rdi, %rsi
	movq	%rbp, %rdi
	call	memcpy@PLT
	movq	176(%rsp), %rdi
	jmp	.L1866
.L1953:
	call	__stack_chk_fail@PLT
.L1923:
	movzbl	32(%rsp), %esi
	leaq	8(%r13), %rdi
	movq	%rax, %rbx
	call	_ZN8nlohmann10basic_jsonISt3mapSt6vectorNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEblmdSaNS_14adl_serializerEE10json_value7destroyENS_6detail7value_tE
	movq	%rbx, %rdi
.LEHB302:
	call	_Unwind_Resume@PLT
.L1929:
.L1950:
	movq	%rax, %rbp
.L1893:
	movq	176(%rsp), %rdi
	addq	$16, %rbx
	cmpq	%rbx, %rdi
	je	.L1871
	call	_ZdlPv@PLT
.L1871:
	movq	16(%rsp), %rax
	movq	240(%rsp), %rdi
	addq	$16, %rax
	cmpq	%rax, %rdi
	je	.L1895
	call	_ZdlPv@PLT
.L1895:
	movq	%rbp, %rbx
.L1896:
	movq	144(%rsp), %rdi
	addq	$16, %r14
	cmpq	%r14, %rdi
	je	.L1911
.L1903:
	call	_ZdlPv@PLT
.L1911:
	movq	24(%rsp), %rdx
	movq	80(%rsp), %rdi
	addq	$16, %rdx
	cmpq	%rdx, %rdi
	je	.L1915
	call	_ZdlPv@PLT
.L1915:
	movq	48(%rsp), %rdi
	addq	$16, %r12
	cmpq	%r12, %rdi
	je	.L1917
	call	_ZdlPv@PLT
.L1917:
	movq	%r15, %rdi
	call	__cxa_free_exception@PLT
	movq	%rbx, %rdi
	call	_Unwind_Resume@PLT
.LEHE302:
.L1930:
.L1947:
	movq	208(%rsp), %rdi
	addq	$16, %r13
	movq	%rax, %rbp
	cmpq	%r13, %rdi
	je	.L1893
	call	_ZdlPv@PLT
	jmp	.L1893
.L1931:
	movq	272(%rsp), %rdi
	addq	$16, %rbp
	cmpq	%rbp, %rdi
	je	.L1947
	movq	%rax, 8(%rsp)
	call	_ZdlPv@PLT
	movq	8(%rsp), %rax
	jmp	.L1947
.L1961:
	leaq	.LC5(%rip), %rdi
.LEHB303:
	call	_ZSt20__throw_length_errorPKc@PLT
.LEHE303:
.L1928:
	movq	8(%rsp), %rdx
	movq	112(%rsp), %rdi
	movq	%rax, %rbx
	addq	$16, %rdx
	cmpq	%rdx, %rdi
	je	.L1896
	call	_ZdlPv@PLT
	jmp	.L1896
.L1926:
	movq	%rax, %rbx
	jmp	.L1911
.L1925:
	movq	%rax, %rbx
	jmp	.L1915
.L1924:
	movq	%rax, %rbx
	jmp	.L1917
.L1933:
	movq	%r15, %rdi
	movq	%rax, %rbx
	addq	$16, %rbp
	call	_ZNSt9exceptionD2Ev@PLT
	movq	272(%rsp), %rdi
	cmpq	%rbp, %rdi
	jne	.L1903
	jmp	.L1911
.L1927:
	movq	%rax, %rbx
	jmp	.L1896
.L1957:
	leaq	.LC5(%rip), %rdi
.LEHB304:
	call	_ZSt20__throw_length_errorPKc@PLT
.LEHE304:
.L1932:
	jmp	.L1950
	.cfi_endproc
.LFE6834:
	.section	.gcc_except_table._ZN8nlohmann10basic_jsonISt3mapSt6vectorNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEblmdSaNS_14adl_serializerEEixEm,"aG",@progbits,_ZN8nlohmann10basic_jsonISt3mapSt6vectorNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEblmdSaNS_14adl_serializerEEixEm,comdat
.LLSDA6834:
	.byte	0xff
	.byte	0xff
	.byte	0x1
	.uleb128 .LLSDACSE6834-.LLSDACSB6834
.LLSDACSB6834:
	.uleb128 .LEHB287-.LFB6834
	.uleb128 .LEHE287-.LEHB287
	.uleb128 0
	.uleb128 0
	.uleb128 .LEHB288-.LFB6834
	.uleb128 .LEHE288-.LEHB288
	.uleb128 .L1923-.LFB6834
	.uleb128 0
	.uleb128 .LEHB289-.LFB6834
	.uleb128 .LEHE289-.LEHB289
	.uleb128 .L1924-.LFB6834
	.uleb128 0
	.uleb128 .LEHB290-.LFB6834
	.uleb128 .LEHE290-.LEHB290
	.uleb128 .L1925-.LFB6834
	.uleb128 0
	.uleb128 .LEHB291-.LFB6834
	.uleb128 .LEHE291-.LEHB291
	.uleb128 .L1926-.LFB6834
	.uleb128 0
	.uleb128 .LEHB292-.LFB6834
	.uleb128 .LEHE292-.LEHB292
	.uleb128 .L1927-.LFB6834
	.uleb128 0
	.uleb128 .LEHB293-.LFB6834
	.uleb128 .LEHE293-.LEHB293
	.uleb128 .L1932-.LFB6834
	.uleb128 0
	.uleb128 .LEHB294-.LFB6834
	.uleb128 .LEHE294-.LEHB294
	.uleb128 .L1929-.LFB6834
	.uleb128 0
	.uleb128 .LEHB295-.LFB6834
	.uleb128 .LEHE295-.LEHB295
	.uleb128 .L1930-.LFB6834
	.uleb128 0
	.uleb128 .LEHB296-.LFB6834
	.uleb128 .LEHE296-.LEHB296
	.uleb128 .L1931-.LFB6834
	.uleb128 0
	.uleb128 .LEHB297-.LFB6834
	.uleb128 .LEHE297-.LEHB297
	.uleb128 .L1928-.LFB6834
	.uleb128 0
	.uleb128 .LEHB298-.LFB6834
	.uleb128 .LEHE298-.LEHB298
	.uleb128 .L1933-.LFB6834
	.uleb128 0
	.uleb128 .LEHB299-.LFB6834
	.uleb128 .LEHE299-.LEHB299
	.uleb128 0
	.uleb128 0
	.uleb128 .LEHB300-.LFB6834
	.uleb128 .LEHE300-.LEHB300
	.uleb128 .L1932-.LFB6834
	.uleb128 0
	.uleb128 .LEHB301-.LFB6834
	.uleb128 .LEHE301-.LEHB301
	.uleb128 .L1930-.LFB6834
	.uleb128 0
	.uleb128 .LEHB302-.LFB6834
	.uleb128 .LEHE302-.LEHB302
	.uleb128 0
	.uleb128 0
	.uleb128 .LEHB303-.LFB6834
	.uleb128 .LEHE303-.LEHB303
	.uleb128 .L1931-.LFB6834
	.uleb128 0
	.uleb128 .LEHB304-.LFB6834
	.uleb128 .LEHE304-.LEHB304
	.uleb128 .L1929-.LFB6834
	.uleb128 0
.LLSDACSE6834:
	.section	.text._ZN8nlohmann10basic_jsonISt3mapSt6vectorNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEblmdSaNS_14adl_serializerEEixEm,"axG",@progbits,_ZN8nlohmann10basic_jsonISt3mapSt6vectorNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEblmdSaNS_14adl_serializerEEixEm,comdat
	.size	_ZN8nlohmann10basic_jsonISt3mapSt6vectorNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEblmdSaNS_14adl_serializerEEixEm, .-_ZN8nlohmann10basic_jsonISt3mapSt6vectorNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEblmdSaNS_14adl_serializerEEixEm
	.section	.rodata.str1.1
.LC22:
	.string	"%ld | %f | {%ld, %ld, %ld}\n"
	.section	.text.startup,"ax",@progbits
	.p2align 4,,15
	.globl	main
	.type	main, @function
main:
.LFB6411:
	.cfi_startproc
	.cfi_personality 0x9b,DW.ref.__gxx_personality_v0
	.cfi_lsda 0x1b,.LLSDA6411
	pushq	%r15
	.cfi_def_cfa_offset 16
	.cfi_offset 15, -16
	pushq	%r14
	.cfi_def_cfa_offset 24
	.cfi_offset 14, -24
	movl	$12, %edi
	pushq	%r13
	.cfi_def_cfa_offset 32
	.cfi_offset 13, -32
	pushq	%r12
	.cfi_def_cfa_offset 40
	.cfi_offset 12, -40
	pushq	%rbp
	.cfi_def_cfa_offset 48
	.cfi_offset 6, -48
	pushq	%rbx
	.cfi_def_cfa_offset 56
	.cfi_offset 3, -56
	subq	$136, %rsp
	.cfi_def_cfa_offset 192
	movq	%fs:40, %rax
	movq	%rax, 120(%rsp)
	xorl	%eax, %eax
	movq	.LC21(%rip), %rax
	movb	$5, 64(%rsp)
	movq	$42, 72(%rsp)
	movb	$7, 80(%rsp)
	movq	%rax, 88(%rsp)
.LEHB305:
	call	_Znwm@PLT
.LEHE305:
	movq	%rax, %rbx
	movq	._99(%rip), %rax
	leaq	40(%rsp), %rsi
	leaq	48(%rsp), %rdi
	movq	$0, 104(%rsp)
	movb	$2, 96(%rsp)
	movq	%rbx, 48(%rsp)
	movq	%rax, (%rbx)
	movl	8+._99(%rip), %eax
	movl	%eax, 8(%rbx)
	leaq	12(%rbx), %rax
	movq	%rax, 40(%rsp)
.LEHB306:
	call	_ZN8nlohmann10basic_jsonISt3mapSt6vectorNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEblmdSaNS_14adl_serializerEE6createIS2_ISA_SaISA_EEJN9__gnu_cxx17__normal_iteratorIPKiS2_IiSaIiEEEESK_EEEPT_DpOT0_
.LEHE306:
	leaq	96(%rsp), %r12
	movq	%rbx, %rdi
	movq	%rax, 104(%rsp)
	call	_ZdlPv@PLT
	movl	$2, %esi
	movq	%r12, %rdi
	leaq	80(%rsp), %rbp
	leaq	64(%rsp), %rbx
.LEHB307:
	call	_ZN8nlohmann10basic_jsonISt3mapSt6vectorNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEblmdSaNS_14adl_serializerEEixEm
	leaq	24(%rsp), %rsi
	movq	%rax, %rdi
	leaq	80(%rsp), %rbp
	leaq	64(%rsp), %rbx
	call	_ZN8nlohmann6detail9from_jsonINS_10basic_jsonISt3mapSt6vectorNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEblmdSaNS_14adl_serializerEEEiLi0EEEvRKT_RT0_
	movl	$1, %esi
	movq	%r12, %rdi
	movl	24(%rsp), %r15d
	leaq	80(%rsp), %rbp
	leaq	64(%rsp), %rbx
	call	_ZN8nlohmann10basic_jsonISt3mapSt6vectorNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEblmdSaNS_14adl_serializerEEixEm
	leaq	28(%rsp), %rsi
	movq	%rax, %rdi
	leaq	80(%rsp), %rbp
	leaq	64(%rsp), %rbx
	call	_ZN8nlohmann6detail9from_jsonINS_10basic_jsonISt3mapSt6vectorNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEblmdSaNS_14adl_serializerEEEiLi0EEEvRKT_RT0_
	xorl	%esi, %esi
	movq	%r12, %rdi
	movl	28(%rsp), %r14d
	leaq	80(%rsp), %rbp
	leaq	64(%rsp), %rbx
	call	_ZN8nlohmann10basic_jsonISt3mapSt6vectorNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEblmdSaNS_14adl_serializerEEixEm
	leaq	32(%rsp), %rsi
	movq	%rax, %rdi
	leaq	80(%rsp), %rbp
	leaq	64(%rsp), %rbx
	call	_ZN8nlohmann6detail9from_jsonINS_10basic_jsonISt3mapSt6vectorNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEblmdSaNS_14adl_serializerEEEiLi0EEEvRKT_RT0_
	leaq	80(%rsp), %rbp
	leaq	56(%rsp), %rsi
	movl	32(%rsp), %r13d
	leaq	64(%rsp), %rbx
	movq	%rbp, %rdi
	call	_ZN8nlohmann6detail20get_arithmetic_valueINS_10basic_jsonISt3mapSt6vectorNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEblmdSaNS_14adl_serializerEEEdLi0EEEvRKT_RT0_
	leaq	64(%rsp), %rbx
	movsd	56(%rsp), %xmm1
	leaq	36(%rsp), %rsi
	movq	%rbx, %rdi
	movsd	%xmm1, 8(%rsp)
	call	_ZN8nlohmann6detail9from_jsonINS_10basic_jsonISt3mapSt6vectorNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEblmdSaNS_14adl_serializerEEEiLi0EEEvRKT_RT0_
	movl	36(%rsp), %esi
	leaq	.LC22(%rip), %rdi
	movl	%r15d, %r8d
	movsd	8(%rsp), %xmm0
	movl	%r14d, %ecx
	movl	%r13d, %edx
	movl	$1, %eax
	call	printf@PLT
.LEHE307:
	movzbl	96(%rsp), %esi
	leaq	8(%r12), %rdi
	call	_ZN8nlohmann10basic_jsonISt3mapSt6vectorNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEblmdSaNS_14adl_serializerEE10json_value7destroyENS_6detail7value_tE
	movzbl	80(%rsp), %esi
	leaq	8(%rbp), %rdi
	call	_ZN8nlohmann10basic_jsonISt3mapSt6vectorNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEblmdSaNS_14adl_serializerEE10json_value7destroyENS_6detail7value_tE
	movzbl	64(%rsp), %esi
	leaq	8(%rbx), %rdi
	call	_ZN8nlohmann10basic_jsonISt3mapSt6vectorNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEblmdSaNS_14adl_serializerEE10json_value7destroyENS_6detail7value_tE
	xorl	%eax, %eax
	movq	120(%rsp), %rdx
	xorq	%fs:40, %rdx
	jne	.L1976
	addq	$136, %rsp
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
	ret
.L1976:
	.cfi_restore_state
	call	__stack_chk_fail@PLT
.L1973:
	movzbl	96(%rsp), %esi
	movq	%rax, %r13
	leaq	8(%r12), %rdi
	movq	%r13, %r12
	call	_ZN8nlohmann10basic_jsonISt3mapSt6vectorNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEblmdSaNS_14adl_serializerEE10json_value7destroyENS_6detail7value_tE
.L1968:
	movzbl	80(%rsp), %esi
	leaq	8(%rbp), %rdi
	call	_ZN8nlohmann10basic_jsonISt3mapSt6vectorNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEblmdSaNS_14adl_serializerEE10json_value7destroyENS_6detail7value_tE
	movzbl	64(%rsp), %esi
	leaq	8(%rbx), %rdi
	call	_ZN8nlohmann10basic_jsonISt3mapSt6vectorNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEblmdSaNS_14adl_serializerEE10json_value7destroyENS_6detail7value_tE
	movq	%r12, %rdi
.LEHB308:
	call	_Unwind_Resume@PLT
.LEHE308:
.L1971:
	movq	%rbx, %rdi
	movq	%rax, %r12
	leaq	80(%rsp), %rbp
	call	_ZdlPv@PLT
	leaq	64(%rsp), %rbx
	jmp	.L1968
.L1972:
	movq	%rax, %r12
	leaq	80(%rsp), %rbp
	leaq	64(%rsp), %rbx
	jmp	.L1968
	.cfi_endproc
.LFE6411:
	.section	.gcc_except_table,"a",@progbits
.LLSDA6411:
	.byte	0xff
	.byte	0xff
	.byte	0x1
	.uleb128 .LLSDACSE6411-.LLSDACSB6411
.LLSDACSB6411:
	.uleb128 .LEHB305-.LFB6411
	.uleb128 .LEHE305-.LEHB305
	.uleb128 .L1972-.LFB6411
	.uleb128 0
	.uleb128 .LEHB306-.LFB6411
	.uleb128 .LEHE306-.LEHB306
	.uleb128 .L1971-.LFB6411
	.uleb128 0
	.uleb128 .LEHB307-.LFB6411
	.uleb128 .LEHE307-.LEHB307
	.uleb128 .L1973-.LFB6411
	.uleb128 0
	.uleb128 .LEHB308-.LFB6411
	.uleb128 .LEHE308-.LEHB308
	.uleb128 0
	.uleb128 0
.LLSDACSE6411:
	.section	.text.startup
	.size	main, .-main
	.section	.text._ZNK8nlohmann6detail9exception4whatEv,"axG",@progbits,_ZNK8nlohmann6detail9exception4whatEv,comdat
	.align 2
	.p2align 4,,15
	.weak	_ZNK8nlohmann6detail9exception4whatEv
	.type	_ZNK8nlohmann6detail9exception4whatEv, @function
_ZNK8nlohmann6detail9exception4whatEv:
.LFB4357:
	.cfi_startproc
	addq	$16, %rdi
	jmp	_ZNKSt13runtime_error4whatEv@PLT
	.cfi_endproc
.LFE4357:
	.size	_ZNK8nlohmann6detail9exception4whatEv, .-_ZNK8nlohmann6detail9exception4whatEv
	.section	.text._ZN8nlohmann6detail9exceptionD2Ev,"axG",@progbits,_ZN8nlohmann6detail9exceptionD5Ev,comdat
	.align 2
	.p2align 4,,15
	.weak	_ZN8nlohmann6detail9exceptionD2Ev
	.type	_ZN8nlohmann6detail9exceptionD2Ev, @function
_ZN8nlohmann6detail9exceptionD2Ev:
.LFB4365:
	.cfi_startproc
	leaq	16+_ZTVN8nlohmann6detail9exceptionE(%rip), %rax
	pushq	%rbx
	.cfi_def_cfa_offset 16
	.cfi_offset 3, -16
	movq	%rdi, %rbx
	leaq	16(%rdi), %rdi
	movq	%rax, -16(%rdi)
	call	_ZNSt13runtime_errorD1Ev@PLT
	movq	%rbx, %rdi
	popq	%rbx
	.cfi_def_cfa_offset 8
	jmp	_ZNSt9exceptionD2Ev@PLT
	.cfi_endproc
.LFE4365:
	.size	_ZN8nlohmann6detail9exceptionD2Ev, .-_ZN8nlohmann6detail9exceptionD2Ev
	.weak	_ZN8nlohmann6detail9exceptionD1Ev
	.set	_ZN8nlohmann6detail9exceptionD1Ev,_ZN8nlohmann6detail9exceptionD2Ev
	.section	.text._ZN8nlohmann6detail9exceptionD0Ev,"axG",@progbits,_ZN8nlohmann6detail9exceptionD5Ev,comdat
	.align 2
	.p2align 4,,15
	.weak	_ZN8nlohmann6detail9exceptionD0Ev
	.type	_ZN8nlohmann6detail9exceptionD0Ev, @function
_ZN8nlohmann6detail9exceptionD0Ev:
.LFB4367:
	.cfi_startproc
	leaq	16+_ZTVN8nlohmann6detail9exceptionE(%rip), %rax
	pushq	%rbx
	.cfi_def_cfa_offset 16
	.cfi_offset 3, -16
	movq	%rdi, %rbx
	leaq	16(%rdi), %rdi
	movq	%rax, -16(%rdi)
	call	_ZNSt13runtime_errorD1Ev@PLT
	movq	%rbx, %rdi
	call	_ZNSt9exceptionD2Ev@PLT
	movq	%rbx, %rdi
	movl	$32, %esi
	popq	%rbx
	.cfi_def_cfa_offset 8
	jmp	_ZdlPvm@PLT
	.cfi_endproc
.LFE4367:
	.size	_ZN8nlohmann6detail9exceptionD0Ev, .-_ZN8nlohmann6detail9exceptionD0Ev
	.section	.text._ZN8nlohmann6detail10type_errorD2Ev,"axG",@progbits,_ZN8nlohmann6detail10type_errorD5Ev,comdat
	.align 2
	.p2align 4,,15
	.weak	_ZN8nlohmann6detail10type_errorD2Ev
	.type	_ZN8nlohmann6detail10type_errorD2Ev, @function
_ZN8nlohmann6detail10type_errorD2Ev:
.LFB8874:
	.cfi_startproc
	leaq	16+_ZTVN8nlohmann6detail9exceptionE(%rip), %rax
	pushq	%rbx
	.cfi_def_cfa_offset 16
	.cfi_offset 3, -16
	movq	%rdi, %rbx
	leaq	16(%rdi), %rdi
	movq	%rax, -16(%rdi)
	call	_ZNSt13runtime_errorD1Ev@PLT
	movq	%rbx, %rdi
	popq	%rbx
	.cfi_def_cfa_offset 8
	jmp	_ZNSt9exceptionD2Ev@PLT
	.cfi_endproc
.LFE8874:
	.size	_ZN8nlohmann6detail10type_errorD2Ev, .-_ZN8nlohmann6detail10type_errorD2Ev
	.weak	_ZN8nlohmann6detail10type_errorD1Ev
	.set	_ZN8nlohmann6detail10type_errorD1Ev,_ZN8nlohmann6detail10type_errorD2Ev
	.section	.text._ZN8nlohmann6detail10type_errorD0Ev,"axG",@progbits,_ZN8nlohmann6detail10type_errorD5Ev,comdat
	.align 2
	.p2align 4,,15
	.weak	_ZN8nlohmann6detail10type_errorD0Ev
	.type	_ZN8nlohmann6detail10type_errorD0Ev, @function
_ZN8nlohmann6detail10type_errorD0Ev:
.LFB8876:
	.cfi_startproc
	leaq	16+_ZTVN8nlohmann6detail9exceptionE(%rip), %rax
	pushq	%rbx
	.cfi_def_cfa_offset 16
	.cfi_offset 3, -16
	movq	%rdi, %rbx
	leaq	16(%rdi), %rdi
	movq	%rax, -16(%rdi)
	call	_ZNSt13runtime_errorD1Ev@PLT
	movq	%rbx, %rdi
	call	_ZNSt9exceptionD2Ev@PLT
	movq	%rbx, %rdi
	movl	$32, %esi
	popq	%rbx
	.cfi_def_cfa_offset 8
	jmp	_ZdlPvm@PLT
	.cfi_endproc
.LFE8876:
	.size	_ZN8nlohmann6detail10type_errorD0Ev, .-_ZN8nlohmann6detail10type_errorD0Ev
	.section	.text._ZN8nlohmann6detail11other_errorD2Ev,"axG",@progbits,_ZN8nlohmann6detail11other_errorD5Ev,comdat
	.align 2
	.p2align 4,,15
	.weak	_ZN8nlohmann6detail11other_errorD2Ev
	.type	_ZN8nlohmann6detail11other_errorD2Ev, @function
_ZN8nlohmann6detail11other_errorD2Ev:
.LFB8866:
	.cfi_startproc
	leaq	16+_ZTVN8nlohmann6detail9exceptionE(%rip), %rax
	pushq	%rbx
	.cfi_def_cfa_offset 16
	.cfi_offset 3, -16
	movq	%rdi, %rbx
	leaq	16(%rdi), %rdi
	movq	%rax, -16(%rdi)
	call	_ZNSt13runtime_errorD1Ev@PLT
	movq	%rbx, %rdi
	popq	%rbx
	.cfi_def_cfa_offset 8
	jmp	_ZNSt9exceptionD2Ev@PLT
	.cfi_endproc
.LFE8866:
	.size	_ZN8nlohmann6detail11other_errorD2Ev, .-_ZN8nlohmann6detail11other_errorD2Ev
	.weak	_ZN8nlohmann6detail11other_errorD1Ev
	.set	_ZN8nlohmann6detail11other_errorD1Ev,_ZN8nlohmann6detail11other_errorD2Ev
	.section	.text._ZN8nlohmann6detail11other_errorD0Ev,"axG",@progbits,_ZN8nlohmann6detail11other_errorD5Ev,comdat
	.align 2
	.p2align 4,,15
	.weak	_ZN8nlohmann6detail11other_errorD0Ev
	.type	_ZN8nlohmann6detail11other_errorD0Ev, @function
_ZN8nlohmann6detail11other_errorD0Ev:
.LFB8868:
	.cfi_startproc
	leaq	16+_ZTVN8nlohmann6detail9exceptionE(%rip), %rax
	pushq	%rbx
	.cfi_def_cfa_offset 16
	.cfi_offset 3, -16
	movq	%rdi, %rbx
	leaq	16(%rdi), %rdi
	movq	%rax, -16(%rdi)
	call	_ZNSt13runtime_errorD1Ev@PLT
	movq	%rbx, %rdi
	call	_ZNSt9exceptionD2Ev@PLT
	movq	%rbx, %rdi
	movl	$32, %esi
	popq	%rbx
	.cfi_def_cfa_offset 8
	jmp	_ZdlPvm@PLT
	.cfi_endproc
.LFE8868:
	.size	_ZN8nlohmann6detail11other_errorD0Ev, .-_ZN8nlohmann6detail11other_errorD0Ev
	.section	.rodata
	.align 8
	.type	._99, @object
	.size	._99, 12
._99:
	.long	1
	.long	2
	.long	3
	.weak	_ZTSN8nlohmann6detail9exceptionE
	.section	.rodata._ZTSN8nlohmann6detail9exceptionE,"aG",@progbits,_ZTSN8nlohmann6detail9exceptionE,comdat
	.align 16
	.type	_ZTSN8nlohmann6detail9exceptionE, @object
	.size	_ZTSN8nlohmann6detail9exceptionE, 29
_ZTSN8nlohmann6detail9exceptionE:
	.string	"N8nlohmann6detail9exceptionE"
	.weak	_ZTIN8nlohmann6detail9exceptionE
	.section	.data.rel.ro._ZTIN8nlohmann6detail9exceptionE,"awG",@progbits,_ZTIN8nlohmann6detail9exceptionE,comdat
	.align 8
	.type	_ZTIN8nlohmann6detail9exceptionE, @object
	.size	_ZTIN8nlohmann6detail9exceptionE, 24
_ZTIN8nlohmann6detail9exceptionE:
	.quad	_ZTVN10__cxxabiv120__si_class_type_infoE+16
	.quad	_ZTSN8nlohmann6detail9exceptionE
	.quad	_ZTISt9exception
	.weak	_ZTSN8nlohmann6detail10type_errorE
	.section	.rodata._ZTSN8nlohmann6detail10type_errorE,"aG",@progbits,_ZTSN8nlohmann6detail10type_errorE,comdat
	.align 16
	.type	_ZTSN8nlohmann6detail10type_errorE, @object
	.size	_ZTSN8nlohmann6detail10type_errorE, 31
_ZTSN8nlohmann6detail10type_errorE:
	.string	"N8nlohmann6detail10type_errorE"
	.weak	_ZTIN8nlohmann6detail10type_errorE
	.section	.data.rel.ro._ZTIN8nlohmann6detail10type_errorE,"awG",@progbits,_ZTIN8nlohmann6detail10type_errorE,comdat
	.align 8
	.type	_ZTIN8nlohmann6detail10type_errorE, @object
	.size	_ZTIN8nlohmann6detail10type_errorE, 24
_ZTIN8nlohmann6detail10type_errorE:
	.quad	_ZTVN10__cxxabiv120__si_class_type_infoE+16
	.quad	_ZTSN8nlohmann6detail10type_errorE
	.quad	_ZTIN8nlohmann6detail9exceptionE
	.weak	_ZTVN8nlohmann6detail9exceptionE
	.section	.data.rel.ro.local._ZTVN8nlohmann6detail9exceptionE,"awG",@progbits,_ZTVN8nlohmann6detail9exceptionE,comdat
	.align 8
	.type	_ZTVN8nlohmann6detail9exceptionE, @object
	.size	_ZTVN8nlohmann6detail9exceptionE, 40
_ZTVN8nlohmann6detail9exceptionE:
	.quad	0
	.quad	_ZTIN8nlohmann6detail9exceptionE
	.quad	_ZN8nlohmann6detail9exceptionD1Ev
	.quad	_ZN8nlohmann6detail9exceptionD0Ev
	.quad	_ZNK8nlohmann6detail9exception4whatEv
	.weak	_ZTVN8nlohmann6detail10type_errorE
	.section	.data.rel.ro.local._ZTVN8nlohmann6detail10type_errorE,"awG",@progbits,_ZTVN8nlohmann6detail10type_errorE,comdat
	.align 8
	.type	_ZTVN8nlohmann6detail10type_errorE, @object
	.size	_ZTVN8nlohmann6detail10type_errorE, 40
_ZTVN8nlohmann6detail10type_errorE:
	.quad	0
	.quad	_ZTIN8nlohmann6detail10type_errorE
	.quad	_ZN8nlohmann6detail10type_errorD1Ev
	.quad	_ZN8nlohmann6detail10type_errorD0Ev
	.quad	_ZNK8nlohmann6detail9exception4whatEv
	.section	.rodata.cst8,"aM",@progbits,8
	.align 8
.LC21:
	.long	1293080650
	.long	1074340347
	.hidden	DW.ref.__gxx_personality_v0
	.weak	DW.ref.__gxx_personality_v0
	.section	.data.DW.ref.__gxx_personality_v0,"awG",@progbits,DW.ref.__gxx_personality_v0,comdat
	.align 8
	.type	DW.ref.__gxx_personality_v0, @object
	.size	DW.ref.__gxx_personality_v0, 8
DW.ref.__gxx_personality_v0:
	.quad	__gxx_personality_v0
	.ident	"GCC: (GNU) 7.3.0"
	.section	.note.GNU-stack,"",@progbits
