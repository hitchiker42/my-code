	.file	"json.cpp"
	.text
	.section	.text._ZNSt10_HashtableINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESt4pairIKS5_10json_valueESaIS9_ENSt8__detail10_Select1stESt8equal_toIS5_ESt4hashIS5_ENSB_18_Mod_range_hashingENSB_20_Default_ranged_hashENSB_20_Prime_rehash_policyENSB_17_Hashtable_traitsILb1ELb0ELb1EEEED2Ev,"axG",@progbits,_ZNSt10_HashtableINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESt4pairIKS5_10json_valueESaIS9_ENSt8__detail10_Select1stESt8equal_toIS5_ESt4hashIS5_ENSB_18_Mod_range_hashingENSB_20_Default_ranged_hashENSB_20_Prime_rehash_policyENSB_17_Hashtable_traitsILb1ELb0ELb1EEEED5Ev,comdat
	.align 2
	.p2align 4,,15
	.weak	_ZNSt10_HashtableINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESt4pairIKS5_10json_valueESaIS9_ENSt8__detail10_Select1stESt8equal_toIS5_ESt4hashIS5_ENSB_18_Mod_range_hashingENSB_20_Default_ranged_hashENSB_20_Prime_rehash_policyENSB_17_Hashtable_traitsILb1ELb0ELb1EEEED2Ev
	.type	_ZNSt10_HashtableINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESt4pairIKS5_10json_valueESaIS9_ENSt8__detail10_Select1stESt8equal_toIS5_ESt4hashIS5_ENSB_18_Mod_range_hashingENSB_20_Default_ranged_hashENSB_20_Prime_rehash_policyENSB_17_Hashtable_traitsILb1ELb0ELb1EEEED2Ev, @function
_ZNSt10_HashtableINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESt4pairIKS5_10json_valueESaIS9_ENSt8__detail10_Select1stESt8equal_toIS5_ESt4hashIS5_ENSB_18_Mod_range_hashingENSB_20_Default_ranged_hashENSB_20_Prime_rehash_policyENSB_17_Hashtable_traitsILb1ELb0ELb1EEEED2Ev:
.LFB2838:
	.cfi_startproc
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
	subq	$200, %rsp
	.cfi_def_cfa_offset 256
	movq	16(%rdi), %r15
	testq	%r15, %r15
	je	.L2
	movq	%rdi, 8(%rsp)
	movq	%r15, %r14
	jmp	.L85
	.p2align 4,,10
	.p2align 3
.L210:
	jb	.L5
	cmpl	$2, %edx
	je	.L6
	cmpl	$3, %edx
	jne	.L3
	andq	$-8, %rdi
	movl	$8, %esi
	call	_ZdlPvm@PLT
.L3:
	movq	8(%r14), %rdi
	leaq	24(%r14), %rdx
	cmpq	%rdx, %rdi
	je	.L84
	call	_ZdlPv@PLT
.L84:
	movq	%r14, %rdi
	movq	%r15, %r14
	call	_ZdlPv@PLT
	testq	%r15, %r15
	je	.L209
.L85:
	movq	40(%r14), %rdi
	movq	(%r14), %r15
	movl	%edi, %edx
	andl	$7, %edx
	cmpl	$1, %edx
	jne	.L210
	andq	$-8, %rdi
	movq	%rdi, (%rsp)
	je	.L3
	movq	8(%rdi), %r12
	movq	(%rdi), %r13
	cmpq	%r13, %r12
	je	.L8
	movq	%r13, %rax
	movq	%r14, 24(%rsp)
	movq	%r12, %r13
	movq	%r15, 32(%rsp)
	movq	%rax, %r12
	jmp	.L81
	.p2align 4,,10
	.p2align 3
.L212:
	jb	.L11
	cmpl	$2, %edx
	je	.L12
	cmpl	$3, %edx
	jne	.L9
	andq	$-8, %rdi
	movl	$8, %esi
	call	_ZdlPvm@PLT
.L9:
	addq	$8, %r12
	cmpq	%r12, %r13
	je	.L211
.L81:
	movq	(%r12), %rdi
	movl	%edi, %edx
	andl	$7, %edx
	cmpl	$1, %edx
	jne	.L212
	andq	$-8, %rdi
	movq	%rdi, 16(%rsp)
	je	.L9
	movq	8(%rdi), %r14
	movq	(%rdi), %r15
	cmpq	%r15, %r14
	je	.L14
	movq	%r12, 56(%rsp)
	movq	%r13, 48(%rsp)
	movq	%r15, %r12
	movq	%r14, %r15
	jmp	.L78
	.p2align 4,,10
	.p2align 3
.L214:
	jb	.L17
	cmpl	$2, %edx
	je	.L18
	cmpl	$3, %edx
	jne	.L15
	andq	$-8, %rdi
	movl	$8, %esi
	call	_ZdlPvm@PLT
.L15:
	addq	$8, %r12
	cmpq	%r12, %r15
	je	.L213
.L78:
	movq	(%r12), %rdi
	movl	%edi, %edx
	andl	$7, %edx
	cmpl	$1, %edx
	jne	.L214
	andq	$-8, %rdi
	movq	%rdi, 40(%rsp)
	je	.L15
	movq	8(%rdi), %r13
	movq	(%rdi), %r14
	cmpq	%r14, %r13
	je	.L20
	movq	%r15, 72(%rsp)
	movq	%r12, 80(%rsp)
	movq	%r14, %r15
	jmp	.L75
	.p2align 4,,10
	.p2align 3
.L216:
	jb	.L23
	cmpl	$2, %edx
	je	.L24
	cmpl	$3, %edx
	jne	.L21
	andq	$-8, %rdi
	movl	$8, %esi
	call	_ZdlPvm@PLT
.L21:
	addq	$8, %r15
	cmpq	%r15, %r13
	je	.L215
.L75:
	movq	(%r15), %rdi
	movl	%edi, %edx
	andl	$7, %edx
	cmpl	$1, %edx
	jne	.L216
	andq	$-8, %rdi
	movq	%rdi, 64(%rsp)
	je	.L21
	movq	8(%rdi), %r14
	movq	(%rdi), %r12
	cmpq	%r12, %r14
	je	.L26
	movq	%r15, 120(%rsp)
	movq	%r13, 112(%rsp)
	movq	%r12, %r15
	jmp	.L72
	.p2align 4,,10
	.p2align 3
.L218:
	jb	.L29
	cmpl	$2, %eax
	je	.L30
	cmpl	$3, %eax
	jne	.L27
	andq	$-8, %rdi
	movl	$8, %esi
	call	_ZdlPvm@PLT
	.p2align 4,,10
	.p2align 3
.L27:
	addq	$8, %r15
	cmpq	%r15, %r14
	je	.L217
.L72:
	movq	(%r15), %rdi
	movl	%edi, %eax
	andl	$7, %eax
	cmpl	$1, %eax
	jne	.L218
	andq	$-8, %rdi
	movq	%rdi, 96(%rsp)
	je	.L27
	movq	8(%rdi), %rcx
	movq	(%rdi), %r13
	cmpq	%r13, %rcx
	movq	%rcx, 88(%rsp)
	je	.L32
	movq	%r14, 152(%rsp)
	movq	%r15, 160(%rsp)
	movq	%r13, %r14
	jmp	.L69
.L220:
	cmpl	$2, %eax
	je	.L36
	cmpl	$3, %eax
	jne	.L33
	andq	$-8, %rdi
	movl	$8, %esi
	call	_ZdlPvm@PLT
.L33:
	addq	$8, %r14
	cmpq	%r14, 88(%rsp)
	je	.L219
.L69:
	movq	(%r14), %rdi
	movl	%edi, %eax
	andl	$7, %eax
	cmpl	$1, %eax
	je	.L34
	jnb	.L220
	movq	%rdi, %rbx
	andq	$-8, %rbx
	je	.L33
	movq	%rbx, %rdi
	call	_ZNSt10_HashtableINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESt4pairIKS5_10json_valueESaIS9_ENSt8__detail10_Select1stESt8equal_toIS5_ESt4hashIS5_ENSB_18_Mod_range_hashingENSB_20_Default_ranged_hashENSB_20_Prime_rehash_policyENSB_17_Hashtable_traitsILb1ELb0ELb1EEEED1Ev
	movl	$56, %esi
	movq	%rbx, %rdi
	call	_ZdlPvm@PLT
	jmp	.L33
	.p2align 4,,10
	.p2align 3
.L6:
	movq	%rdi, %rbx
	andq	$-8, %rbx
	je	.L3
	movq	(%rbx), %rdi
	leaq	16(%rbx), %rdx
	cmpq	%rdx, %rdi
	je	.L83
	call	_ZdlPv@PLT
.L83:
	movl	$32, %esi
	movq	%rbx, %rdi
	call	_ZdlPvm@PLT
	jmp	.L3
	.p2align 4,,10
	.p2align 3
.L5:
	movq	%rdi, %rbx
	andq	$-8, %rbx
	je	.L3
	movq	%rbx, %rdi
	call	_ZNSt10_HashtableINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESt4pairIKS5_10json_valueESaIS9_ENSt8__detail10_Select1stESt8equal_toIS5_ESt4hashIS5_ENSB_18_Mod_range_hashingENSB_20_Default_ranged_hashENSB_20_Prime_rehash_policyENSB_17_Hashtable_traitsILb1ELb0ELb1EEEED1Ev
	movl	$56, %esi
	movq	%rbx, %rdi
	call	_ZdlPvm@PLT
	jmp	.L3
	.p2align 4,,10
	.p2align 3
.L209:
	movq	8(%rsp), %r14
.L2:
	movq	8(%r14), %rax
	movq	(%r14), %rdi
	xorl	%esi, %esi
	leaq	0(,%rax,8), %rdx
	call	memset@PLT
	movq	(%r14), %rdi
	leaq	48(%r14), %rax
	movq	$0, 24(%r14)
	movq	$0, 16(%r14)
	cmpq	%rax, %rdi
	je	.L1
	addq	$200, %rsp
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
.L12:
	.cfi_restore_state
	movq	%rdi, %rbx
	andq	$-8, %rbx
	je	.L9
	movq	(%rbx), %rdi
	leaq	16(%rbx), %rdx
	cmpq	%rdx, %rdi
	je	.L80
	call	_ZdlPv@PLT
.L80:
	movl	$32, %esi
	movq	%rbx, %rdi
	addq	$8, %r12
	call	_ZdlPvm@PLT
	cmpq	%r12, %r13
	jne	.L81
	.p2align 4,,10
	.p2align 3
.L211:
	movq	(%rsp), %rax
	movq	24(%rsp), %r14
	movq	32(%rsp), %r15
	movq	(%rax), %r13
.L8:
	testq	%r13, %r13
	je	.L82
	movq	%r13, %rdi
	call	_ZdlPv@PLT
.L82:
	movq	(%rsp), %rdi
	movl	$24, %esi
	call	_ZdlPvm@PLT
	jmp	.L3
	.p2align 4,,10
	.p2align 3
.L11:
	movq	%rdi, %rbx
	andq	$-8, %rbx
	je	.L9
	movq	%rbx, %rdi
	call	_ZNSt10_HashtableINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESt4pairIKS5_10json_valueESaIS9_ENSt8__detail10_Select1stESt8equal_toIS5_ESt4hashIS5_ENSB_18_Mod_range_hashingENSB_20_Default_ranged_hashENSB_20_Prime_rehash_policyENSB_17_Hashtable_traitsILb1ELb0ELb1EEEED1Ev
	movl	$56, %esi
	movq	%rbx, %rdi
	call	_ZdlPvm@PLT
	jmp	.L9
.L215:
	movq	40(%rsp), %rax
	movq	72(%rsp), %r15
	movq	80(%rsp), %r12
	movq	(%rax), %r14
.L20:
	testq	%r14, %r14
	je	.L76
	movq	%r14, %rdi
	call	_ZdlPv@PLT
.L76:
	movq	40(%rsp), %rdi
	movl	$24, %esi
	call	_ZdlPvm@PLT
	jmp	.L15
	.p2align 4,,10
	.p2align 3
.L18:
	movq	%rdi, %rbx
	andq	$-8, %rbx
	je	.L15
	movq	(%rbx), %rdi
	leaq	16(%rbx), %rdx
	cmpq	%rdx, %rdi
	je	.L77
	call	_ZdlPv@PLT
.L77:
	movl	$32, %esi
	movq	%rbx, %rdi
	call	_ZdlPvm@PLT
	jmp	.L15
	.p2align 4,,10
	.p2align 3
.L17:
	movq	%rdi, %rbx
	andq	$-8, %rbx
	je	.L15
	movq	%rbx, %rdi
	call	_ZNSt10_HashtableINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESt4pairIKS5_10json_valueESaIS9_ENSt8__detail10_Select1stESt8equal_toIS5_ESt4hashIS5_ENSB_18_Mod_range_hashingENSB_20_Default_ranged_hashENSB_20_Prime_rehash_policyENSB_17_Hashtable_traitsILb1ELb0ELb1EEEED1Ev
	movl	$56, %esi
	movq	%rbx, %rdi
	call	_ZdlPvm@PLT
	jmp	.L15
	.p2align 4,,10
	.p2align 3
.L213:
	movq	16(%rsp), %rax
	movq	48(%rsp), %r13
	movq	56(%rsp), %r12
	movq	(%rax), %r15
.L14:
	testq	%r15, %r15
	je	.L79
	movq	%r15, %rdi
	call	_ZdlPv@PLT
.L79:
	movq	16(%rsp), %rdi
	movl	$24, %esi
	call	_ZdlPvm@PLT
	jmp	.L9
	.p2align 4,,10
	.p2align 3
.L1:
	addq	$200, %rsp
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
.L24:
	.cfi_restore_state
	movq	%rdi, %rbx
	andq	$-8, %rbx
	je	.L21
	movq	(%rbx), %rdi
	leaq	16(%rbx), %rdx
	cmpq	%rdx, %rdi
	je	.L74
	call	_ZdlPv@PLT
.L74:
	movl	$32, %esi
	movq	%rbx, %rdi
	call	_ZdlPvm@PLT
	jmp	.L21
	.p2align 4,,10
	.p2align 3
.L23:
	movq	%rdi, %rbx
	andq	$-8, %rbx
	je	.L21
	movq	%rbx, %rdi
	call	_ZNSt10_HashtableINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESt4pairIKS5_10json_valueESaIS9_ENSt8__detail10_Select1stESt8equal_toIS5_ESt4hashIS5_ENSB_18_Mod_range_hashingENSB_20_Default_ranged_hashENSB_20_Prime_rehash_policyENSB_17_Hashtable_traitsILb1ELb0ELb1EEEED1Ev
	movl	$56, %esi
	movq	%rbx, %rdi
	call	_ZdlPvm@PLT
	jmp	.L21
.L29:
	movq	%rdi, %rbx
	andq	$-8, %rbx
	je	.L27
	movq	%rbx, %rdi
	call	_ZNSt10_HashtableINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESt4pairIKS5_10json_valueESaIS9_ENSt8__detail10_Select1stESt8equal_toIS5_ESt4hashIS5_ENSB_18_Mod_range_hashingENSB_20_Default_ranged_hashENSB_20_Prime_rehash_policyENSB_17_Hashtable_traitsILb1ELb0ELb1EEEED1Ev
	movl	$56, %esi
	movq	%rbx, %rdi
	call	_ZdlPvm@PLT
	jmp	.L27
.L30:
	movq	%rdi, %rbx
	andq	$-8, %rbx
	je	.L27
	movq	(%rbx), %rdi
	leaq	16(%rbx), %rax
	cmpq	%rax, %rdi
	je	.L71
	call	_ZdlPv@PLT
.L71:
	movl	$32, %esi
	movq	%rbx, %rdi
	call	_ZdlPvm@PLT
	jmp	.L27
.L217:
	movq	64(%rsp), %rax
	movq	112(%rsp), %r13
	movq	120(%rsp), %r15
	movq	(%rax), %r12
.L26:
	testq	%r12, %r12
	je	.L73
	movq	%r12, %rdi
	call	_ZdlPv@PLT
.L73:
	movq	64(%rsp), %rdi
	movl	$24, %esi
	call	_ZdlPvm@PLT
	jmp	.L21
.L36:
	movq	%rdi, %rbx
	andq	$-8, %rbx
	je	.L33
	movq	(%rbx), %rdi
	leaq	16(%rbx), %rax
	cmpq	%rax, %rdi
	je	.L68
	call	_ZdlPv@PLT
.L68:
	movl	$32, %esi
	movq	%rbx, %rdi
	call	_ZdlPvm@PLT
	jmp	.L33
.L34:
	andq	$-8, %rdi
	movq	%rdi, 104(%rsp)
	je	.L33
	movq	8(%rdi), %rcx
	movq	(%rdi), %r15
	cmpq	%r15, %rcx
	movq	%rcx, 128(%rsp)
	je	.L38
	movq	%r14, 168(%rsp)
	jmp	.L66
.L222:
	jb	.L41
	cmpl	$2, %eax
	je	.L42
	cmpl	$3, %eax
	jne	.L39
	andq	$-8, %rdi
	movl	$8, %esi
	call	_ZdlPvm@PLT
.L39:
	addq	$8, %r15
	cmpq	%r15, 128(%rsp)
	je	.L221
.L66:
	movq	(%r15), %rdi
	movl	%edi, %eax
	andl	$7, %eax
	cmpl	$1, %eax
	jne	.L222
	andq	$-8, %rdi
	movq	%rdi, %r13
	je	.L39
	movq	8(%rdi), %rax
	movq	(%rdi), %rbp
	cmpq	%rbp, %rax
	movq	%rax, 136(%rsp)
	je	.L44
	movq	%rdi, 176(%rsp)
	jmp	.L63
.L224:
	jb	.L47
	cmpl	$2, %eax
	je	.L48
	cmpl	$3, %eax
	jne	.L45
	andq	$-8, %rbx
	movl	$8, %esi
	movq	%rbx, %rdi
	call	_ZdlPvm@PLT
.L45:
	addq	$8, %rbp
	cmpq	%rbp, 136(%rsp)
	je	.L223
.L63:
	movq	0(%rbp), %rbx
	movl	%ebx, %eax
	andl	$7, %eax
	cmpl	$1, %eax
	jne	.L224
	andq	$-8, %rbx
	je	.L45
	movq	8(%rbx), %r13
	movq	(%rbx), %r12
	cmpq	%r12, %r13
	jne	.L60
	jmp	.L50
	.p2align 4,,10
	.p2align 3
.L225:
	jb	.L53
	cmpl	$2, %eax
	je	.L54
	cmpl	$3, %eax
	jne	.L51
	andq	$-8, %rdi
	movl	$8, %esi
	call	_ZdlPvm@PLT
.L51:
	addq	$8, %r12
	cmpq	%r12, %r13
	je	.L50
.L60:
	movq	(%r12), %rdi
	movl	%edi, %eax
	andl	$7, %eax
	cmpl	$1, %eax
	jne	.L225
	andq	$-8, %rdi
	movq	%rdi, %r14
	je	.L51
	movq	8(%rdi), %rax
	movq	(%rdi), %rdx
	movq	%rax, 144(%rsp)
	jmp	.L57
.L226:
	movq	%rdx, %rdi
	movq	%rdx, 184(%rsp)
	call	_ZN10json_valueD1Ev
	movq	184(%rsp), %rdx
	addq	$8, %rdx
.L57:
	cmpq	%rdx, 144(%rsp)
	jne	.L226
	movq	(%r14), %rdi
	testq	%rdi, %rdi
	je	.L58
	call	_ZdlPv@PLT
.L58:
	movl	$24, %esi
	movq	%r14, %rdi
	call	_ZdlPvm@PLT
	jmp	.L51
	.p2align 4,,10
	.p2align 3
.L219:
	movq	96(%rsp), %rax
	movq	152(%rsp), %r14
	movq	160(%rsp), %r15
	movq	(%rax), %r13
.L32:
	testq	%r13, %r13
	je	.L70
	movq	%r13, %rdi
	call	_ZdlPv@PLT
.L70:
	movq	96(%rsp), %rdi
	movl	$24, %esi
	call	_ZdlPvm@PLT
	jmp	.L27
.L42:
	movq	%rdi, %rbx
	andq	$-8, %rbx
	je	.L39
	movq	(%rbx), %rdi
	leaq	16(%rbx), %rax
	cmpq	%rax, %rdi
	je	.L65
	call	_ZdlPv@PLT
.L65:
	movl	$32, %esi
	movq	%rbx, %rdi
	call	_ZdlPvm@PLT
	jmp	.L39
.L41:
	movq	%rdi, %rbx
	andq	$-8, %rbx
	je	.L39
	movq	%rbx, %rdi
	call	_ZNSt10_HashtableINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESt4pairIKS5_10json_valueESaIS9_ENSt8__detail10_Select1stESt8equal_toIS5_ESt4hashIS5_ENSB_18_Mod_range_hashingENSB_20_Default_ranged_hashENSB_20_Prime_rehash_policyENSB_17_Hashtable_traitsILb1ELb0ELb1EEEED1Ev
	movl	$56, %esi
	movq	%rbx, %rdi
	call	_ZdlPvm@PLT
	jmp	.L39
.L221:
	movq	168(%rsp), %r14
.L38:
	movq	104(%rsp), %rax
	movq	(%rax), %rdi
	testq	%rdi, %rdi
	je	.L67
	call	_ZdlPv@PLT
.L67:
	movq	104(%rsp), %rdi
	movl	$24, %esi
	call	_ZdlPvm@PLT
	jmp	.L33
.L223:
	movq	176(%rsp), %r13
.L44:
	movq	0(%r13), %rdi
	testq	%rdi, %rdi
	je	.L64
	call	_ZdlPv@PLT
.L64:
	movl	$24, %esi
	movq	%r13, %rdi
	call	_ZdlPvm@PLT
	jmp	.L39
.L48:
	andq	$-8, %rbx
	je	.L45
	movq	(%rbx), %rdi
	leaq	16(%rbx), %rax
	cmpq	%rax, %rdi
	je	.L62
	call	_ZdlPv@PLT
.L62:
	movl	$32, %esi
	movq	%rbx, %rdi
	call	_ZdlPvm@PLT
	jmp	.L45
.L47:
	andq	$-8, %rbx
	je	.L45
	movq	%rbx, %rdi
	call	_ZNSt10_HashtableINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESt4pairIKS5_10json_valueESaIS9_ENSt8__detail10_Select1stESt8equal_toIS5_ESt4hashIS5_ENSB_18_Mod_range_hashingENSB_20_Default_ranged_hashENSB_20_Prime_rehash_policyENSB_17_Hashtable_traitsILb1ELb0ELb1EEEED1Ev
	movl	$56, %esi
	movq	%rbx, %rdi
	call	_ZdlPvm@PLT
	jmp	.L45
.L50:
	movq	(%rbx), %rdi
	testq	%rdi, %rdi
	je	.L61
	call	_ZdlPv@PLT
.L61:
	movl	$24, %esi
	movq	%rbx, %rdi
	call	_ZdlPvm@PLT
	jmp	.L45
.L54:
	andq	$-8, %rdi
	movq	%rdi, %r14
	je	.L51
	movq	(%rdi), %rdi
	leaq	16(%r14), %rax
	cmpq	%rax, %rdi
	je	.L59
	call	_ZdlPv@PLT
.L59:
	movl	$32, %esi
	movq	%r14, %rdi
	call	_ZdlPvm@PLT
	jmp	.L51
.L53:
	andq	$-8, %rdi
	movq	%rdi, %r14
	je	.L51
	call	_ZNSt10_HashtableINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESt4pairIKS5_10json_valueESaIS9_ENSt8__detail10_Select1stESt8equal_toIS5_ESt4hashIS5_ENSB_18_Mod_range_hashingENSB_20_Default_ranged_hashENSB_20_Prime_rehash_policyENSB_17_Hashtable_traitsILb1ELb0ELb1EEEED1Ev
	movl	$56, %esi
	movq	%r14, %rdi
	call	_ZdlPvm@PLT
	jmp	.L51
	.cfi_endproc
.LFE2838:
	.size	_ZNSt10_HashtableINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESt4pairIKS5_10json_valueESaIS9_ENSt8__detail10_Select1stESt8equal_toIS5_ESt4hashIS5_ENSB_18_Mod_range_hashingENSB_20_Default_ranged_hashENSB_20_Prime_rehash_policyENSB_17_Hashtable_traitsILb1ELb0ELb1EEEED2Ev, .-_ZNSt10_HashtableINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESt4pairIKS5_10json_valueESaIS9_ENSt8__detail10_Select1stESt8equal_toIS5_ESt4hashIS5_ENSB_18_Mod_range_hashingENSB_20_Default_ranged_hashENSB_20_Prime_rehash_policyENSB_17_Hashtable_traitsILb1ELb0ELb1EEEED2Ev
	.weak	_ZNSt10_HashtableINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESt4pairIKS5_10json_valueESaIS9_ENSt8__detail10_Select1stESt8equal_toIS5_ESt4hashIS5_ENSB_18_Mod_range_hashingENSB_20_Default_ranged_hashENSB_20_Prime_rehash_policyENSB_17_Hashtable_traitsILb1ELb0ELb1EEEED1Ev
	.set	_ZNSt10_HashtableINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESt4pairIKS5_10json_valueESaIS9_ENSt8__detail10_Select1stESt8equal_toIS5_ESt4hashIS5_ENSB_18_Mod_range_hashingENSB_20_Default_ranged_hashENSB_20_Prime_rehash_policyENSB_17_Hashtable_traitsILb1ELb0ELb1EEEED1Ev,_ZNSt10_HashtableINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESt4pairIKS5_10json_valueESaIS9_ENSt8__detail10_Select1stESt8equal_toIS5_ESt4hashIS5_ENSB_18_Mod_range_hashingENSB_20_Default_ranged_hashENSB_20_Prime_rehash_policyENSB_17_Hashtable_traitsILb1ELb0ELb1EEEED2Ev
	.section	.text._ZN10json_valueD2Ev,"axG",@progbits,_ZN10json_valueD5Ev,comdat
	.align 2
	.p2align 4,,15
	.weak	_ZN10json_valueD2Ev
	.type	_ZN10json_valueD2Ev, @function
_ZN10json_valueD2Ev:
.LFB2576:
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
	pushq	%rbp
	.cfi_def_cfa_offset 48
	.cfi_offset 6, -48
	pushq	%rbx
	.cfi_def_cfa_offset 56
	.cfi_offset 3, -56
	subq	$40, %rsp
	.cfi_def_cfa_offset 96
	movq	(%rdi), %rdi
	movl	%edi, %eax
	andl	$7, %eax
	cmpl	$1, %eax
	je	.L229
	jb	.L230
	cmpl	$2, %eax
	je	.L231
	cmpl	$3, %eax
	jne	.L227
	andq	$-8, %rdi
	movl	$8, %esi
.L295:
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
	jmp	_ZdlPvm@PLT
	.p2align 4,,10
	.p2align 3
.L231:
	.cfi_restore_state
	movq	%rdi, %rbx
	andq	$-8, %rbx
	je	.L227
	movq	(%rbx), %rdi
	leaq	16(%rbx), %rax
	cmpq	%rax, %rdi
	je	.L254
	call	_ZdlPv@PLT
.L254:
	movl	$32, %esi
	movq	%rbx, %rdi
	jmp	.L295
	.p2align 4,,10
	.p2align 3
.L229:
	movq	%rdi, %rbx
	andq	$-8, %rbx
	jne	.L296
.L227:
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
.L230:
	.cfi_restore_state
	movq	%rdi, %rbx
	andq	$-8, %rbx
	je	.L227
	movq	%rbx, %rdi
	call	_ZNSt10_HashtableINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESt4pairIKS5_10json_valueESaIS9_ENSt8__detail10_Select1stESt8equal_toIS5_ESt4hashIS5_ENSB_18_Mod_range_hashingENSB_20_Default_ranged_hashENSB_20_Prime_rehash_policyENSB_17_Hashtable_traitsILb1ELb0ELb1EEEED1Ev
	movl	$56, %esi
	movq	%rbx, %rdi
	jmp	.L295
	.p2align 4,,10
	.p2align 3
.L296:
	movq	8(%rbx), %r12
	movq	(%rbx), %r15
	cmpq	%r15, %r12
	jne	.L252
	jmp	.L233
	.p2align 4,,10
	.p2align 3
.L298:
	jb	.L236
	cmpl	$2, %edx
	je	.L237
	cmpl	$3, %edx
	jne	.L234
	andq	$-8, %rdi
	movl	$8, %esi
	call	_ZdlPvm@PLT
.L234:
	addq	$8, %r15
	cmpq	%r15, %r12
	je	.L297
.L252:
	movq	(%r15), %rdi
	movl	%edi, %edx
	andl	$7, %edx
	cmpl	$1, %edx
	jne	.L298
	andq	$-8, %rdi
	movq	%rdi, 8(%rsp)
	je	.L234
	movq	8(%rdi), %r14
	movq	(%rdi), %rbp
	cmpq	%rbp, %r14
	jne	.L249
	jmp	.L239
	.p2align 4,,10
	.p2align 3
.L300:
	jb	.L242
	cmpl	$2, %ecx
	je	.L243
	cmpl	$3, %ecx
	jne	.L240
	movq	%r13, %rdi
	movl	$8, %esi
	andq	$-8, %rdi
	call	_ZdlPvm@PLT
.L240:
	addq	$8, %rbp
	cmpq	%rbp, %r14
	je	.L299
.L249:
	movq	0(%rbp), %r13
	movl	%r13d, %ecx
	andl	$7, %ecx
	cmpl	$1, %ecx
	jne	.L300
	andq	$-8, %r13
	movq	%r13, 24(%rsp)
	je	.L240
	movq	8(%r13), %rsi
	movq	0(%r13), %r13
	cmpq	%r13, %rsi
	movq	%rsi, 16(%rsp)
	je	.L245
	.p2align 4,,10
	.p2align 3
.L246:
	movq	%r13, %rdi
	addq	$8, %r13
	call	_ZN10json_valueD1Ev
	cmpq	%r13, 16(%rsp)
	jne	.L246
	movq	24(%rsp), %rax
	movq	(%rax), %r13
.L245:
	testq	%r13, %r13
	je	.L247
	movq	%r13, %rdi
	call	_ZdlPv@PLT
.L247:
	movq	24(%rsp), %rdi
	movl	$24, %esi
	call	_ZdlPvm@PLT
	jmp	.L240
	.p2align 4,,10
	.p2align 3
.L237:
	andq	$-8, %rdi
	movq	%rdi, %rbp
	je	.L234
	movq	(%rdi), %rdi
	leaq	16(%rbp), %rdx
	cmpq	%rdx, %rdi
	je	.L251
	call	_ZdlPv@PLT
.L251:
	movl	$32, %esi
	movq	%rbp, %rdi
	call	_ZdlPvm@PLT
	jmp	.L234
	.p2align 4,,10
	.p2align 3
.L236:
	andq	$-8, %rdi
	movq	%rdi, %rbp
	je	.L234
	call	_ZNSt10_HashtableINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESt4pairIKS5_10json_valueESaIS9_ENSt8__detail10_Select1stESt8equal_toIS5_ESt4hashIS5_ENSB_18_Mod_range_hashingENSB_20_Default_ranged_hashENSB_20_Prime_rehash_policyENSB_17_Hashtable_traitsILb1ELb0ELb1EEEED1Ev
	movl	$56, %esi
	movq	%rbp, %rdi
	call	_ZdlPvm@PLT
	jmp	.L234
	.p2align 4,,10
	.p2align 3
.L297:
	movq	(%rbx), %r15
.L233:
	testq	%r15, %r15
	je	.L253
	movq	%r15, %rdi
	call	_ZdlPv@PLT
.L253:
	movl	$24, %esi
	movq	%rbx, %rdi
	jmp	.L295
	.p2align 4,,10
	.p2align 3
.L243:
	andq	$-8, %r13
	je	.L240
	movq	0(%r13), %rdi
	leaq	16(%r13), %rcx
	cmpq	%rcx, %rdi
	je	.L248
	call	_ZdlPv@PLT
.L248:
	movl	$32, %esi
	movq	%r13, %rdi
	call	_ZdlPvm@PLT
	jmp	.L240
	.p2align 4,,10
	.p2align 3
.L242:
	andq	$-8, %r13
	je	.L240
	movq	%r13, %rdi
	call	_ZNSt10_HashtableINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESt4pairIKS5_10json_valueESaIS9_ENSt8__detail10_Select1stESt8equal_toIS5_ESt4hashIS5_ENSB_18_Mod_range_hashingENSB_20_Default_ranged_hashENSB_20_Prime_rehash_policyENSB_17_Hashtable_traitsILb1ELb0ELb1EEEED1Ev
	movl	$56, %esi
	movq	%r13, %rdi
	call	_ZdlPvm@PLT
	jmp	.L240
	.p2align 4,,10
	.p2align 3
.L299:
	movq	8(%rsp), %rax
	movq	(%rax), %rbp
.L239:
	testq	%rbp, %rbp
	je	.L250
	movq	%rbp, %rdi
	call	_ZdlPv@PLT
.L250:
	movq	8(%rsp), %rdi
	movl	$24, %esi
	call	_ZdlPvm@PLT
	jmp	.L234
	.cfi_endproc
.LFE2576:
	.size	_ZN10json_valueD2Ev, .-_ZN10json_valueD2Ev
	.weak	_ZN10json_valueD1Ev
	.set	_ZN10json_valueD1Ev,_ZN10json_valueD2Ev
	.section	.text._ZNSt6vectorI10json_valueSaIS0_EED2Ev,"axG",@progbits,_ZNSt6vectorI10json_valueSaIS0_EED5Ev,comdat
	.align 2
	.p2align 4,,15
	.weak	_ZNSt6vectorI10json_valueSaIS0_EED2Ev
	.type	_ZNSt6vectorI10json_valueSaIS0_EED2Ev, @function
_ZNSt6vectorI10json_valueSaIS0_EED2Ev:
.LFB2841:
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
	pushq	%rbp
	.cfi_def_cfa_offset 48
	.cfi_offset 6, -48
	pushq	%rbx
	.cfi_def_cfa_offset 56
	.cfi_offset 3, -56
	subq	$200, %rsp
	.cfi_def_cfa_offset 256
	movq	8(%rdi), %r14
	movq	(%rdi), %r15
	cmpq	%r15, %r14
	je	.L302
	movq	%r15, %rax
	movq	%rdi, 8(%rsp)
	movq	%r14, %r15
	movq	%rax, %r14
	jmp	.L384
	.p2align 4,,10
	.p2align 3
.L505:
	jb	.L305
	cmpl	$2, %edx
	je	.L306
	cmpl	$3, %edx
	jne	.L303
	andq	$-8, %rdi
	movl	$8, %esi
	call	_ZdlPvm@PLT
.L303:
	addq	$8, %r14
	cmpq	%r14, %r15
	je	.L504
.L384:
	movq	(%r14), %rdi
	movl	%edi, %edx
	andl	$7, %edx
	cmpl	$1, %edx
	jne	.L505
	andq	$-8, %rdi
	movq	%rdi, (%rsp)
	je	.L303
	movq	8(%rdi), %r12
	movq	(%rdi), %r13
	cmpq	%r13, %r12
	je	.L308
	movq	%r13, %rax
	movq	%r15, 24(%rsp)
	movq	%r12, %r13
	movq	%r14, 32(%rsp)
	movq	%rax, %r12
	jmp	.L381
	.p2align 4,,10
	.p2align 3
.L507:
	jb	.L311
	cmpl	$2, %edx
	je	.L312
	cmpl	$3, %edx
	jne	.L309
	andq	$-8, %rdi
	movl	$8, %esi
	call	_ZdlPvm@PLT
.L309:
	addq	$8, %r12
	cmpq	%r12, %r13
	je	.L506
.L381:
	movq	(%r12), %rdi
	movl	%edi, %edx
	andl	$7, %edx
	cmpl	$1, %edx
	jne	.L507
	andq	$-8, %rdi
	movq	%rdi, 16(%rsp)
	je	.L309
	movq	8(%rdi), %r14
	movq	(%rdi), %r15
	cmpq	%r15, %r14
	je	.L314
	movq	%r12, 56(%rsp)
	movq	%r13, 48(%rsp)
	movq	%r15, %r12
	movq	%r14, %r15
	jmp	.L378
	.p2align 4,,10
	.p2align 3
.L509:
	jb	.L317
	cmpl	$2, %edx
	je	.L318
	cmpl	$3, %edx
	jne	.L315
	andq	$-8, %rdi
	movl	$8, %esi
	call	_ZdlPvm@PLT
.L315:
	addq	$8, %r12
	cmpq	%r12, %r15
	je	.L508
.L378:
	movq	(%r12), %rdi
	movl	%edi, %edx
	andl	$7, %edx
	cmpl	$1, %edx
	jne	.L509
	andq	$-8, %rdi
	movq	%rdi, 40(%rsp)
	je	.L315
	movq	8(%rdi), %r13
	movq	(%rdi), %r14
	cmpq	%r14, %r13
	je	.L320
	movq	%r15, 72(%rsp)
	movq	%r12, 80(%rsp)
	movq	%r14, %r15
	jmp	.L375
	.p2align 4,,10
	.p2align 3
.L511:
	jb	.L323
	cmpl	$2, %edx
	je	.L324
	cmpl	$3, %edx
	jne	.L321
	andq	$-8, %rdi
	movl	$8, %esi
	call	_ZdlPvm@PLT
.L321:
	addq	$8, %r15
	cmpq	%r15, %r13
	je	.L510
.L375:
	movq	(%r15), %rdi
	movl	%edi, %edx
	andl	$7, %edx
	cmpl	$1, %edx
	jne	.L511
	andq	$-8, %rdi
	movq	%rdi, 64(%rsp)
	je	.L321
	movq	8(%rdi), %r14
	movq	(%rdi), %r12
	cmpq	%r12, %r14
	je	.L326
	movq	%r15, 120(%rsp)
	movq	%r13, 112(%rsp)
	movq	%r12, %r15
	jmp	.L372
	.p2align 4,,10
	.p2align 3
.L513:
	jb	.L329
	cmpl	$2, %eax
	je	.L330
	cmpl	$3, %eax
	jne	.L327
	andq	$-8, %rdi
	movl	$8, %esi
	call	_ZdlPvm@PLT
	.p2align 4,,10
	.p2align 3
.L327:
	addq	$8, %r15
	cmpq	%r15, %r14
	je	.L512
.L372:
	movq	(%r15), %rdi
	movl	%edi, %eax
	andl	$7, %eax
	cmpl	$1, %eax
	jne	.L513
	andq	$-8, %rdi
	movq	%rdi, 96(%rsp)
	je	.L327
	movq	8(%rdi), %rcx
	movq	(%rdi), %r13
	cmpq	%r13, %rcx
	movq	%rcx, 88(%rsp)
	je	.L332
	movq	%r14, 152(%rsp)
	movq	%r15, 160(%rsp)
	movq	%r13, %r14
	jmp	.L369
.L515:
	cmpl	$2, %eax
	je	.L336
	cmpl	$3, %eax
	jne	.L333
	andq	$-8, %rdi
	movl	$8, %esi
	call	_ZdlPvm@PLT
.L333:
	addq	$8, %r14
	cmpq	%r14, 88(%rsp)
	je	.L514
.L369:
	movq	(%r14), %rdi
	movl	%edi, %eax
	andl	$7, %eax
	cmpl	$1, %eax
	je	.L334
	jnb	.L515
	movq	%rdi, %rbx
	andq	$-8, %rbx
	je	.L333
	movq	%rbx, %rdi
	call	_ZNSt10_HashtableINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESt4pairIKS5_10json_valueESaIS9_ENSt8__detail10_Select1stESt8equal_toIS5_ESt4hashIS5_ENSB_18_Mod_range_hashingENSB_20_Default_ranged_hashENSB_20_Prime_rehash_policyENSB_17_Hashtable_traitsILb1ELb0ELb1EEEED1Ev
	movl	$56, %esi
	movq	%rbx, %rdi
	call	_ZdlPvm@PLT
	jmp	.L333
	.p2align 4,,10
	.p2align 3
.L306:
	movq	%rdi, %rbx
	andq	$-8, %rbx
	je	.L303
	movq	(%rbx), %rdi
	leaq	16(%rbx), %rdx
	cmpq	%rdx, %rdi
	je	.L383
	call	_ZdlPv@PLT
.L383:
	movl	$32, %esi
	movq	%rbx, %rdi
	addq	$8, %r14
	call	_ZdlPvm@PLT
	cmpq	%r14, %r15
	jne	.L384
	.p2align 4,,10
	.p2align 3
.L504:
	movq	8(%rsp), %r13
	movq	0(%r13), %r15
.L302:
	testq	%r15, %r15
	je	.L301
	addq	$200, %rsp
	.cfi_remember_state
	.cfi_def_cfa_offset 56
	movq	%r15, %rdi
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
.L305:
	.cfi_restore_state
	movq	%rdi, %rbx
	andq	$-8, %rbx
	je	.L303
	movq	%rbx, %rdi
	call	_ZNSt10_HashtableINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESt4pairIKS5_10json_valueESaIS9_ENSt8__detail10_Select1stESt8equal_toIS5_ESt4hashIS5_ENSB_18_Mod_range_hashingENSB_20_Default_ranged_hashENSB_20_Prime_rehash_policyENSB_17_Hashtable_traitsILb1ELb0ELb1EEEED1Ev
	movl	$56, %esi
	movq	%rbx, %rdi
	call	_ZdlPvm@PLT
	jmp	.L303
	.p2align 4,,10
	.p2align 3
.L301:
	addq	$200, %rsp
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
.L312:
	.cfi_restore_state
	movq	%rdi, %rbx
	andq	$-8, %rbx
	je	.L309
	movq	(%rbx), %rdi
	leaq	16(%rbx), %rdx
	cmpq	%rdx, %rdi
	je	.L380
	call	_ZdlPv@PLT
.L380:
	movl	$32, %esi
	movq	%rbx, %rdi
	addq	$8, %r12
	call	_ZdlPvm@PLT
	cmpq	%r12, %r13
	jne	.L381
	.p2align 4,,10
	.p2align 3
.L506:
	movq	(%rsp), %rax
	movq	24(%rsp), %r15
	movq	32(%rsp), %r14
	movq	(%rax), %r13
.L308:
	testq	%r13, %r13
	je	.L382
	movq	%r13, %rdi
	call	_ZdlPv@PLT
.L382:
	movq	(%rsp), %rdi
	movl	$24, %esi
	call	_ZdlPvm@PLT
	jmp	.L303
	.p2align 4,,10
	.p2align 3
.L311:
	movq	%rdi, %rbx
	andq	$-8, %rbx
	je	.L309
	movq	%rbx, %rdi
	call	_ZNSt10_HashtableINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESt4pairIKS5_10json_valueESaIS9_ENSt8__detail10_Select1stESt8equal_toIS5_ESt4hashIS5_ENSB_18_Mod_range_hashingENSB_20_Default_ranged_hashENSB_20_Prime_rehash_policyENSB_17_Hashtable_traitsILb1ELb0ELb1EEEED1Ev
	movl	$56, %esi
	movq	%rbx, %rdi
	call	_ZdlPvm@PLT
	jmp	.L309
.L510:
	movq	40(%rsp), %rax
	movq	72(%rsp), %r15
	movq	80(%rsp), %r12
	movq	(%rax), %r14
.L320:
	testq	%r14, %r14
	je	.L376
	movq	%r14, %rdi
	call	_ZdlPv@PLT
.L376:
	movq	40(%rsp), %rdi
	movl	$24, %esi
	call	_ZdlPvm@PLT
	jmp	.L315
	.p2align 4,,10
	.p2align 3
.L317:
	movq	%rdi, %rbx
	andq	$-8, %rbx
	je	.L315
	movq	%rbx, %rdi
	call	_ZNSt10_HashtableINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESt4pairIKS5_10json_valueESaIS9_ENSt8__detail10_Select1stESt8equal_toIS5_ESt4hashIS5_ENSB_18_Mod_range_hashingENSB_20_Default_ranged_hashENSB_20_Prime_rehash_policyENSB_17_Hashtable_traitsILb1ELb0ELb1EEEED1Ev
	movl	$56, %esi
	movq	%rbx, %rdi
	call	_ZdlPvm@PLT
	jmp	.L315
	.p2align 4,,10
	.p2align 3
.L318:
	movq	%rdi, %rbx
	andq	$-8, %rbx
	je	.L315
	movq	(%rbx), %rdi
	leaq	16(%rbx), %rdx
	cmpq	%rdx, %rdi
	je	.L377
	call	_ZdlPv@PLT
.L377:
	movl	$32, %esi
	movq	%rbx, %rdi
	call	_ZdlPvm@PLT
	jmp	.L315
	.p2align 4,,10
	.p2align 3
.L508:
	movq	16(%rsp), %rax
	movq	48(%rsp), %r13
	movq	56(%rsp), %r12
	movq	(%rax), %r15
.L314:
	testq	%r15, %r15
	je	.L379
	movq	%r15, %rdi
	call	_ZdlPv@PLT
.L379:
	movq	16(%rsp), %rdi
	movl	$24, %esi
	call	_ZdlPvm@PLT
	jmp	.L309
	.p2align 4,,10
	.p2align 3
.L324:
	movq	%rdi, %rbx
	andq	$-8, %rbx
	je	.L321
	movq	(%rbx), %rdi
	leaq	16(%rbx), %rdx
	cmpq	%rdx, %rdi
	je	.L374
	call	_ZdlPv@PLT
.L374:
	movl	$32, %esi
	movq	%rbx, %rdi
	call	_ZdlPvm@PLT
	jmp	.L321
	.p2align 4,,10
	.p2align 3
.L323:
	movq	%rdi, %rbx
	andq	$-8, %rbx
	je	.L321
	movq	%rbx, %rdi
	call	_ZNSt10_HashtableINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESt4pairIKS5_10json_valueESaIS9_ENSt8__detail10_Select1stESt8equal_toIS5_ESt4hashIS5_ENSB_18_Mod_range_hashingENSB_20_Default_ranged_hashENSB_20_Prime_rehash_policyENSB_17_Hashtable_traitsILb1ELb0ELb1EEEED1Ev
	movl	$56, %esi
	movq	%rbx, %rdi
	call	_ZdlPvm@PLT
	jmp	.L321
.L329:
	movq	%rdi, %rbx
	andq	$-8, %rbx
	je	.L327
	movq	%rbx, %rdi
	call	_ZNSt10_HashtableINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESt4pairIKS5_10json_valueESaIS9_ENSt8__detail10_Select1stESt8equal_toIS5_ESt4hashIS5_ENSB_18_Mod_range_hashingENSB_20_Default_ranged_hashENSB_20_Prime_rehash_policyENSB_17_Hashtable_traitsILb1ELb0ELb1EEEED1Ev
	movl	$56, %esi
	movq	%rbx, %rdi
	call	_ZdlPvm@PLT
	jmp	.L327
.L330:
	movq	%rdi, %rbx
	andq	$-8, %rbx
	je	.L327
	movq	(%rbx), %rdi
	leaq	16(%rbx), %rax
	cmpq	%rax, %rdi
	je	.L371
	call	_ZdlPv@PLT
.L371:
	movl	$32, %esi
	movq	%rbx, %rdi
	call	_ZdlPvm@PLT
	jmp	.L327
.L512:
	movq	64(%rsp), %rax
	movq	112(%rsp), %r13
	movq	120(%rsp), %r15
	movq	(%rax), %r12
.L326:
	testq	%r12, %r12
	je	.L373
	movq	%r12, %rdi
	call	_ZdlPv@PLT
.L373:
	movq	64(%rsp), %rdi
	movl	$24, %esi
	call	_ZdlPvm@PLT
	jmp	.L321
.L336:
	movq	%rdi, %rbx
	andq	$-8, %rbx
	je	.L333
	movq	(%rbx), %rdi
	leaq	16(%rbx), %rax
	cmpq	%rax, %rdi
	je	.L368
	call	_ZdlPv@PLT
.L368:
	movl	$32, %esi
	movq	%rbx, %rdi
	call	_ZdlPvm@PLT
	jmp	.L333
.L334:
	andq	$-8, %rdi
	movq	%rdi, 104(%rsp)
	je	.L333
	movq	8(%rdi), %rcx
	movq	(%rdi), %r15
	cmpq	%r15, %rcx
	movq	%rcx, 128(%rsp)
	je	.L338
	movq	%r14, 168(%rsp)
	jmp	.L366
.L517:
	jb	.L341
	cmpl	$2, %eax
	je	.L342
	cmpl	$3, %eax
	jne	.L339
	andq	$-8, %rdi
	movl	$8, %esi
	call	_ZdlPvm@PLT
.L339:
	addq	$8, %r15
	cmpq	%r15, 128(%rsp)
	je	.L516
.L366:
	movq	(%r15), %rdi
	movl	%edi, %eax
	andl	$7, %eax
	cmpl	$1, %eax
	jne	.L517
	andq	$-8, %rdi
	movq	%rdi, %r13
	je	.L339
	movq	8(%rdi), %rax
	movq	(%rdi), %rbp
	cmpq	%rbp, %rax
	movq	%rax, 136(%rsp)
	je	.L344
	movq	%rdi, 176(%rsp)
	jmp	.L363
.L519:
	jb	.L347
	cmpl	$2, %eax
	je	.L348
	cmpl	$3, %eax
	jne	.L345
	andq	$-8, %rbx
	movl	$8, %esi
	movq	%rbx, %rdi
	call	_ZdlPvm@PLT
.L345:
	addq	$8, %rbp
	cmpq	%rbp, 136(%rsp)
	je	.L518
.L363:
	movq	0(%rbp), %rbx
	movl	%ebx, %eax
	andl	$7, %eax
	cmpl	$1, %eax
	jne	.L519
	andq	$-8, %rbx
	je	.L345
	movq	8(%rbx), %r13
	movq	(%rbx), %r12
	cmpq	%r12, %r13
	jne	.L360
	jmp	.L350
	.p2align 4,,10
	.p2align 3
.L520:
	jb	.L353
	cmpl	$2, %eax
	je	.L354
	cmpl	$3, %eax
	jne	.L351
	andq	$-8, %rdi
	movl	$8, %esi
	call	_ZdlPvm@PLT
.L351:
	addq	$8, %r12
	cmpq	%r12, %r13
	je	.L350
.L360:
	movq	(%r12), %rdi
	movl	%edi, %eax
	andl	$7, %eax
	cmpl	$1, %eax
	jne	.L520
	andq	$-8, %rdi
	movq	%rdi, %r14
	je	.L351
	movq	8(%rdi), %rax
	movq	(%rdi), %rdx
	movq	%rax, 144(%rsp)
	jmp	.L357
.L521:
	movq	%rdx, %rdi
	movq	%rdx, 184(%rsp)
	call	_ZN10json_valueD1Ev
	movq	184(%rsp), %rdx
	addq	$8, %rdx
.L357:
	cmpq	%rdx, 144(%rsp)
	jne	.L521
	movq	(%r14), %rdi
	testq	%rdi, %rdi
	je	.L358
	call	_ZdlPv@PLT
.L358:
	movl	$24, %esi
	movq	%r14, %rdi
	call	_ZdlPvm@PLT
	jmp	.L351
	.p2align 4,,10
	.p2align 3
.L514:
	movq	96(%rsp), %rax
	movq	152(%rsp), %r14
	movq	160(%rsp), %r15
	movq	(%rax), %r13
.L332:
	testq	%r13, %r13
	je	.L370
	movq	%r13, %rdi
	call	_ZdlPv@PLT
.L370:
	movq	96(%rsp), %rdi
	movl	$24, %esi
	call	_ZdlPvm@PLT
	jmp	.L327
.L342:
	movq	%rdi, %rbx
	andq	$-8, %rbx
	je	.L339
	movq	(%rbx), %rdi
	leaq	16(%rbx), %rax
	cmpq	%rax, %rdi
	je	.L365
	call	_ZdlPv@PLT
.L365:
	movl	$32, %esi
	movq	%rbx, %rdi
	call	_ZdlPvm@PLT
	jmp	.L339
.L341:
	movq	%rdi, %rbx
	andq	$-8, %rbx
	je	.L339
	movq	%rbx, %rdi
	call	_ZNSt10_HashtableINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESt4pairIKS5_10json_valueESaIS9_ENSt8__detail10_Select1stESt8equal_toIS5_ESt4hashIS5_ENSB_18_Mod_range_hashingENSB_20_Default_ranged_hashENSB_20_Prime_rehash_policyENSB_17_Hashtable_traitsILb1ELb0ELb1EEEED1Ev
	movl	$56, %esi
	movq	%rbx, %rdi
	call	_ZdlPvm@PLT
	jmp	.L339
.L516:
	movq	168(%rsp), %r14
.L338:
	movq	104(%rsp), %rax
	movq	(%rax), %rdi
	testq	%rdi, %rdi
	je	.L367
	call	_ZdlPv@PLT
.L367:
	movq	104(%rsp), %rdi
	movl	$24, %esi
	call	_ZdlPvm@PLT
	jmp	.L333
.L518:
	movq	176(%rsp), %r13
.L344:
	movq	0(%r13), %rdi
	testq	%rdi, %rdi
	je	.L364
	call	_ZdlPv@PLT
.L364:
	movl	$24, %esi
	movq	%r13, %rdi
	call	_ZdlPvm@PLT
	jmp	.L339
.L348:
	andq	$-8, %rbx
	je	.L345
	movq	(%rbx), %rdi
	leaq	16(%rbx), %rax
	cmpq	%rax, %rdi
	je	.L362
	call	_ZdlPv@PLT
.L362:
	movl	$32, %esi
	movq	%rbx, %rdi
	call	_ZdlPvm@PLT
	jmp	.L345
.L347:
	andq	$-8, %rbx
	je	.L345
	movq	%rbx, %rdi
	call	_ZNSt10_HashtableINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESt4pairIKS5_10json_valueESaIS9_ENSt8__detail10_Select1stESt8equal_toIS5_ESt4hashIS5_ENSB_18_Mod_range_hashingENSB_20_Default_ranged_hashENSB_20_Prime_rehash_policyENSB_17_Hashtable_traitsILb1ELb0ELb1EEEED1Ev
	movl	$56, %esi
	movq	%rbx, %rdi
	call	_ZdlPvm@PLT
	jmp	.L345
.L350:
	movq	(%rbx), %rdi
	testq	%rdi, %rdi
	je	.L361
	call	_ZdlPv@PLT
.L361:
	movl	$24, %esi
	movq	%rbx, %rdi
	call	_ZdlPvm@PLT
	jmp	.L345
.L354:
	andq	$-8, %rdi
	movq	%rdi, %r14
	je	.L351
	movq	(%rdi), %rdi
	leaq	16(%r14), %rax
	cmpq	%rax, %rdi
	je	.L359
	call	_ZdlPv@PLT
.L359:
	movl	$32, %esi
	movq	%r14, %rdi
	call	_ZdlPvm@PLT
	jmp	.L351
.L353:
	andq	$-8, %rdi
	movq	%rdi, %r14
	je	.L351
	call	_ZNSt10_HashtableINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESt4pairIKS5_10json_valueESaIS9_ENSt8__detail10_Select1stESt8equal_toIS5_ESt4hashIS5_ENSB_18_Mod_range_hashingENSB_20_Default_ranged_hashENSB_20_Prime_rehash_policyENSB_17_Hashtable_traitsILb1ELb0ELb1EEEED1Ev
	movl	$56, %esi
	movq	%r14, %rdi
	call	_ZdlPvm@PLT
	jmp	.L351
	.cfi_endproc
.LFE2841:
	.size	_ZNSt6vectorI10json_valueSaIS0_EED2Ev, .-_ZNSt6vectorI10json_valueSaIS0_EED2Ev
	.weak	_ZNSt6vectorI10json_valueSaIS0_EED1Ev
	.set	_ZNSt6vectorI10json_valueSaIS0_EED1Ev,_ZNSt6vectorI10json_valueSaIS0_EED2Ev
	.section	.rodata.str1.1,"aMS",@progbits,1
.LC1:
	.string	"%ld | %f | {%ld, %ld, %ld}\n"
	.section	.text.startup,"ax",@progbits
	.p2align 4,,15
	.globl	main
	.type	main, @function
main:
.LFB2579:
	.cfi_startproc
	.cfi_personality 0x9b,DW.ref.__gxx_personality_v0
	.cfi_lsda 0x1b,.LLSDA2579
	pushq	%r15
	.cfi_def_cfa_offset 16
	.cfi_offset 15, -16
	pushq	%r14
	.cfi_def_cfa_offset 24
	.cfi_offset 14, -24
	movl	$8, %edi
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
	subq	$264, %rsp
	.cfi_def_cfa_offset 320
	movq	%fs:40, %rax
	movq	%rax, 248(%rsp)
	xorl	%eax, %eax
	movq	$340, 168(%rsp)
.LEHB0:
	call	_Znwm@PLT
.LEHE0:
	movq	.LC0(%rip), %rsi
	movl	$24, %edi
	movq	$12, 224(%rsp)
	movq	$20, 232(%rsp)
	movq	$28, 240(%rsp)
	movq	$0, 192(%rsp)
	movq	$0, 200(%rsp)
	movq	%rsi, (%rax)
	orq	$3, %rax
	movq	$0, 208(%rsp)
	movq	%rax, 176(%rsp)
.LEHB1:
	call	_Znwm@PLT
.LEHE1:
	movq	224(%rsp), %rcx
	leaq	24(%rax), %rdx
	movl	$24, %edi
	movq	%rax, 192(%rsp)
	movq	%rdx, 208(%rsp)
	movq	%rdx, 200(%rsp)
	movq	%rcx, (%rax)
	movq	232(%rsp), %rcx
	movq	%rcx, 8(%rax)
	movq	240(%rsp), %rcx
	movq	%rcx, 16(%rax)
.LEHB2:
	call	_Znwm@PLT
.LEHE2:
	movq	200(%rsp), %rdx
	movq	%rax, %rbx
	movq	$0, (%rbx)
	movq	$0, 8(%rbx)
	movq	$0, 16(%rbx)
	movq	%rdx, %rbp
	subq	192(%rsp), %rbp
	movq	%rbp, %rax
	sarq	$3, %rax
	testq	%rax, %rax
	je	.L721
	movabsq	$2305843009213693951, %rdx
	cmpq	%rdx, %rax
	ja	.L722
	movq	%rbp, %rdi
.LEHB3:
	call	_Znwm@PLT
.LEHE3:
	movq	200(%rsp), %rdx
.L528:
	movq	192(%rsp), %rsi
	addq	%rax, %rbp
	movq	%rax, (%rbx)
	movq	%rax, 8(%rbx)
	movq	%rbp, 16(%rbx)
	cmpq	%rsi, %rdx
	je	.L530
	leaq	8(%rsi), %rcx
	subq	%rcx, %rdx
	shrq	$3, %rdx
	leaq	8(,%rdx,8), %rdi
	xorl	%edx, %edx
	.p2align 4,,10
	.p2align 3
.L531:
	movq	(%rsi,%rdx), %rcx
	movq	%rcx, (%rax,%rdx)
	addq	$8, %rdx
	cmpq	%rdx, %rdi
	jne	.L531
	addq	%rdi, %rax
.L530:
	leaq	240(%rsp), %r13
	leaq	192(%rsp), %rdi
	movq	%rax, 8(%rbx)
	leaq	216(%rsp), %r15
	orq	$1, %rbx
	movq	%rbx, 184(%rsp)
	call	_ZNSt6vectorI10json_valueSaIS0_EED1Ev
	movq	%r13, %rax
	movq	%r15, %r13
	movq	%rax, %r15
.L532:
	movq	(%r15), %rdi
	movl	%edi, %edx
	andl	$7, %edx
	cmpl	$1, %edx
	je	.L536
	jb	.L537
	cmpl	$2, %edx
	je	.L538
	cmpl	$3, %edx
	jne	.L535
	andq	$-8, %rdi
	movl	$8, %esi
	call	_ZdlPvm@PLT
.L535:
	subq	$8, %r15
	cmpq	%r15, %r13
	jne	.L532
	movq	184(%rsp), %rax
	movq	168(%rsp), %rsi
	leaq	.LC1(%rip), %rdi
	andq	$-8, %rax
	sarq	$3, %rsi
	movq	(%rax), %rax
	movq	16(%rax), %r8
	movq	8(%rax), %rcx
	movq	(%rax), %rdx
	movq	176(%rsp), %rax
	sarq	$3, %rcx
	sarq	$3, %r8
	andq	$-8, %rax
	sarq	$3, %rdx
	movsd	(%rax), %xmm0
	movl	$1, %eax
.LEHB4:
	call	printf@PLT
.LEHE4:
	leaq	184(%rsp), %rdi
	call	_ZN10json_valueD1Ev
	leaq	176(%rsp), %rdi
	call	_ZN10json_valueD1Ev
	leaq	168(%rsp), %rdi
	call	_ZN10json_valueD1Ev
	xorl	%eax, %eax
	movq	248(%rsp), %rsi
	xorq	%fs:40, %rsi
	jne	.L723
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
.L538:
	.cfi_restore_state
	movq	%rdi, %rbx
	andq	$-8, %rbx
	je	.L535
	movq	(%rbx), %rdi
	leaq	16(%rbx), %rdx
	cmpq	%rdx, %rdi
	je	.L606
	call	_ZdlPv@PLT
.L606:
	movl	$32, %esi
	movq	%rbx, %rdi
	call	_ZdlPvm@PLT
	jmp	.L535
.L537:
	movq	%rdi, %rbx
	andq	$-8, %rbx
	je	.L535
	movq	%rbx, %rdi
	call	_ZNSt10_HashtableINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESt4pairIKS5_10json_valueESaIS9_ENSt8__detail10_Select1stESt8equal_toIS5_ESt4hashIS5_ENSB_18_Mod_range_hashingENSB_20_Default_ranged_hashENSB_20_Prime_rehash_policyENSB_17_Hashtable_traitsILb1ELb0ELb1EEEED1Ev
	movl	$56, %esi
	movq	%rbx, %rdi
	call	_ZdlPvm@PLT
	jmp	.L535
.L536:
	andq	$-8, %rdi
	movq	%rdi, (%rsp)
	je	.L535
	movq	8(%rdi), %r14
	movq	(%rdi), %r12
	cmpq	%r12, %r14
	je	.L540
	movq	%r15, 24(%rsp)
	movq	%r13, 16(%rsp)
	movq	%r12, %r15
	movq	%r14, %r12
	jmp	.L604
	.p2align 4,,10
	.p2align 3
.L725:
	cmpl	$2, %edx
	je	.L544
	cmpl	$3, %edx
	jne	.L541
	andq	$-8, %rdi
	movl	$8, %esi
	call	_ZdlPvm@PLT
	.p2align 4,,10
	.p2align 3
.L541:
	addq	$8, %r15
	cmpq	%r15, %r12
	je	.L724
.L604:
	movq	(%r15), %rdi
	movl	%edi, %edx
	andl	$7, %edx
	cmpl	$1, %edx
	je	.L542
	jnb	.L725
	movq	%rdi, %rbx
	andq	$-8, %rbx
	je	.L541
	movq	%rbx, %rdi
	call	_ZNSt10_HashtableINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESt4pairIKS5_10json_valueESaIS9_ENSt8__detail10_Select1stESt8equal_toIS5_ESt4hashIS5_ENSB_18_Mod_range_hashingENSB_20_Default_ranged_hashENSB_20_Prime_rehash_policyENSB_17_Hashtable_traitsILb1ELb0ELb1EEEED1Ev
	movl	$56, %esi
	movq	%rbx, %rdi
	call	_ZdlPvm@PLT
	jmp	.L541
.L542:
	andq	$-8, %rdi
	movq	%rdi, 8(%rsp)
	je	.L541
	movq	8(%rdi), %r13
	movq	(%rdi), %r14
	movq	%r12, 40(%rsp)
	movq	%r15, 48(%rsp)
	jmp	.L601
.L726:
	jb	.L549
	cmpl	$2, %edx
	je	.L550
	cmpl	$3, %edx
	jne	.L547
	andq	$-8, %rdi
	movl	$8, %esi
	call	_ZdlPvm@PLT
.L547:
	addq	$8, %r14
.L601:
	cmpq	%r14, %r13
	je	.L546
	movq	(%r14), %rdi
	movl	%edi, %edx
	andl	$7, %edx
	cmpl	$1, %edx
	jne	.L726
	andq	$-8, %rdi
	movq	%rdi, 32(%rsp)
	je	.L547
	movq	8(%rdi), %rsi
	movq	%r14, 64(%rsp)
	movq	(%rdi), %r15
	movq	%r13, 56(%rsp)
	movq	%rsi, %r14
	jmp	.L598
.L727:
	jb	.L555
	cmpl	$2, %edx
	je	.L556
	cmpl	$3, %edx
	jne	.L553
	andq	$-8, %rdi
	movl	$8, %esi
	call	_ZdlPvm@PLT
.L553:
	addq	$8, %r15
.L598:
	cmpq	%r15, %r14
	je	.L552
	movq	(%r15), %rdi
	movl	%edi, %edx
	andl	$7, %edx
	cmpl	$1, %edx
	jne	.L727
	andq	$-8, %rdi
	movq	%rdi, 72(%rsp)
	je	.L553
	movq	8(%rdi), %rsi
	movq	(%rdi), %r13
	movq	%r14, 120(%rsp)
	movq	%r15, 128(%rsp)
	movq	%rsi, %r14
	movq	%r13, %r12
	jmp	.L595
.L728:
	jb	.L561
	cmpl	$2, %eax
	je	.L562
	cmpl	$3, %eax
	jne	.L559
	andq	$-8, %rdi
	movl	$8, %esi
	call	_ZdlPvm@PLT
.L559:
	addq	$8, %r12
.L595:
	cmpq	%r12, %r14
	je	.L558
	movq	(%r12), %rdi
	movl	%edi, %eax
	andl	$7, %eax
	cmpl	$1, %eax
	jne	.L728
	andq	$-8, %rdi
	movq	%rdi, 80(%rsp)
	je	.L559
	movq	8(%rdi), %rsi
	movq	(%rdi), %r13
	movq	%r14, 136(%rsp)
	movq	%r12, 144(%rsp)
	movq	%rsi, 88(%rsp)
	jmp	.L592
.L729:
	jb	.L567
	cmpl	$2, %eax
	je	.L568
	cmpl	$3, %eax
	jne	.L565
	andq	$-8, %rbx
	movl	$8, %esi
	movq	%rbx, %rdi
	call	_ZdlPvm@PLT
.L565:
	addq	$8, %r13
.L592:
	cmpq	%r13, 88(%rsp)
	je	.L564
	movq	0(%r13), %rbx
	movl	%ebx, %eax
	andl	$7, %eax
	cmpl	$1, %eax
	jne	.L729
	andq	$-8, %rbx
	je	.L565
	movq	8(%rbx), %rax
	movq	(%rbx), %r14
	movq	%rax, 96(%rsp)
	jmp	.L589
.L730:
	cmpl	$2, %eax
	je	.L574
	cmpl	$3, %eax
	jne	.L571
	movq	%rbp, %rdi
	movl	$8, %esi
	andq	$-8, %rdi
	call	_ZdlPvm@PLT
.L571:
	addq	$8, %r14
.L589:
	cmpq	%r14, 96(%rsp)
	je	.L570
	movq	(%r14), %rbp
	movl	%ebp, %eax
	andl	$7, %eax
	cmpl	$1, %eax
	je	.L572
	jnb	.L730
	andq	$-8, %rbp
	je	.L571
	movq	%rbp, %rdi
	call	_ZNSt10_HashtableINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESt4pairIKS5_10json_valueESaIS9_ENSt8__detail10_Select1stESt8equal_toIS5_ESt4hashIS5_ENSB_18_Mod_range_hashingENSB_20_Default_ranged_hashENSB_20_Prime_rehash_policyENSB_17_Hashtable_traitsILb1ELb0ELb1EEEED1Ev
	movl	$56, %esi
	movq	%rbp, %rdi
	call	_ZdlPvm@PLT
	jmp	.L571
.L544:
	movq	%rdi, %rbx
	andq	$-8, %rbx
	je	.L541
	movq	(%rbx), %rdi
	leaq	16(%rbx), %rdx
	cmpq	%rdx, %rdi
	je	.L603
	call	_ZdlPv@PLT
.L603:
	movl	$32, %esi
	movq	%rbx, %rdi
	call	_ZdlPvm@PLT
	jmp	.L541
.L724:
	movq	16(%rsp), %r13
	movq	24(%rsp), %r15
.L540:
	movq	(%rsp), %rax
	movq	(%rax), %rdi
	testq	%rdi, %rdi
	je	.L605
	call	_ZdlPv@PLT
.L605:
	movq	(%rsp), %rdi
	movl	$24, %esi
	call	_ZdlPvm@PLT
	jmp	.L535
.L721:
	xorl	%eax, %eax
	jmp	.L528
.L549:
	movq	%rdi, %rbx
	andq	$-8, %rbx
	je	.L547
	movq	%rbx, %rdi
	call	_ZNSt10_HashtableINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESt4pairIKS5_10json_valueESaIS9_ENSt8__detail10_Select1stESt8equal_toIS5_ESt4hashIS5_ENSB_18_Mod_range_hashingENSB_20_Default_ranged_hashENSB_20_Prime_rehash_policyENSB_17_Hashtable_traitsILb1ELb0ELb1EEEED1Ev
	movl	$56, %esi
	movq	%rbx, %rdi
	call	_ZdlPvm@PLT
	jmp	.L547
.L550:
	movq	%rdi, %rbx
	andq	$-8, %rbx
	je	.L547
	movq	(%rbx), %rdi
	leaq	16(%rbx), %rdx
	cmpq	%rdx, %rdi
	je	.L600
	call	_ZdlPv@PLT
.L600:
	movl	$32, %esi
	movq	%rbx, %rdi
	call	_ZdlPvm@PLT
	jmp	.L547
.L546:
	movq	8(%rsp), %rax
	movq	40(%rsp), %r12
	movq	48(%rsp), %r15
	movq	(%rax), %rdi
	testq	%rdi, %rdi
	je	.L602
	call	_ZdlPv@PLT
.L602:
	movq	8(%rsp), %rdi
	movl	$24, %esi
	call	_ZdlPvm@PLT
	jmp	.L541
.L555:
	movq	%rdi, %rbx
	andq	$-8, %rbx
	je	.L553
	movq	%rbx, %rdi
	call	_ZNSt10_HashtableINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESt4pairIKS5_10json_valueESaIS9_ENSt8__detail10_Select1stESt8equal_toIS5_ESt4hashIS5_ENSB_18_Mod_range_hashingENSB_20_Default_ranged_hashENSB_20_Prime_rehash_policyENSB_17_Hashtable_traitsILb1ELb0ELb1EEEED1Ev
	movl	$56, %esi
	movq	%rbx, %rdi
	call	_ZdlPvm@PLT
	jmp	.L553
.L556:
	movq	%rdi, %rbx
	andq	$-8, %rbx
	je	.L553
	movq	(%rbx), %rdi
	leaq	16(%rbx), %rdx
	cmpq	%rdx, %rdi
	je	.L597
	call	_ZdlPv@PLT
.L597:
	movl	$32, %esi
	movq	%rbx, %rdi
	call	_ZdlPvm@PLT
	jmp	.L553
.L552:
	movq	32(%rsp), %rax
	movq	56(%rsp), %r13
	movq	64(%rsp), %r14
	movq	(%rax), %rdi
	testq	%rdi, %rdi
	je	.L599
	call	_ZdlPv@PLT
.L599:
	movq	32(%rsp), %rdi
	movl	$24, %esi
	call	_ZdlPvm@PLT
	jmp	.L547
.L562:
	movq	%rdi, %rbx
	andq	$-8, %rbx
	je	.L559
	movq	(%rbx), %rdi
	leaq	16(%rbx), %rax
	cmpq	%rax, %rdi
	je	.L594
	call	_ZdlPv@PLT
.L594:
	movl	$32, %esi
	movq	%rbx, %rdi
	call	_ZdlPvm@PLT
	jmp	.L559
.L561:
	movq	%rdi, %rbx
	andq	$-8, %rbx
	je	.L559
	movq	%rbx, %rdi
	call	_ZNSt10_HashtableINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESt4pairIKS5_10json_valueESaIS9_ENSt8__detail10_Select1stESt8equal_toIS5_ESt4hashIS5_ENSB_18_Mod_range_hashingENSB_20_Default_ranged_hashENSB_20_Prime_rehash_policyENSB_17_Hashtable_traitsILb1ELb0ELb1EEEED1Ev
	movl	$56, %esi
	movq	%rbx, %rdi
	call	_ZdlPvm@PLT
	jmp	.L559
.L558:
	movq	72(%rsp), %rax
	movq	120(%rsp), %r14
	movq	128(%rsp), %r15
	movq	(%rax), %rdi
	testq	%rdi, %rdi
	je	.L596
	call	_ZdlPv@PLT
.L596:
	movq	72(%rsp), %rdi
	movl	$24, %esi
	call	_ZdlPvm@PLT
	jmp	.L553
.L567:
	andq	$-8, %rbx
	je	.L565
	movq	%rbx, %rdi
	call	_ZNSt10_HashtableINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESt4pairIKS5_10json_valueESaIS9_ENSt8__detail10_Select1stESt8equal_toIS5_ESt4hashIS5_ENSB_18_Mod_range_hashingENSB_20_Default_ranged_hashENSB_20_Prime_rehash_policyENSB_17_Hashtable_traitsILb1ELb0ELb1EEEED1Ev
	movl	$56, %esi
	movq	%rbx, %rdi
	call	_ZdlPvm@PLT
	jmp	.L565
.L568:
	andq	$-8, %rbx
	je	.L565
	movq	(%rbx), %rdi
	leaq	16(%rbx), %rax
	cmpq	%rax, %rdi
	je	.L591
	call	_ZdlPv@PLT
.L591:
	movl	$32, %esi
	movq	%rbx, %rdi
	call	_ZdlPvm@PLT
	jmp	.L565
.L564:
	movq	80(%rsp), %rax
	movq	136(%rsp), %r14
	movq	144(%rsp), %r12
	movq	(%rax), %rdi
	testq	%rdi, %rdi
	je	.L593
	call	_ZdlPv@PLT
.L593:
	movq	80(%rsp), %rdi
	movl	$24, %esi
	call	_ZdlPvm@PLT
	jmp	.L559
.L574:
	andq	$-8, %rbp
	je	.L571
	movq	0(%rbp), %rdi
	leaq	16(%rbp), %rax
	cmpq	%rax, %rdi
	je	.L588
	call	_ZdlPv@PLT
.L588:
	movl	$32, %esi
	movq	%rbp, %rdi
	call	_ZdlPvm@PLT
	jmp	.L571
.L572:
	andq	$-8, %rbp
	je	.L571
	movq	8(%rbp), %rax
	movq	0(%rbp), %r12
	movq	%rax, 104(%rsp)
	jmp	.L586
.L731:
	movq	(%r12), %rdi
	movl	%edi, %eax
	andl	$7, %eax
	cmpl	$1, %eax
	je	.L578
	jb	.L579
	cmpl	$2, %eax
	je	.L580
	cmpl	$3, %eax
	jne	.L577
	andq	$-8, %rdi
	movl	$8, %esi
	call	_ZdlPvm@PLT
.L577:
	addq	$8, %r12
.L586:
	cmpq	%r12, 104(%rsp)
	jne	.L731
	movq	0(%rbp), %rdi
	testq	%rdi, %rdi
	je	.L587
	call	_ZdlPv@PLT
.L587:
	movl	$24, %esi
	movq	%rbp, %rdi
	call	_ZdlPvm@PLT
	jmp	.L571
.L570:
	movq	(%rbx), %rdi
	testq	%rdi, %rdi
	je	.L590
	call	_ZdlPv@PLT
.L590:
	movl	$24, %esi
	movq	%rbx, %rdi
	call	_ZdlPvm@PLT
	jmp	.L565
.L723:
	call	__stack_chk_fail@PLT
.L722:
.LEHB5:
	call	_ZSt17__throw_bad_allocv@PLT
.LEHE5:
.L615:
	movq	%rax, %rbp
	movq	%rbx, %rdi
	movl	$24, %esi
	movq	%rbp, %rbx
	call	_ZdlPvm@PLT
.L534:
	leaq	192(%rsp), %rdi
	call	_ZNSt6vectorI10json_valueSaIS0_EED1Ev
.L526:
	leaq	224(%rsp), %rbp
	leaq	240(%rsp), %rdi
	call	_ZN10json_valueD1Ev
	leaq	8(%rbp), %rdi
	call	_ZN10json_valueD1Ev
	movq	%rbp, %rdi
	call	_ZN10json_valueD1Ev
.L607:
	leaq	176(%rsp), %rdi
	call	_ZN10json_valueD1Ev
.L609:
	leaq	168(%rsp), %rdi
	call	_ZN10json_valueD1Ev
	movq	%rbx, %rdi
.LEHB6:
	call	_Unwind_Resume@PLT
.LEHE6:
.L580:
	andq	$-8, %rdi
	movq	%rdi, %r15
	je	.L577
	movq	(%rdi), %rdi
	leaq	16(%r15), %rax
	cmpq	%rax, %rdi
	je	.L585
	call	_ZdlPv@PLT
.L585:
	movl	$32, %esi
	movq	%r15, %rdi
	call	_ZdlPvm@PLT
	jmp	.L577
.L578:
	andq	$-8, %rdi
	movq	%rdi, %r15
	je	.L577
	movq	8(%rdi), %rax
	movq	(%rdi), %rdx
	movq	%rax, 112(%rsp)
	jmp	.L583
.L732:
	movq	%rdx, %rdi
	movq	%rdx, 152(%rsp)
	call	_ZN10json_valueD1Ev
	movq	152(%rsp), %rdx
	addq	$8, %rdx
.L583:
	cmpq	%rdx, 112(%rsp)
	jne	.L732
	movq	(%r15), %rdi
	testq	%rdi, %rdi
	je	.L584
	call	_ZdlPv@PLT
.L584:
	movl	$24, %esi
	movq	%r15, %rdi
	call	_ZdlPvm@PLT
	jmp	.L577
.L579:
	andq	$-8, %rdi
	movq	%rdi, %r15
	je	.L577
	call	_ZNSt10_HashtableINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESt4pairIKS5_10json_valueESaIS9_ENSt8__detail10_Select1stESt8equal_toIS5_ESt4hashIS5_ENSB_18_Mod_range_hashingENSB_20_Default_ranged_hashENSB_20_Prime_rehash_policyENSB_17_Hashtable_traitsILb1ELb0ELb1EEEED1Ev
	movl	$56, %esi
	movq	%r15, %rdi
	call	_ZdlPvm@PLT
	jmp	.L577
.L611:
	movq	%rax, %rbx
	jmp	.L609
.L614:
	movq	192(%rsp), %rdi
	movq	%rax, %rbx
	testq	%rdi, %rdi
	je	.L526
	call	_ZdlPv@PLT
	jmp	.L526
.L613:
	leaq	184(%rsp), %rdi
	movq	%rax, %rbx
	call	_ZN10json_valueD1Ev
	jmp	.L607
.L612:
	movq	%rax, %rbx
	jmp	.L534
	.cfi_endproc
.LFE2579:
	.globl	__gxx_personality_v0
	.section	.gcc_except_table,"a",@progbits
.LLSDA2579:
	.byte	0xff
	.byte	0xff
	.byte	0x1
	.uleb128 .LLSDACSE2579-.LLSDACSB2579
.LLSDACSB2579:
	.uleb128 .LEHB0-.LFB2579
	.uleb128 .LEHE0-.LEHB0
	.uleb128 .L611-.LFB2579
	.uleb128 0
	.uleb128 .LEHB1-.LFB2579
	.uleb128 .LEHE1-.LEHB1
	.uleb128 .L614-.LFB2579
	.uleb128 0
	.uleb128 .LEHB2-.LFB2579
	.uleb128 .LEHE2-.LEHB2
	.uleb128 .L612-.LFB2579
	.uleb128 0
	.uleb128 .LEHB3-.LFB2579
	.uleb128 .LEHE3-.LEHB3
	.uleb128 .L615-.LFB2579
	.uleb128 0
	.uleb128 .LEHB4-.LFB2579
	.uleb128 .LEHE4-.LEHB4
	.uleb128 .L613-.LFB2579
	.uleb128 0
	.uleb128 .LEHB5-.LFB2579
	.uleb128 .LEHE5-.LEHB5
	.uleb128 .L615-.LFB2579
	.uleb128 0
	.uleb128 .LEHB6-.LFB2579
	.uleb128 .LEHE6-.LEHB6
	.uleb128 0
	.uleb128 0
.LLSDACSE2579:
	.section	.text.startup
	.size	main, .-main
	.globl	_ZN10json_value8type_tagINS_11json_null_tEEE
	.section	.rodata
	.align 4
	.type	_ZN10json_value8type_tagINS_11json_null_tEEE, @object
	.size	_ZN10json_value8type_tagINS_11json_null_tEEE, 4
_ZN10json_value8type_tagINS_11json_null_tEEE:
	.long	6
	.globl	_ZN10json_value8type_tagIbEE
	.align 4
	.type	_ZN10json_value8type_tagIbEE, @object
	.size	_ZN10json_value8type_tagIbEE, 4
_ZN10json_value8type_tagIbEE:
	.long	5
	.globl	_ZN10json_value8type_tagIiEE
	.align 4
	.type	_ZN10json_value8type_tagIiEE, @object
	.size	_ZN10json_value8type_tagIiEE, 4
_ZN10json_value8type_tagIiEE:
	.long	4
	.globl	_ZN10json_value8type_tagIlEE
	.align 4
	.type	_ZN10json_value8type_tagIlEE, @object
	.size	_ZN10json_value8type_tagIlEE, 4
_ZN10json_value8type_tagIlEE:
	.long	4
	.globl	_ZN10json_value8type_tagIdEE
	.align 4
	.type	_ZN10json_value8type_tagIdEE, @object
	.size	_ZN10json_value8type_tagIdEE, 4
_ZN10json_value8type_tagIdEE:
	.long	3
	.globl	_ZN10json_value8type_tagINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEEE
	.align 4
	.type	_ZN10json_value8type_tagINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEEE, @object
	.size	_ZN10json_value8type_tagINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEEE, 4
_ZN10json_value8type_tagINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEEE:
	.long	2
	.globl	_ZN10json_value8type_tagISt6vectorIS_SaIS_EEEE
	.align 4
	.type	_ZN10json_value8type_tagISt6vectorIS_SaIS_EEEE, @object
	.size	_ZN10json_value8type_tagISt6vectorIS_SaIS_EEEE, 4
_ZN10json_value8type_tagISt6vectorIS_SaIS_EEEE:
	.long	1
	.globl	_ZN10json_value8type_tagISt13unordered_mapINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEES_St4hashIS7_ESt8equal_toIS7_ESaISt4pairIKS7_S_EEEEE
	.align 4
	.type	_ZN10json_value8type_tagISt13unordered_mapINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEES_St4hashIS7_ESt8equal_toIS7_ESaISt4pairIKS7_S_EEEEE, @object
	.size	_ZN10json_value8type_tagISt13unordered_mapINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEES_St4hashIS7_ESt8equal_toIS7_ESaISt4pairIKS7_S_EEEEE, 4
_ZN10json_value8type_tagISt13unordered_mapINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEES_St4hashIS7_ESt8equal_toIS7_ESaISt4pairIKS7_S_EEEEE:
	.zero	4
	.section	.rodata.cst8,"aM",@progbits,8
	.align 8
.LC0:
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
