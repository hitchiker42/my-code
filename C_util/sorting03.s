	.file	"sorting.c"
# GNU C11 (GCC) version 5.3.0 (x86_64-unknown-linux-gnu)
#	compiled by GNU C version 5.3.0, GMP version 6.1.0, MPFR version 3.1.3-p5, MPC version 1.0.3
# GGC heuristics: --param ggc-min-expand=100 --param ggc-min-heapsize=131072
# options passed:  -D NDEBUG sorting.c -mtune=generic -march=x86-64 -O3
# -fverbose-asm
# options enabled:  -faggressive-loop-optimizations -falign-labels
# -fasynchronous-unwind-tables -fauto-inc-dec -fbranch-count-reg
# -fcaller-saves -fchkp-check-incomplete-type -fchkp-check-read
# -fchkp-check-write -fchkp-instrument-calls -fchkp-narrow-bounds
# -fchkp-optimize -fchkp-store-bounds -fchkp-use-static-bounds
# -fchkp-use-static-const-bounds -fchkp-use-wrappers
# -fcombine-stack-adjustments -fcommon -fcompare-elim -fcprop-registers
# -fcrossjumping -fcse-follow-jumps -fdefer-pop
# -fdelete-null-pointer-checks -fdevirtualize -fdevirtualize-speculatively
# -fdwarf2-cfi-asm -fearly-inlining -feliminate-unused-debug-types
# -fexpensive-optimizations -fforward-propagate -ffunction-cse -fgcse
# -fgcse-after-reload -fgcse-lm -fgnu-runtime -fgnu-unique
# -fguess-branch-probability -fhoist-adjacent-loads -fident -fif-conversion
# -fif-conversion2 -findirect-inlining -finline -finline-atomics
# -finline-functions -finline-functions-called-once
# -finline-small-functions -fipa-cp -fipa-cp-alignment -fipa-cp-clone
# -fipa-icf -fipa-icf-functions -fipa-icf-variables -fipa-profile
# -fipa-pure-const -fipa-ra -fipa-reference -fipa-sra -fira-hoist-pressure
# -fira-share-save-slots -fira-share-spill-slots
# -fisolate-erroneous-paths-dereference -fivopts -fkeep-static-consts
# -fleading-underscore -flifetime-dse -flra-remat -flto-odr-type-merging
# -fmath-errno -fmerge-constants -fmerge-debug-strings
# -fmove-loop-invariants -fomit-frame-pointer -foptimize-sibling-calls
# -foptimize-strlen -fpartial-inlining -fpeephole -fpeephole2
# -fpredictive-commoning -fprefetch-loop-arrays -free -freg-struct-return
# -freorder-blocks -freorder-blocks-and-partition -freorder-functions
# -frerun-cse-after-loop -fsched-critical-path-heuristic
# -fsched-dep-count-heuristic -fsched-group-heuristic -fsched-interblock
# -fsched-last-insn-heuristic -fsched-rank-heuristic -fsched-spec
# -fsched-spec-insn-heuristic -fsched-stalled-insns-dep -fschedule-fusion
# -fschedule-insns2 -fsemantic-interposition -fshow-column -fshrink-wrap
# -fsigned-zeros -fsplit-ivs-in-unroller -fsplit-wide-types -fssa-phiopt
# -fstdarg-opt -fstrict-aliasing -fstrict-overflow
# -fstrict-volatile-bitfields -fsync-libcalls -fthread-jumps
# -ftoplevel-reorder -ftrapping-math -ftree-bit-ccp -ftree-builtin-call-dce
# -ftree-ccp -ftree-ch -ftree-coalesce-vars -ftree-copy-prop
# -ftree-copyrename -ftree-cselim -ftree-dce -ftree-dominator-opts
# -ftree-dse -ftree-forwprop -ftree-fre -ftree-loop-distribute-patterns
# -ftree-loop-if-convert -ftree-loop-im -ftree-loop-ivcanon
# -ftree-loop-optimize -ftree-loop-vectorize -ftree-parallelize-loops=
# -ftree-partial-pre -ftree-phiprop -ftree-pre -ftree-pta -ftree-reassoc
# -ftree-scev-cprop -ftree-sink -ftree-slp-vectorize -ftree-slsr -ftree-sra
# -ftree-switch-conversion -ftree-tail-merge -ftree-ter -ftree-vrp
# -funit-at-a-time -funswitch-loops -funwind-tables -fverbose-asm
# -fzero-initialized-in-bss -m128bit-long-double -m64 -m80387
# -malign-stringops -mavx256-split-unaligned-load
# -mavx256-split-unaligned-store -mfancy-math-387 -mfp-ret-in-387 -mfxsr
# -mglibc -mieee-fp -mlong-double-80 -mmmx -mno-sse4 -mpush-args -mred-zone
# -msse -msse2 -mtls-direct-seg-refs -mvzeroupper

	.section	.text.unlikely,"ax",@progbits
.LCOLDB0:
	.text
.LHOTB0:
	.p2align 4,,15
	.type	mergesort_internal_u64, @function
mergesort_internal_u64:
.LFB70:
	.cfi_startproc
	cmpq	$4, %rdx	#, len
	jbe	.L34	#,
	pushq	%r15	#
	.cfi_def_cfa_offset 16
	.cfi_offset 15, -16
	movq	%rdx, %r15	# len, mid
	pushq	%r14	#
	.cfi_def_cfa_offset 24
	.cfi_offset 14, -24
	shrq	%r15	# mid
	pushq	%r13	#
	.cfi_def_cfa_offset 32
	.cfi_offset 13, -32
	pushq	%r12	#
	.cfi_def_cfa_offset 40
	.cfi_offset 12, -40
	pushq	%rbp	#
	.cfi_def_cfa_offset 48
	.cfi_offset 6, -48
	movq	%rdx, %rbp	# len, len
	pushq	%rbx	#
	.cfi_def_cfa_offset 56
	.cfi_offset 3, -56
	movq	%rdi, %rbx	# arr, arr
	movq	%rbp, %r14	# len, n2
	movq	%rsi, %r12	# tmp, tmp
	leaq	(%rbx,%r15,8), %r13	#, B
	subq	$8, %rsp	#,
	.cfi_def_cfa_offset 64
	movq	%r15, %rdx	# mid,
	subq	%r15, %r14	# mid, n2
	call	mergesort_internal_u64	#
	movq	%r12, %rsi	# tmp,
	movq	%r14, %rdx	# n2,
	movq	%r13, %rdi	# B,
	call	mergesort_internal_u64	#
	testq	%r14, %r14	# n2
	movq	%r12, %r8	# tmp, tmp
	movq	%rbx, %rsi	# arr, arr
	jne	.L12	#,
	jmp	.L11	#
	.p2align 4,,10
	.p2align 3
.L35:
	testq	%r14, %r14	# n2
	je	.L19	#,
.L12:
	movq	(%rsi), %rax	# *arr_82, D.7471
	movq	0(%r13), %rdx	# *B_97, D.7471
	leaq	8(%rsi), %rdi	#, arr
	leaq	8(%r13), %rcx	#, B
	cmpq	%rdx, %rax	# D.7471, D.7471
	cmovb	%rdi, %rsi	# arr,, arr
	cmovnb	%rcx, %r13	# B,, B
	addq	$8, %r8	#, tmp
	cmpq	%rdx, %rax	# D.7471, D.7471
	sbbq	%rcx, %rcx	# D.7473
	movq	%rcx, %rdi	# D.7473, D.7473
	notq	%rdi	# D.7473
	addq	%rdi, %r14	# D.7473, n2
	cmpq	%rdx, %rax	# D.7471, D.7471
	cmova	%rdx, %rax	# D.7471,, D.7471, D.7475
	addq	%rcx, %r15	# D.7473, mid
	movq	%rax, -8(%r8)	# D.7475, MEM[base: tmp_117, offset: -8B]
	jne	.L35	#,
.L19:
	testq	%r15, %r15	# mid
	jne	.L11	#,
	testq	%r14, %r14	# n2
	jne	.L36	#,
.L15:
	addq	$8, %rsp	#,
	.cfi_remember_state
	.cfi_def_cfa_offset 56
	leaq	0(,%rbp,8), %rdx	#, D.7471
	movq	%r12, %rsi	# tmp,
	movq	%rbx, %rdi	# arr,
	popq	%rbx	#
	.cfi_restore 3
	.cfi_def_cfa_offset 48
	popq	%rbp	#
	.cfi_restore 6
	.cfi_def_cfa_offset 40
	popq	%r12	#
	.cfi_restore 12
	.cfi_def_cfa_offset 32
	popq	%r13	#
	.cfi_restore 13
	.cfi_def_cfa_offset 24
	popq	%r14	#
	.cfi_restore 14
	.cfi_def_cfa_offset 16
	popq	%r15	#
	.cfi_restore 15
	.cfi_def_cfa_offset 8
	jmp	memcpy	#
.L36:
	.cfi_restore_state
	leaq	0(,%r14,8), %rdx	#, D.7471
	movq	%r13, %rsi	# B,
	movq	%r8, %rdi	# tmp,
	call	memcpy	#
	jmp	.L15	#
.L34:
	.cfi_def_cfa_offset 8
	.cfi_restore 3
	.cfi_restore 6
	.cfi_restore 12
	.cfi_restore 13
	.cfi_restore 14
	.cfi_restore 15
	cmpq	$1, %rdx	#, len
	jbe	.L32	#,
	movl	$1, %r9d	#, ivtmp.64
	movl	$1, %eax	#, D.7471
	movl	$2, %r10d	#, ivtmp.65
	testl	%r9d, %r9d	# ivtmp.64
	movq	(%rdi,%rax,8), %r8	# *_18, temp
	je	.L6	#,
	.p2align 4,,10
	.p2align 3
.L41:
	leal	-1(%r9), %ecx	#,
	movq	%rcx, %rax	#,
	movq	(%rdi,%rcx,8), %rcx	# *_89, D.7471
	cmpq	%rcx, %r8	# D.7471, temp
	jb	.L27	#,
	jmp	.L37	#
	.p2align 4,,10
	.p2align 3
.L39:
	leal	-1(%rax), %ecx	#,
	movq	%rcx, %rsi	#,
	movq	(%rdi,%rcx,8), %rcx	# *_28, D.7471
	cmpq	%rcx, %r8	# D.7471, temp
	jnb	.L38	#,
	movl	%esi, %eax	# j,
.L27:
	leal	1(%rax), %esi	#, D.7471
	testl	%eax, %eax	# j
	movq	%rcx, (%rdi,%rsi,8)	# D.7471, *_23
	jne	.L39	#,
	xorl	%eax, %eax	# D.7471
.L8:
	movq	%r8, (%rdi,%rax)	# temp, *_31
	movl	%r10d, %eax	# ivtmp.65, D.7471
	cmpq	%rax, %rdx	# D.7471, len
	jbe	.L40	#,
.L10:
	addl	$1, %r9d	#, ivtmp.64
	addl	$1, %r10d	#, ivtmp.65
	movq	(%rdi,%rax,8), %r8	# *_18, temp
	testl	%r9d, %r9d	# ivtmp.64
	jne	.L41	#,
.L6:
	movq	%r8, (%rdi)	# temp, *arr_4(D)
	movl	%r10d, %eax	# ivtmp.65, D.7471
	jmp	.L10	#
	.p2align 4,,10
	.p2align 3
.L38:
	salq	$3, %rax	#, D.7471
	movq	%r8, (%rdi,%rax)	# temp, *_31
	movl	%r10d, %eax	# ivtmp.65, D.7471
	cmpq	%rax, %rdx	# D.7471, len
	ja	.L10	#,
.L40:
	rep ret
.L32:
	rep ret
.L11:
	.cfi_def_cfa_offset 64
	.cfi_offset 3, -56
	.cfi_offset 6, -48
	.cfi_offset 12, -40
	.cfi_offset 13, -32
	.cfi_offset 14, -24
	.cfi_offset 15, -16
	leaq	0(,%r15,8), %rdx	#, D.7471
	movq	%r8, %rdi	# tmp,
	call	memcpy	#
	testq	%r14, %r14	# n2
	movq	%rax, %r8	#, tmp
	je	.L15	#,
	jmp	.L36	#
.L37:
	.cfi_def_cfa_offset 8
	.cfi_restore 3
	.cfi_restore 6
	.cfi_restore 12
	.cfi_restore 13
	.cfi_restore 14
	.cfi_restore 15
	movl	%r9d, %eax	# ivtmp.64, D.7471
	salq	$3, %rax	#, D.7471
	jmp	.L8	#
	.cfi_endproc
.LFE70:
	.size	mergesort_internal_u64, .-mergesort_internal_u64
	.section	.text.unlikely
.LCOLDE0:
	.text
.LHOTE0:
	.section	.text.unlikely
.LCOLDB1:
	.text
.LHOTB1:
	.p2align 4,,15
	.type	mergesort_internal, @function
mergesort_internal:
.LFB67:
	.cfi_startproc
	pushq	%r15	#
	.cfi_def_cfa_offset 16
	.cfi_offset 15, -16
	pushq	%r14	#
	.cfi_def_cfa_offset 24
	.cfi_offset 14, -24
	pushq	%r13	#
	.cfi_def_cfa_offset 32
	.cfi_offset 13, -32
	pushq	%r12	#
	.cfi_def_cfa_offset 40
	.cfi_offset 12, -40
	pushq	%rbp	#
	.cfi_def_cfa_offset 48
	.cfi_offset 6, -48
	pushq	%rbx	#
	.cfi_def_cfa_offset 56
	.cfi_offset 3, -56
	movq	%rcx, %rbp	# cmp, cmp
	movq	%rdi, %rbx	# arr, arr
	subq	$40, %rsp	#,
	.cfi_def_cfa_offset 96
	cmpq	$4, %rdx	#, len
	movq	%rsi, 16(%rsp)	# tmp, %sfp
	movq	%rdx, 24(%rsp)	# len, %sfp
	jbe	.L77	#,
	movq	%rdx, %r12	# len, mid
	movq	%rdx, %r13	# len, len
	movq	%rsi, %r15	# tmp, tmp
	shrq	%r12	# mid
	leaq	(%rbx,%r12,8), %r14	#, B
	movq	%r12, %rdx	# mid,
	subq	%r12, %r13	# mid, n2
	call	mergesort_internal	#
	movq	%rbp, %rcx	# cmp,
	movq	%r13, %rdx	# n2,
	movq	%r15, %rsi	# tmp,
	movq	%r14, %rdi	# B,
	call	mergesort_internal	#
	testq	%r13, %r13	# n2
	je	.L60	#,
	leaq	8(%r15), %rdx	#, ivtmp.84
	movq	%rbx, %r15	# arr, arr
	jmp	.L56	#
	.p2align 4,,10
	.p2align 3
.L78:
	movq	(%r15), %rax	# *arr_98, D.7516
	subq	$1, %r12	#, mid
	addq	$8, %r15	#, arr
	addq	$8, %rdx	#, ivtmp.84
	movq	%rax, -16(%rdx)	# D.7516, MEM[base: _117, offset: -8B]
	testq	%r12, %r12	# mid
	je	.L61	#,
.L79:
	testq	%r13, %r13	# n2
	je	.L61	#,
.L56:
	movq	%rdx, 8(%rsp)	# ivtmp.84, %sfp
	movq	(%r14), %rsi	# *B_90,
	movq	(%r15), %rdi	# *arr_98,
	call	*%rbp	# cmp
	movq	8(%rsp), %rdx	# %sfp, ivtmp.84
	testl	%eax, %eax	# D.7517
	movq	%rdx, %r9	# ivtmp.84, tmp
	jne	.L78	#,
	movq	(%r14), %rax	# *B_90, D.7516
	subq	$1, %r13	#, n2
	addq	$8, %r14	#, B
	addq	$8, %rdx	#, ivtmp.84
	movq	%rax, -16(%rdx)	# D.7516, MEM[base: _117, offset: -8B]
	testq	%r12, %r12	# mid
	jne	.L79	#,
.L61:
	testq	%r12, %r12	# mid
	jne	.L53	#,
	testq	%r13, %r13	# n2
	jne	.L80	#,
.L59:
	movq	24(%rsp), %rdx	# %sfp, D.7514
	movq	16(%rsp), %rsi	# %sfp,
	addq	$40, %rsp	#,
	.cfi_remember_state
	.cfi_def_cfa_offset 56
	movq	%rbx, %rdi	# arr,
	popq	%rbx	#
	.cfi_def_cfa_offset 48
	popq	%rbp	#
	.cfi_def_cfa_offset 40
	popq	%r12	#
	.cfi_def_cfa_offset 32
	popq	%r13	#
	.cfi_def_cfa_offset 24
	popq	%r14	#
	.cfi_def_cfa_offset 16
	popq	%r15	#
	.cfi_def_cfa_offset 8
	salq	$3, %rdx	#, D.7514
	jmp	memcpy	#
.L80:
	.cfi_restore_state
	leaq	0(,%r13,8), %rdx	#, D.7514
	movq	%r14, %rsi	# B,
	movq	%r9, %rdi	# tmp,
	call	memcpy	#
	jmp	.L59	#
.L77:
	cmpq	$1, %rdx	#,
	jbe	.L42	#,
	movl	$1, %eax	#, D.7514
	movl	$1, 8(%rsp)	#, %sfp
	movl	$2, 16(%rsp)	#, %sfp
	movq	(%rbx,%rax,8), %r12	# *_19, temp
	movl	8(%rsp), %eax	# %sfp, j
	testl	%eax, %eax	# j
	movl	%eax, %r15d	# j, j
	jne	.L68	#,
	jmp	.L81	#
	.p2align 4,,10
	.p2align 3
.L51:
	movq	(%r14), %rax	# *_25, D.7516
	movl	%r15d, %edx	# j, D.7514
	testl	%r13d, %r13d	# j
	movl	%r13d, %r15d	# j, j
	movq	%rax, (%rbx,%rdx,8)	# D.7516, *_24
	je	.L82	#,
.L68:
	leal	-1(%r15), %eax	#,
	movq	%r12, %rsi	# temp,
	leaq	(%rbx,%rax,8), %r14	#, D.7515
	movq	%rax, %r13	#,
	movq	(%r14), %rdi	# *_25,
	call	*%rbp	# cmp
	testl	%eax, %eax	# D.7517
	je	.L51	#,
	movl	%r15d, %edx	# j, D.7514
	movl	16(%rsp), %eax	# %sfp, D.7514
	salq	$3, %rdx	#, D.7514
	cmpq	%rax, 24(%rsp)	# D.7514, %sfp
	movq	%r12, (%rbx,%rdx)	# temp, *_34
	jbe	.L42	#,
.L52:
	movl	8(%rsp), %ecx	# %sfp, j
	movq	(%rbx,%rax,8), %r12	# *_19, temp
	addl	$1, 16(%rsp)	#, %sfp
	addl	$1, %ecx	#, j
	movl	%ecx, 8(%rsp)	# j, %sfp
	movl	8(%rsp), %eax	# %sfp, j
	testl	%eax, %eax	# j
	movl	%eax, %r15d	# j, j
	jne	.L68	#,
.L81:
	movq	%r12, (%rbx)	# temp, *arr_4(D)
	movl	16(%rsp), %eax	# %sfp, D.7514
	jmp	.L52	#
	.p2align 4,,10
	.p2align 3
.L82:
	movl	16(%rsp), %eax	# %sfp, D.7514
	xorl	%edx, %edx	# D.7514
	cmpq	%rax, 24(%rsp)	# D.7514, %sfp
	movq	%r12, (%rbx,%rdx)	# temp, *_34
	ja	.L52	#,
.L42:
	addq	$40, %rsp	#,
	.cfi_remember_state
	.cfi_def_cfa_offset 56
	popq	%rbx	#
	.cfi_def_cfa_offset 48
	popq	%rbp	#
	.cfi_def_cfa_offset 40
	popq	%r12	#
	.cfi_def_cfa_offset 32
	popq	%r13	#
	.cfi_def_cfa_offset 24
	popq	%r14	#
	.cfi_def_cfa_offset 16
	popq	%r15	#
	.cfi_def_cfa_offset 8
	ret
.L60:
	.cfi_restore_state
	movq	16(%rsp), %r9	# %sfp, tmp
	movq	%rbx, %r15	# arr, arr
.L53:
	leaq	0(,%r12,8), %rdx	#, D.7514
	movq	%r9, %rdi	# tmp,
	movq	%r15, %rsi	# arr,
	call	memcpy	#
	testq	%r13, %r13	# n2
	movq	%rax, %r9	#, tmp
	je	.L59	#,
	jmp	.L80	#
	.cfi_endproc
.LFE67:
	.size	mergesort_internal, .-mergesort_internal
	.section	.text.unlikely
.LCOLDE1:
	.text
.LHOTE1:
	.section	.text.unlikely
.LCOLDB2:
	.text
.LHOTB2:
	.p2align 4,,15
	.globl	cmp_gt
	.type	cmp_gt, @function
cmp_gt:
.LFB58:
	.cfi_startproc
	xorl	%eax, %eax	# D.7532
	cmpq	%rsi, %rdi	# y, x
	seta	%al	#, D.7532
	ret
	.cfi_endproc
.LFE58:
	.size	cmp_gt, .-cmp_gt
	.section	.text.unlikely
.LCOLDE2:
	.text
.LHOTE2:
	.section	.text.unlikely
.LCOLDB3:
	.text
.LHOTB3:
	.p2align 4,,15
	.globl	cmp_lt
	.type	cmp_lt, @function
cmp_lt:
.LFB59:
	.cfi_startproc
	xorl	%eax, %eax	# D.7537
	cmpq	%rsi, %rdi	# y, x
	setb	%al	#, D.7537
	ret
	.cfi_endproc
.LFE59:
	.size	cmp_lt, .-cmp_lt
	.section	.text.unlikely
.LCOLDE3:
	.text
.LHOTE3:
	.section	.text.unlikely
.LCOLDB4:
	.text
.LHOTB4:
	.p2align 4,,15
	.globl	insertion_sort_generic
	.type	insertion_sort_generic, @function
insertion_sort_generic:
.LFB60:
	.cfi_startproc
	cmpq	$1, %rsi	#, len
	jbe	.L99	#,
	pushq	%r15	#
	.cfi_def_cfa_offset 16
	.cfi_offset 15, -16
	pushq	%r14	#
	.cfi_def_cfa_offset 24
	.cfi_offset 14, -24
	movl	$1, %eax	#, D.7558
	pushq	%r13	#
	.cfi_def_cfa_offset 32
	.cfi_offset 13, -32
	pushq	%r12	#
	.cfi_def_cfa_offset 40
	.cfi_offset 12, -40
	movq	%rdi, %r15	# input, input
	pushq	%rbp	#
	.cfi_def_cfa_offset 48
	.cfi_offset 6, -48
	pushq	%rbx	#
	.cfi_def_cfa_offset 56
	.cfi_offset 3, -56
	movq	%rdx, %r12	# cmp, cmp
	subq	$24, %rsp	#,
	.cfi_def_cfa_offset 80
	movq	(%r15,%rax,8), %rbp	# *_11, temp
	movl	$1, (%rsp)	#, %sfp
	movl	(%rsp), %eax	# %sfp, j
	movq	%rsi, 8(%rsp)	# len, %sfp
	movl	$2, 4(%rsp)	#, %sfp
	testl	%eax, %eax	# j
	movl	%eax, %ebx	# j,
	jne	.L94	#,
	jmp	.L102	#
	.p2align 4,,10
	.p2align 3
.L92:
	movq	(%r14), %rax	# *_16, D.7560
	movq	%rax, (%r15,%rbx,8)	# D.7560, *_23
	movl	%r13d, %ebx	# j,
	testl	%ebx, %ebx	# j
	je	.L103	#,
.L94:
	leal	-1(%rbx), %eax	#,
	movq	%rbp, %rsi	# temp,
	leaq	(%r15,%rax,8), %r14	#, D.7559
	movq	%rax, %r13	#,
	movq	(%r14), %rdi	# *_16,
	call	*%r12	# cmp
	testl	%eax, %eax	# D.7561
	je	.L92	#,
	movl	4(%rsp), %eax	# %sfp, D.7558
	salq	$3, %rbx	#, D.7558
	cmpq	8(%rsp), %rax	# %sfp, D.7558
	movq	%rbp, (%r15,%rbx)	# temp, *_28
	jnb	.L104	#,
.L93:
	movl	(%rsp), %edx	# %sfp, j
	movq	(%r15,%rax,8), %rbp	# *_11, temp
	addl	$1, 4(%rsp)	#, %sfp
	leal	1(%rdx), %ecx	#, j
	movl	%ecx, (%rsp)	# j, %sfp
	movl	(%rsp), %eax	# %sfp, j
	testl	%eax, %eax	# j
	movl	%eax, %ebx	# j,
	jne	.L94	#,
.L102:
	movq	%rbp, (%r15)	# temp, *input_10(D)
	movl	4(%rsp), %eax	# %sfp, D.7558
	jmp	.L93	#
	.p2align 4,,10
	.p2align 3
.L103:
	movl	4(%rsp), %eax	# %sfp, D.7558
	xorl	%ebx, %ebx	# D.7558
	cmpq	8(%rsp), %rax	# %sfp, D.7558
	movq	%rbp, (%r15,%rbx)	# temp, *_28
	jb	.L93	#,
.L104:
	addq	$24, %rsp	#,
	.cfi_def_cfa_offset 56
	popq	%rbx	#
	.cfi_restore 3
	.cfi_def_cfa_offset 48
	popq	%rbp	#
	.cfi_restore 6
	.cfi_def_cfa_offset 40
	popq	%r12	#
	.cfi_restore 12
	.cfi_def_cfa_offset 32
	popq	%r13	#
	.cfi_restore 13
	.cfi_def_cfa_offset 24
	popq	%r14	#
	.cfi_restore 14
	.cfi_def_cfa_offset 16
	popq	%r15	#
	.cfi_restore 15
	.cfi_def_cfa_offset 8
.L99:
	rep ret
	.cfi_endproc
.LFE60:
	.size	insertion_sort_generic, .-insertion_sort_generic
	.section	.text.unlikely
.LCOLDE4:
	.text
.LHOTE4:
	.section	.text.unlikely
.LCOLDB5:
	.text
.LHOTB5:
	.p2align 4,,15
	.globl	insertion_sort_u64
	.type	insertion_sort_u64, @function
insertion_sort_u64:
.LFB61:
	.cfi_startproc
	cmpq	$1, %rsi	#, len
	jbe	.L105	#,
	movl	$1, %r9d	#, ivtmp.112
	movl	$1, %eax	#, D.7581
	movl	$2, %r10d	#, ivtmp.113
	testl	%r9d, %r9d	# ivtmp.112
	movq	(%rdi,%rax,8), %r8	# *_10, temp
	je	.L108	#,
	.p2align 4,,10
	.p2align 3
.L120:
	leal	-1(%r9), %edx	#,
	movq	%rdx, %rax	#,
	movq	(%rdi,%rdx,8), %rdx	# *_35, D.7581
	cmpq	%rdx, %r8	# D.7581, temp
	jb	.L115	#,
	jmp	.L116	#
	.p2align 4,,10
	.p2align 3
.L118:
	leal	-1(%rax), %edx	#,
	movq	%rdx, %rcx	#,
	movq	(%rdi,%rdx,8), %rdx	# *_15, D.7581
	cmpq	%rdx, %r8	# D.7581, temp
	jnb	.L117	#,
	movl	%ecx, %eax	# j,
.L115:
	leal	1(%rax), %ecx	#, D.7581
	testl	%eax, %eax	# j
	movq	%rdx, (%rdi,%rcx,8)	# D.7581, *_19
	jne	.L118	#,
	xorl	%eax, %eax	# D.7581
.L110:
	movq	%r8, (%rdi,%rax)	# temp, *_23
	movl	%r10d, %eax	# ivtmp.113, D.7581
	cmpq	%rsi, %rax	# len, D.7581
	jnb	.L119	#,
.L112:
	addl	$1, %r9d	#, ivtmp.112
	addl	$1, %r10d	#, ivtmp.113
	movq	(%rdi,%rax,8), %r8	# *_10, temp
	testl	%r9d, %r9d	# ivtmp.112
	jne	.L120	#,
.L108:
	movq	%r8, (%rdi)	# temp, *input_9(D)
	movl	%r10d, %eax	# ivtmp.113, D.7581
	jmp	.L112	#
	.p2align 4,,10
	.p2align 3
.L117:
	salq	$3, %rax	#, D.7581
	movq	%r8, (%rdi,%rax)	# temp, *_23
	movl	%r10d, %eax	# ivtmp.113, D.7581
	cmpq	%rsi, %rax	# len, D.7581
	jb	.L112	#,
.L119:
	rep ret
.L105:
	rep ret
.L116:
	movl	%r9d, %eax	# ivtmp.112, D.7581
	salq	$3, %rax	#, D.7581
	jmp	.L110	#
	.cfi_endproc
.LFE61:
	.size	insertion_sort_u64, .-insertion_sort_u64
	.section	.text.unlikely
.LCOLDE5:
	.text
.LHOTE5:
	.section	.text.unlikely
.LCOLDB6:
	.text
.LHOTB6:
	.p2align 4,,15
	.globl	qsort_generic
	.type	qsort_generic, @function
qsort_generic:
.LFB63:
	.cfi_startproc
	pushq	%r15	#
	.cfi_def_cfa_offset 16
	.cfi_offset 15, -16
	pushq	%r14	#
	.cfi_def_cfa_offset 24
	.cfi_offset 14, -24
	pushq	%r13	#
	.cfi_def_cfa_offset 32
	.cfi_offset 13, -32
	pushq	%r12	#
	.cfi_def_cfa_offset 40
	.cfi_offset 12, -40
	pushq	%rbp	#
	.cfi_def_cfa_offset 48
	.cfi_offset 6, -48
	pushq	%rbx	#
	.cfi_def_cfa_offset 56
	.cfi_offset 3, -56
	movq	%rdx, %rbx	# cmp, cmp
	subq	$40, %rsp	#,
	.cfi_def_cfa_offset 96
	cmpq	$4, %rsi	#, len
	movq	%rdi, 16(%rsp)	# arr, %sfp
	movq	%rsi, 24(%rsp)	# len, %sfp
	jbe	.L135	#,
.L140:
	movq	16(%rsp), %rax	# %sfp, arr
	movq	24(%rsp), %rbp	# %sfp, j
	movq	$-1, 8(%rsp)	#, %sfp
	movq	(%rax), %r12	# *arr_95, pivot
	.p2align 4,,10
	.p2align 3
.L131:
	addq	$1, 8(%rsp)	#, %sfp
	movq	16(%rsp), %r14	# %sfp, arr
	movq	%r12, %rsi	# pivot,
	movq	8(%rsp), %rax	# %sfp, i
	movq	(%r14,%rax,8), %rdi	# MEM[base: arr_95, index: _62, step: 8, offset: 0B], MEM[base: arr_95, index: _62, step: 8, offset: 0B]
	call	*%rbx	# cmp
	testl	%eax, %eax	# D.7626
	jne	.L131	#,
	leaq	-1(%rbp), %r15	#, j
	leaq	(%r14,%r15,8), %r14	#, ivtmp.130
	jmp	.L133	#
	.p2align 4,,10
	.p2align 3
.L145:
	subq	$1, %r15	#, j
.L133:
	movq	(%r14), %rsi	# MEM[base: _64, offset: 0B],
	movq	%r14, %r13	# ivtmp.130, D.7624
	movq	%r12, %rdi	# pivot,
	subq	$8, %r14	#, ivtmp.130
	movq	%r15, %rbp	# j, j
	call	*%rbx	# cmp
	testl	%eax, %eax	# D.7626
	jne	.L145	#,
	movq	8(%rsp), %rcx	# %sfp, i
	cmpq	%r15, %rcx	# j, i
	jge	.L134	#,
	movq	16(%rsp), %rsi	# %sfp, arr
	movq	0(%r13), %rdx	# *_64, D.7625
	movq	(%rsi,%rcx,8), %rax	# MEM[base: arr_95, index: _62, step: 8, offset: 0B], __temp
	movq	%rdx, (%rsi,%rcx,8)	# D.7625, MEM[base: arr_95, index: _62, step: 8, offset: 0B]
	movq	%rax, 0(%r13)	# __temp, *_64
	jmp	.L131	#
.L134:
	movslq	%r15d, %rbp	# j, pivot_idx
	movq	16(%rsp), %r15	# %sfp, arr
	movq	%rbx, %rdx	# cmp,
	leaq	1(%rbp), %r12	#, D.7622
	movq	%r15, %rdi	# arr,
	movq	%r12, %rsi	# D.7622,
	call	qsort_generic	#
	movq	%rbp, %rcx	# pivot_idx, D.7621
	leaq	(%r15,%r12,8), %rdx	#, arr
	notq	%rcx	# D.7621
	addq	%rcx, 24(%rsp)	# D.7621, %sfp
	movq	24(%rsp), %rax	# %sfp, len
	movq	%rdx, 16(%rsp)	# arr, %sfp
	cmpq	$4, %rax	#, len
	ja	.L140	#,
.L135:
	cmpq	$1, 24(%rsp)	#, %sfp
	jbe	.L121	#,
	movl	$2, 8(%rsp)	#, %sfp
	movl	$1, %eax	#, D.7621
	movl	$1, %r12d	#, j
.L124:
	movq	16(%rsp), %rdx	# %sfp, arr
	testl	%r12d, %r12d	# j
	movl	%r12d, %r14d	# j,
	movq	(%rdx,%rax,8), %rbp	# *_30, temp
	jne	.L138	#,
	jmp	.L146	#
	.p2align 4,,10
	.p2align 3
.L129:
	movq	(%r15), %rax	# *_36, D.7625
	movq	16(%rsp), %rdx	# %sfp, arr
	movq	%rax, (%rdx,%r14,8)	# D.7625, *_35
	movl	%r13d, %r14d	# j,
	testl	%r14d, %r14d	# j
	je	.L147	#,
.L138:
	leal	-1(%r14), %eax	#,
	movq	%rbp, %rsi	# temp,
	leaq	(%rdx,%rax,8), %r15	#, D.7624
	movq	%rax, %r13	#,
	movq	(%r15), %rdi	# *_36,
	call	*%rbx	# cmp
	testl	%eax, %eax	# D.7626
	je	.L129	#,
	movq	16(%rsp), %rdx	# %sfp, arr
	movl	%r14d, %eax	# j, D.7621
	salq	$3, %rax	#, D.7621
	movq	%rbp, (%rdx,%rax)	# temp, *_45
	movl	8(%rsp), %eax	# %sfp, D.7621
	cmpq	24(%rsp), %rax	# %sfp, D.7621
	jnb	.L121	#,
.L130:
	addl	$1, 8(%rsp)	#, %sfp
	addl	$1, %r12d	#, j
	jmp	.L124	#
.L147:
	xorl	%eax, %eax	# D.7621
	movq	%rbp, (%rdx,%rax)	# temp, *_45
	movl	8(%rsp), %eax	# %sfp, D.7621
	cmpq	24(%rsp), %rax	# %sfp, D.7621
	jb	.L130	#,
.L121:
	addq	$40, %rsp	#,
	.cfi_remember_state
	.cfi_def_cfa_offset 56
	popq	%rbx	#
	.cfi_def_cfa_offset 48
	popq	%rbp	#
	.cfi_def_cfa_offset 40
	popq	%r12	#
	.cfi_def_cfa_offset 32
	popq	%r13	#
	.cfi_def_cfa_offset 24
	popq	%r14	#
	.cfi_def_cfa_offset 16
	popq	%r15	#
	.cfi_def_cfa_offset 8
	ret
.L146:
	.cfi_restore_state
	movq	%rbp, (%rdx)	# temp, *arr_103
	movl	8(%rsp), %eax	# %sfp, D.7621
	jmp	.L130	#
	.cfi_endproc
.LFE63:
	.size	qsort_generic, .-qsort_generic
	.section	.text.unlikely
.LCOLDE6:
	.text
.LHOTE6:
	.section	.text.unlikely
.LCOLDB7:
	.text
.LHOTB7:
	.p2align 4,,15
	.globl	qsort_u64
	.type	qsort_u64, @function
qsort_u64:
.LFB65:
	.cfi_startproc
	pushq	%r13	#
	.cfi_def_cfa_offset 16
	.cfi_offset 13, -16
	pushq	%r12	#
	.cfi_def_cfa_offset 24
	.cfi_offset 12, -24
	movq	%rsi, %r12	# len, len
	pushq	%rbp	#
	.cfi_def_cfa_offset 32
	.cfi_offset 6, -32
	pushq	%rbx	#
	.cfi_def_cfa_offset 40
	.cfi_offset 3, -40
	movq	%rdi, %rbp	# arr, arr
	subq	$8, %rsp	#,
	.cfi_def_cfa_offset 48
	cmpq	$4, %rsi	#, len
	jbe	.L161	#,
.L166:
	movq	0(%rbp), %rdi	# *arr_93, pivot
	movq	%r12, %rsi	# len, j
	movq	$-1, %r9	#, i
	.p2align 4,,10
	.p2align 3
.L157:
	addq	$1, %r9	#, i
	movq	0(%rbp,%r9,8), %r10	# MEM[base: arr_93, index: _56, step: 8, offset: 0B], D.7663
	cmpq	%r10, %rdi	# D.7663, pivot
	ja	.L157	#,
	leaq	-1(%rsi), %rdx	#, j
	leaq	0(%rbp,%rdx,8), %rax	#, ivtmp.155
	jmp	.L159	#
	.p2align 4,,10
	.p2align 3
.L170:
	subq	$1, %rdx	#, j
.L159:
	movq	%rax, %r8	# ivtmp.155, D.7666
	subq	$8, %rax	#, ivtmp.155
	movq	8(%rax), %rcx	# MEM[base: _217, offset: 8B], D.7663
	movq	%rdx, %rsi	# j, j
	cmpq	%rcx, %rdi	# D.7663, pivot
	jb	.L170	#,
	cmpq	%rdx, %r9	# j, i
	jge	.L160	#,
	movq	%rcx, 0(%rbp,%r9,8)	# D.7663, MEM[base: arr_93, index: _56, step: 8, offset: 0B]
	movq	%r10, (%r8)	# D.7663, *_58
	jmp	.L157	#
.L160:
	movslq	%edx, %rbx	# j, pivot_idx
	movq	%rbp, %rdi	# arr,
	leaq	1(%rbx), %r13	#, D.7664
	notq	%rbx	# D.7663
	addq	%rbx, %r12	# D.7663, len
	movq	%r13, %rsi	# D.7664,
	leaq	0(%rbp,%r13,8), %rbp	#, arr
	call	qsort_u64	#
	cmpq	$4, %r12	#, len
	ja	.L166	#,
.L161:
	cmpq	$1, %r12	#, len
	jbe	.L148	#,
	movl	$1, %edi	#, ivtmp.148
	movl	$1, %eax	#, D.7663
	movl	$2, %r8d	#, ivtmp.149
	testl	%edi, %edi	# ivtmp.148
	movq	0(%rbp,%rax,8), %rsi	# *_27, temp
	je	.L152	#,
.L174:
	leal	-1(%rdi), %edx	#,
	movq	%rdx, %rax	#,
	movq	0(%rbp,%rdx,8), %rdx	# *_24, D.7663
	cmpq	%rdx, %rsi	# D.7663, temp
	jb	.L167	#,
	jmp	.L171	#
	.p2align 4,,10
	.p2align 3
.L173:
	leal	-1(%rax), %edx	#,
	movq	%rdx, %rcx	#,
	movq	0(%rbp,%rdx,8), %rdx	# *_37, D.7663
	cmpq	%rdx, %rsi	# D.7663, temp
	jnb	.L172	#,
	movl	%ecx, %eax	# j,
.L167:
	leal	1(%rax), %ecx	#, D.7663
	testl	%eax, %eax	# j
	movq	%rdx, 0(%rbp,%rcx,8)	# D.7663, *_32
	jne	.L173	#,
	xorl	%eax, %eax	# D.7663
.L154:
	movq	%rsi, 0(%rbp,%rax)	# temp, *_40
	movl	%r8d, %eax	# ivtmp.149, D.7663
	cmpq	%r12, %rax	# len, D.7663
	jnb	.L148	#,
.L156:
	addl	$1, %edi	#, ivtmp.148
	addl	$1, %r8d	#, ivtmp.149
	movq	0(%rbp,%rax,8), %rsi	# *_27, temp
	testl	%edi, %edi	# ivtmp.148
	jne	.L174	#,
.L152:
	movq	%rsi, 0(%rbp)	# temp, *arr_90
	movl	%r8d, %eax	# ivtmp.149, D.7663
	jmp	.L156	#
.L172:
	salq	$3, %rax	#, D.7663
	movq	%rsi, 0(%rbp,%rax)	# temp, *_40
	movl	%r8d, %eax	# ivtmp.149, D.7663
	cmpq	%r12, %rax	# len, D.7663
	jb	.L156	#,
.L148:
	addq	$8, %rsp	#,
	.cfi_remember_state
	.cfi_def_cfa_offset 40
	popq	%rbx	#
	.cfi_def_cfa_offset 32
	popq	%rbp	#
	.cfi_def_cfa_offset 24
	popq	%r12	#
	.cfi_def_cfa_offset 16
	popq	%r13	#
	.cfi_def_cfa_offset 8
	ret
.L171:
	.cfi_restore_state
	movl	%edi, %eax	# ivtmp.148, D.7663
	salq	$3, %rax	#, D.7663
	jmp	.L154	#
	.cfi_endproc
.LFE65:
	.size	qsort_u64, .-qsort_u64
	.section	.text.unlikely
.LCOLDE7:
	.text
.LHOTE7:
	.section	.rodata.str1.1,"aMS",@progbits,1
.LC8:
	.string	"Out of memory\n"
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
	pushq	%r14	#
	.cfi_def_cfa_offset 16
	.cfi_offset 14, -16
	pushq	%r13	#
	.cfi_def_cfa_offset 24
	.cfi_offset 13, -24
	movq	%rdi, %r13	# arr, arr
	pushq	%r12	#
	.cfi_def_cfa_offset 32
	.cfi_offset 12, -32
	leaq	0(,%rsi,8), %r12	#, D.7676
	pushq	%rbp	#
	.cfi_def_cfa_offset 40
	.cfi_offset 6, -40
	pushq	%rbx	#
	.cfi_def_cfa_offset 48
	.cfi_offset 3, -48
	movq	%rsi, %rbp	# len, len
	movl	$1, %esi	#,
	movq	%r12, %rdi	# D.7676,
	movq	%rdx, %r14	# cmp, cmp
	call	calloc	#
	testq	%rax, %rax	# tmp
	movq	%rax, %rbx	#, tmp
	jne	.L176	#,
	testq	%r12, %r12	# D.7676
	je	.L176	#,
	movq	stderr(%rip), %rcx	# stderr,
	movl	$.LC8, %edi	#,
	movl	$14, %edx	#,
	movl	$1, %esi	#,
	call	fwrite	#
	movl	$6, %edi	#,
	call	raise	#
.L176:
	movq	%r13, %rdi	# arr,
	movq	%r14, %rcx	# cmp,
	movq	%rbp, %rdx	# len,
	movq	%rbx, %rsi	# tmp,
	call	mergesort_internal	#
	movq	%rbx, %rdi	# tmp,
	popq	%rbx	#
	.cfi_def_cfa_offset 40
	popq	%rbp	#
	.cfi_def_cfa_offset 32
	popq	%r12	#
	.cfi_def_cfa_offset 24
	popq	%r13	#
	.cfi_def_cfa_offset 16
	popq	%r14	#
	.cfi_def_cfa_offset 8
	jmp	free	#
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
.LFB71:
	.cfi_startproc
	pushq	%r13	#
	.cfi_def_cfa_offset 16
	.cfi_offset 13, -16
	pushq	%r12	#
	.cfi_def_cfa_offset 24
	.cfi_offset 12, -24
	leaq	0(,%rsi,8), %r12	#, D.7687
	pushq	%rbp	#
	.cfi_def_cfa_offset 32
	.cfi_offset 6, -32
	pushq	%rbx	#
	.cfi_def_cfa_offset 40
	.cfi_offset 3, -40
	movq	%rdi, %r13	# input, input
	movq	%rsi, %rbp	# len, len
	movq	%r12, %rdi	# D.7687,
	movl	$1, %esi	#,
	subq	$8, %rsp	#,
	.cfi_def_cfa_offset 48
	call	calloc	#
	testq	%rax, %rax	# tmp
	movq	%rax, %rbx	#, tmp
	jne	.L185	#,
	testq	%r12, %r12	# D.7687
	je	.L185	#,
	movq	stderr(%rip), %rcx	# stderr,
	movl	$.LC8, %edi	#,
	movl	$14, %edx	#,
	movl	$1, %esi	#,
	call	fwrite	#
	movl	$6, %edi	#,
	call	raise	#
.L185:
	movq	%r13, %rdi	# input,
	movq	%rbp, %rdx	# len,
	movq	%rbx, %rsi	# tmp,
	call	mergesort_internal_u64	#
	addq	$8, %rsp	#,
	.cfi_def_cfa_offset 40
	movq	%rbx, %rdi	# tmp,
	popq	%rbx	#
	.cfi_def_cfa_offset 32
	popq	%rbp	#
	.cfi_def_cfa_offset 24
	popq	%r12	#
	.cfi_def_cfa_offset 16
	popq	%r13	#
	.cfi_def_cfa_offset 8
	jmp	free	#
	.cfi_endproc
.LFE71:
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
.LFB74:
	.cfi_startproc
	pushq	%rbx	#
	.cfi_def_cfa_offset 16
	.cfi_offset 3, -16
	leaq	-2(%rsi), %rbx	#, D.7719
	shrq	%rbx	# start
	leaq	1(%rbx,%rbx), %r11	#, ivtmp.183
	.p2align 4,,10
	.p2align 3
.L195:
	cmpq	%r11, %rsi	# ivtmp.183, len
	movq	%rbx, %rdx	# start, root
	movq	%r11, %rcx	# ivtmp.183, swap
	jbe	.L200	#,
	movq	(%rdi,%rbx,8), %r9	# MEM[base: arr_4(D), index: root_22, step: 8, offset: 0B], D.7722
	movq	%r11, %r8	# ivtmp.183, D.7719
	jmp	.L201	#
	.p2align 4,,10
	.p2align 3
.L222:
	movq	%rcx, %rax	# swap, root
	cmpq	%rax, %rdx	# root, root
	je	.L200	#,
.L223:
	leaq	(%rdi,%rax,8), %rdx	#, D.7720
	leaq	1(%rax,%rax), %r8	#, D.7719
	movq	(%rdx), %rcx	# *_48, D.7719
	cmpq	%r8, %rsi	# D.7719, len
	movq	%rcx, (%r10)	# D.7719, *_35
	movq	%r9, (%rdx)	# D.7722, *_48
	movq	%r8, %rcx	# D.7719, swap
	jbe	.L200	#,
	movq	%rax, %rdx	# root, root
.L201:
	cmpq	%r9, (%rdi,%r8,8)	# D.7722, *_32
	leaq	2(%rdx,%rdx), %rax	#, D.7719
	leaq	(%rdi,%rdx,8), %r10	#, D.7720
	cmovbe	%rdx, %rcx	# swap,, root, swap
	cmpq	%rax, %rsi	# D.7719, len
	jbe	.L222	#,
	movq	(%rdi,%rcx,8), %r8	# *_43, tmp158
	cmpq	%r8, (%rdi,%rax,8)	# tmp158, *_38
	cmovbe	%rcx, %rax	# swap,, root
	cmpq	%rax, %rdx	# root, root
	jne	.L223	#,
.L200:
	subq	$2, %r11	#, ivtmp.183
	subq	$1, %rbx	#, start
	cmpq	$-1, %r11	#, ivtmp.183
	jne	.L195	#,
	subq	$1, %rsi	#, end
	.p2align 4,,10
	.p2align 3
.L203:
	movq	(%rdi,%rsi,8), %r9	# MEM[base: arr_4(D), index: _9, step: 8, offset: 0B], __temp
	movq	(%rdi), %rax	# *arr_4(D), D.7719
	xorl	%edx, %edx	# root
	cmpq	$1, %rsi	#, end
	movl	$1, %ecx	#, swap
	movl	$1, %r8d	#, D.7719
	movq	%rax, (%rdi,%rsi,8)	# D.7719, MEM[base: arr_4(D), index: _9, step: 8, offset: 0B]
	movq	%r9, (%rdi)	# __temp, *arr_4(D)
	ja	.L216	#,
	jmp	.L208	#
	.p2align 4,,10
	.p2align 3
.L224:
	movq	%rcx, %rax	# swap, root
	cmpq	%rax, %rdx	# root, root
	je	.L208	#,
.L225:
	leaq	(%rdi,%rax,8), %rdx	#, D.7720
	leaq	1(%rax,%rax), %r8	#, D.7719
	movq	(%rdx), %rcx	# *_74, D.7719
	cmpq	%r8, %rsi	# D.7719, end
	movq	%rcx, (%r10)	# D.7719, *_61
	movq	%r9, (%rdx)	# __temp, *_74
	movq	%r8, %rcx	# D.7719, swap
	jbe	.L208	#,
	movq	%rax, %rdx	# root, root
.L216:
	cmpq	(%rdi,%r8,8), %r9	# *_58, __temp
	leaq	2(%rdx,%rdx), %rax	#, D.7719
	leaq	(%rdi,%rdx,8), %r10	#, D.7720
	cmovnb	%rdx, %rcx	# swap,, root, swap
	cmpq	%rax, %rsi	# D.7719, end
	jbe	.L224	#,
	movq	(%rdi,%rcx,8), %rbx	# *_69, tmp159
	cmpq	%rbx, (%rdi,%rax,8)	# tmp159, *_64
	cmovbe	%rcx, %rax	# swap,, root
	cmpq	%rax, %rdx	# root, root
	jne	.L225	#,
.L208:
	subq	$1, %rsi	#, end
	testq	%rsi, %rsi	# end
	jg	.L203	#,
	popq	%rbx	#
	.cfi_def_cfa_offset 8
	ret
	.cfi_endproc
.LFE74:
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
.LFB75:
	.cfi_startproc
	pushq	%rbp	#
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movl	$16384, %edx	#,
	movq	%rsp, %rbp	#,
	.cfi_def_cfa_register 6
	pushq	%r15	#
	pushq	%r14	#
	pushq	%r13	#
	pushq	%r12	#
	pushq	%rbx	#
	.cfi_offset 15, -24
	.cfi_offset 14, -32
	.cfi_offset 13, -40
	.cfi_offset 12, -48
	.cfi_offset 3, -56
	movq	%rdi, %rbx	# in, in
	subq	$120, %rsp	#,
	movq	%rdi, -144(%rbp)	# in, %sfp
	movq	%rsi, -136(%rbp)	# sz, %sfp
	subq	$16400, %rsp	#,
	xorl	%esi, %esi	#
	leaq	15(%rsp), %rax	#, tmp271
	andq	$-16, %rax	#, tmp273
	movq	%rax, %r11	# tmp273, tmp273
	addq	$2048, %rax	#, hist1
	movq	%rax, -152(%rbp)	# hist1, %sfp
	leaq	12288(%r11), %rax	#, hist6
	movq	%r11, %rdi	# tmp273,
	leaq	4096(%r11), %r13	#, hist2
	leaq	6144(%r11), %r14	#, hist3
	leaq	8192(%r11), %r15	#, hist4
	movq	%rax, -112(%rbp)	# hist6, %sfp
	leaq	14336(%r11), %rax	#, hist7
	leaq	10240(%r11), %r12	#, hist5
	movq	%r11, -56(%rbp)	# tmp273, %sfp
	movq	%rax, -120(%rbp)	# hist7, %sfp
	call	memset	#
	movq	-136(%rbp), %rdi	# %sfp, sz
	movq	-56(%rbp), %r11	# %sfp, tmp273
	testq	%rdi, %rdi	# sz
	leaq	(%rbx,%rdi,8), %rsi	#, D.7794
	je	.L230	#,
	movq	-152(%rbp), %rdi	# %sfp, hist1
	movq	-112(%rbp), %r8	# %sfp, hist6
	movq	%rbx, %rcx	# in, ivtmp.248
	movq	-120(%rbp), %r9	# %sfp, hist7
	.p2align 4,,10
	.p2align 3
.L249:
	movq	(%rcx), %rax	# MEM[base: _139, offset: 0B], D.7792
	addq	$8, %rcx	#, ivtmp.248
	movzbl	%al, %edx	# D.7792, D.7792
	addq	$1, (%r11,%rdx,8)	#, *_48
	movzbl	%ah, %edx	# D.7792, D.7792
	addq	$1, (%rdi,%rdx,8)	#, *_58
	movq	%rax, %rdx	# D.7792, D.7792
	shrq	$13, %rdx	#, D.7792
	andl	$2040, %edx	#, D.7792
	addq	$1, 0(%r13,%rdx)	#, *_68
	movq	%rax, %rdx	# D.7792, D.7792
	shrq	$21, %rdx	#, D.7792
	andl	$2040, %edx	#, D.7792
	addq	$1, (%r14,%rdx)	#, *_78
	movq	%rax, %rdx	# D.7792, D.7792
	shrq	$29, %rdx	#, D.7792
	andl	$2040, %edx	#, D.7792
	addq	$1, (%r15,%rdx)	#, *_85
	movq	%rax, %rdx	# D.7792, D.7792
	shrq	$37, %rdx	#, D.7792
	andl	$2040, %edx	#, D.7792
	addq	$1, (%r12,%rdx)	#, *_92
	movq	%rax, %rdx	# D.7792, D.7792
	shrq	$56, %rax	#, D.7792
	shrq	$45, %rdx	#, D.7792
	andl	$2040, %edx	#, D.7792
	addq	$1, (%r8,%rdx)	#, *_99
	addq	$1, (%r9,%rax,8)	#, *_105
	cmpq	%rcx, %rsi	# ivtmp.248, D.7794
	jne	.L249	#,
.L230:
	movq	%r13, -128(%rbp)	# hist2, %sfp
	xorl	%eax, %eax	# i
	movq	%r14, %r13	# hist3, hist3
	xorl	%r10d, %r10d	# sum7
	movq	%r15, %r14	# hist4, hist4
	xorl	%r9d, %r9d	# sum6
	xorl	%ebx, %ebx	# sum5
	xorl	%r8d, %r8d	# sum4
	xorl	%edi, %edi	# sum3
	xorl	%esi, %esi	# sum2
	xorl	%ecx, %ecx	# sum1
	xorl	%edx, %edx	# sum0
	movq	%r12, %r15	# hist5, hist5
	.p2align 4,,10
	.p2align 3
.L228:
	movq	%rdx, %r12	# sum0, sum0
	addq	(%r11,%rax,8), %r12	# MEM[base: hist0_31, index: i_316, step: 8, offset: 0B], sum0
	movq	%rdx, (%r11,%rax,8)	# sum0, MEM[base: hist0_31, index: i_316, step: 8, offset: 0B]
	movq	%rcx, %rdx	# sum1, sum1
	addq	2048(%r11,%rax,8), %rdx	# MEM[base: hist0_31, index: i_316, step: 8, offset: 2048B], sum1
	movq	%rcx, 2048(%r11,%rax,8)	# sum1, MEM[base: hist0_31, index: i_316, step: 8, offset: 2048B]
	movq	%rsi, %rcx	# sum2, sum2
	movq	%rdx, -56(%rbp)	# sum1, %sfp
	movq	-128(%rbp), %rdx	# %sfp, hist2
	addq	(%rdx,%rax,8), %rcx	# MEM[base: hist2_34, index: i_316, step: 8, offset: 0B], sum2
	movq	%rsi, (%rdx,%rax,8)	# sum2, MEM[base: hist2_34, index: i_316, step: 8, offset: 0B]
	movq	%rdi, %rsi	# sum3, sum3
	movq	%rbx, %rdx	# sum5, sum5
	addq	0(%r13,%rax,8), %rsi	# MEM[base: hist3_35, index: i_316, step: 8, offset: 0B], sum3
	movq	%rdi, 0(%r13,%rax,8)	# sum3, MEM[base: hist3_35, index: i_316, step: 8, offset: 0B]
	movq	%r8, %rdi	# sum4, sum4
	addq	(%r14,%rax,8), %rdi	# MEM[base: hist4_36, index: i_316, step: 8, offset: 0B], sum4
	movq	%r8, (%r14,%rax,8)	# sum4, MEM[base: hist4_36, index: i_316, step: 8, offset: 0B]
	addq	(%r15,%rax,8), %rdx	# MEM[base: hist5_37, index: i_316, step: 8, offset: 0B], sum5
	movq	%rbx, (%r15,%rax,8)	# sum5, MEM[base: hist5_37, index: i_316, step: 8, offset: 0B]
	movq	%r9, %r8	# sum6, sum6
	movq	-112(%rbp), %rbx	# %sfp, hist6
	movq	%rcx, -64(%rbp)	# sum2, %sfp
	movq	%rsi, -72(%rbp)	# sum3, %sfp
	movq	-56(%rbp), %rcx	# %sfp, sum1
	movq	%rdi, -80(%rbp)	# sum4, %sfp
	movq	-64(%rbp), %rsi	# %sfp, sum2
	addq	(%rbx,%rax,8), %r8	# MEM[base: hist6_38, index: i_316, step: 8, offset: 0B], sum6
	movq	%r9, (%rbx,%rax,8)	# sum6, MEM[base: hist6_38, index: i_316, step: 8, offset: 0B]
	movq	%r10, %r9	# sum7, sum7
	movq	-120(%rbp), %rbx	# %sfp, hist7
	movq	%rdx, -88(%rbp)	# sum5, %sfp
	movq	%r12, %rdx	# sum0, sum0
	movq	-72(%rbp), %rdi	# %sfp, sum3
	addq	(%rbx,%rax,8), %r9	# MEM[base: hist7_39, index: i_316, step: 8, offset: 0B], sum7
	movq	%r10, (%rbx,%rax,8)	# sum7, MEM[base: hist7_39, index: i_316, step: 8, offset: 0B]
	addq	$1, %rax	#, i
	cmpq	$256, %rax	#, i
	movq	%r8, -96(%rbp)	# sum6, %sfp
	movq	-88(%rbp), %rbx	# %sfp, sum5
	movq	-80(%rbp), %r8	# %sfp, sum4
	movq	%r9, -104(%rbp)	# sum7, %sfp
	movq	-96(%rbp), %r9	# %sfp, sum6
	movq	-104(%rbp), %r10	# %sfp, sum7
	jne	.L228	#,
	movq	-136(%rbp), %rax	# %sfp, sz
	movl	$1, %esi	#,
	movq	%r11, -64(%rbp)	# tmp273, %sfp
	movq	%r15, %r12	# hist5, hist5
	movq	%r14, %r15	# hist4, hist4
	movq	%r13, %r14	# hist3, hist3
	movq	-128(%rbp), %r13	# %sfp, hist2
	leaq	0(,%rax,8), %r9	#, D.7792
	movq	%r9, %rdi	# D.7792,
	movq	%r9, -56(%rbp)	# D.7792, %sfp
	call	calloc	#
	testq	%rax, %rax	# b
	movq	%rax, %rbx	#, b
	movq	-56(%rbp), %r9	# %sfp, D.7792
	movq	-64(%rbp), %r11	# %sfp, tmp273
	jne	.L231	#,
	testq	%r9, %r9	# D.7792
	je	.L231	#,
	movq	stderr(%rip), %rcx	# stderr,
	movl	$14, %edx	#,
	movl	$1, %esi	#,
	movl	$.LC8, %edi	#,
	call	fwrite	#
	movl	$6, %edi	#,
	call	raise	#
	cmpq	$0, -136(%rbp)	#, %sfp
	movq	-56(%rbp), %r9	# %sfp, D.7792
	movq	-64(%rbp), %r11	# %sfp, tmp273
	jne	.L242	#,
.L232:
	movq	%rbx, %rdi	# b,
	call	free	#
	leaq	-40(%rbp), %rsp	#,
	popq	%rbx	#
	popq	%r12	#
	popq	%r13	#
	popq	%r14	#
	popq	%r15	#
	popq	%rbp	#
	.cfi_remember_state
	.cfi_def_cfa 7, 8
	ret
.L231:
	.cfi_restore_state
	cmpq	$0, -136(%rbp)	#, %sfp
	je	.L232	#,
.L242:
	movq	-144(%rbp), %rdi	# %sfp, in
	leaq	(%rdi,%r9), %r8	#, D.7794
	movq	%rdi, %rax	# in, ivtmp.228
	.p2align 4,,10
	.p2align 3
.L233:
	movq	(%rax), %rcx	# MEM[base: _163, offset: 0B], D.7792
	addq	$8, %rax	#, ivtmp.228
	cmpq	%r8, %rax	# D.7794, ivtmp.228
	movzbl	%cl, %edx	# D.7792, pos
	leaq	(%r11,%rdx,8), %rsi	#, D.7793
	movq	(%rsi), %rdx	# *_152, D.7792
	leaq	1(%rdx), %rdi	#, tmp316
	movq	%rcx, (%rbx,%rdx,8)	# D.7792, *_157
	movq	%rdi, (%rsi)	# tmp316, *_152
	jne	.L233	#,
	leaq	(%rbx,%r9), %r8	#, D.7794
	movq	-144(%rbp), %r10	# %sfp, in
	movq	-152(%rbp), %r9	# %sfp, hist1
	movq	%rbx, %rax	# b, ivtmp.223
	.p2align 4,,10
	.p2align 3
.L234:
	movq	(%rax), %rcx	# MEM[base: _195, offset: 0B], D.7792
	addq	$8, %rax	#, ivtmp.223
	cmpq	%r8, %rax	# D.7794, ivtmp.223
	movzbl	%ch, %edx	# D.7792, pos
	leaq	(%r9,%rdx,8), %rsi	#, D.7793
	movq	(%rsi), %rdx	# *_168, D.7792
	leaq	1(%rdx), %rdi	#, tmp320
	movq	%rcx, (%r10,%rdx,8)	# D.7792, *_173
	movq	%rdi, (%rsi)	# tmp320, *_168
	jne	.L234	#,
	movq	-144(%rbp), %r8	# %sfp, in
	movq	-136(%rbp), %r9	# %sfp, sz
	xorl	%edx, %edx	# i
	.p2align 4,,10
	.p2align 3
.L235:
	movq	(%r8,%rdx,8), %rsi	# MEM[base: in_43(D), index: i_193, step: 8, offset: 0B], D.7792
	addq	$1, %rdx	#, i
	movq	%rsi, %rax	# D.7792, pos
	shrq	$13, %rax	#, pos
	andl	$2040, %eax	#, D.7792
	addq	%r13, %rax	# hist2, D.7793
	cmpq	%rdx, %r9	# i, sz
	movq	(%rax), %rcx	# *_184, D.7792
	leaq	1(%rcx), %rdi	#, tmp324
	movq	%rsi, (%rbx,%rcx,8)	# D.7792, *_189
	movq	%rdi, (%rax)	# tmp324, *_184
	ja	.L235	#,
	movq	-144(%rbp), %r8	# %sfp, in
	movq	-136(%rbp), %r9	# %sfp, sz
	xorl	%edx, %edx	# i
	.p2align 4,,10
	.p2align 3
.L236:
	movq	(%rbx,%rdx,8), %rsi	# MEM[base: b_271, index: i_209, step: 8, offset: 0B], D.7792
	addq	$1, %rdx	#, i
	movq	%rsi, %rax	# D.7792, pos
	shrq	$21, %rax	#, pos
	andl	$2040, %eax	#, D.7792
	addq	%r14, %rax	# hist3, D.7793
	cmpq	%rdx, %r9	# i, sz
	movq	(%rax), %rcx	# *_200, D.7792
	leaq	1(%rcx), %rdi	#, tmp328
	movq	%rsi, (%r8,%rcx,8)	# D.7792, *_205
	movq	%rdi, (%rax)	# tmp328, *_200
	ja	.L236	#,
	movq	-144(%rbp), %r8	# %sfp, in
	movq	-136(%rbp), %r9	# %sfp, sz
	xorl	%edx, %edx	# i
	.p2align 4,,10
	.p2align 3
.L237:
	movq	(%r8,%rdx,8), %rsi	# MEM[base: in_43(D), index: i_225, step: 8, offset: 0B], D.7792
	addq	$1, %rdx	#, i
	movq	%rsi, %rax	# D.7792, pos
	shrq	$29, %rax	#, pos
	andl	$2040, %eax	#, D.7792
	addq	%r15, %rax	# hist4, D.7793
	cmpq	%rdx, %r9	# i, sz
	movq	(%rax), %rcx	# *_216, D.7792
	leaq	1(%rcx), %rdi	#, tmp332
	movq	%rsi, (%rbx,%rcx,8)	# D.7792, *_221
	movq	%rdi, (%rax)	# tmp332, *_216
	ja	.L237	#,
	movq	-144(%rbp), %r8	# %sfp, in
	movq	-136(%rbp), %r9	# %sfp, sz
	xorl	%edx, %edx	# i
	.p2align 4,,10
	.p2align 3
.L238:
	movq	(%rbx,%rdx,8), %rsi	# MEM[base: b_271, index: i_241, step: 8, offset: 0B], D.7792
	addq	$1, %rdx	#, i
	movq	%rsi, %rax	# D.7792, pos
	shrq	$37, %rax	#, pos
	andl	$2040, %eax	#, D.7792
	addq	%r12, %rax	# hist5, D.7793
	cmpq	%rdx, %r9	# i, sz
	movq	(%rax), %rcx	# *_232, D.7792
	leaq	1(%rcx), %rdi	#, tmp336
	movq	%rsi, (%r8,%rcx,8)	# D.7792, *_237
	movq	%rdi, (%rax)	# tmp336, *_232
	ja	.L238	#,
	movq	-112(%rbp), %r8	# %sfp, hist6
	movq	-144(%rbp), %r9	# %sfp, in
	xorl	%edx, %edx	# i
	movq	-136(%rbp), %r10	# %sfp, sz
	.p2align 4,,10
	.p2align 3
.L240:
	movq	(%r9,%rdx,8), %rsi	# MEM[base: in_43(D), index: i_73, step: 8, offset: 0B], D.7792
	addq	$1, %rdx	#, i
	movq	%rsi, %rax	# D.7792, pos
	shrq	$45, %rax	#, pos
	andl	$2040, %eax	#, D.7792
	addq	%r8, %rax	# hist6, D.7793
	cmpq	%rdx, %r10	# i, sz
	movq	(%rax), %rcx	# *_248, D.7792
	leaq	1(%rcx), %rdi	#, tmp340
	movq	%rsi, (%rbx,%rcx,8)	# D.7792, *_253
	movq	%rdi, (%rax)	# tmp340, *_248
	ja	.L240	#,
	movq	-120(%rbp), %r8	# %sfp, hist7
	movq	-144(%rbp), %r9	# %sfp, in
	xorl	%eax, %eax	# i
	movq	-136(%rbp), %r10	# %sfp, sz
	.p2align 4,,10
	.p2align 3
.L241:
	movq	(%rbx,%rax,8), %rcx	# MEM[base: b_271, index: i_64, step: 8, offset: 0B], D.7792
	addq	$1, %rax	#, i
	movq	%rcx, %rdx	# D.7792, pos
	shrq	$56, %rdx	#, pos
	cmpq	%rax, %r10	# i, sz
	leaq	(%r8,%rdx,8), %rsi	#, D.7793
	movq	(%rsi), %rdx	# *_262, D.7792
	leaq	1(%rdx), %rdi	#, tmp343
	movq	%rcx, (%r9,%rdx,8)	# D.7792, *_267
	movq	%rdi, (%rsi)	# tmp343, *_262
	ja	.L241	#,
	jmp	.L232	#
	.cfi_endproc
.LFE75:
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
.LFB76:
	.cfi_startproc
	pushq	%r15	#
	.cfi_def_cfa_offset 16
	.cfi_offset 15, -16
	pushq	%r14	#
	.cfi_def_cfa_offset 24
	.cfi_offset 14, -24
	movq	%rdi, %r14	# in, in
	pushq	%r13	#
	.cfi_def_cfa_offset 32
	.cfi_offset 13, -32
	pushq	%r12	#
	.cfi_def_cfa_offset 40
	.cfi_offset 12, -40
	movl	$16384, %edx	#,
	pushq	%rbp	#
	.cfi_def_cfa_offset 48
	.cfi_offset 6, -48
	pushq	%rbx	#
	.cfi_def_cfa_offset 56
	.cfi_offset 3, -56
	movq	%rsi, %rbx	# sz, sz
	subq	$16504, %rsp	#,
	.cfi_def_cfa_offset 16560
	movq	%rdi, 32(%rsp)	# in, %sfp
	leaq	112(%rsp), %rdi	#, tmp284
	movq	%rsi, 40(%rsp)	# sz, %sfp
	xorl	%esi, %esi	#
	call	memset	#
	leaq	48(%rsp), %rdx	#, tmp205
	xorl	%eax, %eax	# tmp207
	movl	$8, %ecx	#, tmp208
	testq	%rbx, %rbx	# sz
	movq	%rdx, %rdi	# tmp205, tmp206
	rep stosq
	je	.L267	#,
	leaq	(%r14,%rbx,8), %rsi	#, D.7869
	movq	%r14, %rcx	# in, ivtmp.304
	.p2align 4,,10
	.p2align 3
.L268:
	movq	(%rcx), %rax	# MEM[base: _69, offset: 0B], D.7863
	addq	$8, %rcx	#, ivtmp.304
	movzbl	%al, %edx	# D.7863, D.7863
	addq	$1, 112(%rsp,%rdx,8)	#, hist
	movzbl	%ah, %edx	# D.7863, D.7863
	addq	$1, 2160(%rsp,%rdx,8)	#, hist
	movq	%rax, %rdx	# D.7863, D.7863
	shrq	$16, %rdx	#, D.7863
	movzbl	%dl, %edx	# D.7863, D.7863
	addq	$1, 4208(%rsp,%rdx,8)	#, hist
	movq	%rax, %rdx	# D.7863, D.7863
	shrq	$24, %rdx	#, D.7863
	movzbl	%dl, %edx	# D.7863, D.7863
	addq	$1, 6256(%rsp,%rdx,8)	#, hist
	movq	%rax, %rdx	# D.7863, D.7863
	shrq	$32, %rdx	#, D.7863
	movzbl	%dl, %edx	# D.7863, D.7863
	addq	$1, 8304(%rsp,%rdx,8)	#, hist
	movq	%rax, %rdx	# D.7863, D.7863
	shrq	$40, %rdx	#, D.7863
	movzbl	%dl, %edx	# D.7863, D.7863
	addq	$1, 10352(%rsp,%rdx,8)	#, hist
	movq	%rax, %rdx	# D.7863, D.7863
	shrq	$56, %rax	#, D.7863
	shrq	$48, %rdx	#, D.7863
	movzbl	%dl, %edx	# D.7863, D.7863
	addq	$1, 12400(%rsp,%rdx,8)	#, hist
	addq	$1, 14448(%rsp,%rax,8)	#, hist
	cmpq	%rcx, %rsi	# ivtmp.304, D.7869
	jne	.L268	#,
.L267:
	leaq	112(%rsp), %rax	#, tmp294
	xorl	%r8d, %r8d	# D.7866
	xorl	%edx, %edx	# D.7866
	xorl	%r9d, %r9d	# D.7866
	xorl	%ecx, %ecx	# D.7866
	xorl	%r10d, %r10d	# D.7866
	leaq	2048(%rax), %rsi	#, D.7869
	xorl	%r11d, %r11d	# D.7866
	xorl	%edi, %edi	# D.7866
	movq	%rsi, 24(%rsp)	# D.7869, %sfp
	xorl	%esi, %esi	# D.7866
	.p2align 4,,10
	.p2align 3
.L269:
	movq	%rdi, %r15	# D.7866, total
	movq	%r11, %r14	# D.7866, total
	addq	(%rax), %r15	# MEM[base: _86, offset: 0B], total
	addq	2048(%rax), %r14	# MEM[base: _86, offset: 2048B], total
	movq	%rdi, (%rax)	# D.7866, MEM[base: _86, offset: 0B]
	movq	%rsi, %r13	# D.7866, total
	movq	%r11, 2048(%rax)	# D.7866, MEM[base: _86, offset: 2048B]
	addq	4096(%rax), %r13	# MEM[base: _86, offset: 4096B], total
	movq	%r10, %r12	# D.7866, total
	movq	%rsi, 4096(%rax)	# D.7866, MEM[base: _86, offset: 4096B]
	addq	6144(%rax), %r12	# MEM[base: _86, offset: 6144B], total
	movq	%rcx, %rbp	# D.7866, total
	movq	%r10, 6144(%rax)	# D.7866, MEM[base: _86, offset: 6144B]
	addq	8192(%rax), %rbp	# MEM[base: _86, offset: 8192B], total
	movq	%r9, %rbx	# D.7866, total
	movq	%rcx, 8192(%rax)	# D.7866, MEM[base: _86, offset: 8192B]
	addq	10240(%rax), %rbx	# MEM[base: _86, offset: 10240B], total
	movq	%rdx, %rcx	# D.7866, total
	movq	%r9, 10240(%rax)	# D.7866, MEM[base: _86, offset: 10240B]
	addq	12288(%rax), %rcx	# MEM[base: _86, offset: 12288B], total
	movq	%r8, %rdi	# D.7866, total
	movq	%rdx, 12288(%rax)	# D.7866, MEM[base: _86, offset: 12288B]
	addq	14336(%rax), %rdi	# MEM[base: _86, offset: 14336B], total
	addq	$8, %rax	#, ivtmp.292
	movq	%r8, 14328(%rax)	# D.7866, MEM[base: _86, offset: 14336B]
	cmpq	24(%rsp), %rax	# %sfp, ivtmp.292
	movq	%r14, %r11	# total, D.7866
	movq	%r13, %rsi	# total, D.7866
	movq	%r12, %r10	# total, D.7866
	movq	%rbx, %r9	# total, D.7866
	movq	%rcx, 8(%rsp)	# total, %sfp
	movq	%rbp, %rcx	# total, D.7866
	movq	8(%rsp), %rdx	# %sfp, D.7866
	movq	%rdi, 16(%rsp)	# total, %sfp
	movq	%r15, %rdi	# total, D.7866
	movq	16(%rsp), %r8	# %sfp, D.7866
	jne	.L269	#,
	movq	%r15, 8(%rsp)	# D.7866, %sfp
	movq	40(%rsp), %rax	# %sfp, sz
	movl	$1, %esi	#,
	movq	8(%rsp), %xmm0	# %sfp, vect_cst_.271
	movq	%r14, 8(%rsp)	# D.7866, %sfp
	movhps	8(%rsp), %xmm0	# %sfp, vect_cst_.271
	movq	%r13, 8(%rsp)	# D.7866, %sfp
	movaps	%xmm0, 48(%rsp)	# vect_cst_.271, MEM[(long unsigned int *)&sum]
	movq	8(%rsp), %xmm0	# %sfp, vect_cst_.270
	movq	%r12, 8(%rsp)	# D.7866, %sfp
	movhps	8(%rsp), %xmm0	# %sfp, vect_cst_.270
	movq	%rbp, 8(%rsp)	# D.7866, %sfp
	movaps	%xmm0, 64(%rsp)	# vect_cst_.270, MEM[(long unsigned int *)&sum + 16B]
	movq	8(%rsp), %xmm0	# %sfp, vect_cst_.269
	movq	%rbx, 8(%rsp)	# D.7866, %sfp
	movhps	8(%rsp), %xmm0	# %sfp, vect_cst_.269
	movq	%rdx, 8(%rsp)	# D.7866, %sfp
	leaq	0(,%rax,8), %rbx	#, D.7863
	movaps	%xmm0, 80(%rsp)	# vect_cst_.269, MEM[(long unsigned int *)&sum + 32B]
	movq	%rbx, %rdi	# D.7863,
	movq	8(%rsp), %xmm0	# %sfp, vect_cst_.268
	movq	%r8, 8(%rsp)	# D.7866, %sfp
	movhps	8(%rsp), %xmm0	# %sfp, vect_cst_.268
	movaps	%xmm0, 96(%rsp)	# vect_cst_.268, MEM[(long unsigned int *)&sum + 48B]
	call	calloc	#
	testq	%rax, %rax	# in
	movq	%rax, %rbp	#, in
	jne	.L270	#,
	testq	%rbx, %rbx	# D.7863
	je	.L270	#,
	movq	stderr(%rip), %rcx	# stderr,
	movl	$.LC8, %edi	#,
	movl	$14, %edx	#,
	movl	$1, %esi	#,
	call	fwrite	#
	movl	$6, %edi	#,
	call	raise	#
.L270:
	movq	%rbp, %r8	# in, in
	xorl	%ecx, %ecx	# ivtmp.285
	xorl	%r11d, %r11d	# j
	.p2align 4,,10
	.p2align 3
.L271:
	cmpq	$0, 40(%rsp)	#, %sfp
	je	.L274	#,
	movq	32(%rsp), %rax	# %sfp, in
	movq	%r11, %r9	# j, tmp283
	salq	$8, %r9	#, tmp283
	leaq	(%rax,%rbx), %r10	#, D.7869
	movq	%rax, %rdx	# in, ivtmp.277
	.p2align 4,,10
	.p2align 3
.L272:
	movq	(%rdx), %rdi	# MEM[base: _193, offset: 0B], D.7863
	addq	$8, %rdx	#, ivtmp.277
	movq	%rdi, %rax	# D.7863, D.7863
	shrq	%cl, %rax	# ivtmp.285, D.7863
	movzbl	%al, %eax	# D.7863, pos
	addq	%r9, %rax	# tmp283, tmp278
	cmpq	%r10, %rdx	# D.7869, ivtmp.277
	movq	112(%rsp,%rax,8), %rsi	# hist, D.7863
	leaq	1(%rsi), %r12	#, tmp281
	movq	%rdi, (%r8,%rsi,8)	# D.7863, *_84
	movq	%r12, 112(%rsp,%rax,8)	# tmp281, hist
	jne	.L272	#,
.L274:
	addq	$1, %r11	#, j
	addl	$8, %ecx	#, ivtmp.285
	movq	%r8, %rax	# in, in
	cmpq	$8, %r11	#, j
	movq	32(%rsp), %r8	# %sfp, in
	je	.L273	#,
	movq	%rax, 32(%rsp)	# in, %sfp
	jmp	.L271	#
.L273:
	addq	$16504, %rsp	#,
	.cfi_def_cfa_offset 56
	movq	%rbp, %rdi	# in,
	popq	%rbx	#
	.cfi_def_cfa_offset 48
	popq	%rbp	#
	.cfi_def_cfa_offset 40
	popq	%r12	#
	.cfi_def_cfa_offset 32
	popq	%r13	#
	.cfi_def_cfa_offset 24
	popq	%r14	#
	.cfi_def_cfa_offset 16
	popq	%r15	#
	.cfi_def_cfa_offset 8
	jmp	free	#
	.cfi_endproc
.LFE76:
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
.LFB77:
	.cfi_startproc
	pushq	%r15	#
	.cfi_def_cfa_offset 16
	.cfi_offset 15, -16
	pushq	%r14	#
	.cfi_def_cfa_offset 24
	.cfi_offset 14, -24
	movq	%rsi, %r14	# sz, sz
	pushq	%r13	#
	.cfi_def_cfa_offset 32
	.cfi_offset 13, -32
	pushq	%r12	#
	.cfi_def_cfa_offset 40
	.cfi_offset 12, -40
	pushq	%rbp	#
	.cfi_def_cfa_offset 48
	.cfi_offset 6, -48
	pushq	%rbx	#
	.cfi_def_cfa_offset 56
	.cfi_offset 3, -56
	movq	%rdi, %rbx	# in, in
	subq	$16504, %rsp	#,
	.cfi_def_cfa_offset 16560
	leaq	112(%rsp), %rdi	#, tmp283
	movq	%rsi, 32(%rsp)	# sz, %sfp
	movq	%rdx, 40(%rsp)	# get_key, %sfp
	xorl	%esi, %esi	#
	movl	$16384, %edx	#,
	call	memset	#
	leaq	48(%rsp), %rdx	#, tmp203
	xorl	%eax, %eax	# tmp205
	movl	$8, %ecx	#, tmp206
	testq	%r14, %r14	# sz
	movq	%rdx, %rdi	# tmp203, tmp204
	rep stosq
	je	.L292	#,
	xorl	%ebp, %ebp	# i
	.p2align 4,,10
	.p2align 3
.L293:
	movq	(%rbx,%rbp,8), %rdi	# MEM[base: in_18(D), index: i_63, step: 8, offset: 0B], MEM[base: in_18(D), index: i_63, step: 8, offset: 0B]
	movq	40(%rsp), %rax	# %sfp, get_key
	addq	$1, %rbp	#, i
	call	*%rax	# get_key
	movzbl	%al, %edx	# key, D.7934
	addq	$1, 112(%rsp,%rdx,8)	#, hist
	movzbl	%ah, %edx	# key, D.7934
	addq	$1, 2160(%rsp,%rdx,8)	#, hist
	movq	%rax, %rdx	# key, D.7934
	shrq	$16, %rdx	#, D.7934
	movzbl	%dl, %edx	# D.7934, D.7934
	addq	$1, 4208(%rsp,%rdx,8)	#, hist
	movq	%rax, %rdx	# key, D.7934
	shrq	$24, %rdx	#, D.7934
	movzbl	%dl, %edx	# D.7934, D.7934
	addq	$1, 6256(%rsp,%rdx,8)	#, hist
	movq	%rax, %rdx	# key, D.7934
	shrq	$32, %rdx	#, D.7934
	movzbl	%dl, %edx	# D.7934, D.7934
	addq	$1, 8304(%rsp,%rdx,8)	#, hist
	movq	%rax, %rdx	# key, D.7934
	shrq	$40, %rdx	#, D.7934
	movzbl	%dl, %edx	# D.7934, D.7934
	addq	$1, 10352(%rsp,%rdx,8)	#, hist
	movq	%rax, %rdx	# key, D.7934
	shrq	$56, %rax	#, D.7934
	shrq	$48, %rdx	#, D.7934
	movzbl	%dl, %edx	# D.7934, D.7934
	addq	$1, 12400(%rsp,%rdx,8)	#, hist
	addq	$1, 14448(%rsp,%rax,8)	#, hist
	cmpq	%rbp, 32(%rsp)	# i, %sfp
	jne	.L293	#,
.L292:
	leaq	112(%rsp), %rax	#, tmp293
	xorl	%r8d, %r8d	# D.7939
	xorl	%r9d, %r9d	# D.7939
	xorl	%ecx, %ecx	# D.7939
	xorl	%r10d, %r10d	# D.7939
	xorl	%esi, %esi	# D.7939
	leaq	2048(%rax), %rdx	#, D.7940
	xorl	%r11d, %r11d	# D.7939
	xorl	%edi, %edi	# D.7939
	movq	%rdx, 24(%rsp)	# D.7940, %sfp
	xorl	%edx, %edx	# D.7939
	.p2align 4,,10
	.p2align 3
.L294:
	movq	%rdi, %r15	# D.7939, total
	movq	%r11, %r14	# D.7939, total
	addq	(%rax), %r15	# MEM[base: _221, offset: 0B], total
	addq	2048(%rax), %r14	# MEM[base: _221, offset: 2048B], total
	movq	%rdi, (%rax)	# D.7939, MEM[base: _221, offset: 0B]
	movq	%rsi, %r13	# D.7939, total
	movq	%r11, 2048(%rax)	# D.7939, MEM[base: _221, offset: 2048B]
	addq	4096(%rax), %r13	# MEM[base: _221, offset: 4096B], total
	movq	%r10, %r12	# D.7939, total
	movq	%rsi, 4096(%rax)	# D.7939, MEM[base: _221, offset: 4096B]
	addq	6144(%rax), %r12	# MEM[base: _221, offset: 6144B], total
	movq	%rcx, %rbp	# D.7939, total
	movq	%r10, 6144(%rax)	# D.7939, MEM[base: _221, offset: 6144B]
	addq	8192(%rax), %rbp	# MEM[base: _221, offset: 8192B], total
	movq	%r9, %rsi	# D.7939, total
	movq	%rcx, 8192(%rax)	# D.7939, MEM[base: _221, offset: 8192B]
	addq	10240(%rax), %rsi	# MEM[base: _221, offset: 10240B], total
	movq	%rdx, %rdi	# D.7939, total
	movq	%r9, 10240(%rax)	# D.7939, MEM[base: _221, offset: 10240B]
	addq	12288(%rax), %rdi	# MEM[base: _221, offset: 12288B], total
	movq	%r8, %rcx	# D.7939, total
	movq	%rdx, 12288(%rax)	# D.7939, MEM[base: _221, offset: 12288B]
	addq	14336(%rax), %rcx	# MEM[base: _221, offset: 14336B], total
	addq	$8, %rax	#, ivtmp.348
	movq	%r8, 14328(%rax)	# D.7939, MEM[base: _221, offset: 14336B]
	cmpq	24(%rsp), %rax	# %sfp, ivtmp.348
	movq	%r14, %r11	# total, D.7939
	movq	%rsi, (%rsp)	# total, %sfp
	movq	%r12, %r10	# total, D.7939
	movq	%r13, %rsi	# total, D.7939
	movq	%rdi, 8(%rsp)	# total, %sfp
	movq	(%rsp), %r9	# %sfp, D.7939
	movq	%r15, %rdi	# total, D.7939
	movq	%rcx, 16(%rsp)	# total, %sfp
	movq	8(%rsp), %rdx	# %sfp, D.7939
	movq	%rbp, %rcx	# total, D.7939
	movq	16(%rsp), %r8	# %sfp, D.7939
	jne	.L294	#,
	movq	%r15, (%rsp)	# D.7939, %sfp
	movq	32(%rsp), %rax	# %sfp, sz
	movl	$1, %esi	#,
	movq	(%rsp), %xmm0	# %sfp, vect_cst_.327
	movq	%r14, (%rsp)	# D.7939, %sfp
	movhps	(%rsp), %xmm0	# %sfp, vect_cst_.327
	movq	%r13, (%rsp)	# D.7939, %sfp
	movaps	%xmm0, 48(%rsp)	# vect_cst_.327, MEM[(long unsigned int *)&sum]
	movq	(%rsp), %xmm0	# %sfp, vect_cst_.326
	movq	%r12, (%rsp)	# D.7939, %sfp
	movhps	(%rsp), %xmm0	# %sfp, vect_cst_.326
	movq	%rbp, (%rsp)	# D.7939, %sfp
	leaq	0(,%rax,8), %rbp	#, D.7934
	movaps	%xmm0, 64(%rsp)	# vect_cst_.326, MEM[(long unsigned int *)&sum + 16B]
	movq	%rbp, %rdi	# D.7934,
	movq	(%rsp), %xmm0	# %sfp, vect_cst_.325
	movq	%r9, (%rsp)	# D.7939, %sfp
	movhps	(%rsp), %xmm0	# %sfp, vect_cst_.325
	movq	%rdx, (%rsp)	# D.7939, %sfp
	movaps	%xmm0, 80(%rsp)	# vect_cst_.325, MEM[(long unsigned int *)&sum + 32B]
	movq	(%rsp), %xmm0	# %sfp, vect_cst_.324
	movq	%r8, (%rsp)	# D.7939, %sfp
	movhps	(%rsp), %xmm0	# %sfp, vect_cst_.324
	movaps	%xmm0, 96(%rsp)	# vect_cst_.324, MEM[(long unsigned int *)&sum + 48B]
	call	calloc	#
	testq	%rax, %rax	# in
	movq	%rax, (%rsp)	# in, %sfp
	jne	.L295	#,
	testq	%rbp, %rbp	# D.7934
	je	.L295	#,
	movq	stderr(%rip), %rcx	# stderr,
	movl	$.LC8, %edi	#,
	movl	$14, %edx	#,
	movl	$1, %esi	#,
	call	fwrite	#
	movl	$6, %edi	#,
	call	raise	#
.L295:
	movq	(%rsp), %rbp	# %sfp, in
	xorl	%r14d, %r14d	# j
	.p2align 4,,10
	.p2align 3
.L296:
	cmpq	$0, 32(%rsp)	#, %sfp
	je	.L299	#,
	movq	%r14, %r12	# j, tmp282
	leal	0(,%r14,8), %r13d	#, D.7936
	xorl	%r15d, %r15d	# i
	salq	$8, %r12	#, tmp282
	.p2align 4,,10
	.p2align 3
.L297:
	movq	(%rbx,%r15,8), %rdi	# MEM[base: in_105, index: i_124, step: 8, offset: 0B], MEM[base: in_105, index: i_124, step: 8, offset: 0B]
	movq	40(%rsp), %rax	# %sfp, get_key
	call	*%rax	# get_key
	movl	%r13d, %ecx	# D.7936, tmp319
	shrq	%cl, %rax	# tmp319, D.7934
	movzbl	%al, %eax	# D.7934, pos
	addq	%r12, %rax	# tmp282, tmp277
	movq	112(%rsp,%rax,8), %rcx	# hist, D.7934
	leaq	1(%rcx), %rsi	#, tmp280
	movq	%rsi, 112(%rsp,%rax,8)	# tmp280, hist
	movq	(%rbx,%r15,8), %rax	# MEM[base: in_105, index: i_124, step: 8, offset: 0B], D.7933
	addq	$1, %r15	#, i
	cmpq	%r15, 32(%rsp)	# i, %sfp
	movq	%rax, 0(%rbp,%rcx,8)	# D.7933, *_88
	jne	.L297	#,
.L299:
	addq	$1, %r14	#, j
	movq	%rbp, %rax	# in, in
	movq	%rbx, %rbp	# in, in
	cmpq	$8, %r14	#, j
	je	.L298	#,
	movq	%rax, %rbx	# in, in
	jmp	.L296	#
.L298:
	movq	(%rsp), %rdi	# %sfp,
	addq	$16504, %rsp	#,
	.cfi_def_cfa_offset 56
	popq	%rbx	#
	.cfi_def_cfa_offset 48
	popq	%rbp	#
	.cfi_def_cfa_offset 40
	popq	%r12	#
	.cfi_def_cfa_offset 32
	popq	%r13	#
	.cfi_def_cfa_offset 24
	popq	%r14	#
	.cfi_def_cfa_offset 16
	popq	%r15	#
	.cfi_def_cfa_offset 8
	jmp	free	#
	.cfi_endproc
.LFE77:
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
.LFB78:
	.cfi_startproc
	pushq	%rbp	#
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movl	$2097152, %edx	#,
	movq	%rsp, %rbp	#,
	.cfi_def_cfa_register 6
	pushq	%r15	#
	pushq	%r14	#
	pushq	%r13	#
	pushq	%r12	#
	.cfi_offset 15, -24
	.cfi_offset 14, -32
	.cfi_offset 13, -40
	.cfi_offset 12, -48
	movq	%rdi, %r14	# in, in
	pushq	%rbx	#
	subq	$40, %rsp	#,
	.cfi_offset 3, -56
	movq	%rsi, -56(%rbp)	# sz, %sfp
	xorl	%esi, %esi	#
	subq	$2097168, %rsp	#,
	leaq	15(%rsp), %rbx	#, tmp191
	andq	$-16, %rbx	#, tmp193
	movq	%rbx, %rdi	# tmp193,
	leaq	524288(%rbx), %r15	#, hist1
	leaq	1048576(%rbx), %r12	#, hist2
	call	memset	#
	movq	-56(%rbp), %rax	# %sfp, sz
	leaq	1572864(%rbx), %r13	#, hist3
	movq	%r14, %rcx	# in, ivtmp.398
	testq	%rax, %rax	# sz
	leaq	(%r14,%rax,8), %rsi	#, D.7988
	je	.L320	#,
	.p2align 4,,10
	.p2align 3
.L335:
	movq	(%rcx), %rax	# MEM[base: _75, offset: 0B], D.7990
	addq	$8, %rcx	#, ivtmp.398
	movzwl	%ax, %edx	# D.7990, D.7990
	addq	$1, (%rbx,%rdx,8)	#, *_32
	movq	%rax, %rdx	# D.7990, D.7990
	shrq	$13, %rdx	#, D.7990
	andl	$524280, %edx	#, D.7990
	addq	$1, (%r15,%rdx)	#, *_42
	movq	%rax, %rdx	# D.7990, D.7990
	shrq	$48, %rax	#, D.7990
	shrq	$29, %rdx	#, D.7990
	andl	$524280, %edx	#, D.7990
	addq	$1, (%r12,%rdx)	#, *_51
	addq	$1, 0(%r13,%rax,8)	#, *_57
	cmpq	%rcx, %rsi	# ivtmp.398, D.7988
	jne	.L335	#,
.L320:
	xorl	%eax, %eax	# i
	xorl	%edi, %edi	# sum3
	xorl	%esi, %esi	# sum2
	xorl	%ecx, %ecx	# sum1
	xorl	%edx, %edx	# sum0
	.p2align 4,,10
	.p2align 3
.L318:
	movq	%rdx, %r11	# sum0, sum0
	movq	%rcx, %r10	# sum1, sum1
	addq	(%rbx,%rax,8), %r11	# MEM[base: hist0_19, index: i_154, step: 8, offset: 0B], sum0
	addq	524288(%rbx,%rax,8), %r10	# MEM[base: hist0_19, index: i_154, step: 8, offset: 524288B], sum1
	movq	%rdx, (%rbx,%rax,8)	# sum0, MEM[base: hist0_19, index: i_154, step: 8, offset: 0B]
	movq	%rsi, %r9	# sum2, sum2
	movq	%rcx, 524288(%rbx,%rax,8)	# sum1, MEM[base: hist0_19, index: i_154, step: 8, offset: 524288B]
	movq	%rdi, %r8	# sum3, sum3
	addq	(%r12,%rax,8), %r9	# MEM[base: hist2_22, index: i_154, step: 8, offset: 0B], sum2
	movq	%rsi, (%r12,%rax,8)	# sum2, MEM[base: hist2_22, index: i_154, step: 8, offset: 0B]
	addq	0(%r13,%rax,8), %r8	# MEM[base: hist3_23, index: i_154, step: 8, offset: 0B], sum3
	movq	%rdi, 0(%r13,%rax,8)	# sum3, MEM[base: hist3_23, index: i_154, step: 8, offset: 0B]
	addq	$1, %rax	#, i
	movq	%r11, %rdx	# sum0, sum0
	cmpq	$65536, %rax	#, i
	movq	%r10, %rcx	# sum1, sum1
	movq	%r9, %rsi	# sum2, sum2
	movq	%r8, %rdi	# sum3, sum3
	jne	.L318	#,
	movq	-56(%rbp), %rax	# %sfp, sz
	movl	$1, %esi	#,
	leaq	0(,%rax,8), %r9	#, D.7990
	movq	%r9, %rdi	# D.7990,
	movq	%r9, -64(%rbp)	# D.7990, %sfp
	call	calloc	#
	testq	%rax, %rax	# a
	movq	%rax, %r8	#, a
	movq	-64(%rbp), %r9	# %sfp, D.7990
	jne	.L321	#,
	testq	%r9, %r9	# D.7990
	je	.L321	#,
	movq	stderr(%rip), %rcx	# stderr,
	movl	$14, %edx	#,
	movl	$1, %esi	#,
	movl	$.LC8, %edi	#,
	movq	%rax, -72(%rbp)	# a, %sfp
	call	fwrite	#
	movl	$6, %edi	#,
	call	raise	#
	cmpq	$0, -56(%rbp)	#, %sfp
	movq	-64(%rbp), %r9	# %sfp, D.7990
	movq	-72(%rbp), %r8	# %sfp, a
	jne	.L328	#,
.L322:
	movq	%r8, %rdi	# a,
	call	free	#
	leaq	-40(%rbp), %rsp	#,
	popq	%rbx	#
	popq	%r12	#
	popq	%r13	#
	popq	%r14	#
	popq	%r15	#
	popq	%rbp	#
	.cfi_remember_state
	.cfi_def_cfa 7, 8
	ret
.L321:
	.cfi_restore_state
	cmpq	$0, -56(%rbp)	#, %sfp
	je	.L322	#,
.L328:
	leaq	(%r14,%r9), %r10	#, D.7988
	movq	%r14, %rax	# in, ivtmp.382
	.p2align 4,,10
	.p2align 3
.L323:
	movq	(%rax), %rcx	# MEM[base: _99, offset: 0B], D.7990
	addq	$8, %rax	#, ivtmp.382
	cmpq	%r10, %rax	# D.7988, ivtmp.382
	movzwl	%cx, %edx	# D.7990, pos
	leaq	(%rbx,%rdx,8), %rsi	#, D.7989
	movq	(%rsi), %rdx	# *_88, D.7990
	leaq	1(%rdx), %rdi	#, tmp220
	movq	%rcx, (%r8,%rdx,8)	# D.7990, *_93
	movq	%rdi, (%rsi)	# tmp220, *_88
	jne	.L323	#,
	movq	%r8, %rdx	# a, ivtmp.377
	addq	%r8, %r9	# a, D.7988
	.p2align 4,,10
	.p2align 3
.L324:
	movq	(%rdx), %rsi	# MEM[base: _130, offset: 0B], D.7990
	addq	$8, %rdx	#, ivtmp.377
	movq	%rsi, %rax	# D.7990, pos
	shrq	$13, %rax	#, pos
	andl	$524280, %eax	#, D.7990
	addq	%r15, %rax	# hist1, D.7989
	cmpq	%rdx, %r9	# ivtmp.377, D.7988
	movq	(%rax), %rcx	# *_104, D.7990
	leaq	1(%rcx), %rdi	#, tmp224
	movq	%rsi, (%r14,%rcx,8)	# D.7990, *_109
	movq	%rdi, (%rax)	# tmp224, *_104
	jne	.L324	#,
	xorl	%edx, %edx	# i
	.p2align 4,,10
	.p2align 3
.L326:
	movq	(%r14,%rdx,8), %rsi	# MEM[base: in_27(D), index: i_46, step: 8, offset: 0B], D.7990
	addq	$1, %rdx	#, i
	movq	%rsi, %rax	# D.7990, pos
	shrq	$29, %rax	#, pos
	andl	$524280, %eax	#, D.7990
	addq	%r12, %rax	# hist2, D.7989
	cmpq	%rdx, -56(%rbp)	# i, %sfp
	movq	(%rax), %rcx	# *_120, D.7990
	leaq	1(%rcx), %rdi	#, tmp228
	movq	%rsi, (%r8,%rcx,8)	# D.7990, *_125
	movq	%rdi, (%rax)	# tmp228, *_120
	ja	.L326	#,
	xorl	%eax, %eax	# i
	.p2align 4,,10
	.p2align 3
.L327:
	movq	(%r8,%rax,8), %rcx	# MEM[base: a_143, index: i_37, step: 8, offset: 0B], D.7990
	addq	$1, %rax	#, i
	movq	%rcx, %rdx	# D.7990, pos
	shrq	$48, %rdx	#, pos
	cmpq	%rax, -56(%rbp)	# i, %sfp
	leaq	0(%r13,%rdx,8), %rsi	#, D.7989
	movq	(%rsi), %rdx	# *_134, D.7990
	leaq	1(%rdx), %rdi	#, tmp231
	movq	%rcx, (%r14,%rdx,8)	# D.7990, *_139
	movq	%rdi, (%rsi)	# tmp231, *_134
	ja	.L327	#,
	jmp	.L322	#
	.cfi_endproc
.LFE78:
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
.LFB79:
	.cfi_startproc
	jmp	radix_sort_u16_u64	#
	.cfi_endproc
.LFE79:
	.size	radix_sort_u64, .-radix_sort_u64
	.section	.text.unlikely
.LCOLDE16:
	.text
.LHOTE16:
	.ident	"GCC: (GNU) 5.3.0"
	.section	.note.GNU-stack,"",@progbits
