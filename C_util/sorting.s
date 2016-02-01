	.file	"sorting.c"
# GNU C11 (GCC) version 5.3.0 (x86_64-unknown-linux-gnu)
#	compiled by GNU C version 5.3.0, GMP version 6.1.0, MPFR version 3.1.3-p5, MPC version 1.0.3
# GGC heuristics: --param ggc-min-expand=100 --param ggc-min-heapsize=131072
# options passed:  -D NDEBUG sorting.c -mtune=generic -march=x86-64 -O2
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
# -fgcse-lm -fgnu-runtime -fgnu-unique -fguess-branch-probability
# -fhoist-adjacent-loads -fident -fif-conversion -fif-conversion2
# -findirect-inlining -finline -finline-atomics
# -finline-functions-called-once -finline-small-functions -fipa-cp
# -fipa-cp-alignment -fipa-icf -fipa-icf-functions -fipa-icf-variables
# -fipa-profile -fipa-pure-const -fipa-ra -fipa-reference -fipa-sra
# -fira-hoist-pressure -fira-share-save-slots -fira-share-spill-slots
# -fisolate-erroneous-paths-dereference -fivopts -fkeep-static-consts
# -fleading-underscore -flifetime-dse -flra-remat -flto-odr-type-merging
# -fmath-errno -fmerge-constants -fmerge-debug-strings
# -fmove-loop-invariants -fomit-frame-pointer -foptimize-sibling-calls
# -foptimize-strlen -fpartial-inlining -fpeephole -fpeephole2
# -fprefetch-loop-arrays -free -freg-struct-return -freorder-blocks
# -freorder-blocks-and-partition -freorder-functions -frerun-cse-after-loop
# -fsched-critical-path-heuristic -fsched-dep-count-heuristic
# -fsched-group-heuristic -fsched-interblock -fsched-last-insn-heuristic
# -fsched-rank-heuristic -fsched-spec -fsched-spec-insn-heuristic
# -fsched-stalled-insns-dep -fschedule-fusion -fschedule-insns2
# -fsemantic-interposition -fshow-column -fshrink-wrap -fsigned-zeros
# -fsplit-ivs-in-unroller -fsplit-wide-types -fssa-phiopt -fstdarg-opt
# -fstrict-aliasing -fstrict-overflow -fstrict-volatile-bitfields
# -fsync-libcalls -fthread-jumps -ftoplevel-reorder -ftrapping-math
# -ftree-bit-ccp -ftree-builtin-call-dce -ftree-ccp -ftree-ch
# -ftree-coalesce-vars -ftree-copy-prop -ftree-copyrename -ftree-cselim
# -ftree-dce -ftree-dominator-opts -ftree-dse -ftree-forwprop -ftree-fre
# -ftree-loop-if-convert -ftree-loop-im -ftree-loop-ivcanon
# -ftree-loop-optimize -ftree-parallelize-loops= -ftree-phiprop -ftree-pre
# -ftree-pta -ftree-reassoc -ftree-scev-cprop -ftree-sink -ftree-slsr
# -ftree-sra -ftree-switch-conversion -ftree-tail-merge -ftree-ter
# -ftree-vrp -funit-at-a-time -funwind-tables -fverbose-asm
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
	.type	sift_down, @function
sift_down:
.LFB72:
	.cfi_startproc
	leaq	1(%rsi,%rsi), %rcx	#, D.7446
	cmpq	%rcx, %rdx	# D.7446, len
	jbe	.L1	#,
	movq	(%rdi,%rsi,8), %r9	# *_52, D.7448
	jmp	.L7	#
	.p2align 4,,10
	.p2align 3
.L11:
	leaq	(%rdi,%rax,8), %rcx	#, D.7447
	movq	(%rcx), %rsi	# *_31, D.7446
	movq	%rsi, (%r10)	# D.7446, *_20
	movq	%r9, (%rcx)	# D.7448, *_31
	leaq	1(%rax,%rax), %rcx	#, D.7446
	cmpq	%rdx, %rcx	# len, D.7446
	jnb	.L1	#,
	movq	%rax, %rsi	# root, root
.L7:
	cmpq	%r9, (%rdi,%rcx,8)	# D.7448, *_17
	leaq	2(%rsi,%rsi), %r8	#, D.7446
	movq	%rcx, %rax	# D.7446, swap
	leaq	(%rdi,%rsi,8), %r10	#, D.7447
	cmovbe	%rsi, %rax	# swap,, root, swap
	cmpq	%r8, %rdx	# D.7446, len
	jbe	.L5	#,
	movq	(%rdi,%rax,8), %rcx	# *_27, tmp123
	cmpq	%rcx, (%rdi,%r8,8)	# tmp123, *_23
	cmova	%r8, %rax	# swap,, D.7446, root
.L5:
	cmpq	%rax, %rsi	# root, root
	jne	.L11	#,
.L1:
	rep ret
	.cfi_endproc
.LFE72:
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
	subq	$8, %rsp	#,
	.cfi_def_cfa_offset 16
	movq	stderr(%rip), %rcx	# stderr,
	movl	$.LC1, %edi	#,
	movl	$14, %edx	#,
	movl	$1, %esi	#,
	call	fwrite	#
	movl	$6, %edi	#,
	addq	$8, %rsp	#,
	.cfi_def_cfa_offset 8
	jmp	raise	#
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
	.globl	cmp_gt
	.type	cmp_gt, @function
cmp_gt:
.LFB58:
	.cfi_startproc
	xorl	%eax, %eax	# D.7457
	cmpq	%rsi, %rdi	# y, x
	seta	%al	#, D.7457
	ret
	.cfi_endproc
.LFE58:
	.size	cmp_gt, .-cmp_gt
	.section	.text.unlikely
.LCOLDE3:
	.text
.LHOTE3:
	.section	.text.unlikely
.LCOLDB4:
	.text
.LHOTB4:
	.p2align 4,,15
	.globl	cmp_lt
	.type	cmp_lt, @function
cmp_lt:
.LFB59:
	.cfi_startproc
	xorl	%eax, %eax	# D.7462
	cmpq	%rsi, %rdi	# y, x
	setb	%al	#, D.7462
	ret
	.cfi_endproc
.LFE59:
	.size	cmp_lt, .-cmp_lt
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
.LFB60:
	.cfi_startproc
	cmpq	$1, %rsi	#, len
	jbe	.L30	#,
	pushq	%r15	#
	.cfi_def_cfa_offset 16
	.cfi_offset 15, -16
	pushq	%r14	#
	.cfi_def_cfa_offset 24
	.cfi_offset 14, -24
	movl	$1, %eax	#, D.7476
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
	jne	.L25	#,
	jmp	.L33	#
	.p2align 4,,10
	.p2align 3
.L23:
	movq	(%r14), %rax	# *_16, D.7478
	movq	%rax, (%r15,%rbx,8)	# D.7478, *_23
	movl	%r13d, %ebx	# j,
	testl	%ebx, %ebx	# j
	je	.L34	#,
.L25:
	leal	-1(%rbx), %eax	#,
	movq	%rbp, %rsi	# temp,
	leaq	(%r15,%rax,8), %r14	#, D.7477
	movq	%rax, %r13	#,
	movq	(%r14), %rdi	# *_16,
	call	*%r12	# cmp
	testl	%eax, %eax	# D.7479
	je	.L23	#,
	movl	4(%rsp), %eax	# %sfp, D.7476
	salq	$3, %rbx	#, D.7476
	cmpq	8(%rsp), %rax	# %sfp, D.7476
	movq	%rbp, (%r15,%rbx)	# temp, *_28
	jnb	.L35	#,
.L24:
	movl	(%rsp), %edx	# %sfp, j
	movq	(%r15,%rax,8), %rbp	# *_11, temp
	addl	$1, 4(%rsp)	#, %sfp
	leal	1(%rdx), %ecx	#, j
	movl	%ecx, (%rsp)	# j, %sfp
	movl	(%rsp), %eax	# %sfp, j
	testl	%eax, %eax	# j
	movl	%eax, %ebx	# j,
	jne	.L25	#,
.L33:
	movq	%rbp, (%r15)	# temp, *input_10(D)
	movl	4(%rsp), %eax	# %sfp, D.7476
	jmp	.L24	#
	.p2align 4,,10
	.p2align 3
.L34:
	movl	4(%rsp), %eax	# %sfp, D.7476
	xorl	%ebx, %ebx	# D.7476
	cmpq	8(%rsp), %rax	# %sfp, D.7476
	movq	%rbp, (%r15,%rbx)	# temp, *_28
	jb	.L24	#,
.L35:
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
.L30:
	rep ret
	.cfi_endproc
.LFE60:
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
	movq	%rdx, %r13	# len, len
	pushq	%rbp	#
	.cfi_def_cfa_offset 48
	.cfi_offset 6, -48
	pushq	%rbx	#
	.cfi_def_cfa_offset 56
	.cfi_offset 3, -56
	subq	$40, %rsp	#,
	.cfi_def_cfa_offset 96
	cmpq	$4, %rdx	#, len
	jbe	.L56	#,
	movq	%r13, %rbx	# len, mid
	movq	%rdi, %r14	# arr, arr
	movq	%rdx, %rbp	# len, len
	shrq	%rbx	# mid
	movq	%rsi, %r15	# tmp, tmp
	movq	%rcx, %r12	# cmp, cmp
	leaq	(%r14,%rbx,8), %r13	#, B
	movq	%rdx, 24(%rsp)	# len, %sfp
	subq	%rbx, %rbp	# mid, n2
	movq	%rbx, %rdx	# mid,
	movq	%rsi, 16(%rsp)	# tmp, %sfp
	movq	%rdi, 8(%rsp)	# arr, %sfp
	call	mergesort_internal	#
	movq	%r15, %rsi	# tmp,
	movq	%r12, %rcx	# cmp,
	movq	%rbp, %rdx	# n2,
	movq	%r13, %rdi	# B,
	leaq	8(%r15), %r15	#, ivtmp.79
	call	mergesort_internal	#
	jmp	.L38	#
	.p2align 4,,10
	.p2align 3
.L57:
	movq	(%r14), %rax	# *arr_19, D.7495
	subq	$1, %rbx	#, mid
	addq	$8, %r14	#, arr
	movq	%rax, -8(%r15)	# D.7495, MEM[base: tmp_23, offset: -8B]
.L40:
	addq	$8, %r15	#, ivtmp.79
.L38:
	testq	%rbx, %rbx	# mid
	leaq	-8(%r15), %r9	#, D.7498
	je	.L45	#,
	testq	%rbp, %rbp	# n2
	je	.L45	#,
	movq	0(%r13), %rsi	# *B_17,
	movq	(%r14), %rdi	# *arr_19,
	call	*%r12	# cmp
	testl	%eax, %eax	# D.7496
	jne	.L57	#,
	movq	0(%r13), %rax	# *B_17, D.7495
	subq	$1, %rbp	#, n2
	addq	$8, %r13	#, B
	movq	%rax, -8(%r15)	# D.7495, MEM[base: tmp_28, offset: -8B]
	jmp	.L40	#
	.p2align 4,,10
	.p2align 3
.L45:
	testq	%rbx, %rbx	# mid
	jne	.L58	#,
	testq	%rbp, %rbp	# n2
	jne	.L59	#,
.L44:
	movq	24(%rsp), %rdx	# %sfp, D.7494
	movq	16(%rsp), %rsi	# %sfp,
	movq	8(%rsp), %rdi	# %sfp,
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
	salq	$3, %rdx	#, D.7494
	jmp	memcpy	#
	.p2align 4,,10
	.p2align 3
.L59:
	.cfi_restore_state
	leaq	0(,%rbp,8), %rdx	#, D.7494
	movq	%r13, %rsi	# B,
	movq	%r9, %rdi	# D.7498,
	call	memcpy	#
	jmp	.L44	#
	.p2align 4,,10
	.p2align 3
.L56:
	addq	$40, %rsp	#,
	.cfi_remember_state
	.cfi_def_cfa_offset 56
	movq	%r13, %rsi	# len,
	movq	%rcx, %rdx	# cmp,
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
	jmp	insertion_sort_generic	#
	.p2align 4,,10
	.p2align 3
.L58:
	.cfi_restore_state
	leaq	0(,%rbx,8), %rdx	#, D.7494
	movq	%r9, %rdi	# D.7498,
	movq	%r14, %rsi	# arr,
	call	memcpy	#
	testq	%rbp, %rbp	# n2
	movq	%rax, %r9	#, D.7498
	je	.L44	#,
	jmp	.L59	#
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
	.globl	insertion_sort_u64
	.type	insertion_sort_u64, @function
insertion_sort_u64:
.LFB61:
	.cfi_startproc
	cmpq	$1, %rsi	#, len
	jbe	.L60	#,
	movl	$1, %r9d	#, ivtmp.90
	movl	$1, %eax	#, D.7522
	movl	$2, %r10d	#, ivtmp.91
	testl	%r9d, %r9d	# ivtmp.90
	movq	(%rdi,%rax,8), %r8	# *_10, temp
	je	.L63	#,
	.p2align 4,,10
	.p2align 3
.L75:
	leal	-1(%r9), %edx	#,
	movq	%rdx, %rax	#,
	movq	(%rdi,%rdx,8), %rdx	# *_35, D.7522
	cmpq	%rdx, %r8	# D.7522, temp
	jb	.L70	#,
	jmp	.L71	#
	.p2align 4,,10
	.p2align 3
.L73:
	leal	-1(%rax), %edx	#,
	movq	%rdx, %rcx	#,
	movq	(%rdi,%rdx,8), %rdx	# *_15, D.7522
	cmpq	%rdx, %r8	# D.7522, temp
	jnb	.L72	#,
	movl	%ecx, %eax	# j,
.L70:
	leal	1(%rax), %ecx	#, D.7522
	testl	%eax, %eax	# j
	movq	%rdx, (%rdi,%rcx,8)	# D.7522, *_19
	jne	.L73	#,
	xorl	%eax, %eax	# D.7522
.L65:
	movq	%r8, (%rdi,%rax)	# temp, *_23
	movl	%r10d, %eax	# ivtmp.91, D.7522
	cmpq	%rsi, %rax	# len, D.7522
	jnb	.L74	#,
.L67:
	addl	$1, %r9d	#, ivtmp.90
	addl	$1, %r10d	#, ivtmp.91
	movq	(%rdi,%rax,8), %r8	# *_10, temp
	testl	%r9d, %r9d	# ivtmp.90
	jne	.L75	#,
.L63:
	movq	%r8, (%rdi)	# temp, *input_9(D)
	movl	%r10d, %eax	# ivtmp.91, D.7522
	jmp	.L67	#
	.p2align 4,,10
	.p2align 3
.L72:
	salq	$3, %rax	#, D.7522
	movq	%r8, (%rdi,%rax)	# temp, *_23
	movl	%r10d, %eax	# ivtmp.91, D.7522
	cmpq	%rsi, %rax	# len, D.7522
	jb	.L67	#,
.L74:
	rep ret
.L60:
	rep ret
.L71:
	movl	%r9d, %eax	# ivtmp.90, D.7522
	salq	$3, %rax	#, D.7522
	jmp	.L65	#
	.cfi_endproc
.LFE61:
	.size	insertion_sort_u64, .-insertion_sort_u64
	.section	.text.unlikely
.LCOLDE7:
	.text
.LHOTE7:
	.section	.text.unlikely
.LCOLDB8:
	.text
.LHOTB8:
	.p2align 4,,15
	.type	mergesort_internal_u64, @function
mergesort_internal_u64:
.LFB70:
	.cfi_startproc
	cmpq	$4, %rdx	#, len
	jbe	.L96	#,
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
	movq	%rdi, %r12	# arr, arr
	pushq	%rbp	#
	.cfi_def_cfa_offset 48
	.cfi_offset 6, -48
	pushq	%rbx	#
	.cfi_def_cfa_offset 56
	.cfi_offset 3, -56
	movq	%rdx, %rbx	# len, mid
	shrq	%rbx	# mid
	movq	%rdx, %rbp	# len, len
	movq	%rsi, %r13	# tmp, tmp
	movq	%rbp, %r14	# len, n2
	leaq	(%r12,%rbx,8), %r15	#, B
	subq	$8, %rsp	#,
	.cfi_def_cfa_offset 64
	movq	%rbx, %rdx	# mid,
	subq	%rbx, %r14	# mid, n2
	call	mergesort_internal_u64	#
	movq	%r13, %rsi	# tmp,
	movq	%r14, %rdx	# n2,
	movq	%r15, %rdi	# B,
	call	mergesort_internal_u64	#
	leaq	8(%r13), %rax	#, ivtmp.98
	movq	%r12, %rsi	# arr, arr
	jmp	.L78	#
	.p2align 4,,10
	.p2align 3
.L97:
	addq	$8, %rsi	#, arr
	movq	%rdi, -8(%rax)	# D.7536, MEM[base: tmp_21, offset: -8B]
	subq	$1, %rbx	#, mid
.L80:
	addq	$8, %rax	#, ivtmp.98
.L78:
	testq	%rbx, %rbx	# mid
	leaq	-8(%rax), %rcx	#, D.7538
	je	.L85	#,
	testq	%r14, %r14	# n2
	je	.L85	#,
	movq	(%rsi), %rdi	# *arr_16, D.7536
	movq	(%r15), %rcx	# *B_18, D.7536
	cmpq	%rcx, %rdi	# D.7536, D.7536
	jb	.L97	#,
	addq	$8, %r15	#, B
	movq	%rcx, -8(%rax)	# D.7536, MEM[base: tmp_25, offset: -8B]
	subq	$1, %r14	#, n2
	jmp	.L80	#
	.p2align 4,,10
	.p2align 3
.L85:
	testq	%rbx, %rbx	# mid
	jne	.L98	#,
	testq	%r14, %r14	# n2
	jne	.L99	#,
.L84:
	addq	$8, %rsp	#,
	.cfi_remember_state
	.cfi_def_cfa_offset 56
	leaq	0(,%rbp,8), %rdx	#, D.7536
	movq	%r13, %rsi	# tmp,
	popq	%rbx	#
	.cfi_restore 3
	.cfi_def_cfa_offset 48
	movq	%r12, %rdi	# arr,
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
	.p2align 4,,10
	.p2align 3
.L99:
	.cfi_restore_state
	leaq	0(,%r14,8), %rdx	#, D.7536
	movq	%r15, %rsi	# B,
	movq	%rcx, %rdi	# D.7538,
	call	memcpy	#
	jmp	.L84	#
	.p2align 4,,10
	.p2align 3
.L96:
	.cfi_def_cfa_offset 8
	.cfi_restore 3
	.cfi_restore 6
	.cfi_restore 12
	.cfi_restore 13
	.cfi_restore 14
	.cfi_restore 15
	movq	%rdx, %rsi	# len,
	jmp	insertion_sort_u64	#
	.p2align 4,,10
	.p2align 3
.L98:
	.cfi_def_cfa_offset 64
	.cfi_offset 3, -56
	.cfi_offset 6, -48
	.cfi_offset 12, -40
	.cfi_offset 13, -32
	.cfi_offset 14, -24
	.cfi_offset 15, -16
	leaq	0(,%rbx,8), %rdx	#, D.7536
	movq	%rcx, %rdi	# D.7538,
	call	memcpy	#
	testq	%r14, %r14	# n2
	movq	%rax, %rcx	#, D.7538
	je	.L84	#,
	jmp	.L99	#
	.cfi_endproc
.LFE70:
	.size	mergesort_internal_u64, .-mergesort_internal_u64
	.section	.text.unlikely
.LCOLDE8:
	.text
.LHOTE8:
	.section	.text.unlikely
.LCOLDB9:
	.text
.LHOTB9:
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
	jbe	.L106	#,
.L109:
	movq	16(%rsp), %rax	# %sfp, arr
	movq	24(%rsp), %rbp	# %sfp, j
	movq	$-1, 8(%rsp)	#, %sfp
	movq	(%rax), %r12	# *arr_52, pivot
	.p2align 4,,10
	.p2align 3
.L102:
	addq	$1, 8(%rsp)	#, %sfp
	movq	16(%rsp), %r14	# %sfp, arr
	movq	%r12, %rsi	# pivot,
	movq	8(%rsp), %rax	# %sfp, i
	movq	(%r14,%rax,8), %rdi	# MEM[base: arr_52, index: _43, step: 8, offset: 0B], MEM[base: arr_52, index: _43, step: 8, offset: 0B]
	call	*%rbx	# cmp
	testl	%eax, %eax	# D.7577
	jne	.L102	#,
	leaq	-1(%rbp), %r15	#, j
	leaq	(%r14,%r15,8), %r14	#, ivtmp.106
	jmp	.L104	#
	.p2align 4,,10
	.p2align 3
.L112:
	subq	$1, %r15	#, j
.L104:
	movq	(%r14), %rsi	# MEM[base: _45, offset: 0B],
	movq	%r14, %r13	# ivtmp.106, D.7578
	movq	%r12, %rdi	# pivot,
	subq	$8, %r14	#, ivtmp.106
	movq	%r15, %rbp	# j, j
	call	*%rbx	# cmp
	testl	%eax, %eax	# D.7577
	jne	.L112	#,
	movq	8(%rsp), %rcx	# %sfp, i
	cmpq	%r15, %rcx	# j, i
	jge	.L105	#,
	movq	16(%rsp), %rsi	# %sfp, arr
	movq	0(%r13), %rdx	# *_45, D.7576
	movq	(%rsi,%rcx,8), %rax	# MEM[base: arr_52, index: _43, step: 8, offset: 0B], __temp
	movq	%rdx, (%rsi,%rcx,8)	# D.7576, MEM[base: arr_52, index: _43, step: 8, offset: 0B]
	movq	%rax, 0(%r13)	# __temp, *_45
	jmp	.L102	#
.L105:
	movslq	%r15d, %rbp	# j, pivot_idx
	movq	16(%rsp), %r15	# %sfp, arr
	movq	%rbx, %rdx	# cmp,
	leaq	1(%rbp), %r12	#, D.7574
	movq	%r15, %rdi	# arr,
	movq	%r12, %rsi	# D.7574,
	call	qsort_generic	#
	movq	%rbp, %rcx	# pivot_idx, D.7573
	leaq	(%r15,%r12,8), %rdx	#, arr
	notq	%rcx	# D.7573
	addq	%rcx, 24(%rsp)	# D.7573, %sfp
	movq	24(%rsp), %rax	# %sfp, len
	movq	%rdx, 16(%rsp)	# arr, %sfp
	cmpq	$4, %rax	#, len
	ja	.L109	#,
.L106:
	movq	24(%rsp), %rsi	# %sfp,
	movq	16(%rsp), %rdi	# %sfp,
	addq	$40, %rsp	#,
	.cfi_def_cfa_offset 56
	movq	%rbx, %rdx	# cmp,
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
	jmp	insertion_sort_generic	#
	.cfi_endproc
.LFE63:
	.size	qsort_generic, .-qsort_generic
	.section	.text.unlikely
.LCOLDE9:
	.text
.LHOTE9:
	.section	.text.unlikely
.LCOLDB10:
	.text
.LHOTB10:
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
	jbe	.L119	#,
.L122:
	movq	0(%rbp), %rdi	# *arr_51, pivot
	movq	%r12, %rsi	# len, j
	movq	$-1, %r9	#, i
	.p2align 4,,10
	.p2align 3
.L115:
	addq	$1, %r9	#, i
	movq	0(%rbp,%r9,8), %r10	# MEM[base: arr_51, index: _39, step: 8, offset: 0B], D.7601
	cmpq	%r10, %rdi	# D.7601, pivot
	ja	.L115	#,
	leaq	-1(%rsi), %rdx	#, j
	leaq	0(%rbp,%rdx,8), %rax	#, ivtmp.121
	jmp	.L117	#
	.p2align 4,,10
	.p2align 3
.L125:
	subq	$1, %rdx	#, j
.L117:
	movq	%rax, %r8	# ivtmp.121, D.7604
	subq	$8, %rax	#, ivtmp.121
	movq	8(%rax), %rcx	# MEM[base: _109, offset: 8B], D.7601
	movq	%rdx, %rsi	# j, j
	cmpq	%rcx, %rdi	# D.7601, pivot
	jb	.L125	#,
	cmpq	%rdx, %r9	# j, i
	jge	.L118	#,
	movq	%rcx, 0(%rbp,%r9,8)	# D.7601, MEM[base: arr_51, index: _39, step: 8, offset: 0B]
	movq	%r10, (%r8)	# D.7601, *_41
	jmp	.L115	#
.L118:
	movslq	%edx, %rbx	# j, pivot_idx
	movq	%rbp, %rdi	# arr,
	leaq	1(%rbx), %r13	#, D.7602
	notq	%rbx	# D.7601
	addq	%rbx, %r12	# D.7601, len
	movq	%r13, %rsi	# D.7602,
	leaq	0(%rbp,%r13,8), %rbp	#, arr
	call	qsort_u64	#
	cmpq	$4, %r12	#, len
	ja	.L122	#,
.L119:
	addq	$8, %rsp	#,
	.cfi_def_cfa_offset 40
	movq	%r12, %rsi	# len,
	movq	%rbp, %rdi	# arr,
	popq	%rbx	#
	.cfi_def_cfa_offset 32
	popq	%rbp	#
	.cfi_def_cfa_offset 24
	popq	%r12	#
	.cfi_def_cfa_offset 16
	popq	%r13	#
	.cfi_def_cfa_offset 8
	jmp	insertion_sort_u64	#
	.cfi_endproc
.LFE65:
	.size	qsort_u64, .-qsort_u64
	.section	.text.unlikely
.LCOLDE10:
	.text
.LHOTE10:
	.section	.text.unlikely
.LCOLDB11:
	.text
.LHOTB11:
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
	leaq	0(,%rsi,8), %r12	#, D.7614
	pushq	%rbp	#
	.cfi_def_cfa_offset 40
	.cfi_offset 6, -40
	pushq	%rbx	#
	.cfi_def_cfa_offset 48
	.cfi_offset 3, -48
	movq	%rsi, %rbp	# len, len
	movl	$1, %esi	#,
	movq	%r12, %rdi	# D.7614,
	movq	%rdx, %r14	# cmp, cmp
	call	calloc	#
	testq	%rax, %rax	# tmp
	movq	%rax, %rbx	#, tmp
	jne	.L127	#,
	testq	%r12, %r12	# D.7614
	je	.L127	#,
	call	oom_fun	#
.L127:
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
.LCOLDE11:
	.text
.LHOTE11:
	.section	.text.unlikely
.LCOLDB12:
	.text
.LHOTB12:
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
	leaq	0(,%rsi,8), %r12	#, D.7623
	pushq	%rbp	#
	.cfi_def_cfa_offset 32
	.cfi_offset 6, -32
	pushq	%rbx	#
	.cfi_def_cfa_offset 40
	.cfi_offset 3, -40
	movq	%rdi, %r13	# input, input
	movq	%rsi, %rbp	# len, len
	movq	%r12, %rdi	# D.7623,
	movl	$1, %esi	#,
	subq	$8, %rsp	#,
	.cfi_def_cfa_offset 48
	call	calloc	#
	testq	%rax, %rax	# tmp
	movq	%rax, %rbx	#, tmp
	jne	.L136	#,
	testq	%r12, %r12	# D.7623
	je	.L136	#,
	call	oom_fun	#
.L136:
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
.LCOLDE12:
	.text
.LHOTE12:
	.section	.text.unlikely
.LCOLDB13:
	.text
.LHOTB13:
	.p2align 4,,15
	.globl	heapsort_u64
	.type	heapsort_u64, @function
heapsort_u64:
.LFB74:
	.cfi_startproc
	pushq	%rbp	#
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	leaq	-2(%rsi), %rbp	#, D.7641
	pushq	%rbx	#
	.cfi_def_cfa_offset 24
	.cfi_offset 3, -24
	movq	%rsi, %r11	# len, len
	movq	%rdi, %rbx	# arr, arr
	shrq	%rbp	# start
	.p2align 4,,10
	.p2align 3
.L145:
	movq	%rbp, %rsi	# start,
	movq	%r11, %rdx	# len,
	movq	%rbx, %rdi	# arr,
	subq	$1, %rbp	#, start
	call	sift_down	#
	cmpq	$-1, %rbp	#, start
	jne	.L145	#,
	subq	$1, %r11	#, end
	.p2align 4,,10
	.p2align 3
.L146:
	movq	(%rbx,%r11,8), %rax	# MEM[base: arr_4(D), index: _9, step: 8, offset: 0B], __temp
	movq	(%rbx), %rdx	# *arr_4(D), D.7641
	xorl	%esi, %esi	#
	movq	%rbx, %rdi	# arr,
	movq	%rdx, (%rbx,%r11,8)	# D.7641, MEM[base: arr_4(D), index: _9, step: 8, offset: 0B]
	movq	%r11, %rdx	# end,
	movq	%rax, (%rbx)	# __temp, *arr_4(D)
	call	sift_down	#
	subq	$1, %r11	#, end
	testq	%r11, %r11	# end
	jg	.L146	#,
	popq	%rbx	#
	.cfi_def_cfa_offset 16
	popq	%rbp	#
	.cfi_def_cfa_offset 8
	ret
	.cfi_endproc
.LFE74:
	.size	heapsort_u64, .-heapsort_u64
	.section	.text.unlikely
.LCOLDE13:
	.text
.LHOTE13:
	.section	.text.unlikely
.LCOLDB14:
	.text
.LHOTB14:
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
	subq	$120, %rsp	#,
	.cfi_offset 15, -24
	.cfi_offset 14, -32
	.cfi_offset 13, -40
	.cfi_offset 12, -48
	.cfi_offset 3, -56
	movq	%rdi, -144(%rbp)	# in, %sfp
	movq	%rsi, -136(%rbp)	# sz, %sfp
	subq	$16400, %rsp	#,
	xorl	%esi, %esi	#
	leaq	15(%rsp), %rax	#, tmp270
	movq	%rax, %rbx	# tmp270, tmp272
	andq	$-16, %rbx	#, tmp272
	leaq	2048(%rbx), %rcx	#, hist1
	movq	%rbx, %rdi	# tmp272,
	leaq	4096(%rbx), %r12	#, hist2
	leaq	6144(%rbx), %r13	#, hist3
	leaq	8192(%rbx), %r14	#, hist4
	leaq	10240(%rbx), %r15	#, hist5
	movq	%rcx, -152(%rbp)	# hist1, %sfp
	leaq	12288(%rbx), %rcx	#, hist6
	movq	%rcx, -120(%rbp)	# hist6, %sfp
	leaq	14336(%rbx), %rcx	#, hist7
	movq	%rcx, -128(%rbp)	# hist7, %sfp
	call	memset	#
	movq	-136(%rbp), %rdi	# %sfp, sz
	movq	-144(%rbp), %rax	# %sfp, in
	testq	%rdi, %rdi	# sz
	leaq	(%rax,%rdi,8), %rcx	#, D.7713
	je	.L154	#,
	leaq	2048(%rbx), %rdi	#, hist1
	leaq	12288(%rbx), %r8	#, hist6
	leaq	14336(%rbx), %r9	#, hist7
	movq	%rax, %rdx	# in, ivtmp.209
	.p2align 4,,10
	.p2align 3
.L173:
	movq	(%rdx), %rax	# MEM[base: _139, offset: 0B], D.7711
	addq	$8, %rdx	#, ivtmp.209
	movzbl	%al, %esi	# D.7711, D.7711
	addq	$1, (%rbx,%rsi,8)	#, *_48
	movzbl	%ah, %esi	# D.7711, D.7711
	addq	$1, (%rdi,%rsi,8)	#, *_58
	movq	%rax, %rsi	# D.7711, D.7711
	shrq	$13, %rsi	#, D.7711
	andl	$2040, %esi	#, D.7711
	addq	$1, (%r12,%rsi)	#, *_68
	movq	%rax, %rsi	# D.7711, D.7711
	shrq	$21, %rsi	#, D.7711
	andl	$2040, %esi	#, D.7711
	addq	$1, 0(%r13,%rsi)	#, *_78
	movq	%rax, %rsi	# D.7711, D.7711
	shrq	$29, %rsi	#, D.7711
	andl	$2040, %esi	#, D.7711
	addq	$1, (%r14,%rsi)	#, *_85
	movq	%rax, %rsi	# D.7711, D.7711
	shrq	$37, %rsi	#, D.7711
	andl	$2040, %esi	#, D.7711
	addq	$1, (%r15,%rsi)	#, *_92
	movq	%rax, %rsi	# D.7711, D.7711
	shrq	$56, %rax	#, D.7711
	shrq	$45, %rsi	#, D.7711
	andl	$2040, %esi	#, D.7711
	addq	$1, (%r8,%rsi)	#, *_99
	addq	$1, (%r9,%rax,8)	#, *_105
	cmpq	%rdx, %rcx	# ivtmp.209, D.7713
	jne	.L173	#,
.L154:
	xorl	%eax, %eax	# i
	xorl	%r11d, %r11d	# sum7
	xorl	%r10d, %r10d	# sum6
	xorl	%r9d, %r9d	# sum5
	xorl	%r8d, %r8d	# sum4
	xorl	%edi, %edi	# sum3
	xorl	%esi, %esi	# sum2
	xorl	%ecx, %ecx	# sum1
	xorl	%edx, %edx	# sum0
	movq	%r12, -112(%rbp)	# hist2, %sfp
	.p2align 4,,10
	.p2align 3
.L152:
	movq	%rdx, %r12	# sum0, sum0
	addq	(%rbx,%rax,8), %r12	# MEM[base: hist0_31, index: i_314, step: 8, offset: 0B], sum0
	movq	%rdx, (%rbx,%rax,8)	# sum0, MEM[base: hist0_31, index: i_314, step: 8, offset: 0B]
	movq	%rcx, %rdx	# sum1, sum1
	addq	2048(%rbx,%rax,8), %rdx	# MEM[base: hist0_31, index: i_314, step: 8, offset: 2048B], sum1
	movq	%rcx, 2048(%rbx,%rax,8)	# sum1, MEM[base: hist0_31, index: i_314, step: 8, offset: 2048B]
	movq	%rsi, %rcx	# sum2, sum2
	movq	%rdx, -56(%rbp)	# sum1, %sfp
	movq	-112(%rbp), %rdx	# %sfp, hist2
	addq	(%rdx,%rax,8), %rcx	# MEM[base: hist2_34, index: i_314, step: 8, offset: 0B], sum2
	movq	%rsi, (%rdx,%rax,8)	# sum2, MEM[base: hist2_34, index: i_314, step: 8, offset: 0B]
	movq	%rdi, %rsi	# sum3, sum3
	movq	%r9, %rdx	# sum5, sum5
	addq	0(%r13,%rax,8), %rsi	# MEM[base: hist3_35, index: i_314, step: 8, offset: 0B], sum3
	movq	%rdi, 0(%r13,%rax,8)	# sum3, MEM[base: hist3_35, index: i_314, step: 8, offset: 0B]
	movq	%r8, %rdi	# sum4, sum4
	addq	(%r14,%rax,8), %rdi	# MEM[base: hist4_36, index: i_314, step: 8, offset: 0B], sum4
	movq	%r8, (%r14,%rax,8)	# sum4, MEM[base: hist4_36, index: i_314, step: 8, offset: 0B]
	addq	(%r15,%rax,8), %rdx	# MEM[base: hist5_37, index: i_314, step: 8, offset: 0B], sum5
	movq	%r9, (%r15,%rax,8)	# sum5, MEM[base: hist5_37, index: i_314, step: 8, offset: 0B]
	movq	%r10, %r8	# sum6, sum6
	movq	-120(%rbp), %r9	# %sfp, hist6
	movq	%rcx, -64(%rbp)	# sum2, %sfp
	movq	%rsi, -72(%rbp)	# sum3, %sfp
	movq	-56(%rbp), %rcx	# %sfp, sum1
	movq	%rdi, -80(%rbp)	# sum4, %sfp
	movq	-64(%rbp), %rsi	# %sfp, sum2
	addq	(%r9,%rax,8), %r8	# MEM[base: hist6_38, index: i_314, step: 8, offset: 0B], sum6
	movq	%r10, (%r9,%rax,8)	# sum6, MEM[base: hist6_38, index: i_314, step: 8, offset: 0B]
	movq	%r11, %r9	# sum7, sum7
	movq	-128(%rbp), %r10	# %sfp, hist7
	movq	%rdx, -88(%rbp)	# sum5, %sfp
	movq	%r12, %rdx	# sum0, sum0
	movq	-72(%rbp), %rdi	# %sfp, sum3
	addq	(%r10,%rax,8), %r9	# MEM[base: hist7_39, index: i_314, step: 8, offset: 0B], sum7
	movq	%r11, (%r10,%rax,8)	# sum7, MEM[base: hist7_39, index: i_314, step: 8, offset: 0B]
	addq	$1, %rax	#, i
	cmpq	$256, %rax	#, i
	movq	%r8, -96(%rbp)	# sum6, %sfp
	movq	-80(%rbp), %r8	# %sfp, sum4
	movq	-96(%rbp), %r10	# %sfp, sum6
	movq	%r9, -104(%rbp)	# sum7, %sfp
	movq	-88(%rbp), %r9	# %sfp, sum5
	movq	-104(%rbp), %r11	# %sfp, sum7
	jne	.L152	#,
	movq	-136(%rbp), %rax	# %sfp, sz
	movl	$1, %esi	#,
	movq	-112(%rbp), %r12	# %sfp, hist2
	leaq	0(,%rax,8), %r10	#, D.7711
	movq	%r10, %rdi	# D.7711,
	movq	%r10, -56(%rbp)	# D.7711, %sfp
	call	calloc	#
	testq	%rax, %rax	# b
	movq	-56(%rbp), %r10	# %sfp, D.7711
	jne	.L155	#,
	testq	%r10, %r10	# D.7711
	je	.L155	#,
	movq	%rax, -64(%rbp)	# b, %sfp
	call	oom_fun	#
	cmpq	$0, -136(%rbp)	#, %sfp
	movq	-56(%rbp), %r10	# %sfp, D.7711
	movq	-64(%rbp), %rax	# %sfp, b
	jne	.L166	#,
.L156:
	movq	%rax, %rdi	# b,
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
.L155:
	.cfi_restore_state
	cmpq	$0, -136(%rbp)	#, %sfp
	je	.L156	#,
.L166:
	movq	-144(%rbp), %rdi	# %sfp, in
	leaq	(%rdi,%r10), %r9	#, D.7713
	movq	%rdi, %rdx	# in, ivtmp.189
	.p2align 4,,10
	.p2align 3
.L157:
	movq	(%rdx), %rsi	# MEM[base: _163, offset: 0B], D.7711
	addq	$8, %rdx	#, ivtmp.189
	cmpq	%r9, %rdx	# D.7713, ivtmp.189
	movzbl	%sil, %ecx	# D.7711, pos
	leaq	(%rbx,%rcx,8), %rdi	#, D.7712
	movq	(%rdi), %rcx	# *_152, D.7711
	leaq	1(%rcx), %r8	#, tmp315
	movq	%rsi, (%rax,%rcx,8)	# D.7711, *_157
	movq	%r8, (%rdi)	# tmp315, *_152
	jne	.L157	#,
	leaq	(%rax,%r10), %r8	#, D.7713
	movq	-152(%rbp), %r9	# %sfp, hist1
	movq	-144(%rbp), %r10	# %sfp, in
	movq	%rax, %rdx	# b, ivtmp.184
	.p2align 4,,10
	.p2align 3
.L158:
	movq	(%rdx), %rbx	# MEM[base: _195, offset: 0B], D.7711
	addq	$8, %rdx	#, ivtmp.184
	cmpq	%r8, %rdx	# D.7713, ivtmp.184
	movzbl	%bh, %ecx	# D.7711, pos
	leaq	(%r9,%rcx,8), %rsi	#, D.7712
	movq	(%rsi), %rcx	# *_168, D.7711
	leaq	1(%rcx), %rdi	#, tmp319
	movq	%rbx, (%r10,%rcx,8)	# D.7711, *_173
	movq	%rdi, (%rsi)	# tmp319, *_168
	jne	.L158	#,
	movq	-144(%rbp), %r9	# %sfp, in
	movq	-136(%rbp), %r10	# %sfp, sz
	xorl	%ecx, %ecx	# i
	.p2align 4,,10
	.p2align 3
.L159:
	movq	(%r9,%rcx,8), %rdi	# MEM[base: in_43(D), index: i_193, step: 8, offset: 0B], D.7711
	addq	$1, %rcx	#, i
	movq	%rdi, %rdx	# D.7711, pos
	shrq	$13, %rdx	#, pos
	andl	$2040, %edx	#, D.7711
	addq	%r12, %rdx	# hist2, D.7712
	cmpq	%rcx, %r10	# i, sz
	movq	(%rdx), %rsi	# *_184, D.7711
	leaq	1(%rsi), %r8	#, tmp323
	movq	%rdi, (%rax,%rsi,8)	# D.7711, *_189
	movq	%r8, (%rdx)	# tmp323, *_184
	ja	.L159	#,
	movq	-144(%rbp), %r9	# %sfp, in
	movq	-136(%rbp), %r10	# %sfp, sz
	xorl	%ecx, %ecx	# i
	.p2align 4,,10
	.p2align 3
.L160:
	movq	(%rax,%rcx,8), %rdi	# MEM[base: b_271, index: i_209, step: 8, offset: 0B], D.7711
	addq	$1, %rcx	#, i
	movq	%rdi, %rdx	# D.7711, pos
	shrq	$21, %rdx	#, pos
	andl	$2040, %edx	#, D.7711
	addq	%r13, %rdx	# hist3, D.7712
	cmpq	%rcx, %r10	# i, sz
	movq	(%rdx), %rsi	# *_200, D.7711
	leaq	1(%rsi), %r8	#, tmp327
	movq	%rdi, (%r9,%rsi,8)	# D.7711, *_205
	movq	%r8, (%rdx)	# tmp327, *_200
	ja	.L160	#,
	movq	-144(%rbp), %r9	# %sfp, in
	movq	-136(%rbp), %r10	# %sfp, sz
	xorl	%ecx, %ecx	# i
	.p2align 4,,10
	.p2align 3
.L161:
	movq	(%r9,%rcx,8), %rdi	# MEM[base: in_43(D), index: i_225, step: 8, offset: 0B], D.7711
	addq	$1, %rcx	#, i
	movq	%rdi, %rdx	# D.7711, pos
	shrq	$29, %rdx	#, pos
	andl	$2040, %edx	#, D.7711
	addq	%r14, %rdx	# hist4, D.7712
	cmpq	%rcx, %r10	# i, sz
	movq	(%rdx), %rsi	# *_216, D.7711
	leaq	1(%rsi), %r8	#, tmp331
	movq	%rdi, (%rax,%rsi,8)	# D.7711, *_221
	movq	%r8, (%rdx)	# tmp331, *_216
	ja	.L161	#,
	movq	-144(%rbp), %r9	# %sfp, in
	movq	-136(%rbp), %r10	# %sfp, sz
	xorl	%ecx, %ecx	# i
	.p2align 4,,10
	.p2align 3
.L162:
	movq	(%rax,%rcx,8), %rdi	# MEM[base: b_271, index: i_241, step: 8, offset: 0B], D.7711
	addq	$1, %rcx	#, i
	movq	%rdi, %rdx	# D.7711, pos
	shrq	$37, %rdx	#, pos
	andl	$2040, %edx	#, D.7711
	addq	%r15, %rdx	# hist5, D.7712
	cmpq	%rcx, %r10	# i, sz
	movq	(%rdx), %rsi	# *_232, D.7711
	leaq	1(%rsi), %r8	#, tmp335
	movq	%rdi, (%r9,%rsi,8)	# D.7711, *_237
	movq	%r8, (%rdx)	# tmp335, *_232
	ja	.L162	#,
	movq	-120(%rbp), %r9	# %sfp, hist6
	movq	-144(%rbp), %r10	# %sfp, in
	xorl	%ecx, %ecx	# i
	movq	-136(%rbp), %r11	# %sfp, sz
	.p2align 4,,10
	.p2align 3
.L164:
	movq	(%r10,%rcx,8), %rdi	# MEM[base: in_43(D), index: i_73, step: 8, offset: 0B], D.7711
	addq	$1, %rcx	#, i
	movq	%rdi, %rdx	# D.7711, pos
	shrq	$45, %rdx	#, pos
	andl	$2040, %edx	#, D.7711
	addq	%r9, %rdx	# hist6, D.7712
	cmpq	%rcx, %r11	# i, sz
	movq	(%rdx), %rsi	# *_248, D.7711
	leaq	1(%rsi), %r8	#, tmp339
	movq	%rdi, (%rax,%rsi,8)	# D.7711, *_253
	movq	%r8, (%rdx)	# tmp339, *_248
	ja	.L164	#,
	movq	-128(%rbp), %r9	# %sfp, hist7
	movq	-144(%rbp), %r10	# %sfp, in
	xorl	%edx, %edx	# i
	movq	-136(%rbp), %r11	# %sfp, sz
	.p2align 4,,10
	.p2align 3
.L165:
	movq	(%rax,%rdx,8), %rsi	# MEM[base: b_271, index: i_64, step: 8, offset: 0B], D.7711
	addq	$1, %rdx	#, i
	movq	%rsi, %rcx	# D.7711, pos
	shrq	$56, %rcx	#, pos
	cmpq	%rdx, %r11	# i, sz
	leaq	(%r9,%rcx,8), %rdi	#, D.7712
	movq	(%rdi), %rcx	# *_262, D.7711
	leaq	1(%rcx), %r8	#, tmp342
	movq	%rsi, (%r10,%rcx,8)	# D.7711, *_267
	movq	%r8, (%rdi)	# tmp342, *_262
	ja	.L165	#,
	jmp	.L156	#
	.cfi_endproc
.LFE75:
	.size	radix_sort_u8_u64, .-radix_sort_u8_u64
	.section	.text.unlikely
.LCOLDE14:
	.text
.LHOTE14:
	.section	.text.unlikely
.LCOLDB15:
	.text
.LHOTB15:
	.p2align 4,,15
	.globl	radix_sort_compact_u64
	.type	radix_sort_compact_u64, @function
radix_sort_compact_u64:
.LFB76:
	.cfi_startproc
	pushq	%r14	#
	.cfi_def_cfa_offset 16
	.cfi_offset 14, -16
	pushq	%r13	#
	.cfi_def_cfa_offset 24
	.cfi_offset 13, -24
	movl	$16384, %edx	#,
	pushq	%r12	#
	.cfi_def_cfa_offset 32
	.cfi_offset 12, -32
	pushq	%rbp	#
	.cfi_def_cfa_offset 40
	.cfi_offset 6, -40
	movq	%rsi, %rbp	# sz, sz
	pushq	%rbx	#
	.cfi_def_cfa_offset 48
	.cfi_offset 3, -48
	movq	%rdi, %rbx	# in, in
	xorl	%esi, %esi	#
	subq	$16448, %rsp	#,
	.cfi_def_cfa_offset 16496
	leaq	64(%rsp), %rdi	#, tmp257
	call	memset	#
	xorl	%eax, %eax	# tmp188
	movl	$8, %ecx	#, tmp189
	movq	%rsp, %rdi	# ivtmp.241, tmp187
	testq	%rbp, %rbp	# sz
	movq	%rsp, %r9	#, ivtmp.241
	rep stosq
	je	.L202	#,
	leaq	(%rbx,%rbp,8), %rsi	#, D.7789
	movq	%rbx, %rcx	# in, ivtmp.270
	.p2align 4,,10
	.p2align 3
.L192:
	movq	(%rcx), %rax	# MEM[base: _86, offset: 0B], D.7786
	addq	$8, %rcx	#, ivtmp.270
	movzbl	%al, %edx	# D.7786, D.7786
	addq	$1, 64(%rsp,%rdx,8)	#, hist
	movzbl	%ah, %edx	# D.7786, D.7786
	addq	$1, 2112(%rsp,%rdx,8)	#, hist
	movq	%rax, %rdx	# D.7786, D.7786
	shrq	$16, %rdx	#, D.7786
	movzbl	%dl, %edx	# D.7786, D.7786
	addq	$1, 4160(%rsp,%rdx,8)	#, hist
	movq	%rax, %rdx	# D.7786, D.7786
	shrq	$24, %rdx	#, D.7786
	movzbl	%dl, %edx	# D.7786, D.7786
	addq	$1, 6208(%rsp,%rdx,8)	#, hist
	movq	%rax, %rdx	# D.7786, D.7786
	shrq	$32, %rdx	#, D.7786
	movzbl	%dl, %edx	# D.7786, D.7786
	addq	$1, 8256(%rsp,%rdx,8)	#, hist
	movq	%rax, %rdx	# D.7786, D.7786
	shrq	$40, %rdx	#, D.7786
	movzbl	%dl, %edx	# D.7786, D.7786
	addq	$1, 10304(%rsp,%rdx,8)	#, hist
	movq	%rax, %rdx	# D.7786, D.7786
	shrq	$56, %rax	#, D.7786
	shrq	$48, %rdx	#, D.7786
	movzbl	%dl, %edx	# D.7786, D.7786
	addq	$1, 12352(%rsp,%rdx,8)	#, hist
	addq	$1, 14400(%rsp,%rax,8)	#, hist
	cmpq	%rcx, %rsi	# ivtmp.270, D.7789
	jne	.L192	#,
	movq	64(%rsp), %rsi	# hist, D.7791
.L191:
	leaq	64(%rsp), %r8	#, tmp264
	leaq	64(%r9), %rdi	#, D.7789
	xorl	%eax, %eax	# D.7791
	leaq	2048(%r8), %r10	#, D.7789
	.p2align 4,,10
	.p2align 3
.L195:
	movq	%r9, %rdx	# ivtmp.241, ivtmp.241
	movq	%r8, %rcx	# ivtmp.265, ivtmp.237
	jmp	.L196	#
	.p2align 4,,10
	.p2align 3
.L193:
	movq	(%rcx), %rsi	# MEM[base: _144, offset: 0B], D.7791
	movq	(%rdx), %rax	# MEM[base: _143, offset: 0B], D.7791
.L196:
	movq	%rax, (%rcx)	# D.7791, MEM[base: _148, offset: 0B]
	addq	%rsi, %rax	# D.7791, tmp239
	addq	$8, %rdx	#, ivtmp.241
	movq	%rax, -8(%rdx)	# tmp239, MEM[base: _147, offset: 0B]
	addq	$2048, %rcx	#, ivtmp.237
	cmpq	%rdx, %rdi	# ivtmp.241, D.7789
	jne	.L193	#,
	addq	$8, %r8	#, ivtmp.265
	cmpq	%r8, %r10	# ivtmp.265, D.7789
	je	.L194	#,
	movq	(%r8), %rsi	# MEM[base: _68, offset: 0B], D.7791
	movq	(%rsp), %rax	# sum, D.7791
	jmp	.L195	#
.L194:
	leaq	0(,%rbp,8), %r12	#, D.7786
	movl	$1, %esi	#,
	movq	%r12, %rdi	# D.7786,
	call	calloc	#
	testq	%rax, %rax	# in
	movq	%rax, %r13	#, in
	jne	.L197	#,
	testq	%r12, %r12	# D.7786
	je	.L197	#,
	call	oom_fun	#
.L197:
	movq	%r13, %r10	# in, in
	xorl	%ecx, %ecx	# ivtmp.224
	xorl	%eax, %eax	# j
	.p2align 4,,10
	.p2align 3
.L198:
	testq	%rbp, %rbp	# sz
	je	.L201	#,
	movq	%rax, %r11	# j, tmp255
	leaq	(%rbx,%r12), %rdi	#, D.7789
	movq	%rbx, %rsi	# in, ivtmp.216
	salq	$8, %r11	#, tmp255
	.p2align 4,,10
	.p2align 3
.L199:
	movq	(%rsi), %r9	# MEM[base: _155, offset: 0B], D.7786
	addq	$8, %rsi	#, ivtmp.216
	movq	%r9, %rdx	# D.7786, D.7786
	shrq	%cl, %rdx	# ivtmp.224, D.7786
	movzbl	%dl, %edx	# D.7786, pos
	addq	%r11, %rdx	# tmp255, tmp249
	cmpq	%rsi, %rdi	# ivtmp.216, D.7789
	movq	64(%rsp,%rdx,8), %r8	# hist, D.7786
	leaq	1(%r8), %r14	#, tmp252
	movq	%r9, (%r10,%r8,8)	# D.7786, *_84
	movq	%r14, 64(%rsp,%rdx,8)	# tmp252, hist
	jne	.L199	#,
.L201:
	addq	$1, %rax	#, j
	addl	$8, %ecx	#, ivtmp.224
	movq	%r10, %rdx	# in, in
	cmpq	$8, %rax	#, j
	movq	%rbx, %r10	# in, in
	je	.L200	#,
	movq	%rdx, %rbx	# in, in
	jmp	.L198	#
.L200:
	addq	$16448, %rsp	#,
	.cfi_remember_state
	.cfi_def_cfa_offset 48
	movq	%r13, %rdi	# in,
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
.L202:
	.cfi_restore_state
	xorl	%esi, %esi	# D.7791
	jmp	.L191	#
	.cfi_endproc
.LFE76:
	.size	radix_sort_compact_u64, .-radix_sort_compact_u64
	.section	.text.unlikely
.LCOLDE15:
	.text
.LHOTE15:
	.section	.text.unlikely
.LCOLDB16:
	.text
.LHOTB16:
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
	movq	%rdi, %r15	# in, in
	pushq	%r13	#
	.cfi_def_cfa_offset 32
	.cfi_offset 13, -32
	pushq	%r12	#
	.cfi_def_cfa_offset 40
	.cfi_offset 12, -40
	movq	%rdx, %r12	# get_key, get_key
	pushq	%rbp	#
	.cfi_def_cfa_offset 48
	.cfi_offset 6, -48
	pushq	%rbx	#
	.cfi_def_cfa_offset 56
	.cfi_offset 3, -56
	movl	$16384, %edx	#,
	movq	%rsi, %rbx	# sz, sz
	xorl	%esi, %esi	#
	subq	$16488, %rsp	#,
	.cfi_def_cfa_offset 16544
	leaq	96(%rsp), %rdi	#, tmp256
	leaq	32(%rsp), %rbp	#, ivtmp.302
	call	memset	#
	xorl	%eax, %eax	# tmp186
	movl	$8, %ecx	#, tmp187
	movq	%rbp, %rdi	# ivtmp.302, tmp185
	testq	%rbx, %rbx	# sz
	rep stosq
	je	.L227	#,
	xorl	%r13d, %r13d	# i
	.p2align 4,,10
	.p2align 3
.L217:
	movq	(%r15,%r13,8), %rdi	# MEM[base: in_18(D), index: i_114, step: 8, offset: 0B], MEM[base: in_18(D), index: i_114, step: 8, offset: 0B]
	addq	$1, %r13	#, i
	call	*%r12	# get_key
	movzbl	%al, %edx	# key, D.7861
	addq	$1, 96(%rsp,%rdx,8)	#, hist
	movzbl	%ah, %edx	# key, D.7861
	addq	$1, 2144(%rsp,%rdx,8)	#, hist
	movq	%rax, %rdx	# key, D.7861
	shrq	$16, %rdx	#, D.7861
	movzbl	%dl, %edx	# D.7861, D.7861
	addq	$1, 4192(%rsp,%rdx,8)	#, hist
	movq	%rax, %rdx	# key, D.7861
	shrq	$24, %rdx	#, D.7861
	movzbl	%dl, %edx	# D.7861, D.7861
	addq	$1, 6240(%rsp,%rdx,8)	#, hist
	movq	%rax, %rdx	# key, D.7861
	shrq	$32, %rdx	#, D.7861
	movzbl	%dl, %edx	# D.7861, D.7861
	addq	$1, 8288(%rsp,%rdx,8)	#, hist
	movq	%rax, %rdx	# key, D.7861
	shrq	$40, %rdx	#, D.7861
	movzbl	%dl, %edx	# D.7861, D.7861
	addq	$1, 10336(%rsp,%rdx,8)	#, hist
	movq	%rax, %rdx	# key, D.7861
	shrq	$56, %rax	#, D.7861
	shrq	$48, %rdx	#, D.7861
	movzbl	%dl, %edx	# D.7861, D.7861
	addq	$1, 12384(%rsp,%rdx,8)	#, hist
	addq	$1, 14432(%rsp,%rax,8)	#, hist
	cmpq	%r13, %rbx	# i, sz
	jne	.L217	#,
	movq	96(%rsp), %rsi	# hist, D.7865
.L216:
	leaq	96(%rsp), %r9	#, tmp263
	leaq	64(%rbp), %rdi	#, D.7866
	xorl	%eax, %eax	# D.7865
	leaq	2048(%r9), %r10	#, D.7866
	.p2align 4,,10
	.p2align 3
.L220:
	movq	%rbp, %rdx	# ivtmp.302, ivtmp.302
	movq	%r9, %rcx	# ivtmp.326, ivtmp.298
	jmp	.L221	#
	.p2align 4,,10
	.p2align 3
.L218:
	movq	(%rcx), %rsi	# MEM[base: _135, offset: 0B], D.7865
	movq	(%rdx), %rax	# MEM[base: _134, offset: 0B], D.7865
.L221:
	movq	%rax, (%rcx)	# D.7865, MEM[base: _5, offset: 0B]
	addq	%rsi, %rax	# D.7865, tmp237
	addq	$8, %rdx	#, ivtmp.302
	movq	%rax, -8(%rdx)	# tmp237, MEM[base: _3, offset: 0B]
	addq	$2048, %rcx	#, ivtmp.298
	cmpq	%rdi, %rdx	# D.7866, ivtmp.302
	jne	.L218	#,
	addq	$8, %r9	#, ivtmp.326
	cmpq	%r10, %r9	# D.7866, ivtmp.326
	je	.L219	#,
	movq	(%r9), %rsi	# MEM[base: _69, offset: 0B], D.7865
	movq	32(%rsp), %rax	# sum, D.7865
	jmp	.L220	#
.L219:
	leaq	0(,%rbx,8), %rbp	#, D.7861
	movl	$1, %esi	#,
	movq	%rbp, %rdi	# D.7861,
	call	calloc	#
	testq	%rax, %rax	# in
	movq	%rax, 24(%rsp)	# in, %sfp
	jne	.L222	#,
	testq	%rbp, %rbp	# D.7861
	je	.L222	#,
	call	oom_fun	#
.L222:
	movq	24(%rsp), %rbp	# %sfp, in
	movq	$0, 16(%rsp)	#, %sfp
	.p2align 4,,10
	.p2align 3
.L223:
	testq	%rbx, %rbx	# sz
	je	.L226	#,
	movq	16(%rsp), %r13	# %sfp, j
	xorl	%r14d, %r14d	# i
	movl	%r13d, %eax	# j, tmp269
	salq	$8, %r13	#, tmp254
	sall	$3, %eax	#, D.7862
	movl	%eax, 12(%rsp)	# D.7862, %sfp
	.p2align 4,,10
	.p2align 3
.L224:
	movq	(%r15,%r14,8), %rdi	# MEM[base: in_74, index: i_112, step: 8, offset: 0B], MEM[base: in_74, index: i_112, step: 8, offset: 0B]
	call	*%r12	# get_key
	movzbl	12(%rsp), %ecx	# %sfp, tmp265
	shrq	%cl, %rax	# tmp265, D.7861
	movzbl	%al, %eax	# D.7861, pos
	addq	%r13, %rax	# tmp254, tmp248
	movq	96(%rsp,%rax,8), %rcx	# hist, D.7861
	leaq	1(%rcx), %rsi	#, tmp251
	movq	%rsi, 96(%rsp,%rax,8)	# tmp251, hist
	movq	(%r15,%r14,8), %rax	# MEM[base: in_74, index: i_112, step: 8, offset: 0B], D.7860
	addq	$1, %r14	#, i
	cmpq	%r14, %rbx	# i, sz
	movq	%rax, 0(%rbp,%rcx,8)	# D.7860, *_88
	jne	.L224	#,
.L226:
	addq	$1, 16(%rsp)	#, %sfp
	movq	%rbp, %rax	# in, in
	movq	%r15, %rbp	# in, in
	movq	16(%rsp), %rdi	# %sfp, j
	cmpq	$8, %rdi	#, j
	je	.L225	#,
	movq	%rax, %r15	# in, in
	jmp	.L223	#
.L225:
	movq	24(%rsp), %rdi	# %sfp,
	addq	$16488, %rsp	#,
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
	jmp	free	#
.L227:
	.cfi_restore_state
	xorl	%esi, %esi	# D.7865
	jmp	.L216	#
	.cfi_endproc
.LFE77:
	.size	radix_sort_keys, .-radix_sort_keys
	.section	.text.unlikely
.LCOLDE16:
	.text
.LHOTE16:
	.section	.text.unlikely
.LCOLDB17:
	.text
.LHOTB17:
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
	leaq	15(%rsp), %rbx	#, tmp190
	andq	$-16, %rbx	#, tmp192
	movq	%rbx, %rdi	# tmp192,
	leaq	524288(%rbx), %r15	#, hist1
	leaq	1048576(%rbx), %r12	#, hist2
	call	memset	#
	movq	-56(%rbp), %rax	# %sfp, sz
	leaq	1572864(%rbx), %r13	#, hist3
	movq	%r14, %rcx	# in, ivtmp.369
	testq	%rax, %rax	# sz
	leaq	(%r14,%rax,8), %rsi	#, D.7916
	je	.L244	#,
	.p2align 4,,10
	.p2align 3
.L259:
	movq	(%rcx), %rax	# MEM[base: _75, offset: 0B], D.7914
	addq	$8, %rcx	#, ivtmp.369
	movzwl	%ax, %edx	# D.7914, D.7914
	addq	$1, (%rbx,%rdx,8)	#, *_32
	movq	%rax, %rdx	# D.7914, D.7914
	shrq	$13, %rdx	#, D.7914
	andl	$524280, %edx	#, D.7914
	addq	$1, (%r15,%rdx)	#, *_42
	movq	%rax, %rdx	# D.7914, D.7914
	shrq	$48, %rax	#, D.7914
	shrq	$29, %rdx	#, D.7914
	andl	$524280, %edx	#, D.7914
	addq	$1, (%r12,%rdx)	#, *_51
	addq	$1, 0(%r13,%rax,8)	#, *_57
	cmpq	%rcx, %rsi	# ivtmp.369, D.7916
	jne	.L259	#,
.L244:
	xorl	%eax, %eax	# i
	xorl	%edi, %edi	# sum3
	xorl	%esi, %esi	# sum2
	xorl	%ecx, %ecx	# sum1
	xorl	%edx, %edx	# sum0
	.p2align 4,,10
	.p2align 3
.L242:
	movq	%rdx, %r11	# sum0, sum0
	movq	%rcx, %r10	# sum1, sum1
	addq	(%rbx,%rax,8), %r11	# MEM[base: hist0_19, index: i_152, step: 8, offset: 0B], sum0
	addq	524288(%rbx,%rax,8), %r10	# MEM[base: hist0_19, index: i_152, step: 8, offset: 524288B], sum1
	movq	%rdx, (%rbx,%rax,8)	# sum0, MEM[base: hist0_19, index: i_152, step: 8, offset: 0B]
	movq	%rsi, %r9	# sum2, sum2
	movq	%rcx, 524288(%rbx,%rax,8)	# sum1, MEM[base: hist0_19, index: i_152, step: 8, offset: 524288B]
	movq	%rdi, %r8	# sum3, sum3
	addq	(%r12,%rax,8), %r9	# MEM[base: hist2_22, index: i_152, step: 8, offset: 0B], sum2
	movq	%rsi, (%r12,%rax,8)	# sum2, MEM[base: hist2_22, index: i_152, step: 8, offset: 0B]
	addq	0(%r13,%rax,8), %r8	# MEM[base: hist3_23, index: i_152, step: 8, offset: 0B], sum3
	movq	%rdi, 0(%r13,%rax,8)	# sum3, MEM[base: hist3_23, index: i_152, step: 8, offset: 0B]
	addq	$1, %rax	#, i
	movq	%r11, %rdx	# sum0, sum0
	cmpq	$65536, %rax	#, i
	movq	%r10, %rcx	# sum1, sum1
	movq	%r9, %rsi	# sum2, sum2
	movq	%r8, %rdi	# sum3, sum3
	jne	.L242	#,
	movq	-56(%rbp), %rax	# %sfp, sz
	movl	$1, %esi	#,
	leaq	0(,%rax,8), %r8	#, D.7914
	movq	%r8, %rdi	# D.7914,
	movq	%r8, -64(%rbp)	# D.7914, %sfp
	call	calloc	#
	testq	%rax, %rax	# a
	movq	-64(%rbp), %r8	# %sfp, D.7914
	jne	.L245	#,
	testq	%r8, %r8	# D.7914
	je	.L245	#,
	movq	%rax, -72(%rbp)	# a, %sfp
	call	oom_fun	#
	cmpq	$0, -56(%rbp)	#, %sfp
	movq	-64(%rbp), %r8	# %sfp, D.7914
	movq	-72(%rbp), %rax	# %sfp, a
	jne	.L252	#,
.L246:
	movq	%rax, %rdi	# a,
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
.L245:
	.cfi_restore_state
	cmpq	$0, -56(%rbp)	#, %sfp
	je	.L246	#,
.L252:
	leaq	(%r14,%r8), %r10	#, D.7916
	movq	%r14, %rdx	# in, ivtmp.353
	.p2align 4,,10
	.p2align 3
.L247:
	movq	(%rdx), %rsi	# MEM[base: _99, offset: 0B], D.7914
	addq	$8, %rdx	#, ivtmp.353
	cmpq	%r10, %rdx	# D.7916, ivtmp.353
	movzwl	%si, %ecx	# D.7914, pos
	leaq	(%rbx,%rcx,8), %rdi	#, D.7915
	movq	(%rdi), %rcx	# *_88, D.7914
	leaq	1(%rcx), %r9	#, tmp219
	movq	%rsi, (%rax,%rcx,8)	# D.7914, *_93
	movq	%r9, (%rdi)	# tmp219, *_88
	jne	.L247	#,
	movq	%rax, %rcx	# a, ivtmp.348
	addq	%rax, %r8	# a, D.7916
	.p2align 4,,10
	.p2align 3
.L248:
	movq	(%rcx), %rdi	# MEM[base: _130, offset: 0B], D.7914
	addq	$8, %rcx	#, ivtmp.348
	movq	%rdi, %rdx	# D.7914, pos
	shrq	$13, %rdx	#, pos
	andl	$524280, %edx	#, D.7914
	addq	%r15, %rdx	# hist1, D.7915
	cmpq	%r8, %rcx	# D.7916, ivtmp.348
	movq	(%rdx), %rsi	# *_104, D.7914
	leaq	1(%rsi), %r9	#, tmp223
	movq	%rdi, (%r14,%rsi,8)	# D.7914, *_109
	movq	%r9, (%rdx)	# tmp223, *_104
	jne	.L248	#,
	xorl	%ecx, %ecx	# i
	.p2align 4,,10
	.p2align 3
.L250:
	movq	(%r14,%rcx,8), %rdi	# MEM[base: in_27(D), index: i_46, step: 8, offset: 0B], D.7914
	addq	$1, %rcx	#, i
	movq	%rdi, %rdx	# D.7914, pos
	shrq	$29, %rdx	#, pos
	andl	$524280, %edx	#, D.7914
	addq	%r12, %rdx	# hist2, D.7915
	cmpq	%rcx, -56(%rbp)	# i, %sfp
	movq	(%rdx), %rsi	# *_120, D.7914
	leaq	1(%rsi), %r8	#, tmp227
	movq	%rdi, (%rax,%rsi,8)	# D.7914, *_125
	movq	%r8, (%rdx)	# tmp227, *_120
	ja	.L250	#,
	xorl	%edx, %edx	# i
	.p2align 4,,10
	.p2align 3
.L251:
	movq	(%rax,%rdx,8), %rsi	# MEM[base: a_143, index: i_37, step: 8, offset: 0B], D.7914
	addq	$1, %rdx	#, i
	movq	%rsi, %rcx	# D.7914, pos
	shrq	$48, %rcx	#, pos
	cmpq	%rdx, -56(%rbp)	# i, %sfp
	leaq	0(%r13,%rcx,8), %rdi	#, D.7915
	movq	(%rdi), %rcx	# *_134, D.7914
	leaq	1(%rcx), %r8	#, tmp230
	movq	%rsi, (%r14,%rcx,8)	# D.7914, *_139
	movq	%r8, (%rdi)	# tmp230, *_134
	ja	.L251	#,
	jmp	.L246	#
	.cfi_endproc
.LFE78:
	.size	radix_sort_u16_u64, .-radix_sort_u16_u64
	.section	.text.unlikely
.LCOLDE17:
	.text
.LHOTE17:
	.section	.text.unlikely
.LCOLDB18:
	.text
.LHOTB18:
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
.LCOLDE18:
	.text
.LHOTE18:
	.ident	"GCC: (GNU) 5.3.0"
	.section	.note.GNU-stack,"",@progbits
