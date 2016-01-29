	.file	"sorting.c"
# GNU C11 (GCC) version 5.3.0 (x86_64-unknown-linux-gnu)
#	compiled by GNU C version 5.3.0, GMP version 6.1.0, MPFR version 3.1.3-p5, MPC version 1.0.3
# GGC heuristics: --param ggc-min-expand=100 --param ggc-min-heapsize=131072
# options passed:  sorting.c -mtune=generic -march=x86-64 -O2 -fverbose-asm
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
	.globl	cmp_lt
	.type	cmp_lt, @function
cmp_lt:
.LFB54:
	.cfi_startproc
	xorl	%eax, %eax	# D.6377
	cmpq	%rsi, %rdi	# y, x
	setb	%al	#, D.6377
	ret
	.cfi_endproc
.LFE54:
	.size	cmp_lt, .-cmp_lt
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
.LFB50:
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
.LFE50:
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
	.type	is_prefix, @function
is_prefix:
.LFB70:
	.cfi_startproc
	pushq	%r12	#
	.cfi_def_cfa_offset 16
	.cfi_offset 12, -16
	pushq	%rbp	#
	.cfi_def_cfa_offset 24
	.cfi_offset 6, -24
	movq	%rsi, %r12	# b, b
	pushq	%rbx	#
	.cfi_def_cfa_offset 32
	.cfi_offset 3, -32
	movq	%rdi, %rbp	# a, a
	call	strlen	#
	movq	%r12, %rdi	# b,
	movq	%rax, %rbx	#, tmp102
	call	strlen	#
	cmpl	%eax, %ebx	# tmp105, tmp102
	jg	.L6	#,
	testl	%ebx, %ebx	# tmp102
	je	.L6	#,
	movslq	%ebx, %rdx	# tmp102, D.6386
	movq	%r12, %rsi	# b,
	movq	%rbp, %rdi	# a,
	call	memcmp	#
	testl	%eax, %eax	# tmp112
	sete	%al	#, D.6385
	movzbl	%al, %eax	# D.6385, D.6385
.L5:
	popq	%rbx	#
	.cfi_remember_state
	.cfi_def_cfa_offset 24
	popq	%rbp	#
	.cfi_def_cfa_offset 16
	popq	%r12	#
	.cfi_def_cfa_offset 8
	ret
.L6:
	.cfi_restore_state
	xorl	%eax, %eax	# D.6385
	jmp	.L5	#
	.cfi_endproc
.LFE70:
	.size	is_prefix, .-is_prefix
	.section	.text.unlikely
.LCOLDE3:
	.text
.LHOTE3:
	.section	.text.unlikely
.LCOLDB4:
	.text
.LHOTB4:
	.p2align 4,,15
	.globl	cmp_gt
	.type	cmp_gt, @function
cmp_gt:
.LFB53:
	.cfi_startproc
	xorl	%eax, %eax	# D.6391
	cmpq	%rsi, %rdi	# y, x
	seta	%al	#, D.6391
	ret
	.cfi_endproc
.LFE53:
	.size	cmp_gt, .-cmp_gt
	.section	.text.unlikely
.LCOLDE4:
	.text
.LHOTE4:
	.section	.text.unlikely
.LCOLDB5:
	.text
.LHOTB5:
	.p2align 4,,15
	.globl	is_sorted_generic
	.type	is_sorted_generic, @function
is_sorted_generic:
.LFB55:
	.cfi_startproc
	cmpq	$1, %rsi	#, len
	je	.L25	#,
	pushq	%r13	#
	.cfi_def_cfa_offset 16
	.cfi_offset 13, -16
	pushq	%r12	#
	.cfi_def_cfa_offset 24
	.cfi_offset 12, -24
	movq	%rdx, %r13	# cmp, cmp
	pushq	%rbp	#
	.cfi_def_cfa_offset 32
	.cfi_offset 6, -32
	pushq	%rbx	#
	.cfi_def_cfa_offset 40
	.cfi_offset 3, -40
	movq	%rdi, %rbp	# input, input
	movq	%rsi, %r12	# len, len
	movl	$1, %ebx	#, ivtmp.46
	subq	$8, %rsp	#,
	.cfi_def_cfa_offset 48
	jmp	.L17	#
	.p2align 4,,10
	.p2align 3
.L27:
	addq	$1, %rbx	#, ivtmp.46
	cmpq	%rbx, %r12	# ivtmp.46, len
	je	.L26	#,
.L17:
	movq	0(%rbp,%rbx,8), %rsi	# MEM[base: input_11(D), index: ivtmp.46_23, step: 8, offset: 0B], MEM[base: input_11(D), index: ivtmp.46_23, step: 8, offset: 0B]
	movq	-8(%rbp,%rbx,8), %rdi	# MEM[base: input_11(D), index: ivtmp.46_23, step: 8, offset: -8B], MEM[base: input_11(D), index: ivtmp.46_23, step: 8, offset: -8B]
	call	*%r13	# cmp
	testl	%eax, %eax	# D.6403
	jne	.L27	#,
	addq	$8, %rsp	#,
	.cfi_remember_state
	.cfi_def_cfa_offset 40
	popq	%rbx	#
	.cfi_restore 3
	.cfi_def_cfa_offset 32
	popq	%rbp	#
	.cfi_restore 6
	.cfi_def_cfa_offset 24
	popq	%r12	#
	.cfi_restore 12
	.cfi_def_cfa_offset 16
	popq	%r13	#
	.cfi_restore 13
	.cfi_def_cfa_offset 8
	ret
	.p2align 4,,10
	.p2align 3
.L26:
	.cfi_restore_state
	addq	$8, %rsp	#,
	.cfi_def_cfa_offset 40
	movl	$1, %eax	#, D.6403
	popq	%rbx	#
	.cfi_restore 3
	.cfi_def_cfa_offset 32
	popq	%rbp	#
	.cfi_restore 6
	.cfi_def_cfa_offset 24
	popq	%r12	#
	.cfi_restore 12
	.cfi_def_cfa_offset 16
	popq	%r13	#
	.cfi_restore 13
	.cfi_def_cfa_offset 8
	ret
.L25:
	movl	$1, %eax	#, D.6403
	ret
	.cfi_endproc
.LFE55:
	.size	is_sorted_generic, .-is_sorted_generic
	.section	.text.unlikely
.LCOLDE5:
	.text
.LHOTE5:
	.section	.text.unlikely
.LCOLDB6:
	.text
.LHOTB6:
	.p2align 4,,15
	.globl	is_sorted
	.type	is_sorted, @function
is_sorted:
.LFB56:
	.cfi_startproc
	subq	$1, %rsi	#, D.6416
	je	.L32	#,
	movq	8(%rdi), %rax	# MEM[(uint64_t *)input_7(D) + 8B], tmp99
	cmpq	%rax, (%rdi)	# tmp99, *input_7(D)
	ja	.L34	#,
	movl	$1, %eax	#, ivtmp.57
	jmp	.L30	#
	.p2align 4,,10
	.p2align 3
.L31:
	movq	(%rdi,%rax,8), %rdx	# MEM[base: input_7(D), index: ivtmp.57_24, step: 8, offset: 0B], D.6416
	addq	$1, %rax	#, ivtmp.57
	cmpq	(%rdi,%rax,8), %rdx	# MEM[base: input_7(D), index: ivtmp.57_19, step: 8, offset: 0B], D.6416
	ja	.L34	#,
.L30:
	cmpq	%rax, %rsi	# ivtmp.57, D.6416
	jne	.L31	#,
.L32:
	movl	$1, %eax	#, D.6415
	ret
	.p2align 4,,10
	.p2align 3
.L34:
	xorl	%eax, %eax	# D.6415
	ret
	.cfi_endproc
.LFE56:
	.size	is_sorted, .-is_sorted
	.section	.text.unlikely
.LCOLDE6:
	.text
.LHOTE6:
	.section	.text.unlikely
.LCOLDB7:
	.text
.LHOTB7:
	.p2align 4,,15
	.globl	insertion_sort_generic
	.type	insertion_sort_generic, @function
insertion_sort_generic:
.LFB57:
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
	subq	$24, %rsp	#,
	.cfi_def_cfa_offset 80
	cmpq	$1, %rsi	#, len
	movq	%rsi, 8(%rsp)	# len, %sfp
	movq	$1, (%rsp)	#, %sfp
	jbe	.L35	#,
	leaq	8(%rdi), %r13	#, ivtmp.79
	movq	%rdi, %r15	# input, input
	movq	%rdx, %r12	# cmp, cmp
	.p2align 4,,10
	.p2align 3
.L43:
	movq	0(%r13), %rbx	# MEM[base: _26, offset: 0B], temp
	movq	%r13, %r14	# ivtmp.79, ivtmp.72
	.p2align 4,,10
	.p2align 3
.L37:
	movq	%r14, %rbp	# ivtmp.72, D.6442
	movq	-8(%r14), %rdi	# MEM[base: _38, offset: -8B], MEM[base: _38, offset: -8B]
	movq	%rbx, %rsi	# temp,
	subq	%r15, %rbp	# input, D.6442
	call	*%r12	# cmp
	testl	%eax, %eax	# D.6440
	jne	.L38	#,
	movq	-8(%r14), %rax	# MEM[base: _38, offset: -8B], MEM[base: _38, offset: -8B]
	subq	$8, %r14	#, ivtmp.72
	movq	%rax, 8(%r14)	# MEM[base: _38, offset: -8B], MEM[base: _38, offset: 0B]
	cmpq	%r14, %r15	# ivtmp.72, input
	jne	.L37	#,
	xorl	%ebp, %ebp	# D.6442
.L38:
	addq	$1, (%rsp)	#, %sfp
	addq	$8, %r13	#, ivtmp.79
	movq	%rbx, (%r15,%rbp)	# temp, *_27
	movq	(%rsp), %rax	# %sfp, ivtmp.78
	cmpq	%rax, 8(%rsp)	# ivtmp.78, %sfp
	jne	.L43	#,
.L35:
	addq	$24, %rsp	#,
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
	.cfi_endproc
.LFE57:
	.size	insertion_sort_generic, .-insertion_sort_generic
	.section	.text.unlikely
.LCOLDE7:
	.text
.LHOTE7:
	.section	.text.unlikely
.LCOLDB8:
	.text
.LHOTB8:
	.p2align 4,,15
	.globl	qsort_generic
	.type	qsort_generic, @function
qsort_generic:
.LFB59:
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
	movq	%rdx, %r13	# cmp, cmp
	pushq	%rbp	#
	.cfi_def_cfa_offset 48
	.cfi_offset 6, -48
	pushq	%rbx	#
	.cfi_def_cfa_offset 56
	.cfi_offset 3, -56
	subq	$40, %rsp	#,
	.cfi_def_cfa_offset 96
	cmpl	$3, %esi	#, len
	movq	%rdi, 16(%rsp)	# arr, %sfp
	movl	%esi, 28(%rsp)	# len, %sfp
	jle	.L52	#,
.L55:
	movq	16(%rsp), %rax	# %sfp, arr
	movl	28(%rsp), %ebx	# %sfp, len
	movq	$0, 8(%rsp)	#, %sfp
	movq	(%rax), %rax	# *arr_17, pivot
	movq	%rax, (%rsp)	# pivot, %sfp
	.p2align 4,,10
	.p2align 3
.L48:
	movq	8(%rsp), %rax	# %sfp, ivtmp.96
	movq	16(%rsp), %r14	# %sfp, arr
	movq	(%rsp), %rsi	# %sfp,
	movl	%eax, 24(%rsp)	# tmp145, %sfp
	movq	(%r14,%rax,8), %rdi	# MEM[base: arr_17, index: ivtmp.96_41, step: 8, offset: 0B], MEM[base: arr_17, index: ivtmp.96_41, step: 8, offset: 0B]
	call	*%r13	# cmp
	testl	%eax, %eax	# D.6474
	jne	.L49	#,
	movslq	%ebx, %rax	# len, D.6472
	leaq	-1(%rax), %r12	#, ivtmp.89
	leaq	-8(%r14,%rax,8), %r15	#, ivtmp.92
	jmp	.L50	#
	.p2align 4,,10
	.p2align 3
.L54:
	movq	%rcx, %r12	# ivtmp.89, ivtmp.89
	movl	%ebp, %ebx	# len, len
.L50:
	movq	(%r15), %rsi	# MEM[base: _42, offset: 0B],
	movq	%r15, %r14	# ivtmp.92, D.6475
	movq	(%rsp), %rdi	# %sfp,
	subq	$8, %r15	#, ivtmp.92
	leal	-1(%rbx), %ebp	#, len
	call	*%r13	# cmp
	testl	%eax, %eax	# D.6474
	leaq	-1(%r12), %rcx	#, ivtmp.89
	jne	.L54	#,
	cmpl	%ebp, 24(%rsp)	# len, %sfp
	jge	.L51	#,
	movq	16(%rsp), %rsi	# %sfp, arr
	movq	8(%rsp), %rdi	# %sfp, ivtmp.96
	movl	%ebp, %ebx	# len, len
	movq	(%r14), %rdx	# *_42, D.6473
	movq	(%rsi,%rdi,8), %rax	# MEM[base: arr_17, index: ivtmp.96_41, step: 8, offset: 0B], __temp
	movq	%rdx, (%rsi,%rdi,8)	# D.6473, MEM[base: arr_17, index: ivtmp.96_41, step: 8, offset: 0B]
	movq	%rax, (%r14)	# __temp, *_42
.L49:
	addq	$1, 8(%rsp)	#, %sfp
	jmp	.L48	#
.L51:
	movq	16(%rsp), %r14	# %sfp, arr
	movq	%r13, %rdx	# cmp,
	movl	%ebx, %esi	# len,
	movq	%r14, %rdi	# arr,
	call	qsort_generic	#
	subl	%ebx, 28(%rsp)	# len, %sfp
	leaq	8(%r14,%r12,8), %rdx	#, arr
	movl	28(%rsp), %eax	# %sfp, len
	movq	%rdx, 16(%rsp)	# arr, %sfp
	cmpl	$3, %eax	#, len
	jg	.L55	#,
.L52:
	movslq	28(%rsp), %rsi	# %sfp, D.6472
	movq	16(%rsp), %rdi	# %sfp,
	addq	$40, %rsp	#,
	.cfi_def_cfa_offset 56
	popq	%rbx	#
	.cfi_def_cfa_offset 48
	movq	%r13, %rdx	# cmp,
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
.LFE59:
	.size	qsort_generic, .-qsort_generic
	.section	.text.unlikely
.LCOLDE8:
	.text
.LHOTE8:
	.section	.text.unlikely
.LCOLDB9:
	.text
.LHOTB9:
	.p2align 4,,15
	.globl	merge
	.type	merge, @function
merge:
.LFB60:
	.cfi_startproc
	pushq	%r15	#
	.cfi_def_cfa_offset 16
	.cfi_offset 15, -16
	pushq	%r14	#
	.cfi_def_cfa_offset 24
	.cfi_offset 14, -24
	movq	%r8, %r15	# n2, n2
	pushq	%r13	#
	.cfi_def_cfa_offset 32
	.cfi_offset 13, -32
	pushq	%r12	#
	.cfi_def_cfa_offset 40
	.cfi_offset 12, -40
	movq	%rcx, %r13	# n1, n1
	pushq	%rbp	#
	.cfi_def_cfa_offset 48
	.cfi_offset 6, -48
	pushq	%rbx	#
	.cfi_def_cfa_offset 56
	.cfi_offset 3, -56
	movq	%rdi, %r12	# A, A
	movq	%rsi, %rbp	# B, B
	movq	%r9, %r14	# cmp, cmp
	leaq	8(%rdx), %rbx	#, ivtmp.106
	subq	$8, %rsp	#,
	.cfi_def_cfa_offset 64
	jmp	.L58	#
	.p2align 4,,10
	.p2align 3
.L73:
	movq	(%r12), %rax	# *A_1, D.6484
	subq	$1, %r13	#, n1
	addq	$8, %r12	#, A
	movq	%rax, -8(%rbx)	# D.6484, MEM[base: tmp_24, offset: -8B]
.L60:
	addq	$8, %rbx	#, ivtmp.106
.L58:
	testq	%r13, %r13	# n1
	leaq	-8(%rbx), %rcx	#, D.6487
	je	.L65	#,
	testq	%r15, %r15	# n2
	je	.L65	#,
	movq	0(%rbp), %rsi	# *B_2,
	movq	(%r12), %rdi	# *A_1,
	call	*%r14	# cmp
	testl	%eax, %eax	# D.6485
	jne	.L73	#,
	movq	0(%rbp), %rax	# *B_2, D.6484
	subq	$1, %r15	#, n2
	addq	$8, %rbp	#, B
	movq	%rax, -8(%rbx)	# D.6484, MEM[base: tmp_31, offset: -8B]
	jmp	.L60	#
	.p2align 4,,10
	.p2align 3
.L65:
	testq	%r13, %r13	# n1
	jne	.L74	#,
.L63:
	testq	%r15, %r15	# n2
	jne	.L75	#,
	addq	$8, %rsp	#,
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
	.p2align 4,,10
	.p2align 3
.L75:
	.cfi_restore_state
	addq	$8, %rsp	#,
	.cfi_remember_state
	.cfi_def_cfa_offset 56
	leaq	0(,%r15,8), %rdx	#, D.6486
	movq	%rbp, %rsi	# B,
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
	movq	%rcx, %rdi	# D.6487,
	jmp	memcpy	#
	.p2align 4,,10
	.p2align 3
.L74:
	.cfi_restore_state
	leaq	0(,%r13,8), %rdx	#, D.6486
	movq	%rcx, %rdi	# D.6487,
	movq	%r12, %rsi	# A,
	call	memcpy	#
	movq	%rax, %rcx	#, D.6487
	jmp	.L63	#
	.cfi_endproc
.LFE60:
	.size	merge, .-merge
	.section	.text.unlikely
.LCOLDE9:
	.text
.LHOTE9:
	.section	.text.unlikely
.LCOLDB10:
	.text
.LHOTB10:
	.p2align 4,,15
	.globl	mergesort_internal
	.type	mergesort_internal, @function
mergesort_internal:
.LFB61:
	.cfi_startproc
	cmpq	$1, %rdx	#, len
	jbe	.L76	#,
	jmp	mergesort_internal.part.0	#
	.p2align 4,,10
	.p2align 3
.L76:
	rep ret
	.cfi_endproc
.LFE61:
	.size	mergesort_internal, .-mergesort_internal
	.section	.text.unlikely
.LCOLDE10:
	.text
.LHOTE10:
	.section	.text.unlikely
.LCOLDB11:
	.text
.LHOTB11:
	.p2align 4,,15
	.type	mergesort_internal.part.0, @function
mergesort_internal.part.0:
.LFB72:
	.cfi_startproc
	pushq	%r15	#
	.cfi_def_cfa_offset 16
	.cfi_offset 15, -16
	pushq	%r14	#
	.cfi_def_cfa_offset 24
	.cfi_offset 14, -24
	movq	%rcx, %r14	# cmp, cmp
	pushq	%r13	#
	.cfi_def_cfa_offset 32
	.cfi_offset 13, -32
	movq	%rdx, %r13	# len, mid
	pushq	%r12	#
	.cfi_def_cfa_offset 40
	.cfi_offset 12, -40
	pushq	%rbp	#
	.cfi_def_cfa_offset 48
	.cfi_offset 6, -48
	pushq	%rbx	#
	.cfi_def_cfa_offset 56
	.cfi_offset 3, -56
	shrq	%r13	# mid
	movq	%rdi, %rbp	# arr, arr
	movq	%rdx, %rbx	# len, len
	movq	%r13, %rdx	# mid,
	subq	$24, %rsp	#,
	.cfi_def_cfa_offset 80
	movq	%rsi, %r12	# tmp, tmp
	leaq	0(%rbp,%r13,8), %r15	#, D.6501
	call	mergesort_internal	#
	movq	%rbx, %r8	# len, D.6500
	movq	%r14, %rcx	# cmp,
	movq	%r12, %rsi	# tmp,
	subq	%r13, %r8	# mid, D.6500
	movq	%r15, %rdi	# D.6501,
	movq	%r8, %rdx	# D.6500,
	movq	%r8, 8(%rsp)	# D.6500, %sfp
	call	mergesort_internal	#
	movq	8(%rsp), %r8	# %sfp, D.6500
	movq	%r12, %rdx	# tmp,
	movq	%r15, %rsi	# D.6501,
	movq	%rbp, %rdi	# arr,
	movq	%r14, %r9	# cmp,
	movq	%r13, %rcx	# mid,
	call	merge	#
	addq	$24, %rsp	#,
	.cfi_def_cfa_offset 56
	leaq	0(,%rbx,8), %rdx	#, D.6500
	movq	%r12, %rsi	# tmp,
	popq	%rbx	#
	.cfi_def_cfa_offset 48
	movq	%rbp, %rdi	# arr,
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
	jmp	memcpy	#
	.cfi_endproc
.LFE72:
	.size	mergesort_internal.part.0, .-mergesort_internal.part.0
	.section	.text.unlikely
.LCOLDE11:
	.text
.LHOTE11:
	.section	.text.unlikely
.LCOLDB12:
	.text
.LHOTB12:
	.p2align 4,,15
	.globl	mergesort_generic
	.type	mergesort_generic, @function
mergesort_generic:
.LFB62:
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
	leaq	0(,%rsi,8), %r12	#, D.6512
	pushq	%rbp	#
	.cfi_def_cfa_offset 40
	.cfi_offset 6, -40
	pushq	%rbx	#
	.cfi_def_cfa_offset 48
	.cfi_offset 3, -48
	movq	%rsi, %rbp	# len, len
	movl	$1, %esi	#,
	movq	%r12, %rdi	# D.6512,
	movq	%rdx, %r14	# cmp, cmp
	call	calloc	#
	testq	%rax, %rax	# tmp
	movq	%rax, %rbx	#, tmp
	jne	.L81	#,
	testq	%r12, %r12	# D.6512
	je	.L81	#,
	call	oom_fun	#
.L81:
	cmpq	$1, %rbp	#, len
	jbe	.L82	#,
	movq	%r14, %rcx	# cmp,
	movq	%rbp, %rdx	# len,
	movq	%rbx, %rsi	# tmp,
	movq	%r13, %rdi	# arr,
	call	mergesort_internal.part.0	#
.L82:
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
.LFE62:
	.size	mergesort_generic, .-mergesort_generic
	.section	.text.unlikely
.LCOLDE12:
	.text
.LHOTE12:
	.section	.text.unlikely
.LCOLDB13:
	.text
.LHOTB13:
	.p2align 4,,15
	.globl	qsort_u64
	.type	qsort_u64, @function
qsort_u64:
.LFB63:
	.cfi_startproc
	pushq	%rbx	#
	.cfi_def_cfa_offset 16
	.cfi_offset 3, -16
	movl	$cmp_lt, %edx	#,
	movq	%rdi, %rbx	# input, input
	call	qsort_generic	#
	movq	%rbx, %rax	# input,
	popq	%rbx	#
	.cfi_def_cfa_offset 8
	ret
	.cfi_endproc
.LFE63:
	.size	qsort_u64, .-qsort_u64
	.section	.text.unlikely
.LCOLDE13:
	.text
.LHOTE13:
	.section	.text.unlikely
.LCOLDB14:
	.text
.LHOTB14:
	.p2align 4,,15
	.globl	insertion_sort_u64
	.type	insertion_sort_u64, @function
insertion_sort_u64:
.LFB64:
	.cfi_startproc
	pushq	%rbx	#
	.cfi_def_cfa_offset 16
	.cfi_offset 3, -16
	movl	$cmp_lt, %edx	#,
	movq	%rdi, %rbx	# input, input
	call	insertion_sort_generic	#
	movq	%rbx, %rax	# input,
	popq	%rbx	#
	.cfi_def_cfa_offset 8
	ret
	.cfi_endproc
.LFE64:
	.size	insertion_sort_u64, .-insertion_sort_u64
	.section	.text.unlikely
.LCOLDE14:
	.text
.LHOTE14:
	.section	.text.unlikely
.LCOLDB15:
	.text
.LHOTB15:
	.p2align 4,,15
	.globl	mergesort_u64
	.type	mergesort_u64, @function
mergesort_u64:
.LFB65:
	.cfi_startproc
	pushq	%rbx	#
	.cfi_def_cfa_offset 16
	.cfi_offset 3, -16
	movl	$cmp_lt, %edx	#,
	movq	%rdi, %rbx	# input, input
	call	mergesort_generic	#
	movq	%rbx, %rax	# input,
	popq	%rbx	#
	.cfi_def_cfa_offset 8
	ret
	.cfi_endproc
.LFE65:
	.size	mergesort_u64, .-mergesort_u64
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
.LFB66:
	.cfi_startproc
	pushq	%r14	#
	.cfi_def_cfa_offset 16
	.cfi_offset 14, -16
	pushq	%r13	#
	.cfi_def_cfa_offset 24
	.cfi_offset 13, -24
	xorl	%eax, %eax	# tmp193
	pushq	%r12	#
	.cfi_def_cfa_offset 32
	.cfi_offset 12, -32
	pushq	%rbp	#
	.cfi_def_cfa_offset 40
	.cfi_offset 6, -40
	movl	$8, %ecx	#, tmp194
	pushq	%rbx	#
	.cfi_def_cfa_offset 48
	.cfi_offset 3, -48
	movq	%rdi, %rbx	# in, in
	movq	%rsi, %rbp	# sz, sz
	movl	$16320, %edx	#,
	xorl	%esi, %esi	#
	subq	$64, %rsp	#,
	.cfi_def_cfa_offset 112
	movq	%rsp, %rdi	# ivtmp.149, tmp192
	movq	%rsp, %r12	#, ivtmp.149
	rep stosq
	movl	$hist.5963, %edi	#,
	call	memset	#
	testq	%rbp, %rbp	# sz
	je	.L108	#,
	leaq	(%rbx,%rbp,8), %rcx	#, D.6588
	movq	%rbx, %rax	# in, ivtmp.178
	.p2align 4,,10
	.p2align 3
.L98:
	movzbl	(%rax), %edx	# MEM[base: _95, offset: 0B], D.6590
	addq	$8, %rax	#, ivtmp.178
	addq	$1, hist.5963(,%rdx,8)	#, hist
	movq	-8(%rax), %rdx	# MEM[base: _95, offset: 0B], MEM[base: _95, offset: 0B]
	movzbl	%dh, %edx	# MEM[base: _95, offset: 0B], D.6590
	addq	$1, hist.5963+2040(,%rdx,8)	#, hist
	movzbl	-6(%rax), %edx	# MEM[base: _95, offset: 0B], D.6590
	addq	$1, hist.5963+4080(,%rdx,8)	#, hist
	movzbl	-5(%rax), %edx	# MEM[base: _95, offset: 0B], D.6590
	addq	$1, hist.5963+6120(,%rdx,8)	#, hist
	movzbl	-4(%rax), %edx	# MEM[base: _95, offset: 0B], D.6590
	addq	$1, hist.5963+8160(,%rdx,8)	#, hist
	movzbl	-3(%rax), %edx	# MEM[base: _95, offset: 0B], D.6590
	addq	$1, hist.5963+10200(,%rdx,8)	#, hist
	movzbl	-2(%rax), %edx	# MEM[base: _95, offset: 0B], D.6590
	addq	$1, hist.5963+12240(,%rdx,8)	#, hist
	movq	-8(%rax), %rdx	# MEM[base: _95, offset: 0B], D.6590
	shrq	$56, %rdx	#, D.6590
	addq	$1, hist.5963+14280(,%rdx,8)	#, hist
	cmpq	%rcx, %rax	# D.6588, ivtmp.178
	jne	.L98	#,
	movq	hist.5963(%rip), %rsi	# hist, D.6593
.L97:
	leaq	64(%rsp), %rdi	#, D.6588
	movl	$hist.5963, %r8d	#, ivtmp.173
	movl	$hist.5963+2040, %r9d	#, D.6588
	xorl	%eax, %eax	# D.6593
	.p2align 4,,10
	.p2align 3
.L101:
	movq	%r12, %rdx	# ivtmp.149, ivtmp.149
	movq	%r8, %rcx	# ivtmp.173, ivtmp.145
	jmp	.L102	#
	.p2align 4,,10
	.p2align 3
.L99:
	movq	(%rcx), %rsi	# MEM[base: _39, offset: 0B], D.6593
	movq	(%rdx), %rax	# MEM[base: _12, offset: 0B], D.6593
.L102:
	movq	%rax, (%rcx)	# D.6593, MEM[base: _113, offset: 0B]
	addq	%rsi, %rax	# D.6593, tmp278
	addq	$8, %rdx	#, ivtmp.149
	movq	%rax, -8(%rdx)	# tmp278, MEM[base: _107, offset: 0B]
	addq	$2040, %rcx	#, ivtmp.145
	cmpq	%rdx, %rdi	# ivtmp.149, D.6588
	jne	.L99	#,
	addq	$8, %r8	#, ivtmp.173
	cmpq	%r8, %r9	# ivtmp.173, D.6588
	je	.L100	#,
	movq	(%r8), %rsi	# MEM[base: _75, offset: 0B], D.6593
	movq	(%rsp), %rax	# sum, D.6593
	jmp	.L101	#
.L100:
	leaq	0(,%rbp,8), %r12	#, D.6590
	movl	$1, %esi	#,
	movq	%r12, %rdi	# D.6590,
	call	calloc	#
	testq	%rax, %rax	# in
	movq	%rax, %r13	#, in
	jne	.L103	#,
	testq	%r12, %r12	# D.6590
	je	.L103	#,
	call	oom_fun	#
.L103:
	movq	%r13, %r10	# in, in
	movq	%rbx, %rax	# in, in
	xorl	%ecx, %ecx	# ivtmp.132
	xorl	%edi, %edi	# i
	.p2align 4,,10
	.p2align 3
.L104:
	testq	%rbp, %rbp	# sz
	je	.L107	#,
	movq	%rdi, %r9	# i, tmp301
	leaq	(%rax,%r12), %r11	#, D.6588
	movq	%rax, %rsi	# in, ivtmp.124
	salq	$8, %r9	#, tmp301
	subq	%rdi, %r9	# i, tmp302
	.p2align 4,,10
	.p2align 3
.L105:
	movq	(%rsi), %rdx	# MEM[base: _125, offset: 0B], D.6590
	addq	$8, %rsi	#, ivtmp.124
	shrq	%cl, %rdx	# ivtmp.132, D.6590
	movzbl	%dl, %edx	# D.6590, pos
	addq	%r9, %rdx	# tmp302, tmp291
	movq	hist.5963(,%rdx,8), %r8	# hist, D.6591
	leaq	1(%r8), %r14	#, tmp297
	movq	%r14, hist.5963(,%rdx,8)	# tmp297, hist
	movq	-8(%rsi), %rdx	# MEM[base: _125, offset: 0B], D.6590
	cmpq	%rsi, %r11	# ivtmp.124, D.6588
	movq	%rdx, (%r10,%r8,8)	# D.6590, *_92
	jne	.L105	#,
.L107:
	addq	$1, %rdi	#, i
	addl	$8, %ecx	#, ivtmp.132
	movq	%r10, %rdx	# in, in
	cmpq	$8, %rdi	#, i
	movq	%rax, %r10	# in, in
	je	.L106	#,
	movq	%rdx, %rax	# in, in
	jmp	.L104	#
.L106:
	movq	%r13, %rdi	# in,
	call	free	#
	addq	$64, %rsp	#,
	.cfi_remember_state
	.cfi_def_cfa_offset 48
	movq	%rbx, %rax	# in,
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
	ret
.L108:
	.cfi_restore_state
	xorl	%esi, %esi	# D.6593
	jmp	.L97	#
	.cfi_endproc
.LFE66:
	.size	radix_sort_u64, .-radix_sort_u64
	.section	.text.unlikely
.LCOLDE16:
	.text
.LHOTE16:
	.section	.text.unlikely
.LCOLDB17:
	.text
.LHOTB17:
	.p2align 4,,15
	.globl	strtoul_checked
	.type	strtoul_checked, @function
strtoul_checked:
.LFB67:
	.cfi_startproc
	pushq	%r13	#
	.cfi_def_cfa_offset 16
	.cfi_offset 13, -16
	pushq	%r12	#
	.cfi_def_cfa_offset 24
	.cfi_offset 12, -24
	movq	%rdx, %r12	# endptr, endptr
	pushq	%rbp	#
	.cfi_def_cfa_offset 32
	.cfi_offset 6, -32
	pushq	%rbx	#
	.cfi_def_cfa_offset 40
	.cfi_offset 3, -40
	movq	%rdi, %rbp	# str, str
	movq	%rsi, %r13	# ret, ret
	subq	$8, %rsp	#,
	.cfi_def_cfa_offset 48
	call	__errno_location	#
	xorl	%edx, %edx	#
	movq	%rax, %rbx	#, D.6599
	movl	$0, (%rax)	#, *_3
	movq	%r12, %rsi	# endptr,
	movq	%rbp, %rdi	# str,
	call	strtoul	#
	movl	(%rbx), %edx	# *_3, D.6600
	testl	%edx, %edx	# D.6600
	jne	.L122	#,
	movq	%rax, 0(%r13)	# temp, *ret_11(D)
.L122:
	addq	$8, %rsp	#,
	.cfi_def_cfa_offset 40
	movl	%edx, %eax	# D.6600,
	popq	%rbx	#
	.cfi_def_cfa_offset 32
	popq	%rbp	#
	.cfi_def_cfa_offset 24
	popq	%r12	#
	.cfi_def_cfa_offset 16
	popq	%r13	#
	.cfi_def_cfa_offset 8
	ret
	.cfi_endproc
.LFE67:
	.size	strtoul_checked, .-strtoul_checked
	.section	.text.unlikely
.LCOLDE17:
	.text
.LHOTE17:
	.section	.text.unlikely
.LCOLDB18:
	.text
.LHOTB18:
	.p2align 4,,15
	.globl	read_arr
	.type	read_arr, @function
read_arr:
.LFB68:
	.cfi_startproc
	pushq	%r14	#
	.cfi_def_cfa_offset 16
	.cfi_offset 14, -16
	pushq	%r13	#
	.cfi_def_cfa_offset 24
	.cfi_offset 13, -24
	pushq	%r12	#
	.cfi_def_cfa_offset 32
	.cfi_offset 12, -32
	pushq	%rbp	#
	.cfi_def_cfa_offset 40
	.cfi_offset 6, -40
	movq	%rsi, %rbp	# str, str
	pushq	%rbx	#
	.cfi_def_cfa_offset 48
	.cfi_offset 3, -48
	movslq	%edi, %rbx	# len,
	movl	$1, %esi	#,
	movq	%rbx, %r13	#,
	salq	$3, %rbx	#, D.6619
	subq	$16, %rsp	#,
	.cfi_def_cfa_offset 64
	movq	%rbx, %rdi	# D.6619,
	call	calloc	#
	testq	%rax, %rax	# arr
	movq	%rax, %r14	#, arr
	jne	.L125	#,
	testq	%rbx, %rbx	# D.6619
	je	.L125	#,
	call	oom_fun	#
.L125:
	testl	%r13d, %r13d	# len
	movq	%rbp, 8(%rsp)	# str, strptr
	jle	.L130	#,
	call	__errno_location	#
	movq	%rax, %r12	#, D.6623
	leal	-1(%r13), %eax	#, D.6620
	movq	%r14, %rbx	# arr, ivtmp.190
	movq	%rbp, %rdi	# str, D.6626
	leaq	8(%r14,%rax,8), %r13	#, D.6627
	jmp	.L128	#
	.p2align 4,,10
	.p2align 3
.L138:
	addq	$8, %rbx	#, ivtmp.190
	cmpq	%r13, %rbx	# D.6627, ivtmp.190
	je	.L130	#,
	movq	8(%rsp), %rdi	# strptr, D.6626
.L128:
	leaq	8(%rsp), %rsi	#, tmp124
	xorl	%edx, %edx	#
	movl	$0, (%r12)	#, *_32
	call	strtoul	#
	movq	%rax, (%rbx)	# D.6619, MEM[base: _37, offset: 0B]
	movl	(%r12), %eax	# *_32,
	testl	%eax, %eax	#
	je	.L138	#,
.L127:
	movq	%r14, %rdi	# arr,
	call	free	#
	addq	$16, %rsp	#,
	.cfi_remember_state
	.cfi_def_cfa_offset 48
	xorl	%eax, %eax	# D.6618
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
	ret
	.p2align 4,,10
	.p2align 3
.L130:
	.cfi_restore_state
	addq	$16, %rsp	#,
	.cfi_def_cfa_offset 48
	movq	%r14, %rax	# arr, D.6618
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
	ret
	.cfi_endproc
.LFE68:
	.size	read_arr, .-read_arr
	.section	.text.unlikely
.LCOLDE18:
	.text
.LHOTE18:
	.section	.rodata.str1.1
.LC19:
	.string	"%lu"
.LC20:
	.string	" %lu"
	.section	.text.unlikely
.LCOLDB21:
	.text
.LHOTB21:
	.p2align 4,,15
	.globl	print_arr
	.type	print_arr, @function
print_arr:
.LFB69:
	.cfi_startproc
	pushq	%r13	#
	.cfi_def_cfa_offset 16
	.cfi_offset 13, -16
	pushq	%r12	#
	.cfi_def_cfa_offset 24
	.cfi_offset 12, -24
	movl	%esi, %r13d	# len, len
	pushq	%rbp	#
	.cfi_def_cfa_offset 32
	.cfi_offset 6, -32
	pushq	%rbx	#
	.cfi_def_cfa_offset 40
	.cfi_offset 3, -40
	movq	%rdx, %rbp	# out, out
	xorl	%eax, %eax	#
	movq	%rdi, %r12	# arr, arr
	movl	$.LC19, %esi	#,
	subq	$8, %rsp	#,
	.cfi_def_cfa_offset 48
	movq	(%rdi), %rdx	# *arr_4(D),
	movq	%rbp, %rdi	# out,
	call	fprintf	#
	cmpl	$1, %r13d	#, len
	jle	.L142	#,
	leal	-2(%r13), %eax	#, D.6641
	leaq	8(%r12), %rbx	#, ivtmp.200
	leaq	16(%r12,%rax,8), %r12	#, D.6642
	.p2align 4,,10
	.p2align 3
.L141:
	movq	(%rbx), %rdx	# MEM[base: _22, offset: 0B],
	xorl	%eax, %eax	#
	movl	$.LC20, %esi	#,
	movq	%rbp, %rdi	# out,
	addq	$8, %rbx	#, ivtmp.200
	call	fprintf	#
	cmpq	%r12, %rbx	# D.6642, ivtmp.200
	jne	.L141	#,
.L142:
	addq	$8, %rsp	#,
	.cfi_def_cfa_offset 40
	movq	%rbp, %rsi	# out,
	movl	$10, %edi	#,
	popq	%rbx	#
	.cfi_def_cfa_offset 32
	popq	%rbp	#
	.cfi_def_cfa_offset 24
	popq	%r12	#
	.cfi_def_cfa_offset 16
	popq	%r13	#
	.cfi_def_cfa_offset 8
	jmp	fputc	#
	.cfi_endproc
.LFE69:
	.size	print_arr, .-print_arr
	.section	.text.unlikely
.LCOLDE21:
	.text
.LHOTE21:
	.section	.rodata.str1.8,"aMS",@progbits,1
	.align 8
.LC22:
	.string	"Usage: ./sort method array_length array"
	.section	.rodata.str1.1
.LC23:
	.string	"quick"
.LC24:
	.string	"merge"
.LC25:
	.string	"insertion"
.LC26:
	.string	"radix"
	.section	.rodata.str1.8
	.align 8
.LC27:
	.string	"Uknown sorting method %s, using default\n"
	.section	.rodata.str1.1
.LC28:
	.string	"strtoul"
.LC29:
	.string	"Array was sorted"
.LC30:
	.string	"Failed to sort array"
	.section	.text.unlikely
.LCOLDB31:
	.section	.text.startup,"ax",@progbits
.LHOTB31:
	.p2align 4,,15
	.globl	main
	.type	main, @function
main:
.LFB71:
	.cfi_startproc
	pushq	%r12	#
	.cfi_def_cfa_offset 16
	.cfi_offset 12, -16
	pushq	%rbp	#
	.cfi_def_cfa_offset 24
	.cfi_offset 6, -24
	pushq	%rbx	#
	.cfi_def_cfa_offset 32
	.cfi_offset 3, -32
	subq	$16, %rsp	#,
	.cfi_def_cfa_offset 48
	cmpl	$2, %edi	#, argc
	jle	.L174	#,
	xorl	%ebp, %ebp	# method
	cmpl	$4, %edi	#, argc
	movq	%rsi, %rbx	# argv, argv
	je	.L175	#,
.L147:
	movq	8(%rbx), %rdi	# MEM[(char * *)argv_1 + 8B], MEM[(char * *)argv_1 + 8B]
	leaq	8(%rsp), %rsi	#, tmp130
	xorl	%edx, %edx	#
	call	strtoul_checked	#
	testl	%eax, %eax	# err
	jne	.L150	#,
	movq	16(%rbx), %rsi	# MEM[(char * *)argv_1 + 16B], MEM[(char * *)argv_1 + 16B]
	movl	8(%rsp), %edi	# len,
	call	read_arr	#
	testq	%rax, %rax	# arr
	movq	%rax, %rbx	#, arr
	je	.L150	#,
	cmpl	$2, %ebp	#, method
	je	.L152	#,
	cmpl	$3, %ebp	#, method
	je	.L153	#,
	subl	$1, %ebp	#, method
	movl	$cmp_lt, %edx	#,
	je	.L154	#,
	movl	8(%rsp), %esi	# len,
	movq	%rax, %rdi	# arr,
	call	qsort_generic	#
.L155:
	movq	8(%rsp), %rcx	# len, D.6657
	subq	$1, %rcx	#, D.6657
	je	.L156	#,
	movq	8(%rbx), %rax	# MEM[(uint64_t *)arr_30 + 8B], tmp132
	cmpq	%rax, (%rbx)	# tmp132, MEM[(uint64_t *)arr_30]
	ja	.L157	#,
	movl	$1, %eax	#, ivtmp.208
	jmp	.L158	#
	.p2align 4,,10
	.p2align 3
.L159:
	movq	(%rbx,%rax,8), %rdx	# MEM[base: arr_30, index: ivtmp.208_69, step: 8, offset: 0B], D.6657
	addq	$1, %rax	#, ivtmp.208
	cmpq	(%rbx,%rax,8), %rdx	# MEM[base: arr_30, index: ivtmp.208_68, step: 8, offset: 0B], D.6657
	ja	.L157	#,
.L158:
	cmpq	%rax, %rcx	# ivtmp.208, D.6657
	jne	.L159	#,
.L156:
	movl	$.LC29, %edi	#,
	call	puts	#
.L160:
	movl	8(%rsp), %esi	# len,
	movq	stdout(%rip), %rdx	# stdout,
	movq	%rbx, %rdi	# arr,
	call	print_arr	#
	movq	%rbx, %rdi	# arr,
	call	free	#
	addq	$16, %rsp	#,
	.cfi_remember_state
	.cfi_def_cfa_offset 32
	xorl	%eax, %eax	#
	popq	%rbx	#
	.cfi_def_cfa_offset 24
	popq	%rbp	#
	.cfi_def_cfa_offset 16
	popq	%r12	#
	.cfi_def_cfa_offset 8
	ret
.L154:
	.cfi_restore_state
	movq	8(%rsp), %rsi	# len,
	movq	%rax, %rdi	# arr,
	call	mergesort_generic	#
	jmp	.L155	#
.L157:
	movl	$.LC30, %edi	#,
	call	puts	#
	jmp	.L160	#
.L175:
	movq	8(%rsi), %r12	# MEM[(char * *)argv_9(D) + 8B], D.6655
	movl	$.LC23, %esi	#,
	movq	%r12, %rdi	# D.6655,
	call	is_prefix	#
	testl	%eax, %eax	# D.6656
	jne	.L148	#,
	movl	$.LC24, %esi	#,
	movq	%r12, %rdi	# D.6655,
	movl	$1, %ebp	#, method
	call	is_prefix	#
	testl	%eax, %eax	# D.6656
	jne	.L148	#,
	movl	$.LC25, %esi	#,
	movq	%r12, %rdi	# D.6655,
	movl	$2, %ebp	#, method
	call	is_prefix	#
	testl	%eax, %eax	# D.6656
	jne	.L148	#,
	movl	$.LC26, %esi	#,
	movq	%r12, %rdi	# D.6655,
	movl	$3, %ebp	#, method
	call	is_prefix	#
	testl	%eax, %eax	# D.6656
	jne	.L148	#,
	movq	stderr(%rip), %rdi	# stderr,
	movq	%r12, %rdx	# D.6655,
	movl	$.LC27, %esi	#,
	xorl	%ebp, %ebp	# method
	call	fprintf	#
.L148:
	addq	$8, %rbx	#, argv
	jmp	.L147	#
.L153:
	movq	8(%rsp), %rsi	# len,
	movq	%rax, %rdi	# arr,
	call	radix_sort_u64	#
	jmp	.L155	#
.L152:
	movq	8(%rsp), %rsi	# len,
	movl	$cmp_lt, %edx	#,
	movq	%rax, %rdi	# arr,
	call	insertion_sort_generic	#
	jmp	.L155	#
.L174:
	movl	$.LC22, %edi	#,
	call	puts	#
	movl	$1, %edi	#,
	call	exit	#
.L150:
	movl	$.LC28, %edi	#,
	call	perror	#
	movl	$1, %edi	#,
	call	exit	#
	.cfi_endproc
.LFE71:
	.size	main, .-main
	.section	.text.unlikely
.LCOLDE31:
	.section	.text.startup
.LHOTE31:
	.local	hist.5963
	.comm	hist.5963,16320,32
	.ident	"GCC: (GNU) 5.3.0"
	.section	.note.GNU-stack,"",@progbits
