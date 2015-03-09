	.file	"postprocess.c"
# GNU C (GCC) version 4.9.2 20141224 (prerelease) (x86_64-unknown-linux-gnu)
#	compiled by GNU C version 4.9.2 20141224 (prerelease), GMP version 6.0.0, MPFR version 3.1.2-p11, MPC version 1.0.2
# GGC heuristics: --param ggc-min-expand=100 --param ggc-min-heapsize=131072
# options passed:  -I /home/tucker/Repo/ffmpeg/libpostproc/.. postprocess.c
# -mtune=generic -march=x86-64 -auxbase-strip postprocess_asm.s -O1
# -fverbose-asm
# options enabled:  -faggressive-loop-optimizations
# -fasynchronous-unwind-tables -fauto-inc-dec -fbranch-count-reg
# -fcombine-stack-adjustments -fcommon -fcompare-elim -fcprop-registers
# -fdefer-pop -fdelete-null-pointer-checks -fdwarf2-cfi-asm
# -fearly-inlining -feliminate-unused-debug-types -fforward-propagate
# -ffunction-cse -fgcse-lm -fgnu-runtime -fgnu-unique
# -fguess-branch-probability -fident -fif-conversion -fif-conversion2
# -finline -finline-atomics -finline-functions-called-once -fipa-profile
# -fipa-pure-const -fipa-reference -fira-hoist-pressure
# -fira-share-save-slots -fira-share-spill-slots -fivopts
# -fkeep-static-consts -fleading-underscore -fmath-errno -fmerge-constants
# -fmerge-debug-strings -fmove-loop-invariants -fomit-frame-pointer
# -fpeephole -fprefetch-loop-arrays -freg-struct-return
# -fsched-critical-path-heuristic -fsched-dep-count-heuristic
# -fsched-group-heuristic -fsched-interblock -fsched-last-insn-heuristic
# -fsched-rank-heuristic -fsched-spec -fsched-spec-insn-heuristic
# -fsched-stalled-insns-dep -fshow-column -fshrink-wrap -fsigned-zeros
# -fsplit-ivs-in-unroller -fsplit-wide-types -fstrict-volatile-bitfields
# -fsync-libcalls -ftoplevel-reorder -ftrapping-math -ftree-bit-ccp
# -ftree-ccp -ftree-ch -ftree-coalesce-vars -ftree-copy-prop
# -ftree-copyrename -ftree-cselim -ftree-dce -ftree-dominator-opts
# -ftree-dse -ftree-forwprop -ftree-fre -ftree-loop-if-convert
# -ftree-loop-im -ftree-loop-ivcanon -ftree-loop-optimize
# -ftree-parallelize-loops= -ftree-phiprop -ftree-pta -ftree-reassoc
# -ftree-scev-cprop -ftree-sink -ftree-slsr -ftree-sra -ftree-ter
# -funit-at-a-time -funwind-tables -fverbose-asm -fzero-initialized-in-bss
# -m128bit-long-double -m64 -m80387 -malign-stringops
# -mavx256-split-unaligned-load -mavx256-split-unaligned-store
# -mfancy-math-387 -mfp-ret-in-387 -mfxsr -mglibc -mieee-fp
# -mlong-double-80 -mmmx -mno-sse4 -mpush-args -mred-zone -msse -msse2
# -mtls-direct-seg-refs

	.text
	.type	deInterlaceInterpolateCubic_C, @function
deInterlaceInterpolateCubic_C:
.LFB95:
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
	leal	(%rsi,%rsi), %ebx	#, tmp249
	leal	(%rbx,%rsi), %ebp	#, D.12911
	movslq	%ebp, %r14	# D.12911, D.12912
	addq	%r14, %rdi	# D.12912, src
	movslq	%ebx, %rbx	# tmp249, D.12912
	leal	0(,%rsi,4), %r9d	#, D.12911
	movslq	%r9d, %r11	# D.12911, D.12912
	leal	(%rbp,%rbp), %r12d	#, tmp256
	movslq	%r12d, %r12	# tmp256, D.12912
	addl	%esi, %r9d	# stride, D.12911
	movslq	%r9d, %r15	# D.12911, D.12912
	leal	0(,%rsi,8), %eax	#, D.12911
	movslq	%eax, %r10	# D.12911, D.12912
	movl	%eax, %r13d	# D.12911, D.12911
	subl	%esi, %r13d	# stride, D.12911
	movslq	%r13d, %r13	# D.12911, D.12912
	addl	%r9d, %r9d	# tmp267
	movslq	%r9d, %r9	# tmp267, D.12912
	addl	%eax, %esi	# D.12911, D.12911
	movslq	%esi, %rsi	# D.12911, D.12912
	sall	$2, %ebp	#, tmp274
	movslq	%ebp, %rbp	# tmp274, D.12912
	leaq	(%rdi,%r12), %r8	#, ivtmp.257
	leaq	8(%rdi,%r12), %rax	#, D.12917
	movq	%rax, -16(%rsp)	# D.12917, %sfp
	movq	%r12, %rax	# D.12912, D.12912
	negq	%rax	# D.12912
	movq	%rax, -8(%rsp)	# D.12912, %sfp
.L10:
	movq	%r8, %rax	# ivtmp.257, D.12916
	subq	%r12, %rax	# D.12912, D.12916
	movq	%r8, %rcx	# ivtmp.257, D.12916
	movzbl	(%rax,%rbx), %edx	# MEM[base: _133, index: _12, offset: 0B], D.12911
	leal	(%rdx,%rdx,8), %edx	#, D.12911
	movq	-8(%rsp), %rdi	# %sfp, D.12912
	movzbl	(%r8,%rdi), %edi	# MEM[base: _135, index: _137, offset: 0B], D.12911
	subl	%edi, %edx	# D.12911, D.12911
	movzbl	(%rax,%r11), %edi	# MEM[base: _133, index: _21, offset: 0B], D.12911
	leal	(%rdi,%rdi,8), %edi	#, D.12911
	addl	%edi, %edx	# D.12911, D.12911
	movzbl	(%r8), %edi	# MEM[base: _135, offset: 0B], D.12911
	subl	%edi, %edx	# D.12911, D.12911
	sarl	$4, %edx	#, D.12911
	movl	%edx, %edi	# D.12911, D.12914
	testl	$-256, %edx	#, D.12911
	je	.L3	#,
	negl	%edx	# D.12911
	sarl	$31, %edx	#, tmp358
	movl	%edx, %edi	# tmp358, D.12914
.L3:
	movb	%dil, (%rax,%r14)	# D.12914, MEM[base: _133, index: _6, offset: 0B]
	movzbl	(%rax,%r11), %edx	# MEM[base: _133, index: _21, offset: 0B], D.12911
	leal	(%rdx,%rdx,8), %edx	#, D.12911
	movzbl	(%rax,%rbx), %edi	# MEM[base: _133, index: _12, offset: 0B], D.12911
	subl	%edi, %edx	# D.12911, D.12911
	movzbl	(%rcx), %edi	# MEM[base: _135, offset: 0B], D.12911
	leal	(%rdi,%rdi,8), %edi	#, D.12911
	addl	%edi, %edx	# D.12911, D.12911
	movzbl	(%rax,%r10), %edi	# MEM[base: _133, index: _49, offset: 0B], D.12911
	subl	%edi, %edx	# D.12911, D.12911
	sarl	$4, %edx	#, D.12911
	movl	%edx, %edi	# D.12911, D.12914
	testl	$-256, %edx	#, D.12911
	je	.L5	#,
	negl	%edx	# D.12911
	sarl	$31, %edx	#, tmp359
	movl	%edx, %edi	# tmp359, D.12914
.L5:
	movb	%dil, (%rax,%r15)	# D.12914, MEM[base: _133, index: _36, offset: 0B]
	movzbl	(%rcx), %edx	# MEM[base: _135, offset: 0B], D.12911
	leal	(%rdx,%rdx,8), %edx	#, D.12911
	movzbl	(%rax,%r11), %edi	# MEM[base: _133, index: _21, offset: 0B], D.12911
	subl	%edi, %edx	# D.12911, D.12911
	movzbl	(%rax,%r10), %edi	# MEM[base: _133, index: _49, offset: 0B], D.12911
	leal	(%rdi,%rdi,8), %edi	#, D.12911
	addl	%edi, %edx	# D.12911, D.12911
	movzbl	(%rax,%r9), %edi	# MEM[base: _133, index: _70, offset: 0B], D.12911
	subl	%edi, %edx	# D.12911, D.12911
	sarl	$4, %edx	#, D.12911
	movl	%edx, %edi	# D.12911, D.12914
	testl	$-256, %edx	#, D.12911
	je	.L7	#,
	negl	%edx	# D.12911
	sarl	$31, %edx	#, tmp360
	movl	%edx, %edi	# tmp360, D.12914
.L7:
	movb	%dil, (%rax,%r13)	# D.12914, MEM[base: _133, index: _57, offset: 0B]
	movzbl	(%rax,%r10), %edx	# MEM[base: _133, index: _49, offset: 0B], D.12911
	leal	(%rdx,%rdx,8), %edx	#, D.12911
	movzbl	(%rcx), %ecx	# MEM[base: _135, offset: 0B], D.12911
	subl	%ecx, %edx	# D.12911, D.12911
	movzbl	(%rax,%r9), %ecx	# MEM[base: _133, index: _70, offset: 0B], D.12911
	leal	(%rcx,%rcx,8), %ecx	#, D.12911
	addl	%ecx, %edx	# D.12911, D.12911
	movzbl	(%rax,%rbp), %ecx	# MEM[base: _133, index: _91, offset: 0B], D.12911
	subl	%ecx, %edx	# D.12911, D.12911
	sarl	$4, %edx	#, D.12911
	movl	%edx, %ecx	# D.12911, D.12914
	testl	$-256, %edx	#, D.12911
	je	.L9	#,
	negl	%edx	# D.12911
	sarl	$31, %edx	#, tmp361
	movl	%edx, %ecx	# tmp361, D.12914
.L9:
	movb	%cl, (%rax,%rsi)	# D.12914, MEM[base: _133, index: _78, offset: 0B]
	addq	$1, %r8	#, ivtmp.257
	cmpq	-16(%rsp), %r8	# %sfp, ivtmp.257
	jne	.L10	#,
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
.LFE95:
	.size	deInterlaceInterpolateCubic_C, .-deInterlaceInterpolateCubic_C
	.type	deInterlaceFF_C, @function
deInterlaceFF_C:
.LFB96:
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
	leal	0(,%rsi,4), %r9d	#, D.12947
	movslq	%r9d, %rbp	# D.12947, D.12948
	addq	%rbp, %rdi	# D.12948, src
	leal	(%rsi,%rsi), %r8d	#, D.12947
	movslq	%r8d, %r10	# D.12947, D.12948
	addl	%esi, %r8d	# stride, D.12947
	movslq	%r8d, %rcx	# D.12947, D.12948
	addl	%esi, %r9d	# stride, D.12947
	movslq	%r9d, %r9	# D.12947, D.12948
	addl	%r8d, %r8d	# tmp254
	movslq	%r8d, %r8	# tmp254, D.12948
	leal	0(,%rsi,8), %r13d	#, tmp256
	movl	%r13d, %ebx	# tmp256, D.12947
	subl	%esi, %ebx	# stride, D.12947
	movslq	%ebx, %rbx	# D.12947, D.12948
	movslq	%r13d, %r15	# tmp256, D.12948
	addl	%esi, %r13d	# stride, D.12947
	movslq	%r13d, %r13	# D.12947, D.12948
	movl	$0, %eax	#, ivtmp.268
	movslq	%esi, %rsi	# stride, D.12948
	leaq	(%rdi,%rsi), %r12	#, D.12951
	leaq	(%r12,%rsi), %r14	#, D.12951
	addq	%r14, %rsi	# D.12951, D.12951
	movq	%rsi, -24(%rsp)	# D.12951, %sfp
	addq	%rdi, %rbp	# src, D.12951
	addq	%rdi, %r9	# src, D.12951
	leaq	(%rdi,%r10), %rsi	#, D.12951
	movq	%rsi, -16(%rsp)	# D.12951, %sfp
	leaq	(%rdi,%rcx), %rsi	#, D.12951
	movq	%rsi, -8(%rsp)	# D.12951, %sfp
	addq	%rdi, %r8	# src, D.12951
	addq	%rdi, %rbx	# src, D.12951
.L22:
	movzbl	(%r12,%rax), %esi	# MEM[base: _133, index: ivtmp.268_2, offset: 0B], t2
	movzbl	(%rdi,%rax), %ecx	# MEM[base: src_8, index: ivtmp.268_2, offset: 0B], D.12947
	sall	$2, %ecx	#, D.12947
	movzbl	(%rdx,%rax), %r10d	# MEM[base: tmp_11(D), index: ivtmp.268_2, offset: 0B], t1
	subl	%r10d, %ecx	# t1, D.12947
	leal	(%rcx,%rsi,2), %ecx	#, D.12947
	movzbl	(%r14,%rax), %r10d	# MEM[base: _139, index: ivtmp.268_2, offset: 0B], D.12947
	leal	(%rcx,%r10,4), %ecx	#, D.12947
	movq	-24(%rsp), %r11	# %sfp, D.12951
	movzbl	(%r11,%rax), %r10d	# MEM[base: _141, index: ivtmp.268_2, offset: 0B], D.12947
	subl	%r10d, %ecx	# D.12947, D.12947
	addl	$4, %ecx	#, D.12947
	sarl	$3, %ecx	#, D.12947
	movl	%ecx, %r10d	# D.12947, D.12950
	testl	$-256, %ecx	#, D.12947
	je	.L15	#,
	negl	%ecx	# D.12947
	sarl	$31, %ecx	#, tmp347
	movl	%ecx, %r10d	# tmp347, D.12950
.L15:
	movb	%r10b, (%r12,%rax)	# D.12950, MEM[base: _133, index: ivtmp.268_2, offset: 0B]
	movzbl	0(%rbp,%rax), %r10d	# MEM[base: _163, index: ivtmp.268_2, offset: 0B], t1
	movq	-16(%rsp), %rcx	# %sfp, D.12951
	movzbl	(%rcx,%rax), %ecx	# MEM[base: _165, index: ivtmp.268_2, offset: 0B], D.12947
	sall	$2, %ecx	#, D.12947
	subl	%esi, %ecx	# t2, D.12947
	leal	(%rcx,%r10,2), %ecx	#, D.12947
	leal	(%rcx,%r10,4), %ecx	#, D.12947
	movzbl	(%r9,%rax), %esi	# MEM[base: _167, index: ivtmp.268_2, offset: 0B], D.12947
	subl	%esi, %ecx	# D.12947, D.12947
	addl	$4, %ecx	#, D.12947
	sarl	$3, %ecx	#, D.12947
	movl	%ecx, %esi	# D.12947, D.12950
	testl	$-256, %ecx	#, D.12947
	je	.L17	#,
	negl	%ecx	# D.12947
	sarl	$31, %ecx	#, tmp349
	movl	%ecx, %esi	# tmp349, D.12950
.L17:
	movq	-8(%rsp), %rcx	# %sfp, D.12951
	movb	%sil, (%rcx,%rax)	# D.12950, MEM[base: _153, index: ivtmp.268_2, offset: 0B]
	movzbl	(%r8,%rax), %esi	# MEM[base: _155, index: ivtmp.268_2, offset: 0B], t2
	movzbl	0(%rbp,%rax), %ecx	# MEM[base: _163, index: ivtmp.268_2, offset: 0B], D.12947
	sall	$2, %ecx	#, D.12947
	subl	%r10d, %ecx	# t1, D.12947
	leal	(%rcx,%rsi,2), %ecx	#, D.12947
	leal	(%rcx,%rsi,4), %ecx	#, D.12947
	movzbl	(%rbx,%rax), %r10d	# MEM[base: _159, index: ivtmp.268_2, offset: 0B], D.12947
	subl	%r10d, %ecx	# D.12947, D.12947
	addl	$4, %ecx	#, D.12947
	sarl	$3, %ecx	#, D.12947
	movl	%ecx, %r10d	# D.12947, D.12950
	testl	$-256, %ecx	#, D.12947
	je	.L19	#,
	negl	%ecx	# D.12947
	sarl	$31, %ecx	#, tmp351
	movl	%ecx, %r10d	# tmp351, D.12950
.L19:
	movb	%r10b, (%r9,%rax)	# D.12950, MEM[base: _167, index: ivtmp.268_2, offset: 0B]
	leaq	(%rdi,%r15), %rcx	#, D.12951
	movzbl	(%rcx,%rax), %r10d	# MEM[base: _147, index: ivtmp.268_2, offset: 0B], D.12949
	movzbl	%r10b, %r11d	# D.12949, t1
	movzbl	(%r8,%rax), %ecx	# MEM[base: _155, index: ivtmp.268_2, offset: 0B], D.12947
	sall	$2, %ecx	#, D.12947
	subl	%esi, %ecx	# t2, D.12947
	leal	(%rcx,%r11,2), %ecx	#, D.12947
	leal	(%rcx,%r11,4), %ecx	#, D.12947
	leaq	(%rdi,%r13), %rsi	#, D.12951
	movzbl	(%rsi,%rax), %esi	# MEM[base: _151, index: ivtmp.268_2, offset: 0B], D.12947
	subl	%esi, %ecx	# D.12947, D.12947
	addl	$4, %ecx	#, D.12947
	sarl	$3, %ecx	#, D.12947
	movl	%ecx, %esi	# D.12947, D.12950
	testl	$-256, %ecx	#, D.12947
	je	.L21	#,
	negl	%ecx	# D.12947
	sarl	$31, %ecx	#, tmp352
	movl	%ecx, %esi	# tmp352, D.12950
.L21:
	movb	%sil, (%rbx,%rax)	# D.12950, MEM[base: _159, index: ivtmp.268_2, offset: 0B]
	movb	%r10b, (%rdx,%rax)	# D.12949, MEM[base: tmp_11(D), index: ivtmp.268_2, offset: 0B]
	addq	$1, %rax	#, ivtmp.268
	cmpq	$8, %rax	#, ivtmp.268
	jne	.L22	#,
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
.LFE96:
	.size	deInterlaceFF_C, .-deInterlaceFF_C
	.type	deInterlaceL5_C, @function
deInterlaceL5_C:
.LFB97:
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
	leal	0(,%rsi,4), %r9d	#, D.12972
	movslq	%r9d, %rax	# D.12972, D.12973
	movq	%rax, -72(%rsp)	# D.12973, %sfp
	addq	%rax, %rdi	# D.12973, src
	leal	(%rsi,%rsi), %eax	#, D.12972
	movslq	%eax, %r8	# D.12972, D.12973
	addl	%esi, %eax	# stride, D.12972
	movslq	%eax, %r13	# D.12972, D.12973
	addl	%esi, %r9d	# stride, D.12972
	movslq	%r9d, %rbx	# D.12972, D.12973
	movq	%rbx, -64(%rsp)	# D.12973, %sfp
	addl	%eax, %eax	# tmp335
	cltq
	movq	%rax, -56(%rsp)	# D.12973, %sfp
	leal	0(,%rsi,8), %eax	#, tmp337
	subl	%esi, %eax	# stride, D.12972
	cltq
	movq	%rax, -48(%rsp)	# D.12973, %sfp
	movl	$0, %eax	#, ivtmp.287
	movslq	%esi, %rsi	# stride, D.12973
	leaq	(%rdi,%rsi), %rbp	#, D.12976
	leaq	0(%rbp,%rsi), %rbx	#, D.12976
	leaq	(%rbx,%rsi), %r14	#, D.12976
	leaq	(%r14,%rsi), %r15	#, D.12976
	movq	%r15, -40(%rsp)	# D.12976, %sfp
	leaq	(%rdi,%r8), %r15	#, D.12976
	movq	%r15, -32(%rsp)	# D.12976, %sfp
	addq	%rsi, %r15	# D.12973, D.12976
	movq	%r15, -24(%rsp)	# D.12976, %sfp
	addq	%rsi, %r15	# D.12973, D.12976
	movq	%r15, -16(%rsp)	# D.12976, %sfp
	addq	%rsi, %r15	# D.12973, D.12976
	movq	%r15, -8(%rsp)	# D.12976, %sfp
	addq	%rdi, %r13	# src, D.12976
.L42:
	movzbl	(%rcx,%rax), %r9d	# MEM[base: tmp2_15(D), index: ivtmp.287_2, offset: 0B], t2
	movzbl	(%rdi,%rax), %r10d	# MEM[base: src_8, index: ivtmp.287_2, offset: 0B], t3
	movzbl	0(%rbp,%rax), %r8d	# MEM[base: _216, index: ivtmp.287_2, offset: 0B], D.12972
	addl	%r9d, %r8d	# t2, D.12972
	addl	%r8d, %r8d	# D.12972
	movzbl	(%rbx,%rax), %r12d	# MEM[base: _222, index: ivtmp.287_2, offset: 0B], D.12972
	movzbl	(%rdx,%rax), %r11d	# MEM[base: tmp_11(D), index: ivtmp.287_2, offset: 0B], t1
	addl	%r12d, %r11d	# D.12972, D.12972
	subl	%r11d, %r8d	# D.12972, D.12972
	leal	(%r10,%r10,2), %r11d	#, D.12972
	leal	4(%r8,%r11,2), %r8d	#, D.12972
	sarl	$3, %r8d	#, D.12972
	movl	%r8d, %r11d	# D.12972, D.12975
	testl	$-256, %r8d	#, D.12972
	je	.L27	#,
	negl	%r8d	# D.12972
	sarl	$31, %r8d	#, tmp500
	movl	%r8d, %r11d	# tmp500, D.12975
.L27:
	movb	%r11b, (%rdi,%rax)	# D.12975, MEM[base: src_8, index: ivtmp.287_2, offset: 0B]
	movzbl	0(%rbp,%rax), %r11d	# MEM[base: _216, index: ivtmp.287_2, offset: 0B], t1
	movzbl	(%rbx,%rax), %r8d	# MEM[base: _222, index: ivtmp.287_2, offset: 0B], D.12972
	addl	%r10d, %r8d	# t3, D.12972
	addl	%r8d, %r8d	# D.12972
	movzbl	(%r14,%rax), %r12d	# MEM[base: _278, index: ivtmp.287_2, offset: 0B], D.12972
	addl	%r12d, %r9d	# D.12972, D.12972
	subl	%r9d, %r8d	# D.12972, D.12972
	leal	(%r11,%r11,2), %r9d	#, D.12972
	leal	4(%r8,%r9,2), %r8d	#, D.12972
	sarl	$3, %r8d	#, D.12972
	movl	%r8d, %r9d	# D.12972, D.12975
	testl	$-256, %r8d	#, D.12972
	je	.L29	#,
	negl	%r8d	# D.12972
	sarl	$31, %r8d	#, tmp501
	movl	%r8d, %r9d	# tmp501, D.12975
.L29:
	movb	%r9b, 0(%rbp,%rax)	# D.12975, MEM[base: _216, index: ivtmp.287_2, offset: 0B]
	movzbl	(%rbx,%rax), %r9d	# MEM[base: _222, index: ivtmp.287_2, offset: 0B], t2
	movzbl	(%r14,%rax), %r8d	# MEM[base: _278, index: ivtmp.287_2, offset: 0B], D.12972
	addl	%r11d, %r8d	# t1, D.12972
	addl	%r8d, %r8d	# D.12972
	movq	-40(%rsp), %r15	# %sfp, D.12976
	movzbl	(%r15,%rax), %r12d	# MEM[base: _272, index: ivtmp.287_2, offset: 0B], D.12972
	addl	%r12d, %r10d	# D.12972, D.12972
	subl	%r10d, %r8d	# D.12972, D.12972
	leal	(%r9,%r9,2), %r10d	#, D.12972
	leal	4(%r8,%r10,2), %r8d	#, D.12972
	sarl	$3, %r8d	#, D.12972
	movl	%r8d, %r10d	# D.12972, D.12975
	testl	$-256, %r8d	#, D.12972
	je	.L31	#,
	negl	%r8d	# D.12972
	sarl	$31, %r8d	#, tmp503
	movl	%r8d, %r10d	# tmp503, D.12975
.L31:
	movq	-32(%rsp), %r15	# %sfp, D.12976
	movb	%r10b, (%r15,%rax)	# D.12975, MEM[base: _258, index: ivtmp.287_2, offset: 0B]
	movq	-24(%rsp), %r15	# %sfp, D.12976
	movzbl	(%r15,%rax), %r10d	# MEM[base: _260, index: ivtmp.287_2, offset: 0B], t3
	movq	-16(%rsp), %r15	# %sfp, D.12976
	movzbl	(%r15,%rax), %r8d	# MEM[base: _262, index: ivtmp.287_2, offset: 0B], D.12972
	addl	%r9d, %r8d	# t2, D.12972
	addl	%r8d, %r8d	# D.12972
	movq	-8(%rsp), %r15	# %sfp, D.12976
	movzbl	(%r15,%rax), %r12d	# MEM[base: _264, index: ivtmp.287_2, offset: 0B], D.12972
	addl	%r12d, %r11d	# D.12972, D.12972
	subl	%r11d, %r8d	# D.12972, D.12972
	leal	(%r10,%r10,2), %r11d	#, D.12972
	leal	4(%r8,%r11,2), %r8d	#, D.12972
	sarl	$3, %r8d	#, D.12972
	movl	%r8d, %r11d	# D.12972, D.12975
	testl	$-256, %r8d	#, D.12972
	je	.L33	#,
	negl	%r8d	# D.12972
	sarl	$31, %r8d	#, tmp508
	movl	%r8d, %r11d	# tmp508, D.12975
.L33:
	movb	%r11b, 0(%r13,%rax)	# D.12975, MEM[base: _250, index: ivtmp.287_2, offset: 0B]
	leaq	0(%r13,%rsi), %r8	#, D.12976
	movzbl	(%r8,%rax), %r15d	# MEM[base: _252, index: ivtmp.287_2, offset: 0B], t1
	addq	%rsi, %r8	# D.12973, D.12976
	movzbl	(%r8,%rax), %r11d	# MEM[base: _254, index: ivtmp.287_2, offset: 0B], D.12972
	addl	%r10d, %r11d	# t3, D.12972
	addl	%r11d, %r11d	# D.12972
	addq	%rsi, %r8	# D.12973, D.12976
	movzbl	(%r8,%rax), %r8d	# MEM[base: _256, index: ivtmp.287_2, offset: 0B], D.12972
	addl	%r8d, %r9d	# D.12972, D.12972
	subl	%r9d, %r11d	# D.12972, D.12972
	leal	(%r15,%r15,2), %r8d	#, D.12972
	leal	4(%r11,%r8,2), %r8d	#, D.12972
	sarl	$3, %r8d	#, D.12972
	movl	%r8d, %r9d	# D.12972, D.12975
	testl	$-256, %r8d	#, D.12972
	je	.L35	#,
	negl	%r8d	# D.12972
	sarl	$31, %r8d	#, tmp509
	movl	%r8d, %r9d	# tmp509, D.12975
.L35:
	movq	-72(%rsp), %r11	# %sfp, D.12973
	leaq	(%rdi,%r11), %r8	#, D.12976
	movb	%r9b, (%r8,%rax)	# D.12975, MEM[base: _242, index: ivtmp.287_2, offset: 0B]
	addq	%rsi, %r8	# D.12973, D.12976
	movzbl	(%r8,%rax), %r11d	# MEM[base: _244, index: ivtmp.287_2, offset: 0B], t2
	addq	%rsi, %r8	# D.12973, D.12976
	movzbl	(%r8,%rax), %r9d	# MEM[base: _246, index: ivtmp.287_2, offset: 0B], D.12972
	addl	%r15d, %r9d	# t1, D.12972
	addl	%r9d, %r9d	# D.12972
	addq	%rsi, %r8	# D.12973, D.12976
	movzbl	(%r8,%rax), %r8d	# MEM[base: _248, index: ivtmp.287_2, offset: 0B], D.12972
	addl	%r8d, %r10d	# D.12972, D.12972
	subl	%r10d, %r9d	# D.12972, D.12972
	leal	(%r11,%r11,2), %r8d	#, D.12972
	leal	4(%r9,%r8,2), %r8d	#, D.12972
	sarl	$3, %r8d	#, D.12972
	movl	%r8d, %r9d	# D.12972, D.12975
	testl	$-256, %r8d	#, D.12972
	je	.L37	#,
	negl	%r8d	# D.12972
	sarl	$31, %r8d	#, tmp511
	movl	%r8d, %r9d	# tmp511, D.12975
.L37:
	movq	-64(%rsp), %r10	# %sfp, D.12973
	leaq	(%rdi,%r10), %r8	#, D.12976
	movb	%r9b, (%r8,%rax)	# D.12975, MEM[base: _234, index: ivtmp.287_2, offset: 0B]
	addq	%rsi, %r8	# D.12973, D.12976
	movzbl	(%r8,%rax), %r12d	# MEM[base: _236, index: ivtmp.287_2, offset: 0B], D.12974
	movzbl	%r12b, %r9d	# D.12974, t3
	addq	%rsi, %r8	# D.12973, D.12976
	movzbl	(%r8,%rax), %r10d	# MEM[base: _238, index: ivtmp.287_2, offset: 0B], D.12972
	addl	%r11d, %r10d	# t2, D.12972
	addl	%r10d, %r10d	# D.12972
	addq	%rsi, %r8	# D.12973, D.12976
	movzbl	(%r8,%rax), %r8d	# MEM[base: _240, index: ivtmp.287_2, offset: 0B], D.12972
	addl	%r8d, %r15d	# D.12972, D.12972
	subl	%r15d, %r10d	# D.12972, D.12972
	leal	(%r9,%r9,2), %r8d	#, D.12972
	leal	4(%r10,%r8,2), %r8d	#, D.12972
	sarl	$3, %r8d	#, D.12972
	movl	%r8d, %r10d	# D.12972, D.12975
	testl	$-256, %r8d	#, D.12972
	je	.L39	#,
	negl	%r8d	# D.12972
	sarl	$31, %r8d	#, tmp513
	movl	%r8d, %r10d	# tmp513, D.12975
.L39:
	movq	-56(%rsp), %r15	# %sfp, D.12973
	leaq	(%rdi,%r15), %r8	#, D.12976
	movb	%r10b, (%r8,%rax)	# D.12975, MEM[base: _226, index: ivtmp.287_2, offset: 0B]
	addq	%rsi, %r8	# D.12973, D.12976
	movzbl	(%r8,%rax), %r10d	# MEM[base: _228, index: ivtmp.287_2, offset: 0B], D.12974
	addq	%rsi, %r8	# D.12973, D.12976
	movzbl	(%r8,%rax), %r15d	# MEM[base: _230, index: ivtmp.287_2, offset: 0B], D.12972
	addl	%r15d, %r9d	# D.12972, D.12972
	addl	%r9d, %r9d	# D.12972
	addq	%rsi, %r8	# D.12973, D.12976
	movzbl	(%r8,%rax), %r8d	# MEM[base: _232, index: ivtmp.287_2, offset: 0B], D.12972
	addl	%r8d, %r11d	# D.12972, D.12972
	subl	%r11d, %r9d	# D.12972, D.12972
	movzbl	%r10b, %r8d	# D.12974, t1
	leal	(%r8,%r8,2), %r8d	#, D.12972
	leal	4(%r9,%r8,2), %r8d	#, D.12972
	sarl	$3, %r8d	#, D.12972
	movl	%r8d, %r9d	# D.12972, D.12975
	testl	$-256, %r8d	#, D.12972
	je	.L41	#,
	negl	%r8d	# D.12972
	sarl	$31, %r8d	#, tmp515
	movl	%r8d, %r9d	# tmp515, D.12975
.L41:
	movq	-48(%rsp), %r15	# %sfp, D.12973
	leaq	(%rdi,%r15), %r8	#, D.12976
	movb	%r9b, (%r8,%rax)	# D.12975, MEM[base: _224, index: ivtmp.287_2, offset: 0B]
	movb	%r12b, (%rdx,%rax)	# D.12974, MEM[base: tmp_11(D), index: ivtmp.287_2, offset: 0B]
	movb	%r10b, (%rcx,%rax)	# D.12974, MEM[base: tmp2_15(D), index: ivtmp.287_2, offset: 0B]
	addq	$1, %rax	#, ivtmp.287
	cmpq	$8, %rax	#, ivtmp.287
	jne	.L42	#,
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
.LFE97:
	.size	deInterlaceL5_C, .-deInterlaceL5_C
	.type	deInterlaceBlendLinear_C, @function
deInterlaceBlendLinear_C:
.LFB98:
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
	leal	0(,%rsi,4), %r9d	#, D.13007
	movslq	%r9d, %rbp	# D.13007, D.13008
	addq	%rbp, %rdi	# D.13008, src
	leal	(%rsi,%rsi), %ecx	#, D.13007
	movslq	%ecx, %r13	# D.13007, D.13008
	addl	%esi, %ecx	# stride, D.13007
	movslq	%ecx, %r12	# D.13007, D.13008
	addl	%esi, %r9d	# stride, D.13007
	movslq	%r9d, %r9	# D.13007, D.13008
	addl	%ecx, %ecx	# tmp284
	movslq	%ecx, %rcx	# tmp284, D.13008
	leal	0(,%rsi,8), %ebx	#, tmp286
	movl	%ebx, %r11d	# tmp286, D.13007
	subl	%esi, %r11d	# stride, D.13007
	movslq	%r11d, %r11	# D.13007, D.13008
	movslq	%ebx, %rbx	# tmp286, D.13008
	leaq	8(%rdi), %r14	#, D.13012
	movslq	%esi, %rsi	# stride, D.13008
.L46:
	movl	(%rdi), %r8d	# MEM[base: src_205, offset: 0B], b
	movl	(%rdi,%rsi), %eax	# MEM[base: src_205, index: _16, offset: 0B], c
	movl	%eax, %r10d	# c, D.13007
	xorl	(%rdx), %r10d	# MEM[base: tmp_206, offset: 0B], D.13007
	andl	$-16843010, %r10d	#, D.13011
	movq	%r10, %r15	# D.13011, D.13011
	shrq	%r15	# D.13011
	movl	%eax, %r10d	# c, D.13007
	andl	(%rdx), %r10d	# MEM[base: tmp_206, offset: 0B], D.13007
	addl	%r15d, %r10d	# D.13011, D.13010
	movl	%r10d, %r15d	# D.13010, D.13007
	orl	%r8d, %r15d	# b, D.13007
	xorl	%r8d, %r10d	# b, D.13007
	andl	$-16843010, %r10d	#, D.13011
	shrq	%r10	# D.13011
	subl	%r10d, %r15d	# D.13011, tmp299
	movl	%r15d, (%rdi)	# tmp299, MEM[base: src_205, offset: 0B]
	movl	(%rdi,%r13), %r10d	# MEM[base: src_205, index: _39, offset: 0B], a
	movl	%r10d, %r15d	# a, D.13007
	xorl	%r8d, %r15d	# b, D.13007
	andl	$-16843010, %r15d	#, D.13011
	shrq	%r15	# D.13011
	andl	%r10d, %r8d	# a, D.13007
	addl	%r15d, %r8d	# D.13011, D.13010
	movl	%r8d, %r15d	# D.13010, D.13007
	orl	%eax, %r15d	# c, D.13007
	xorl	%eax, %r8d	# c, D.13007
	andl	$-16843010, %r8d	#, D.13011
	shrq	%r8	# D.13011
	subl	%r8d, %r15d	# D.13011, tmp310
	movl	%r15d, (%rdi,%rsi)	# tmp310, MEM[base: src_205, index: _16, offset: 0B]
	movl	(%rdi,%r12), %r8d	# MEM[base: src_205, index: _62, offset: 0B], b
	movl	%r8d, %r15d	# b, D.13007
	xorl	%eax, %r15d	# c, D.13007
	andl	$-16843010, %r15d	#, D.13011
	shrq	%r15	# D.13011
	andl	%r8d, %eax	# b, D.13007
	addl	%r15d, %eax	# D.13011, D.13010
	movl	%eax, %r15d	# D.13010, D.13007
	orl	%r10d, %r15d	# a, D.13007
	xorl	%r10d, %eax	# a, D.13007
	andl	$-16843010, %eax	#, D.13011
	shrq	%rax	# D.13011
	subl	%eax, %r15d	# D.13011, tmp321
	movl	%r15d, (%rdi,%r13)	# tmp321, MEM[base: src_205, index: _39, offset: 0B]
	movl	(%rdi,%rbp), %eax	# MEM[base: src_205, index: _7, offset: 0B], c
	movl	%eax, %r15d	# c, D.13007
	xorl	%r10d, %r15d	# a, D.13007
	andl	$-16843010, %r15d	#, D.13011
	shrq	%r15	# D.13011
	andl	%eax, %r10d	# c, D.13007
	addl	%r15d, %r10d	# D.13011, D.13010
	movl	%r10d, %r15d	# D.13010, D.13007
	orl	%r8d, %r15d	# b, D.13007
	xorl	%r8d, %r10d	# b, D.13007
	andl	$-16843010, %r10d	#, D.13011
	shrq	%r10	# D.13011
	subl	%r10d, %r15d	# D.13011, tmp332
	movl	%r15d, (%rdi,%r12)	# tmp332, MEM[base: src_205, index: _62, offset: 0B]
	movl	(%rdi,%r9), %r10d	# MEM[base: src_205, index: _106, offset: 0B], a
	movl	%r10d, %r15d	# a, D.13007
	xorl	%r8d, %r15d	# b, D.13007
	andl	$-16843010, %r15d	#, D.13011
	shrq	%r15	# D.13011
	andl	%r10d, %r8d	# a, D.13007
	addl	%r15d, %r8d	# D.13011, D.13010
	movl	%r8d, %r15d	# D.13010, D.13007
	orl	%eax, %r15d	# c, D.13007
	xorl	%eax, %r8d	# c, D.13007
	andl	$-16843010, %r8d	#, D.13011
	shrq	%r8	# D.13011
	subl	%r8d, %r15d	# D.13011, tmp343
	movl	%r15d, (%rdi,%rbp)	# tmp343, MEM[base: src_205, index: _7, offset: 0B]
	movl	(%rdi,%rcx), %r15d	# MEM[base: src_205, index: _129, offset: 0B], b
	movl	%r15d, %r8d	# b, D.13007
	xorl	%eax, %r8d	# c, D.13007
	andl	$-16843010, %r8d	#, D.13011
	shrq	%r8	# D.13011
	andl	%r15d, %eax	# b, D.13007
	addl	%r8d, %eax	# D.13011, D.13010
	movl	%eax, %r8d	# D.13010, D.13007
	orl	%r10d, %r8d	# a, D.13007
	xorl	%r10d, %eax	# a, D.13007
	andl	$-16843010, %eax	#, D.13011
	shrq	%rax	# D.13011
	subl	%eax, %r8d	# D.13011, tmp354
	movl	%r8d, (%rdi,%r9)	# tmp354, MEM[base: src_205, index: _106, offset: 0B]
	movl	(%rdi,%r11), %r8d	# MEM[base: src_205, index: _152, offset: 0B], D.13009
	movl	%r8d, %eax	# D.13009, D.13007
	xorl	%r10d, %eax	# a, D.13007
	andl	$-16843010, %eax	#, D.13011
	shrq	%rax	# D.13011
	andl	%r8d, %r10d	# D.13009, D.13007
	addl	%eax, %r10d	# D.13011, D.13010
	movl	%r10d, %eax	# D.13010, D.13007
	orl	%r15d, %eax	# b, D.13007
	xorl	%r15d, %r10d	# b, D.13007
	andl	$-16843010, %r10d	#, D.13011
	shrq	%r10	# D.13011
	subl	%r10d, %eax	# D.13011, tmp365
	movl	%eax, (%rdi,%rcx)	# tmp365, MEM[base: src_205, index: _129, offset: 0B]
	movl	(%rdi,%rbx), %eax	# MEM[base: src_205, index: _175, offset: 0B], a
	movl	%eax, %r10d	# a, D.13007
	xorl	%r15d, %r10d	# b, D.13007
	andl	$-16843010, %r10d	#, D.13011
	shrq	%r10	# D.13011
	andl	%eax, %r15d	# a, D.13007
	leal	(%r15,%r10), %eax	#, D.13010
	movl	%eax, %r10d	# D.13010, D.13007
	orl	%r8d, %r10d	# D.13009, D.13007
	xorl	%r8d, %eax	# D.13009, D.13007
	andl	$-16843010, %eax	#, D.13011
	shrq	%rax	# D.13011
	subl	%eax, %r10d	# D.13011, tmp376
	movl	%r10d, (%rdi,%r11)	# tmp376, MEM[base: src_205, index: _152, offset: 0B]
	movl	%r8d, (%rdx)	# D.13009, MEM[base: tmp_206, offset: 0B]
	addq	$4, %rdi	#, src
	addq	$4, %rdx	#, tmp
	cmpq	%r14, %rdi	# D.13012, src
	jne	.L46	#,
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
.LFE98:
	.size	deInterlaceBlendLinear_C, .-deInterlaceBlendLinear_C
	.type	doVertLowPass_MMX, @function
doVertLowPass_MMX:
.LFB105:
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
	movq	%rdx, -24(%rsp)	# c, %sfp
	leal	(%rsi,%rsi), %ebp	#, l2
	leal	(%rsi,%rbp), %r10d	#, l3
	leal	(%rsi,%r10), %r9d	#, l4
	leal	(%rsi,%r9), %ecx	#, l5
	leal	(%rsi,%rcx), %edx	#, l6
	leal	(%rsi,%rdx), %eax	#, l7
	leal	(%rsi,%rax), %ebx	#, l8
	leal	(%rsi,%rbx), %r11d	#, l9
	movslq	%r10d, %r8	# l3, D.13032
	addq	%r8, %rdi	# D.13032, src
	movslq	%ebp, %r8	# l2, D.13032
	movq	%r8, %r14	# D.13032, D.13032
	movq	%r8, -40(%rsp)	# D.13032, %sfp
	addq	%rdi, %r8	# src, ivtmp.335
	leaq	8(%rdi,%r14), %rdi	#, D.13036
	movq	%rdi, -32(%rsp)	# D.13036, %sfp
	negq	%r14	# D.13032
	movq	%r14, -16(%rsp)	# D.13032, %sfp
	movslq	%esi, %rdi	# stride, D.13032
	movq	%rdi, -96(%rsp)	# D.13032, %sfp
	movslq	%ebx, %rbx	# l8, D.13032
	movq	%rbx, -88(%rsp)	# D.13032, %sfp
	movslq	%r11d, %rbx	# l9, D.13032
	movq	%rbx, -8(%rsp)	# D.13032, %sfp
	movslq	%r10d, %rbx	# l3, D.13032
	movq	%rbx, -80(%rsp)	# D.13032, %sfp
	movslq	%r9d, %rbx	# l4, D.13032
	movq	%rbx, -72(%rsp)	# D.13032, %sfp
	movslq	%ecx, %rcx	# l5, D.13032
	movq	%rcx, -64(%rsp)	# D.13032, %sfp
	movslq	%edx, %rcx	# l6, D.13032
	movq	%rcx, -56(%rsp)	# D.13032, %sfp
	movslq	%eax, %rbp	# l7, D.13032
.L52:
	movq	-16(%rsp), %rax	# %sfp, D.13032
	movzbl	(%r8,%rax), %edx	# MEM[base: _158, index: _160, offset: 0B], D.13031
	movq	%r8, %rax	# ivtmp.335, D.13035
	subq	-40(%rsp), %rax	# %sfp, D.13035
	movq	-96(%rsp), %rbx	# %sfp, D.13032
	movzbl	(%rax,%rbx), %r10d	# MEM[base: _163, index: _22, offset: 0B], D.13031
	movq	-24(%rsp), %rcx	# %sfp, c
	movl	1176(%rcx), %edi	# c_28(D)->QP, D.13031
	movl	%edx, %ecx	# D.13031, D.13031
	subl	%r10d, %ecx	# D.13031, D.13031
	movl	%ecx, %esi	# D.13031, tmp269
	sarl	$31, %esi	#, tmp269
	xorl	%esi, %ecx	# tmp269, tmp270
	subl	%esi, %ecx	# tmp269, D.13031
	cmpl	%edi, %ecx	# D.13031, D.13031
	cmovge	%r10d, %edx	# D.13031,, D.13031, D.13031
	movq	-88(%rsp), %rbx	# %sfp, D.13032
	movzbl	(%rax,%rbx), %ebx	# MEM[base: _163, index: _30, offset: 0B], D.13031
	movq	-8(%rsp), %rcx	# %sfp, D.13032
	movzbl	(%rax,%rcx), %ecx	# MEM[base: _163, index: _34, offset: 0B], D.13031
	movl	%ebx, %esi	# D.13031, D.13031
	subl	%ecx, %esi	# D.13031, D.13031
	movl	%esi, %r9d	# D.13031, tmp274
	sarl	$31, %r9d	#, tmp274
	xorl	%r9d, %esi	# tmp274, tmp275
	subl	%r9d, %esi	# tmp274, D.13031
	cmpl	%esi, %edi	# D.13031, D.13031
	cmovle	%ebx, %ecx	# D.13031,, D.13031, D.13031
	movzbl	(%r8), %r14d	# MEM[base: _158, offset: 0B], D.13031
	movq	-80(%rsp), %rdi	# %sfp, D.13032
	movzbl	(%rax,%rdi), %r13d	# MEM[base: _163, index: _47, offset: 0B], D.13031
	leal	(%r10,%rdx,4), %esi	#, D.13031
	addl	%r14d, %esi	# D.13031, D.13031
	leal	4(%r13,%rsi), %r11d	#, D.13031
	movq	-72(%rsp), %rsi	# %sfp, D.13032
	movzbl	(%rax,%rsi), %r12d	# MEM[base: _163, index: _54, offset: 0B], D.13031
	movl	%r11d, %r9d	# D.13031, D.13031
	subl	%edx, %r9d	# D.13031, D.13031
	addl	%r12d, %r9d	# D.13031, D.13031
	movq	-64(%rsp), %r15	# %sfp, D.13032
	movzbl	(%rax,%r15), %esi	# MEM[base: _163, index: _60, offset: 0B], D.13031
	movl	%r9d, %r15d	# D.13031, D.13031
	subl	%edx, %r15d	# D.13031, D.13031
	movl	%r15d, %edi	# D.13031, D.13031
	movl	%esi, -44(%rsp)	# D.13031, %sfp
	addl	%esi, %edi	# D.13031, D.13031
	movl	%edi, %r15d	# D.13031, D.13031
	subl	%edx, %r15d	# D.13031, D.13031
	movq	-56(%rsp), %rsi	# %sfp, D.13032
	movzbl	(%rax,%rsi), %esi	# MEM[base: _163, index: _66, offset: 0B], D.13031
	addl	%r15d, %esi	# D.13031, D.13031
	movl	%esi, %r15d	# D.13031, D.13031
	subl	%edx, %r15d	# D.13031, D.13031
	movzbl	(%rax,%rbp), %edx	# MEM[base: _163, index: _72, offset: 0B], D.13031
	addl	%edx, %r15d	# D.13031, D.13031
	movl	%r15d, %edx	# D.13031, D.13031
	subl	%r10d, %edx	# D.13031, D.13031
	addl	%ebx, %edx	# D.13031, D.13031
	movl	%edx, %ebx	# D.13031, D.13031
	subl	%r14d, %ebx	# D.13031, D.13031
	movl	%ebx, %r14d	# D.13031, D.13031
	addl	%ecx, %r14d	# D.13031, D.13031
	movl	%r14d, %ebx	# D.13031, D.13031
	subl	%r13d, %ebx	# D.13031, D.13031
	movl	%ebx, %r13d	# D.13031, D.13031
	addl	%ecx, %r13d	# D.13031, D.13031
	movl	%r13d, %ebx	# D.13031, D.13031
	subl	%r12d, %ebx	# D.13031, D.13031
	movl	%ebx, %r12d	# D.13031, D.13031
	addl	%ecx, %r12d	# D.13031, D.13031
	addl	%edi, %r11d	# D.13031, D.13031
	leal	(%r11,%r10,2), %r10d	#, D.13031
	sarl	$4, %r10d	#, D.13031
	movq	-96(%rsp), %r11	# %sfp, D.13032
	movb	%r10b, (%rax,%r11)	# D.13031, MEM[base: _163, index: _22, offset: 0B]
	addl	%esi, %r9d	# D.13031, D.13031
	movzbl	(%r8), %r10d	# MEM[base: _158, offset: 0B], D.13031
	leal	(%r9,%r10,2), %r9d	#, D.13031
	sarl	$4, %r9d	#, D.13031
	movb	%r9b, (%r8)	# D.13031, MEM[base: _158, offset: 0B]
	addl	%r15d, %edi	# D.13031, D.13031
	movq	-80(%rsp), %r11	# %sfp, D.13032
	movzbl	(%rax,%r11), %r9d	# MEM[base: _163, index: _47, offset: 0B], D.13031
	leal	(%rdi,%r9,2), %edi	#, D.13031
	sarl	$4, %edi	#, D.13031
	movb	%dil, (%rax,%r11)	# D.13031, MEM[base: _163, index: _47, offset: 0B]
	addl	%edx, %esi	# D.13031, D.13031
	movq	-72(%rsp), %r11	# %sfp, D.13032
	movzbl	(%rax,%r11), %edi	# MEM[base: _163, index: _54, offset: 0B], D.13031
	leal	(%rsi,%rdi,2), %esi	#, D.13031
	sarl	$4, %esi	#, D.13031
	movb	%sil, (%rax,%r11)	# D.13031, MEM[base: _163, index: _54, offset: 0B]
	addl	%r14d, %r15d	# D.13031, D.13031
	movq	-64(%rsp), %rdi	# %sfp, D.13032
	movzbl	(%rax,%rdi), %esi	# MEM[base: _163, index: _60, offset: 0B], D.13031
	leal	(%r15,%rsi,2), %esi	#, D.13031
	sarl	$4, %esi	#, D.13031
	movb	%sil, (%rax,%rdi)	# D.13031, MEM[base: _163, index: _60, offset: 0B]
	addl	%r13d, %edx	# D.13031, D.13031
	movq	-56(%rsp), %rdi	# %sfp, D.13032
	movzbl	(%rax,%rdi), %esi	# MEM[base: _163, index: _66, offset: 0B], D.13031
	leal	(%rdx,%rsi,2), %edx	#, D.13031
	sarl	$4, %edx	#, D.13031
	movb	%dl, (%rax,%rdi)	# D.13031, MEM[base: _163, index: _66, offset: 0B]
	addl	%r12d, %r14d	# D.13031, D.13031
	movzbl	(%rax,%rbp), %edx	# MEM[base: _163, index: _72, offset: 0B], D.13031
	leal	(%r14,%rdx,2), %edx	#, D.13031
	sarl	$4, %edx	#, D.13031
	movb	%dl, (%rax,%rbp)	# D.13031, MEM[base: _163, index: _72, offset: 0B]
	subl	-44(%rsp), %r12d	# %sfp, D.13031
	addl	%ecx, %r12d	# D.13031, D.13031
	addl	%r12d, %r13d	# D.13031, D.13031
	movq	-88(%rsp), %rbx	# %sfp, D.13032
	movzbl	(%rax,%rbx), %edx	# MEM[base: _163, index: _30, offset: 0B], D.13031
	leal	0(%r13,%rdx,2), %edx	#, D.13031
	sarl	$4, %edx	#, D.13031
	movb	%dl, (%rax,%rbx)	# D.13031, MEM[base: _163, index: _30, offset: 0B]
	addq	$1, %r8	#, ivtmp.335
	cmpq	-32(%rsp), %r8	# %sfp, ivtmp.335
	jne	.L52	#,
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
.LFE105:
	.size	doVertLowPass_MMX, .-doVertLowPass_MMX
	.type	doVertDefFilter_MMX, @function
doVertDefFilter_MMX:
.LFB107:
	.cfi_startproc
	leal	0(,%rsi,4), %eax	#, D.13038
	cltq
	addq	%rax, %rdi	# D.13039, src
	movslq	%esi, %rsi	# stride, D.13040
	leaq	-32(%rsp), %rcx	#, tmp94
#APP
# 821 "postprocess_template.c" 1
	pxor %mm7, %mm7                      
	movq (%rdi), %mm0                       	# src
	movq %mm0, %mm1                      
	punpcklbw %mm7, %mm0                 
	punpckhbw %mm7, %mm1                 
	movq (%rdi, %rsi), %mm2                   	# src, D.13040
	lea (%rdi, %rsi, 2), %rax             	# src, D.13040
	movq %mm2, %mm3                      
	punpcklbw %mm7, %mm2                 
	punpckhbw %mm7, %mm3                 
	movq (%rax), %mm4                
	movq %mm4, %mm5                      
	punpcklbw %mm7, %mm4                 
	punpckhbw %mm7, %mm5                 
	paddw %mm0, %mm0                     
	paddw %mm1, %mm1                     
	psubw %mm4, %mm2                     
	psubw %mm5, %mm3                     
	psubw %mm2, %mm0                     
	psubw %mm3, %mm1                     
	psllw $2, %mm2                        
	psllw $2, %mm3                        
	psubw %mm2, %mm0                     
	psubw %mm3, %mm1                     
	movq (%rax, %rsi), %mm2            	# D.13040
	movq %mm2, %mm3                      
	punpcklbw %mm7, %mm2                 
	punpckhbw %mm7, %mm3                 
	psubw %mm2, %mm0                     
	psubw %mm3, %mm1                     
	psubw %mm2, %mm0                     
	psubw %mm3, %mm1                     
	movq %mm0, (%rcx)                       	# tmp94
	movq %mm1, 8(%rcx)                      	# tmp94
	movq (%rax, %rsi, 2), %mm0         	# D.13040
	movq %mm0, %mm1                      
	punpcklbw %mm7, %mm0                 
	punpckhbw %mm7, %mm1                 
	psubw %mm0, %mm2                     
	psubw %mm1, %mm3                     
	movq %mm2, 16(%rcx)                     	# tmp94
	movq %mm3, 24(%rcx)                     	# tmp94
	paddw %mm4, %mm4                     
	paddw %mm5, %mm5                     
	psubw %mm2, %mm4                     
	psubw %mm3, %mm5                     
	lea (%rax, %rsi), %rdi                	# D.13040, src
	psllw $2, %mm2                        
	psllw $2, %mm3                        
	psubw %mm2, %mm4                     
	psubw %mm3, %mm5                     
	movq (%rdi, %rsi, 2), %mm2                	# src, D.13040
	movq %mm2, %mm3                      
	punpcklbw %mm7, %mm2                 
	punpckhbw %mm7, %mm3                 
	psubw %mm2, %mm4                     
	psubw %mm3, %mm5                     
	psubw %mm2, %mm4                     
	psubw %mm3, %mm5                     
	movq (%rax, %rsi, 4), %mm6         	# D.13040
	punpcklbw %mm7, %mm6                 
	psubw %mm6, %mm2                     
	movq (%rax, %rsi, 4), %mm6         	# D.13040
	punpckhbw %mm7, %mm6                 
	psubw %mm6, %mm3                     
	paddw %mm0, %mm0                     
	paddw %mm1, %mm1                     
	psubw %mm2, %mm0                     
	psubw %mm3, %mm1                     
	psllw $2, %mm2                        
	psllw $2, %mm3                        
	psubw %mm2, %mm0                     
	psubw %mm3, %mm1                     
	movq (%rdi, %rsi, 4), %mm2                	# src, D.13040
	movq %mm2, %mm3                      
	punpcklbw %mm7, %mm2                 
	punpckhbw %mm7, %mm3                 
	paddw %mm2, %mm2                     
	paddw %mm3, %mm3                     
	psubw %mm2, %mm0                     
	psubw %mm3, %mm1                     
	movq (%rcx), %mm2                       	# tmp94
	movq 8(%rcx), %mm3                      	# tmp94
	movq %mm7, %mm6                      
	pcmpgtw %mm0, %mm6                   
	pxor %mm6, %mm0                      
	psubw %mm6, %mm0                     
	movq %mm7, %mm6                      
	pcmpgtw %mm1, %mm6                   
	pxor %mm6, %mm1                      
	psubw %mm6, %mm1                     
	movq %mm7, %mm6                      
	pcmpgtw %mm2, %mm6                   
	pxor %mm6, %mm2                      
	psubw %mm6, %mm2                     
	movq %mm7, %mm6                      
	pcmpgtw %mm3, %mm6                   
	pxor %mm6, %mm3                      
	psubw %mm6, %mm3                     
	movq %mm0, %mm6                      
	psubusw %mm2, %mm6                   
	psubw %mm6, %mm0                     
	movq %mm1, %mm6                      
	psubusw %mm3, %mm6                   
	psubw %mm6, %mm1                     
	movd 112(%rdx), %mm2                         	# c_8(D)->pQPb
	punpcklbw %mm7, %mm2                 
	movq %mm7, %mm6                      
	pcmpgtw %mm4, %mm6                   
	pxor %mm6, %mm4                      
	psubw %mm6, %mm4                     
	pcmpgtw %mm5, %mm7                   
	pxor %mm7, %mm5                      
	psubw %mm7, %mm5                     
	psllw $3, %mm2                        
	movq %mm2, %mm3                      
	pcmpgtw %mm4, %mm2                   
	pcmpgtw %mm5, %mm3                   
	pand %mm2, %mm4                      
	pand %mm3, %mm5                      
	psubusw %mm0, %mm4                   
	psubusw %mm1, %mm5                   
	movq w05, %mm2              
	pmullw %mm2, %mm4                    
	pmullw %mm2, %mm5                    
	movq w20, %mm2              
	paddw %mm2, %mm4                     
	paddw %mm2, %mm5                     
	psrlw $6, %mm4                        
	psrlw $6, %mm5                        
	movq 16(%rcx), %mm0                     	# tmp94
	movq 24(%rcx), %mm1                     	# tmp94
	pxor %mm2, %mm2                      
	pxor %mm3, %mm3                      
	pcmpgtw %mm0, %mm2                   
	pcmpgtw %mm1, %mm3                   
	pxor %mm2, %mm0                      
	pxor %mm3, %mm1                      
	psubw %mm2, %mm0                     
	psubw %mm3, %mm1                     
	psrlw $1, %mm0                        
	psrlw $1, %mm1                        
	pxor %mm6, %mm2                      
	pxor %mm7, %mm3                      
	pand %mm2, %mm4                      
	pand %mm3, %mm5                      
	movq %mm4, %mm2                      
	psubusw %mm0, %mm2                   
	psubw %mm2, %mm4                     
	movq %mm5, %mm2                      
	psubusw %mm1, %mm2                   
	psubw %mm2, %mm5                     
	pxor %mm6, %mm4                      
	pxor %mm7, %mm5                      
	psubw %mm6, %mm4                     
	psubw %mm7, %mm5                     
	packsswb %mm5, %mm4                  
	movq (%rdi), %mm0                       	# src
	paddb   %mm4, %mm0                   
	movq %mm0, (%rdi)                       	# src
	movq (%rdi, %rsi), %mm0                   	# src, D.13040
	psubb %mm4, %mm0                     
	movq %mm0, (%rdi, %rsi)                   	# src, D.13040
	
# 0 "" 2
#NO_APP
	ret
	.cfi_endproc
.LFE107:
	.size	doVertDefFilter_MMX, .-doVertDefFilter_MMX
	.type	deInterlaceInterpolateCubic_MMX, @function
deInterlaceInterpolateCubic_MMX:
.LFB110:
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
	leal	(%rsi,%rsi), %ebx	#, tmp237
	leal	(%rbx,%rsi), %ebp	#, D.13067
	movslq	%ebp, %r14	# D.13067, D.13068
	addq	%r14, %rdi	# D.13068, src
	movslq	%ebx, %rbx	# tmp237, D.13068
	leal	0(,%rsi,4), %r9d	#, D.13067
	movslq	%r9d, %r11	# D.13067, D.13068
	leal	(%rbp,%rbp), %r12d	#, tmp244
	movslq	%r12d, %r12	# tmp244, D.13068
	addl	%esi, %r9d	# stride, D.13067
	movslq	%r9d, %r15	# D.13067, D.13068
	leal	0(,%rsi,8), %eax	#, D.13067
	movslq	%eax, %r10	# D.13067, D.13068
	movl	%eax, %r13d	# D.13067, D.13067
	subl	%esi, %r13d	# stride, D.13067
	movslq	%r13d, %r13	# D.13067, D.13068
	addl	%r9d, %r9d	# tmp255
	movslq	%r9d, %r9	# tmp255, D.13068
	addl	%eax, %esi	# D.13067, D.13067
	movslq	%esi, %rsi	# D.13067, D.13068
	sall	$2, %ebp	#, tmp262
	movslq	%ebp, %rbp	# tmp262, D.13068
	leaq	(%rdi,%r12), %r8	#, ivtmp.355
	leaq	8(%rdi,%r12), %rax	#, D.13073
	movq	%rax, -16(%rsp)	# D.13073, %sfp
	movq	%r12, %rax	# D.13068, D.13068
	negq	%rax	# D.13068
	movq	%rax, -8(%rsp)	# D.13068, %sfp
.L65:
	movq	%r8, %rax	# ivtmp.355, D.13072
	subq	%r12, %rax	# D.13068, D.13072
	movq	%r8, %rcx	# ivtmp.355, D.13072
	movzbl	(%rax,%rbx), %edx	# MEM[base: _133, index: _12, offset: 0B], D.13067
	leal	(%rdx,%rdx,8), %edx	#, D.13067
	movq	-8(%rsp), %rdi	# %sfp, D.13068
	movzbl	(%r8,%rdi), %edi	# MEM[base: _135, index: _137, offset: 0B], D.13067
	subl	%edi, %edx	# D.13067, D.13067
	movzbl	(%rax,%r11), %edi	# MEM[base: _133, index: _21, offset: 0B], D.13067
	leal	(%rdi,%rdi,8), %edi	#, D.13067
	addl	%edi, %edx	# D.13067, D.13067
	movzbl	(%r8), %edi	# MEM[base: _135, offset: 0B], D.13067
	subl	%edi, %edx	# D.13067, D.13067
	sarl	$4, %edx	#, D.13067
	movl	%edx, %edi	# D.13067, D.13070
	testl	$-256, %edx	#, D.13067
	je	.L58	#,
	negl	%edx	# D.13067
	sarl	$31, %edx	#, tmp346
	movl	%edx, %edi	# tmp346, D.13070
.L58:
	movb	%dil, (%rax,%r14)	# D.13070, MEM[base: _133, index: _6, offset: 0B]
	movzbl	(%rax,%r11), %edx	# MEM[base: _133, index: _21, offset: 0B], D.13067
	leal	(%rdx,%rdx,8), %edx	#, D.13067
	movzbl	(%rax,%rbx), %edi	# MEM[base: _133, index: _12, offset: 0B], D.13067
	subl	%edi, %edx	# D.13067, D.13067
	movzbl	(%rcx), %edi	# MEM[base: _135, offset: 0B], D.13067
	leal	(%rdi,%rdi,8), %edi	#, D.13067
	addl	%edi, %edx	# D.13067, D.13067
	movzbl	(%rax,%r10), %edi	# MEM[base: _133, index: _49, offset: 0B], D.13067
	subl	%edi, %edx	# D.13067, D.13067
	sarl	$4, %edx	#, D.13067
	movl	%edx, %edi	# D.13067, D.13070
	testl	$-256, %edx	#, D.13067
	je	.L60	#,
	negl	%edx	# D.13067
	sarl	$31, %edx	#, tmp347
	movl	%edx, %edi	# tmp347, D.13070
.L60:
	movb	%dil, (%rax,%r15)	# D.13070, MEM[base: _133, index: _36, offset: 0B]
	movzbl	(%rcx), %edx	# MEM[base: _135, offset: 0B], D.13067
	leal	(%rdx,%rdx,8), %edx	#, D.13067
	movzbl	(%rax,%r11), %edi	# MEM[base: _133, index: _21, offset: 0B], D.13067
	subl	%edi, %edx	# D.13067, D.13067
	movzbl	(%rax,%r10), %edi	# MEM[base: _133, index: _49, offset: 0B], D.13067
	leal	(%rdi,%rdi,8), %edi	#, D.13067
	addl	%edi, %edx	# D.13067, D.13067
	movzbl	(%rax,%r9), %edi	# MEM[base: _133, index: _70, offset: 0B], D.13067
	subl	%edi, %edx	# D.13067, D.13067
	sarl	$4, %edx	#, D.13067
	movl	%edx, %edi	# D.13067, D.13070
	testl	$-256, %edx	#, D.13067
	je	.L62	#,
	negl	%edx	# D.13067
	sarl	$31, %edx	#, tmp348
	movl	%edx, %edi	# tmp348, D.13070
.L62:
	movb	%dil, (%rax,%r13)	# D.13070, MEM[base: _133, index: _57, offset: 0B]
	movzbl	(%rax,%r10), %edx	# MEM[base: _133, index: _49, offset: 0B], D.13067
	leal	(%rdx,%rdx,8), %edx	#, D.13067
	movzbl	(%rcx), %ecx	# MEM[base: _135, offset: 0B], D.13067
	subl	%ecx, %edx	# D.13067, D.13067
	movzbl	(%rax,%r9), %ecx	# MEM[base: _133, index: _70, offset: 0B], D.13067
	leal	(%rcx,%rcx,8), %ecx	#, D.13067
	addl	%ecx, %edx	# D.13067, D.13067
	movzbl	(%rax,%rbp), %ecx	# MEM[base: _133, index: _91, offset: 0B], D.13067
	subl	%ecx, %edx	# D.13067, D.13067
	sarl	$4, %edx	#, D.13067
	movl	%edx, %ecx	# D.13067, D.13070
	testl	$-256, %edx	#, D.13067
	je	.L64	#,
	negl	%edx	# D.13067
	sarl	$31, %edx	#, tmp349
	movl	%edx, %ecx	# tmp349, D.13070
.L64:
	movb	%cl, (%rax,%rsi)	# D.13070, MEM[base: _133, index: _78, offset: 0B]
	addq	$1, %r8	#, ivtmp.355
	cmpq	-16(%rsp), %r8	# %sfp, ivtmp.355
	jne	.L65	#,
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
.LFE110:
	.size	deInterlaceInterpolateCubic_MMX, .-deInterlaceInterpolateCubic_MMX
	.type	deInterlaceFF_MMX, @function
deInterlaceFF_MMX:
.LFB111:
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
	leal	0(,%rsi,4), %r9d	#, D.13102
	movslq	%r9d, %rbp	# D.13102, D.13103
	addq	%rbp, %rdi	# D.13103, src
	leal	(%rsi,%rsi), %r8d	#, D.13102
	movslq	%r8d, %r10	# D.13102, D.13103
	addl	%esi, %r8d	# stride, D.13102
	movslq	%r8d, %rcx	# D.13102, D.13103
	addl	%esi, %r9d	# stride, D.13102
	movslq	%r9d, %r9	# D.13102, D.13103
	addl	%r8d, %r8d	# tmp254
	movslq	%r8d, %r8	# tmp254, D.13103
	leal	0(,%rsi,8), %r13d	#, tmp256
	movl	%r13d, %ebx	# tmp256, D.13102
	subl	%esi, %ebx	# stride, D.13102
	movslq	%ebx, %rbx	# D.13102, D.13103
	movslq	%r13d, %r15	# tmp256, D.13103
	addl	%esi, %r13d	# stride, D.13102
	movslq	%r13d, %r13	# D.13102, D.13103
	movl	$0, %eax	#, ivtmp.365
	movslq	%esi, %rsi	# stride, D.13103
	leaq	(%rdi,%rsi), %r12	#, D.13106
	leaq	(%r12,%rsi), %r14	#, D.13106
	addq	%r14, %rsi	# D.13106, D.13106
	movq	%rsi, -24(%rsp)	# D.13106, %sfp
	addq	%rdi, %rbp	# src, D.13106
	addq	%rdi, %r9	# src, D.13106
	leaq	(%rdi,%r10), %rsi	#, D.13106
	movq	%rsi, -16(%rsp)	# D.13106, %sfp
	leaq	(%rdi,%rcx), %rsi	#, D.13106
	movq	%rsi, -8(%rsp)	# D.13106, %sfp
	addq	%rdi, %r8	# src, D.13106
	addq	%rdi, %rbx	# src, D.13106
.L77:
	movzbl	(%r12,%rax), %esi	# MEM[base: _133, index: ivtmp.365_2, offset: 0B], t2
	movzbl	(%rdi,%rax), %ecx	# MEM[base: src_8, index: ivtmp.365_2, offset: 0B], D.13102
	sall	$2, %ecx	#, D.13102
	movzbl	(%rdx,%rax), %r10d	# MEM[base: tmp_11(D), index: ivtmp.365_2, offset: 0B], t1
	subl	%r10d, %ecx	# t1, D.13102
	leal	(%rcx,%rsi,2), %ecx	#, D.13102
	movzbl	(%r14,%rax), %r10d	# MEM[base: _139, index: ivtmp.365_2, offset: 0B], D.13102
	leal	(%rcx,%r10,4), %ecx	#, D.13102
	movq	-24(%rsp), %r11	# %sfp, D.13106
	movzbl	(%r11,%rax), %r10d	# MEM[base: _141, index: ivtmp.365_2, offset: 0B], D.13102
	subl	%r10d, %ecx	# D.13102, D.13102
	addl	$4, %ecx	#, D.13102
	sarl	$3, %ecx	#, D.13102
	movl	%ecx, %r10d	# D.13102, D.13105
	testl	$-256, %ecx	#, D.13102
	je	.L70	#,
	negl	%ecx	# D.13102
	sarl	$31, %ecx	#, tmp347
	movl	%ecx, %r10d	# tmp347, D.13105
.L70:
	movb	%r10b, (%r12,%rax)	# D.13105, MEM[base: _133, index: ivtmp.365_2, offset: 0B]
	movzbl	0(%rbp,%rax), %r10d	# MEM[base: _163, index: ivtmp.365_2, offset: 0B], t1
	movq	-16(%rsp), %rcx	# %sfp, D.13106
	movzbl	(%rcx,%rax), %ecx	# MEM[base: _165, index: ivtmp.365_2, offset: 0B], D.13102
	sall	$2, %ecx	#, D.13102
	subl	%esi, %ecx	# t2, D.13102
	leal	(%rcx,%r10,2), %ecx	#, D.13102
	leal	(%rcx,%r10,4), %ecx	#, D.13102
	movzbl	(%r9,%rax), %esi	# MEM[base: _167, index: ivtmp.365_2, offset: 0B], D.13102
	subl	%esi, %ecx	# D.13102, D.13102
	addl	$4, %ecx	#, D.13102
	sarl	$3, %ecx	#, D.13102
	movl	%ecx, %esi	# D.13102, D.13105
	testl	$-256, %ecx	#, D.13102
	je	.L72	#,
	negl	%ecx	# D.13102
	sarl	$31, %ecx	#, tmp349
	movl	%ecx, %esi	# tmp349, D.13105
.L72:
	movq	-8(%rsp), %rcx	# %sfp, D.13106
	movb	%sil, (%rcx,%rax)	# D.13105, MEM[base: _153, index: ivtmp.365_2, offset: 0B]
	movzbl	(%r8,%rax), %esi	# MEM[base: _155, index: ivtmp.365_2, offset: 0B], t2
	movzbl	0(%rbp,%rax), %ecx	# MEM[base: _163, index: ivtmp.365_2, offset: 0B], D.13102
	sall	$2, %ecx	#, D.13102
	subl	%r10d, %ecx	# t1, D.13102
	leal	(%rcx,%rsi,2), %ecx	#, D.13102
	leal	(%rcx,%rsi,4), %ecx	#, D.13102
	movzbl	(%rbx,%rax), %r10d	# MEM[base: _159, index: ivtmp.365_2, offset: 0B], D.13102
	subl	%r10d, %ecx	# D.13102, D.13102
	addl	$4, %ecx	#, D.13102
	sarl	$3, %ecx	#, D.13102
	movl	%ecx, %r10d	# D.13102, D.13105
	testl	$-256, %ecx	#, D.13102
	je	.L74	#,
	negl	%ecx	# D.13102
	sarl	$31, %ecx	#, tmp351
	movl	%ecx, %r10d	# tmp351, D.13105
.L74:
	movb	%r10b, (%r9,%rax)	# D.13105, MEM[base: _167, index: ivtmp.365_2, offset: 0B]
	leaq	(%rdi,%r15), %rcx	#, D.13106
	movzbl	(%rcx,%rax), %r10d	# MEM[base: _147, index: ivtmp.365_2, offset: 0B], D.13104
	movzbl	%r10b, %r11d	# D.13104, t1
	movzbl	(%r8,%rax), %ecx	# MEM[base: _155, index: ivtmp.365_2, offset: 0B], D.13102
	sall	$2, %ecx	#, D.13102
	subl	%esi, %ecx	# t2, D.13102
	leal	(%rcx,%r11,2), %ecx	#, D.13102
	leal	(%rcx,%r11,4), %ecx	#, D.13102
	leaq	(%rdi,%r13), %rsi	#, D.13106
	movzbl	(%rsi,%rax), %esi	# MEM[base: _151, index: ivtmp.365_2, offset: 0B], D.13102
	subl	%esi, %ecx	# D.13102, D.13102
	addl	$4, %ecx	#, D.13102
	sarl	$3, %ecx	#, D.13102
	movl	%ecx, %esi	# D.13102, D.13105
	testl	$-256, %ecx	#, D.13102
	je	.L76	#,
	negl	%ecx	# D.13102
	sarl	$31, %ecx	#, tmp352
	movl	%ecx, %esi	# tmp352, D.13105
.L76:
	movb	%sil, (%rbx,%rax)	# D.13105, MEM[base: _159, index: ivtmp.365_2, offset: 0B]
	movb	%r10b, (%rdx,%rax)	# D.13104, MEM[base: tmp_11(D), index: ivtmp.365_2, offset: 0B]
	addq	$1, %rax	#, ivtmp.365
	cmpq	$8, %rax	#, ivtmp.365
	jne	.L77	#,
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
.LFE111:
	.size	deInterlaceFF_MMX, .-deInterlaceFF_MMX
	.type	deInterlaceL5_MMX, @function
deInterlaceL5_MMX:
.LFB112:
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
	leal	0(,%rsi,4), %r9d	#, D.13127
	movslq	%r9d, %rax	# D.13127, D.13128
	movq	%rax, -72(%rsp)	# D.13128, %sfp
	addq	%rax, %rdi	# D.13128, src
	leal	(%rsi,%rsi), %eax	#, D.13127
	movslq	%eax, %r8	# D.13127, D.13128
	addl	%esi, %eax	# stride, D.13127
	movslq	%eax, %r13	# D.13127, D.13128
	addl	%esi, %r9d	# stride, D.13127
	movslq	%r9d, %rbx	# D.13127, D.13128
	movq	%rbx, -64(%rsp)	# D.13128, %sfp
	addl	%eax, %eax	# tmp335
	cltq
	movq	%rax, -56(%rsp)	# D.13128, %sfp
	leal	0(,%rsi,8), %eax	#, tmp337
	subl	%esi, %eax	# stride, D.13127
	cltq
	movq	%rax, -48(%rsp)	# D.13128, %sfp
	movl	$0, %eax	#, ivtmp.384
	movslq	%esi, %rsi	# stride, D.13128
	leaq	(%rdi,%rsi), %rbp	#, D.13131
	leaq	0(%rbp,%rsi), %rbx	#, D.13131
	leaq	(%rbx,%rsi), %r14	#, D.13131
	leaq	(%r14,%rsi), %r15	#, D.13131
	movq	%r15, -40(%rsp)	# D.13131, %sfp
	leaq	(%rdi,%r8), %r15	#, D.13131
	movq	%r15, -32(%rsp)	# D.13131, %sfp
	addq	%rsi, %r15	# D.13128, D.13131
	movq	%r15, -24(%rsp)	# D.13131, %sfp
	addq	%rsi, %r15	# D.13128, D.13131
	movq	%r15, -16(%rsp)	# D.13131, %sfp
	addq	%rsi, %r15	# D.13128, D.13131
	movq	%r15, -8(%rsp)	# D.13131, %sfp
	addq	%rdi, %r13	# src, D.13131
.L97:
	movzbl	(%rcx,%rax), %r9d	# MEM[base: tmp2_15(D), index: ivtmp.384_2, offset: 0B], t2
	movzbl	(%rdi,%rax), %r10d	# MEM[base: src_8, index: ivtmp.384_2, offset: 0B], t3
	movzbl	0(%rbp,%rax), %r8d	# MEM[base: _216, index: ivtmp.384_2, offset: 0B], D.13127
	addl	%r9d, %r8d	# t2, D.13127
	addl	%r8d, %r8d	# D.13127
	movzbl	(%rbx,%rax), %r12d	# MEM[base: _222, index: ivtmp.384_2, offset: 0B], D.13127
	movzbl	(%rdx,%rax), %r11d	# MEM[base: tmp_11(D), index: ivtmp.384_2, offset: 0B], t1
	addl	%r12d, %r11d	# D.13127, D.13127
	subl	%r11d, %r8d	# D.13127, D.13127
	leal	(%r10,%r10,2), %r11d	#, D.13127
	leal	4(%r8,%r11,2), %r8d	#, D.13127
	sarl	$3, %r8d	#, D.13127
	movl	%r8d, %r11d	# D.13127, D.13130
	testl	$-256, %r8d	#, D.13127
	je	.L82	#,
	negl	%r8d	# D.13127
	sarl	$31, %r8d	#, tmp500
	movl	%r8d, %r11d	# tmp500, D.13130
.L82:
	movb	%r11b, (%rdi,%rax)	# D.13130, MEM[base: src_8, index: ivtmp.384_2, offset: 0B]
	movzbl	0(%rbp,%rax), %r11d	# MEM[base: _216, index: ivtmp.384_2, offset: 0B], t1
	movzbl	(%rbx,%rax), %r8d	# MEM[base: _222, index: ivtmp.384_2, offset: 0B], D.13127
	addl	%r10d, %r8d	# t3, D.13127
	addl	%r8d, %r8d	# D.13127
	movzbl	(%r14,%rax), %r12d	# MEM[base: _278, index: ivtmp.384_2, offset: 0B], D.13127
	addl	%r12d, %r9d	# D.13127, D.13127
	subl	%r9d, %r8d	# D.13127, D.13127
	leal	(%r11,%r11,2), %r9d	#, D.13127
	leal	4(%r8,%r9,2), %r8d	#, D.13127
	sarl	$3, %r8d	#, D.13127
	movl	%r8d, %r9d	# D.13127, D.13130
	testl	$-256, %r8d	#, D.13127
	je	.L84	#,
	negl	%r8d	# D.13127
	sarl	$31, %r8d	#, tmp501
	movl	%r8d, %r9d	# tmp501, D.13130
.L84:
	movb	%r9b, 0(%rbp,%rax)	# D.13130, MEM[base: _216, index: ivtmp.384_2, offset: 0B]
	movzbl	(%rbx,%rax), %r9d	# MEM[base: _222, index: ivtmp.384_2, offset: 0B], t2
	movzbl	(%r14,%rax), %r8d	# MEM[base: _278, index: ivtmp.384_2, offset: 0B], D.13127
	addl	%r11d, %r8d	# t1, D.13127
	addl	%r8d, %r8d	# D.13127
	movq	-40(%rsp), %r15	# %sfp, D.13131
	movzbl	(%r15,%rax), %r12d	# MEM[base: _272, index: ivtmp.384_2, offset: 0B], D.13127
	addl	%r12d, %r10d	# D.13127, D.13127
	subl	%r10d, %r8d	# D.13127, D.13127
	leal	(%r9,%r9,2), %r10d	#, D.13127
	leal	4(%r8,%r10,2), %r8d	#, D.13127
	sarl	$3, %r8d	#, D.13127
	movl	%r8d, %r10d	# D.13127, D.13130
	testl	$-256, %r8d	#, D.13127
	je	.L86	#,
	negl	%r8d	# D.13127
	sarl	$31, %r8d	#, tmp503
	movl	%r8d, %r10d	# tmp503, D.13130
.L86:
	movq	-32(%rsp), %r15	# %sfp, D.13131
	movb	%r10b, (%r15,%rax)	# D.13130, MEM[base: _258, index: ivtmp.384_2, offset: 0B]
	movq	-24(%rsp), %r15	# %sfp, D.13131
	movzbl	(%r15,%rax), %r10d	# MEM[base: _260, index: ivtmp.384_2, offset: 0B], t3
	movq	-16(%rsp), %r15	# %sfp, D.13131
	movzbl	(%r15,%rax), %r8d	# MEM[base: _262, index: ivtmp.384_2, offset: 0B], D.13127
	addl	%r9d, %r8d	# t2, D.13127
	addl	%r8d, %r8d	# D.13127
	movq	-8(%rsp), %r15	# %sfp, D.13131
	movzbl	(%r15,%rax), %r12d	# MEM[base: _264, index: ivtmp.384_2, offset: 0B], D.13127
	addl	%r12d, %r11d	# D.13127, D.13127
	subl	%r11d, %r8d	# D.13127, D.13127
	leal	(%r10,%r10,2), %r11d	#, D.13127
	leal	4(%r8,%r11,2), %r8d	#, D.13127
	sarl	$3, %r8d	#, D.13127
	movl	%r8d, %r11d	# D.13127, D.13130
	testl	$-256, %r8d	#, D.13127
	je	.L88	#,
	negl	%r8d	# D.13127
	sarl	$31, %r8d	#, tmp508
	movl	%r8d, %r11d	# tmp508, D.13130
.L88:
	movb	%r11b, 0(%r13,%rax)	# D.13130, MEM[base: _250, index: ivtmp.384_2, offset: 0B]
	leaq	0(%r13,%rsi), %r8	#, D.13131
	movzbl	(%r8,%rax), %r15d	# MEM[base: _252, index: ivtmp.384_2, offset: 0B], t1
	addq	%rsi, %r8	# D.13128, D.13131
	movzbl	(%r8,%rax), %r11d	# MEM[base: _254, index: ivtmp.384_2, offset: 0B], D.13127
	addl	%r10d, %r11d	# t3, D.13127
	addl	%r11d, %r11d	# D.13127
	addq	%rsi, %r8	# D.13128, D.13131
	movzbl	(%r8,%rax), %r8d	# MEM[base: _256, index: ivtmp.384_2, offset: 0B], D.13127
	addl	%r8d, %r9d	# D.13127, D.13127
	subl	%r9d, %r11d	# D.13127, D.13127
	leal	(%r15,%r15,2), %r8d	#, D.13127
	leal	4(%r11,%r8,2), %r8d	#, D.13127
	sarl	$3, %r8d	#, D.13127
	movl	%r8d, %r9d	# D.13127, D.13130
	testl	$-256, %r8d	#, D.13127
	je	.L90	#,
	negl	%r8d	# D.13127
	sarl	$31, %r8d	#, tmp509
	movl	%r8d, %r9d	# tmp509, D.13130
.L90:
	movq	-72(%rsp), %r11	# %sfp, D.13128
	leaq	(%rdi,%r11), %r8	#, D.13131
	movb	%r9b, (%r8,%rax)	# D.13130, MEM[base: _242, index: ivtmp.384_2, offset: 0B]
	addq	%rsi, %r8	# D.13128, D.13131
	movzbl	(%r8,%rax), %r11d	# MEM[base: _244, index: ivtmp.384_2, offset: 0B], t2
	addq	%rsi, %r8	# D.13128, D.13131
	movzbl	(%r8,%rax), %r9d	# MEM[base: _246, index: ivtmp.384_2, offset: 0B], D.13127
	addl	%r15d, %r9d	# t1, D.13127
	addl	%r9d, %r9d	# D.13127
	addq	%rsi, %r8	# D.13128, D.13131
	movzbl	(%r8,%rax), %r8d	# MEM[base: _248, index: ivtmp.384_2, offset: 0B], D.13127
	addl	%r8d, %r10d	# D.13127, D.13127
	subl	%r10d, %r9d	# D.13127, D.13127
	leal	(%r11,%r11,2), %r8d	#, D.13127
	leal	4(%r9,%r8,2), %r8d	#, D.13127
	sarl	$3, %r8d	#, D.13127
	movl	%r8d, %r9d	# D.13127, D.13130
	testl	$-256, %r8d	#, D.13127
	je	.L92	#,
	negl	%r8d	# D.13127
	sarl	$31, %r8d	#, tmp511
	movl	%r8d, %r9d	# tmp511, D.13130
.L92:
	movq	-64(%rsp), %r10	# %sfp, D.13128
	leaq	(%rdi,%r10), %r8	#, D.13131
	movb	%r9b, (%r8,%rax)	# D.13130, MEM[base: _234, index: ivtmp.384_2, offset: 0B]
	addq	%rsi, %r8	# D.13128, D.13131
	movzbl	(%r8,%rax), %r12d	# MEM[base: _236, index: ivtmp.384_2, offset: 0B], D.13129
	movzbl	%r12b, %r9d	# D.13129, t3
	addq	%rsi, %r8	# D.13128, D.13131
	movzbl	(%r8,%rax), %r10d	# MEM[base: _238, index: ivtmp.384_2, offset: 0B], D.13127
	addl	%r11d, %r10d	# t2, D.13127
	addl	%r10d, %r10d	# D.13127
	addq	%rsi, %r8	# D.13128, D.13131
	movzbl	(%r8,%rax), %r8d	# MEM[base: _240, index: ivtmp.384_2, offset: 0B], D.13127
	addl	%r8d, %r15d	# D.13127, D.13127
	subl	%r15d, %r10d	# D.13127, D.13127
	leal	(%r9,%r9,2), %r8d	#, D.13127
	leal	4(%r10,%r8,2), %r8d	#, D.13127
	sarl	$3, %r8d	#, D.13127
	movl	%r8d, %r10d	# D.13127, D.13130
	testl	$-256, %r8d	#, D.13127
	je	.L94	#,
	negl	%r8d	# D.13127
	sarl	$31, %r8d	#, tmp513
	movl	%r8d, %r10d	# tmp513, D.13130
.L94:
	movq	-56(%rsp), %r15	# %sfp, D.13128
	leaq	(%rdi,%r15), %r8	#, D.13131
	movb	%r10b, (%r8,%rax)	# D.13130, MEM[base: _226, index: ivtmp.384_2, offset: 0B]
	addq	%rsi, %r8	# D.13128, D.13131
	movzbl	(%r8,%rax), %r10d	# MEM[base: _228, index: ivtmp.384_2, offset: 0B], D.13129
	addq	%rsi, %r8	# D.13128, D.13131
	movzbl	(%r8,%rax), %r15d	# MEM[base: _230, index: ivtmp.384_2, offset: 0B], D.13127
	addl	%r15d, %r9d	# D.13127, D.13127
	addl	%r9d, %r9d	# D.13127
	addq	%rsi, %r8	# D.13128, D.13131
	movzbl	(%r8,%rax), %r8d	# MEM[base: _232, index: ivtmp.384_2, offset: 0B], D.13127
	addl	%r8d, %r11d	# D.13127, D.13127
	subl	%r11d, %r9d	# D.13127, D.13127
	movzbl	%r10b, %r8d	# D.13129, t1
	leal	(%r8,%r8,2), %r8d	#, D.13127
	leal	4(%r9,%r8,2), %r8d	#, D.13127
	sarl	$3, %r8d	#, D.13127
	movl	%r8d, %r9d	# D.13127, D.13130
	testl	$-256, %r8d	#, D.13127
	je	.L96	#,
	negl	%r8d	# D.13127
	sarl	$31, %r8d	#, tmp515
	movl	%r8d, %r9d	# tmp515, D.13130
.L96:
	movq	-48(%rsp), %r15	# %sfp, D.13128
	leaq	(%rdi,%r15), %r8	#, D.13131
	movb	%r9b, (%r8,%rax)	# D.13130, MEM[base: _224, index: ivtmp.384_2, offset: 0B]
	movb	%r12b, (%rdx,%rax)	# D.13129, MEM[base: tmp_11(D), index: ivtmp.384_2, offset: 0B]
	movb	%r10b, (%rcx,%rax)	# D.13129, MEM[base: tmp2_15(D), index: ivtmp.384_2, offset: 0B]
	addq	$1, %rax	#, ivtmp.384
	cmpq	$8, %rax	#, ivtmp.384
	jne	.L97	#,
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
.LFE112:
	.size	deInterlaceL5_MMX, .-deInterlaceL5_MMX
	.type	deInterlaceBlendLinear_MMX, @function
deInterlaceBlendLinear_MMX:
.LFB113:
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
	leal	0(,%rsi,4), %r9d	#, D.13162
	movslq	%r9d, %rbp	# D.13162, D.13163
	addq	%rbp, %rdi	# D.13163, src
	leal	(%rsi,%rsi), %ecx	#, D.13162
	movslq	%ecx, %r13	# D.13162, D.13163
	addl	%esi, %ecx	# stride, D.13162
	movslq	%ecx, %r12	# D.13162, D.13163
	addl	%esi, %r9d	# stride, D.13162
	movslq	%r9d, %r9	# D.13162, D.13163
	addl	%ecx, %ecx	# tmp272
	movslq	%ecx, %rcx	# tmp272, D.13163
	leal	0(,%rsi,8), %ebx	#, tmp274
	movl	%ebx, %r11d	# tmp274, D.13162
	subl	%esi, %r11d	# stride, D.13162
	movslq	%r11d, %r11	# D.13162, D.13163
	movslq	%ebx, %rbx	# tmp274, D.13163
	leaq	8(%rdi), %r14	#, D.13167
	movslq	%esi, %rsi	# stride, D.13163
.L101:
	movl	(%rdi), %r8d	# MEM[base: src_205, offset: 0B], b
	movl	(%rdi,%rsi), %eax	# MEM[base: src_205, index: _16, offset: 0B], c
	movl	%eax, %r10d	# c, D.13162
	xorl	(%rdx), %r10d	# MEM[base: tmp_206, offset: 0B], D.13162
	andl	$-16843010, %r10d	#, D.13166
	movq	%r10, %r15	# D.13166, D.13166
	shrq	%r15	# D.13166
	movl	%eax, %r10d	# c, D.13162
	andl	(%rdx), %r10d	# MEM[base: tmp_206, offset: 0B], D.13162
	addl	%r15d, %r10d	# D.13166, D.13165
	movl	%r10d, %r15d	# D.13165, D.13162
	orl	%r8d, %r15d	# b, D.13162
	xorl	%r8d, %r10d	# b, D.13162
	andl	$-16843010, %r10d	#, D.13166
	shrq	%r10	# D.13166
	subl	%r10d, %r15d	# D.13166, tmp287
	movl	%r15d, (%rdi)	# tmp287, MEM[base: src_205, offset: 0B]
	movl	(%rdi,%r13), %r10d	# MEM[base: src_205, index: _39, offset: 0B], a
	movl	%r10d, %r15d	# a, D.13162
	xorl	%r8d, %r15d	# b, D.13162
	andl	$-16843010, %r15d	#, D.13166
	shrq	%r15	# D.13166
	andl	%r10d, %r8d	# a, D.13162
	addl	%r15d, %r8d	# D.13166, D.13165
	movl	%r8d, %r15d	# D.13165, D.13162
	orl	%eax, %r15d	# c, D.13162
	xorl	%eax, %r8d	# c, D.13162
	andl	$-16843010, %r8d	#, D.13166
	shrq	%r8	# D.13166
	subl	%r8d, %r15d	# D.13166, tmp298
	movl	%r15d, (%rdi,%rsi)	# tmp298, MEM[base: src_205, index: _16, offset: 0B]
	movl	(%rdi,%r12), %r8d	# MEM[base: src_205, index: _62, offset: 0B], b
	movl	%r8d, %r15d	# b, D.13162
	xorl	%eax, %r15d	# c, D.13162
	andl	$-16843010, %r15d	#, D.13166
	shrq	%r15	# D.13166
	andl	%r8d, %eax	# b, D.13162
	addl	%r15d, %eax	# D.13166, D.13165
	movl	%eax, %r15d	# D.13165, D.13162
	orl	%r10d, %r15d	# a, D.13162
	xorl	%r10d, %eax	# a, D.13162
	andl	$-16843010, %eax	#, D.13166
	shrq	%rax	# D.13166
	subl	%eax, %r15d	# D.13166, tmp309
	movl	%r15d, (%rdi,%r13)	# tmp309, MEM[base: src_205, index: _39, offset: 0B]
	movl	(%rdi,%rbp), %eax	# MEM[base: src_205, index: _7, offset: 0B], c
	movl	%eax, %r15d	# c, D.13162
	xorl	%r10d, %r15d	# a, D.13162
	andl	$-16843010, %r15d	#, D.13166
	shrq	%r15	# D.13166
	andl	%eax, %r10d	# c, D.13162
	addl	%r15d, %r10d	# D.13166, D.13165
	movl	%r10d, %r15d	# D.13165, D.13162
	orl	%r8d, %r15d	# b, D.13162
	xorl	%r8d, %r10d	# b, D.13162
	andl	$-16843010, %r10d	#, D.13166
	shrq	%r10	# D.13166
	subl	%r10d, %r15d	# D.13166, tmp320
	movl	%r15d, (%rdi,%r12)	# tmp320, MEM[base: src_205, index: _62, offset: 0B]
	movl	(%rdi,%r9), %r10d	# MEM[base: src_205, index: _106, offset: 0B], a
	movl	%r10d, %r15d	# a, D.13162
	xorl	%r8d, %r15d	# b, D.13162
	andl	$-16843010, %r15d	#, D.13166
	shrq	%r15	# D.13166
	andl	%r10d, %r8d	# a, D.13162
	addl	%r15d, %r8d	# D.13166, D.13165
	movl	%r8d, %r15d	# D.13165, D.13162
	orl	%eax, %r15d	# c, D.13162
	xorl	%eax, %r8d	# c, D.13162
	andl	$-16843010, %r8d	#, D.13166
	shrq	%r8	# D.13166
	subl	%r8d, %r15d	# D.13166, tmp331
	movl	%r15d, (%rdi,%rbp)	# tmp331, MEM[base: src_205, index: _7, offset: 0B]
	movl	(%rdi,%rcx), %r15d	# MEM[base: src_205, index: _129, offset: 0B], b
	movl	%r15d, %r8d	# b, D.13162
	xorl	%eax, %r8d	# c, D.13162
	andl	$-16843010, %r8d	#, D.13166
	shrq	%r8	# D.13166
	andl	%r15d, %eax	# b, D.13162
	addl	%r8d, %eax	# D.13166, D.13165
	movl	%eax, %r8d	# D.13165, D.13162
	orl	%r10d, %r8d	# a, D.13162
	xorl	%r10d, %eax	# a, D.13162
	andl	$-16843010, %eax	#, D.13166
	shrq	%rax	# D.13166
	subl	%eax, %r8d	# D.13166, tmp342
	movl	%r8d, (%rdi,%r9)	# tmp342, MEM[base: src_205, index: _106, offset: 0B]
	movl	(%rdi,%r11), %r8d	# MEM[base: src_205, index: _152, offset: 0B], D.13164
	movl	%r8d, %eax	# D.13164, D.13162
	xorl	%r10d, %eax	# a, D.13162
	andl	$-16843010, %eax	#, D.13166
	shrq	%rax	# D.13166
	andl	%r8d, %r10d	# D.13164, D.13162
	addl	%eax, %r10d	# D.13166, D.13165
	movl	%r10d, %eax	# D.13165, D.13162
	orl	%r15d, %eax	# b, D.13162
	xorl	%r15d, %r10d	# b, D.13162
	andl	$-16843010, %r10d	#, D.13166
	shrq	%r10	# D.13166
	subl	%r10d, %eax	# D.13166, tmp353
	movl	%eax, (%rdi,%rcx)	# tmp353, MEM[base: src_205, index: _129, offset: 0B]
	movl	(%rdi,%rbx), %eax	# MEM[base: src_205, index: _175, offset: 0B], a
	movl	%eax, %r10d	# a, D.13162
	xorl	%r15d, %r10d	# b, D.13162
	andl	$-16843010, %r10d	#, D.13166
	shrq	%r10	# D.13166
	andl	%eax, %r15d	# a, D.13162
	leal	(%r15,%r10), %eax	#, D.13165
	movl	%eax, %r10d	# D.13165, D.13162
	orl	%r8d, %r10d	# D.13164, D.13162
	xorl	%r8d, %eax	# D.13164, D.13162
	andl	$-16843010, %eax	#, D.13166
	shrq	%rax	# D.13166
	subl	%eax, %r10d	# D.13166, tmp364
	movl	%r10d, (%rdi,%r11)	# tmp364, MEM[base: src_205, index: _152, offset: 0B]
	movl	%r8d, (%rdx)	# D.13164, MEM[base: tmp_206, offset: 0B]
	addq	$4, %rdi	#, src
	addq	$4, %rdx	#, tmp
	cmpq	%r14, %rdi	# D.13167, src
	jne	.L101	#,
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
.LFE113:
	.size	deInterlaceBlendLinear_MMX, .-deInterlaceBlendLinear_MMX
	.type	deInterlaceMedian_MMX, @function
deInterlaceMedian_MMX:
.LFB114:
	.cfi_startproc
	leal	0(,%rsi,4), %eax	#, D.13174
	cltq
	addq	%rax, %rdi	# D.13175, src
	movslq	%esi, %rsi	# stride, D.13176
#APP
# 1926 "postprocess_template.c" 1
	lea (%rdi, %rsi), %rax                	# src, D.13176
	lea (%rax, %rsi, 4), %rdx      	# D.13176
	pxor %mm7, %mm7                      
	movq (%rdi), %mm0                     	# src
	movq (%rax), %mm2                     
	movq (%rax, %rsi), %mm1                     	# D.13176
	movq %mm0, %mm3                      
	movq %mm1, %mm4                      
	movq %mm2, %mm5                      
	psubusb %mm1, %mm3                   
	psubusb %mm2, %mm4                   
	psubusb %mm0, %mm5                   
	pcmpeqb %mm7, %mm3                   
	pcmpeqb %mm7, %mm4                   
	pcmpeqb %mm7, %mm5                   
	movq %mm3, %mm6                      
	pxor %mm4, %mm3                      
	pxor %mm5, %mm4                      
	pxor %mm6, %mm5                      
	por %mm3, %mm1                       
	por %mm4, %mm2                       
	por %mm5, %mm0                       
	pand %mm2, %mm0                      
	pand %mm1, %mm0                      
	movq %mm0, (%rax)                     
	movq (%rax, %rsi), %mm0                     	# D.13176
	movq (%rax, %rsi, 2), %mm2                     	# D.13176
	movq (%rdi, %rsi, 4), %mm1                     	# src, D.13176
	movq %mm0, %mm3                      
	movq %mm1, %mm4                      
	movq %mm2, %mm5                      
	psubusb %mm1, %mm3                   
	psubusb %mm2, %mm4                   
	psubusb %mm0, %mm5                   
	pcmpeqb %mm7, %mm3                   
	pcmpeqb %mm7, %mm4                   
	pcmpeqb %mm7, %mm5                   
	movq %mm3, %mm6                      
	pxor %mm4, %mm3                      
	pxor %mm5, %mm4                      
	pxor %mm6, %mm5                      
	por %mm3, %mm1                       
	por %mm4, %mm2                       
	por %mm5, %mm0                       
	pand %mm2, %mm0                      
	pand %mm1, %mm0                      
	movq %mm0, (%rax, %rsi, 2)                     	# D.13176
	movq (%rdi, %rsi, 4), %mm0                     	# src, D.13176
	movq (%rdx), %mm2                     
	movq (%rdx, %rsi), %mm1                     	# D.13176
	movq %mm0, %mm3                      
	movq %mm1, %mm4                      
	movq %mm2, %mm5                      
	psubusb %mm1, %mm3                   
	psubusb %mm2, %mm4                   
	psubusb %mm0, %mm5                   
	pcmpeqb %mm7, %mm3                   
	pcmpeqb %mm7, %mm4                   
	pcmpeqb %mm7, %mm5                   
	movq %mm3, %mm6                      
	pxor %mm4, %mm3                      
	pxor %mm5, %mm4                      
	pxor %mm6, %mm5                      
	por %mm3, %mm1                       
	por %mm4, %mm2                       
	por %mm5, %mm0                       
	pand %mm2, %mm0                      
	pand %mm1, %mm0                      
	movq %mm0, (%rdx)                     
	movq (%rdx, %rsi), %mm0                     	# D.13176
	movq (%rdx, %rsi, 2), %mm2                     	# D.13176
	movq (%rdi, %rsi, 8), %mm1                     	# src, D.13176
	movq %mm0, %mm3                      
	movq %mm1, %mm4                      
	movq %mm2, %mm5                      
	psubusb %mm1, %mm3                   
	psubusb %mm2, %mm4                   
	psubusb %mm0, %mm5                   
	pcmpeqb %mm7, %mm3                   
	pcmpeqb %mm7, %mm4                   
	pcmpeqb %mm7, %mm5                   
	movq %mm3, %mm6                      
	pxor %mm4, %mm3                      
	pxor %mm5, %mm4                      
	pxor %mm6, %mm5                      
	por %mm3, %mm1                       
	por %mm4, %mm2                       
	por %mm5, %mm0                       
	pand %mm2, %mm0                      
	pand %mm1, %mm0                      
	movq %mm0, (%rdx, %rsi, 2)                     	# D.13176
	
# 0 "" 2
#NO_APP
	ret
	.cfi_endproc
.LFE114:
	.size	deInterlaceMedian_MMX, .-deInterlaceMedian_MMX
	.type	tempNoiseReducer_MMX, @function
tempNoiseReducer_MMX:
.LFB117:
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
	movl	(%r8), %eax	# *maxNoise_24(D), *maxNoise_24(D)
	movl	%eax, 508(%rcx)	# *maxNoise_24(D), MEM[(uint32_t *)tempBlurredPast_22(D) + 508B]
	movl	4(%r8), %eax	# MEM[(const int *)maxNoise_24(D) + 4B], MEM[(const int *)maxNoise_24(D) + 4B]
	movl	%eax, 512(%rcx)	# MEM[(const int *)maxNoise_24(D) + 4B], MEM[(uint32_t *)tempBlurredPast_22(D) + 512B]
	movl	8(%r8), %eax	# MEM[(const int *)maxNoise_24(D) + 8B], MEM[(const int *)maxNoise_24(D) + 8B]
	movl	%eax, 516(%rcx)	# MEM[(const int *)maxNoise_24(D) + 8B], MEM[(uint32_t *)tempBlurredPast_22(D) + 516B]
	movl	$0, %r12d	#, ivtmp.497
	movl	$8, %ebx	#, ivtmp.437
	movl	$0, %r11d	#, d
	jmp	.L106	#
.L107:
	movslq	%r9d, %r10	# ivtmp.490, D.13280
	movzbl	(%rdx,%r10), %eax	# *_39, ref
	movzbl	(%rdi,%r10), %r10d	# *_43, cur
	subl	%r10d, %eax	# cur, d1
	imull	%eax, %eax	# d1, D.13278
	addl	%eax, %r11d	# D.13278, d
	addl	$1, %r9d	#, ivtmp.490
	cmpl	%ebp, %r9d	# D.13277, ivtmp.490
	jne	.L107	#,
	addl	%esi, %r12d	# D.13277, ivtmp.497
	subl	$1, %ebx	#, ivtmp.437
	je	.L108	#,
.L106:
	leal	8(%r12), %ebp	#, D.13277
	movl	%r12d, %r9d	# ivtmp.497, ivtmp.490
	jmp	.L107	#
.L108:
	movl	-4(%rcx), %eax	# MEM[(uint32_t *)tempBlurredPast_22(D) + -4B], MEM[(uint32_t *)tempBlurredPast_22(D) + -4B]
	addl	-1024(%rcx), %eax	# MEM[(uint32_t *)tempBlurredPast_22(D) + -1024B], D.13279
	movl	4(%rcx), %r9d	# MEM[(uint32_t *)tempBlurredPast_22(D) + 4B], MEM[(uint32_t *)tempBlurredPast_22(D) + 4B]
	leal	4(%rax,%r9), %eax	#, D.13279
	addl	1024(%rcx), %eax	# MEM[(uint32_t *)tempBlurredPast_22(D) + 1024B], D.13279
	leal	(%rax,%r11,4), %eax	#, D.13279
	shrl	$3, %eax	#, D.13279
	movl	%r11d, (%rcx)	# d, *tempBlurredPast_22(D)
	cmpl	4(%r8), %eax	# MEM[(const int *)maxNoise_24(D) + 4B], D.13279
	jle	.L109	#,
	movl	$8, %r10d	#, D.13277
	movl	$8, %ebp	#, D.13277
	cmpl	8(%r8), %eax	# MEM[(const int *)maxNoise_24(D) + 8B], D.13279
	jl	.L112	#,
	jmp	.L111	#
.L113:
	movslq	%r8d, %rax	# ivtmp.430, D.13280
	leaq	(%rdx,%rax), %r9	#, D.13281
	addq	%rdi, %rax	# src, D.13281
	movzbl	(%r9), %r10d	# *_81, ref
	movzbl	(%rax), %ecx	# *_84, cur
	leal	1(%r10,%rcx), %ecx	#, D.13278
	sarl	%ecx	# D.13278
	movb	%cl, (%rax)	# D.13278, *_84
	movb	%cl, (%r9)	# D.13278, *_81
	addl	$1, %r8d	#, ivtmp.430
	cmpl	%r11d, %r8d	# D.13277, ivtmp.430
	jne	.L113	#,
	addl	%esi, %ebx	# D.13277, ivtmp.437
	subl	$1, %ebp	#, D.13277
	je	.L105	#,
.L112:
	leal	8(%rbx), %r11d	#, D.13277
	movl	%ebx, %r8d	# ivtmp.437, ivtmp.430
	jmp	.L113	#
.L115:
	movslq	%eax, %rcx	# ivtmp.445, D.13280
	movzbl	(%rdi,%rcx), %r8d	# *_73, D.13282
	movb	%r8b, (%rdx,%rcx)	# D.13282, *_72
	addl	$1, %eax	#, ivtmp.445
	cmpl	%r9d, %eax	# D.13277, ivtmp.445
	jne	.L115	#,
	addl	%esi, %ebx	# D.13277, ivtmp.437
	subl	$1, %r10d	#, D.13277
	je	.L105	#,
.L111:
	leal	8(%rbx), %r9d	#, D.13277
	movl	%ebx, %eax	# ivtmp.437, ivtmp.445
	jmp	.L115	#
.L109:
	movl	$8, %ebp	#, D.13277
	cmpl	(%r8), %eax	# *maxNoise_24(D), D.13279
	jge	.L117	#,
.L116:
	movl	$8, %ebp	#, D.13277
	jmp	.L118	#
.L119:
	movslq	%r8d, %rax	# ivtmp.460, D.13280
	leaq	(%rdx,%rax), %r9	#, D.13281
	addq	%rdi, %rax	# src, D.13281
	movzbl	(%r9), %r10d	# *_99, ref
	leal	0(,%r10,8), %ecx	#, tmp230
	subl	%r10d, %ecx	# ref, D.13278
	movzbl	(%rax), %r10d	# *_105, cur
	leal	4(%rcx,%r10), %ecx	#, D.13278
	sarl	$3, %ecx	#, D.13278
	movb	%cl, (%rax)	# D.13278, *_105
	movb	%cl, (%r9)	# D.13278, *_99
	addl	$1, %r8d	#, ivtmp.460
	cmpl	%r11d, %r8d	# D.13277, ivtmp.460
	jne	.L119	#,
	addl	%esi, %ebx	# D.13277, ivtmp.437
	subl	$1, %ebp	#, D.13277
	je	.L105	#,
.L118:
	leal	8(%rbx), %r11d	#, D.13277
	movl	%ebx, %r8d	# ivtmp.437, ivtmp.460
	jmp	.L119	#
.L120:
	movslq	%r8d, %rax	# ivtmp.475, D.13280
	leaq	(%rdx,%rax), %r9	#, D.13281
	addq	%rdi, %rax	# src, D.13281
	movzbl	(%r9), %ecx	# *_123, ref
	leal	(%rcx,%rcx,2), %r10d	#, D.13278
	movzbl	(%rax), %ecx	# *_129, cur
	leal	2(%r10,%rcx), %ecx	#, D.13278
	sarl	$2, %ecx	#, D.13278
	movb	%cl, (%rax)	# D.13278, *_129
	movb	%cl, (%r9)	# D.13278, *_123
	addl	$1, %r8d	#, ivtmp.475
	cmpl	%r11d, %r8d	# D.13277, ivtmp.475
	jne	.L120	#,
	addl	%esi, %ebx	# D.13277, ivtmp.437
	subl	$1, %ebp	#, D.13277
	je	.L105	#,
.L117:
	leal	8(%rbx), %r11d	#, D.13277
	movl	%ebx, %r8d	# ivtmp.437, ivtmp.475
	jmp	.L120	#
.L105:
	popq	%rbx	#
	.cfi_def_cfa_offset 24
	popq	%rbp	#
	.cfi_def_cfa_offset 16
	popq	%r12	#
	.cfi_def_cfa_offset 8
	ret
	.cfi_endproc
.LFE117:
	.size	tempNoiseReducer_MMX, .-tempNoiseReducer_MMX
	.type	doVertLowPass_MMX2, @function
doVertLowPass_MMX2:
.LFB123:
	.cfi_startproc
	leal	(%rsi,%rsi,2), %eax	#, D.13290
	cltq
	addq	%rax, %rdi	# D.13291, src
	movslq	%esi, %rsi	# stride, D.13292
#APP
# 232 "postprocess_template.c" 1
	movq 112(%rdx), %mm0                         	# c_8(D)->pQPb
	pxor %mm4, %mm4                      
	movq (%rdi), %mm6                       	# src
	movq (%rdi, %rsi), %mm5                   	# src, D.13292
	movq %mm5, %mm1                      
	movq %mm6, %mm2                      
	psubusb %mm6, %mm5                   
	psubusb %mm1, %mm2                   
	por %mm5, %mm2                       
	psubusb %mm0, %mm2                   
	pcmpeqb %mm4, %mm2                   
	pand %mm2, %mm6                      
	pandn %mm1, %mm2                     
	por %mm2, %mm6                       
	movq (%rdi, %rsi, 8), %mm5                	# src, D.13292
	lea (%rdi, %rsi, 4), %rax             	# src, D.13292
	lea (%rdi, %rsi, 8), %rcx             	# src, D.13292
	sub %rsi, %rcx                      	# D.13292
	add %rsi, %rdi                             	# D.13292, src
	movq (%rdi, %rsi, 8), %mm7                	# src, D.13292
	movq %mm5, %mm1                      
	movq %mm7, %mm2                      
	psubusb %mm7, %mm5                   
	psubusb %mm1, %mm2                   
	por %mm5, %mm2                       
	psubusb %mm0, %mm2                   
	pcmpeqb %mm4, %mm2                   
	pand %mm2, %mm7                      
	pandn %mm1, %mm2                     
	por %mm2, %mm7                       
	movq (%rdi, %rsi), %mm0                   	# src, D.13292
	movq %mm0, %mm1                      
	pavgb %mm6, %mm0 
	pavgb %mm6, %mm0 
	movq (%rdi, %rsi, 4), %mm2                	# src, D.13292
	movq %mm2, %mm5                      
	pavgb (%rax), %mm2 
	pavgb (%rdi, %rsi, 2), %mm2 	# src, D.13292
	movq %mm2, %mm3                      
	movq (%rdi), %mm4                       	# src
	pavgb %mm4, %mm3 
	pavgb %mm0, %mm3 
	movq %mm3, (%rdi)                       	# src
	movq %mm1, %mm0                      
	pavgb %mm6, %mm0 
	movq %mm4, %mm3                      
	pavgb (%rdi,%rsi,2), %mm3 	# src, D.13292
	pavgb (%rax,%rsi,2), %mm5 	# D.13292
	pavgb (%rax), %mm5 
	pavgb %mm5, %mm3 
	pavgb %mm0, %mm3 
	movq %mm3, (%rdi,%rsi)                    	# src, D.13292
	pavgb %mm4, %mm6 
	movq (%rcx), %mm0                
	pavgb (%rax, %rsi, 2), %mm0 	# D.13292
	movq %mm0, %mm3                      
	pavgb %mm1, %mm0 
	pavgb %mm6, %mm0 
	pavgb %mm2, %mm0 
	movq (%rdi, %rsi, 2), %mm2                	# src, D.13292
	movq %mm0, (%rdi, %rsi, 2)                	# src, D.13292
	movq (%rax, %rsi, 4), %mm0         	# D.13292
	pavgb (%rcx), %mm0 
	pavgb %mm0, %mm6 
	pavgb %mm1, %mm4 
	pavgb %mm2, %mm1 
	pavgb %mm1, %mm6 
	pavgb %mm5, %mm6 
	movq (%rax), %mm5                
	movq %mm6, (%rax)                
	movq (%rax, %rsi, 4), %mm6         	# D.13292
	pavgb %mm7, %mm6 
	pavgb %mm4, %mm6 
	pavgb %mm3, %mm6 
	pavgb %mm5, %mm2 
	movq (%rdi, %rsi, 4), %mm4                	# src, D.13292
	pavgb %mm4, %mm2 
	pavgb %mm2, %mm6 
	movq %mm6, (%rdi, %rsi, 4)                	# src, D.13292
	pavgb %mm7, %mm1 
	pavgb %mm4, %mm5 
	pavgb %mm5, %mm0 
	movq (%rax, %rsi, 2), %mm6         	# D.13292
	pavgb %mm6, %mm1 
	pavgb %mm0, %mm1 
	movq %mm1, (%rax, %rsi, 2)         	# D.13292
	pavgb (%rcx), %mm2 
	movq (%rax, %rsi, 4), %mm0         	# D.13292
	pavgb %mm0, %mm6 
	pavgb %mm7, %mm6 
	pavgb %mm2, %mm6 
	movq %mm6, (%rcx)                
	pavgb %mm7, %mm5 
	pavgb %mm7, %mm5 
	pavgb %mm3, %mm0 
	pavgb %mm0, %mm5 
	movq %mm5, (%rax, %rsi, 4)         	# D.13292
	sub %rsi, %rdi                             	# D.13292, src
	
# 0 "" 2
#NO_APP
	ret
	.cfi_endproc
.LFE123:
	.size	doVertLowPass_MMX2, .-doVertLowPass_MMX2
	.type	dering_MMX2, @function
dering_MMX2:
.LFB126:
	.cfi_startproc
	movq	%rdx, %rcx	# c, c
	movslq	%esi, %rsi	# stride, D.13294
	leaq	-32(%rsp), %r8	#, tmp88
#APP
# 1097 "postprocess_template.c" 1
	pxor %mm6, %mm6                      
	pcmpeqb %mm7, %mm7                   
	movq 112(%rcx), %mm0                         	# c_5(D)->pQPb
	punpcklbw %mm6, %mm0                 
	psrlw $1, %mm0                        
	psubw %mm7, %mm0                     
	packuswb %mm0, %mm0                  
	movq %mm0, 120(%rcx)                         	# c_5(D)->pQPb2
	lea (%rdi, %rsi), %rax                	# src, D.13294
	lea (%rax, %rsi, 4), %rdx      	# D.13294
	movq (%rax), %mm0                  
	pminub %mm0, %mm7                    
	pmaxub %mm0, %mm6                    
	movq (%rax, %rsi), %mm0                  	# D.13294
	pminub %mm0, %mm7                    
	pmaxub %mm0, %mm6                    
	movq (%rax, %rsi, 2), %mm0                  	# D.13294
	pminub %mm0, %mm7                    
	pmaxub %mm0, %mm6                    
	movq (%rdi, %rsi, 4), %mm0                  	# src, D.13294
	pminub %mm0, %mm7                    
	pmaxub %mm0, %mm6                    
	movq (%rdx), %mm0                  
	pminub %mm0, %mm7                    
	pmaxub %mm0, %mm6                    
	movq (%rdx, %rsi), %mm0                  	# D.13294
	pminub %mm0, %mm7                    
	pmaxub %mm0, %mm6                    
	movq (%rdx, %rsi, 2), %mm0                  	# D.13294
	pminub %mm0, %mm7                    
	pmaxub %mm0, %mm6                    
	movq (%rdi, %rsi, 8), %mm0                  	# src, D.13294
	pminub %mm0, %mm7                    
	pmaxub %mm0, %mm6                    
	movq %mm7, %mm4                      
	psrlq $8, %mm7                        
	pminub %mm4, %mm7                    
	pshufw $0xF9, %mm7, %mm4             
	pminub %mm4, %mm7                    
	pshufw $0xFE, %mm7, %mm4             
	pminub %mm4, %mm7                    
	movq %mm6, %mm4                      
	psrlq $8, %mm6                        
	pmaxub %mm4, %mm6                    
	pshufw $0xF9, %mm6, %mm4             
	pmaxub %mm4, %mm6                    
	pshufw $0xFE, %mm6, %mm4             
	pmaxub %mm4, %mm6                    
	movq %mm6, %mm0                      
	psubb %mm7, %mm6                     
	push %r8                              	# tmp88
	movd %mm6, %r8d                        	# tmp88
	cmpb deringThreshold, %r8b    	# tmp88
	pop %r8                               	# tmp88
	 jb .skip                                 
	pavgb %mm0, %mm7 
	punpcklbw %mm7, %mm7                 
	punpcklbw %mm7, %mm7                 
	punpcklbw %mm7, %mm7                 
	movq %mm7, (%r8)                       	# tmp88
	movq (%rdi), %mm0                       	# src
	movq %mm0, %mm1                      
	movq %mm0, %mm2                      
	psllq $8, %mm1                        
	psrlq $8, %mm2                        
.skip:
	movd -4(%rdi), %mm3                     	# src
	movd 8(%rdi), %mm4                      	# src
	psrlq $24, %mm3                       
	psllq $56, %mm4                       
	por %mm3, %mm1                       
	por %mm4, %mm2                       
	movq %mm1, %mm3                      
	pavgb %mm2, %mm1 
	pavgb %mm0, %mm1 
	psubusb %mm7, %mm0                   
	psubusb %mm7, %mm2                   
	psubusb %mm7, %mm3                   
	pcmpeqb b00, %mm0           
	pcmpeqb b00, %mm2           
	pcmpeqb b00, %mm3           
	paddb %mm2, %mm0                     
	paddb %mm3, %mm0                     
	movq (%rax), %mm2                
	movq %mm2, %mm3                      
	movq %mm2, %mm4                      
	psllq $8, %mm3                        
	psrlq $8, %mm4                        
	movd -4(%rax), %mm5              
	movd 8(%rax), %mm6               
	psrlq $24, %mm5                       
	psllq $56, %mm6                       
	por %mm5, %mm3                       
	por %mm6, %mm4                       
	movq %mm3, %mm5                      
	pavgb %mm4, %mm3 
	pavgb %mm2, %mm3 
	psubusb %mm7, %mm2                   
	psubusb %mm7, %mm4                   
	psubusb %mm7, %mm5                   
	pcmpeqb b00, %mm2           
	pcmpeqb b00, %mm4           
	pcmpeqb b00, %mm5           
	paddb %mm4, %mm2                     
	paddb %mm5, %mm2                     
	movq (%rax, %rsi), %mm4                 	# D.13294
	movq %mm4, %mm5                  
	movq %mm4, %mm6                  
	psllq $8, %mm5                      
	psrlq $8, %mm6                      
	movd -4(%rax, %rsi), %mm7               	# D.13294
	psrlq $24, %mm7                     
	por %mm7, %mm5                   
	movd 8(%rax, %rsi), %mm7                	# D.13294
	psllq $56, %mm7                     
	por %mm7, %mm6                   
	movq %mm5, %mm7                  
	pavgb %mm6, %mm5 
	pavgb %mm4, %mm5 
	pavgb %mm5, %mm1 
	movq %mm5, 8(%r8)                    	# tmp88
	movq (%r8), %mm5                     	# tmp88
	psubusb %mm5, %mm7               
	psubusb %mm5, %mm6               
	psubusb %mm5, %mm4               
	movq b00, %mm5            
	pcmpeqb %mm5, %mm7               
	pcmpeqb %mm5, %mm6               
	pcmpeqb %mm5, %mm4               
	paddb %mm7, %mm6                 
	paddb %mm6, %mm4                 
	pavgb %mm3, %mm1 
	movq (%rax), %mm6                 
	movq %mm6, %mm7                  
	psubusb 120(%rcx), %mm6                    	# c_5(D)->pQPb2
	paddusb 120(%rcx), %mm7                    	# c_5(D)->pQPb2
	pmaxub %mm6, %mm1 
	pminub %mm7, %mm1 
	paddb %mm4, %mm0               
	paddb %mm2, %mm0              
	#paddb b02, %mm0        
	pand b08, %mm0          
	pcmpeqb %mm5, %mm0             
	pand %mm0, %mm1              
	pandn (%rax), %mm0              
	por %mm1, %mm0               
	movq %mm0, (%rax)               
	movq 8(%r8), %mm5                    	# tmp88
	movq (%rax, %rsi, 2), %mm0                 	# D.13294
	movq %mm0, %mm1                  
	movq %mm0, %mm6                  
	psllq $8, %mm1                      
	psrlq $8, %mm6                      
	movd -4(%rax, %rsi, 2), %mm7               	# D.13294
	psrlq $24, %mm7                     
	por %mm7, %mm1                   
	movd 8(%rax, %rsi, 2), %mm7                	# D.13294
	psllq $56, %mm7                     
	por %mm7, %mm6                   
	movq %mm1, %mm7                  
	pavgb %mm6, %mm1 
	pavgb %mm0, %mm1 
	pavgb %mm1, %mm3 
	movq %mm1, 8(%r8)                    	# tmp88
	movq (%r8), %mm1                     	# tmp88
	psubusb %mm1, %mm7               
	psubusb %mm1, %mm6               
	psubusb %mm1, %mm0               
	movq b00, %mm1            
	pcmpeqb %mm1, %mm7               
	pcmpeqb %mm1, %mm6               
	pcmpeqb %mm1, %mm0               
	paddb %mm7, %mm6                 
	paddb %mm6, %mm0                 
	pavgb %mm5, %mm3 
	movq (%rax, %rsi), %mm6                 	# D.13294
	movq %mm6, %mm7                  
	psubusb 120(%rcx), %mm6                    	# c_5(D)->pQPb2
	paddusb 120(%rcx), %mm7                    	# c_5(D)->pQPb2
	pmaxub %mm6, %mm3 
	pminub %mm7, %mm3 
	paddb %mm0, %mm2               
	paddb %mm4, %mm2              
	#paddb b02, %mm2        
	pand b08, %mm2          
	pcmpeqb %mm1, %mm2             
	pand %mm2, %mm3              
	pandn (%rax, %rsi), %mm2              	# D.13294
	por %mm3, %mm2               
	movq %mm2, (%rax, %rsi)               	# D.13294
	movq 8(%r8), %mm1                    	# tmp88
	movq (%rdi, %rsi, 4), %mm2                 	# src, D.13294
	movq %mm2, %mm3                  
	movq %mm2, %mm6                  
	psllq $8, %mm3                      
	psrlq $8, %mm6                      
	movd -4(%rdi, %rsi, 4), %mm7               	# src, D.13294
	psrlq $24, %mm7                     
	por %mm7, %mm3                   
	movd 8(%rdi, %rsi, 4), %mm7                	# src, D.13294
	psllq $56, %mm7                     
	por %mm7, %mm6                   
	movq %mm3, %mm7                  
	pavgb %mm6, %mm3 
	pavgb %mm2, %mm3 
	pavgb %mm3, %mm5 
	movq %mm3, 8(%r8)                    	# tmp88
	movq (%r8), %mm3                     	# tmp88
	psubusb %mm3, %mm7               
	psubusb %mm3, %mm6               
	psubusb %mm3, %mm2               
	movq b00, %mm3            
	pcmpeqb %mm3, %mm7               
	pcmpeqb %mm3, %mm6               
	pcmpeqb %mm3, %mm2               
	paddb %mm7, %mm6                 
	paddb %mm6, %mm2                 
	pavgb %mm1, %mm5 
	movq (%rax, %rsi, 2), %mm6                 	# D.13294
	movq %mm6, %mm7                  
	psubusb 120(%rcx), %mm6                    	# c_5(D)->pQPb2
	paddusb 120(%rcx), %mm7                    	# c_5(D)->pQPb2
	pmaxub %mm6, %mm5 
	pminub %mm7, %mm5 
	paddb %mm2, %mm4               
	paddb %mm0, %mm4              
	#paddb b02, %mm4        
	pand b08, %mm4          
	pcmpeqb %mm3, %mm4             
	pand %mm4, %mm5              
	pandn (%rax, %rsi, 2), %mm4              	# D.13294
	por %mm5, %mm4               
	movq %mm4, (%rax, %rsi, 2)               	# D.13294
	movq 8(%r8), %mm3                    	# tmp88
	movq (%rdx), %mm4                 
	movq %mm4, %mm5                  
	movq %mm4, %mm6                  
	psllq $8, %mm5                      
	psrlq $8, %mm6                      
	movd -4(%rdx), %mm7               
	psrlq $24, %mm7                     
	por %mm7, %mm5                   
	movd 8(%rdx), %mm7                
	psllq $56, %mm7                     
	por %mm7, %mm6                   
	movq %mm5, %mm7                  
	pavgb %mm6, %mm5 
	pavgb %mm4, %mm5 
	pavgb %mm5, %mm1 
	movq %mm5, 8(%r8)                    	# tmp88
	movq (%r8), %mm5                     	# tmp88
	psubusb %mm5, %mm7               
	psubusb %mm5, %mm6               
	psubusb %mm5, %mm4               
	movq b00, %mm5            
	pcmpeqb %mm5, %mm7               
	pcmpeqb %mm5, %mm6               
	pcmpeqb %mm5, %mm4               
	paddb %mm7, %mm6                 
	paddb %mm6, %mm4                 
	pavgb %mm3, %mm1 
	movq (%rdi, %rsi, 4), %mm6                 	# src, D.13294
	movq %mm6, %mm7                  
	psubusb 120(%rcx), %mm6                    	# c_5(D)->pQPb2
	paddusb 120(%rcx), %mm7                    	# c_5(D)->pQPb2
	pmaxub %mm6, %mm1 
	pminub %mm7, %mm1 
	paddb %mm4, %mm0               
	paddb %mm2, %mm0              
	#paddb b02, %mm0        
	pand b08, %mm0          
	pcmpeqb %mm5, %mm0             
	pand %mm0, %mm1              
	pandn (%rdi, %rsi, 4), %mm0              	# src, D.13294
	por %mm1, %mm0               
	movq %mm0, (%rdi, %rsi, 4)               	# src, D.13294
	movq 8(%r8), %mm5                    	# tmp88
	movq (%rdx, %rsi), %mm0                 	# D.13294
	movq %mm0, %mm1                  
	movq %mm0, %mm6                  
	psllq $8, %mm1                      
	psrlq $8, %mm6                      
	movd -4(%rdx, %rsi), %mm7               	# D.13294
	psrlq $24, %mm7                     
	por %mm7, %mm1                   
	movd 8(%rdx, %rsi), %mm7                	# D.13294
	psllq $56, %mm7                     
	por %mm7, %mm6                   
	movq %mm1, %mm7                  
	pavgb %mm6, %mm1 
	pavgb %mm0, %mm1 
	pavgb %mm1, %mm3 
	movq %mm1, 8(%r8)                    	# tmp88
	movq (%r8), %mm1                     	# tmp88
	psubusb %mm1, %mm7               
	psubusb %mm1, %mm6               
	psubusb %mm1, %mm0               
	movq b00, %mm1            
	pcmpeqb %mm1, %mm7               
	pcmpeqb %mm1, %mm6               
	pcmpeqb %mm1, %mm0               
	paddb %mm7, %mm6                 
	paddb %mm6, %mm0                 
	pavgb %mm5, %mm3 
	movq (%rdx), %mm6                 
	movq %mm6, %mm7                  
	psubusb 120(%rcx), %mm6                    	# c_5(D)->pQPb2
	paddusb 120(%rcx), %mm7                    	# c_5(D)->pQPb2
	pmaxub %mm6, %mm3 
	pminub %mm7, %mm3 
	paddb %mm0, %mm2               
	paddb %mm4, %mm2              
	#paddb b02, %mm2        
	pand b08, %mm2          
	pcmpeqb %mm1, %mm2             
	pand %mm2, %mm3              
	pandn (%rdx), %mm2              
	por %mm3, %mm2               
	movq %mm2, (%rdx)               
	movq 8(%r8), %mm1                    	# tmp88
	movq (%rdx, %rsi, 2), %mm2                 	# D.13294
	movq %mm2, %mm3                  
	movq %mm2, %mm6                  
	psllq $8, %mm3                      
	psrlq $8, %mm6                      
	movd -4(%rdx, %rsi, 2), %mm7               	# D.13294
	psrlq $24, %mm7                     
	por %mm7, %mm3                   
	movd 8(%rdx, %rsi, 2), %mm7                	# D.13294
	psllq $56, %mm7                     
	por %mm7, %mm6                   
	movq %mm3, %mm7                  
	pavgb %mm6, %mm3 
	pavgb %mm2, %mm3 
	pavgb %mm3, %mm5 
	movq %mm3, 8(%r8)                    	# tmp88
	movq (%r8), %mm3                     	# tmp88
	psubusb %mm3, %mm7               
	psubusb %mm3, %mm6               
	psubusb %mm3, %mm2               
	movq b00, %mm3            
	pcmpeqb %mm3, %mm7               
	pcmpeqb %mm3, %mm6               
	pcmpeqb %mm3, %mm2               
	paddb %mm7, %mm6                 
	paddb %mm6, %mm2                 
	pavgb %mm1, %mm5 
	movq (%rdx, %rsi), %mm6                 	# D.13294
	movq %mm6, %mm7                  
	psubusb 120(%rcx), %mm6                    	# c_5(D)->pQPb2
	paddusb 120(%rcx), %mm7                    	# c_5(D)->pQPb2
	pmaxub %mm6, %mm5 
	pminub %mm7, %mm5 
	paddb %mm2, %mm4               
	paddb %mm0, %mm4              
	#paddb b02, %mm4        
	pand b08, %mm4          
	pcmpeqb %mm3, %mm4             
	pand %mm4, %mm5              
	pandn (%rdx, %rsi), %mm4              	# D.13294
	por %mm5, %mm4               
	movq %mm4, (%rdx, %rsi)               	# D.13294
	movq 8(%r8), %mm3                    	# tmp88
	movq (%rdi, %rsi, 8), %mm4                 	# src, D.13294
	movq %mm4, %mm5                  
	movq %mm4, %mm6                  
	psllq $8, %mm5                      
	psrlq $8, %mm6                      
	movd -4(%rdi, %rsi, 8), %mm7               	# src, D.13294
	psrlq $24, %mm7                     
	por %mm7, %mm5                   
	movd 8(%rdi, %rsi, 8), %mm7                	# src, D.13294
	psllq $56, %mm7                     
	por %mm7, %mm6                   
	movq %mm5, %mm7                  
	pavgb %mm6, %mm5 
	pavgb %mm4, %mm5 
	pavgb %mm5, %mm1 
	movq %mm5, 8(%r8)                    	# tmp88
	movq (%r8), %mm5                     	# tmp88
	psubusb %mm5, %mm7               
	psubusb %mm5, %mm6               
	psubusb %mm5, %mm4               
	movq b00, %mm5            
	pcmpeqb %mm5, %mm7               
	pcmpeqb %mm5, %mm6               
	pcmpeqb %mm5, %mm4               
	paddb %mm7, %mm6                 
	paddb %mm6, %mm4                 
	pavgb %mm3, %mm1 
	movq (%rdx, %rsi, 2), %mm6                 	# D.13294
	movq %mm6, %mm7                  
	psubusb 120(%rcx), %mm6                    	# c_5(D)->pQPb2
	paddusb 120(%rcx), %mm7                    	# c_5(D)->pQPb2
	pmaxub %mm6, %mm1 
	pminub %mm7, %mm1 
	paddb %mm4, %mm0               
	paddb %mm2, %mm0              
	#paddb b02, %mm0        
	pand b08, %mm0          
	pcmpeqb %mm5, %mm0             
	pand %mm0, %mm1              
	pandn (%rdx, %rsi, 2), %mm0              	# D.13294
	por %mm1, %mm0               
	movq %mm0, (%rdx, %rsi, 2)               	# D.13294
	movq 8(%r8), %mm5                    	# tmp88
	movq (%rdx, %rsi, 4), %mm0                 	# D.13294
	movq %mm0, %mm1                  
	movq %mm0, %mm6                  
	psllq $8, %mm1                      
	psrlq $8, %mm6                      
	movd -4(%rdx, %rsi, 4), %mm7               	# D.13294
	psrlq $24, %mm7                     
	por %mm7, %mm1                   
	movd 8(%rdx, %rsi, 4), %mm7                	# D.13294
	psllq $56, %mm7                     
	por %mm7, %mm6                   
	movq %mm1, %mm7                  
	pavgb %mm6, %mm1 
	pavgb %mm0, %mm1 
	pavgb %mm1, %mm3 
	movq %mm1, 8(%r8)                    	# tmp88
	movq (%r8), %mm1                     	# tmp88
	psubusb %mm1, %mm7               
	psubusb %mm1, %mm6               
	psubusb %mm1, %mm0               
	movq b00, %mm1            
	pcmpeqb %mm1, %mm7               
	pcmpeqb %mm1, %mm6               
	pcmpeqb %mm1, %mm0               
	paddb %mm7, %mm6                 
	paddb %mm6, %mm0                 
	pavgb %mm5, %mm3 
	movq (%rdi, %rsi, 8), %mm6                 	# src, D.13294
	movq %mm6, %mm7                  
	psubusb 120(%rcx), %mm6                    	# c_5(D)->pQPb2
	paddusb 120(%rcx), %mm7                    	# c_5(D)->pQPb2
	pmaxub %mm6, %mm3 
	pminub %mm7, %mm3 
	paddb %mm0, %mm2               
	paddb %mm4, %mm2              
	#paddb b02, %mm2        
	pand b08, %mm2          
	pcmpeqb %mm1, %mm2             
	pand %mm2, %mm3              
	pandn (%rdi, %rsi, 8), %mm2              	# src, D.13294
	por %mm3, %mm2               
	movq %mm2, (%rdi, %rsi, 8)               	# src, D.13294
	movq 8(%r8), %mm1                    	# tmp88
	1:                        
	
# 0 "" 2
#NO_APP
	ret
	.cfi_endproc
.LFE126:
	.size	dering_MMX2, .-dering_MMX2
	.type	deInterlaceInterpolateCubic_MMX2, @function
deInterlaceInterpolateCubic_MMX2:
.LFB128:
	.cfi_startproc
	leal	(%rsi,%rsi,2), %eax	#, D.13301
	cltq
	addq	%rax, %rdi	# D.13302, src
	movslq	%esi, %rsi	# stride, D.13303
#APP
# 1508 "postprocess_template.c" 1
	lea (%rdi, %rsi), %rax                	# src, D.13303
	lea (%rax, %rsi, 4), %rdx      	# D.13303
	lea (%rdx, %rsi, 4), %rcx      	# D.13303
	add %rsi, %rcx                      	# D.13303
	pxor %mm7, %mm7                      
	movq (%rdi), %mm0                     	# src
	movq (%rax, %rsi), %mm1                     	# D.13303
	movq (%rdi, %rsi, 4), %mm2                     	# src, D.13303
	movq (%rdx, %rsi), %mm3                     	# D.13303
	pavgb %mm2, %mm1 
	pavgb %mm3, %mm0 
	movq %mm0, %mm2                      
	punpcklbw %mm7, %mm0                 
	punpckhbw %mm7, %mm2                 
	movq %mm1, %mm3                      
	punpcklbw %mm7, %mm1                 
	punpckhbw %mm7, %mm3                 
	psubw %mm1, %mm0                     
	psubw %mm3, %mm2                     
	psraw $3, %mm0                        
	psraw $3, %mm2                        
	psubw %mm0, %mm1                     
	psubw %mm2, %mm3                     
	packuswb %mm3, %mm1                  
	movq %mm1, (%rax, %rsi, 2)                     	# D.13303
	movq (%rax, %rsi), %mm0                     	# D.13303
	movq (%rdi, %rsi, 4), %mm1                     	# src, D.13303
	movq (%rdx, %rsi), %mm2                     	# D.13303
	movq (%rdi, %rsi, 8), %mm3                     	# src, D.13303
	pavgb %mm2, %mm1 
	pavgb %mm3, %mm0 
	movq %mm0, %mm2                      
	punpcklbw %mm7, %mm0                 
	punpckhbw %mm7, %mm2                 
	movq %mm1, %mm3                      
	punpcklbw %mm7, %mm1                 
	punpckhbw %mm7, %mm3                 
	psubw %mm1, %mm0                     
	psubw %mm3, %mm2                     
	psraw $3, %mm0                        
	psraw $3, %mm2                        
	psubw %mm0, %mm1                     
	psubw %mm2, %mm3                     
	packuswb %mm3, %mm1                  
	movq %mm1, (%rdx)                     
	movq (%rdi, %rsi, 4), %mm0                     	# src, D.13303
	movq (%rdx, %rsi), %mm1                     	# D.13303
	movq (%rdi, %rsi, 8), %mm2                     	# src, D.13303
	movq (%rcx), %mm3                     
	pavgb %mm2, %mm1 
	pavgb %mm3, %mm0 
	movq %mm0, %mm2                      
	punpcklbw %mm7, %mm0                 
	punpckhbw %mm7, %mm2                 
	movq %mm1, %mm3                      
	punpcklbw %mm7, %mm1                 
	punpckhbw %mm7, %mm3                 
	psubw %mm1, %mm0                     
	psubw %mm3, %mm2                     
	psraw $3, %mm0                        
	psraw $3, %mm2                        
	psubw %mm0, %mm1                     
	psubw %mm2, %mm3                     
	packuswb %mm3, %mm1                  
	movq %mm1, (%rdx, %rsi, 2)                     	# D.13303
	movq (%rdx, %rsi), %mm0                     	# D.13303
	movq (%rdi, %rsi, 8), %mm1                     	# src, D.13303
	movq (%rcx), %mm2                     
	movq (%rcx, %rsi, 2), %mm3                     	# D.13303
	pavgb %mm2, %mm1 
	pavgb %mm3, %mm0 
	movq %mm0, %mm2                      
	punpcklbw %mm7, %mm0                 
	punpckhbw %mm7, %mm2                 
	movq %mm1, %mm3                      
	punpcklbw %mm7, %mm1                 
	punpckhbw %mm7, %mm3                 
	psubw %mm1, %mm0                     
	psubw %mm3, %mm2                     
	psraw $3, %mm0                        
	psraw $3, %mm2                        
	psubw %mm0, %mm1                     
	psubw %mm2, %mm3                     
	packuswb %mm3, %mm1                  
	movq %mm1, (%rdx, %rsi, 4)                     	# D.13303
	
# 0 "" 2
#NO_APP
	ret
	.cfi_endproc
.LFE128:
	.size	deInterlaceInterpolateCubic_MMX2, .-deInterlaceInterpolateCubic_MMX2
	.type	deInterlaceFF_MMX2, @function
deInterlaceFF_MMX2:
.LFB129:
	.cfi_startproc
	movq	%rdx, %rcx	# tmp, tmp
	leal	0(,%rsi,4), %eax	#, D.13311
	cltq
	addq	%rax, %rdi	# D.13312, src
	movslq	%esi, %rsi	# stride, D.13313
#APP
# 1595 "postprocess_template.c" 1
	lea (%rdi, %rsi), %rax                	# src, D.13313
	lea (%rax, %rsi, 4), %rdx      	# D.13313
	pxor %mm7, %mm7                      
	movq (%rcx), %mm0                       	# tmp
	movq (%rdi), %mm1                     	# src
	movq (%rax), %mm2                     
	movq (%rax, %rsi), %mm3                     	# D.13313
	movq (%rax, %rsi, 2), %mm4                     	# D.13313
	pavgb %mm3, %mm1 
	pavgb %mm4, %mm0 
	movq %mm0, %mm3                      
	punpcklbw %mm7, %mm0                 
	punpckhbw %mm7, %mm3                 
	movq %mm1, %mm4                      
	punpcklbw %mm7, %mm1                 
	punpckhbw %mm7, %mm4                 
	psllw $2, %mm1                        
	psllw $2, %mm4                        
	psubw %mm0, %mm1                     
	psubw %mm3, %mm4                     
	movq %mm2, %mm5                      
	movq %mm2, %mm0                      
	punpcklbw %mm7, %mm2                 
	punpckhbw %mm7, %mm5                 
	paddw %mm2, %mm1                     
	paddw %mm5, %mm4                     
	psraw $2, %mm1                        
	psraw $2, %mm4                        
	packuswb %mm4, %mm1                  
	movq %mm1, (%rax)                     
	movq (%rax, %rsi), %mm1                     	# D.13313
	movq (%rax, %rsi, 2), %mm2                     	# D.13313
	movq (%rdi, %rsi, 4), %mm3                     	# src, D.13313
	movq (%rdx), %mm4                     
	pavgb %mm3, %mm1 
	pavgb %mm4, %mm0 
	movq %mm0, %mm3                      
	punpcklbw %mm7, %mm0                 
	punpckhbw %mm7, %mm3                 
	movq %mm1, %mm4                      
	punpcklbw %mm7, %mm1                 
	punpckhbw %mm7, %mm4                 
	psllw $2, %mm1                        
	psllw $2, %mm4                        
	psubw %mm0, %mm1                     
	psubw %mm3, %mm4                     
	movq %mm2, %mm5                      
	movq %mm2, %mm0                      
	punpcklbw %mm7, %mm2                 
	punpckhbw %mm7, %mm5                 
	paddw %mm2, %mm1                     
	paddw %mm5, %mm4                     
	psraw $2, %mm1                        
	psraw $2, %mm4                        
	packuswb %mm4, %mm1                  
	movq %mm1, (%rax, %rsi, 2)                     	# D.13313
	movq (%rdi, %rsi, 4), %mm1                     	# src, D.13313
	movq (%rdx), %mm2                     
	movq (%rdx, %rsi), %mm3                     	# D.13313
	movq (%rdx, %rsi, 2), %mm4                     	# D.13313
	pavgb %mm3, %mm1 
	pavgb %mm4, %mm0 
	movq %mm0, %mm3                      
	punpcklbw %mm7, %mm0                 
	punpckhbw %mm7, %mm3                 
	movq %mm1, %mm4                      
	punpcklbw %mm7, %mm1                 
	punpckhbw %mm7, %mm4                 
	psllw $2, %mm1                        
	psllw $2, %mm4                        
	psubw %mm0, %mm1                     
	psubw %mm3, %mm4                     
	movq %mm2, %mm5                      
	movq %mm2, %mm0                      
	punpcklbw %mm7, %mm2                 
	punpckhbw %mm7, %mm5                 
	paddw %mm2, %mm1                     
	paddw %mm5, %mm4                     
	psraw $2, %mm1                        
	psraw $2, %mm4                        
	packuswb %mm4, %mm1                  
	movq %mm1, (%rdx)                     
	movq (%rdx, %rsi), %mm1                     	# D.13313
	movq (%rdx, %rsi, 2), %mm2                     	# D.13313
	movq (%rdi, %rsi, 8), %mm3                     	# src, D.13313
	movq (%rdx, %rsi, 4), %mm4                     	# D.13313
	pavgb %mm3, %mm1 
	pavgb %mm4, %mm0 
	movq %mm0, %mm3                      
	punpcklbw %mm7, %mm0                 
	punpckhbw %mm7, %mm3                 
	movq %mm1, %mm4                      
	punpcklbw %mm7, %mm1                 
	punpckhbw %mm7, %mm4                 
	psllw $2, %mm1                        
	psllw $2, %mm4                        
	psubw %mm0, %mm1                     
	psubw %mm3, %mm4                     
	movq %mm2, %mm5                      
	movq %mm2, %mm0                      
	punpcklbw %mm7, %mm2                 
	punpckhbw %mm7, %mm5                 
	paddw %mm2, %mm1                     
	paddw %mm5, %mm4                     
	psraw $2, %mm1                        
	psraw $2, %mm4                        
	packuswb %mm4, %mm1                  
	movq %mm1, (%rdx, %rsi, 2)                     	# D.13313
	movq %mm0, (%rcx)                       	# tmp
	
# 0 "" 2
#NO_APP
	ret
	.cfi_endproc
.LFE129:
	.size	deInterlaceFF_MMX2, .-deInterlaceFF_MMX2
	.type	deInterlaceL5_MMX2, @function
deInterlaceL5_MMX2:
.LFB130:
	.cfi_startproc
	movq	%rdx, %r8	# tmp, tmp
	leal	0(,%rsi,4), %eax	#, D.13315
	cltq
	addq	%rax, %rdi	# D.13316, src
	movslq	%esi, %rsi	# stride, D.13317
#APP
# 1674 "postprocess_template.c" 1
	lea (%rdi, %rsi), %rax                	# src, D.13317
	lea (%rax, %rsi, 4), %rdx      	# D.13317
	pxor %mm7, %mm7                      
	movq (%r8), %mm0                       	# tmp
	movq (%rcx), %mm1                       	# tmp2
	movq (%rdi), %mm2                     	# src
	movq (%rax), %mm3                     
	movq (%rax, %rsi), %mm4                     	# D.13317
	pavgb %mm1, %mm3 
	pavgb %mm0, %mm4 
	movq %mm2, %mm5                      
	movq %mm2, %mm0                    
	punpcklbw %mm7, %mm2                 
	punpckhbw %mm7, %mm5                 
	movq %mm2, %mm6                      
	paddw %mm2, %mm2                     
	paddw %mm6, %mm2                     
	movq %mm5, %mm6                      
	paddw %mm5, %mm5                     
	paddw %mm6, %mm5                     
	movq %mm3, %mm6                      
	punpcklbw %mm7, %mm3                 
	punpckhbw %mm7, %mm6                 
	paddw %mm3, %mm3                     
	paddw %mm6, %mm6                     
	paddw %mm3, %mm2                     
	paddw %mm6, %mm5                     
	movq %mm4, %mm6                      
	punpcklbw %mm7, %mm4                 
	punpckhbw %mm7, %mm6                 
	psubw %mm4, %mm2                     
	psubw %mm6, %mm5                     
	psraw $2, %mm2                        
	psraw $2, %mm5                        
	packuswb %mm5, %mm2                  
	movq %mm2, (%rdi)                     	# src
	movq (%rax), %mm2                     
	movq (%rax, %rsi), %mm3                     	# D.13317
	movq (%rax, %rsi, 2), %mm4                     	# D.13317
	pavgb %mm0, %mm3 
	pavgb %mm1, %mm4 
	movq %mm2, %mm5                      
	movq %mm2, %mm1                    
	punpcklbw %mm7, %mm2                 
	punpckhbw %mm7, %mm5                 
	movq %mm2, %mm6                      
	paddw %mm2, %mm2                     
	paddw %mm6, %mm2                     
	movq %mm5, %mm6                      
	paddw %mm5, %mm5                     
	paddw %mm6, %mm5                     
	movq %mm3, %mm6                      
	punpcklbw %mm7, %mm3                 
	punpckhbw %mm7, %mm6                 
	paddw %mm3, %mm3                     
	paddw %mm6, %mm6                     
	paddw %mm3, %mm2                     
	paddw %mm6, %mm5                     
	movq %mm4, %mm6                      
	punpcklbw %mm7, %mm4                 
	punpckhbw %mm7, %mm6                 
	psubw %mm4, %mm2                     
	psubw %mm6, %mm5                     
	psraw $2, %mm2                        
	psraw $2, %mm5                        
	packuswb %mm5, %mm2                  
	movq %mm2, (%rax)                     
	movq (%rax, %rsi), %mm2                     	# D.13317
	movq (%rax, %rsi, 2), %mm3                     	# D.13317
	movq (%rdi, %rsi, 4), %mm4                     	# src, D.13317
	pavgb %mm1, %mm3 
	pavgb %mm0, %mm4 
	movq %mm2, %mm5                      
	movq %mm2, %mm0                    
	punpcklbw %mm7, %mm2                 
	punpckhbw %mm7, %mm5                 
	movq %mm2, %mm6                      
	paddw %mm2, %mm2                     
	paddw %mm6, %mm2                     
	movq %mm5, %mm6                      
	paddw %mm5, %mm5                     
	paddw %mm6, %mm5                     
	movq %mm3, %mm6                      
	punpcklbw %mm7, %mm3                 
	punpckhbw %mm7, %mm6                 
	paddw %mm3, %mm3                     
	paddw %mm6, %mm6                     
	paddw %mm3, %mm2                     
	paddw %mm6, %mm5                     
	movq %mm4, %mm6                      
	punpcklbw %mm7, %mm4                 
	punpckhbw %mm7, %mm6                 
	psubw %mm4, %mm2                     
	psubw %mm6, %mm5                     
	psraw $2, %mm2                        
	psraw $2, %mm5                        
	packuswb %mm5, %mm2                  
	movq %mm2, (%rax, %rsi)                     	# D.13317
	movq (%rax, %rsi, 2), %mm2                     	# D.13317
	movq (%rdi, %rsi, 4), %mm3                     	# src, D.13317
	movq (%rdx), %mm4                     
	pavgb %mm0, %mm3 
	pavgb %mm1, %mm4 
	movq %mm2, %mm5                      
	movq %mm2, %mm1                    
	punpcklbw %mm7, %mm2                 
	punpckhbw %mm7, %mm5                 
	movq %mm2, %mm6                      
	paddw %mm2, %mm2                     
	paddw %mm6, %mm2                     
	movq %mm5, %mm6                      
	paddw %mm5, %mm5                     
	paddw %mm6, %mm5                     
	movq %mm3, %mm6                      
	punpcklbw %mm7, %mm3                 
	punpckhbw %mm7, %mm6                 
	paddw %mm3, %mm3                     
	paddw %mm6, %mm6                     
	paddw %mm3, %mm2                     
	paddw %mm6, %mm5                     
	movq %mm4, %mm6                      
	punpcklbw %mm7, %mm4                 
	punpckhbw %mm7, %mm6                 
	psubw %mm4, %mm2                     
	psubw %mm6, %mm5                     
	psraw $2, %mm2                        
	psraw $2, %mm5                        
	packuswb %mm5, %mm2                  
	movq %mm2, (%rax, %rsi, 2)                     	# D.13317
	movq (%rdi, %rsi, 4), %mm2                     	# src, D.13317
	movq (%rdx), %mm3                     
	movq (%rdx, %rsi), %mm4                     	# D.13317
	pavgb %mm1, %mm3 
	pavgb %mm0, %mm4 
	movq %mm2, %mm5                      
	movq %mm2, %mm0                    
	punpcklbw %mm7, %mm2                 
	punpckhbw %mm7, %mm5                 
	movq %mm2, %mm6                      
	paddw %mm2, %mm2                     
	paddw %mm6, %mm2                     
	movq %mm5, %mm6                      
	paddw %mm5, %mm5                     
	paddw %mm6, %mm5                     
	movq %mm3, %mm6                      
	punpcklbw %mm7, %mm3                 
	punpckhbw %mm7, %mm6                 
	paddw %mm3, %mm3                     
	paddw %mm6, %mm6                     
	paddw %mm3, %mm2                     
	paddw %mm6, %mm5                     
	movq %mm4, %mm6                      
	punpcklbw %mm7, %mm4                 
	punpckhbw %mm7, %mm6                 
	psubw %mm4, %mm2                     
	psubw %mm6, %mm5                     
	psraw $2, %mm2                        
	psraw $2, %mm5                        
	packuswb %mm5, %mm2                  
	movq %mm2, (%rdi, %rsi, 4)                     	# src, D.13317
	movq (%rdx), %mm2                     
	movq (%rdx, %rsi), %mm3                     	# D.13317
	movq (%rdx, %rsi, 2), %mm4                     	# D.13317
	pavgb %mm0, %mm3 
	pavgb %mm1, %mm4 
	movq %mm2, %mm5                      
	movq %mm2, %mm1                    
	punpcklbw %mm7, %mm2                 
	punpckhbw %mm7, %mm5                 
	movq %mm2, %mm6                      
	paddw %mm2, %mm2                     
	paddw %mm6, %mm2                     
	movq %mm5, %mm6                      
	paddw %mm5, %mm5                     
	paddw %mm6, %mm5                     
	movq %mm3, %mm6                      
	punpcklbw %mm7, %mm3                 
	punpckhbw %mm7, %mm6                 
	paddw %mm3, %mm3                     
	paddw %mm6, %mm6                     
	paddw %mm3, %mm2                     
	paddw %mm6, %mm5                     
	movq %mm4, %mm6                      
	punpcklbw %mm7, %mm4                 
	punpckhbw %mm7, %mm6                 
	psubw %mm4, %mm2                     
	psubw %mm6, %mm5                     
	psraw $2, %mm2                        
	psraw $2, %mm5                        
	packuswb %mm5, %mm2                  
	movq %mm2, (%rdx)                     
	movq (%rdx, %rsi), %mm2                     	# D.13317
	movq (%rdx, %rsi, 2), %mm3                     	# D.13317
	movq (%rdi, %rsi, 8), %mm4                     	# src, D.13317
	pavgb %mm1, %mm3 
	pavgb %mm0, %mm4 
	movq %mm2, %mm5                      
	movq %mm2, %mm0                    
	punpcklbw %mm7, %mm2                 
	punpckhbw %mm7, %mm5                 
	movq %mm2, %mm6                      
	paddw %mm2, %mm2                     
	paddw %mm6, %mm2                     
	movq %mm5, %mm6                      
	paddw %mm5, %mm5                     
	paddw %mm6, %mm5                     
	movq %mm3, %mm6                      
	punpcklbw %mm7, %mm3                 
	punpckhbw %mm7, %mm6                 
	paddw %mm3, %mm3                     
	paddw %mm6, %mm6                     
	paddw %mm3, %mm2                     
	paddw %mm6, %mm5                     
	movq %mm4, %mm6                      
	punpcklbw %mm7, %mm4                 
	punpckhbw %mm7, %mm6                 
	psubw %mm4, %mm2                     
	psubw %mm6, %mm5                     
	psraw $2, %mm2                        
	psraw $2, %mm5                        
	packuswb %mm5, %mm2                  
	movq %mm2, (%rdx, %rsi)                     	# D.13317
	movq (%rdx, %rsi, 2), %mm2                     	# D.13317
	movq (%rdi, %rsi, 8), %mm3                     	# src, D.13317
	movq (%rdx, %rsi, 4), %mm4                     	# D.13317
	pavgb %mm0, %mm3 
	pavgb %mm1, %mm4 
	movq %mm2, %mm5                      
	movq %mm2, %mm1                    
	punpcklbw %mm7, %mm2                 
	punpckhbw %mm7, %mm5                 
	movq %mm2, %mm6                      
	paddw %mm2, %mm2                     
	paddw %mm6, %mm2                     
	movq %mm5, %mm6                      
	paddw %mm5, %mm5                     
	paddw %mm6, %mm5                     
	movq %mm3, %mm6                      
	punpcklbw %mm7, %mm3                 
	punpckhbw %mm7, %mm6                 
	paddw %mm3, %mm3                     
	paddw %mm6, %mm6                     
	paddw %mm3, %mm2                     
	paddw %mm6, %mm5                     
	movq %mm4, %mm6                      
	punpcklbw %mm7, %mm4                 
	punpckhbw %mm7, %mm6                 
	psubw %mm4, %mm2                     
	psubw %mm6, %mm5                     
	psraw $2, %mm2                        
	psraw $2, %mm5                        
	packuswb %mm5, %mm2                  
	movq %mm2, (%rdx, %rsi, 2)                     	# D.13317
	movq %mm0, (%r8)                       	# tmp
	movq %mm1, (%rcx)                       	# tmp2
	
# 0 "" 2
#NO_APP
	ret
	.cfi_endproc
.LFE130:
	.size	deInterlaceL5_MMX2, .-deInterlaceL5_MMX2
	.type	tempNoiseReducer_MMX2, @function
tempNoiseReducer_MMX2:
.LFB135:
	.cfi_startproc
	movq	%rdx, %r9	# tempBlurred, tempBlurred
	movq	%rcx, -8(%rsp)	# tempBlurredPast, tempBlurredPast
	movl	(%r8), %eax	# *maxNoise_3(D), *maxNoise_3(D)
	movl	%eax, 508(%rcx)	# *maxNoise_3(D), MEM[(uint32_t *)_2 + 508B]
	movl	4(%r8), %eax	# MEM[(const int *)maxNoise_3(D) + 4B], MEM[(const int *)maxNoise_3(D) + 4B]
	movl	%eax, 512(%rcx)	# MEM[(const int *)maxNoise_3(D) + 4B], MEM[(uint32_t *)_2 + 512B]
	movl	8(%r8), %eax	# MEM[(const int *)maxNoise_3(D) + 8B], MEM[(const int *)maxNoise_3(D) + 8B]
	movl	%eax, 516(%rcx)	# MEM[(const int *)maxNoise_3(D) + 8B], MEM[(uint32_t *)_2 + 516B]
	movslq	%esi, %rsi	# stride, D.13330
#APP
# 2169 "postprocess_template.c" 1
	lea (%rsi, %rsi, 2), %rax             	# D.13330
	lea (%rsi, %rsi, 4), %rdx             	# D.13330
	lea (%rdx, %rsi, 2), %rcx      	# D.13330
	pcmpeqb %mm7, %mm7                   
	movq b80, %mm6              
	pxor %mm0, %mm0                      
	movq (%rdi), %mm5                     	# src
	movq (%r9), %mm2                     	# tempBlurred
	pxor %mm7, %mm2                      
	pavgb %mm2, %mm5 
	paddb %mm6, %mm5                     
	movq %mm5, %mm2                      
	psllw $8, %mm5                        
	pmaddwd %mm5, %mm5                   
	pmaddwd %mm2, %mm2                   
	paddd %mm2, %mm5                     
	psrld $14, %mm5                       
	paddd %mm5, %mm0                     
	movq (%rdi, %rsi), %mm5                     	# src, D.13330
	movq (%r9, %rsi), %mm2                     	# tempBlurred, D.13330
	pxor %mm7, %mm2                      
	pavgb %mm2, %mm5 
	paddb %mm6, %mm5                     
	movq %mm5, %mm2                      
	psllw $8, %mm5                        
	pmaddwd %mm5, %mm5                   
	pmaddwd %mm2, %mm2                   
	paddd %mm2, %mm5                     
	psrld $14, %mm5                       
	paddd %mm5, %mm0                     
	movq (%rdi, %rsi, 2), %mm5                     	# src, D.13330
	movq (%r9, %rsi, 2), %mm2                     	# tempBlurred, D.13330
	pxor %mm7, %mm2                      
	pavgb %mm2, %mm5 
	paddb %mm6, %mm5                     
	movq %mm5, %mm2                      
	psllw $8, %mm5                        
	pmaddwd %mm5, %mm5                   
	pmaddwd %mm2, %mm2                   
	paddd %mm2, %mm5                     
	psrld $14, %mm5                       
	paddd %mm5, %mm0                     
	movq (%rdi, %rax), %mm5                     	# src
	movq (%r9, %rax), %mm2                     	# tempBlurred
	pxor %mm7, %mm2                      
	pavgb %mm2, %mm5 
	paddb %mm6, %mm5                     
	movq %mm5, %mm2                      
	psllw $8, %mm5                        
	pmaddwd %mm5, %mm5                   
	pmaddwd %mm2, %mm2                   
	paddd %mm2, %mm5                     
	psrld $14, %mm5                       
	paddd %mm5, %mm0                     
	movq (%rdi, %rsi, 4), %mm5                     	# src, D.13330
	movq (%r9, %rsi, 4), %mm2                     	# tempBlurred, D.13330
	pxor %mm7, %mm2                      
	pavgb %mm2, %mm5 
	paddb %mm6, %mm5                     
	movq %mm5, %mm2                      
	psllw $8, %mm5                        
	pmaddwd %mm5, %mm5                   
	pmaddwd %mm2, %mm2                   
	paddd %mm2, %mm5                     
	psrld $14, %mm5                       
	paddd %mm5, %mm0                     
	movq (%rdi, %rdx), %mm5                     	# src
	movq (%r9, %rdx), %mm2                     	# tempBlurred
	pxor %mm7, %mm2                      
	pavgb %mm2, %mm5 
	paddb %mm6, %mm5                     
	movq %mm5, %mm2                      
	psllw $8, %mm5                        
	pmaddwd %mm5, %mm5                   
	pmaddwd %mm2, %mm2                   
	paddd %mm2, %mm5                     
	psrld $14, %mm5                       
	paddd %mm5, %mm0                     
	movq (%rdi, %rax,2), %mm5                     	# src
	movq (%r9, %rax,2), %mm2                     	# tempBlurred
	pxor %mm7, %mm2                      
	pavgb %mm2, %mm5 
	paddb %mm6, %mm5                     
	movq %mm5, %mm2                      
	psllw $8, %mm5                        
	pmaddwd %mm5, %mm5                   
	pmaddwd %mm2, %mm2                   
	paddd %mm2, %mm5                     
	psrld $14, %mm5                       
	paddd %mm5, %mm0                     
	movq (%rdi, %rcx), %mm5                     	# src
	movq (%r9, %rcx), %mm2                     	# tempBlurred
	pxor %mm7, %mm2                      
	pavgb %mm2, %mm5 
	paddb %mm6, %mm5                     
	movq %mm5, %mm2                      
	psllw $8, %mm5                        
	pmaddwd %mm5, %mm5                   
	pmaddwd %mm2, %mm2                   
	paddd %mm2, %mm5                     
	psrld $14, %mm5                       
	paddd %mm5, %mm0                     
	movq %mm0, %mm4                      
	psrlq $32, %mm0                       
	paddd %mm0, %mm4                     
	movd %mm4, %ecx                      
	shll $2, %ecx                         
	mov -8(%rsp), %rdx                      	# tempBlurredPast
	addl -4(%rdx), %ecx              
	addl 4(%rdx), %ecx               
	addl -1024(%rdx), %ecx           
	addl $4, %ecx                         
	addl 1024(%rdx), %ecx            
	shrl $3, %ecx                         
	movl %ecx, (%rdx)                
	cmpl 512(%rdx), %ecx             
	 jb 2f                                 
	cmpl 516(%rdx), %ecx             
	 jb 1f                                 
	lea (%rax, %rsi, 2), %rdx      	# D.13330
	lea (%rdx, %rsi, 2), %rcx      	# D.13330
	movq (%rdi), %mm0                       	# src
	movq (%rdi, %rsi), %mm1                   	# src, D.13330
	movq (%rdi, %rsi, 2), %mm2                	# src, D.13330
	movq (%rdi, %rax), %mm3            	# src
	movq (%rdi, %rsi, 4), %mm4                	# src, D.13330
	movq (%rdi, %rdx), %mm5            	# src
	movq (%rdi, %rax, 2), %mm6         	# src
	movq (%rdi, %rcx), %mm7            	# src
	movq %mm0, (%r9)                       	# tempBlurred
	movq %mm1, (%r9, %rsi)                   	# tempBlurred, D.13330
	movq %mm2, (%r9, %rsi, 2)                	# tempBlurred, D.13330
	movq %mm3, (%r9, %rax)            	# tempBlurred
	movq %mm4, (%r9, %rsi, 4)                	# tempBlurred, D.13330
	movq %mm5, (%r9, %rdx)            	# tempBlurred
	movq %mm6, (%r9, %rax, 2)         	# tempBlurred
	movq %mm7, (%r9, %rcx)            	# tempBlurred
	jmp 4f                                 
	1:                                     
	lea (%rax, %rsi, 2), %rdx      	# D.13330
	lea (%rdx, %rsi, 2), %rcx      	# D.13330
	movq (%rdi), %mm0                       	# src
	pavgb (%r9), %mm0 	# tempBlurred
	movq (%rdi, %rsi), %mm1                   	# src, D.13330
	pavgb (%r9, %rsi), %mm1 	# tempBlurred, D.13330
	movq (%rdi, %rsi, 2), %mm2                	# src, D.13330
	pavgb (%r9, %rsi, 2), %mm2 	# tempBlurred, D.13330
	movq (%rdi, %rax), %mm3            	# src
	pavgb (%r9, %rax), %mm3 	# tempBlurred
	movq (%rdi, %rsi, 4), %mm4                	# src, D.13330
	pavgb (%r9, %rsi, 4), %mm4 	# tempBlurred, D.13330
	movq (%rdi, %rdx), %mm5            	# src
	pavgb (%r9, %rdx), %mm5 	# tempBlurred
	movq (%rdi, %rax, 2), %mm6         	# src
	pavgb (%r9, %rax, 2), %mm6 	# tempBlurred
	movq (%rdi, %rcx), %mm7            	# src
	pavgb (%r9, %rcx), %mm7 	# tempBlurred
	movq %mm0, (%r9)                       	# tempBlurred
	movq %mm1, (%r9, %rsi)                   	# tempBlurred, D.13330
	movq %mm2, (%r9, %rsi, 2)                	# tempBlurred, D.13330
	movq %mm3, (%r9, %rax)            	# tempBlurred
	movq %mm4, (%r9, %rsi, 4)                	# tempBlurred, D.13330
	movq %mm5, (%r9, %rdx)            	# tempBlurred
	movq %mm6, (%r9, %rax, 2)         	# tempBlurred
	movq %mm7, (%r9, %rcx)            	# tempBlurred
	movq %mm0, (%rdi)                       	# src
	movq %mm1, (%rdi, %rsi)                   	# src, D.13330
	movq %mm2, (%rdi, %rsi, 2)                	# src, D.13330
	movq %mm3, (%rdi, %rax)            	# src
	movq %mm4, (%rdi, %rsi, 4)                	# src, D.13330
	movq %mm5, (%rdi, %rdx)            	# src
	movq %mm6, (%rdi, %rax, 2)         	# src
	movq %mm7, (%rdi, %rcx)            	# src
	jmp 4f                                 
	2:                                     
	cmpl 508(%rdx), %ecx             
	 jb 3f                                 
	lea (%rax, %rsi, 2), %rdx      	# D.13330
	lea (%rdx, %rsi, 2), %rcx      	# D.13330
	movq (%rdi), %mm0                       	# src
	movq (%rdi, %rsi), %mm1                   	# src, D.13330
	movq (%rdi, %rsi, 2), %mm2                	# src, D.13330
	movq (%rdi, %rax), %mm3            	# src
	movq (%r9), %mm4                       	# tempBlurred
	movq (%r9, %rsi), %mm5                   	# tempBlurred, D.13330
	movq (%r9, %rsi, 2), %mm6                	# tempBlurred, D.13330
	movq (%r9, %rax), %mm7            	# tempBlurred
	pavgb %mm4, %mm0 
	pavgb %mm5, %mm1 
	pavgb %mm6, %mm2 
	pavgb %mm7, %mm3 
	pavgb %mm4, %mm0 
	pavgb %mm5, %mm1 
	pavgb %mm6, %mm2 
	pavgb %mm7, %mm3 
	movq %mm0, (%r9)                       	# tempBlurred
	movq %mm1, (%r9, %rsi)                   	# tempBlurred, D.13330
	movq %mm2, (%r9, %rsi, 2)                	# tempBlurred, D.13330
	movq %mm3, (%r9, %rax)            	# tempBlurred
	movq %mm0, (%rdi)                       	# src
	movq %mm1, (%rdi, %rsi)                   	# src, D.13330
	movq %mm2, (%rdi, %rsi, 2)                	# src, D.13330
	movq %mm3, (%rdi, %rax)            	# src
	movq (%rdi, %rsi, 4), %mm0                	# src, D.13330
	movq (%rdi, %rdx), %mm1            	# src
	movq (%rdi, %rax, 2), %mm2         	# src
	movq (%rdi, %rcx), %mm3            	# src
	movq (%r9, %rsi, 4), %mm4                	# tempBlurred, D.13330
	movq (%r9, %rdx), %mm5            	# tempBlurred
	movq (%r9, %rax, 2), %mm6         	# tempBlurred
	movq (%r9, %rcx), %mm7            	# tempBlurred
	pavgb %mm4, %mm0 
	pavgb %mm5, %mm1 
	pavgb %mm6, %mm2 
	pavgb %mm7, %mm3 
	pavgb %mm4, %mm0 
	pavgb %mm5, %mm1 
	pavgb %mm6, %mm2 
	pavgb %mm7, %mm3 
	movq %mm0, (%r9, %rsi, 4)                	# tempBlurred, D.13330
	movq %mm1, (%r9, %rdx)            	# tempBlurred
	movq %mm2, (%r9, %rax, 2)         	# tempBlurred
	movq %mm3, (%r9, %rcx)            	# tempBlurred
	movq %mm0, (%rdi, %rsi, 4)                	# src, D.13330
	movq %mm1, (%rdi, %rdx)            	# src
	movq %mm2, (%rdi, %rax, 2)         	# src
	movq %mm3, (%rdi, %rcx)            	# src
	jmp 4f                                 
	3:                                     
	lea (%rax, %rsi, 2), %rdx      	# D.13330
	lea (%rdx, %rsi, 2), %rcx      	# D.13330
	movq (%rdi), %mm0                       	# src
	movq (%rdi, %rsi), %mm1                   	# src, D.13330
	movq (%rdi, %rsi, 2), %mm2                	# src, D.13330
	movq (%rdi, %rax), %mm3            	# src
	movq (%r9), %mm4                       	# tempBlurred
	movq (%r9, %rsi), %mm5                   	# tempBlurred, D.13330
	movq (%r9, %rsi, 2), %mm6                	# tempBlurred, D.13330
	movq (%r9, %rax), %mm7            	# tempBlurred
	pavgb %mm4, %mm0 
	pavgb %mm5, %mm1 
	pavgb %mm6, %mm2 
	pavgb %mm7, %mm3 
	pavgb %mm4, %mm0 
	pavgb %mm5, %mm1 
	pavgb %mm6, %mm2 
	pavgb %mm7, %mm3 
	pavgb %mm4, %mm0 
	pavgb %mm5, %mm1 
	pavgb %mm6, %mm2 
	pavgb %mm7, %mm3 
	movq %mm0, (%r9)                       	# tempBlurred
	movq %mm1, (%r9, %rsi)                   	# tempBlurred, D.13330
	movq %mm2, (%r9, %rsi, 2)                	# tempBlurred, D.13330
	movq %mm3, (%r9, %rax)            	# tempBlurred
	movq %mm0, (%rdi)                       	# src
	movq %mm1, (%rdi, %rsi)                   	# src, D.13330
	movq %mm2, (%rdi, %rsi, 2)                	# src, D.13330
	movq %mm3, (%rdi, %rax)            	# src
	movq (%rdi, %rsi, 4), %mm0                	# src, D.13330
	movq (%rdi, %rdx), %mm1            	# src
	movq (%rdi, %rax, 2), %mm2         	# src
	movq (%rdi, %rcx), %mm3            	# src
	movq (%r9, %rsi, 4), %mm4                	# tempBlurred, D.13330
	movq (%r9, %rdx), %mm5            	# tempBlurred
	movq (%r9, %rax, 2), %mm6         	# tempBlurred
	movq (%r9, %rcx), %mm7            	# tempBlurred
	pavgb %mm4, %mm0 
	pavgb %mm5, %mm1 
	pavgb %mm6, %mm2 
	pavgb %mm7, %mm3 
	pavgb %mm4, %mm0 
	pavgb %mm5, %mm1 
	pavgb %mm6, %mm2 
	pavgb %mm7, %mm3 
	pavgb %mm4, %mm0 
	pavgb %mm5, %mm1 
	pavgb %mm6, %mm2 
	pavgb %mm7, %mm3 
	movq %mm0, (%r9, %rsi, 4)                	# tempBlurred, D.13330
	movq %mm1, (%r9, %rdx)            	# tempBlurred
	movq %mm2, (%r9, %rax, 2)         	# tempBlurred
	movq %mm3, (%r9, %rcx)            	# tempBlurred
	movq %mm0, (%rdi, %rsi, 4)                	# src, D.13330
	movq %mm1, (%rdi, %rdx)            	# src
	movq %mm2, (%rdi, %rax, 2)         	# src
	movq %mm3, (%rdi, %rcx)            	# src
	4:                                     
	
# 0 "" 2
#NO_APP
	ret
	.cfi_endproc
.LFE135:
	.size	tempNoiseReducer_MMX2, .-tempNoiseReducer_MMX2
	.type	doVertLowPass_3DNow, @function
doVertLowPass_3DNow:
.LFB141:
	.cfi_startproc
	leal	(%rsi,%rsi,2), %eax	#, D.13332
	cltq
	addq	%rax, %rdi	# D.13333, src
	movslq	%esi, %rsi	# stride, D.13334
#APP
# 232 "postprocess_template.c" 1
	movq 112(%rdx), %mm0                         	# c_8(D)->pQPb
	pxor %mm4, %mm4                      
	movq (%rdi), %mm6                       	# src
	movq (%rdi, %rsi), %mm5                   	# src, D.13334
	movq %mm5, %mm1                      
	movq %mm6, %mm2                      
	psubusb %mm6, %mm5                   
	psubusb %mm1, %mm2                   
	por %mm5, %mm2                       
	psubusb %mm0, %mm2                   
	pcmpeqb %mm4, %mm2                   
	pand %mm2, %mm6                      
	pandn %mm1, %mm2                     
	por %mm2, %mm6                       
	movq (%rdi, %rsi, 8), %mm5                	# src, D.13334
	lea (%rdi, %rsi, 4), %rax             	# src, D.13334
	lea (%rdi, %rsi, 8), %rcx             	# src, D.13334
	sub %rsi, %rcx                      	# D.13334
	add %rsi, %rdi                             	# D.13334, src
	movq (%rdi, %rsi, 8), %mm7                	# src, D.13334
	movq %mm5, %mm1                      
	movq %mm7, %mm2                      
	psubusb %mm7, %mm5                   
	psubusb %mm1, %mm2                   
	por %mm5, %mm2                       
	psubusb %mm0, %mm2                   
	pcmpeqb %mm4, %mm2                   
	pand %mm2, %mm7                      
	pandn %mm1, %mm2                     
	por %mm2, %mm7                       
	movq (%rdi, %rsi), %mm0                   	# src, D.13334
	movq %mm0, %mm1                      
	pavgusb %mm6, %mm0 
	pavgusb %mm6, %mm0 
	movq (%rdi, %rsi, 4), %mm2                	# src, D.13334
	movq %mm2, %mm5                      
	pavgusb (%rax), %mm2 
	pavgusb (%rdi, %rsi, 2), %mm2 	# src, D.13334
	movq %mm2, %mm3                      
	movq (%rdi), %mm4                       	# src
	pavgusb %mm4, %mm3 
	pavgusb %mm0, %mm3 
	movq %mm3, (%rdi)                       	# src
	movq %mm1, %mm0                      
	pavgusb %mm6, %mm0 
	movq %mm4, %mm3                      
	pavgusb (%rdi,%rsi,2), %mm3 	# src, D.13334
	pavgusb (%rax,%rsi,2), %mm5 	# D.13334
	pavgusb (%rax), %mm5 
	pavgusb %mm5, %mm3 
	pavgusb %mm0, %mm3 
	movq %mm3, (%rdi,%rsi)                    	# src, D.13334
	pavgusb %mm4, %mm6 
	movq (%rcx), %mm0                
	pavgusb (%rax, %rsi, 2), %mm0 	# D.13334
	movq %mm0, %mm3                      
	pavgusb %mm1, %mm0 
	pavgusb %mm6, %mm0 
	pavgusb %mm2, %mm0 
	movq (%rdi, %rsi, 2), %mm2                	# src, D.13334
	movq %mm0, (%rdi, %rsi, 2)                	# src, D.13334
	movq (%rax, %rsi, 4), %mm0         	# D.13334
	pavgusb (%rcx), %mm0 
	pavgusb %mm0, %mm6 
	pavgusb %mm1, %mm4 
	pavgusb %mm2, %mm1 
	pavgusb %mm1, %mm6 
	pavgusb %mm5, %mm6 
	movq (%rax), %mm5                
	movq %mm6, (%rax)                
	movq (%rax, %rsi, 4), %mm6         	# D.13334
	pavgusb %mm7, %mm6 
	pavgusb %mm4, %mm6 
	pavgusb %mm3, %mm6 
	pavgusb %mm5, %mm2 
	movq (%rdi, %rsi, 4), %mm4                	# src, D.13334
	pavgusb %mm4, %mm2 
	pavgusb %mm2, %mm6 
	movq %mm6, (%rdi, %rsi, 4)                	# src, D.13334
	pavgusb %mm7, %mm1 
	pavgusb %mm4, %mm5 
	pavgusb %mm5, %mm0 
	movq (%rax, %rsi, 2), %mm6         	# D.13334
	pavgusb %mm6, %mm1 
	pavgusb %mm0, %mm1 
	movq %mm1, (%rax, %rsi, 2)         	# D.13334
	pavgusb (%rcx), %mm2 
	movq (%rax, %rsi, 4), %mm0         	# D.13334
	pavgusb %mm0, %mm6 
	pavgusb %mm7, %mm6 
	pavgusb %mm2, %mm6 
	movq %mm6, (%rcx)                
	pavgusb %mm7, %mm5 
	pavgusb %mm7, %mm5 
	pavgusb %mm3, %mm0 
	pavgusb %mm0, %mm5 
	movq %mm5, (%rax, %rsi, 4)         	# D.13334
	sub %rsi, %rdi                             	# D.13334, src
	
# 0 "" 2
#NO_APP
	ret
	.cfi_endproc
.LFE141:
	.size	doVertLowPass_3DNow, .-doVertLowPass_3DNow
	.type	dering_3DNow, @function
dering_3DNow:
.LFB144:
	.cfi_startproc
	movq	%rdx, %rcx	# c, c
	movslq	%esi, %rsi	# stride, D.13336
	leaq	-32(%rsp), %r8	#, tmp88
#APP
# 1097 "postprocess_template.c" 1
	pxor %mm6, %mm6                      
	pcmpeqb %mm7, %mm7                   
	movq 112(%rcx), %mm0                         	# c_5(D)->pQPb
	punpcklbw %mm6, %mm0                 
	psrlw $1, %mm0                        
	psubw %mm7, %mm0                     
	packuswb %mm0, %mm0                  
	movq %mm0, 120(%rcx)                         	# c_5(D)->pQPb2
	lea (%rdi, %rsi), %rax                	# src, D.13336
	lea (%rax, %rsi, 4), %rdx      	# D.13336
	movq (%rax), %mm0                  
	movq %mm7, %mm1                      
	psubusb %mm0, %mm6                   
	paddb %mm0, %mm6                     
	psubusb %mm0, %mm1                   
	psubb %mm1, %mm7                     
	movq (%rax, %rsi), %mm0                  	# D.13336
	movq %mm7, %mm1                      
	psubusb %mm0, %mm6                   
	paddb %mm0, %mm6                     
	psubusb %mm0, %mm1                   
	psubb %mm1, %mm7                     
	movq (%rax, %rsi, 2), %mm0                  	# D.13336
	movq %mm7, %mm1                      
	psubusb %mm0, %mm6                   
	paddb %mm0, %mm6                     
	psubusb %mm0, %mm1                   
	psubb %mm1, %mm7                     
	movq (%rdi, %rsi, 4), %mm0                  	# src, D.13336
	movq %mm7, %mm1                      
	psubusb %mm0, %mm6                   
	paddb %mm0, %mm6                     
	psubusb %mm0, %mm1                   
	psubb %mm1, %mm7                     
	movq (%rdx), %mm0                  
	movq %mm7, %mm1                      
	psubusb %mm0, %mm6                   
	paddb %mm0, %mm6                     
	psubusb %mm0, %mm1                   
	psubb %mm1, %mm7                     
	movq (%rdx, %rsi), %mm0                  	# D.13336
	movq %mm7, %mm1                      
	psubusb %mm0, %mm6                   
	paddb %mm0, %mm6                     
	psubusb %mm0, %mm1                   
	psubb %mm1, %mm7                     
	movq (%rdx, %rsi, 2), %mm0                  	# D.13336
	movq %mm7, %mm1                      
	psubusb %mm0, %mm6                   
	paddb %mm0, %mm6                     
	psubusb %mm0, %mm1                   
	psubb %mm1, %mm7                     
	movq (%rdi, %rsi, 8), %mm0                  	# src, D.13336
	movq %mm7, %mm1                      
	psubusb %mm0, %mm6                   
	paddb %mm0, %mm6                     
	psubusb %mm0, %mm1                   
	psubb %mm1, %mm7                     
	movq %mm7, %mm4                      
	psrlq $8, %mm7                        
	movq %mm7, %mm1                      
	psubusb %mm4, %mm1                   
	psubb %mm1, %mm7                     
	movq %mm7, %mm4                      
	psrlq $16, %mm7                       
	movq %mm7, %mm1                      
	psubusb %mm4, %mm1                   
	psubb %mm1, %mm7                     
	movq %mm7, %mm4                      
	psrlq $32, %mm7                       
	movq %mm7, %mm1                      
	psubusb %mm4, %mm1                   
	psubb %mm1, %mm7                     
	movq %mm6, %mm4                      
	psrlq $8, %mm6                        
	psubusb %mm4, %mm6                   
	paddb %mm4, %mm6                     
	movq %mm6, %mm4                      
	psrlq $16, %mm6                       
	psubusb %mm4, %mm6                   
	paddb %mm4, %mm6                     
	movq %mm6, %mm4                      
	psrlq $32, %mm6                       
	psubusb %mm4, %mm6                   
	paddb %mm4, %mm6                     
	movq %mm6, %mm0                      
	psubb %mm7, %mm6                     
	push %r8                              	# tmp88
	movd %mm6, %r8d                        	# tmp88
	cmpb deringThreshold, %r8b    	# tmp88
	pop %r8                               	# tmp88
	 jb .skip                                 
	pavgusb %mm0, %mm7 
	punpcklbw %mm7, %mm7                 
	punpcklbw %mm7, %mm7                 
	punpcklbw %mm7, %mm7                 
	movq %mm7, (%r8)                       	# tmp88
	movq (%rdi), %mm0                       	# src
	movq %mm0, %mm1                      
	movq %mm0, %mm2                      
	psllq $8, %mm1                        
	psrlq $8, %mm2                        
.skip:
	movd -4(%rdi), %mm3                     	# src
	movd 8(%rdi), %mm4                      	# src
	psrlq $24, %mm3                       
	psllq $56, %mm4                       
	por %mm3, %mm1                       
	por %mm4, %mm2                       
	movq %mm1, %mm3                      
	pavgusb %mm2, %mm1 
	pavgusb %mm0, %mm1 
	psubusb %mm7, %mm0                   
	psubusb %mm7, %mm2                   
	psubusb %mm7, %mm3                   
	pcmpeqb b00, %mm0           
	pcmpeqb b00, %mm2           
	pcmpeqb b00, %mm3           
	paddb %mm2, %mm0                     
	paddb %mm3, %mm0                     
	movq (%rax), %mm2                
	movq %mm2, %mm3                      
	movq %mm2, %mm4                      
	psllq $8, %mm3                        
	psrlq $8, %mm4                        
	movd -4(%rax), %mm5              
	movd 8(%rax), %mm6               
	psrlq $24, %mm5                       
	psllq $56, %mm6                       
	por %mm5, %mm3                       
	por %mm6, %mm4                       
	movq %mm3, %mm5                      
	pavgusb %mm4, %mm3 
	pavgusb %mm2, %mm3 
	psubusb %mm7, %mm2                   
	psubusb %mm7, %mm4                   
	psubusb %mm7, %mm5                   
	pcmpeqb b00, %mm2           
	pcmpeqb b00, %mm4           
	pcmpeqb b00, %mm5           
	paddb %mm4, %mm2                     
	paddb %mm5, %mm2                     
	movq (%rax, %rsi), %mm4                 	# D.13336
	movq %mm4, %mm5                  
	movq %mm4, %mm6                  
	psllq $8, %mm5                      
	psrlq $8, %mm6                      
	movd -4(%rax, %rsi), %mm7               	# D.13336
	psrlq $24, %mm7                     
	por %mm7, %mm5                   
	movd 8(%rax, %rsi), %mm7                	# D.13336
	psllq $56, %mm7                     
	por %mm7, %mm6                   
	movq %mm5, %mm7                  
	pavgusb %mm6, %mm5 
	pavgusb %mm4, %mm5 
	pavgusb %mm5, %mm1 
	movq %mm5, 8(%r8)                    	# tmp88
	movq (%r8), %mm5                     	# tmp88
	psubusb %mm5, %mm7               
	psubusb %mm5, %mm6               
	psubusb %mm5, %mm4               
	movq b00, %mm5            
	pcmpeqb %mm5, %mm7               
	pcmpeqb %mm5, %mm6               
	pcmpeqb %mm5, %mm4               
	paddb %mm7, %mm6                 
	paddb %mm6, %mm4                 
	pavgusb %mm3, %mm1 
	movq (%rax), %mm6                 
	movq %mm6, %mm7                  
	psubusb 120(%rcx), %mm6                    	# c_5(D)->pQPb2
	paddusb 120(%rcx), %mm7                    	# c_5(D)->pQPb2
	psubusb %mm6, %mm1 
	paddb %mm6, %mm1 
	movq %mm1, %mm6 
	psubusb %mm7, %mm6 
	psubb %mm6, %mm1 
	paddb %mm4, %mm0               
	paddb %mm2, %mm0              
	#paddb b02, %mm0        
	pand b08, %mm0          
	pcmpeqb %mm5, %mm0             
	pand %mm0, %mm1              
	pandn (%rax), %mm0              
	por %mm1, %mm0               
	movq %mm0, (%rax)               
	movq 8(%r8), %mm5                    	# tmp88
	movq (%rax, %rsi, 2), %mm0                 	# D.13336
	movq %mm0, %mm1                  
	movq %mm0, %mm6                  
	psllq $8, %mm1                      
	psrlq $8, %mm6                      
	movd -4(%rax, %rsi, 2), %mm7               	# D.13336
	psrlq $24, %mm7                     
	por %mm7, %mm1                   
	movd 8(%rax, %rsi, 2), %mm7                	# D.13336
	psllq $56, %mm7                     
	por %mm7, %mm6                   
	movq %mm1, %mm7                  
	pavgusb %mm6, %mm1 
	pavgusb %mm0, %mm1 
	pavgusb %mm1, %mm3 
	movq %mm1, 8(%r8)                    	# tmp88
	movq (%r8), %mm1                     	# tmp88
	psubusb %mm1, %mm7               
	psubusb %mm1, %mm6               
	psubusb %mm1, %mm0               
	movq b00, %mm1            
	pcmpeqb %mm1, %mm7               
	pcmpeqb %mm1, %mm6               
	pcmpeqb %mm1, %mm0               
	paddb %mm7, %mm6                 
	paddb %mm6, %mm0                 
	pavgusb %mm5, %mm3 
	movq (%rax, %rsi), %mm6                 	# D.13336
	movq %mm6, %mm7                  
	psubusb 120(%rcx), %mm6                    	# c_5(D)->pQPb2
	paddusb 120(%rcx), %mm7                    	# c_5(D)->pQPb2
	psubusb %mm6, %mm3 
	paddb %mm6, %mm3 
	movq %mm3, %mm6 
	psubusb %mm7, %mm6 
	psubb %mm6, %mm3 
	paddb %mm0, %mm2               
	paddb %mm4, %mm2              
	#paddb b02, %mm2        
	pand b08, %mm2          
	pcmpeqb %mm1, %mm2             
	pand %mm2, %mm3              
	pandn (%rax, %rsi), %mm2              	# D.13336
	por %mm3, %mm2               
	movq %mm2, (%rax, %rsi)               	# D.13336
	movq 8(%r8), %mm1                    	# tmp88
	movq (%rdi, %rsi, 4), %mm2                 	# src, D.13336
	movq %mm2, %mm3                  
	movq %mm2, %mm6                  
	psllq $8, %mm3                      
	psrlq $8, %mm6                      
	movd -4(%rdi, %rsi, 4), %mm7               	# src, D.13336
	psrlq $24, %mm7                     
	por %mm7, %mm3                   
	movd 8(%rdi, %rsi, 4), %mm7                	# src, D.13336
	psllq $56, %mm7                     
	por %mm7, %mm6                   
	movq %mm3, %mm7                  
	pavgusb %mm6, %mm3 
	pavgusb %mm2, %mm3 
	pavgusb %mm3, %mm5 
	movq %mm3, 8(%r8)                    	# tmp88
	movq (%r8), %mm3                     	# tmp88
	psubusb %mm3, %mm7               
	psubusb %mm3, %mm6               
	psubusb %mm3, %mm2               
	movq b00, %mm3            
	pcmpeqb %mm3, %mm7               
	pcmpeqb %mm3, %mm6               
	pcmpeqb %mm3, %mm2               
	paddb %mm7, %mm6                 
	paddb %mm6, %mm2                 
	pavgusb %mm1, %mm5 
	movq (%rax, %rsi, 2), %mm6                 	# D.13336
	movq %mm6, %mm7                  
	psubusb 120(%rcx), %mm6                    	# c_5(D)->pQPb2
	paddusb 120(%rcx), %mm7                    	# c_5(D)->pQPb2
	psubusb %mm6, %mm5 
	paddb %mm6, %mm5 
	movq %mm5, %mm6 
	psubusb %mm7, %mm6 
	psubb %mm6, %mm5 
	paddb %mm2, %mm4               
	paddb %mm0, %mm4              
	#paddb b02, %mm4        
	pand b08, %mm4          
	pcmpeqb %mm3, %mm4             
	pand %mm4, %mm5              
	pandn (%rax, %rsi, 2), %mm4              	# D.13336
	por %mm5, %mm4               
	movq %mm4, (%rax, %rsi, 2)               	# D.13336
	movq 8(%r8), %mm3                    	# tmp88
	movq (%rdx), %mm4                 
	movq %mm4, %mm5                  
	movq %mm4, %mm6                  
	psllq $8, %mm5                      
	psrlq $8, %mm6                      
	movd -4(%rdx), %mm7               
	psrlq $24, %mm7                     
	por %mm7, %mm5                   
	movd 8(%rdx), %mm7                
	psllq $56, %mm7                     
	por %mm7, %mm6                   
	movq %mm5, %mm7                  
	pavgusb %mm6, %mm5 
	pavgusb %mm4, %mm5 
	pavgusb %mm5, %mm1 
	movq %mm5, 8(%r8)                    	# tmp88
	movq (%r8), %mm5                     	# tmp88
	psubusb %mm5, %mm7               
	psubusb %mm5, %mm6               
	psubusb %mm5, %mm4               
	movq b00, %mm5            
	pcmpeqb %mm5, %mm7               
	pcmpeqb %mm5, %mm6               
	pcmpeqb %mm5, %mm4               
	paddb %mm7, %mm6                 
	paddb %mm6, %mm4                 
	pavgusb %mm3, %mm1 
	movq (%rdi, %rsi, 4), %mm6                 	# src, D.13336
	movq %mm6, %mm7                  
	psubusb 120(%rcx), %mm6                    	# c_5(D)->pQPb2
	paddusb 120(%rcx), %mm7                    	# c_5(D)->pQPb2
	psubusb %mm6, %mm1 
	paddb %mm6, %mm1 
	movq %mm1, %mm6 
	psubusb %mm7, %mm6 
	psubb %mm6, %mm1 
	paddb %mm4, %mm0               
	paddb %mm2, %mm0              
	#paddb b02, %mm0        
	pand b08, %mm0          
	pcmpeqb %mm5, %mm0             
	pand %mm0, %mm1              
	pandn (%rdi, %rsi, 4), %mm0              	# src, D.13336
	por %mm1, %mm0               
	movq %mm0, (%rdi, %rsi, 4)               	# src, D.13336
	movq 8(%r8), %mm5                    	# tmp88
	movq (%rdx, %rsi), %mm0                 	# D.13336
	movq %mm0, %mm1                  
	movq %mm0, %mm6                  
	psllq $8, %mm1                      
	psrlq $8, %mm6                      
	movd -4(%rdx, %rsi), %mm7               	# D.13336
	psrlq $24, %mm7                     
	por %mm7, %mm1                   
	movd 8(%rdx, %rsi), %mm7                	# D.13336
	psllq $56, %mm7                     
	por %mm7, %mm6                   
	movq %mm1, %mm7                  
	pavgusb %mm6, %mm1 
	pavgusb %mm0, %mm1 
	pavgusb %mm1, %mm3 
	movq %mm1, 8(%r8)                    	# tmp88
	movq (%r8), %mm1                     	# tmp88
	psubusb %mm1, %mm7               
	psubusb %mm1, %mm6               
	psubusb %mm1, %mm0               
	movq b00, %mm1            
	pcmpeqb %mm1, %mm7               
	pcmpeqb %mm1, %mm6               
	pcmpeqb %mm1, %mm0               
	paddb %mm7, %mm6                 
	paddb %mm6, %mm0                 
	pavgusb %mm5, %mm3 
	movq (%rdx), %mm6                 
	movq %mm6, %mm7                  
	psubusb 120(%rcx), %mm6                    	# c_5(D)->pQPb2
	paddusb 120(%rcx), %mm7                    	# c_5(D)->pQPb2
	psubusb %mm6, %mm3 
	paddb %mm6, %mm3 
	movq %mm3, %mm6 
	psubusb %mm7, %mm6 
	psubb %mm6, %mm3 
	paddb %mm0, %mm2               
	paddb %mm4, %mm2              
	#paddb b02, %mm2        
	pand b08, %mm2          
	pcmpeqb %mm1, %mm2             
	pand %mm2, %mm3              
	pandn (%rdx), %mm2              
	por %mm3, %mm2               
	movq %mm2, (%rdx)               
	movq 8(%r8), %mm1                    	# tmp88
	movq (%rdx, %rsi, 2), %mm2                 	# D.13336
	movq %mm2, %mm3                  
	movq %mm2, %mm6                  
	psllq $8, %mm3                      
	psrlq $8, %mm6                      
	movd -4(%rdx, %rsi, 2), %mm7               	# D.13336
	psrlq $24, %mm7                     
	por %mm7, %mm3                   
	movd 8(%rdx, %rsi, 2), %mm7                	# D.13336
	psllq $56, %mm7                     
	por %mm7, %mm6                   
	movq %mm3, %mm7                  
	pavgusb %mm6, %mm3 
	pavgusb %mm2, %mm3 
	pavgusb %mm3, %mm5 
	movq %mm3, 8(%r8)                    	# tmp88
	movq (%r8), %mm3                     	# tmp88
	psubusb %mm3, %mm7               
	psubusb %mm3, %mm6               
	psubusb %mm3, %mm2               
	movq b00, %mm3            
	pcmpeqb %mm3, %mm7               
	pcmpeqb %mm3, %mm6               
	pcmpeqb %mm3, %mm2               
	paddb %mm7, %mm6                 
	paddb %mm6, %mm2                 
	pavgusb %mm1, %mm5 
	movq (%rdx, %rsi), %mm6                 	# D.13336
	movq %mm6, %mm7                  
	psubusb 120(%rcx), %mm6                    	# c_5(D)->pQPb2
	paddusb 120(%rcx), %mm7                    	# c_5(D)->pQPb2
	psubusb %mm6, %mm5 
	paddb %mm6, %mm5 
	movq %mm5, %mm6 
	psubusb %mm7, %mm6 
	psubb %mm6, %mm5 
	paddb %mm2, %mm4               
	paddb %mm0, %mm4              
	#paddb b02, %mm4        
	pand b08, %mm4          
	pcmpeqb %mm3, %mm4             
	pand %mm4, %mm5              
	pandn (%rdx, %rsi), %mm4              	# D.13336
	por %mm5, %mm4               
	movq %mm4, (%rdx, %rsi)               	# D.13336
	movq 8(%r8), %mm3                    	# tmp88
	movq (%rdi, %rsi, 8), %mm4                 	# src, D.13336
	movq %mm4, %mm5                  
	movq %mm4, %mm6                  
	psllq $8, %mm5                      
	psrlq $8, %mm6                      
	movd -4(%rdi, %rsi, 8), %mm7               	# src, D.13336
	psrlq $24, %mm7                     
	por %mm7, %mm5                   
	movd 8(%rdi, %rsi, 8), %mm7                	# src, D.13336
	psllq $56, %mm7                     
	por %mm7, %mm6                   
	movq %mm5, %mm7                  
	pavgusb %mm6, %mm5 
	pavgusb %mm4, %mm5 
	pavgusb %mm5, %mm1 
	movq %mm5, 8(%r8)                    	# tmp88
	movq (%r8), %mm5                     	# tmp88
	psubusb %mm5, %mm7               
	psubusb %mm5, %mm6               
	psubusb %mm5, %mm4               
	movq b00, %mm5            
	pcmpeqb %mm5, %mm7               
	pcmpeqb %mm5, %mm6               
	pcmpeqb %mm5, %mm4               
	paddb %mm7, %mm6                 
	paddb %mm6, %mm4                 
	pavgusb %mm3, %mm1 
	movq (%rdx, %rsi, 2), %mm6                 	# D.13336
	movq %mm6, %mm7                  
	psubusb 120(%rcx), %mm6                    	# c_5(D)->pQPb2
	paddusb 120(%rcx), %mm7                    	# c_5(D)->pQPb2
	psubusb %mm6, %mm1 
	paddb %mm6, %mm1 
	movq %mm1, %mm6 
	psubusb %mm7, %mm6 
	psubb %mm6, %mm1 
	paddb %mm4, %mm0               
	paddb %mm2, %mm0              
	#paddb b02, %mm0        
	pand b08, %mm0          
	pcmpeqb %mm5, %mm0             
	pand %mm0, %mm1              
	pandn (%rdx, %rsi, 2), %mm0              	# D.13336
	por %mm1, %mm0               
	movq %mm0, (%rdx, %rsi, 2)               	# D.13336
	movq 8(%r8), %mm5                    	# tmp88
	movq (%rdx, %rsi, 4), %mm0                 	# D.13336
	movq %mm0, %mm1                  
	movq %mm0, %mm6                  
	psllq $8, %mm1                      
	psrlq $8, %mm6                      
	movd -4(%rdx, %rsi, 4), %mm7               	# D.13336
	psrlq $24, %mm7                     
	por %mm7, %mm1                   
	movd 8(%rdx, %rsi, 4), %mm7                	# D.13336
	psllq $56, %mm7                     
	por %mm7, %mm6                   
	movq %mm1, %mm7                  
	pavgusb %mm6, %mm1 
	pavgusb %mm0, %mm1 
	pavgusb %mm1, %mm3 
	movq %mm1, 8(%r8)                    	# tmp88
	movq (%r8), %mm1                     	# tmp88
	psubusb %mm1, %mm7               
	psubusb %mm1, %mm6               
	psubusb %mm1, %mm0               
	movq b00, %mm1            
	pcmpeqb %mm1, %mm7               
	pcmpeqb %mm1, %mm6               
	pcmpeqb %mm1, %mm0               
	paddb %mm7, %mm6                 
	paddb %mm6, %mm0                 
	pavgusb %mm5, %mm3 
	movq (%rdi, %rsi, 8), %mm6                 	# src, D.13336
	movq %mm6, %mm7                  
	psubusb 120(%rcx), %mm6                    	# c_5(D)->pQPb2
	paddusb 120(%rcx), %mm7                    	# c_5(D)->pQPb2
	psubusb %mm6, %mm3 
	paddb %mm6, %mm3 
	movq %mm3, %mm6 
	psubusb %mm7, %mm6 
	psubb %mm6, %mm3 
	paddb %mm0, %mm2               
	paddb %mm4, %mm2              
	#paddb b02, %mm2        
	pand b08, %mm2          
	pcmpeqb %mm1, %mm2             
	pand %mm2, %mm3              
	pandn (%rdi, %rsi, 8), %mm2              	# src, D.13336
	por %mm3, %mm2               
	movq %mm2, (%rdi, %rsi, 8)               	# src, D.13336
	movq 8(%r8), %mm1                    	# tmp88
	1:                        
	
# 0 "" 2
#NO_APP
	ret
	.cfi_endproc
.LFE144:
	.size	dering_3DNow, .-dering_3DNow
	.type	deInterlaceInterpolateCubic_3DNow, @function
deInterlaceInterpolateCubic_3DNow:
.LFB146:
	.cfi_startproc
	leal	(%rsi,%rsi,2), %eax	#, D.13343
	cltq
	addq	%rax, %rdi	# D.13344, src
	movslq	%esi, %rsi	# stride, D.13345
#APP
# 1508 "postprocess_template.c" 1
	lea (%rdi, %rsi), %rax                	# src, D.13345
	lea (%rax, %rsi, 4), %rdx      	# D.13345
	lea (%rdx, %rsi, 4), %rcx      	# D.13345
	add %rsi, %rcx                      	# D.13345
	pxor %mm7, %mm7                      
	movq (%rdi), %mm0                     	# src
	movq (%rax, %rsi), %mm1                     	# D.13345
	movq (%rdi, %rsi, 4), %mm2                     	# src, D.13345
	movq (%rdx, %rsi), %mm3                     	# D.13345
	pavgusb %mm2, %mm1 
	pavgusb %mm3, %mm0 
	movq %mm0, %mm2                      
	punpcklbw %mm7, %mm0                 
	punpckhbw %mm7, %mm2                 
	movq %mm1, %mm3                      
	punpcklbw %mm7, %mm1                 
	punpckhbw %mm7, %mm3                 
	psubw %mm1, %mm0                     
	psubw %mm3, %mm2                     
	psraw $3, %mm0                        
	psraw $3, %mm2                        
	psubw %mm0, %mm1                     
	psubw %mm2, %mm3                     
	packuswb %mm3, %mm1                  
	movq %mm1, (%rax, %rsi, 2)                     	# D.13345
	movq (%rax, %rsi), %mm0                     	# D.13345
	movq (%rdi, %rsi, 4), %mm1                     	# src, D.13345
	movq (%rdx, %rsi), %mm2                     	# D.13345
	movq (%rdi, %rsi, 8), %mm3                     	# src, D.13345
	pavgusb %mm2, %mm1 
	pavgusb %mm3, %mm0 
	movq %mm0, %mm2                      
	punpcklbw %mm7, %mm0                 
	punpckhbw %mm7, %mm2                 
	movq %mm1, %mm3                      
	punpcklbw %mm7, %mm1                 
	punpckhbw %mm7, %mm3                 
	psubw %mm1, %mm0                     
	psubw %mm3, %mm2                     
	psraw $3, %mm0                        
	psraw $3, %mm2                        
	psubw %mm0, %mm1                     
	psubw %mm2, %mm3                     
	packuswb %mm3, %mm1                  
	movq %mm1, (%rdx)                     
	movq (%rdi, %rsi, 4), %mm0                     	# src, D.13345
	movq (%rdx, %rsi), %mm1                     	# D.13345
	movq (%rdi, %rsi, 8), %mm2                     	# src, D.13345
	movq (%rcx), %mm3                     
	pavgusb %mm2, %mm1 
	pavgusb %mm3, %mm0 
	movq %mm0, %mm2                      
	punpcklbw %mm7, %mm0                 
	punpckhbw %mm7, %mm2                 
	movq %mm1, %mm3                      
	punpcklbw %mm7, %mm1                 
	punpckhbw %mm7, %mm3                 
	psubw %mm1, %mm0                     
	psubw %mm3, %mm2                     
	psraw $3, %mm0                        
	psraw $3, %mm2                        
	psubw %mm0, %mm1                     
	psubw %mm2, %mm3                     
	packuswb %mm3, %mm1                  
	movq %mm1, (%rdx, %rsi, 2)                     	# D.13345
	movq (%rdx, %rsi), %mm0                     	# D.13345
	movq (%rdi, %rsi, 8), %mm1                     	# src, D.13345
	movq (%rcx), %mm2                     
	movq (%rcx, %rsi, 2), %mm3                     	# D.13345
	pavgusb %mm2, %mm1 
	pavgusb %mm3, %mm0 
	movq %mm0, %mm2                      
	punpcklbw %mm7, %mm0                 
	punpckhbw %mm7, %mm2                 
	movq %mm1, %mm3                      
	punpcklbw %mm7, %mm1                 
	punpckhbw %mm7, %mm3                 
	psubw %mm1, %mm0                     
	psubw %mm3, %mm2                     
	psraw $3, %mm0                        
	psraw $3, %mm2                        
	psubw %mm0, %mm1                     
	psubw %mm2, %mm3                     
	packuswb %mm3, %mm1                  
	movq %mm1, (%rdx, %rsi, 4)                     	# D.13345
	
# 0 "" 2
#NO_APP
	ret
	.cfi_endproc
.LFE146:
	.size	deInterlaceInterpolateCubic_3DNow, .-deInterlaceInterpolateCubic_3DNow
	.type	deInterlaceFF_3DNow, @function
deInterlaceFF_3DNow:
.LFB147:
	.cfi_startproc
	movq	%rdx, %rcx	# tmp, tmp
	leal	0(,%rsi,4), %eax	#, D.13353
	cltq
	addq	%rax, %rdi	# D.13354, src
	movslq	%esi, %rsi	# stride, D.13355
#APP
# 1595 "postprocess_template.c" 1
	lea (%rdi, %rsi), %rax                	# src, D.13355
	lea (%rax, %rsi, 4), %rdx      	# D.13355
	pxor %mm7, %mm7                      
	movq (%rcx), %mm0                       	# tmp
	movq (%rdi), %mm1                     	# src
	movq (%rax), %mm2                     
	movq (%rax, %rsi), %mm3                     	# D.13355
	movq (%rax, %rsi, 2), %mm4                     	# D.13355
	pavgusb %mm3, %mm1 
	pavgusb %mm4, %mm0 
	movq %mm0, %mm3                      
	punpcklbw %mm7, %mm0                 
	punpckhbw %mm7, %mm3                 
	movq %mm1, %mm4                      
	punpcklbw %mm7, %mm1                 
	punpckhbw %mm7, %mm4                 
	psllw $2, %mm1                        
	psllw $2, %mm4                        
	psubw %mm0, %mm1                     
	psubw %mm3, %mm4                     
	movq %mm2, %mm5                      
	movq %mm2, %mm0                      
	punpcklbw %mm7, %mm2                 
	punpckhbw %mm7, %mm5                 
	paddw %mm2, %mm1                     
	paddw %mm5, %mm4                     
	psraw $2, %mm1                        
	psraw $2, %mm4                        
	packuswb %mm4, %mm1                  
	movq %mm1, (%rax)                     
	movq (%rax, %rsi), %mm1                     	# D.13355
	movq (%rax, %rsi, 2), %mm2                     	# D.13355
	movq (%rdi, %rsi, 4), %mm3                     	# src, D.13355
	movq (%rdx), %mm4                     
	pavgusb %mm3, %mm1 
	pavgusb %mm4, %mm0 
	movq %mm0, %mm3                      
	punpcklbw %mm7, %mm0                 
	punpckhbw %mm7, %mm3                 
	movq %mm1, %mm4                      
	punpcklbw %mm7, %mm1                 
	punpckhbw %mm7, %mm4                 
	psllw $2, %mm1                        
	psllw $2, %mm4                        
	psubw %mm0, %mm1                     
	psubw %mm3, %mm4                     
	movq %mm2, %mm5                      
	movq %mm2, %mm0                      
	punpcklbw %mm7, %mm2                 
	punpckhbw %mm7, %mm5                 
	paddw %mm2, %mm1                     
	paddw %mm5, %mm4                     
	psraw $2, %mm1                        
	psraw $2, %mm4                        
	packuswb %mm4, %mm1                  
	movq %mm1, (%rax, %rsi, 2)                     	# D.13355
	movq (%rdi, %rsi, 4), %mm1                     	# src, D.13355
	movq (%rdx), %mm2                     
	movq (%rdx, %rsi), %mm3                     	# D.13355
	movq (%rdx, %rsi, 2), %mm4                     	# D.13355
	pavgusb %mm3, %mm1 
	pavgusb %mm4, %mm0 
	movq %mm0, %mm3                      
	punpcklbw %mm7, %mm0                 
	punpckhbw %mm7, %mm3                 
	movq %mm1, %mm4                      
	punpcklbw %mm7, %mm1                 
	punpckhbw %mm7, %mm4                 
	psllw $2, %mm1                        
	psllw $2, %mm4                        
	psubw %mm0, %mm1                     
	psubw %mm3, %mm4                     
	movq %mm2, %mm5                      
	movq %mm2, %mm0                      
	punpcklbw %mm7, %mm2                 
	punpckhbw %mm7, %mm5                 
	paddw %mm2, %mm1                     
	paddw %mm5, %mm4                     
	psraw $2, %mm1                        
	psraw $2, %mm4                        
	packuswb %mm4, %mm1                  
	movq %mm1, (%rdx)                     
	movq (%rdx, %rsi), %mm1                     	# D.13355
	movq (%rdx, %rsi, 2), %mm2                     	# D.13355
	movq (%rdi, %rsi, 8), %mm3                     	# src, D.13355
	movq (%rdx, %rsi, 4), %mm4                     	# D.13355
	pavgusb %mm3, %mm1 
	pavgusb %mm4, %mm0 
	movq %mm0, %mm3                      
	punpcklbw %mm7, %mm0                 
	punpckhbw %mm7, %mm3                 
	movq %mm1, %mm4                      
	punpcklbw %mm7, %mm1                 
	punpckhbw %mm7, %mm4                 
	psllw $2, %mm1                        
	psllw $2, %mm4                        
	psubw %mm0, %mm1                     
	psubw %mm3, %mm4                     
	movq %mm2, %mm5                      
	movq %mm2, %mm0                      
	punpcklbw %mm7, %mm2                 
	punpckhbw %mm7, %mm5                 
	paddw %mm2, %mm1                     
	paddw %mm5, %mm4                     
	psraw $2, %mm1                        
	psraw $2, %mm4                        
	packuswb %mm4, %mm1                  
	movq %mm1, (%rdx, %rsi, 2)                     	# D.13355
	movq %mm0, (%rcx)                       	# tmp
	
# 0 "" 2
#NO_APP
	ret
	.cfi_endproc
.LFE147:
	.size	deInterlaceFF_3DNow, .-deInterlaceFF_3DNow
	.type	deInterlaceL5_3DNow, @function
deInterlaceL5_3DNow:
.LFB148:
	.cfi_startproc
	movq	%rdx, %r8	# tmp, tmp
	leal	0(,%rsi,4), %eax	#, D.13357
	cltq
	addq	%rax, %rdi	# D.13358, src
	movslq	%esi, %rsi	# stride, D.13359
#APP
# 1674 "postprocess_template.c" 1
	lea (%rdi, %rsi), %rax                	# src, D.13359
	lea (%rax, %rsi, 4), %rdx      	# D.13359
	pxor %mm7, %mm7                      
	movq (%r8), %mm0                       	# tmp
	movq (%rcx), %mm1                       	# tmp2
	movq (%rdi), %mm2                     	# src
	movq (%rax), %mm3                     
	movq (%rax, %rsi), %mm4                     	# D.13359
	pavgusb %mm1, %mm3 
	pavgusb %mm0, %mm4 
	movq %mm2, %mm5                      
	movq %mm2, %mm0                    
	punpcklbw %mm7, %mm2                 
	punpckhbw %mm7, %mm5                 
	movq %mm2, %mm6                      
	paddw %mm2, %mm2                     
	paddw %mm6, %mm2                     
	movq %mm5, %mm6                      
	paddw %mm5, %mm5                     
	paddw %mm6, %mm5                     
	movq %mm3, %mm6                      
	punpcklbw %mm7, %mm3                 
	punpckhbw %mm7, %mm6                 
	paddw %mm3, %mm3                     
	paddw %mm6, %mm6                     
	paddw %mm3, %mm2                     
	paddw %mm6, %mm5                     
	movq %mm4, %mm6                      
	punpcklbw %mm7, %mm4                 
	punpckhbw %mm7, %mm6                 
	psubw %mm4, %mm2                     
	psubw %mm6, %mm5                     
	psraw $2, %mm2                        
	psraw $2, %mm5                        
	packuswb %mm5, %mm2                  
	movq %mm2, (%rdi)                     	# src
	movq (%rax), %mm2                     
	movq (%rax, %rsi), %mm3                     	# D.13359
	movq (%rax, %rsi, 2), %mm4                     	# D.13359
	pavgusb %mm0, %mm3 
	pavgusb %mm1, %mm4 
	movq %mm2, %mm5                      
	movq %mm2, %mm1                    
	punpcklbw %mm7, %mm2                 
	punpckhbw %mm7, %mm5                 
	movq %mm2, %mm6                      
	paddw %mm2, %mm2                     
	paddw %mm6, %mm2                     
	movq %mm5, %mm6                      
	paddw %mm5, %mm5                     
	paddw %mm6, %mm5                     
	movq %mm3, %mm6                      
	punpcklbw %mm7, %mm3                 
	punpckhbw %mm7, %mm6                 
	paddw %mm3, %mm3                     
	paddw %mm6, %mm6                     
	paddw %mm3, %mm2                     
	paddw %mm6, %mm5                     
	movq %mm4, %mm6                      
	punpcklbw %mm7, %mm4                 
	punpckhbw %mm7, %mm6                 
	psubw %mm4, %mm2                     
	psubw %mm6, %mm5                     
	psraw $2, %mm2                        
	psraw $2, %mm5                        
	packuswb %mm5, %mm2                  
	movq %mm2, (%rax)                     
	movq (%rax, %rsi), %mm2                     	# D.13359
	movq (%rax, %rsi, 2), %mm3                     	# D.13359
	movq (%rdi, %rsi, 4), %mm4                     	# src, D.13359
	pavgusb %mm1, %mm3 
	pavgusb %mm0, %mm4 
	movq %mm2, %mm5                      
	movq %mm2, %mm0                    
	punpcklbw %mm7, %mm2                 
	punpckhbw %mm7, %mm5                 
	movq %mm2, %mm6                      
	paddw %mm2, %mm2                     
	paddw %mm6, %mm2                     
	movq %mm5, %mm6                      
	paddw %mm5, %mm5                     
	paddw %mm6, %mm5                     
	movq %mm3, %mm6                      
	punpcklbw %mm7, %mm3                 
	punpckhbw %mm7, %mm6                 
	paddw %mm3, %mm3                     
	paddw %mm6, %mm6                     
	paddw %mm3, %mm2                     
	paddw %mm6, %mm5                     
	movq %mm4, %mm6                      
	punpcklbw %mm7, %mm4                 
	punpckhbw %mm7, %mm6                 
	psubw %mm4, %mm2                     
	psubw %mm6, %mm5                     
	psraw $2, %mm2                        
	psraw $2, %mm5                        
	packuswb %mm5, %mm2                  
	movq %mm2, (%rax, %rsi)                     	# D.13359
	movq (%rax, %rsi, 2), %mm2                     	# D.13359
	movq (%rdi, %rsi, 4), %mm3                     	# src, D.13359
	movq (%rdx), %mm4                     
	pavgusb %mm0, %mm3 
	pavgusb %mm1, %mm4 
	movq %mm2, %mm5                      
	movq %mm2, %mm1                    
	punpcklbw %mm7, %mm2                 
	punpckhbw %mm7, %mm5                 
	movq %mm2, %mm6                      
	paddw %mm2, %mm2                     
	paddw %mm6, %mm2                     
	movq %mm5, %mm6                      
	paddw %mm5, %mm5                     
	paddw %mm6, %mm5                     
	movq %mm3, %mm6                      
	punpcklbw %mm7, %mm3                 
	punpckhbw %mm7, %mm6                 
	paddw %mm3, %mm3                     
	paddw %mm6, %mm6                     
	paddw %mm3, %mm2                     
	paddw %mm6, %mm5                     
	movq %mm4, %mm6                      
	punpcklbw %mm7, %mm4                 
	punpckhbw %mm7, %mm6                 
	psubw %mm4, %mm2                     
	psubw %mm6, %mm5                     
	psraw $2, %mm2                        
	psraw $2, %mm5                        
	packuswb %mm5, %mm2                  
	movq %mm2, (%rax, %rsi, 2)                     	# D.13359
	movq (%rdi, %rsi, 4), %mm2                     	# src, D.13359
	movq (%rdx), %mm3                     
	movq (%rdx, %rsi), %mm4                     	# D.13359
	pavgusb %mm1, %mm3 
	pavgusb %mm0, %mm4 
	movq %mm2, %mm5                      
	movq %mm2, %mm0                    
	punpcklbw %mm7, %mm2                 
	punpckhbw %mm7, %mm5                 
	movq %mm2, %mm6                      
	paddw %mm2, %mm2                     
	paddw %mm6, %mm2                     
	movq %mm5, %mm6                      
	paddw %mm5, %mm5                     
	paddw %mm6, %mm5                     
	movq %mm3, %mm6                      
	punpcklbw %mm7, %mm3                 
	punpckhbw %mm7, %mm6                 
	paddw %mm3, %mm3                     
	paddw %mm6, %mm6                     
	paddw %mm3, %mm2                     
	paddw %mm6, %mm5                     
	movq %mm4, %mm6                      
	punpcklbw %mm7, %mm4                 
	punpckhbw %mm7, %mm6                 
	psubw %mm4, %mm2                     
	psubw %mm6, %mm5                     
	psraw $2, %mm2                        
	psraw $2, %mm5                        
	packuswb %mm5, %mm2                  
	movq %mm2, (%rdi, %rsi, 4)                     	# src, D.13359
	movq (%rdx), %mm2                     
	movq (%rdx, %rsi), %mm3                     	# D.13359
	movq (%rdx, %rsi, 2), %mm4                     	# D.13359
	pavgusb %mm0, %mm3 
	pavgusb %mm1, %mm4 
	movq %mm2, %mm5                      
	movq %mm2, %mm1                    
	punpcklbw %mm7, %mm2                 
	punpckhbw %mm7, %mm5                 
	movq %mm2, %mm6                      
	paddw %mm2, %mm2                     
	paddw %mm6, %mm2                     
	movq %mm5, %mm6                      
	paddw %mm5, %mm5                     
	paddw %mm6, %mm5                     
	movq %mm3, %mm6                      
	punpcklbw %mm7, %mm3                 
	punpckhbw %mm7, %mm6                 
	paddw %mm3, %mm3                     
	paddw %mm6, %mm6                     
	paddw %mm3, %mm2                     
	paddw %mm6, %mm5                     
	movq %mm4, %mm6                      
	punpcklbw %mm7, %mm4                 
	punpckhbw %mm7, %mm6                 
	psubw %mm4, %mm2                     
	psubw %mm6, %mm5                     
	psraw $2, %mm2                        
	psraw $2, %mm5                        
	packuswb %mm5, %mm2                  
	movq %mm2, (%rdx)                     
	movq (%rdx, %rsi), %mm2                     	# D.13359
	movq (%rdx, %rsi, 2), %mm3                     	# D.13359
	movq (%rdi, %rsi, 8), %mm4                     	# src, D.13359
	pavgusb %mm1, %mm3 
	pavgusb %mm0, %mm4 
	movq %mm2, %mm5                      
	movq %mm2, %mm0                    
	punpcklbw %mm7, %mm2                 
	punpckhbw %mm7, %mm5                 
	movq %mm2, %mm6                      
	paddw %mm2, %mm2                     
	paddw %mm6, %mm2                     
	movq %mm5, %mm6                      
	paddw %mm5, %mm5                     
	paddw %mm6, %mm5                     
	movq %mm3, %mm6                      
	punpcklbw %mm7, %mm3                 
	punpckhbw %mm7, %mm6                 
	paddw %mm3, %mm3                     
	paddw %mm6, %mm6                     
	paddw %mm3, %mm2                     
	paddw %mm6, %mm5                     
	movq %mm4, %mm6                      
	punpcklbw %mm7, %mm4                 
	punpckhbw %mm7, %mm6                 
	psubw %mm4, %mm2                     
	psubw %mm6, %mm5                     
	psraw $2, %mm2                        
	psraw $2, %mm5                        
	packuswb %mm5, %mm2                  
	movq %mm2, (%rdx, %rsi)                     	# D.13359
	movq (%rdx, %rsi, 2), %mm2                     	# D.13359
	movq (%rdi, %rsi, 8), %mm3                     	# src, D.13359
	movq (%rdx, %rsi, 4), %mm4                     	# D.13359
	pavgusb %mm0, %mm3 
	pavgusb %mm1, %mm4 
	movq %mm2, %mm5                      
	movq %mm2, %mm1                    
	punpcklbw %mm7, %mm2                 
	punpckhbw %mm7, %mm5                 
	movq %mm2, %mm6                      
	paddw %mm2, %mm2                     
	paddw %mm6, %mm2                     
	movq %mm5, %mm6                      
	paddw %mm5, %mm5                     
	paddw %mm6, %mm5                     
	movq %mm3, %mm6                      
	punpcklbw %mm7, %mm3                 
	punpckhbw %mm7, %mm6                 
	paddw %mm3, %mm3                     
	paddw %mm6, %mm6                     
	paddw %mm3, %mm2                     
	paddw %mm6, %mm5                     
	movq %mm4, %mm6                      
	punpcklbw %mm7, %mm4                 
	punpckhbw %mm7, %mm6                 
	psubw %mm4, %mm2                     
	psubw %mm6, %mm5                     
	psraw $2, %mm2                        
	psraw $2, %mm5                        
	packuswb %mm5, %mm2                  
	movq %mm2, (%rdx, %rsi, 2)                     	# D.13359
	movq %mm0, (%r8)                       	# tmp
	movq %mm1, (%rcx)                       	# tmp2
	
# 0 "" 2
#NO_APP
	ret
	.cfi_endproc
.LFE148:
	.size	deInterlaceL5_3DNow, .-deInterlaceL5_3DNow
	.type	deInterlaceMedian_3DNow, @function
deInterlaceMedian_3DNow:
.LFB150:
	.cfi_startproc
	leal	0(,%rsi,4), %eax	#, D.13366
	cltq
	addq	%rax, %rdi	# D.13367, src
	movslq	%esi, %rsi	# stride, D.13368
#APP
# 1926 "postprocess_template.c" 1
	lea (%rdi, %rsi), %rax                	# src, D.13368
	lea (%rax, %rsi, 4), %rdx      	# D.13368
	pxor %mm7, %mm7                      
	movq (%rdi), %mm0                     	# src
	movq (%rax), %mm2                     
	movq (%rax, %rsi), %mm1                     	# D.13368
	movq %mm0, %mm3                      
	movq %mm1, %mm4                      
	movq %mm2, %mm5                      
	psubusb %mm1, %mm3                   
	psubusb %mm2, %mm4                   
	psubusb %mm0, %mm5                   
	pcmpeqb %mm7, %mm3                   
	pcmpeqb %mm7, %mm4                   
	pcmpeqb %mm7, %mm5                   
	movq %mm3, %mm6                      
	pxor %mm4, %mm3                      
	pxor %mm5, %mm4                      
	pxor %mm6, %mm5                      
	por %mm3, %mm1                       
	por %mm4, %mm2                       
	por %mm5, %mm0                       
	pand %mm2, %mm0                      
	pand %mm1, %mm0                      
	movq %mm0, (%rax)                     
	movq (%rax, %rsi), %mm0                     	# D.13368
	movq (%rax, %rsi, 2), %mm2                     	# D.13368
	movq (%rdi, %rsi, 4), %mm1                     	# src, D.13368
	movq %mm0, %mm3                      
	movq %mm1, %mm4                      
	movq %mm2, %mm5                      
	psubusb %mm1, %mm3                   
	psubusb %mm2, %mm4                   
	psubusb %mm0, %mm5                   
	pcmpeqb %mm7, %mm3                   
	pcmpeqb %mm7, %mm4                   
	pcmpeqb %mm7, %mm5                   
	movq %mm3, %mm6                      
	pxor %mm4, %mm3                      
	pxor %mm5, %mm4                      
	pxor %mm6, %mm5                      
	por %mm3, %mm1                       
	por %mm4, %mm2                       
	por %mm5, %mm0                       
	pand %mm2, %mm0                      
	pand %mm1, %mm0                      
	movq %mm0, (%rax, %rsi, 2)                     	# D.13368
	movq (%rdi, %rsi, 4), %mm0                     	# src, D.13368
	movq (%rdx), %mm2                     
	movq (%rdx, %rsi), %mm1                     	# D.13368
	movq %mm0, %mm3                      
	movq %mm1, %mm4                      
	movq %mm2, %mm5                      
	psubusb %mm1, %mm3                   
	psubusb %mm2, %mm4                   
	psubusb %mm0, %mm5                   
	pcmpeqb %mm7, %mm3                   
	pcmpeqb %mm7, %mm4                   
	pcmpeqb %mm7, %mm5                   
	movq %mm3, %mm6                      
	pxor %mm4, %mm3                      
	pxor %mm5, %mm4                      
	pxor %mm6, %mm5                      
	por %mm3, %mm1                       
	por %mm4, %mm2                       
	por %mm5, %mm0                       
	pand %mm2, %mm0                      
	pand %mm1, %mm0                      
	movq %mm0, (%rdx)                     
	movq (%rdx, %rsi), %mm0                     	# D.13368
	movq (%rdx, %rsi, 2), %mm2                     	# D.13368
	movq (%rdi, %rsi, 8), %mm1                     	# src, D.13368
	movq %mm0, %mm3                      
	movq %mm1, %mm4                      
	movq %mm2, %mm5                      
	psubusb %mm1, %mm3                   
	psubusb %mm2, %mm4                   
	psubusb %mm0, %mm5                   
	pcmpeqb %mm7, %mm3                   
	pcmpeqb %mm7, %mm4                   
	pcmpeqb %mm7, %mm5                   
	movq %mm3, %mm6                      
	pxor %mm4, %mm3                      
	pxor %mm5, %mm4                      
	pxor %mm6, %mm5                      
	por %mm3, %mm1                       
	por %mm4, %mm2                       
	por %mm5, %mm0                       
	pand %mm2, %mm0                      
	pand %mm1, %mm0                      
	movq %mm0, (%rdx, %rsi, 2)                     	# D.13368
	
# 0 "" 2
#NO_APP
	ret
	.cfi_endproc
.LFE150:
	.size	deInterlaceMedian_3DNow, .-deInterlaceMedian_3DNow
	.type	tempNoiseReducer_3DNow, @function
tempNoiseReducer_3DNow:
.LFB153:
	.cfi_startproc
	movq	%rdx, %r9	# tempBlurred, tempBlurred
	movq	%rcx, -8(%rsp)	# tempBlurredPast, tempBlurredPast
	movl	(%r8), %eax	# *maxNoise_3(D), *maxNoise_3(D)
	movl	%eax, 508(%rcx)	# *maxNoise_3(D), MEM[(uint32_t *)_2 + 508B]
	movl	4(%r8), %eax	# MEM[(const int *)maxNoise_3(D) + 4B], MEM[(const int *)maxNoise_3(D) + 4B]
	movl	%eax, 512(%rcx)	# MEM[(const int *)maxNoise_3(D) + 4B], MEM[(uint32_t *)_2 + 512B]
	movl	8(%r8), %eax	# MEM[(const int *)maxNoise_3(D) + 8B], MEM[(const int *)maxNoise_3(D) + 8B]
	movl	%eax, 516(%rcx)	# MEM[(const int *)maxNoise_3(D) + 8B], MEM[(uint32_t *)_2 + 516B]
	movslq	%esi, %rsi	# stride, D.13381
#APP
# 2169 "postprocess_template.c" 1
	lea (%rsi, %rsi, 2), %rax             	# D.13381
	lea (%rsi, %rsi, 4), %rdx             	# D.13381
	lea (%rdx, %rsi, 2), %rcx      	# D.13381
	pcmpeqb %mm7, %mm7                   
	movq b80, %mm6              
	pxor %mm0, %mm0                      
	movq (%rdi), %mm5                     	# src
	movq (%r9), %mm2                     	# tempBlurred
	pxor %mm7, %mm2                      
	pavgusb %mm2, %mm5 
	paddb %mm6, %mm5                     
	movq %mm5, %mm2                      
	psllw $8, %mm5                        
	pmaddwd %mm5, %mm5                   
	pmaddwd %mm2, %mm2                   
	paddd %mm2, %mm5                     
	psrld $14, %mm5                       
	paddd %mm5, %mm0                     
	movq (%rdi, %rsi), %mm5                     	# src, D.13381
	movq (%r9, %rsi), %mm2                     	# tempBlurred, D.13381
	pxor %mm7, %mm2                      
	pavgusb %mm2, %mm5 
	paddb %mm6, %mm5                     
	movq %mm5, %mm2                      
	psllw $8, %mm5                        
	pmaddwd %mm5, %mm5                   
	pmaddwd %mm2, %mm2                   
	paddd %mm2, %mm5                     
	psrld $14, %mm5                       
	paddd %mm5, %mm0                     
	movq (%rdi, %rsi, 2), %mm5                     	# src, D.13381
	movq (%r9, %rsi, 2), %mm2                     	# tempBlurred, D.13381
	pxor %mm7, %mm2                      
	pavgusb %mm2, %mm5 
	paddb %mm6, %mm5                     
	movq %mm5, %mm2                      
	psllw $8, %mm5                        
	pmaddwd %mm5, %mm5                   
	pmaddwd %mm2, %mm2                   
	paddd %mm2, %mm5                     
	psrld $14, %mm5                       
	paddd %mm5, %mm0                     
	movq (%rdi, %rax), %mm5                     	# src
	movq (%r9, %rax), %mm2                     	# tempBlurred
	pxor %mm7, %mm2                      
	pavgusb %mm2, %mm5 
	paddb %mm6, %mm5                     
	movq %mm5, %mm2                      
	psllw $8, %mm5                        
	pmaddwd %mm5, %mm5                   
	pmaddwd %mm2, %mm2                   
	paddd %mm2, %mm5                     
	psrld $14, %mm5                       
	paddd %mm5, %mm0                     
	movq (%rdi, %rsi, 4), %mm5                     	# src, D.13381
	movq (%r9, %rsi, 4), %mm2                     	# tempBlurred, D.13381
	pxor %mm7, %mm2                      
	pavgusb %mm2, %mm5 
	paddb %mm6, %mm5                     
	movq %mm5, %mm2                      
	psllw $8, %mm5                        
	pmaddwd %mm5, %mm5                   
	pmaddwd %mm2, %mm2                   
	paddd %mm2, %mm5                     
	psrld $14, %mm5                       
	paddd %mm5, %mm0                     
	movq (%rdi, %rdx), %mm5                     	# src
	movq (%r9, %rdx), %mm2                     	# tempBlurred
	pxor %mm7, %mm2                      
	pavgusb %mm2, %mm5 
	paddb %mm6, %mm5                     
	movq %mm5, %mm2                      
	psllw $8, %mm5                        
	pmaddwd %mm5, %mm5                   
	pmaddwd %mm2, %mm2                   
	paddd %mm2, %mm5                     
	psrld $14, %mm5                       
	paddd %mm5, %mm0                     
	movq (%rdi, %rax,2), %mm5                     	# src
	movq (%r9, %rax,2), %mm2                     	# tempBlurred
	pxor %mm7, %mm2                      
	pavgusb %mm2, %mm5 
	paddb %mm6, %mm5                     
	movq %mm5, %mm2                      
	psllw $8, %mm5                        
	pmaddwd %mm5, %mm5                   
	pmaddwd %mm2, %mm2                   
	paddd %mm2, %mm5                     
	psrld $14, %mm5                       
	paddd %mm5, %mm0                     
	movq (%rdi, %rcx), %mm5                     	# src
	movq (%r9, %rcx), %mm2                     	# tempBlurred
	pxor %mm7, %mm2                      
	pavgusb %mm2, %mm5 
	paddb %mm6, %mm5                     
	movq %mm5, %mm2                      
	psllw $8, %mm5                        
	pmaddwd %mm5, %mm5                   
	pmaddwd %mm2, %mm2                   
	paddd %mm2, %mm5                     
	psrld $14, %mm5                       
	paddd %mm5, %mm0                     
	movq %mm0, %mm4                      
	psrlq $32, %mm0                       
	paddd %mm0, %mm4                     
	movd %mm4, %ecx                      
	shll $2, %ecx                         
	mov -8(%rsp), %rdx                      	# tempBlurredPast
	addl -4(%rdx), %ecx              
	addl 4(%rdx), %ecx               
	addl -1024(%rdx), %ecx           
	addl $4, %ecx                         
	addl 1024(%rdx), %ecx            
	shrl $3, %ecx                         
	movl %ecx, (%rdx)                
	cmpl 512(%rdx), %ecx             
	 jb 2f                                 
	cmpl 516(%rdx), %ecx             
	 jb 1f                                 
	lea (%rax, %rsi, 2), %rdx      	# D.13381
	lea (%rdx, %rsi, 2), %rcx      	# D.13381
	movq (%rdi), %mm0                       	# src
	movq (%rdi, %rsi), %mm1                   	# src, D.13381
	movq (%rdi, %rsi, 2), %mm2                	# src, D.13381
	movq (%rdi, %rax), %mm3            	# src
	movq (%rdi, %rsi, 4), %mm4                	# src, D.13381
	movq (%rdi, %rdx), %mm5            	# src
	movq (%rdi, %rax, 2), %mm6         	# src
	movq (%rdi, %rcx), %mm7            	# src
	movq %mm0, (%r9)                       	# tempBlurred
	movq %mm1, (%r9, %rsi)                   	# tempBlurred, D.13381
	movq %mm2, (%r9, %rsi, 2)                	# tempBlurred, D.13381
	movq %mm3, (%r9, %rax)            	# tempBlurred
	movq %mm4, (%r9, %rsi, 4)                	# tempBlurred, D.13381
	movq %mm5, (%r9, %rdx)            	# tempBlurred
	movq %mm6, (%r9, %rax, 2)         	# tempBlurred
	movq %mm7, (%r9, %rcx)            	# tempBlurred
	jmp 4f                                 
	1:                                     
	lea (%rax, %rsi, 2), %rdx      	# D.13381
	lea (%rdx, %rsi, 2), %rcx      	# D.13381
	movq (%rdi), %mm0                       	# src
	pavgusb (%r9), %mm0 	# tempBlurred
	movq (%rdi, %rsi), %mm1                   	# src, D.13381
	pavgusb (%r9, %rsi), %mm1 	# tempBlurred, D.13381
	movq (%rdi, %rsi, 2), %mm2                	# src, D.13381
	pavgusb (%r9, %rsi, 2), %mm2 	# tempBlurred, D.13381
	movq (%rdi, %rax), %mm3            	# src
	pavgusb (%r9, %rax), %mm3 	# tempBlurred
	movq (%rdi, %rsi, 4), %mm4                	# src, D.13381
	pavgusb (%r9, %rsi, 4), %mm4 	# tempBlurred, D.13381
	movq (%rdi, %rdx), %mm5            	# src
	pavgusb (%r9, %rdx), %mm5 	# tempBlurred
	movq (%rdi, %rax, 2), %mm6         	# src
	pavgusb (%r9, %rax, 2), %mm6 	# tempBlurred
	movq (%rdi, %rcx), %mm7            	# src
	pavgusb (%r9, %rcx), %mm7 	# tempBlurred
	movq %mm0, (%r9)                       	# tempBlurred
	movq %mm1, (%r9, %rsi)                   	# tempBlurred, D.13381
	movq %mm2, (%r9, %rsi, 2)                	# tempBlurred, D.13381
	movq %mm3, (%r9, %rax)            	# tempBlurred
	movq %mm4, (%r9, %rsi, 4)                	# tempBlurred, D.13381
	movq %mm5, (%r9, %rdx)            	# tempBlurred
	movq %mm6, (%r9, %rax, 2)         	# tempBlurred
	movq %mm7, (%r9, %rcx)            	# tempBlurred
	movq %mm0, (%rdi)                       	# src
	movq %mm1, (%rdi, %rsi)                   	# src, D.13381
	movq %mm2, (%rdi, %rsi, 2)                	# src, D.13381
	movq %mm3, (%rdi, %rax)            	# src
	movq %mm4, (%rdi, %rsi, 4)                	# src, D.13381
	movq %mm5, (%rdi, %rdx)            	# src
	movq %mm6, (%rdi, %rax, 2)         	# src
	movq %mm7, (%rdi, %rcx)            	# src
	jmp 4f                                 
	2:                                     
	cmpl 508(%rdx), %ecx             
	 jb 3f                                 
	lea (%rax, %rsi, 2), %rdx      	# D.13381
	lea (%rdx, %rsi, 2), %rcx      	# D.13381
	movq (%rdi), %mm0                       	# src
	movq (%rdi, %rsi), %mm1                   	# src, D.13381
	movq (%rdi, %rsi, 2), %mm2                	# src, D.13381
	movq (%rdi, %rax), %mm3            	# src
	movq (%r9), %mm4                       	# tempBlurred
	movq (%r9, %rsi), %mm5                   	# tempBlurred, D.13381
	movq (%r9, %rsi, 2), %mm6                	# tempBlurred, D.13381
	movq (%r9, %rax), %mm7            	# tempBlurred
	pavgusb %mm4, %mm0 
	pavgusb %mm5, %mm1 
	pavgusb %mm6, %mm2 
	pavgusb %mm7, %mm3 
	pavgusb %mm4, %mm0 
	pavgusb %mm5, %mm1 
	pavgusb %mm6, %mm2 
	pavgusb %mm7, %mm3 
	movq %mm0, (%r9)                       	# tempBlurred
	movq %mm1, (%r9, %rsi)                   	# tempBlurred, D.13381
	movq %mm2, (%r9, %rsi, 2)                	# tempBlurred, D.13381
	movq %mm3, (%r9, %rax)            	# tempBlurred
	movq %mm0, (%rdi)                       	# src
	movq %mm1, (%rdi, %rsi)                   	# src, D.13381
	movq %mm2, (%rdi, %rsi, 2)                	# src, D.13381
	movq %mm3, (%rdi, %rax)            	# src
	movq (%rdi, %rsi, 4), %mm0                	# src, D.13381
	movq (%rdi, %rdx), %mm1            	# src
	movq (%rdi, %rax, 2), %mm2         	# src
	movq (%rdi, %rcx), %mm3            	# src
	movq (%r9, %rsi, 4), %mm4                	# tempBlurred, D.13381
	movq (%r9, %rdx), %mm5            	# tempBlurred
	movq (%r9, %rax, 2), %mm6         	# tempBlurred
	movq (%r9, %rcx), %mm7            	# tempBlurred
	pavgusb %mm4, %mm0 
	pavgusb %mm5, %mm1 
	pavgusb %mm6, %mm2 
	pavgusb %mm7, %mm3 
	pavgusb %mm4, %mm0 
	pavgusb %mm5, %mm1 
	pavgusb %mm6, %mm2 
	pavgusb %mm7, %mm3 
	movq %mm0, (%r9, %rsi, 4)                	# tempBlurred, D.13381
	movq %mm1, (%r9, %rdx)            	# tempBlurred
	movq %mm2, (%r9, %rax, 2)         	# tempBlurred
	movq %mm3, (%r9, %rcx)            	# tempBlurred
	movq %mm0, (%rdi, %rsi, 4)                	# src, D.13381
	movq %mm1, (%rdi, %rdx)            	# src
	movq %mm2, (%rdi, %rax, 2)         	# src
	movq %mm3, (%rdi, %rcx)            	# src
	jmp 4f                                 
	3:                                     
	lea (%rax, %rsi, 2), %rdx      	# D.13381
	lea (%rdx, %rsi, 2), %rcx      	# D.13381
	movq (%rdi), %mm0                       	# src
	movq (%rdi, %rsi), %mm1                   	# src, D.13381
	movq (%rdi, %rsi, 2), %mm2                	# src, D.13381
	movq (%rdi, %rax), %mm3            	# src
	movq (%r9), %mm4                       	# tempBlurred
	movq (%r9, %rsi), %mm5                   	# tempBlurred, D.13381
	movq (%r9, %rsi, 2), %mm6                	# tempBlurred, D.13381
	movq (%r9, %rax), %mm7            	# tempBlurred
	pavgusb %mm4, %mm0 
	pavgusb %mm5, %mm1 
	pavgusb %mm6, %mm2 
	pavgusb %mm7, %mm3 
	pavgusb %mm4, %mm0 
	pavgusb %mm5, %mm1 
	pavgusb %mm6, %mm2 
	pavgusb %mm7, %mm3 
	pavgusb %mm4, %mm0 
	pavgusb %mm5, %mm1 
	pavgusb %mm6, %mm2 
	pavgusb %mm7, %mm3 
	movq %mm0, (%r9)                       	# tempBlurred
	movq %mm1, (%r9, %rsi)                   	# tempBlurred, D.13381
	movq %mm2, (%r9, %rsi, 2)                	# tempBlurred, D.13381
	movq %mm3, (%r9, %rax)            	# tempBlurred
	movq %mm0, (%rdi)                       	# src
	movq %mm1, (%rdi, %rsi)                   	# src, D.13381
	movq %mm2, (%rdi, %rsi, 2)                	# src, D.13381
	movq %mm3, (%rdi, %rax)            	# src
	movq (%rdi, %rsi, 4), %mm0                	# src, D.13381
	movq (%rdi, %rdx), %mm1            	# src
	movq (%rdi, %rax, 2), %mm2         	# src
	movq (%rdi, %rcx), %mm3            	# src
	movq (%r9, %rsi, 4), %mm4                	# tempBlurred, D.13381
	movq (%r9, %rdx), %mm5            	# tempBlurred
	movq (%r9, %rax, 2), %mm6         	# tempBlurred
	movq (%r9, %rcx), %mm7            	# tempBlurred
	pavgusb %mm4, %mm0 
	pavgusb %mm5, %mm1 
	pavgusb %mm6, %mm2 
	pavgusb %mm7, %mm3 
	pavgusb %mm4, %mm0 
	pavgusb %mm5, %mm1 
	pavgusb %mm6, %mm2 
	pavgusb %mm7, %mm3 
	pavgusb %mm4, %mm0 
	pavgusb %mm5, %mm1 
	pavgusb %mm6, %mm2 
	pavgusb %mm7, %mm3 
	movq %mm0, (%r9, %rsi, 4)                	# tempBlurred, D.13381
	movq %mm1, (%r9, %rdx)            	# tempBlurred
	movq %mm2, (%r9, %rax, 2)         	# tempBlurred
	movq %mm3, (%r9, %rcx)            	# tempBlurred
	movq %mm0, (%rdi, %rsi, 4)                	# src, D.13381
	movq %mm1, (%rdi, %rdx)            	# src
	movq %mm2, (%rdi, %rax, 2)         	# src
	movq %mm3, (%rdi, %rcx)            	# src
	4:                                     
	
# 0 "" 2
#NO_APP
	ret
	.cfi_endproc
.LFE153:
	.size	tempNoiseReducer_3DNow, .-tempNoiseReducer_3DNow
	.type	doVertLowPass_SSE2, @function
doVertLowPass_SSE2:
.LFB159:
	.cfi_startproc
	leal	(%rsi,%rsi,2), %eax	#, D.13389
	cltq
	addq	%rax, %rdi	# D.13390, src
	movslq	%esi, %rsi	# stride, D.13391
#APP
# 232 "postprocess_template.c" 1
	movq 112(%rdx), %mm0                         	# c_8(D)->pQPb
	pxor %mm4, %mm4                      
	movq (%rdi), %mm6                       	# src
	movq (%rdi, %rsi), %mm5                   	# src, D.13391
	movq %mm5, %mm1                      
	movq %mm6, %mm2                      
	psubusb %mm6, %mm5                   
	psubusb %mm1, %mm2                   
	por %mm5, %mm2                       
	psubusb %mm0, %mm2                   
	pcmpeqb %mm4, %mm2                   
	pand %mm2, %mm6                      
	pandn %mm1, %mm2                     
	por %mm2, %mm6                       
	movq (%rdi, %rsi, 8), %mm5                	# src, D.13391
	lea (%rdi, %rsi, 4), %rax             	# src, D.13391
	lea (%rdi, %rsi, 8), %rcx             	# src, D.13391
	sub %rsi, %rcx                      	# D.13391
	add %rsi, %rdi                             	# D.13391, src
	movq (%rdi, %rsi, 8), %mm7                	# src, D.13391
	movq %mm5, %mm1                      
	movq %mm7, %mm2                      
	psubusb %mm7, %mm5                   
	psubusb %mm1, %mm2                   
	por %mm5, %mm2                       
	psubusb %mm0, %mm2                   
	pcmpeqb %mm4, %mm2                   
	pand %mm2, %mm7                      
	pandn %mm1, %mm2                     
	por %mm2, %mm7                       
	movq (%rdi, %rsi), %mm0                   	# src, D.13391
	movq %mm0, %mm1                      
	pavgb %mm6, %mm0 
	pavgb %mm6, %mm0 
	movq (%rdi, %rsi, 4), %mm2                	# src, D.13391
	movq %mm2, %mm5                      
	pavgb (%rax), %mm2 
	pavgb (%rdi, %rsi, 2), %mm2 	# src, D.13391
	movq %mm2, %mm3                      
	movq (%rdi), %mm4                       	# src
	pavgb %mm4, %mm3 
	pavgb %mm0, %mm3 
	movq %mm3, (%rdi)                       	# src
	movq %mm1, %mm0                      
	pavgb %mm6, %mm0 
	movq %mm4, %mm3                      
	pavgb (%rdi,%rsi,2), %mm3 	# src, D.13391
	pavgb (%rax,%rsi,2), %mm5 	# D.13391
	pavgb (%rax), %mm5 
	pavgb %mm5, %mm3 
	pavgb %mm0, %mm3 
	movq %mm3, (%rdi,%rsi)                    	# src, D.13391
	pavgb %mm4, %mm6 
	movq (%rcx), %mm0                
	pavgb (%rax, %rsi, 2), %mm0 	# D.13391
	movq %mm0, %mm3                      
	pavgb %mm1, %mm0 
	pavgb %mm6, %mm0 
	pavgb %mm2, %mm0 
	movq (%rdi, %rsi, 2), %mm2                	# src, D.13391
	movq %mm0, (%rdi, %rsi, 2)                	# src, D.13391
	movq (%rax, %rsi, 4), %mm0         	# D.13391
	pavgb (%rcx), %mm0 
	pavgb %mm0, %mm6 
	pavgb %mm1, %mm4 
	pavgb %mm2, %mm1 
	pavgb %mm1, %mm6 
	pavgb %mm5, %mm6 
	movq (%rax), %mm5                
	movq %mm6, (%rax)                
	movq (%rax, %rsi, 4), %mm6         	# D.13391
	pavgb %mm7, %mm6 
	pavgb %mm4, %mm6 
	pavgb %mm3, %mm6 
	pavgb %mm5, %mm2 
	movq (%rdi, %rsi, 4), %mm4                	# src, D.13391
	pavgb %mm4, %mm2 
	pavgb %mm2, %mm6 
	movq %mm6, (%rdi, %rsi, 4)                	# src, D.13391
	pavgb %mm7, %mm1 
	pavgb %mm4, %mm5 
	pavgb %mm5, %mm0 
	movq (%rax, %rsi, 2), %mm6         	# D.13391
	pavgb %mm6, %mm1 
	pavgb %mm0, %mm1 
	movq %mm1, (%rax, %rsi, 2)         	# D.13391
	pavgb (%rcx), %mm2 
	movq (%rax, %rsi, 4), %mm0         	# D.13391
	pavgb %mm0, %mm6 
	pavgb %mm7, %mm6 
	pavgb %mm2, %mm6 
	movq %mm6, (%rcx)                
	pavgb %mm7, %mm5 
	pavgb %mm7, %mm5 
	pavgb %mm3, %mm0 
	pavgb %mm0, %mm5 
	movq %mm5, (%rax, %rsi, 4)         	# D.13391
	sub %rsi, %rdi                             	# D.13391, src
	
# 0 "" 2
#NO_APP
	ret
	.cfi_endproc
.LFE159:
	.size	doVertLowPass_SSE2, .-doVertLowPass_SSE2
	.type	dering_SSE2, @function
dering_SSE2:
.LFB162:
	.cfi_startproc
	movq	%rdx, %rcx	# c, c
	movslq	%esi, %rsi	# stride, D.13393
	leaq	-32(%rsp), %r8	#, tmp88
#APP
# 1097 "postprocess_template.c" 1
	pxor %mm6, %mm6                      
	pcmpeqb %mm7, %mm7                   
	movq 112(%rcx), %mm0                         	# c_5(D)->pQPb
	punpcklbw %mm6, %mm0                 
	psrlw $1, %mm0                        
	psubw %mm7, %mm0                     
	packuswb %mm0, %mm0                  
	movq %mm0, 120(%rcx)                         	# c_5(D)->pQPb2
	lea (%rdi, %rsi), %rax                	# src, D.13393
	lea (%rax, %rsi, 4), %rdx      	# D.13393
	movq (%rax), %mm0                  
	pminub %mm0, %mm7                    
	pmaxub %mm0, %mm6                    
	movq (%rax, %rsi), %mm0                  	# D.13393
	pminub %mm0, %mm7                    
	pmaxub %mm0, %mm6                    
	movq (%rax, %rsi, 2), %mm0                  	# D.13393
	pminub %mm0, %mm7                    
	pmaxub %mm0, %mm6                    
	movq (%rdi, %rsi, 4), %mm0                  	# src, D.13393
	pminub %mm0, %mm7                    
	pmaxub %mm0, %mm6                    
	movq (%rdx), %mm0                  
	pminub %mm0, %mm7                    
	pmaxub %mm0, %mm6                    
	movq (%rdx, %rsi), %mm0                  	# D.13393
	pminub %mm0, %mm7                    
	pmaxub %mm0, %mm6                    
	movq (%rdx, %rsi, 2), %mm0                  	# D.13393
	pminub %mm0, %mm7                    
	pmaxub %mm0, %mm6                    
	movq (%rdi, %rsi, 8), %mm0                  	# src, D.13393
	pminub %mm0, %mm7                    
	pmaxub %mm0, %mm6                    
	movq %mm7, %mm4                      
	psrlq $8, %mm7                        
	pminub %mm4, %mm7                    
	pshufw $0xF9, %mm7, %mm4             
	pminub %mm4, %mm7                    
	pshufw $0xFE, %mm7, %mm4             
	pminub %mm4, %mm7                    
	movq %mm6, %mm4                      
	psrlq $8, %mm6                        
	pmaxub %mm4, %mm6                    
	pshufw $0xF9, %mm6, %mm4             
	pmaxub %mm4, %mm6                    
	pshufw $0xFE, %mm6, %mm4             
	pmaxub %mm4, %mm6                    
	movq %mm6, %mm0                      
	psubb %mm7, %mm6                     
	push %r8                              	# tmp88
	movd %mm6, %r8d                        	# tmp88
	cmpb deringThreshold, %r8b    	# tmp88
	pop %r8                               	# tmp88
	 jb .skip                                 
	pavgb %mm0, %mm7 
	punpcklbw %mm7, %mm7                 
	punpcklbw %mm7, %mm7                 
	punpcklbw %mm7, %mm7                 
	movq %mm7, (%r8)                       	# tmp88
	movq (%rdi), %mm0                       	# src
	movq %mm0, %mm1                      
	movq %mm0, %mm2                      
	psllq $8, %mm1                        
	psrlq $8, %mm2                        
.skip:
	movd -4(%rdi), %mm3                     	# src
	movd 8(%rdi), %mm4                      	# src
	psrlq $24, %mm3                       
	psllq $56, %mm4                       
	por %mm3, %mm1                       
	por %mm4, %mm2                       
	movq %mm1, %mm3                      
	pavgb %mm2, %mm1 
	pavgb %mm0, %mm1 
	psubusb %mm7, %mm0                   
	psubusb %mm7, %mm2                   
	psubusb %mm7, %mm3                   
	pcmpeqb b00, %mm0           
	pcmpeqb b00, %mm2           
	pcmpeqb b00, %mm3           
	paddb %mm2, %mm0                     
	paddb %mm3, %mm0                     
	movq (%rax), %mm2                
	movq %mm2, %mm3                      
	movq %mm2, %mm4                      
	psllq $8, %mm3                        
	psrlq $8, %mm4                        
	movd -4(%rax), %mm5              
	movd 8(%rax), %mm6               
	psrlq $24, %mm5                       
	psllq $56, %mm6                       
	por %mm5, %mm3                       
	por %mm6, %mm4                       
	movq %mm3, %mm5                      
	pavgb %mm4, %mm3 
	pavgb %mm2, %mm3 
	psubusb %mm7, %mm2                   
	psubusb %mm7, %mm4                   
	psubusb %mm7, %mm5                   
	pcmpeqb b00, %mm2           
	pcmpeqb b00, %mm4           
	pcmpeqb b00, %mm5           
	paddb %mm4, %mm2                     
	paddb %mm5, %mm2                     
	movq (%rax, %rsi), %mm4                 	# D.13393
	movq %mm4, %mm5                  
	movq %mm4, %mm6                  
	psllq $8, %mm5                      
	psrlq $8, %mm6                      
	movd -4(%rax, %rsi), %mm7               	# D.13393
	psrlq $24, %mm7                     
	por %mm7, %mm5                   
	movd 8(%rax, %rsi), %mm7                	# D.13393
	psllq $56, %mm7                     
	por %mm7, %mm6                   
	movq %mm5, %mm7                  
	pavgb %mm6, %mm5 
	pavgb %mm4, %mm5 
	pavgb %mm5, %mm1 
	movq %mm5, 8(%r8)                    	# tmp88
	movq (%r8), %mm5                     	# tmp88
	psubusb %mm5, %mm7               
	psubusb %mm5, %mm6               
	psubusb %mm5, %mm4               
	movq b00, %mm5            
	pcmpeqb %mm5, %mm7               
	pcmpeqb %mm5, %mm6               
	pcmpeqb %mm5, %mm4               
	paddb %mm7, %mm6                 
	paddb %mm6, %mm4                 
	pavgb %mm3, %mm1 
	movq (%rax), %mm6                 
	movq %mm6, %mm7                  
	psubusb 120(%rcx), %mm6                    	# c_5(D)->pQPb2
	paddusb 120(%rcx), %mm7                    	# c_5(D)->pQPb2
	pmaxub %mm6, %mm1 
	pminub %mm7, %mm1 
	paddb %mm4, %mm0               
	paddb %mm2, %mm0              
	#paddb b02, %mm0        
	pand b08, %mm0          
	pcmpeqb %mm5, %mm0             
	pand %mm0, %mm1              
	pandn (%rax), %mm0              
	por %mm1, %mm0               
	movq %mm0, (%rax)               
	movq 8(%r8), %mm5                    	# tmp88
	movq (%rax, %rsi, 2), %mm0                 	# D.13393
	movq %mm0, %mm1                  
	movq %mm0, %mm6                  
	psllq $8, %mm1                      
	psrlq $8, %mm6                      
	movd -4(%rax, %rsi, 2), %mm7               	# D.13393
	psrlq $24, %mm7                     
	por %mm7, %mm1                   
	movd 8(%rax, %rsi, 2), %mm7                	# D.13393
	psllq $56, %mm7                     
	por %mm7, %mm6                   
	movq %mm1, %mm7                  
	pavgb %mm6, %mm1 
	pavgb %mm0, %mm1 
	pavgb %mm1, %mm3 
	movq %mm1, 8(%r8)                    	# tmp88
	movq (%r8), %mm1                     	# tmp88
	psubusb %mm1, %mm7               
	psubusb %mm1, %mm6               
	psubusb %mm1, %mm0               
	movq b00, %mm1            
	pcmpeqb %mm1, %mm7               
	pcmpeqb %mm1, %mm6               
	pcmpeqb %mm1, %mm0               
	paddb %mm7, %mm6                 
	paddb %mm6, %mm0                 
	pavgb %mm5, %mm3 
	movq (%rax, %rsi), %mm6                 	# D.13393
	movq %mm6, %mm7                  
	psubusb 120(%rcx), %mm6                    	# c_5(D)->pQPb2
	paddusb 120(%rcx), %mm7                    	# c_5(D)->pQPb2
	pmaxub %mm6, %mm3 
	pminub %mm7, %mm3 
	paddb %mm0, %mm2               
	paddb %mm4, %mm2              
	#paddb b02, %mm2        
	pand b08, %mm2          
	pcmpeqb %mm1, %mm2             
	pand %mm2, %mm3              
	pandn (%rax, %rsi), %mm2              	# D.13393
	por %mm3, %mm2               
	movq %mm2, (%rax, %rsi)               	# D.13393
	movq 8(%r8), %mm1                    	# tmp88
	movq (%rdi, %rsi, 4), %mm2                 	# src, D.13393
	movq %mm2, %mm3                  
	movq %mm2, %mm6                  
	psllq $8, %mm3                      
	psrlq $8, %mm6                      
	movd -4(%rdi, %rsi, 4), %mm7               	# src, D.13393
	psrlq $24, %mm7                     
	por %mm7, %mm3                   
	movd 8(%rdi, %rsi, 4), %mm7                	# src, D.13393
	psllq $56, %mm7                     
	por %mm7, %mm6                   
	movq %mm3, %mm7                  
	pavgb %mm6, %mm3 
	pavgb %mm2, %mm3 
	pavgb %mm3, %mm5 
	movq %mm3, 8(%r8)                    	# tmp88
	movq (%r8), %mm3                     	# tmp88
	psubusb %mm3, %mm7               
	psubusb %mm3, %mm6               
	psubusb %mm3, %mm2               
	movq b00, %mm3            
	pcmpeqb %mm3, %mm7               
	pcmpeqb %mm3, %mm6               
	pcmpeqb %mm3, %mm2               
	paddb %mm7, %mm6                 
	paddb %mm6, %mm2                 
	pavgb %mm1, %mm5 
	movq (%rax, %rsi, 2), %mm6                 	# D.13393
	movq %mm6, %mm7                  
	psubusb 120(%rcx), %mm6                    	# c_5(D)->pQPb2
	paddusb 120(%rcx), %mm7                    	# c_5(D)->pQPb2
	pmaxub %mm6, %mm5 
	pminub %mm7, %mm5 
	paddb %mm2, %mm4               
	paddb %mm0, %mm4              
	#paddb b02, %mm4        
	pand b08, %mm4          
	pcmpeqb %mm3, %mm4             
	pand %mm4, %mm5              
	pandn (%rax, %rsi, 2), %mm4              	# D.13393
	por %mm5, %mm4               
	movq %mm4, (%rax, %rsi, 2)               	# D.13393
	movq 8(%r8), %mm3                    	# tmp88
	movq (%rdx), %mm4                 
	movq %mm4, %mm5                  
	movq %mm4, %mm6                  
	psllq $8, %mm5                      
	psrlq $8, %mm6                      
	movd -4(%rdx), %mm7               
	psrlq $24, %mm7                     
	por %mm7, %mm5                   
	movd 8(%rdx), %mm7                
	psllq $56, %mm7                     
	por %mm7, %mm6                   
	movq %mm5, %mm7                  
	pavgb %mm6, %mm5 
	pavgb %mm4, %mm5 
	pavgb %mm5, %mm1 
	movq %mm5, 8(%r8)                    	# tmp88
	movq (%r8), %mm5                     	# tmp88
	psubusb %mm5, %mm7               
	psubusb %mm5, %mm6               
	psubusb %mm5, %mm4               
	movq b00, %mm5            
	pcmpeqb %mm5, %mm7               
	pcmpeqb %mm5, %mm6               
	pcmpeqb %mm5, %mm4               
	paddb %mm7, %mm6                 
	paddb %mm6, %mm4                 
	pavgb %mm3, %mm1 
	movq (%rdi, %rsi, 4), %mm6                 	# src, D.13393
	movq %mm6, %mm7                  
	psubusb 120(%rcx), %mm6                    	# c_5(D)->pQPb2
	paddusb 120(%rcx), %mm7                    	# c_5(D)->pQPb2
	pmaxub %mm6, %mm1 
	pminub %mm7, %mm1 
	paddb %mm4, %mm0               
	paddb %mm2, %mm0              
	#paddb b02, %mm0        
	pand b08, %mm0          
	pcmpeqb %mm5, %mm0             
	pand %mm0, %mm1              
	pandn (%rdi, %rsi, 4), %mm0              	# src, D.13393
	por %mm1, %mm0               
	movq %mm0, (%rdi, %rsi, 4)               	# src, D.13393
	movq 8(%r8), %mm5                    	# tmp88
	movq (%rdx, %rsi), %mm0                 	# D.13393
	movq %mm0, %mm1                  
	movq %mm0, %mm6                  
	psllq $8, %mm1                      
	psrlq $8, %mm6                      
	movd -4(%rdx, %rsi), %mm7               	# D.13393
	psrlq $24, %mm7                     
	por %mm7, %mm1                   
	movd 8(%rdx, %rsi), %mm7                	# D.13393
	psllq $56, %mm7                     
	por %mm7, %mm6                   
	movq %mm1, %mm7                  
	pavgb %mm6, %mm1 
	pavgb %mm0, %mm1 
	pavgb %mm1, %mm3 
	movq %mm1, 8(%r8)                    	# tmp88
	movq (%r8), %mm1                     	# tmp88
	psubusb %mm1, %mm7               
	psubusb %mm1, %mm6               
	psubusb %mm1, %mm0               
	movq b00, %mm1            
	pcmpeqb %mm1, %mm7               
	pcmpeqb %mm1, %mm6               
	pcmpeqb %mm1, %mm0               
	paddb %mm7, %mm6                 
	paddb %mm6, %mm0                 
	pavgb %mm5, %mm3 
	movq (%rdx), %mm6                 
	movq %mm6, %mm7                  
	psubusb 120(%rcx), %mm6                    	# c_5(D)->pQPb2
	paddusb 120(%rcx), %mm7                    	# c_5(D)->pQPb2
	pmaxub %mm6, %mm3 
	pminub %mm7, %mm3 
	paddb %mm0, %mm2               
	paddb %mm4, %mm2              
	#paddb b02, %mm2        
	pand b08, %mm2          
	pcmpeqb %mm1, %mm2             
	pand %mm2, %mm3              
	pandn (%rdx), %mm2              
	por %mm3, %mm2               
	movq %mm2, (%rdx)               
	movq 8(%r8), %mm1                    	# tmp88
	movq (%rdx, %rsi, 2), %mm2                 	# D.13393
	movq %mm2, %mm3                  
	movq %mm2, %mm6                  
	psllq $8, %mm3                      
	psrlq $8, %mm6                      
	movd -4(%rdx, %rsi, 2), %mm7               	# D.13393
	psrlq $24, %mm7                     
	por %mm7, %mm3                   
	movd 8(%rdx, %rsi, 2), %mm7                	# D.13393
	psllq $56, %mm7                     
	por %mm7, %mm6                   
	movq %mm3, %mm7                  
	pavgb %mm6, %mm3 
	pavgb %mm2, %mm3 
	pavgb %mm3, %mm5 
	movq %mm3, 8(%r8)                    	# tmp88
	movq (%r8), %mm3                     	# tmp88
	psubusb %mm3, %mm7               
	psubusb %mm3, %mm6               
	psubusb %mm3, %mm2               
	movq b00, %mm3            
	pcmpeqb %mm3, %mm7               
	pcmpeqb %mm3, %mm6               
	pcmpeqb %mm3, %mm2               
	paddb %mm7, %mm6                 
	paddb %mm6, %mm2                 
	pavgb %mm1, %mm5 
	movq (%rdx, %rsi), %mm6                 	# D.13393
	movq %mm6, %mm7                  
	psubusb 120(%rcx), %mm6                    	# c_5(D)->pQPb2
	paddusb 120(%rcx), %mm7                    	# c_5(D)->pQPb2
	pmaxub %mm6, %mm5 
	pminub %mm7, %mm5 
	paddb %mm2, %mm4               
	paddb %mm0, %mm4              
	#paddb b02, %mm4        
	pand b08, %mm4          
	pcmpeqb %mm3, %mm4             
	pand %mm4, %mm5              
	pandn (%rdx, %rsi), %mm4              	# D.13393
	por %mm5, %mm4               
	movq %mm4, (%rdx, %rsi)               	# D.13393
	movq 8(%r8), %mm3                    	# tmp88
	movq (%rdi, %rsi, 8), %mm4                 	# src, D.13393
	movq %mm4, %mm5                  
	movq %mm4, %mm6                  
	psllq $8, %mm5                      
	psrlq $8, %mm6                      
	movd -4(%rdi, %rsi, 8), %mm7               	# src, D.13393
	psrlq $24, %mm7                     
	por %mm7, %mm5                   
	movd 8(%rdi, %rsi, 8), %mm7                	# src, D.13393
	psllq $56, %mm7                     
	por %mm7, %mm6                   
	movq %mm5, %mm7                  
	pavgb %mm6, %mm5 
	pavgb %mm4, %mm5 
	pavgb %mm5, %mm1 
	movq %mm5, 8(%r8)                    	# tmp88
	movq (%r8), %mm5                     	# tmp88
	psubusb %mm5, %mm7               
	psubusb %mm5, %mm6               
	psubusb %mm5, %mm4               
	movq b00, %mm5            
	pcmpeqb %mm5, %mm7               
	pcmpeqb %mm5, %mm6               
	pcmpeqb %mm5, %mm4               
	paddb %mm7, %mm6                 
	paddb %mm6, %mm4                 
	pavgb %mm3, %mm1 
	movq (%rdx, %rsi, 2), %mm6                 	# D.13393
	movq %mm6, %mm7                  
	psubusb 120(%rcx), %mm6                    	# c_5(D)->pQPb2
	paddusb 120(%rcx), %mm7                    	# c_5(D)->pQPb2
	pmaxub %mm6, %mm1 
	pminub %mm7, %mm1 
	paddb %mm4, %mm0               
	paddb %mm2, %mm0              
	#paddb b02, %mm0        
	pand b08, %mm0          
	pcmpeqb %mm5, %mm0             
	pand %mm0, %mm1              
	pandn (%rdx, %rsi, 2), %mm0              	# D.13393
	por %mm1, %mm0               
	movq %mm0, (%rdx, %rsi, 2)               	# D.13393
	movq 8(%r8), %mm5                    	# tmp88
	movq (%rdx, %rsi, 4), %mm0                 	# D.13393
	movq %mm0, %mm1                  
	movq %mm0, %mm6                  
	psllq $8, %mm1                      
	psrlq $8, %mm6                      
	movd -4(%rdx, %rsi, 4), %mm7               	# D.13393
	psrlq $24, %mm7                     
	por %mm7, %mm1                   
	movd 8(%rdx, %rsi, 4), %mm7                	# D.13393
	psllq $56, %mm7                     
	por %mm7, %mm6                   
	movq %mm1, %mm7                  
	pavgb %mm6, %mm1 
	pavgb %mm0, %mm1 
	pavgb %mm1, %mm3 
	movq %mm1, 8(%r8)                    	# tmp88
	movq (%r8), %mm1                     	# tmp88
	psubusb %mm1, %mm7               
	psubusb %mm1, %mm6               
	psubusb %mm1, %mm0               
	movq b00, %mm1            
	pcmpeqb %mm1, %mm7               
	pcmpeqb %mm1, %mm6               
	pcmpeqb %mm1, %mm0               
	paddb %mm7, %mm6                 
	paddb %mm6, %mm0                 
	pavgb %mm5, %mm3 
	movq (%rdi, %rsi, 8), %mm6                 	# src, D.13393
	movq %mm6, %mm7                  
	psubusb 120(%rcx), %mm6                    	# c_5(D)->pQPb2
	paddusb 120(%rcx), %mm7                    	# c_5(D)->pQPb2
	pmaxub %mm6, %mm3 
	pminub %mm7, %mm3 
	paddb %mm0, %mm2               
	paddb %mm4, %mm2              
	#paddb b02, %mm2        
	pand b08, %mm2          
	pcmpeqb %mm1, %mm2             
	pand %mm2, %mm3              
	pandn (%rdi, %rsi, 8), %mm2              	# src, D.13393
	por %mm3, %mm2               
	movq %mm2, (%rdi, %rsi, 8)               	# src, D.13393
	movq 8(%r8), %mm1                    	# tmp88
	1:                        
	
# 0 "" 2
#NO_APP
	ret
	.cfi_endproc
.LFE162:
	.size	dering_SSE2, .-dering_SSE2
	.type	deInterlaceFF_SSE2, @function
deInterlaceFF_SSE2:
.LFB165:
	.cfi_startproc
	movq	%rdx, %rcx	# tmp, tmp
	leal	0(,%rsi,4), %eax	#, D.13401
	cltq
	addq	%rax, %rdi	# D.13402, src
	movslq	%esi, %rsi	# stride, D.13403
#APP
# 1595 "postprocess_template.c" 1
	lea (%rdi, %rsi), %rax                	# src, D.13403
	lea (%rax, %rsi, 4), %rdx      	# D.13403
	pxor %mm7, %mm7                      
	movq (%rcx), %mm0                       	# tmp
	movq (%rdi), %mm1                     	# src
	movq (%rax), %mm2                     
	movq (%rax, %rsi), %mm3                     	# D.13403
	movq (%rax, %rsi, 2), %mm4                     	# D.13403
	pavgb %mm3, %mm1 
	pavgb %mm4, %mm0 
	movq %mm0, %mm3                      
	punpcklbw %mm7, %mm0                 
	punpckhbw %mm7, %mm3                 
	movq %mm1, %mm4                      
	punpcklbw %mm7, %mm1                 
	punpckhbw %mm7, %mm4                 
	psllw $2, %mm1                        
	psllw $2, %mm4                        
	psubw %mm0, %mm1                     
	psubw %mm3, %mm4                     
	movq %mm2, %mm5                      
	movq %mm2, %mm0                      
	punpcklbw %mm7, %mm2                 
	punpckhbw %mm7, %mm5                 
	paddw %mm2, %mm1                     
	paddw %mm5, %mm4                     
	psraw $2, %mm1                        
	psraw $2, %mm4                        
	packuswb %mm4, %mm1                  
	movq %mm1, (%rax)                     
	movq (%rax, %rsi), %mm1                     	# D.13403
	movq (%rax, %rsi, 2), %mm2                     	# D.13403
	movq (%rdi, %rsi, 4), %mm3                     	# src, D.13403
	movq (%rdx), %mm4                     
	pavgb %mm3, %mm1 
	pavgb %mm4, %mm0 
	movq %mm0, %mm3                      
	punpcklbw %mm7, %mm0                 
	punpckhbw %mm7, %mm3                 
	movq %mm1, %mm4                      
	punpcklbw %mm7, %mm1                 
	punpckhbw %mm7, %mm4                 
	psllw $2, %mm1                        
	psllw $2, %mm4                        
	psubw %mm0, %mm1                     
	psubw %mm3, %mm4                     
	movq %mm2, %mm5                      
	movq %mm2, %mm0                      
	punpcklbw %mm7, %mm2                 
	punpckhbw %mm7, %mm5                 
	paddw %mm2, %mm1                     
	paddw %mm5, %mm4                     
	psraw $2, %mm1                        
	psraw $2, %mm4                        
	packuswb %mm4, %mm1                  
	movq %mm1, (%rax, %rsi, 2)                     	# D.13403
	movq (%rdi, %rsi, 4), %mm1                     	# src, D.13403
	movq (%rdx), %mm2                     
	movq (%rdx, %rsi), %mm3                     	# D.13403
	movq (%rdx, %rsi, 2), %mm4                     	# D.13403
	pavgb %mm3, %mm1 
	pavgb %mm4, %mm0 
	movq %mm0, %mm3                      
	punpcklbw %mm7, %mm0                 
	punpckhbw %mm7, %mm3                 
	movq %mm1, %mm4                      
	punpcklbw %mm7, %mm1                 
	punpckhbw %mm7, %mm4                 
	psllw $2, %mm1                        
	psllw $2, %mm4                        
	psubw %mm0, %mm1                     
	psubw %mm3, %mm4                     
	movq %mm2, %mm5                      
	movq %mm2, %mm0                      
	punpcklbw %mm7, %mm2                 
	punpckhbw %mm7, %mm5                 
	paddw %mm2, %mm1                     
	paddw %mm5, %mm4                     
	psraw $2, %mm1                        
	psraw $2, %mm4                        
	packuswb %mm4, %mm1                  
	movq %mm1, (%rdx)                     
	movq (%rdx, %rsi), %mm1                     	# D.13403
	movq (%rdx, %rsi, 2), %mm2                     	# D.13403
	movq (%rdi, %rsi, 8), %mm3                     	# src, D.13403
	movq (%rdx, %rsi, 4), %mm4                     	# D.13403
	pavgb %mm3, %mm1 
	pavgb %mm4, %mm0 
	movq %mm0, %mm3                      
	punpcklbw %mm7, %mm0                 
	punpckhbw %mm7, %mm3                 
	movq %mm1, %mm4                      
	punpcklbw %mm7, %mm1                 
	punpckhbw %mm7, %mm4                 
	psllw $2, %mm1                        
	psllw $2, %mm4                        
	psubw %mm0, %mm1                     
	psubw %mm3, %mm4                     
	movq %mm2, %mm5                      
	movq %mm2, %mm0                      
	punpcklbw %mm7, %mm2                 
	punpckhbw %mm7, %mm5                 
	paddw %mm2, %mm1                     
	paddw %mm5, %mm4                     
	psraw $2, %mm1                        
	psraw $2, %mm4                        
	packuswb %mm4, %mm1                  
	movq %mm1, (%rdx, %rsi, 2)                     	# D.13403
	movq %mm0, (%rcx)                       	# tmp
	
# 0 "" 2
#NO_APP
	ret
	.cfi_endproc
.LFE165:
	.size	deInterlaceFF_SSE2, .-deInterlaceFF_SSE2
	.type	deInterlaceL5_SSE2, @function
deInterlaceL5_SSE2:
.LFB166:
	.cfi_startproc
	movq	%rdx, %r8	# tmp, tmp
	leal	0(,%rsi,4), %eax	#, D.13405
	cltq
	addq	%rax, %rdi	# D.13406, src
	movslq	%esi, %rsi	# stride, D.13407
#APP
# 1674 "postprocess_template.c" 1
	lea (%rdi, %rsi), %rax                	# src, D.13407
	lea (%rax, %rsi, 4), %rdx      	# D.13407
	pxor %mm7, %mm7                      
	movq (%r8), %mm0                       	# tmp
	movq (%rcx), %mm1                       	# tmp2
	movq (%rdi), %mm2                     	# src
	movq (%rax), %mm3                     
	movq (%rax, %rsi), %mm4                     	# D.13407
	pavgb %mm1, %mm3 
	pavgb %mm0, %mm4 
	movq %mm2, %mm5                      
	movq %mm2, %mm0                    
	punpcklbw %mm7, %mm2                 
	punpckhbw %mm7, %mm5                 
	movq %mm2, %mm6                      
	paddw %mm2, %mm2                     
	paddw %mm6, %mm2                     
	movq %mm5, %mm6                      
	paddw %mm5, %mm5                     
	paddw %mm6, %mm5                     
	movq %mm3, %mm6                      
	punpcklbw %mm7, %mm3                 
	punpckhbw %mm7, %mm6                 
	paddw %mm3, %mm3                     
	paddw %mm6, %mm6                     
	paddw %mm3, %mm2                     
	paddw %mm6, %mm5                     
	movq %mm4, %mm6                      
	punpcklbw %mm7, %mm4                 
	punpckhbw %mm7, %mm6                 
	psubw %mm4, %mm2                     
	psubw %mm6, %mm5                     
	psraw $2, %mm2                        
	psraw $2, %mm5                        
	packuswb %mm5, %mm2                  
	movq %mm2, (%rdi)                     	# src
	movq (%rax), %mm2                     
	movq (%rax, %rsi), %mm3                     	# D.13407
	movq (%rax, %rsi, 2), %mm4                     	# D.13407
	pavgb %mm0, %mm3 
	pavgb %mm1, %mm4 
	movq %mm2, %mm5                      
	movq %mm2, %mm1                    
	punpcklbw %mm7, %mm2                 
	punpckhbw %mm7, %mm5                 
	movq %mm2, %mm6                      
	paddw %mm2, %mm2                     
	paddw %mm6, %mm2                     
	movq %mm5, %mm6                      
	paddw %mm5, %mm5                     
	paddw %mm6, %mm5                     
	movq %mm3, %mm6                      
	punpcklbw %mm7, %mm3                 
	punpckhbw %mm7, %mm6                 
	paddw %mm3, %mm3                     
	paddw %mm6, %mm6                     
	paddw %mm3, %mm2                     
	paddw %mm6, %mm5                     
	movq %mm4, %mm6                      
	punpcklbw %mm7, %mm4                 
	punpckhbw %mm7, %mm6                 
	psubw %mm4, %mm2                     
	psubw %mm6, %mm5                     
	psraw $2, %mm2                        
	psraw $2, %mm5                        
	packuswb %mm5, %mm2                  
	movq %mm2, (%rax)                     
	movq (%rax, %rsi), %mm2                     	# D.13407
	movq (%rax, %rsi, 2), %mm3                     	# D.13407
	movq (%rdi, %rsi, 4), %mm4                     	# src, D.13407
	pavgb %mm1, %mm3 
	pavgb %mm0, %mm4 
	movq %mm2, %mm5                      
	movq %mm2, %mm0                    
	punpcklbw %mm7, %mm2                 
	punpckhbw %mm7, %mm5                 
	movq %mm2, %mm6                      
	paddw %mm2, %mm2                     
	paddw %mm6, %mm2                     
	movq %mm5, %mm6                      
	paddw %mm5, %mm5                     
	paddw %mm6, %mm5                     
	movq %mm3, %mm6                      
	punpcklbw %mm7, %mm3                 
	punpckhbw %mm7, %mm6                 
	paddw %mm3, %mm3                     
	paddw %mm6, %mm6                     
	paddw %mm3, %mm2                     
	paddw %mm6, %mm5                     
	movq %mm4, %mm6                      
	punpcklbw %mm7, %mm4                 
	punpckhbw %mm7, %mm6                 
	psubw %mm4, %mm2                     
	psubw %mm6, %mm5                     
	psraw $2, %mm2                        
	psraw $2, %mm5                        
	packuswb %mm5, %mm2                  
	movq %mm2, (%rax, %rsi)                     	# D.13407
	movq (%rax, %rsi, 2), %mm2                     	# D.13407
	movq (%rdi, %rsi, 4), %mm3                     	# src, D.13407
	movq (%rdx), %mm4                     
	pavgb %mm0, %mm3 
	pavgb %mm1, %mm4 
	movq %mm2, %mm5                      
	movq %mm2, %mm1                    
	punpcklbw %mm7, %mm2                 
	punpckhbw %mm7, %mm5                 
	movq %mm2, %mm6                      
	paddw %mm2, %mm2                     
	paddw %mm6, %mm2                     
	movq %mm5, %mm6                      
	paddw %mm5, %mm5                     
	paddw %mm6, %mm5                     
	movq %mm3, %mm6                      
	punpcklbw %mm7, %mm3                 
	punpckhbw %mm7, %mm6                 
	paddw %mm3, %mm3                     
	paddw %mm6, %mm6                     
	paddw %mm3, %mm2                     
	paddw %mm6, %mm5                     
	movq %mm4, %mm6                      
	punpcklbw %mm7, %mm4                 
	punpckhbw %mm7, %mm6                 
	psubw %mm4, %mm2                     
	psubw %mm6, %mm5                     
	psraw $2, %mm2                        
	psraw $2, %mm5                        
	packuswb %mm5, %mm2                  
	movq %mm2, (%rax, %rsi, 2)                     	# D.13407
	movq (%rdi, %rsi, 4), %mm2                     	# src, D.13407
	movq (%rdx), %mm3                     
	movq (%rdx, %rsi), %mm4                     	# D.13407
	pavgb %mm1, %mm3 
	pavgb %mm0, %mm4 
	movq %mm2, %mm5                      
	movq %mm2, %mm0                    
	punpcklbw %mm7, %mm2                 
	punpckhbw %mm7, %mm5                 
	movq %mm2, %mm6                      
	paddw %mm2, %mm2                     
	paddw %mm6, %mm2                     
	movq %mm5, %mm6                      
	paddw %mm5, %mm5                     
	paddw %mm6, %mm5                     
	movq %mm3, %mm6                      
	punpcklbw %mm7, %mm3                 
	punpckhbw %mm7, %mm6                 
	paddw %mm3, %mm3                     
	paddw %mm6, %mm6                     
	paddw %mm3, %mm2                     
	paddw %mm6, %mm5                     
	movq %mm4, %mm6                      
	punpcklbw %mm7, %mm4                 
	punpckhbw %mm7, %mm6                 
	psubw %mm4, %mm2                     
	psubw %mm6, %mm5                     
	psraw $2, %mm2                        
	psraw $2, %mm5                        
	packuswb %mm5, %mm2                  
	movq %mm2, (%rdi, %rsi, 4)                     	# src, D.13407
	movq (%rdx), %mm2                     
	movq (%rdx, %rsi), %mm3                     	# D.13407
	movq (%rdx, %rsi, 2), %mm4                     	# D.13407
	pavgb %mm0, %mm3 
	pavgb %mm1, %mm4 
	movq %mm2, %mm5                      
	movq %mm2, %mm1                    
	punpcklbw %mm7, %mm2                 
	punpckhbw %mm7, %mm5                 
	movq %mm2, %mm6                      
	paddw %mm2, %mm2                     
	paddw %mm6, %mm2                     
	movq %mm5, %mm6                      
	paddw %mm5, %mm5                     
	paddw %mm6, %mm5                     
	movq %mm3, %mm6                      
	punpcklbw %mm7, %mm3                 
	punpckhbw %mm7, %mm6                 
	paddw %mm3, %mm3                     
	paddw %mm6, %mm6                     
	paddw %mm3, %mm2                     
	paddw %mm6, %mm5                     
	movq %mm4, %mm6                      
	punpcklbw %mm7, %mm4                 
	punpckhbw %mm7, %mm6                 
	psubw %mm4, %mm2                     
	psubw %mm6, %mm5                     
	psraw $2, %mm2                        
	psraw $2, %mm5                        
	packuswb %mm5, %mm2                  
	movq %mm2, (%rdx)                     
	movq (%rdx, %rsi), %mm2                     	# D.13407
	movq (%rdx, %rsi, 2), %mm3                     	# D.13407
	movq (%rdi, %rsi, 8), %mm4                     	# src, D.13407
	pavgb %mm1, %mm3 
	pavgb %mm0, %mm4 
	movq %mm2, %mm5                      
	movq %mm2, %mm0                    
	punpcklbw %mm7, %mm2                 
	punpckhbw %mm7, %mm5                 
	movq %mm2, %mm6                      
	paddw %mm2, %mm2                     
	paddw %mm6, %mm2                     
	movq %mm5, %mm6                      
	paddw %mm5, %mm5                     
	paddw %mm6, %mm5                     
	movq %mm3, %mm6                      
	punpcklbw %mm7, %mm3                 
	punpckhbw %mm7, %mm6                 
	paddw %mm3, %mm3                     
	paddw %mm6, %mm6                     
	paddw %mm3, %mm2                     
	paddw %mm6, %mm5                     
	movq %mm4, %mm6                      
	punpcklbw %mm7, %mm4                 
	punpckhbw %mm7, %mm6                 
	psubw %mm4, %mm2                     
	psubw %mm6, %mm5                     
	psraw $2, %mm2                        
	psraw $2, %mm5                        
	packuswb %mm5, %mm2                  
	movq %mm2, (%rdx, %rsi)                     	# D.13407
	movq (%rdx, %rsi, 2), %mm2                     	# D.13407
	movq (%rdi, %rsi, 8), %mm3                     	# src, D.13407
	movq (%rdx, %rsi, 4), %mm4                     	# D.13407
	pavgb %mm0, %mm3 
	pavgb %mm1, %mm4 
	movq %mm2, %mm5                      
	movq %mm2, %mm1                    
	punpcklbw %mm7, %mm2                 
	punpckhbw %mm7, %mm5                 
	movq %mm2, %mm6                      
	paddw %mm2, %mm2                     
	paddw %mm6, %mm2                     
	movq %mm5, %mm6                      
	paddw %mm5, %mm5                     
	paddw %mm6, %mm5                     
	movq %mm3, %mm6                      
	punpcklbw %mm7, %mm3                 
	punpckhbw %mm7, %mm6                 
	paddw %mm3, %mm3                     
	paddw %mm6, %mm6                     
	paddw %mm3, %mm2                     
	paddw %mm6, %mm5                     
	movq %mm4, %mm6                      
	punpcklbw %mm7, %mm4                 
	punpckhbw %mm7, %mm6                 
	psubw %mm4, %mm2                     
	psubw %mm6, %mm5                     
	psraw $2, %mm2                        
	psraw $2, %mm5                        
	packuswb %mm5, %mm2                  
	movq %mm2, (%rdx, %rsi, 2)                     	# D.13407
	movq %mm0, (%r8)                       	# tmp
	movq %mm1, (%rcx)                       	# tmp2
	
# 0 "" 2
#NO_APP
	ret
	.cfi_endproc
.LFE166:
	.size	deInterlaceL5_SSE2, .-deInterlaceL5_SSE2
	.type	tempNoiseReducer_SSE2, @function
tempNoiseReducer_SSE2:
.LFB171:
	.cfi_startproc
	movq	%rdx, %r9	# tempBlurred, tempBlurred
	movq	%rcx, -8(%rsp)	# tempBlurredPast, tempBlurredPast
	movl	(%r8), %eax	# *maxNoise_3(D), *maxNoise_3(D)
	movl	%eax, 508(%rcx)	# *maxNoise_3(D), MEM[(uint32_t *)_2 + 508B]
	movl	4(%r8), %eax	# MEM[(const int *)maxNoise_3(D) + 4B], MEM[(const int *)maxNoise_3(D) + 4B]
	movl	%eax, 512(%rcx)	# MEM[(const int *)maxNoise_3(D) + 4B], MEM[(uint32_t *)_2 + 512B]
	movl	8(%r8), %eax	# MEM[(const int *)maxNoise_3(D) + 8B], MEM[(const int *)maxNoise_3(D) + 8B]
	movl	%eax, 516(%rcx)	# MEM[(const int *)maxNoise_3(D) + 8B], MEM[(uint32_t *)_2 + 516B]
	movslq	%esi, %rsi	# stride, D.13420
#APP
# 2169 "postprocess_template.c" 1
	lea (%rsi, %rsi, 2), %rax             	# D.13420
	lea (%rsi, %rsi, 4), %rdx             	# D.13420
	lea (%rdx, %rsi, 2), %rcx      	# D.13420
	pcmpeqb %mm7, %mm7                   
	movq b80, %mm6              
	pxor %mm0, %mm0                      
	movq (%rdi), %mm5                     	# src
	movq (%r9), %mm2                     	# tempBlurred
	pxor %mm7, %mm2                      
	pavgb %mm2, %mm5 
	paddb %mm6, %mm5                     
	movq %mm5, %mm2                      
	psllw $8, %mm5                        
	pmaddwd %mm5, %mm5                   
	pmaddwd %mm2, %mm2                   
	paddd %mm2, %mm5                     
	psrld $14, %mm5                       
	paddd %mm5, %mm0                     
	movq (%rdi, %rsi), %mm5                     	# src, D.13420
	movq (%r9, %rsi), %mm2                     	# tempBlurred, D.13420
	pxor %mm7, %mm2                      
	pavgb %mm2, %mm5 
	paddb %mm6, %mm5                     
	movq %mm5, %mm2                      
	psllw $8, %mm5                        
	pmaddwd %mm5, %mm5                   
	pmaddwd %mm2, %mm2                   
	paddd %mm2, %mm5                     
	psrld $14, %mm5                       
	paddd %mm5, %mm0                     
	movq (%rdi, %rsi, 2), %mm5                     	# src, D.13420
	movq (%r9, %rsi, 2), %mm2                     	# tempBlurred, D.13420
	pxor %mm7, %mm2                      
	pavgb %mm2, %mm5 
	paddb %mm6, %mm5                     
	movq %mm5, %mm2                      
	psllw $8, %mm5                        
	pmaddwd %mm5, %mm5                   
	pmaddwd %mm2, %mm2                   
	paddd %mm2, %mm5                     
	psrld $14, %mm5                       
	paddd %mm5, %mm0                     
	movq (%rdi, %rax), %mm5                     	# src
	movq (%r9, %rax), %mm2                     	# tempBlurred
	pxor %mm7, %mm2                      
	pavgb %mm2, %mm5 
	paddb %mm6, %mm5                     
	movq %mm5, %mm2                      
	psllw $8, %mm5                        
	pmaddwd %mm5, %mm5                   
	pmaddwd %mm2, %mm2                   
	paddd %mm2, %mm5                     
	psrld $14, %mm5                       
	paddd %mm5, %mm0                     
	movq (%rdi, %rsi, 4), %mm5                     	# src, D.13420
	movq (%r9, %rsi, 4), %mm2                     	# tempBlurred, D.13420
	pxor %mm7, %mm2                      
	pavgb %mm2, %mm5 
	paddb %mm6, %mm5                     
	movq %mm5, %mm2                      
	psllw $8, %mm5                        
	pmaddwd %mm5, %mm5                   
	pmaddwd %mm2, %mm2                   
	paddd %mm2, %mm5                     
	psrld $14, %mm5                       
	paddd %mm5, %mm0                     
	movq (%rdi, %rdx), %mm5                     	# src
	movq (%r9, %rdx), %mm2                     	# tempBlurred
	pxor %mm7, %mm2                      
	pavgb %mm2, %mm5 
	paddb %mm6, %mm5                     
	movq %mm5, %mm2                      
	psllw $8, %mm5                        
	pmaddwd %mm5, %mm5                   
	pmaddwd %mm2, %mm2                   
	paddd %mm2, %mm5                     
	psrld $14, %mm5                       
	paddd %mm5, %mm0                     
	movq (%rdi, %rax,2), %mm5                     	# src
	movq (%r9, %rax,2), %mm2                     	# tempBlurred
	pxor %mm7, %mm2                      
	pavgb %mm2, %mm5 
	paddb %mm6, %mm5                     
	movq %mm5, %mm2                      
	psllw $8, %mm5                        
	pmaddwd %mm5, %mm5                   
	pmaddwd %mm2, %mm2                   
	paddd %mm2, %mm5                     
	psrld $14, %mm5                       
	paddd %mm5, %mm0                     
	movq (%rdi, %rcx), %mm5                     	# src
	movq (%r9, %rcx), %mm2                     	# tempBlurred
	pxor %mm7, %mm2                      
	pavgb %mm2, %mm5 
	paddb %mm6, %mm5                     
	movq %mm5, %mm2                      
	psllw $8, %mm5                        
	pmaddwd %mm5, %mm5                   
	pmaddwd %mm2, %mm2                   
	paddd %mm2, %mm5                     
	psrld $14, %mm5                       
	paddd %mm5, %mm0                     
	movq %mm0, %mm4                      
	psrlq $32, %mm0                       
	paddd %mm0, %mm4                     
	movd %mm4, %ecx                      
	shll $2, %ecx                         
	mov -8(%rsp), %rdx                      	# tempBlurredPast
	addl -4(%rdx), %ecx              
	addl 4(%rdx), %ecx               
	addl -1024(%rdx), %ecx           
	addl $4, %ecx                         
	addl 1024(%rdx), %ecx            
	shrl $3, %ecx                         
	movl %ecx, (%rdx)                
	cmpl 512(%rdx), %ecx             
	 jb 2f                                 
	cmpl 516(%rdx), %ecx             
	 jb 1f                                 
	lea (%rax, %rsi, 2), %rdx      	# D.13420
	lea (%rdx, %rsi, 2), %rcx      	# D.13420
	movq (%rdi), %mm0                       	# src
	movq (%rdi, %rsi), %mm1                   	# src, D.13420
	movq (%rdi, %rsi, 2), %mm2                	# src, D.13420
	movq (%rdi, %rax), %mm3            	# src
	movq (%rdi, %rsi, 4), %mm4                	# src, D.13420
	movq (%rdi, %rdx), %mm5            	# src
	movq (%rdi, %rax, 2), %mm6         	# src
	movq (%rdi, %rcx), %mm7            	# src
	movq %mm0, (%r9)                       	# tempBlurred
	movq %mm1, (%r9, %rsi)                   	# tempBlurred, D.13420
	movq %mm2, (%r9, %rsi, 2)                	# tempBlurred, D.13420
	movq %mm3, (%r9, %rax)            	# tempBlurred
	movq %mm4, (%r9, %rsi, 4)                	# tempBlurred, D.13420
	movq %mm5, (%r9, %rdx)            	# tempBlurred
	movq %mm6, (%r9, %rax, 2)         	# tempBlurred
	movq %mm7, (%r9, %rcx)            	# tempBlurred
	jmp 4f                                 
	1:                                     
	lea (%rax, %rsi, 2), %rdx      	# D.13420
	lea (%rdx, %rsi, 2), %rcx      	# D.13420
	movq (%rdi), %mm0                       	# src
	pavgb (%r9), %mm0 	# tempBlurred
	movq (%rdi, %rsi), %mm1                   	# src, D.13420
	pavgb (%r9, %rsi), %mm1 	# tempBlurred, D.13420
	movq (%rdi, %rsi, 2), %mm2                	# src, D.13420
	pavgb (%r9, %rsi, 2), %mm2 	# tempBlurred, D.13420
	movq (%rdi, %rax), %mm3            	# src
	pavgb (%r9, %rax), %mm3 	# tempBlurred
	movq (%rdi, %rsi, 4), %mm4                	# src, D.13420
	pavgb (%r9, %rsi, 4), %mm4 	# tempBlurred, D.13420
	movq (%rdi, %rdx), %mm5            	# src
	pavgb (%r9, %rdx), %mm5 	# tempBlurred
	movq (%rdi, %rax, 2), %mm6         	# src
	pavgb (%r9, %rax, 2), %mm6 	# tempBlurred
	movq (%rdi, %rcx), %mm7            	# src
	pavgb (%r9, %rcx), %mm7 	# tempBlurred
	movq %mm0, (%r9)                       	# tempBlurred
	movq %mm1, (%r9, %rsi)                   	# tempBlurred, D.13420
	movq %mm2, (%r9, %rsi, 2)                	# tempBlurred, D.13420
	movq %mm3, (%r9, %rax)            	# tempBlurred
	movq %mm4, (%r9, %rsi, 4)                	# tempBlurred, D.13420
	movq %mm5, (%r9, %rdx)            	# tempBlurred
	movq %mm6, (%r9, %rax, 2)         	# tempBlurred
	movq %mm7, (%r9, %rcx)            	# tempBlurred
	movq %mm0, (%rdi)                       	# src
	movq %mm1, (%rdi, %rsi)                   	# src, D.13420
	movq %mm2, (%rdi, %rsi, 2)                	# src, D.13420
	movq %mm3, (%rdi, %rax)            	# src
	movq %mm4, (%rdi, %rsi, 4)                	# src, D.13420
	movq %mm5, (%rdi, %rdx)            	# src
	movq %mm6, (%rdi, %rax, 2)         	# src
	movq %mm7, (%rdi, %rcx)            	# src
	jmp 4f                                 
	2:                                     
	cmpl 508(%rdx), %ecx             
	 jb 3f                                 
	lea (%rax, %rsi, 2), %rdx      	# D.13420
	lea (%rdx, %rsi, 2), %rcx      	# D.13420
	movq (%rdi), %mm0                       	# src
	movq (%rdi, %rsi), %mm1                   	# src, D.13420
	movq (%rdi, %rsi, 2), %mm2                	# src, D.13420
	movq (%rdi, %rax), %mm3            	# src
	movq (%r9), %mm4                       	# tempBlurred
	movq (%r9, %rsi), %mm5                   	# tempBlurred, D.13420
	movq (%r9, %rsi, 2), %mm6                	# tempBlurred, D.13420
	movq (%r9, %rax), %mm7            	# tempBlurred
	pavgb %mm4, %mm0 
	pavgb %mm5, %mm1 
	pavgb %mm6, %mm2 
	pavgb %mm7, %mm3 
	pavgb %mm4, %mm0 
	pavgb %mm5, %mm1 
	pavgb %mm6, %mm2 
	pavgb %mm7, %mm3 
	movq %mm0, (%r9)                       	# tempBlurred
	movq %mm1, (%r9, %rsi)                   	# tempBlurred, D.13420
	movq %mm2, (%r9, %rsi, 2)                	# tempBlurred, D.13420
	movq %mm3, (%r9, %rax)            	# tempBlurred
	movq %mm0, (%rdi)                       	# src
	movq %mm1, (%rdi, %rsi)                   	# src, D.13420
	movq %mm2, (%rdi, %rsi, 2)                	# src, D.13420
	movq %mm3, (%rdi, %rax)            	# src
	movq (%rdi, %rsi, 4), %mm0                	# src, D.13420
	movq (%rdi, %rdx), %mm1            	# src
	movq (%rdi, %rax, 2), %mm2         	# src
	movq (%rdi, %rcx), %mm3            	# src
	movq (%r9, %rsi, 4), %mm4                	# tempBlurred, D.13420
	movq (%r9, %rdx), %mm5            	# tempBlurred
	movq (%r9, %rax, 2), %mm6         	# tempBlurred
	movq (%r9, %rcx), %mm7            	# tempBlurred
	pavgb %mm4, %mm0 
	pavgb %mm5, %mm1 
	pavgb %mm6, %mm2 
	pavgb %mm7, %mm3 
	pavgb %mm4, %mm0 
	pavgb %mm5, %mm1 
	pavgb %mm6, %mm2 
	pavgb %mm7, %mm3 
	movq %mm0, (%r9, %rsi, 4)                	# tempBlurred, D.13420
	movq %mm1, (%r9, %rdx)            	# tempBlurred
	movq %mm2, (%r9, %rax, 2)         	# tempBlurred
	movq %mm3, (%r9, %rcx)            	# tempBlurred
	movq %mm0, (%rdi, %rsi, 4)                	# src, D.13420
	movq %mm1, (%rdi, %rdx)            	# src
	movq %mm2, (%rdi, %rax, 2)         	# src
	movq %mm3, (%rdi, %rcx)            	# src
	jmp 4f                                 
	3:                                     
	lea (%rax, %rsi, 2), %rdx      	# D.13420
	lea (%rdx, %rsi, 2), %rcx      	# D.13420
	movq (%rdi), %mm0                       	# src
	movq (%rdi, %rsi), %mm1                   	# src, D.13420
	movq (%rdi, %rsi, 2), %mm2                	# src, D.13420
	movq (%rdi, %rax), %mm3            	# src
	movq (%r9), %mm4                       	# tempBlurred
	movq (%r9, %rsi), %mm5                   	# tempBlurred, D.13420
	movq (%r9, %rsi, 2), %mm6                	# tempBlurred, D.13420
	movq (%r9, %rax), %mm7            	# tempBlurred
	pavgb %mm4, %mm0 
	pavgb %mm5, %mm1 
	pavgb %mm6, %mm2 
	pavgb %mm7, %mm3 
	pavgb %mm4, %mm0 
	pavgb %mm5, %mm1 
	pavgb %mm6, %mm2 
	pavgb %mm7, %mm3 
	pavgb %mm4, %mm0 
	pavgb %mm5, %mm1 
	pavgb %mm6, %mm2 
	pavgb %mm7, %mm3 
	movq %mm0, (%r9)                       	# tempBlurred
	movq %mm1, (%r9, %rsi)                   	# tempBlurred, D.13420
	movq %mm2, (%r9, %rsi, 2)                	# tempBlurred, D.13420
	movq %mm3, (%r9, %rax)            	# tempBlurred
	movq %mm0, (%rdi)                       	# src
	movq %mm1, (%rdi, %rsi)                   	# src, D.13420
	movq %mm2, (%rdi, %rsi, 2)                	# src, D.13420
	movq %mm3, (%rdi, %rax)            	# src
	movq (%rdi, %rsi, 4), %mm0                	# src, D.13420
	movq (%rdi, %rdx), %mm1            	# src
	movq (%rdi, %rax, 2), %mm2         	# src
	movq (%rdi, %rcx), %mm3            	# src
	movq (%r9, %rsi, 4), %mm4                	# tempBlurred, D.13420
	movq (%r9, %rdx), %mm5            	# tempBlurred
	movq (%r9, %rax, 2), %mm6         	# tempBlurred
	movq (%r9, %rcx), %mm7            	# tempBlurred
	pavgb %mm4, %mm0 
	pavgb %mm5, %mm1 
	pavgb %mm6, %mm2 
	pavgb %mm7, %mm3 
	pavgb %mm4, %mm0 
	pavgb %mm5, %mm1 
	pavgb %mm6, %mm2 
	pavgb %mm7, %mm3 
	pavgb %mm4, %mm0 
	pavgb %mm5, %mm1 
	pavgb %mm6, %mm2 
	pavgb %mm7, %mm3 
	movq %mm0, (%r9, %rsi, 4)                	# tempBlurred, D.13420
	movq %mm1, (%r9, %rdx)            	# tempBlurred
	movq %mm2, (%r9, %rax, 2)         	# tempBlurred
	movq %mm3, (%r9, %rcx)            	# tempBlurred
	movq %mm0, (%rdi, %rsi, 4)                	# src, D.13420
	movq %mm1, (%rdi, %rdx)            	# src
	movq %mm2, (%rdi, %rax, 2)         	# src
	movq %mm3, (%rdi, %rcx)            	# src
	4:                                     
	
# 0 "" 2
#NO_APP
	ret
	.cfi_endproc
.LFE171:
	.size	tempNoiseReducer_SSE2, .-tempNoiseReducer_SSE2
	.section	.rodata.str1.1,"aMS",@progbits,1
.LC0:
	.string	"postproc"
	.text
	.type	context_to_name, @function
context_to_name:
.LFB181:
	.cfi_startproc
	movl	$.LC0, %eax	#,
	ret
	.cfi_endproc
.LFE181:
	.size	context_to_name, .-context_to_name
	.type	linecpy, @function
linecpy:
.LFB69:
	.cfi_startproc
	subq	$8, %rsp	#,
	.cfi_def_cfa_offset 16
	testl	%ecx, %ecx	# stride
	jle	.L147	#,
	imull	%ecx, %edx	# stride, D.13431
	movslq	%edx, %rdx	# D.13431, D.13432
	call	memcpy	#
	jmp	.L146	#
.L147:
	leal	-1(%rdx), %r8d	#, D.13431
	imull	%ecx, %r8d	# stride, D.13431
	movslq	%r8d, %rax	# D.13431, D.13433
	addq	%rax, %rdi	# D.13433, D.13435
	negl	%edx	# D.13431
	imull	%ecx, %edx	# stride, D.13431
	movslq	%edx, %rdx	# D.13431, D.13432
	addq	%rax, %rsi	# D.13433, D.13434
	call	memcpy	#
.L146:
	addq	$8, %rsp	#,
	.cfi_def_cfa_offset 8
	ret
	.cfi_endproc
.LFE69:
	.size	linecpy, .-linecpy
	.type	reallocAlign, @function
reallocAlign:
.LFB179:
	.cfi_startproc
	pushq	%rbp	#
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	pushq	%rbx	#
	.cfi_def_cfa_offset 24
	.cfi_offset 3, -24
	subq	$8, %rsp	#,
	.cfi_def_cfa_offset 32
	movq	%rdi, %rbp	# p, p
	movl	%esi, %ebx	# size, size
	movq	(%rdi), %rdi	# *p_2(D),
	call	av_free	#
	movslq	%ebx, %rdi	# size, D.13446
	call	av_mallocz	#
	movq	%rax, 0(%rbp)	# tmp89, *p_2(D)
	addq	$8, %rsp	#,
	.cfi_def_cfa_offset 24
	popq	%rbx	#
	.cfi_def_cfa_offset 16
	popq	%rbp	#
	.cfi_def_cfa_offset 8
	ret
	.cfi_endproc
.LFE179:
	.size	reallocAlign, .-reallocAlign
	.type	reallocBuffers, @function
reallocBuffers:
.LFB180:
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
	movq	%rdi, %rbx	# c, c
	movl	%esi, %r14d	# width, width
	movl	%edx, %ebp	# height, height
	movl	%ecx, %r12d	# stride, stride
	movl	%r8d, 12(%rsp)	# qpStride, %sfp
	leal	15(%rsi), %eax	#, D.13467
	sarl	$4, %eax	#, mbWidth
	movl	%eax, 8(%rsp)	# mbWidth, %sfp
	leal	15(%rdx), %eax	#, D.13467
	sarl	$4, %eax	#, mbHeight
	movl	%eax, 4(%rsp)	# mbHeight, %sfp
	movl	%ecx, 1196(%rbx)	# stride, c_12(D)->stride
	movl	%r8d, 1192(%rbx)	# qpStride, c_12(D)->qpStride
	leal	(%rcx,%rcx,2), %r13d	#, tmp136
	sall	$3, %r13d	#, tmp137
	leal	32(%r13), %esi	#, D.13467
	leaq	88(%rdi), %rdi	#, D.13466
	call	reallocAlign	#
	leaq	96(%rbx), %rdi	#, D.13466
	movl	%r13d, %esi	# tmp137,
	call	reallocAlign	#
	leaq	8(%rbx), %rdi	#, D.13466
	movl	$256, %esi	#,
	call	reallocAlign	#
	leaq	16(%rbx), %rdi	#, D.13468
	movl	$2048, %esi	#,
	call	reallocAlign	#
	movl	%r14d, %edx	# width, D.13467
	imull	%ebp, %edx	# height, D.13467
	leal	63(%rdx), %eax	#, tmp146
	testl	%edx, %edx	# D.13467
	cmovns	%edx, %eax	# tmp146,, D.13467, D.13467
	sarl	$6, %eax	#, D.13467
	movl	%eax, %ecx	# D.13467, tmp149
	sall	$4, %ecx	#, tmp149
	subl	%eax, %ecx	# D.13467, D.13467
	leal	255(%rcx), %eax	#, tmp153
	testl	%ecx, %ecx	# D.13467
	cmovs	%eax, %ecx	# tmp153,, D.13467
	sarl	$8, %ecx	#, D.13467
	movslq	%ecx, %rcx	# D.13467, D.13470
	movl	$0, %eax	#, ivtmp.537
.L153:
	movq	16(%rbx), %rdx	# c_12(D)->yHistogram, c_12(D)->yHistogram
	movq	%rcx, (%rdx,%rax)	# D.13470, *_30
	addq	$8, %rax	#, ivtmp.537
	cmpq	$2048, %rax	#, ivtmp.537
	jne	.L153	#,
	movl	%r12d, %r13d	# stride, D.13467
	imull	4(%rsp), %r13d	# %sfp, D.13467
	addl	$1088, %r13d	#, D.13467
	sall	$4, %r13d	#, D.13467
	leal	7(%rbp), %r12d	#, D.13467
	andl	$-8, %r12d	#, D.13467
	sall	$8, %r12d	#, D.13467
	sarl	%r12d	# D.13467
	addl	$17408, %r12d	#, D.13467
	leaq	40(%rbx), %rbp	#, ivtmp.527
	leaq	64(%rbx), %r15	#, D.13465
.L154:
	movl	%r13d, %esi	# D.13467,
	movq	%rbp, %rdi	# ivtmp.527,
	call	reallocAlign	#
	leaq	24(%rbp), %rdi	#, D.13465
	movl	%r12d, %esi	# D.13467,
	call	reallocAlign	#
	addq	$8, %rbp	#, ivtmp.527
	cmpq	%r15, %rbp	# D.13465, ivtmp.527
	jne	.L154	#,
	leal	32(%r14,%r14), %esi	#, D.13467
	leaq	104(%rbx), %rdi	#, D.13466
	call	reallocAlign	#
	movl	12(%rsp), %ebp	# %sfp, D.13467
	imull	4(%rsp), %ebp	# %sfp, D.13467
	leaq	1160(%rbx), %rdi	#, D.13471
	movl	%ebp, %esi	# D.13467,
	call	reallocAlign	#
	leaq	1152(%rbx), %rdi	#, D.13471
	movl	%ebp, %esi	# D.13467,
	call	reallocAlign	#
	leaq	1168(%rbx), %rdi	#, D.13471
	movl	8(%rsp), %esi	# %sfp,
	call	reallocAlign	#
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
.LFE180:
	.size	reallocBuffers, .-reallocBuffers
	.section	.rodata.str1.8,"aMS",@progbits,1
	.align 8
.LC1:
	.string	"Visualization is currently only supported with the accurate deblock filter without SIMD\n"
	.text
	.type	postProcess_C, @function
postProcess_C:
.LFB103:
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
	subq	$2088, %rsp	#,
	.cfi_def_cfa_offset 2144
	movq	%rdi, 584(%rsp)	# src, %sfp
	movl	%esi, 240(%rsp)	# srcStride, %sfp
	movq	%rdx, 528(%rsp)	# dst, %sfp
	movl	%ecx, 56(%rsp)	# dstStride, %sfp
	movl	%r8d, 224(%rsp)	# width, %sfp
	movl	%r9d, 228(%rsp)	# height, %sfp
	leaq	816(%rsp), %rdi	#, tmp3572
	movl	$157, %ecx	#, tmp3574
	movq	2168(%rsp), %rsi	# c2, c2
	rep movsq
	movq	2168(%rsp), %rax	# c2, tmp5728
	movq	16(%rax), %rax	# MEM[(struct PPContext *)c2_51(D) + 16B], c$yHistogram
	movq	%rax, 424(%rsp)	# c$yHistogram, %sfp
	movq	2168(%rsp), %rax	# c2, tmp5730
	movq	88(%rax), %rax	# MEM[(struct PPContext *)c2_51(D) + 88B], c$tempDst
	movq	%rax, 616(%rsp)	# c$tempDst, %sfp
	movq	2168(%rsp), %rax	# c2, tmp5732
	movq	96(%rax), %rax	# MEM[(struct PPContext *)c2_51(D) + 96B], srcBlock
	movq	%rax, 592(%rsp)	# srcBlock, %sfp
	movq	2168(%rsp), %rax	# c2, tmp5734
	movq	104(%rax), %rax	# MEM[(struct PPContext *)c2_51(D) + 104B], c$deintTemp
	movq	%rax, 472(%rsp)	# c$deintTemp, %sfp
	movq	2168(%rsp), %rax	# c2, tmp5736
	movq	1160(%rax), %rax	# MEM[(struct PPContext *)c2_51(D) + 1160B], c$nonBQPTable
	movq	%rax, 600(%rsp)	# c$nonBQPTable, %sfp
	movq	2168(%rsp), %rax	# c2, tmp5738
	movl	1176(%rax), %eax	# MEM[(struct PPContext *)c2_51(D) + 1176B], c$QP
	movl	%eax, 36(%rsp)	# c$QP, %sfp
	movq	2168(%rsp), %rax	# c2, tmp5740
	movl	1180(%rax), %eax	# MEM[(struct PPContext *)c2_51(D) + 1180B], c$nonBQP
	movl	%eax, 176(%rsp)	# c$nonBQP, %sfp
	movq	2168(%rsp), %rax	# c2, tmp5742
	movl	1184(%rax), %eax	# MEM[(struct PPContext *)c2_51(D) + 1184B], c$frameNum
	movl	%eax, 576(%rsp)	# c$frameNum, %sfp
	movq	2168(%rsp), %rax	# c2, tmp5744
	movl	1200(%rax), %eax	# MEM[(struct PPContext *)c2_51(D) + 1200B], c$hChromaSubSample
	movl	%eax, 720(%rsp)	# c$hChromaSubSample, %sfp
	movq	2168(%rsp), %rax	# c2, tmp5746
	movl	1204(%rax), %eax	# MEM[(struct PPContext *)c2_51(D) + 1204B], c$vChromaSubSample
	movl	%eax, 708(%rsp)	# c$vChromaSubSample, %sfp
	movq	2168(%rsp), %rax	# c2, tmp5748
	movl	1208(%rax), %eax	# MEM[(struct PPContext *)c2_51(D) + 1208B], c$ppMode$lumMode
	movl	%eax, 696(%rsp)	# c$ppMode$lumMode, %sfp
	movq	2168(%rsp), %rax	# c2, tmp5750
	movl	1212(%rax), %eax	# MEM[(struct PPContext *)c2_51(D) + 1212B], c$ppMode$chromMode
	movl	%eax, 704(%rsp)	# c$ppMode$chromMode, %sfp
	movq	2168(%rsp), %rax	# c2, tmp5752
	movl	1220(%rax), %eax	# MEM[(struct PPContext *)c2_51(D) + 1220B], c$ppMode$minAllowedY
	movl	%eax, 700(%rsp)	# c$ppMode$minAllowedY, %sfp
	movq	2168(%rsp), %rax	# c2, tmp5754
	movl	1224(%rax), %eax	# MEM[(struct PPContext *)c2_51(D) + 1224B], c$ppMode$maxAllowedY
	movl	%eax, 716(%rsp)	# c$ppMode$maxAllowedY, %sfp
	movq	2168(%rsp), %rax	# c2, tmp5756
	movss	1228(%rax), %xmm2	# MEM[(struct PPContext *)c2_51(D) + 1228B], c$ppMode$maxClippedThreshold
	movss	%xmm2, 712(%rsp)	# c$ppMode$maxClippedThreshold, %sfp
	movl	1232(%rax), %eax	# MEM[(struct PPContext *)c2_51(D) + 1232B], c$1232
	movl	%eax, 460(%rsp)	# c$1232, %sfp
	movq	2168(%rsp), %rax	# c2, tmp5760
	movl	1236(%rax), %eax	# MEM[(struct PPContext *)c2_51(D) + 1236B], c$1236
	movl	%eax, 420(%rsp)	# c$1236, %sfp
	movq	2168(%rsp), %rax	# c2, tmp5762
	movl	1240(%rax), %eax	# MEM[(struct PPContext *)c2_51(D) + 1240B], c$1240
	movl	%eax, 456(%rsp)	# c$1240, %sfp
	movq	2168(%rsp), %rax	# c2, tmp5764
	movl	1244(%rax), %eax	# MEM[(struct PPContext *)c2_51(D) + 1244B], c$ppMode$baseDcDiff
	movl	%eax, 408(%rsp)	# c$ppMode$baseDcDiff, %sfp
	movq	2168(%rsp), %rax	# c2, tmp5766
	movl	1248(%rax), %eax	# MEM[(struct PPContext *)c2_51(D) + 1248B], c$ppMode$flatnessThreshold
	movl	%eax, 60(%rsp)	# c$ppMode$flatnessThreshold, %sfp
	cmpl	$0, 2160(%rsp)	#, isColor
	je	.L393	#,
	movl	$4, %eax	#, tmp3575
	movl	%eax, %ebx	# tmp3575, D.14562
	subl	720(%rsp), %ebx	# %sfp, D.14562
	movl	%ebx, 480(%rsp)	# D.14562, %sfp
	subl	708(%rsp), %eax	# %sfp, D.14562
	movl	%eax, 612(%rsp)	# D.14562, %sfp
	movl	704(%rsp), %eax	# %sfp, c$ppMode$chromMode
	movl	%eax, 32(%rsp)	# c$ppMode$chromMode, %sfp
	jmp	.L159	#
.L393:
	movl	696(%rsp), %eax	# %sfp, c$ppMode$lumMode
	movl	%eax, 32(%rsp)	# c$ppMode$lumMode, %sfp
	movl	$4, 480(%rsp)	#, %sfp
	movl	$4, 612(%rsp)	#, %sfp
.L159:
	movq	592(%rsp), %rax	# %sfp, srcBlock
	movq	%rax, 536(%rsp)	# srcBlock, %sfp
	cmpl	$0, 240(%rsp)	#, %sfp
	jg	.L160	#,
	imull	$23, 240(%rsp), %eax	#, %sfp, D.14562
	cltq
	movq	592(%rsp), %rbx	# %sfp, srcBlock
	subq	%rax, %rbx	# D.14566, srcBlock
	movq	%rbx, 536(%rsp)	# srcBlock, %sfp
.L160:
	cmpl	$0, 56(%rsp)	#, %sfp
	jle	.L161	#,
	movq	616(%rsp), %rax	# %sfp, c$tempDst
	addq	$32, %rax	#, D.14565
	movq	%rax, 504(%rsp)	# D.14565, %sfp
	jmp	.L162	#
.L161:
	imull	$23, 56(%rsp), %eax	#, %sfp, D.14562
	cltq
	movq	616(%rsp), %rbx	# %sfp, D.14566
	subq	%rax, %rbx	# D.14566, D.14566
	leaq	32(%rbx), %rax	#, D.14565
	movq	%rax, 504(%rsp)	# D.14565, %sfp
.L162:
	movl	32(%rsp), %eax	# %sfp, D.14562
	andl	$33554432, %eax	#, D.14562
	movl	%eax, 244(%rsp)	# D.14562, %sfp
	je	.L163	#,
	testl	$17408, 32(%rsp)	#, %sfp
	jne	.L163	#,
	movl	$.LC1, %edx	#,
	movl	$24, %esi	#,
	movq	2168(%rsp), %rdi	# c2,
	movl	$0, %eax	#,
	call	av_log	#
.L163:
	movl	$16, 512(%rsp)	#, %sfp
	movl	32(%rsp), %eax	# %sfp, D.14562
	andl	$262144, %eax	#, D.14562
	movl	%eax, 484(%rsp)	# D.14562, %sfp
	jne	.L164	#,
	movl	$14, 512(%rsp)	#, %sfp
	testl	$12713984, 32(%rsp)	#, %sfp
	jne	.L164	#,
	movl	$13, 512(%rsp)	#, %sfp
	testl	$590849, 32(%rsp)	#, %sfp
	jne	.L164	#,
	movl	$11, 512(%rsp)	#, %sfp
	testl	$512, 32(%rsp)	#, %sfp
	jne	.L164	#,
	movl	32(%rsp), %eax	# %sfp, D.14562
	andl	$4, %eax	#, D.14562
	cmpl	$1, %eax	#, D.14562
	sbbl	%eax, %eax	# copyAhead
	addl	$9, %eax	#, copyAhead
	movl	%eax, 512(%rsp)	# copyAhead, %sfp
.L164:
	movl	512(%rsp), %eax	# %sfp, copyAhead
	subl	$8, %eax	#, copyAhead
	movl	%eax, 544(%rsp)	# copyAhead, %sfp
	cmpl	$0, 2160(%rsp)	#, isColor
	jne	.L400	#,
	addl	$1, 576(%rsp)	#, %sfp
	movl	576(%rsp), %eax	# %sfp, c$frameNum
	cmpl	$1, %eax	#, c$frameNum
	jne	.L166	#,
	movslq	224(%rsp), %rdx	# %sfp, D.14567
	movslq	228(%rsp), %rax	# %sfp, D.14567
	imulq	%rdx, %rax	# D.14567, D.14567
	shrq	$6, %rax	#, D.14567
	movq	%rax, %rdx	# D.14567, tmp3593
	salq	$4, %rdx	#, tmp3593
	subq	%rax, %rdx	# D.14567, D.14567
	movq	%rdx, %rax	# D.14567, D.14567
	shrq	$8, %rax	#, tmp3595
	movq	424(%rsp), %rbx	# %sfp, c$yHistogram
	movq	%rax, (%rbx)	# tmp3595, *c$yHistogram_141
.L166:
	movq	424(%rsp), %rax	# %sfp, c$yHistogram
	movq	%rax, %rdi	# c$yHistogram, ivtmp.1363
	leaq	2048(%rax), %rcx	#, D.14572
	movl	$0, %edx	#, clipped
.L167:
	addq	(%rax), %rdx	# MEM[base: _4113, offset: 0B], clipped
	addq	$8, %rax	#, ivtmp.1379
	cmpq	%rcx, %rax	# D.14572, ivtmp.1379
	jne	.L167	#,
	movq	%rdx, %rcx	# clipped, clipped
	testq	%rdx, %rdx	# clipped
	js	.L168	#,
	pxor	%xmm0, %xmm0	# D.14568
	cvtsi2ssq	%rdx, %xmm0	# clipped, D.14568
	jmp	.L169	#
.L168:
	movq	%rdx, %rax	# clipped, tmp3601
	shrq	%rax	# tmp3601
	movq	%rdx, %rsi	# clipped, tmp3602
	andl	$1, %esi	#, tmp3602
	orq	%rsi, %rax	# tmp3602, tmp3601
	pxor	%xmm0, %xmm0	# tmp3600
	cvtsi2ssq	%rax, %xmm0	# tmp3601, tmp3600
	addss	%xmm0, %xmm0	# tmp3600, D.14568
.L169:
	mulss	712(%rsp), %xmm0	# %sfp, D.14568
	ucomiss	.LC2(%rip), %xmm0	#, D.14568
	jnb	.L170	#,
	cvttss2siq	%xmm0, %rax	# D.14568, maxClipped
	jmp	.L171	#
.L170:
	subss	.LC2(%rip), %xmm0	#, tmp3604
	cvttss2siq	%xmm0, %rax	# tmp3604, maxClipped
	movabsq	$-9223372036854775808, %rsi	#, tmp3606
	xorq	%rsi, %rax	# tmp3606, maxClipped
.L171:
	cmpq	%rcx, %rax	# clipped, maxClipped
	ja	.L401	#,
	movq	424(%rsp), %rbx	# %sfp, c$yHistogram
	leaq	2040(%rbx), %r8	#, ivtmp.1370
	movq	%rdx, %rsi	# clipped, clipped
	movl	$255, %ecx	#, black
.L173:
	subq	(%r8), %rsi	# MEM[base: _4112, offset: 0B], clipped
	subl	$1, %ecx	#, black
	subq	$8, %r8	#, ivtmp.1370
	cmpq	%rsi, %rax	# clipped, maxClipped
	ja	.L414	#,
	testl	%ecx, %ecx	# black
	jg	.L173	#,
.L414:
	movl	$0, %esi	#, white
.L175:
	subq	(%rdi), %rdx	# MEM[base: _4110, offset: 0B], clipped
	addl	$1, %esi	#, white
	addq	$8, %rdi	#, ivtmp.1363
	cmpq	%rdx, %rax	# clipped, maxClipped
	ja	.L172	#,
	cmpl	$255, %esi	#, white
	jle	.L175	#,
	jmp	.L172	#
.L401:
	movl	$255, %ecx	#, black
	movl	$0, %esi	#, white
.L172:
	movl	716(%rsp), %eax	# %sfp, D.14562
	movl	700(%rsp), %ebx	# %sfp, c$ppMode$minAllowedY
	subl	%ebx, %eax	# c$ppMode$minAllowedY, D.14562
	pxor	%xmm1, %xmm1	# D.14569
	cvtsi2sd	%eax, %xmm1	# D.14562, D.14569
	subl	%ecx, %esi	# black, D.14562
	pxor	%xmm0, %xmm0	# D.14569
	cvtsi2sd	%esi, %xmm0	# D.14562, D.14569
	divsd	%xmm0, %xmm1	# D.14569, scale
	movapd	%xmm1, %xmm0	# scale, D.14569
	mulsd	.LC3(%rip), %xmm0	#, D.14569
	addsd	.LC4(%rip), %xmm0	#, D.14569
	cvttsd2si	%xmm0, %eax	# D.14569, D.14570
	movzwl	%ax, %edx	# D.14570, c$packedYScale
	movl	%ecx, %eax	# black, D.14562
	subl	%ebx, %eax	# c$ppMode$minAllowedY, D.14562
	movzwl	%ax, %eax	# D.14562, c$packedYOffset
	movq	%rax, %rcx	# c$packedYOffset, D.14567
	salq	$32, %rcx	#, D.14567
	orq	%rcx, %rax	# D.14567, c$packedYOffset
	movq	%rax, %rcx	# c$packedYOffset, D.14567
	salq	$16, %rcx	#, D.14567
	orq	%rax, %rcx	# c$packedYOffset, c$packedYOffset
	movq	%rcx, 648(%rsp)	# c$packedYOffset, %sfp
	movq	%rdx, %rax	# c$packedYScale, D.14567
	salq	$32, %rax	#, D.14567
	orq	%rdx, %rax	# c$packedYScale, c$packedYScale
	movq	%rax, %rdx	# c$packedYScale, D.14567
	salq	$16, %rdx	#, D.14567
	orq	%rax, %rdx	# c$packedYScale, c$packedYScale
	movq	%rdx, 656(%rsp)	# c$packedYScale, %sfp
	movl	$65536, 452(%rsp)	#, %sfp
	testb	$8, 32(%rsp)	#, %sfp
	je	.L165	#,
	movsd	.LC5(%rip), %xmm0	#, tmp3629
	mulsd	%xmm0, %xmm1	# tmp3629, D.14569
	mulsd	%xmm1, %xmm0	# D.14569, D.14569
	addsd	.LC4(%rip), %xmm0	#, D.14569
	cvttsd2si	%xmm0, %eax	# D.14569, QPCorrecture
	movl	%eax, 452(%rsp)	# QPCorrecture, %sfp
	jmp	.L165	#
.L400:
	movabsq	$72058693566333184, %rax	#, c$packedYScale
	movq	%rax, 656(%rsp)	# c$packedYScale, %sfp
	movq	$0, 648(%rsp)	#, %sfp
	movl	$65536, 452(%rsp)	#, %sfp
.L165:
	movl	240(%rsp), %r11d	# %sfp, srcStride
	movl	%r11d, %esi	# srcStride, D.14562
	negl	%esi	# D.14562
	sall	$3, %esi	#, tmp3636
	movslq	%esi, %rsi	# tmp3636, D.14566
	movl	56(%rsp), %eax	# %sfp, dstStride
	movslq	%eax, %rbx	# dstStride, D.14566
	movq	504(%rsp), %rdi	# %sfp, D.14565
	addq	%rbx, %rdi	# D.14566, dstBlock
	movq	%rdi, %r14	# dstBlock, dstBlock
	movq	%rdi, 520(%rsp)	# dstBlock, %sfp
	cmpl	$0, 224(%rsp)	#, %sfp
	jle	.L177	#,
	leal	0(,%rax,8), %edx	#, D.14562
	movslq	%edx, %rdi	# D.14562, D.14566
	movq	%rdi, 16(%rsp)	# D.14566, %sfp
	leal	0(,%rax,4), %ecx	#, D.14562
	movslq	%ecx, %rdi	# D.14562, D.14566
	movq	%rdi, %r10	# D.14566, D.14566
	movq	%rdi, 80(%rsp)	# D.14566, %sfp
	movl	%eax, %edi	# dstStride, dstStride
	addl	%eax, %eax	# D.14562
	movslq	%eax, %r12	# D.14562, D.14566
	addl	%edi, %eax	# dstStride, D.14562
	movslq	%eax, %r15	# D.14562, D.14566
	movq	%r15, 88(%rsp)	# D.14566, %sfp
	addl	%eax, %eax	# tmp3646
	cltq
	movq	%rax, 96(%rsp)	# D.14566, %sfp
	movl	%edi, %eax	# dstStride, dstStride
	addl	%edi, %ecx	# dstStride, D.14562
	movslq	%ecx, %rdi	# D.14562, D.14566
	movq	%rdi, 104(%rsp)	# D.14566, %sfp
	subl	%eax, %edx	# dstStride, D.14562
	movslq	%edx, %rax	# D.14562, D.14566
	movq	%rax, 112(%rsp)	# D.14566, %sfp
	leal	0(,%r11,8), %r15d	#, D.14562
	movslq	%r15d, %r15	# D.14562, D.14566
	addq	%rsi, %r15	# D.14566, D.14566
	addq	584(%rsp), %r15	# %sfp, ivtmp.1355
	movq	%r14, %rdi	# dstBlock, dstBlock
	addq	%r10, %r14	# D.14566, ivtmp.1357
	movq	%r14, 24(%rsp)	# ivtmp.1357, %sfp
	movq	%rdi, %r13	# dstBlock, dstBlock
	movl	32(%rsp), %esi	# %sfp, D.14562
	movl	%esi, %eax	# D.14562, D.14562
	andl	$8, %eax	#, D.14562
	movl	%eax, 48(%rsp)	# D.14562, %sfp
	movl	%esi, %eax	# D.14562, D.14562
	andl	$65536, %eax	#, D.14562
	movl	%eax, 64(%rsp)	# D.14562, %sfp
	movl	%esi, %eax	# D.14562, D.14562
	andl	$131072, %eax	#, D.14562
	movl	%eax, 72(%rsp)	# D.14562, %sfp
	movl	%esi, %eax	# D.14562, D.14562
	andl	$524288, %eax	#, D.14562
	movl	%eax, 120(%rsp)	# D.14562, %sfp
.L192:
	movl	%r13d, %r9d	# dstBlock, D.14562
	subl	520(%rsp), %r9d	# %sfp, D.14562
	movq	%r15, %rdi	# ivtmp.1355, D.14571
	movq	16(%rsp), %rax	# %sfp, D.14566
	addq	%r13, %rax	# dstBlock, src
	cmpl	$0, 48(%rsp)	#, %sfp
	je	.L178	#,
	movl	240(%rsp), %ebp	# %sfp, D.14563
	movl	56(%rsp), %r11d	# %sfp, D.14563
	movl	$0, %esi	#, ivtmp.1336
	movl	$0, %ecx	#, ivtmp.1335
	movl	$8, %edx	#, D.14563
.L179:
	movslq	%esi, %r8	# ivtmp.1336, D.14566
	movslq	%ecx, %r10	# ivtmp.1335, D.14566
	movq	(%rdi,%r10), %r10	# MEM[(void *)_1002], MEM[(void *)_1002]
	movq	%r10, (%rax,%r8)	# MEM[(void *)_1002], MEM[(void *)_1005]
	addl	%ebp, %ecx	# D.14563, ivtmp.1335
	addl	%r11d, %esi	# D.14563, ivtmp.1336
	subl	$1, %edx	#, D.14563
	jne	.L179	#,
	jmp	.L180	#
.L178:
	movl	240(%rsp), %ebp	# %sfp, D.14563
	movl	56(%rsp), %r11d	# %sfp, D.14563
	movl	$0, %esi	#, ivtmp.1344
	movl	$0, %ecx	#, ivtmp.1343
	movl	$8, %edx	#, D.14563
.L181:
	movslq	%esi, %r8	# ivtmp.1344, D.14566
	movslq	%ecx, %r10	# ivtmp.1343, D.14566
	movq	(%rdi,%r10), %r10	# MEM[(void *)_1010], MEM[(void *)_1010]
	movq	%r10, (%rax,%r8)	# MEM[(void *)_1010], MEM[(void *)_1013]
	addl	%ebp, %ecx	# D.14563, ivtmp.1343
	addl	%r11d, %esi	# D.14563, ivtmp.1344
	subl	$1, %edx	#, D.14563
	jne	.L181	#,
	jmp	.L180	#
.L182:
	subq	%rbx, %rcx	# D.14566, src
	movq	(%rax), %rsi	# MEM[(void *)src_4105], MEM[(void *)src_4105]
	movq	%rsi, (%rcx)	# MEM[(void *)src_4105], MEM[(void *)src_2242]
	subl	$1, %edx	#, D.14563
	jne	.L182	#,
	cmpl	$0, 64(%rsp)	#, %sfp
	je	.L183	#,
	movq	24(%rsp), %rax	# %sfp, ivtmp.1358
	movq	%rax, %rcx	# ivtmp.1358, src
	leaq	8(%rax), %rsi	#, D.14565
	movq	16(%rsp), %rdi	# %sfp, D.14566
.L184:
	movl	(%rcx), %edx	# MEM[base: src_3540, offset: 0B], a
	movl	(%rcx,%r12), %eax	# MEM[base: src_3540, index: _1028, offset: 0B], b
	movl	%eax, %r8d	# b, D.14562
	orl	%edx, %r8d	# a, D.14562
	xorl	%eax, %edx	# b, D.14562
	andl	$-16843010, %edx	#, D.14567
	shrq	%rdx	# D.14567
	subl	%edx, %r8d	# D.14567, tmp3674
	movl	%r8d, (%rcx,%rbx)	# tmp3674, MEM[base: src_3540, index: _165, offset: 0B]
	movq	80(%rsp), %rdx	# %sfp, D.14566
	movl	(%rcx,%rdx), %edx	# MEM[base: src_3540, index: _1022, offset: 0B], a
	movl	%edx, %r8d	# a, D.14562
	orl	%eax, %r8d	# b, D.14562
	xorl	%edx, %eax	# a, D.14562
	andl	$-16843010, %eax	#, D.14567
	shrq	%rax	# D.14567
	subl	%eax, %r8d	# D.14567, tmp3680
	movq	88(%rsp), %r11	# %sfp, D.14566
	movl	%r8d, (%rcx,%r11)	# tmp3680, MEM[base: src_3540, index: _1046, offset: 0B]
	movq	96(%rsp), %rax	# %sfp, D.14566
	movl	(%rcx,%rax), %eax	# MEM[base: src_3540, index: _1057, offset: 0B], b
	movl	%eax, %r8d	# b, D.14562
	orl	%edx, %r8d	# a, D.14562
	xorl	%eax, %edx	# b, D.14562
	andl	$-16843010, %edx	#, D.14567
	shrq	%rdx	# D.14567
	subl	%edx, %r8d	# D.14567, tmp3686
	movq	104(%rsp), %r11	# %sfp, D.14566
	movl	%r8d, (%rcx,%r11)	# tmp3686, MEM[base: src_3540, index: _1062, offset: 0B]
	movl	(%rcx,%rdi), %r8d	# MEM[base: src_3540, index: _172, offset: 0B], a
	movl	%r8d, %edx	# a, D.14562
	orl	%eax, %edx	# b, D.14562
	xorl	%r8d, %eax	# a, D.14562
	andl	$-16843010, %eax	#, D.14567
	shrq	%rax	# D.14567
	subl	%eax, %edx	# D.14567, tmp3692
	movl	%edx, %eax	# tmp3692, tmp3692
	movq	112(%rsp), %rdx	# %sfp, D.14566
	movl	%eax, (%rcx,%rdx)	# tmp3692, MEM[base: src_3540, index: _1078, offset: 0B]
	addq	$4, %rcx	#, src
	cmpq	%rsi, %rcx	# D.14565, src
	jne	.L184	#,
	jmp	.L185	#
.L183:
	cmpl	$0, 72(%rsp)	#, %sfp
	je	.L186	#,
	movslq	%r9d, %rdx	# D.14562, D.14566
	addq	472(%rsp), %rdx	# %sfp, D.14565
	movl	56(%rsp), %esi	# %sfp,
	movq	%r13, %rdi	# dstBlock,
	call	deInterlaceBlendLinear_C	#
	jmp	.L185	#
.L186:
	cmpl	$0, 120(%rsp)	#, %sfp
	je	.L187	#,
	movq	%r14, %r11	# ivtmp.1357, src
	leaq	8(%r14), %rax	#, D.14565
	movq	%rax, 40(%rsp)	# D.14565, %sfp
	jmp	.L188	#
.L189:
	movzbl	(%r9), %ecx	# MEM[base: _1090, offset: 0B], D.14575
	movzbl	%cl, %esi	# D.14575, a
	movzbl	(%r9,%rbx), %r8d	# MEM[base: _1090, index: _165, offset: 0B], D.14575
	movzbl	%r8b, %eax	# D.14575, b
	movzbl	(%r9,%r12), %edx	# MEM[base: _1090, index: _1028, offset: 0B], D.14575
	movzbl	%dl, %ebp	# D.14575, c
	movl	%esi, %edi	# a, D.14562
	subl	%eax, %edi	# b, D.14562
	sarl	$31, %edi	#, d
	movl	%ebp, %r11d	# c, D.14562
	subl	%esi, %r11d	# a, D.14562
	movl	%r11d, %esi	# D.14562, D.14562
	sarl	$31, %esi	#, f
	subl	%ebp, %eax	# c, D.14562
	sarl	$31, %eax	#, e
	movl	%eax, %ebp	# e, D.14583
	xorl	%edi, %ebp	# d, D.14583
	orl	%ebp, %r8d	# D.14583, D.14583
	xorl	%esi, %edi	# f, D.14583
	orl	%edi, %ecx	# D.14583, D.14583
	andl	%r8d, %ecx	# D.14583, D.14583
	xorl	%esi, %eax	# f, D.14583
	orl	%eax, %edx	# D.14583, D.14583
	andl	%ecx, %edx	# D.14583, tmp3710
	movb	%dl, (%r9,%rbx)	# tmp3710, MEM[base: _1090, index: _165, offset: 0B]
	addq	%r12, %r9	# D.14566, ivtmp.1309
	subl	$1, %r10d	#, D.14563
	jne	.L189	#,
	movq	8(%rsp), %r11	# %sfp, src
	addq	$1, %r11	#, src
	cmpq	40(%rsp), %r11	# %sfp, src
	je	.L185	#,
.L188:
	movq	%r11, %r9	# src, ivtmp.1309
	movl	$4, %r10d	#, D.14563
	movq	%r11, 8(%rsp)	# src, %sfp
	jmp	.L189	#
.L187:
	cmpl	$0, 484(%rsp)	#, %sfp
	je	.L190	#,
	movl	56(%rsp), %esi	# %sfp,
	movq	%r13, %rdi	# dstBlock,
	call	deInterlaceInterpolateCubic_C	#
	jmp	.L185	#
.L190:
	testl	$4194304, 32(%rsp)	#, %sfp
	je	.L191	#,
	movslq	%r9d, %rdx	# D.14562, D.14566
	addq	472(%rsp), %rdx	# %sfp, D.14565
	movl	56(%rsp), %esi	# %sfp,
	movq	%r13, %rdi	# dstBlock,
	call	deInterlaceFF_C	#
	jmp	.L185	#
.L191:
	testl	$8388608, 32(%rsp)	#, %sfp
	je	.L185	#,
	movslq	%r9d, %r9	# D.14562, D.14566
	movslq	224(%rsp), %rcx	# %sfp, D.14566
	addq	%r9, %rcx	# D.14566, D.14566
	movq	472(%rsp), %rax	# %sfp, c$deintTemp
	addq	%rax, %rcx	# c$deintTemp, D.14565
	leaq	(%rax,%r9), %rdx	#, D.14565
	movl	56(%rsp), %esi	# %sfp,
	movq	%r13, %rdi	# dstBlock,
	call	deInterlaceL5_C	#
.L185:
	addq	$8, %r13	#, dstBlock
	addq	$8, %r15	#, ivtmp.1355
	addq	$8, %r14	#, ivtmp.1357
	addq	$8, 24(%rsp)	#, %sfp
	movl	%r13d, %eax	# dstBlock, D.14563
	subl	520(%rsp), %eax	# %sfp, D.14563
	cmpl	%eax, 224(%rsp)	# D.14563, %sfp
	jg	.L192	#,
.L177:
	movl	56(%rsp), %esi	# %sfp, dstStride
	movl	%esi, %eax	# dstStride, tmp3720
	sarl	$31, %eax	#, tmp3720
	xorl	%eax, %esi	# tmp3720, D.14562
	subl	%eax, %esi	# tmp3720, D.14562
	movl	%esi, 668(%rsp)	# D.14562, %sfp
	cmpl	224(%rsp), %esi	# %sfp, D.14562
	je	.L193	#,
	cmpl	$0, 544(%rsp)	#, %sfp
	jg	.L194	#,
	jmp	.L195	#
.L193:
	movl	56(%rsp), %eax	# %sfp, dstStride
	leal	(%rax,%rax,8), %esi	#, D.14562
	movslq	%esi, %rsi	# D.14562, D.14566
	addq	504(%rsp), %rsi	# %sfp, D.14573
	movl	%eax, %ecx	# dstStride,
	movl	544(%rsp), %edx	# %sfp,
	movq	528(%rsp), %rdi	# %sfp,
	call	linecpy	#
.L195:
	cmpl	$0, 228(%rsp)	#, %sfp
	jg	.L196	#,
	jmp	.L197	#
.L194:
	movl	56(%rsp), %eax	# %sfp, dstStride
	movl	%eax, 8(%rsp)	# dstStride, %sfp
	movl	512(%rsp), %esi	# %sfp, copyAhead
	leal	-8(%rsi), %r13d	#, D.14562
	leal	(%rax,%rax,8), %r12d	#, D.14563
	movl	$0, %r15d	#, ivtmp.1284
	movl	$0, %ebp	#, i
	movslq	224(%rsp), %r14	# %sfp, D.14567
.L198:
	movslq	%r15d, %rdi	# ivtmp.1284, D.14566
	addq	528(%rsp), %rdi	# %sfp, D.14565
	leal	(%r15,%r12), %esi	#, D.14563
	movslq	%esi, %rsi	# D.14563, D.14566
	addq	504(%rsp), %rsi	# %sfp, D.14565
	movq	%r14, %rdx	# D.14567,
	call	memcpy	#
	addl	$1, %ebp	#, i
	addl	8(%rsp), %r15d	# %sfp, ivtmp.1284
	cmpl	%r13d, %ebp	# D.14562, i
	jne	.L198	#,
	jmp	.L195	#
.L196:
	movl	240(%rsp), %r11d	# %sfp, srcStride
	movl	%r11d, %eax	# srcStride, D.14562
	movl	544(%rsp), %ecx	# %sfp, copyAhead
	imull	%ecx, %eax	# copyAhead, D.14562
	cltq
	movq	%rax, 360(%rsp)	# D.14566, %sfp
	movq	536(%rsp), %rdi	# %sfp, D.14574
	addq	%rax, %rdi	# D.14566, D.14574
	movq	%rdi, 728(%rsp)	# D.14574, %sfp
	movl	228(%rsp), %r10d	# %sfp, height
	leal	-1(%r10), %edx	#, D.14562
	movl	%r11d, %eax	# srcStride, D.14562
	imull	%edx, %eax	# D.14562, D.14562
	cltq
	addq	584(%rsp), %rax	# %sfp, D.14571
	movq	%rax, 736(%rsp)	# D.14571, %sfp
	movl	56(%rsp), %edi	# %sfp, dstStride
	imull	%edi, %edx	# dstStride, D.14562
	movslq	%edx, %rax	# D.14562, D.14566
	addq	528(%rsp), %rax	# %sfp, D.14565
	movq	%rax, 744(%rsp)	# D.14565, %sfp
	leal	(%r11,%r11,2), %eax	#, D.14562
	sall	$2, %eax	#, tmp3747
	cltq
	movq	%rax, 464(%rsp)	# D.14566, %sfp
	movl	%edi, %eax	# dstStride, D.14562
	imull	%ecx, %eax	# copyAhead, D.14562
	cltq
	movq	%rax, 392(%rsp)	# D.14566, %sfp
	movl	%edi, %eax	# dstStride, dstStride
	leal	0(,%rdi,4), %esi	#, D.14562
	movl	%esi, %r14d	# D.14562, D.14562
	movl	%esi, 624(%rsp)	# D.14562, %sfp
	movslq	%esi, %rsi	# D.14562, D.14566
	movq	%rsi, 208(%rsp)	# D.14566, %sfp
	leal	(%rdi,%rdi), %esi	#, D.14562
	movslq	%esi, %rbp	# D.14562, D.14566
	addl	%esi, %eax	# D.14562, tmp3751
	movl	%eax, 632(%rsp)	# tmp3751, %sfp
	movslq	%eax, %rcx	# tmp3751, D.14566
	movq	%rcx, 160(%rsp)	# D.14566, %sfp
	leal	(%rax,%rax), %ecx	#, tmp3755
	movl	%ecx, 636(%rsp)	# tmp3755, %sfp
	movslq	%ecx, %rcx	# tmp3755, D.14566
	movq	%rcx, 64(%rsp)	# D.14566, %sfp
	movl	%r14d, %ecx	# D.14562, tmp3758
	addl	%edi, %ecx	# dstStride, tmp3758
	movl	%ecx, 640(%rsp)	# tmp3758, %sfp
	movslq	%ecx, %r14	# tmp3758, D.14566
	movq	%r14, 96(%rsp)	# D.14566, %sfp
	movl	%edi, %r14d	# dstStride, dstStride
	sall	$3, %edi	#, D.14562
	movslq	%edi, %r15	# D.14562, D.14566
	movq	%r15, 152(%rsp)	# D.14566, %sfp
	movl	%edi, %r8d	# D.14562, tmp3761
	subl	%r14d, %r8d	# dstStride, tmp3761
	movl	%r8d, 516(%rsp)	# tmp3761, %sfp
	movslq	%r8d, %r15	# tmp3761, D.14566
	movq	%r15, 112(%rsp)	# D.14566, %sfp
	addl	%ecx, %ecx	# tmp3765
	movslq	%ecx, %rcx	# tmp3765, D.14566
	movq	%rcx, 672(%rsp)	# D.14566, %sfp
	movl	%r14d, %ecx	# dstStride, dstStride
	addl	%edi, %ecx	# D.14562, D.14562
	movslq	%ecx, %rcx	# D.14562, D.14566
	movq	%rcx, 568(%rsp)	# D.14566, %sfp
	sall	$2, %eax	#, tmp3772
	cltq
	movq	%rax, 680(%rsp)	# D.14566, %sfp
	movl	%r11d, %eax	# srcStride, srcStride
	sall	$3, %eax	#, D.14563
	movl	%eax, 692(%rsp)	# D.14563, %sfp
	movl	%edi, 688(%rsp)	# D.14562, %sfp
	movl	%r10d, %eax	# height, height
	movl	%r10d, 756(%rsp)	# height, %sfp
	addl	$1, %eax	#, ivtmp.1275
	movl	%eax, 628(%rsp)	# ivtmp.1275, %sfp
	movl	%r14d, %eax	# dstStride, D.14562
	negl	%eax	# D.14562
	sall	$3, %eax	#, D.14563
	movl	%eax, 724(%rsp)	# D.14563, %sfp
	leal	(%rdx,%rsi), %eax	#, ivtmp.1276
	movl	%eax, 644(%rsp)	# ivtmp.1276, %sfp
	movl	%esi, 412(%rsp)	# D.14562, %sfp
	movl	%r10d, 548(%rsp)	# height, %sfp
	movl	$0, 580(%rsp)	#, %sfp
	movl	$0, 608(%rsp)	#, %sfp
	movl	$0, 204(%rsp)	#, %sfp
	movl	%r11d, %edx	# srcStride, tmp5247
	sarl	$31, %edx	#, tmp5247
	movl	%r11d, %eax	# srcStride, tmp5248
	xorl	%edx, %eax	# tmp5247, tmp5248
	subl	%edx, %eax	# tmp5247, D.14562
	cltq
	movq	%rax, 760(%rsp)	# D.14567, %sfp
	movq	%rbx, 8(%rsp)	# D.14566, %sfp
	movq	%rbp, 88(%rsp)	# D.14566, %sfp
.L391:
	movslq	608(%rsp), %rax	# %sfp, D.14566
	addq	584(%rsp), %rax	# %sfp, srcBlock
	movq	%rax, 104(%rsp)	# srcBlock, %sfp
	movslq	580(%rsp), %rax	# %sfp, D.14566
	movq	%rax, %rbx	# D.14566, D.14566
	movq	%rax, 488(%rsp)	# D.14566, %sfp
	movq	528(%rsp), %rax	# %sfp, dstBlock
	addq	%rbx, %rax	# D.14566, dstBlock
	movq	%rax, %rsi	# dstBlock, dstBlock
	movq	%rax, 560(%rsp)	# dstBlock, %sfp
	movl	204(%rsp), %ebx	# %sfp, y
	movl	%ebx, %eax	# y, D.14562
	movzbl	612(%rsp), %ecx	# %sfp, tmp5955
	sarl	%cl, %eax	# tmp5955, D.14562
	movl	%eax, %ecx	# D.14562, D.14562
	movl	%eax, %edx	# D.14562, D.14562
	imull	2152(%rsp), %edx	# QPStride, D.14562
	movslq	%edx, %rdx	# D.14562, D.14566
	addq	2144(%rsp), %rdx	# QPs, QPptr
	movq	%rdx, 376(%rsp)	# QPptr, %sfp
	movl	2152(%rsp), %edx	# QPStride, tmp3779
	sarl	$31, %edx	#, tmp3779
	movl	%edx, %eax	# tmp3779, tmp3780
	xorl	2152(%rsp), %eax	# QPStride, tmp3780
	subl	%edx, %eax	# tmp3779, D.14562
	imull	%ecx, %eax	# D.14562, D.14562
	cltq
	addq	600(%rsp), %rax	# %sfp, nonBQPptr
	movq	%rax, 384(%rsp)	# nonBQPptr, %sfp
	movl	%ebx, %eax	# y, y
	movl	%ebx, 372(%rsp)	# y, %sfp
	addl	$15, %eax	#, D.14562
	movl	%eax, 664(%rsp)	# D.14562, %sfp
	cmpl	%eax, 228(%rsp)	# D.14562, %sfp
	jle	.L199	#,
	movq	%rsi, 24(%rsp)	# dstBlock, %sfp
	jmp	.L204	#
.L403:
	movq	520(%rsp), %rax	# %sfp, dstBlock
	movq	%rax, 24(%rsp)	# dstBlock, %sfp
	movq	536(%rsp), %rax	# %sfp, srcBlock
	movq	%rax, 104(%rsp)	# srcBlock, %sfp
.L204:
	cmpl	$0, 224(%rsp)	#, %sfp
	jg	.L200	#,
	movl	$0, 16(%rsp)	#, %sfp
	jmp	.L201	#
.L199:
	movl	756(%rsp), %edx	# %sfp, D.14563
	movl	512(%rsp), %r14d	# %sfp, copyAhead
	subl	%r14d, %edx	# copyAhead, D.14563
	addl	$8, %edx	#, D.14563
	subl	204(%rsp), %edx	# %sfp, D.14563
	movl	$0, %eax	#, tmp3787
	cmovs	%eax, %edx	# D.14563,, tmp3787, D.14563
	movq	104(%rsp), %rsi	# %sfp, D.14573
	addq	360(%rsp), %rsi	# %sfp, D.14573
	movl	240(%rsp), %ebp	# %sfp, srcStride
	movl	%ebp, %ecx	# srcStride,
	movq	728(%rsp), %rdi	# %sfp,
	call	linecpy	#
	movl	548(%rsp), %eax	# %sfp, ivtmp.1273
	cmpl	$8, %eax	#, ivtmp.1273
	movl	$8, %ebx	#, tmp3789
	cmovge	%eax, %ebx	# ivtmp.1273,, i
	cmpl	%ebx, %r14d	# i, copyAhead
	jle	.L202	#,
	movl	%ebp, %r12d	# srcStride, D.14563
	imull	%ebx, %ebp	# i, ivtmp.1264
.L203:
	movslq	%ebp, %rdi	# ivtmp.1264, D.14566
	addq	536(%rsp), %rdi	# %sfp, D.14565
	movq	760(%rsp), %rdx	# %sfp,
	movq	736(%rsp), %rsi	# %sfp,
	call	memcpy	#
	addl	$1, %ebx	#, i
	addl	%r12d, %ebp	# D.14563, ivtmp.1264
	cmpl	%ebx, 512(%rsp)	# i, %sfp
	jg	.L203	#,
.L202:
	movl	628(%rsp), %r14d	# %sfp, ivtmp.1275
	movl	%r14d, %ebx	# ivtmp.1275, i
	movl	512(%rsp), %eax	# %sfp, copyAhead
	subl	$7, %eax	#, D.14562
	cmpl	%eax, %r14d	# D.14562, ivtmp.1275
	cmovle	%r14d, %eax	# ivtmp.1275,, D.14562
	movl	%eax, %edx	# D.14562, D.14562
	movq	560(%rsp), %rsi	# %sfp, D.14565
	subq	8(%rsp), %rsi	# %sfp, D.14565
	movl	56(%rsp), %r13d	# %sfp, dstStride
	movl	%r13d, %ecx	# dstStride,
	movq	504(%rsp), %rdi	# %sfp,
	call	linecpy	#
	cmpl	%r14d, 544(%rsp)	# ivtmp.1275, %sfp
	jl	.L403	#,
	movl	644(%rsp), %ebp	# %sfp, ivtmp.1257
	movslq	668(%rsp), %r12	# %sfp, D.14567
.L205:
	movslq	%ebp, %rdi	# ivtmp.1257, D.14566
	addq	504(%rsp), %rdi	# %sfp, D.14565
	movq	%r12, %rdx	# D.14567,
	movq	744(%rsp), %rsi	# %sfp,
	call	memcpy	#
	addl	$1, %ebx	#, i
	addl	%r13d, %ebp	# D.14563, ivtmp.1257
	cmpl	%ebx, 544(%rsp)	# i, %sfp
	jge	.L205	#,
	movq	520(%rsp), %rax	# %sfp, dstBlock
	movq	%rax, 24(%rsp)	# dstBlock, %sfp
	movq	536(%rsp), %rax	# %sfp, srcBlock
	movq	%rax, 104(%rsp)	# srcBlock, %sfp
	jmp	.L204	#
.L200:
	movl	204(%rsp), %eax	# %sfp, D.14562
	sarl	$3, %eax	#, D.14562
	sall	$8, %eax	#, D.14562
	cltq
	addq	$256, %rax	#, D.14566
	movq	%rax, 496(%rsp)	# D.14566, %sfp
	movq	24(%rsp), %rax	# %sfp, dstBlock
	leaq	8(%rax), %rbx	#, ivtmp.1181
	movq	%rbx, 144(%rsp)	# ivtmp.1181, %sfp
	movq	%rax, %rbx	# dstBlock, dstBlock
	movq	8(%rsp), %r14	# %sfp, D.14566
	addq	%r14, %rax	# D.14566, ivtmp.1193
	movq	%rax, 256(%rsp)	# ivtmp.1193, %sfp
	movq	%rbx, %rax	# dstBlock, ivtmp.1198
	movq	96(%rsp), %r11	# %sfp, D.14566
	addq	%r11, %rax	# D.14566, ivtmp.1198
	movq	%rax, 248(%rsp)	# ivtmp.1198, %sfp
	movq	%rbx, %rax	# dstBlock, ivtmp.1205
	movq	112(%rsp), %r10	# %sfp, D.14566
	addq	%r10, %rax	# D.14566, ivtmp.1205
	movq	%rax, 336(%rsp)	# ivtmp.1205, %sfp
	movq	%rbx, %rax	# dstBlock, ivtmp.1209
	movq	64(%rsp), %r15	# %sfp, D.14566
	addq	%r15, %rax	# D.14566, ivtmp.1209
	movq	%rax, 344(%rsp)	# ivtmp.1209, %sfp
	movq	%rbx, %rax	# dstBlock, D.14565
	movq	88(%rsp), %r9	# %sfp, D.14566
	addq	%r9, %rax	# D.14566, D.14565
	movq	%rax, 264(%rsp)	# D.14565, %sfp
	addq	%r14, %rax	# D.14566, ivtmp.1215
	movq	%rax, 272(%rsp)	# ivtmp.1215, %sfp
	movl	56(%rsp), %edi	# %sfp, dstStride
	movl	%edi, %eax	# dstStride, dstStride
	leal	(%rdi,%rdi), %edx	#, D.14562
	leal	(%rdi,%rdx), %esi	#, D.14562
	movl	%esi, 436(%rsp)	# D.14562, %sfp
	addl	%esi, %eax	# D.14562, D.14562
	movl	%eax, 440(%rsp)	# D.14562, %sfp
	movl	%edi, %esi	# dstStride, D.14562
	addl	%eax, %esi	# D.14562, D.14562
	movl	%esi, %eax	# D.14562, D.14562
	movslq	%esi, %rsi	# D.14562, D.14566
	movq	%rsi, %r8	# D.14566, D.14566
	movq	%rsi, 216(%rsp)	# D.14566, %sfp
	movq	160(%rsp), %rsi	# %sfp, D.14566
	movq	%rsi, %rcx	# D.14566, D.14566
	addq	%r8, %rcx	# D.14566, D.14566
	addq	%rbx, %rcx	# dstBlock, ivtmp.1223
	movq	%rcx, 232(%rsp)	# ivtmp.1223, %sfp
	movslq	%edx, %rcx	# D.14562, D.14566
	movq	%rcx, %r8	# D.14566, D.14566
	movq	%rcx, 168(%rsp)	# D.14566, %sfp
	movq	%rsi, %rcx	# D.14566, D.14566
	movq	%rsi, %rdx	# D.14566, D.14566
	addq	%r8, %rdx	# D.14566, D.14566
	addq	%rbx, %rdx	# dstBlock, ivtmp.1225
	movq	%rdx, 352(%rsp)	# ivtmp.1225, %sfp
	movq	208(%rsp), %rsi	# %sfp, D.14566
	movq	%rsi, %rdx	# D.14566, D.14566
	addq	%r10, %rdx	# D.14566, D.14566
	addq	%rbx, %rdx	# dstBlock, ivtmp.1236
	movq	%rdx, 280(%rsp)	# ivtmp.1236, %sfp
	movq	%rsi, %rdx	# D.14566, D.14566
	addq	%r11, %rdx	# D.14566, D.14566
	addq	%rbx, %rdx	# dstBlock, ivtmp.1237
	movq	%rdx, 288(%rsp)	# ivtmp.1237, %sfp
	movq	%rsi, %rdx	# D.14566, D.14566
	addq	%r15, %rdx	# D.14566, D.14566
	addq	%rbx, %rdx	# dstBlock, ivtmp.1239
	movq	%rdx, 296(%rsp)	# ivtmp.1239, %sfp
	movq	%rsi, %rdx	# D.14566, D.14566
	addq	%rcx, %rsi	# D.14566, D.14566
	movq	%rsi, %rcx	# D.14566, D.14566
	movq	%rsi, 552(%rsp)	# D.14566, %sfp
	movq	%rbx, %rsi	# dstBlock, ivtmp.1241
	addq	%rcx, %rsi	# D.14566, ivtmp.1241
	movq	%rsi, 304(%rsp)	# ivtmp.1241, %sfp
	movq	%rdx, %rsi	# D.14566, D.14566
	leaq	(%rbx,%rdx,2), %rcx	#, ivtmp.1242
	movq	%rcx, 312(%rsp)	# ivtmp.1242, %sfp
	movq	%r14, %rdx	# D.14566, D.14566
	addq	%rsi, %rdx	# D.14566, D.14566
	addq	%rbx, %rdx	# dstBlock, ivtmp.1243
	movq	%rdx, 320(%rsp)	# ivtmp.1243, %sfp
	movq	%rsi, %rdx	# D.14566, D.14566
	addq	%r9, %rdx	# D.14566, D.14566
	addq	%rbx, %rdx	# dstBlock, ivtmp.1244
	movq	%rdx, 328(%rsp)	# ivtmp.1244, %sfp
	movl	$0, 16(%rsp)	#, %sfp
	movl	%eax, %ebx	# D.14562, l6
	movl	%edi, %eax	# dstStride, dstStride
	addl	%edi, %ebx	# dstStride, l6
	movl	%ebx, %esi	# l6, l6
	movl	%ebx, 444(%rsp)	# l6, %sfp
	movl	%edi, %ebx	# dstStride, l7
	addl	%esi, %ebx	# l6, l7
	movl	%ebx, 448(%rsp)	# l7, %sfp
	addl	%ebx, %eax	# l7, l8
	movl	%eax, 752(%rsp)	# l8, %sfp
	cltq
	movq	%rax, 400(%rsp)	# D.14566, %sfp
.L351:
	cmpl	$0, 2160(%rsp)	#, isColor
	je	.L206	#,
	movl	16(%rsp), %eax	# %sfp, D.14562
	movzbl	480(%rsp), %ecx	# %sfp, tmp6064
	sarl	%cl, %eax	# tmp6064, D.14562
	cltq
	movq	376(%rsp), %rbx	# %sfp, QPptr
	movsbl	(%rbx,%rax), %ebx	# *_278, c$QP
	movl	%ebx, 36(%rsp)	# c$QP, %sfp
	movq	384(%rsp), %rbx	# %sfp, nonBQPptr
	movsbl	(%rbx,%rax), %eax	# *_281, c$nonBQP
	movl	%eax, 176(%rsp)	# c$nonBQP, %sfp
	jmp	.L207	#
.L206:
	movl	16(%rsp), %edx	# %sfp, D.14562
	sarl	$4, %edx	#, D.14562
	movslq	%edx, %rdx	# D.14562, D.14566
	movq	376(%rsp), %rax	# %sfp, QPptr
	movsbl	(%rax,%rdx), %eax	# *_287, QP
	movl	452(%rsp), %ebx	# %sfp, QPCorrecture
	imull	%ebx, %eax	# QPCorrecture, D.14562
	addl	$32768, %eax	#, D.14562
	sarl	$16, %eax	#, c$QP
	movl	%eax, 36(%rsp)	# c$QP, %sfp
	movq	384(%rsp), %rax	# %sfp, nonBQPptr
	movsbl	(%rax,%rdx), %eax	# *_293, c$nonBQP
	imull	%ebx, %eax	# QPCorrecture, D.14562
	addl	$32768, %eax	#, D.14562
	sarl	$16, %eax	#, c$nonBQP
	movl	%eax, 176(%rsp)	# c$nonBQP, %sfp
	movq	104(%rsp), %rax	# %sfp, srcBlock
	movq	464(%rsp), %rbx	# %sfp, D.14566
	movzbl	4(%rax,%rbx), %eax	# MEM[base: srcBlock_3500, index: _302, offset: 4B], D.14567
	movq	424(%rsp), %rbx	# %sfp, c$yHistogram
	addq	$1, (%rbx,%rax,8)	#, *_308
	jmp	.L207	#
.L208:
	movslq	%ecx, %r8	# ivtmp.1170, D.14566
	movslq	%edx, %r9	# ivtmp.1169, D.14566
	movq	(%rsi,%r9), %r9	# MEM[(void *)_1136], MEM[(void *)_1136]
	movq	%r9, (%rdi,%r8)	# MEM[(void *)_1136], MEM[(void *)_1139]
	addl	%r11d, %edx	# D.14563, ivtmp.1169
	addl	%r10d, %ecx	# D.14563, ivtmp.1170
	subl	$1, %eax	#, D.14563
	jne	.L208	#,
	jmp	.L209	#
.L210:
	movslq	%ecx, %r8	# ivtmp.1178, D.14566
	movslq	%edx, %r9	# ivtmp.1177, D.14566
	movq	(%rsi,%r9), %r9	# MEM[(void *)_1144], MEM[(void *)_1144]
	movq	%r9, (%rdi,%r8)	# MEM[(void *)_1144], MEM[(void *)_1147]
	addl	%r11d, %edx	# D.14563, ivtmp.1177
	addl	%r10d, %ecx	# D.14563, ivtmp.1178
	subl	$1, %eax	#, D.14563
	jne	.L210	#,
.L209:
	testl	$65536, 32(%rsp)	#, %sfp
	je	.L211	#,
	movq	24(%rsp), %rcx	# %sfp, src
	movq	208(%rsp), %r8	# %sfp, D.14566
	addq	%r8, %rcx	# D.14566, src
	movq	%r8, %rsi	# D.14566, D.14565
	addq	144(%rsp), %rsi	# %sfp, D.14565
	movq	8(%rsp), %rdi	# %sfp, D.14566
	movq	88(%rsp), %r9	# %sfp, D.14566
	movq	160(%rsp), %r10	# %sfp, D.14566
	movq	64(%rsp), %r11	# %sfp, D.14566
	movq	96(%rsp), %rbx	# %sfp, D.14566
	movq	152(%rsp), %rbp	# %sfp, D.14566
	movq	112(%rsp), %r12	# %sfp, D.14566
.L212:
	movl	(%rcx), %edx	# MEM[base: src_3547, offset: 0B], a
	movl	(%rcx,%r9), %eax	# MEM[base: src_3547, index: _1157, offset: 0B], b
	movl	%eax, %r13d	# b, D.14562
	orl	%edx, %r13d	# a, D.14562
	xorl	%eax, %edx	# b, D.14562
	andl	$-16843010, %edx	#, D.14567
	shrq	%rdx	# D.14567
	subl	%edx, %r13d	# D.14567, tmp3848
	movl	%r13d, (%rcx,%rdi)	# tmp3848, MEM[base: src_3547, index: _165, offset: 0B]
	movl	(%rcx,%r8), %edx	# MEM[base: src_3547, index: _1151, offset: 0B], a
	movl	%edx, %r13d	# a, D.14562
	orl	%eax, %r13d	# b, D.14562
	xorl	%edx, %eax	# a, D.14562
	andl	$-16843010, %eax	#, D.14567
	shrq	%rax	# D.14567
	subl	%eax, %r13d	# D.14567, tmp3854
	movl	%r13d, (%rcx,%r10)	# tmp3854, MEM[base: src_3547, index: _1175, offset: 0B]
	movl	(%rcx,%r11), %eax	# MEM[base: src_3547, index: _1186, offset: 0B], b
	movl	%eax, %r13d	# b, D.14562
	orl	%edx, %r13d	# a, D.14562
	xorl	%eax, %edx	# b, D.14562
	andl	$-16843010, %edx	#, D.14567
	shrq	%rdx	# D.14567
	subl	%edx, %r13d	# D.14567, tmp3860
	movl	%r13d, (%rcx,%rbx)	# tmp3860, MEM[base: src_3547, index: _1191, offset: 0B]
	movl	(%rcx,%rbp), %r13d	# MEM[base: src_3547, index: _1202, offset: 0B], a
	movl	%r13d, %edx	# a, D.14562
	orl	%eax, %edx	# b, D.14562
	xorl	%r13d, %eax	# a, D.14562
	andl	$-16843010, %eax	#, D.14567
	shrq	%rax	# D.14567
	subl	%eax, %edx	# D.14567, tmp3866
	movl	%edx, (%rcx,%r12)	# tmp3866, MEM[base: src_3547, index: _1207, offset: 0B]
	addq	$4, %rcx	#, src
	cmpq	%rsi, %rcx	# D.14565, src
	jne	.L212	#,
	jmp	.L213	#
.L211:
	testl	$131072, 32(%rsp)	#, %sfp
	je	.L214	#,
	movslq	16(%rsp), %rsi	# %sfp, D.14566
	addq	472(%rsp), %rsi	# %sfp, tmp
	movq	24(%rsp), %rcx	# %sfp, src
	movq	208(%rsp), %rax	# %sfp, D.14566
	addq	%rax, %rcx	# D.14566, src
	leaq	8(%rsi), %r8	#, D.14565
	movq	8(%rsp), %r9	# %sfp, D.14566
	movq	%rax, %r10	# D.14566, D.14566
	movq	88(%rsp), %r11	# %sfp, D.14566
	movq	160(%rsp), %rbx	# %sfp, D.14566
	movq	64(%rsp), %rbp	# %sfp, D.14566
	movq	96(%rsp), %r12	# %sfp, D.14566
	movq	152(%rsp), %r14	# %sfp, D.14566
	movq	112(%rsp), %r13	# %sfp, D.14566
.L215:
	movl	(%rcx), %edx	# MEM[base: src_3550, offset: 0B], b
	movl	(%rcx,%r9), %eax	# MEM[base: src_3550, index: _165, offset: 0B], c
	movl	%eax, %edi	# c, D.14562
	xorl	(%rsi), %edi	# MEM[base: tmp_3549, offset: 0B], D.14562
	andl	$-16843010, %edi	#, D.14567
	shrq	%rdi	# D.14567
	movq	%rdi, %r15	# D.14567, D.14567
	movl	%eax, %edi	# c, D.14562
	andl	(%rsi), %edi	# MEM[base: tmp_3549, offset: 0B], D.14562
	addl	%r15d, %edi	# D.14567, D.14563
	movl	%edi, %r15d	# D.14563, D.14562
	orl	%edx, %r15d	# b, D.14562
	xorl	%edx, %edi	# b, D.14562
	andl	$-16843010, %edi	#, D.14567
	shrq	%rdi	# D.14567
	subl	%edi, %r15d	# D.14567, tmp3879
	movl	%r15d, (%rcx)	# tmp3879, MEM[base: src_3550, offset: 0B]
	movl	(%rcx,%r11), %edi	# MEM[base: src_3550, index: _1157, offset: 0B], a
	movl	%edi, %r15d	# a, D.14562
	xorl	%edx, %r15d	# b, D.14562
	andl	$-16843010, %r15d	#, D.14567
	shrq	%r15	# D.14567
	andl	%edi, %edx	# a, D.14562
	addl	%r15d, %edx	# D.14567, D.14563
	movl	%edx, %r15d	# D.14563, D.14562
	orl	%eax, %r15d	# c, D.14562
	xorl	%eax, %edx	# c, D.14562
	andl	$-16843010, %edx	#, D.14567
	shrq	%rdx	# D.14567
	subl	%edx, %r15d	# D.14567, tmp3890
	movl	%r15d, (%rcx,%r9)	# tmp3890, MEM[base: src_3550, index: _165, offset: 0B]
	movl	(%rcx,%rbx), %edx	# MEM[base: src_3550, index: _1175, offset: 0B], b
	movl	%edx, %r15d	# b, D.14562
	xorl	%eax, %r15d	# c, D.14562
	andl	$-16843010, %r15d	#, D.14567
	shrq	%r15	# D.14567
	andl	%edx, %eax	# b, D.14562
	addl	%r15d, %eax	# D.14567, D.14563
	movl	%eax, %r15d	# D.14563, D.14562
	orl	%edi, %r15d	# a, D.14562
	xorl	%edi, %eax	# a, D.14562
	andl	$-16843010, %eax	#, D.14567
	shrq	%rax	# D.14567
	subl	%eax, %r15d	# D.14567, tmp3901
	movl	%r15d, (%rcx,%r11)	# tmp3901, MEM[base: src_3550, index: _1157, offset: 0B]
	movl	(%rcx,%r10), %eax	# MEM[base: src_3550, index: _1151, offset: 0B], c
	movl	%eax, %r15d	# c, D.14562
	xorl	%edi, %r15d	# a, D.14562
	andl	$-16843010, %r15d	#, D.14567
	shrq	%r15	# D.14567
	andl	%eax, %edi	# c, D.14562
	addl	%r15d, %edi	# D.14567, D.14563
	movl	%edi, %r15d	# D.14563, D.14562
	orl	%edx, %r15d	# b, D.14562
	xorl	%edx, %edi	# b, D.14562
	andl	$-16843010, %edi	#, D.14567
	shrq	%rdi	# D.14567
	subl	%edi, %r15d	# D.14567, tmp3912
	movl	%r15d, (%rcx,%rbx)	# tmp3912, MEM[base: src_3550, index: _1175, offset: 0B]
	movl	(%rcx,%r12), %edi	# MEM[base: src_3550, index: _1191, offset: 0B], a
	movl	%edi, %r15d	# a, D.14562
	xorl	%edx, %r15d	# b, D.14562
	andl	$-16843010, %r15d	#, D.14567
	shrq	%r15	# D.14567
	andl	%edi, %edx	# a, D.14562
	addl	%r15d, %edx	# D.14567, D.14563
	movl	%edx, %r15d	# D.14563, D.14562
	orl	%eax, %r15d	# c, D.14562
	xorl	%eax, %edx	# c, D.14562
	andl	$-16843010, %edx	#, D.14567
	shrq	%rdx	# D.14567
	subl	%edx, %r15d	# D.14567, tmp3923
	movl	%r15d, (%rcx,%r10)	# tmp3923, MEM[base: src_3550, index: _1151, offset: 0B]
	movl	(%rcx,%rbp), %r15d	# MEM[base: src_3550, index: _1186, offset: 0B], b
	movl	%r15d, %edx	# b, D.14562
	xorl	%eax, %edx	# c, D.14562
	andl	$-16843010, %edx	#, D.14567
	shrq	%rdx	# D.14567
	andl	%r15d, %eax	# b, D.14562
	addl	%edx, %eax	# D.14567, D.14563
	movl	%eax, %edx	# D.14563, D.14562
	orl	%edi, %edx	# a, D.14562
	xorl	%edi, %eax	# a, D.14562
	andl	$-16843010, %eax	#, D.14567
	shrq	%rax	# D.14567
	subl	%eax, %edx	# D.14567, tmp3934
	movl	%edx, (%rcx,%r12)	# tmp3934, MEM[base: src_3550, index: _1191, offset: 0B]
	movl	(%rcx,%r13), %edx	# MEM[base: src_3550, index: _1207, offset: 0B], D.14564
	movl	%edx, %eax	# D.14564, D.14562
	xorl	%edi, %eax	# a, D.14562
	andl	$-16843010, %eax	#, D.14567
	shrq	%rax	# D.14567
	andl	%edx, %edi	# D.14564, D.14562
	addl	%eax, %edi	# D.14567, D.14563
	movl	%edi, %eax	# D.14563, D.14562
	orl	%r15d, %eax	# b, D.14562
	xorl	%r15d, %edi	# b, D.14562
	andl	$-16843010, %edi	#, D.14567
	shrq	%rdi	# D.14567
	subl	%edi, %eax	# D.14567, tmp3945
	movl	%eax, (%rcx,%rbp)	# tmp3945, MEM[base: src_3550, index: _1186, offset: 0B]
	movl	(%rcx,%r14), %eax	# MEM[base: src_3550, index: _1202, offset: 0B], a
	movl	%eax, %edi	# a, D.14562
	xorl	%r15d, %edi	# b, D.14562
	andl	$-16843010, %edi	#, D.14567
	shrq	%rdi	# D.14567
	andl	%eax, %r15d	# a, D.14562
	leal	(%r15,%rdi), %eax	#, D.14563
	movl	%eax, %edi	# D.14563, D.14562
	orl	%edx, %edi	# D.14564, D.14562
	xorl	%edx, %eax	# D.14564, D.14562
	andl	$-16843010, %eax	#, D.14567
	shrq	%rax	# D.14567
	subl	%eax, %edi	# D.14567, tmp3956
	movl	%edi, (%rcx,%r13)	# tmp3956, MEM[base: src_3550, index: _1207, offset: 0B]
	movl	%edx, (%rsi)	# D.14564, MEM[base: tmp_3549, offset: 0B]
	addq	$4, %rcx	#, src
	addq	$4, %rsi	#, tmp
	cmpq	%r8, %rsi	# D.14565, tmp
	jne	.L215	#,
	jmp	.L213	#
.L214:
	testl	$524288, 32(%rsp)	#, %sfp
	je	.L216	#,
	movq	24(%rsp), %r11	# %sfp, src
	movq	208(%rsp), %rax	# %sfp, D.14566
	addq	%rax, %r11	# D.14566, src
	movq	%rax, %rbx	# D.14566, D.14565
	addq	144(%rsp), %rbx	# %sfp, D.14565
	movq	8(%rsp), %rbp	# %sfp, D.14566
	movq	88(%rsp), %r12	# %sfp, D.14566
	jmp	.L217	#
.L218:
	movzbl	(%r9), %ecx	# MEM[base: _1404, offset: 0B], D.14575
	movzbl	%cl, %esi	# D.14575, a
	movzbl	(%r9,%rbp), %r8d	# MEM[base: _1404, index: _165, offset: 0B], D.14575
	movzbl	%r8b, %eax	# D.14575, b
	movzbl	(%r9,%r12), %edx	# MEM[base: _1404, index: _1157, offset: 0B], D.14575
	movzbl	%dl, %r13d	# D.14575, c
	movl	%esi, %edi	# a, D.14562
	subl	%eax, %edi	# b, D.14562
	sarl	$31, %edi	#, d
	movl	%r13d, %r14d	# c, D.14562
	subl	%esi, %r14d	# a, D.14562
	movl	%r14d, %esi	# D.14562, D.14562
	sarl	$31, %esi	#, f
	subl	%r13d, %eax	# c, D.14562
	sarl	$31, %eax	#, e
	movl	%eax, %r13d	# e, D.14583
	xorl	%edi, %r13d	# d, D.14583
	orl	%r13d, %r8d	# D.14583, D.14583
	xorl	%esi, %edi	# f, D.14583
	orl	%edi, %ecx	# D.14583, D.14583
	andl	%r8d, %ecx	# D.14583, D.14583
	xorl	%esi, %eax	# f, D.14583
	orl	%eax, %edx	# D.14583, D.14583
	andl	%ecx, %edx	# D.14583, tmp3971
	movb	%dl, (%r9,%rbp)	# tmp3971, MEM[base: _1404, index: _165, offset: 0B]
	addq	%r12, %r9	# D.14566, ivtmp.1116
	subl	$1, %r10d	#, D.14563
	jne	.L218	#,
	addq	$1, %r11	#, src
	cmpq	%rbx, %r11	# D.14565, src
	je	.L213	#,
.L217:
	movq	%r11, %r9	# src, ivtmp.1116
	movl	$4, %r10d	#, D.14563
	jmp	.L218	#
.L216:
	cmpl	$0, 484(%rsp)	#, %sfp
	je	.L219	#,
	movq	160(%rsp), %rdi	# %sfp, D.14572
	movq	64(%rsp), %rax	# %sfp, D.14566
	addq	%rax, %rdi	# D.14566, D.14572
	movq	24(%rsp), %rbx	# %sfp, dstBlock
	leaq	(%rbx,%rdi), %rsi	#, ivtmp.1137
	addq	144(%rsp), %rdi	# %sfp, D.14572
	movq	%rax, %r8	# D.14566, D.14566
	negq	%r8	# D.14566
	movq	208(%rsp), %r9	# %sfp, D.14566
	movq	88(%rsp), %r11	# %sfp, D.14566
	movq	96(%rsp), %rbp	# %sfp, D.14566
	movq	152(%rsp), %r10	# %sfp, D.14566
	movq	112(%rsp), %r12	# %sfp, D.14566
	movq	672(%rsp), %rbx	# %sfp, D.14566
	movq	568(%rsp), %r13	# %sfp, D.14566
	movq	680(%rsp), %r14	# %sfp, D.14566
.L228:
	movq	%rsi, %rax	# ivtmp.1137, D.14574
	subq	64(%rsp), %rax	# %sfp, D.14574
	movq	%rsi, %rcx	# ivtmp.1137, D.14574
	movzbl	(%rax,%r11), %edx	# MEM[base: _1416, index: _1157, offset: 0B], D.14562
	leal	(%rdx,%rdx,8), %edx	#, D.14562
	movzbl	(%rsi,%r8), %r15d	# MEM[base: _1251, index: _3751, offset: 0B], D.14562
	subl	%r15d, %edx	# D.14562, D.14562
	movzbl	(%rax,%r9), %r15d	# MEM[base: _1416, index: _1151, offset: 0B], D.14562
	leal	(%r15,%r15,8), %r15d	#, D.14562
	addl	%r15d, %edx	# D.14562, D.14562
	movzbl	(%rsi), %r15d	# MEM[base: _1251, offset: 0B], D.14562
	subl	%r15d, %edx	# D.14562, D.14562
	sarl	$4, %edx	#, D.14562
	movl	%edx, %r15d	# D.14562, D.14582
	testl	$-256, %edx	#, D.14562
	je	.L221	#,
	negl	%edx	# D.14562
	sarl	$31, %edx	#, tmp6128
	movl	%edx, %r15d	# tmp6128, D.14582
.L221:
	movq	160(%rsp), %rdx	# %sfp, D.14566
	movb	%r15b, (%rax,%rdx)	# D.14582, MEM[base: _1416, index: _1175, offset: 0B]
	movzbl	(%rax,%r9), %edx	# MEM[base: _1416, index: _1151, offset: 0B], D.14562
	leal	(%rdx,%rdx,8), %edx	#, D.14562
	movzbl	(%rax,%r11), %r15d	# MEM[base: _1416, index: _1157, offset: 0B], D.14562
	subl	%r15d, %edx	# D.14562, D.14562
	movzbl	(%rcx), %r15d	# MEM[base: _1251, offset: 0B], D.14562
	leal	(%r15,%r15,8), %r15d	#, D.14562
	addl	%r15d, %edx	# D.14562, D.14562
	movzbl	(%rax,%r10), %r15d	# MEM[base: _1416, index: _1202, offset: 0B], D.14562
	subl	%r15d, %edx	# D.14562, D.14562
	sarl	$4, %edx	#, D.14562
	movl	%edx, %r15d	# D.14562, D.14582
	testl	$-256, %edx	#, D.14562
	je	.L223	#,
	negl	%edx	# D.14562
	sarl	$31, %edx	#, tmp6130
	movl	%edx, %r15d	# tmp6130, D.14582
.L223:
	movb	%r15b, (%rax,%rbp)	# D.14582, MEM[base: _1416, index: _1191, offset: 0B]
	movzbl	(%rcx), %edx	# MEM[base: _1251, offset: 0B], D.14562
	leal	(%rdx,%rdx,8), %edx	#, D.14562
	movzbl	(%rax,%r9), %r15d	# MEM[base: _1416, index: _1151, offset: 0B], D.14562
	subl	%r15d, %edx	# D.14562, D.14562
	movzbl	(%rax,%r10), %r15d	# MEM[base: _1416, index: _1202, offset: 0B], D.14562
	leal	(%r15,%r15,8), %r15d	#, D.14562
	addl	%r15d, %edx	# D.14562, D.14562
	movzbl	(%rax,%rbx), %r15d	# MEM[base: _1416, index: _1521, offset: 0B], D.14562
	subl	%r15d, %edx	# D.14562, D.14562
	sarl	$4, %edx	#, D.14562
	movl	%edx, %r15d	# D.14562, D.14582
	testl	$-256, %edx	#, D.14562
	je	.L225	#,
	negl	%edx	# D.14562
	sarl	$31, %edx	#, tmp6131
	movl	%edx, %r15d	# tmp6131, D.14582
.L225:
	movb	%r15b, (%rax,%r12)	# D.14582, MEM[base: _1416, index: _1207, offset: 0B]
	movzbl	(%rax,%r10), %edx	# MEM[base: _1416, index: _1202, offset: 0B], D.14562
	leal	(%rdx,%rdx,8), %edx	#, D.14562
	movzbl	(%rcx), %ecx	# MEM[base: _1251, offset: 0B], D.14562
	subl	%ecx, %edx	# D.14562, D.14562
	movzbl	(%rax,%rbx), %ecx	# MEM[base: _1416, index: _1521, offset: 0B], D.14562
	leal	(%rcx,%rcx,8), %ecx	#, D.14562
	addl	%ecx, %edx	# D.14562, D.14562
	movzbl	(%rax,%r14), %ecx	# MEM[base: _1416, index: _1547, offset: 0B], D.14562
	subl	%ecx, %edx	# D.14562, D.14562
	sarl	$4, %edx	#, D.14562
	movl	%edx, %ecx	# D.14562, D.14582
	testl	$-256, %edx	#, D.14562
	je	.L227	#,
	negl	%edx	# D.14562
	sarl	$31, %edx	#, tmp6132
	movl	%edx, %ecx	# tmp6132, D.14582
.L227:
	movb	%cl, (%rax,%r13)	# D.14582, MEM[base: _1416, index: _1534, offset: 0B]
	addq	$1, %rsi	#, ivtmp.1137
	cmpq	%rdi, %rsi	# D.14572, ivtmp.1137
	jne	.L228	#,
	jmp	.L213	#
.L219:
	testl	$4194304, 32(%rsp)	#, %sfp
	je	.L229	#,
	movslq	16(%rsp), %rdi	# %sfp, D.14566
	movq	24(%rsp), %rsi	# %sfp, dstBlock
	movq	%rsi, %r11	# dstBlock, D.14565
	movq	208(%rsp), %rbp	# %sfp, D.14566
	addq	%rbp, %r11	# D.14566, D.14565
	movl	$0, %eax	#, ivtmp.1146
	addq	472(%rsp), %rdi	# %sfp, D.14565
	movq	%rbp, %rbx	# D.14566, D.14572
	addq	152(%rsp), %rbx	# %sfp, D.14572
	addq	%rsi, %rbx	# dstBlock, D.14565
	addq	568(%rsp), %rbp	# %sfp, D.14572
	addq	%rsi, %rbp	# dstBlock, D.14565
	movq	304(%rsp), %rsi	# %sfp, ivtmp.1241
	movq	312(%rsp), %r8	# %sfp, ivtmp.1242
	movq	320(%rsp), %r9	# %sfp, ivtmp.1243
	movq	328(%rsp), %r10	# %sfp, ivtmp.1244
	movq	%r11, 40(%rsp)	# D.14565, %sfp
	movq	280(%rsp), %r11	# %sfp, ivtmp.1236
	movq	%rbx, 48(%rsp)	# D.14565, %sfp
	movq	288(%rsp), %rbx	# %sfp, ivtmp.1237
	movq	%rbp, 72(%rsp)	# D.14565, %sfp
	movq	296(%rsp), %rbp	# %sfp, ivtmp.1239
.L238:
	movq	%r9, %r14	# ivtmp.1243, D.14565
	movzbl	(%r9,%rax), %ecx	# MEM[base: _1559, index: ivtmp.1146_3557, offset: 0B], t2
	movq	%r10, %r13	# ivtmp.1244, D.14565
	movq	%rsi, %r12	# ivtmp.1241, D.14565
	movq	40(%rsp), %rdx	# %sfp, D.14565
	movzbl	(%rdx,%rax), %edx	# MEM[base: _4022, index: ivtmp.1146_3557, offset: 0B], D.14562
	sall	$2, %edx	#, D.14562
	movzbl	(%rdi,%rax), %r15d	# MEM[base: _1875, index: ivtmp.1146_3557, offset: 0B], t1
	subl	%r15d, %edx	# t1, D.14562
	leal	(%rdx,%rcx,2), %edx	#, D.14562
	movzbl	(%r10,%rax), %r15d	# MEM[base: _1535, index: ivtmp.1146_3557, offset: 0B], D.14562
	leal	(%rdx,%r15,4), %edx	#, D.14562
	movzbl	(%rsi,%rax), %r15d	# MEM[base: _1509, index: ivtmp.1146_3557, offset: 0B], D.14562
	subl	%r15d, %edx	# D.14562, D.14562
	addl	$4, %edx	#, D.14562
	sarl	$3, %edx	#, D.14562
	movl	%edx, %r15d	# D.14562, D.14582
	testl	$-256, %edx	#, D.14562
	je	.L231	#,
	negl	%edx	# D.14562
	sarl	$31, %edx	#, tmp6145
	movl	%edx, %r15d	# tmp6145, D.14582
.L231:
	movb	%r15b, (%r14,%rax)	# D.14582, MEM[base: _1559, index: ivtmp.1146_3557, offset: 0B]
	movq	%r8, %r15	# ivtmp.1242, D.14565
	movzbl	(%r8,%rax), %edx	# MEM[base: _3833, index: ivtmp.1146_3557, offset: 0B], t1
	movq	%rbx, %r14	# ivtmp.1237, D.14565
	movzbl	0(%r13,%rax), %r13d	# MEM[base: _1535, index: ivtmp.1146_3557, offset: 0B], D.14562
	sall	$2, %r13d	#, D.14562
	subl	%ecx, %r13d	# t2, D.14562
	leal	0(%r13,%rdx,2), %ecx	#, D.14562
	leal	(%rcx,%rdx,4), %ecx	#, D.14562
	movzbl	(%rbx,%rax), %r13d	# MEM[base: _3837, index: ivtmp.1146_3557, offset: 0B], D.14562
	subl	%r13d, %ecx	# D.14562, D.14562
	addl	$4, %ecx	#, D.14562
	sarl	$3, %ecx	#, D.14562
	movl	%ecx, %r13d	# D.14562, D.14582
	testl	$-256, %ecx	#, D.14562
	je	.L233	#,
	negl	%ecx	# D.14562
	sarl	$31, %ecx	#, tmp6146
	movl	%ecx, %r13d	# tmp6146, D.14582
.L233:
	movb	%r13b, (%r12,%rax)	# D.14582, MEM[base: _1509, index: ivtmp.1146_3557, offset: 0B]
	movq	%rbp, %r13	# ivtmp.1239, D.14565
	movzbl	0(%rbp,%rax), %ecx	# MEM[base: _3825, index: ivtmp.1146_3557, offset: 0B], t2
	movq	%r11, %r12	# ivtmp.1236, D.14565
	movzbl	(%r15,%rax), %r15d	# MEM[base: _3833, index: ivtmp.1146_3557, offset: 0B], D.14562
	sall	$2, %r15d	#, D.14562
	subl	%edx, %r15d	# t1, D.14562
	leal	(%r15,%rcx,2), %edx	#, D.14562
	leal	(%rdx,%rcx,4), %edx	#, D.14562
	movzbl	(%r11,%rax), %r15d	# MEM[base: _3829, index: ivtmp.1146_3557, offset: 0B], D.14562
	subl	%r15d, %edx	# D.14562, D.14562
	addl	$4, %edx	#, D.14562
	sarl	$3, %edx	#, D.14562
	movl	%edx, %r15d	# D.14562, D.14582
	testl	$-256, %edx	#, D.14562
	je	.L235	#,
	negl	%edx	# D.14562
	sarl	$31, %edx	#, tmp6147
	movl	%edx, %r15d	# tmp6147, D.14582
.L235:
	movb	%r15b, (%r14,%rax)	# D.14582, MEM[base: _3837, index: ivtmp.1146_3557, offset: 0B]
	movq	48(%rsp), %rdx	# %sfp, D.14565
	movzbl	(%rdx,%rax), %edx	# MEM[base: _4016, index: ivtmp.1146_3557, offset: 0B], D.14575
	movzbl	%dl, %r14d	# D.14575, t1
	movzbl	0(%r13,%rax), %r13d	# MEM[base: _3825, index: ivtmp.1146_3557, offset: 0B], D.14562
	sall	$2, %r13d	#, D.14562
	subl	%ecx, %r13d	# t2, D.14562
	leal	0(%r13,%r14,2), %ecx	#, D.14562
	leal	(%rcx,%r14,4), %ecx	#, D.14562
	movq	72(%rsp), %r14	# %sfp, D.14565
	movzbl	(%r14,%rax), %r13d	# MEM[base: _4020, index: ivtmp.1146_3557, offset: 0B], D.14562
	subl	%r13d, %ecx	# D.14562, D.14562
	addl	$4, %ecx	#, D.14562
	sarl	$3, %ecx	#, D.14562
	movl	%ecx, %r13d	# D.14562, D.14582
	testl	$-256, %ecx	#, D.14562
	je	.L237	#,
	negl	%ecx	# D.14562
	sarl	$31, %ecx	#, tmp6150
	movl	%ecx, %r13d	# tmp6150, D.14582
.L237:
	movb	%r13b, (%r12,%rax)	# D.14582, MEM[base: _3829, index: ivtmp.1146_3557, offset: 0B]
	movb	%dl, (%rdi,%rax)	# D.14575, MEM[base: _1875, index: ivtmp.1146_3557, offset: 0B]
	addq	$1, %rax	#, ivtmp.1146
	cmpq	$8, %rax	#, ivtmp.1146
	jne	.L238	#,
	jmp	.L213	#
.L229:
	testl	$8388608, 32(%rsp)	#, %sfp
	je	.L213	#,
	movslq	16(%rsp), %rax	# %sfp, D.14566
	movslq	224(%rsp), %rcx	# %sfp, D.14566
	addq	%rax, %rcx	# D.14566, D.14566
	movq	472(%rsp), %rbx	# %sfp, c$deintTemp
	addq	%rbx, %rcx	# c$deintTemp, D.14565
	leaq	(%rbx,%rax), %rdx	#, D.14565
	movl	56(%rsp), %esi	# %sfp,
	movq	24(%rsp), %rdi	# %sfp,
	call	deInterlaceL5_C	#
.L213:
	movl	372(%rsp), %eax	# %sfp, D.14563
	addl	$8, %eax	#, D.14563
	cmpl	%eax, 228(%rsp)	# D.14563, %sfp
	jle	.L239	#,
	testl	$512, 32(%rsp)	#, %sfp
	je	.L240	#,
	movl	36(%rsp), %eax	# %sfp, c$QP
	leal	(%rax,%rax), %r14d	#, D.14562
	movslq	436(%rsp), %rbp	# %sfp, D.14566
	movq	160(%rsp), %rax	# %sfp, D.14566
	leaq	(%rax,%rbp), %rbx	#, D.14572
	movq	24(%rsp), %rax	# %sfp, dstBlock
	leaq	(%rax,%rbx), %rdi	#, ivtmp.959
	addq	144(%rsp), %rbx	# %sfp, D.14572
	movslq	440(%rsp), %r10	# %sfp, D.14566
	movslq	444(%rsp), %r9	# %sfp, D.14566
	movl	$0, %r13d	#, tmp5190
	movslq	448(%rsp), %r12	# %sfp, D.14566
	movl	%r14d, 40(%rsp)	# D.14562, %sfp
	movq	216(%rsp), %r11	# %sfp, D.14566
.L243:
	movq	%rdi, %rcx	# ivtmp.959, D.14574
	subq	%rbp, %rcx	# D.14566, D.14574
	movzbl	(%rcx,%r10), %r15d	# MEM[base: _2691, index: _1695, offset: 0B], D.14562
	movzbl	(%rcx,%r11), %eax	# MEM[base: _2691, index: _3874, offset: 0B], D.14562
	movl	%r15d, %r8d	# D.14562, b
	subl	%eax, %r8d	# D.14562, b
	movl	%r8d, %esi	# b, tmp4103
	sarl	$31, %esi	#, tmp4103
	movl	%esi, %edx	# tmp4103, tmp4104
	xorl	%r8d, %edx	# b, tmp4104
	subl	%esi, %edx	# tmp4103, D.14562
	movzbl	(%rdi), %esi	# MEM[base: _945, offset: 0B], D.14562
	subl	%r15d, %esi	# D.14562, a
	movl	%esi, %r15d	# a, tmp4108
	sarl	$31, %r15d	#, tmp4108
	xorl	%r15d, %esi	# tmp4108, tmp4109
	subl	%r15d, %esi	# tmp4108, D.14562
	movzbl	(%rcx,%r9), %r15d	# MEM[base: _2691, index: _1705, offset: 0B], D.14562
	subl	%r15d, %eax	# D.14562, c
	movl	%eax, %r15d	# c, tmp4113
	sarl	$31, %r15d	#, tmp4113
	xorl	%r15d, %eax	# tmp4113, tmp4114
	subl	%r15d, %eax	# tmp4113, D.14562
	addl	%esi, %eax	# D.14562, D.14562
	sarl	%eax	# D.14562
	subl	%eax, %edx	# D.14562, d
	cmovs	%r13d, %edx	# d,, tmp5190, d
	cmpl	40(%rsp), %edx	# %sfp, d
	jge	.L241	#,
	negl	%r8d	# D.14562
	testl	%r8d, %r8d	# D.14562
	setg	%al	#, D.14562
	movzbl	%al, %eax	# D.14562, D.14562
	leal	-1(%rax,%rax), %eax	#, D.14562
	imull	%eax, %edx	# D.14562, v
	movl	%edx, %eax	# v, D.14562
	sarl	$3, %eax	#, D.14562
	movq	168(%rsp), %rsi	# %sfp, D.14566
	addb	%al, (%rcx,%rsi)	# D.14562, MEM[base: _2691, index: _3882, offset: 0B]
	movl	%edx, %esi	# v, D.14562
	sarl	$2, %esi	#, D.14562
	addb	%sil, (%rdi)	# D.14562, MEM[base: _945, offset: 0B]
	leal	(%rdx,%rdx,2), %edx	#, D.14562
	sarl	$3, %edx	#, D.14562
	addb	%dl, (%rcx,%r10)	# D.14562, MEM[base: _2691, index: _1695, offset: 0B]
	subb	%dl, (%rcx,%r11)	# D.14562, MEM[base: _2691, index: _3874, offset: 0B]
	subb	%sil, (%rcx,%r9)	# D.14562, MEM[base: _2691, index: _1705, offset: 0B]
	subb	%al, (%rcx,%r12)	# D.14562, MEM[base: _2691, index: _1741, offset: 0B]
.L241:
	addq	$1, %rdi	#, ivtmp.959
	cmpq	%rbx, %rdi	# D.14572, ivtmp.959
	jne	.L243	#,
	jmp	.L239	#
.L240:
	testb	$1, 32(%rsp)	#, %sfp
	je	.L244	#,
	movl	176(%rsp), %ecx	# %sfp, D.14562
	imull	408(%rsp), %ecx	# %sfp, D.14562
	sarl	$8, %ecx	#, D.14562
	addl	$1, %ecx	#, dcOffset
	leal	1(%rcx,%rcx), %esi	#, dcThreshold
	movq	24(%rsp), %r15	# %sfp, D.14571
	addq	208(%rsp), %r15	# %sfp, D.14571
	movq	%r15, %rdx	# D.14571, ivtmp.1030
	movl	$7, %edi	#, D.14563
	movl	$0, %eax	#, numEq
	movl	56(%rsp), %r14d	# %sfp, dstStride
	movl	%r14d, %ebx	# dstStride, dstStride
	leal	1(%r14), %r12d	#, D.14562
	movslq	%r12d, %r12	# D.14562, D.14566
	leal	2(%r14), %ebp	#, D.14562
	movslq	%ebp, %rbp	# D.14562, D.14566
	addl	$3, %ebx	#, D.14562
	movslq	%ebx, %rbx	# D.14562, D.14566
	leal	4(%r14), %r11d	#, D.14562
	movslq	%r11d, %r11	# D.14562, D.14566
	leal	5(%r14), %r10d	#, D.14562
	movslq	%r10d, %r10	# D.14562, D.14566
	leal	6(%r14), %r9d	#, D.14562
	movslq	%r9d, %r9	# D.14562, D.14566
	leal	7(%r14), %r8d	#, D.14562
	movslq	%r8d, %r8	# D.14562, D.14566
	movq	8(%rsp), %r14	# %sfp, D.14566
	movq	%r15, 40(%rsp)	# D.14571, %sfp
.L245:
	movzbl	(%rdx), %r13d	# MEM[base: _3396, offset: 0B], D.14562
	movzbl	(%rdx,%r14), %r15d	# MEM[base: _3396, index: _165, offset: 0B], D.14562
	subl	%r15d, %r13d	# D.14562, D.14562
	addl	%ecx, %r13d	# dcOffset, D.14562
	cmpl	%esi, %r13d	# dcThreshold, D.14562
	setb	%r13b	#, D.14581
	movzbl	%r13b, %r13d	# D.14581, D.14581
	addl	%r13d, %eax	# D.14581, numEq
	movzbl	1(%rdx), %r13d	# MEM[base: _3396, offset: 1B], D.14562
	movzbl	(%rdx,%r12), %r15d	# MEM[base: _3396, index: _1779, offset: 0B], D.14562
	subl	%r15d, %r13d	# D.14562, D.14562
	addl	%ecx, %r13d	# dcOffset, D.14562
	cmpl	%r13d, %esi	# D.14562, dcThreshold
	seta	%r13b	#, D.14581
	movzbl	%r13b, %r13d	# D.14581, D.14581
	addl	%r13d, %eax	# D.14581, numEq
	movzbl	2(%rdx), %r13d	# MEM[base: _3396, offset: 2B], D.14562
	movzbl	(%rdx,%rbp), %r15d	# MEM[base: _3396, index: _1792, offset: 0B], D.14562
	subl	%r15d, %r13d	# D.14562, D.14562
	addl	%ecx, %r13d	# dcOffset, D.14562
	cmpl	%r13d, %esi	# D.14562, dcThreshold
	seta	%r13b	#, D.14581
	movzbl	%r13b, %r13d	# D.14581, D.14581
	addl	%r13d, %eax	# D.14581, numEq
	movzbl	3(%rdx), %r13d	# MEM[base: _3396, offset: 3B], D.14562
	movzbl	(%rdx,%rbx), %r15d	# MEM[base: _3396, index: _1805, offset: 0B], D.14562
	subl	%r15d, %r13d	# D.14562, D.14562
	addl	%ecx, %r13d	# dcOffset, D.14562
	cmpl	%r13d, %esi	# D.14562, dcThreshold
	seta	%r13b	#, D.14581
	movzbl	%r13b, %r13d	# D.14581, D.14581
	addl	%r13d, %eax	# D.14581, numEq
	movzbl	4(%rdx), %r13d	# MEM[base: _3396, offset: 4B], D.14562
	movzbl	(%rdx,%r11), %r15d	# MEM[base: _3396, index: _1818, offset: 0B], D.14562
	subl	%r15d, %r13d	# D.14562, D.14562
	addl	%ecx, %r13d	# dcOffset, D.14562
	cmpl	%r13d, %esi	# D.14562, dcThreshold
	seta	%r13b	#, D.14581
	movzbl	%r13b, %r13d	# D.14581, D.14581
	addl	%r13d, %eax	# D.14581, numEq
	movzbl	5(%rdx), %r13d	# MEM[base: _3396, offset: 5B], D.14562
	movzbl	(%rdx,%r10), %r15d	# MEM[base: _3396, index: _1831, offset: 0B], D.14562
	subl	%r15d, %r13d	# D.14562, D.14562
	addl	%ecx, %r13d	# dcOffset, D.14562
	cmpl	%r13d, %esi	# D.14562, dcThreshold
	seta	%r13b	#, D.14581
	movzbl	%r13b, %r13d	# D.14581, D.14581
	addl	%r13d, %eax	# D.14581, numEq
	movzbl	6(%rdx), %r13d	# MEM[base: _3396, offset: 6B], D.14562
	movzbl	(%rdx,%r9), %r15d	# MEM[base: _3396, index: _1844, offset: 0B], D.14562
	subl	%r15d, %r13d	# D.14562, D.14562
	addl	%ecx, %r13d	# dcOffset, D.14562
	cmpl	%r13d, %esi	# D.14562, dcThreshold
	seta	%r13b	#, D.14581
	movzbl	%r13b, %r13d	# D.14581, D.14581
	addl	%r13d, %eax	# D.14581, numEq
	movzbl	7(%rdx), %r13d	# MEM[base: _3396, offset: 7B], D.14562
	movzbl	(%rdx,%r8), %r15d	# MEM[base: _3396, index: _1857, offset: 0B], D.14562
	subl	%r15d, %r13d	# D.14562, D.14562
	addl	%ecx, %r13d	# dcOffset, D.14562
	cmpl	%r13d, %esi	# D.14562, dcThreshold
	seta	%r13b	#, D.14581
	movzbl	%r13b, %r13d	# D.14581, D.14581
	addl	%r13d, %eax	# D.14581, numEq
	addq	%r14, %rdx	# D.14566, ivtmp.1030
	subl	$1, %edi	#, D.14563
	jne	.L245	#,
	movq	40(%rsp), %r15	# %sfp, D.14571
	cmpl	60(%rsp), %eax	# %sfp, numEq
	jle	.L246	#,
	movl	36(%rsp), %eax	# %sfp, c$QP
	leal	(%rax,%rax), %ecx	#, D.14562
	leal	0(,%rax,4), %esi	#, D.14564
	movq	24(%rsp), %rdi	# %sfp, dstBlock
	movq	208(%rsp), %rbp	# %sfp, D.14566
	movzbl	(%rdi,%rbp), %eax	# MEM[base: dstBlock_3501, index: _1151, offset: 0B], D.14562
	movq	96(%rsp), %rbx	# %sfp, D.14566
	movzbl	(%r15,%rbx), %edx	# MEM[base: _3965, index: _1191, offset: 0B], D.14562
	subl	%edx, %eax	# D.14562, D.14562
	addl	%ecx, %eax	# D.14562, D.14562
	cmpl	%esi, %eax	# D.14564, D.14562
	ja	.L239	#,
	movl	516(%rsp), %r14d	# %sfp, D.14562
	movl	%r14d, %eax	# D.14562, D.14562
	addl	$1, %eax	#, ivtmp.991
	movq	%rdi, %rdx	# dstBlock, dstBlock
	leaq	4(%rdx,%rbp), %rdi	#, ivtmp.993
	addq	144(%rsp), %rbp	# %sfp, D.14572
	movl	624(%rsp), %edx	# %sfp, D.14562
	leal	1(%rdx), %r9d	#, D.14563
	subl	%r14d, %r9d	# D.14562, D.14563
	movl	56(%rsp), %edx	# %sfp, dstStride
	leal	1(%rdx), %r8d	#, D.14563
	subl	%r14d, %r8d	# D.14562, D.14563
	movl	636(%rsp), %edx	# %sfp, D.14562
	leal	2(%rdx), %r11d	#, D.14563
	subl	%r14d, %r11d	# D.14562, D.14563
	movl	632(%rsp), %ebx	# %sfp, D.14562
	addl	$2, %ebx	#, D.14563
	subl	%r14d, %ebx	# D.14562, D.14563
	movl	640(%rsp), %edx	# %sfp, D.14562
	addl	$3, %edx	#, D.14563
	subl	%r14d, %edx	# D.14562, D.14563
	movl	%edx, %r12d	# D.14563, D.14563
	jmp	.L247	#
.L248:
	movzbl	(%rdi), %edx	# MEM[base: _1962, offset: 0B], D.14562
	leal	(%r12,%rax), %r10d	#, D.14563
	movslq	%r10d, %r10	# D.14563, D.14566
	movzbl	(%r15,%r10), %r10d	# *_1883, D.14562
	subl	%r10d, %edx	# D.14562, D.14562
	addl	%ecx, %edx	# D.14562, D.14564
	addl	$4, %eax	#, ivtmp.991
	addq	$4, %rdi	#, ivtmp.993
	cmpl	%esi, %edx	# D.14564, D.14564
	ja	.L239	#,
.L247:
	movl	412(%rsp), %edx	# %sfp, D.14563
	subl	516(%rsp), %edx	# %sfp, D.14563
	addl	%eax, %edx	# ivtmp.991, D.14563
	movslq	%edx, %rdx	# D.14563, D.14566
	movzbl	(%r15,%rdx), %edx	# *_1896, D.14562
	movslq	%eax, %r10	# ivtmp.991, D.14566
	movzbl	(%r15,%r10), %r10d	# *_1902, D.14562
	subl	%r10d, %edx	# D.14562, D.14562
	addl	%ecx, %edx	# D.14562, D.14562
	cmpl	%esi, %edx	# D.14564, D.14562
	ja	.L239	#,
	leal	(%r9,%rax), %edx	#, D.14563
	movslq	%edx, %rdx	# D.14563, D.14566
	movzbl	(%r15,%rdx), %edx	# *_1911, D.14562
	leal	(%r8,%rax), %r10d	#, D.14563
	movslq	%r10d, %r10	# D.14563, D.14566
	movzbl	(%r15,%r10), %r10d	# *_1916, D.14562
	subl	%r10d, %edx	# D.14562, D.14562
	addl	%ecx, %edx	# D.14562, D.14562
	cmpl	%esi, %edx	# D.14564, D.14562
	ja	.L239	#,
	leal	(%r11,%rax), %edx	#, D.14563
	movslq	%edx, %rdx	# D.14563, D.14566
	movzbl	(%r15,%rdx), %edx	# *_1926, D.14562
	leal	(%rbx,%rax), %r10d	#, D.14563
	movslq	%r10d, %r10	# D.14563, D.14566
	movzbl	(%r15,%r10), %r10d	# *_1932, D.14562
	subl	%r10d, %edx	# D.14562, D.14562
	addl	%ecx, %edx	# D.14562, D.14562
	cmpl	%esi, %edx	# D.14564, D.14562
	ja	.L239	#,
	cmpq	%rbp, %rdi	# D.14572, ivtmp.993
	jne	.L248	#,
	jmp	.L474	#
.L252:
	movq	128(%rsp), %rax	# %sfp, D.14566
	movzbl	(%r8,%rax), %edx	# MEM[base: _1745, index: _1723, offset: 0B], D.14562
	movq	%r8, %rax	# ivtmp.974, D.14574
	subq	168(%rsp), %rax	# %sfp, D.14574
	movq	8(%rsp), %rbx	# %sfp, D.14566
	movzbl	(%rax,%rbx), %r10d	# MEM[base: _1696, index: _165, offset: 0B], D.14562
	movl	%edx, %ecx	# D.14562, D.14562
	subl	%r10d, %ecx	# D.14562, D.14562
	movl	%ecx, %esi	# D.14562, tmp4238
	sarl	$31, %esi	#, tmp4238
	xorl	%esi, %ecx	# tmp4238, tmp4239
	subl	%esi, %ecx	# tmp4238, D.14562
	movl	36(%rsp), %r14d	# %sfp, c$QP
	cmpl	%r14d, %ecx	# c$QP, D.14562
	cmovge	%r10d, %edx	# D.14562,, D.14562, D.14562
	movq	400(%rsp), %rbx	# %sfp, D.14566
	movzbl	(%rax,%rbx), %ebx	# MEM[base: _1696, index: _1961, offset: 0B], D.14562
	movq	136(%rsp), %rsi	# %sfp, D.14566
	movzbl	(%rax,%rsi), %ecx	# MEM[base: _1696, index: _1965, offset: 0B], D.14562
	movl	%ebx, %esi	# D.14562, D.14562
	subl	%ecx, %esi	# D.14562, D.14562
	movl	%esi, %edi	# D.14562, tmp4243
	sarl	$31, %edi	#, tmp4243
	xorl	%edi, %esi	# tmp4243, tmp4244
	subl	%edi, %esi	# tmp4243, D.14562
	cmpl	%r14d, %esi	# c$QP, D.14562
	cmovge	%ebx, %ecx	# D.14562,, D.14562, D.14562
	movzbl	(%r8), %r14d	# MEM[base: _1745, offset: 0B], D.14562
	movq	40(%rsp), %rsi	# %sfp, D.14566
	movzbl	(%rax,%rsi), %r13d	# MEM[base: _1696, index: _1979, offset: 0B], D.14562
	leal	(%r10,%rdx,4), %esi	#, D.14562
	addl	%r14d, %esi	# D.14562, D.14562
	leal	4(%r13,%rsi), %r11d	#, D.14562
	movq	48(%rsp), %rdi	# %sfp, D.14566
	movzbl	(%rax,%rdi), %r12d	# MEM[base: _1696, index: _1986, offset: 0B], D.14562
	movl	%r11d, %r9d	# D.14562, D.14562
	subl	%edx, %r9d	# D.14562, D.14562
	addl	%r12d, %r9d	# D.14562, D.14562
	movq	216(%rsp), %r15	# %sfp, D.14566
	movzbl	(%rax,%r15), %esi	# MEM[base: _1696, index: _3874, offset: 0B], D.14562
	movl	%r9d, %r15d	# D.14562, D.14562
	subl	%edx, %r15d	# D.14562, D.14562
	movl	%r15d, %edi	# D.14562, D.14562
	movl	%esi, 80(%rsp)	# D.14562, %sfp
	addl	%esi, %edi	# D.14562, D.14562
	movl	%edi, %r15d	# D.14562, D.14562
	subl	%edx, %r15d	# D.14562, D.14562
	movq	72(%rsp), %rsi	# %sfp, D.14566
	movzbl	(%rax,%rsi), %esi	# MEM[base: _1696, index: _1998, offset: 0B], D.14562
	addl	%r15d, %esi	# D.14562, D.14562
	movl	%esi, %r15d	# D.14562, D.14562
	subl	%edx, %r15d	# D.14562, D.14562
	movzbl	(%rax,%rbp), %edx	# MEM[base: _1696, index: _2004, offset: 0B], D.14562
	addl	%edx, %r15d	# D.14562, D.14562
	movl	%r15d, %edx	# D.14562, D.14562
	subl	%r10d, %edx	# D.14562, D.14562
	addl	%ebx, %edx	# D.14562, D.14562
	movl	%edx, %ebx	# D.14562, D.14562
	subl	%r14d, %ebx	# D.14562, D.14562
	movl	%ebx, %r14d	# D.14562, D.14562
	addl	%ecx, %r14d	# D.14562, D.14562
	movl	%r14d, %ebx	# D.14562, D.14562
	subl	%r13d, %ebx	# D.14562, D.14562
	movl	%ebx, %r13d	# D.14562, D.14562
	addl	%ecx, %r13d	# D.14562, D.14562
	movl	%r13d, %ebx	# D.14562, D.14562
	subl	%r12d, %ebx	# D.14562, D.14562
	movl	%ebx, %r12d	# D.14562, D.14562
	addl	%ecx, %r12d	# D.14562, D.14562
	addl	%edi, %r11d	# D.14562, D.14562
	leal	(%r11,%r10,2), %r10d	#, D.14562
	sarl	$4, %r10d	#, D.14562
	movq	8(%rsp), %rbx	# %sfp, D.14566
	movb	%r10b, (%rax,%rbx)	# D.14562, MEM[base: _1696, index: _165, offset: 0B]
	addl	%esi, %r9d	# D.14562, D.14562
	movzbl	(%r8), %r10d	# MEM[base: _1745, offset: 0B], D.14562
	leal	(%r9,%r10,2), %r9d	#, D.14562
	sarl	$4, %r9d	#, D.14562
	movb	%r9b, (%r8)	# D.14562, MEM[base: _1745, offset: 0B]
	addl	%r15d, %edi	# D.14562, D.14562
	movq	40(%rsp), %r11	# %sfp, D.14566
	movzbl	(%rax,%r11), %r9d	# MEM[base: _1696, index: _1979, offset: 0B], D.14562
	leal	(%rdi,%r9,2), %edi	#, D.14562
	sarl	$4, %edi	#, D.14562
	movb	%dil, (%rax,%r11)	# D.14562, MEM[base: _1696, index: _1979, offset: 0B]
	addl	%edx, %esi	# D.14562, D.14562
	movq	48(%rsp), %r11	# %sfp, D.14566
	movzbl	(%rax,%r11), %edi	# MEM[base: _1696, index: _1986, offset: 0B], D.14562
	leal	(%rsi,%rdi,2), %esi	#, D.14562
	sarl	$4, %esi	#, D.14562
	movb	%sil, (%rax,%r11)	# D.14562, MEM[base: _1696, index: _1986, offset: 0B]
	addl	%r14d, %r15d	# D.14562, D.14562
	movq	216(%rsp), %rdi	# %sfp, D.14566
	movzbl	(%rax,%rdi), %esi	# MEM[base: _1696, index: _3874, offset: 0B], D.14562
	leal	(%r15,%rsi,2), %esi	#, D.14562
	sarl	$4, %esi	#, D.14562
	movb	%sil, (%rax,%rdi)	# D.14562, MEM[base: _1696, index: _3874, offset: 0B]
	addl	%r13d, %edx	# D.14562, D.14562
	movq	72(%rsp), %rdi	# %sfp, D.14566
	movzbl	(%rax,%rdi), %esi	# MEM[base: _1696, index: _1998, offset: 0B], D.14562
	leal	(%rdx,%rsi,2), %edx	#, D.14562
	sarl	$4, %edx	#, D.14562
	movb	%dl, (%rax,%rdi)	# D.14562, MEM[base: _1696, index: _1998, offset: 0B]
	addl	%r12d, %r14d	# D.14562, D.14562
	movzbl	(%rax,%rbp), %edx	# MEM[base: _1696, index: _2004, offset: 0B], D.14562
	leal	(%r14,%rdx,2), %edx	#, D.14562
	sarl	$4, %edx	#, D.14562
	movb	%dl, (%rax,%rbp)	# D.14562, MEM[base: _1696, index: _2004, offset: 0B]
	subl	80(%rsp), %r12d	# %sfp, D.14562
	addl	%ecx, %r12d	# D.14562, D.14562
	addl	%r12d, %r13d	# D.14562, D.14562
	movq	400(%rsp), %rbx	# %sfp, D.14566
	movzbl	(%rax,%rbx), %edx	# MEM[base: _1696, index: _1961, offset: 0B], D.14562
	leal	0(%r13,%rdx,2), %edx	#, D.14562
	sarl	$4, %edx	#, D.14562
	movb	%dl, (%rax,%rbx)	# D.14562, MEM[base: _1696, index: _1961, offset: 0B]
	addq	$1, %r8	#, ivtmp.974
	cmpq	120(%rsp), %r8	# %sfp, ivtmp.974
	jne	.L252	#,
	jmp	.L239	#
.L257:
	movq	%rcx, 48(%rsp)	# ivtmp.1014, %sfp
	movzbl	(%rcx), %r9d	# MEM[base: _1922, offset: 0B], D.14562
	movq	%rcx, %rdx	# ivtmp.1014, D.14574
	subq	216(%rsp), %rdx	# %sfp, D.14574
	movzbl	(%rdx,%r13), %r10d	# MEM[base: _1877, index: _2091, offset: 0B], D.14575
	movzbl	%r10b, %r11d	# D.14575, D.14562
	movzbl	(%rdx,%r14), %r8d	# MEM[base: _1877, index: _2097, offset: 0B], D.14562
	movzbl	(%rdx,%r15), %ebx	# MEM[base: _1877, index: _2101, offset: 0B], D.14562
	movl	%r9d, %eax	# D.14562, D.14562
	subl	%r11d, %eax	# D.14562, D.14562
	leal	(%rax,%rax,4), %eax	#, D.14562
	movl	%r8d, %esi	# D.14562, D.14562
	subl	%ebx, %esi	# D.14562, D.14562
	leal	(%rax,%rsi,2), %edi	#, middleEnergy
	movl	%edi, %esi	# middleEnergy, tmp4309
	sarl	$31, %esi	#, tmp4309
	movl	%esi, %eax	# tmp4309, D.14562
	xorl	%edi, %eax	# middleEnergy, D.14562
	subl	%esi, %eax	# tmp4309, D.14562
	cmpl	40(%rsp), %eax	# %sfp, D.14562
	jge	.L253	#,
	movl	%r11d, %esi	# D.14562, D.14562
	subl	%r9d, %esi	# D.14562, D.14562
	movl	%esi, %r12d	# D.14562, tmp4311
	shrl	$31, %r12d	#, tmp4311
	addl	%r12d, %esi	# tmp4311, tmp4312
	sarl	%esi	# tmp4313
	movq	72(%rsp), %r12	# %sfp, D.14566
	movzbl	(%rdx,%r12), %r12d	# MEM[base: _1877, index: _2126, offset: 0B], D.14562
	subl	%ebx, %r12d	# D.14562, D.14562
	leal	(%r12,%r12,4), %ebx	#, D.14562
	movq	400(%rsp), %r12	# %sfp, D.14566
	movzbl	(%rdx,%r12), %r12d	# MEM[base: _1877, index: _2132, offset: 0B], D.14562
	subl	%r12d, %r9d	# D.14562, D.14562
	leal	(%rbx,%r9,2), %r9d	#, rightEnergy
	movl	%r9d, %ebx	# rightEnergy, tmp4326
	sarl	$31, %ebx	#, tmp4326
	xorl	%ebx, %r9d	# tmp4326, tmp4327
	subl	%ebx, %r9d	# tmp4326, D.14562
	movq	168(%rsp), %rbx	# %sfp, D.14566
	movzbl	(%rdx,%rbx), %ebx	# MEM[base: _1877, index: _3882, offset: 0B], D.14562
	subl	%ebx, %r8d	# D.14562, D.14562
	leal	(%r8,%r8,4), %ebx	#, D.14562
	movq	8(%rsp), %r8	# %sfp, D.14566
	movzbl	(%rdx,%r8), %r8d	# MEM[base: _1877, index: _165, offset: 0B], D.14562
	subl	%r11d, %r8d	# D.14562, D.14562
	leal	(%rbx,%r8,2), %r8d	#, leftEnergy
	movl	%r8d, %r11d	# leftEnergy, tmp4338
	sarl	$31, %r11d	#, tmp4338
	xorl	%r11d, %r8d	# tmp4338, tmp4339
	subl	%r11d, %r8d	# tmp4338, D.14562
	cmpl	%r8d, %r9d	# D.14562, D.14562
	cmovle	%r9d, %r8d	# D.14562,, D.14562
	subl	%r8d, %eax	# D.14562, d
	movl	$0, %ebx	#, tmp6267
	cmovs	%ebx, %eax	# d,, tmp6267, d
	leal	32(%rax,%rax,4), %eax	#, D.14562
	sarl	$6, %eax	#, d
	movl	%eax, %r8d	# d, d
	negl	%edi	# D.14562
	testl	%edi, %edi	# D.14562
	setg	%al	#, D.14562
	movzbl	%al, %eax	# D.14562, D.14562
	leal	-1(%rax,%rax), %eax	#, D.14562
	imull	%r8d, %eax	# d, d
	testl	%esi, %esi	# tmp4313
	jle	.L255	#,
	testl	%eax, %eax	# d
	cmovs	%ebx, %eax	# d,, tmp6269, d
	cmpl	%esi, %eax	# tmp4313, d
	cmovg	%esi, %eax	# d,, tmp4313, d
	jmp	.L256	#
.L255:
	testl	%eax, %eax	# d
	movl	$0, %ebx	#, tmp6270
	cmovg	%ebx, %eax	# d,, tmp6270, d
	cmpl	%esi, %eax	# tmp4313, d
	cmovl	%esi, %eax	# d,, tmp4313, d
.L256:
	subl	%eax, %r10d	# d, tmp4349
	movb	%r10b, (%rdx,%r13)	# tmp4349, MEM[base: _1877, index: _2091, offset: 0B]
	movq	48(%rsp), %rbx	# %sfp, D.14574
	addb	%al, (%rbx)	# d, MEM[base: _1922, offset: 0B]
.L253:
	addq	$1, %rcx	#, ivtmp.1014
	cmpq	%rbp, %rcx	# D.14572, ivtmp.1014
	jne	.L257	#,
	jmp	.L239	#
.L244:
	testl	$1024, 32(%rsp)	#, %sfp
	je	.L239	#,
	movl	176(%rsp), %eax	# %sfp, D.14562
	imull	408(%rsp), %eax	# %sfp, D.14562
	sarl	$8, %eax	#, D.14562
	leal	1(%rax), %r13d	#, dcOffset
	leal	1(%r13,%r13), %r14d	#, dcThreshold
	movq	24(%rsp), %rax	# %sfp, dstBlock
	movq	%rax, %r15	# dstBlock, src
	movq	208(%rsp), %rbx	# %sfp, D.14566
	addq	%rbx, %r15	# D.14566, src
	movl	36(%rsp), %esi	# %sfp, c$QP
	leal	(%rsi,%rsi), %edi	#, D.14562
	movl	%edi, 432(%rsp)	# D.14562, %sfp
	sall	$3, %esi	#, D.14562
	movl	%esi, 416(%rsp)	# D.14562, %sfp
	addq	552(%rsp), %rax	# %sfp, ivtmp.1067
	movq	%rax, 72(%rsp)	# ivtmp.1067, %sfp
	leaq	(%r15,%rbx), %rax	#, ivtmp.1068
	movq	%rax, 80(%rsp)	# ivtmp.1068, %sfp
	addq	144(%rsp), %rbx	# %sfp, D.14565
	movq	%rbx, 184(%rsp)	# D.14565, %sfp
	movl	56(%rsp), %eax	# %sfp, D.14562
	negl	%eax	# D.14562
	cltq
	movq	%rax, 192(%rsp)	# D.14566, %sfp
.L276:
	movq	192(%rsp), %rax	# %sfp, D.14566
	movzbl	(%r15,%rax), %r8d	# MEM[base: src_3520, index: _438, offset: 0B], D.14562
	movzbl	(%r15), %edi	# MEM[base: src_3520, offset: 0B], D.14575
	movzbl	%dil, %r12d	# D.14575, max
	movl	%r8d, %eax	# D.14562, D.14562
	subl	%r12d, %eax	# max, D.14562
	movl	%eax, 180(%rsp)	# D.14562, %sfp
	movq	8(%rsp), %rbx	# %sfp, D.14566
	movzbl	(%r15,%rbx), %esi	# MEM[base: src_3520, index: _165, offset: 0B], D.14575
	movzbl	%sil, %ebp	# D.14575, max
	addl	%r13d, %eax	# dcOffset, D.14562
	cmpl	%r14d, %eax	# dcThreshold, D.14562
	setb	%al	#, D.14581
	movzbl	%al, %eax	# D.14581, D.14581
	movl	%r12d, %ecx	# max, D.14562
	subl	%ebp, %ecx	# max, D.14562
	addl	%r13d, %ecx	# dcOffset, D.14562
	cmpl	%ecx, %r14d	# D.14562, dcThreshold
	seta	%dl	#, D.14581
	movzbl	%dl, %edx	# D.14581, D.14581
	addl	%edx, %eax	# D.14581, numEq
	movq	88(%rsp), %rbx	# %sfp, D.14566
	movzbl	(%r15,%rbx), %r9d	# MEM[base: src_3520, index: _1157, offset: 0B], D.14562
	movl	%ebp, %edx	# max, D.14562
	subl	%r9d, %edx	# D.14562, D.14562
	addl	%r13d, %edx	# dcOffset, D.14562
	cmpl	%edx, %r14d	# D.14562, dcThreshold
	seta	%dl	#, D.14581
	movzbl	%dl, %edx	# D.14581, D.14581
	addl	%edx, %eax	# D.14581, numEq
	movq	72(%rsp), %rbx	# %sfp, ivtmp.1067
	movq	%rbx, 136(%rsp)	# ivtmp.1067, %sfp
	movzbl	(%rbx), %ebx	# MEM[base: _3716, offset: 0B], D.14562
	movl	%r9d, %edx	# D.14562, D.14562
	subl	%ebx, %edx	# D.14562, D.14562
	addl	%r13d, %edx	# dcOffset, D.14562
	cmpl	%edx, %r14d	# D.14562, dcThreshold
	seta	%dl	#, D.14581
	movzbl	%dl, %edx	# D.14581, D.14581
	addl	%eax, %edx	# numEq, numEq
	movq	80(%rsp), %rax	# %sfp, ivtmp.1068
	movq	%rax, 128(%rsp)	# ivtmp.1068, %sfp
	movzbl	(%rax), %r10d	# MEM[base: _3717, offset: 0B], D.14562
	movl	%ebx, %eax	# D.14562, D.14562
	subl	%r10d, %eax	# D.14562, D.14562
	movl	%eax, 200(%rsp)	# D.14562, %sfp
	leal	0(%r13,%rax), %ecx	#, D.14562
	cmpl	%ecx, %r14d	# D.14562, dcThreshold
	seta	%cl	#, D.14581
	movzbl	%cl, %ecx	# D.14581, D.14581
	addl	%ecx, %edx	# D.14581, numEq
	movq	96(%rsp), %rax	# %sfp, D.14566
	movzbl	(%r15,%rax), %r11d	# MEM[base: src_3520, index: _1191, offset: 0B], D.14562
	movl	%r10d, %ecx	# D.14562, D.14562
	subl	%r11d, %ecx	# D.14562, D.14562
	addl	%r13d, %ecx	# dcOffset, D.14562
	cmpl	%ecx, %r14d	# D.14562, dcThreshold
	seta	%cl	#, D.14581
	movzbl	%cl, %ecx	# D.14581, D.14581
	addl	%ecx, %edx	# D.14581, numEq
	movq	64(%rsp), %rax	# %sfp, D.14566
	movzbl	(%r15,%rax), %ecx	# MEM[base: src_3520, index: _1186, offset: 0B], D.14562
	movl	%r11d, %eax	# D.14562, D.14562
	movl	%ecx, 48(%rsp)	# D.14562, %sfp
	subl	%ecx, %eax	# D.14562, D.14562
	movl	%eax, %ecx	# D.14562, D.14562
	addl	%r13d, %ecx	# dcOffset, D.14562
	cmpl	%ecx, %r14d	# D.14562, dcThreshold
	seta	%cl	#, D.14581
	movzbl	%cl, %ecx	# D.14581, D.14581
	addl	%ecx, %edx	# D.14581, numEq
	movq	112(%rsp), %rax	# %sfp, D.14566
	movzbl	(%r15,%rax), %ecx	# MEM[base: src_3520, index: _1207, offset: 0B], D.14562
	movl	%ecx, %eax	# D.14562, D.14562
	movl	48(%rsp), %ecx	# %sfp, D.14562
	movl	%eax, 40(%rsp)	# D.14562, %sfp
	subl	%eax, %ecx	# D.14562, D.14562
	addl	%r13d, %ecx	# dcOffset, D.14562
	cmpl	%ecx, %r14d	# D.14562, dcThreshold
	seta	%cl	#, D.14581
	movzbl	%cl, %ecx	# D.14581, D.14581
	addl	%edx, %ecx	# numEq, numEq
	movq	152(%rsp), %rax	# %sfp, D.14566
	movzbl	(%r15,%rax), %eax	# MEM[base: src_3520, index: _1202, offset: 0B], D.14562
	movl	%eax, 120(%rsp)	# D.14562, %sfp
	movl	40(%rsp), %edx	# %sfp, D.14562
	subl	%eax, %edx	# D.14562, D.14562
	addl	%r13d, %edx	# dcOffset, D.14562
	cmpl	%edx, %r14d	# D.14562, dcThreshold
	seta	%dl	#, D.14581
	movzbl	%dl, %edx	# D.14581, D.14581
	addl	%ecx, %edx	# numEq, numEq
	cmpl	60(%rsp), %edx	# %sfp, numEq
	jle	.L258	#,
	cmpb	%sil, %dil	# D.14575, D.14575
	ja	.L406	#,
	movl	%ebp, %esi	# max, max
	movl	%r12d, %edi	# max, max
	jmp	.L259	#
.L262:
	movslq	%eax, %rdx	# ivtmp.1053, D.14566
	movzbl	(%r15,%rdx), %edx	# *_539, D.14575
	addl	%r9d, %eax	# dstStride, D.14563
	movslq	%eax, %rcx	# D.14563, D.14566
	movzbl	(%r15,%rcx), %ecx	# *_544, D.14575
	cmpb	%cl, %dl	# D.14575, D.14575
	jbe	.L260	#,
	movzbl	%dl, %edx	# D.14575, max
	cmpl	%edx, %esi	# max, max
	cmovl	%edx, %esi	# max,, max, max
	movzbl	%cl, %ecx	# D.14575, max
	cmpl	%ecx, %edi	# max, max
	cmovg	%ecx, %edi	# max,, max, max
	jmp	.L261	#
.L260:
	movzbl	%cl, %ecx	# D.14575, max
	cmpl	%ecx, %esi	# max, max
	cmovl	%ecx, %esi	# max,, max, max
	movzbl	%dl, %edx	# D.14575, max
	cmpl	%edx, %edi	# max, max
	cmovg	%edx, %edi	# max,, max, max
.L261:
	addl	%r9d, %eax	# dstStride, ivtmp.1053
	subl	$1, %r8d	#, D.14563
	jne	.L262	#,
	movl	200(%rsp), %r8d	# %sfp, D.14562
	movl	368(%rsp), %r9d	# %sfp, D.14562
	subl	%edi, %esi	# max, D.14562
	cmpl	432(%rsp), %esi	# %sfp, D.14562
	jge	.L263	#,
	movl	180(%rsp), %edx	# %sfp, D.14562
	movl	%edx, %eax	# D.14562, tmp4394
	sarl	$31, %eax	#, tmp4394
	xorl	%eax, %edx	# tmp4394, tmp4395
	subl	%eax, %edx	# tmp4394, D.14562
	movl	36(%rsp), %ecx	# %sfp, c$QP
	cmpl	%ecx, %edx	# c$QP, D.14562
	cmovge	%r12d, %r8d	# D.14562,, max, D.14562
	movl	120(%rsp), %esi	# %sfp, D.14562
	movl	%esi, %eax	# D.14562, D.14562
	movl	40(%rsp), %edi	# %sfp, D.14562
	subl	%edi, %eax	# D.14562, D.14562
	cltd
	xorl	%edx, %eax	# tmp4398, tmp4399
	subl	%edx, %eax	# tmp4398, D.14562
	cmpl	%ecx, %eax	# c$QP, D.14562
	movl	%esi, %eax	# D.14562, D.14562
	cmovge	%edi, %eax	# D.14562,, D.14562, D.14562
	movl	%eax, 120(%rsp)	# D.14562, %sfp
	leal	(%r12,%r8,4), %eax	#, D.14562
	addl	%ebp, %eax	# max, D.14562
	leal	4(%r9,%rax), %ecx	#, D.14562
	movl	%ecx, %esi	# D.14562, D.14562
	subl	%r8d, %esi	# D.14562, D.14562
	movl	%esi, %edi	# D.14562, D.14562
	addl	%ebx, %edi	# D.14562, D.14562
	movl	%edi, %edx	# D.14562, D.14562
	subl	%r8d, %edx	# D.14562, D.14562
	addl	%r10d, %edx	# D.14562, D.14562
	movl	%edx, %esi	# D.14562, D.14562
	subl	%r8d, %esi	# D.14562, D.14562
	movl	%esi, %eax	# D.14562, D.14562
	addl	%r11d, %eax	# D.14562, D.14562
	movl	%eax, %esi	# D.14562, D.14562
	subl	%r8d, %esi	# D.14562, D.14562
	addl	48(%rsp), %esi	# %sfp, D.14562
	movl	%esi, %r8d	# D.14562, D.14562
	subl	%r12d, %r8d	# max, D.14562
	addl	40(%rsp), %r8d	# %sfp, D.14562
	movl	%r8d, %r11d	# D.14562, D.14562
	subl	%ebp, %r11d	# max, D.14562
	movl	120(%rsp), %ebp	# %sfp, D.14562
	addl	%ebp, %r11d	# D.14562, D.14562
	movl	%r11d, %r12d	# D.14562, D.14562
	subl	%r9d, %r12d	# D.14562, D.14562
	movl	%r12d, %r9d	# D.14562, D.14562
	addl	%ebp, %r9d	# D.14562, D.14562
	movl	%r9d, %r12d	# D.14562, D.14562
	subl	%ebx, %r12d	# D.14562, D.14562
	movl	%r12d, %ebx	# D.14562, D.14562
	addl	%ebp, %ebx	# D.14562, D.14562
	movl	%ebx, %r12d	# D.14562, D.14562
	subl	%r10d, %r12d	# D.14562, D.14562
	leal	0(%rbp,%r12), %r12d	#, D.14562
	cmpl	$0, 244(%rsp)	#, %sfp
	je	.L266	#,
	movq	112(%rsp), %r10	# %sfp, D.14566
	movb	$-128, (%r15,%r10)	#, MEM[base: src_3520, index: _1207, offset: 0B]
	movq	64(%rsp), %r10	# %sfp, D.14566
	movb	$-128, (%r15,%r10)	#, MEM[base: src_3520, index: _1186, offset: 0B]
	movq	96(%rsp), %r10	# %sfp, D.14566
	movb	$-128, (%r15,%r10)	#, MEM[base: src_3520, index: _1191, offset: 0B]
	movq	128(%rsp), %r10	# %sfp, D.14574
	movb	$-128, (%r10)	#, MEM[base: _3717, offset: 0B]
	movq	136(%rsp), %r10	# %sfp, D.14574
	movb	$-128, (%r10)	#, MEM[base: _3716, offset: 0B]
	movq	88(%rsp), %r10	# %sfp, D.14566
	movb	$-128, (%r15,%r10)	#, MEM[base: src_3520, index: _1157, offset: 0B]
	movq	8(%rsp), %r10	# %sfp, D.14566
	movb	$-128, (%r15,%r10)	#, MEM[base: src_3520, index: _165, offset: 0B]
	movb	$-128, (%r15)	#, MEM[base: src_3520, offset: 0B]
.L266:
	addl	%edx, %ecx	# D.14562, D.14562
	movzbl	(%r15), %r10d	# MEM[base: src_3520, offset: 0B], D.14562
	leal	(%rcx,%r10,2), %ecx	#, D.14562
	sarl	$4, %ecx	#, D.14562
	movb	%cl, (%r15)	# D.14562, MEM[base: src_3520, offset: 0B]
	addl	%eax, %edi	# D.14562, D.14562
	movq	8(%rsp), %r10	# %sfp, D.14566
	movzbl	(%r15,%r10), %ecx	# MEM[base: src_3520, index: _165, offset: 0B], D.14562
	leal	(%rdi,%rcx,2), %ecx	#, D.14562
	sarl	$4, %ecx	#, D.14562
	movb	%cl, (%r15,%r10)	# D.14562, MEM[base: src_3520, index: _165, offset: 0B]
	addl	%esi, %edx	# D.14562, D.14562
	movq	88(%rsp), %rdi	# %sfp, D.14566
	movzbl	(%r15,%rdi), %ecx	# MEM[base: src_3520, index: _1157, offset: 0B], D.14562
	leal	(%rdx,%rcx,2), %edx	#, D.14562
	sarl	$4, %edx	#, D.14562
	movb	%dl, (%r15,%rdi)	# D.14562, MEM[base: src_3520, index: _1157, offset: 0B]
	addl	%r8d, %eax	# D.14562, D.14562
	movq	136(%rsp), %rdi	# %sfp, D.14574
	movzbl	(%rdi), %edx	# MEM[base: _3716, offset: 0B], D.14562
	leal	(%rax,%rdx,2), %eax	#, D.14562
	sarl	$4, %eax	#, D.14562
	movb	%al, (%rdi)	# D.14562, MEM[base: _3716, offset: 0B]
	addl	%r11d, %esi	# D.14562, D.14562
	movq	128(%rsp), %rdi	# %sfp, D.14574
	movzbl	(%rdi), %eax	# MEM[base: _3717, offset: 0B], D.14562
	leal	(%rsi,%rax,2), %eax	#, D.14562
	sarl	$4, %eax	#, D.14562
	movb	%al, (%rdi)	# D.14562, MEM[base: _3717, offset: 0B]
	addl	%r9d, %r8d	# D.14562, D.14562
	movq	96(%rsp), %rsi	# %sfp, D.14566
	movzbl	(%r15,%rsi), %eax	# MEM[base: src_3520, index: _1191, offset: 0B], D.14562
	leal	(%r8,%rax,2), %eax	#, D.14562
	sarl	$4, %eax	#, D.14562
	movb	%al, (%r15,%rsi)	# D.14562, MEM[base: src_3520, index: _1191, offset: 0B]
	addl	%ebx, %r11d	# D.14562, D.14562
	movq	64(%rsp), %rbx	# %sfp, D.14566
	movzbl	(%r15,%rbx), %eax	# MEM[base: src_3520, index: _1186, offset: 0B], D.14562
	leal	(%r11,%rax,2), %eax	#, D.14562
	sarl	$4, %eax	#, D.14562
	movb	%al, (%r15,%rbx)	# D.14562, MEM[base: src_3520, index: _1186, offset: 0B]
	addl	%r12d, %r9d	# D.14562, D.14562
	movq	112(%rsp), %rbx	# %sfp, D.14566
	movzbl	(%r15,%rbx), %eax	# MEM[base: src_3520, index: _1207, offset: 0B], D.14562
	leal	(%r9,%rax,2), %eax	#, D.14562
	sarl	$4, %eax	#, D.14562
	movb	%al, (%r15,%rbx)	# D.14562, MEM[base: src_3520, index: _1207, offset: 0B]
	jmp	.L263	#
.L258:
	movl	%r10d, %edx	# D.14562, D.14562
	subl	%ebx, %edx	# D.14562, D.14562
	leal	(%rdx,%rdx,4), %edx	#, D.14562
	movl	%r9d, %ecx	# D.14562, D.14562
	subl	%r11d, %ecx	# D.14562, D.14562
	leal	(%rdx,%rcx,2), %ecx	#, middleEnergy
	movl	%ecx, %esi	# middleEnergy, tmp4460
	sarl	$31, %esi	#, tmp4460
	movl	%esi, %edx	# tmp4460, D.14562
	xorl	%ecx, %edx	# middleEnergy, D.14562
	subl	%esi, %edx	# tmp4460, D.14562
	cmpl	416(%rsp), %edx	# %sfp, D.14562
	jge	.L263	#,
	movl	200(%rsp), %eax	# %sfp, D.14562
	movl	%eax, %esi	# D.14562, tmp4461
	shrl	$31, %esi	#, tmp4461
	addl	%esi, %eax	# tmp4461, tmp4462
	sarl	%eax	# tmp4463
	movl	48(%rsp), %esi	# %sfp, D.14562
	subl	%r11d, %esi	# D.14562, D.14562
	leal	(%rsi,%rsi,4), %esi	#, D.14562
	subl	40(%rsp), %r10d	# %sfp, D.14562
	leal	(%rsi,%r10,2), %esi	#, rightEnergy
	movl	%esi, %edi	# rightEnergy, tmp4472
	sarl	$31, %edi	#, tmp4472
	xorl	%edi, %esi	# tmp4472, tmp4473
	subl	%edi, %esi	# tmp4472, D.14562
	subl	%ebp, %r9d	# max, D.14562
	leal	(%r9,%r9,4), %edi	#, D.14562
	movl	%r12d, %r8d	# max, D.14562
	subl	%ebx, %r8d	# D.14562, D.14562
	leal	(%rdi,%r8,2), %edi	#, leftEnergy
	movl	%edi, %r8d	# leftEnergy, tmp4482
	sarl	$31, %r8d	#, tmp4482
	xorl	%r8d, %edi	# tmp4482, tmp4483
	subl	%r8d, %edi	# tmp4482, D.14562
	cmpl	%edi, %esi	# D.14562, D.14562
	cmovg	%edi, %esi	# D.14562,, D.14562, D.14562
	subl	%esi, %edx	# D.14562, d
	movl	$0, %esi	#, tmp6374
	cmovs	%esi, %edx	# d,, tmp6374, d
	leal	32(%rdx,%rdx,4), %edx	#, D.14562
	sarl	$6, %edx	#, d
	negl	%ecx	# D.14562
	testl	%ecx, %ecx	# D.14562
	setg	%cl	#, D.14562
	movzbl	%cl, %ecx	# D.14562, D.14562
	leal	-1(%rcx,%rcx), %ecx	#, D.14562
	imull	%ecx, %edx	# D.14562, d
	testl	%eax, %eax	# tmp4463
	jle	.L268	#,
	testl	%edx, %edx	# d
	cmovs	%esi, %edx	# d,, tmp6375, d
	cmpl	%eax, %edx	# tmp4463, d
	cmovg	%eax, %edx	# d,, tmp4463, d
	jmp	.L269	#
.L268:
	testl	%edx, %edx	# d
	movl	$0, %esi	#, tmp6376
	cmovg	%esi, %edx	# d,, tmp6376, d
	cmpl	%eax, %edx	# tmp4463, d
	cmovl	%eax, %edx	# d,, tmp4463, d
.L269:
	testl	%edx, %edx	# d
	je	.L270	#,
	cmpl	$0, 244(%rsp)	#, %sfp
	je	.L270	#,
	movl	%edx, %ecx	# d, d
	sarl	$31, %ecx	#, d
	andl	$64, %ecx	#, d
	subl	$32, %ecx	#, d
	movl	%ebx, %eax	# D.14562, D.14562
	subl	%ecx, %eax	# d, D.14562
	movl	%eax, %edx	# D.14562, D.14582
	testl	$-256, %eax	#, D.14562
	je	.L273	#,
	negl	%eax	# D.14562
	cltd
.L273:
	movq	136(%rsp), %rax	# %sfp, D.14574
	movb	%dl, (%rax)	# D.14582, MEM[base: _3716, offset: 0B]
	movq	128(%rsp), %rax	# %sfp, D.14574
	movzbl	(%rax), %eax	# MEM[base: _3717, offset: 0B], D.14562
	addl	%ecx, %eax	# d, D.14562
	movl	%eax, %edx	# D.14562, D.14582
	testl	$-256, %eax	#, D.14562
	je	.L275	#,
	negl	%eax	# D.14562
	cltd
.L275:
	movq	128(%rsp), %rax	# %sfp, D.14574
	movb	%dl, (%rax)	# D.14582, MEM[base: _3717, offset: 0B]
	movl	$0, %edx	#, d
.L270:
	movq	136(%rsp), %rax	# %sfp, D.14574
	subb	%dl, (%rax)	# d, MEM[base: _3716, offset: 0B]
	movq	128(%rsp), %rax	# %sfp, D.14574
	addb	%dl, (%rax)	# d, MEM[base: _3717, offset: 0B]
.L263:
	addq	$1, %r15	#, src
	addq	$1, 72(%rsp)	#, %sfp
	addq	$1, 80(%rsp)	#, %sfp
	cmpq	184(%rsp), %r15	# %sfp, src
	jne	.L276	#,
.L239:
	movl	16(%rsp), %eax	# %sfp, tmp5726
	subl	$8, %eax	#, tmp5726
	js	.L277	#,
	testl	$8192, 32(%rsp)	#, %sfp
	je	.L278	#,
	movq	24(%rsp), %rax	# %sfp, dstBlock
	leaq	-4(%rax), %rsi	#, src
	cmpq	$0, lut.5312+2040(%rip)	#, lut
	jne	.L279	#,
	movl	$-512, %ebx	#, ivtmp.868
	movl	$0, %r11d	#, ivtmp.862
.L282:
	leal	(%r11,%r11), %r9d	#, tmp5158
	cmpl	$127, %r11d	#, ivtmp.862
	cmovg	%ebx, %r9d	# tmp5158,, ivtmp.868, v
	leal	15(%r9), %edi	#, tmp4509
	testl	%r9d, %r9d	# v
	cmovns	%r9d, %edi	# tmp4509,, v, v
	sarl	$4, %edi	#, D.14562
	movzbl	%dil, %edi	# D.14562, a
	leal	(%r9,%r9,2), %edx	#, D.14562
	leal	15(%rdx), %eax	#, tmp4517
	testl	%edx, %edx	# D.14562
	cmovns	%edx, %eax	# tmp4517,, D.14562, D.14562
	sarl	$4, %eax	#, D.14562
	movzbl	%al, %eax	# D.14562, b
	leal	(%r9,%r9,4), %edx	#, D.14562
	leal	15(%rdx), %ecx	#, tmp4525
	testl	%edx, %edx	# D.14562
	cmovs	%ecx, %edx	# tmp4525,, D.14562
	movl	%edx, %ecx	# D.14562, D.14562
	sarl	$4, %ecx	#, D.14562
	movzbl	%cl, %edx	# D.14562, c
	movq	%rdx, %r8	# c, D.14567
	negq	%r8	# D.14567
	movzbl	%r8b, %r8d	# D.14567, C
	movq	%rdi, %rcx	# a, D.14567
	negq	%rcx	# D.14567
	movzbl	%cl, %r10d	# D.14567, A
	movq	%rax, %rcx	# b, D.14567
	salq	$48, %rcx	#, D.14567
	orq	%r10, %rcx	# A, D.14567
	salq	$56, %rdi	#, D.14567
	orq	%rcx, %rdi	# D.14567, D.14567
	salq	$40, %rdx	#, D.14567
	orq	%rdi, %rdx	# D.14567, D.14567
	leal	0(,%r9,8), %ecx	#, tmp4539
	subl	%r9d, %ecx	# v, D.14562
	leal	15(%rcx), %edi	#, tmp4543
	testl	%ecx, %ecx	# D.14562
	cmovs	%edi, %ecx	# tmp4543,, D.14562
	sarl	$4, %ecx	#, D.14562
	movzbl	%cl, %ecx	# D.14562, d
	salq	$32, %rcx	#, D.14567
	orq	%rcx, %rdx	# D.14567, D.14567
	movq	%r8, %rcx	# C, D.14567
	salq	$24, %rcx	#, D.14567
	orq	%rcx, %rdx	# D.14567, D.14567
	salq	$16, %r8	#, D.14567
	orq	%r8, %rdx	# D.14567, D.14567
	negq	%rax	# D.14567
	movzbl	%al, %eax	# D.14567, B
	salq	$8, %rax	#, D.14567
	orq	%rax, %rdx	# D.14567, tmp4556
	movq	%rdx, lut.5312(,%r11,8)	# tmp4556, MEM[symbol: lut, index: ivtmp.862_3575, step: 8, offset: 0B]
	addq	$1, %r11	#, ivtmp.862
	addl	$2, %ebx	#, ivtmp.868
	cmpq	$256, %r11	#, ivtmp.862
	jne	.L282	#,
	jmp	.L279	#
.L285:
	movzbl	1(%rsi), %ebp	# MEM[base: src_3574, offset: 1B], D.14575
	movzbl	2(%rsi), %ebx	# MEM[base: src_3574, offset: 2B], D.14575
	movzbl	3(%rsi), %r11d	# MEM[base: src_3574, offset: 3B], D.14575
	movzbl	4(%rsi), %r10d	# MEM[base: src_3574, offset: 4B], D.14575
	movzbl	%r11b, %edi	# D.14575, D.14562
	movzbl	%r10b, %eax	# D.14575, D.14562
	subl	%eax, %edi	# D.14562, b
	movzbl	5(%rsi), %r9d	# MEM[base: src_3574, offset: 5B], D.14575
	movzbl	6(%rsi), %r8d	# MEM[base: src_3574, offset: 6B], D.14575
	movl	%edi, %eax	# b, tmp4559
	sarl	$31, %eax	#, tmp4559
	movl	%eax, %edx	# tmp4559, tmp4560
	xorl	%edi, %edx	# b, tmp4560
	subl	%eax, %edx	# tmp4559, D.14562
	movzbl	%bpl, %ecx	# D.14575, D.14562
	movzbl	%bl, %eax	# D.14575, D.14562
	subl	%eax, %ecx	# D.14562, a
	movl	%ecx, %eax	# a, tmp4565
	sarl	$31, %eax	#, tmp4565
	xorl	%eax, %ecx	# tmp4565, tmp4566
	subl	%eax, %ecx	# tmp4565, D.14562
	movzbl	%r9b, %eax	# D.14575, D.14562
	movzbl	%r8b, %r13d	# D.14575, D.14562
	subl	%r13d, %eax	# D.14562, c
	movl	%eax, %r13d	# c, tmp4571
	sarl	$31, %r13d	#, tmp4571
	xorl	%r13d, %eax	# tmp4571, tmp4572
	subl	%r13d, %eax	# tmp4571, D.14562
	addl	%eax, %ecx	# D.14562, D.14562
	movl	%ecx, %eax	# D.14562, tmp4576
	shrl	$31, %eax	#, tmp4576
	addl	%eax, %ecx	# tmp4576, tmp4577
	sarl	%ecx	# D.14562
	subl	%ecx, %edx	# D.14562, d
	cmovs	%r14d, %edx	# d,, tmp5185, d
	cmpl	%edx, %r15d	# d, c$QP
	jle	.L283	#,
	negl	%edi	# D.14562
	testl	%edi, %edi	# D.14562
	setg	%al	#, D.14562
	movzbl	%al, %eax	# D.14562, D.14562
	leal	-1(%rax,%rax), %eax	#, D.14562
	imull	%eax, %edx	# D.14562, v
	leal	7(%rdx), %eax	#, tmp4583
	testl	%edx, %edx	# v
	cmovns	%edx, %eax	# tmp4583,, v, v
	sarl	$3, %eax	#, D.14562
	addl	%eax, %ebp	# D.14562, tmp4585
	movb	%bpl, 1(%rsi)	# tmp4585, MEM[base: src_3574, offset: 1B]
	leal	3(%rdx), %ecx	#, tmp4588
	testl	%edx, %edx	# v
	cmovns	%edx, %ecx	# tmp4588,, v, v
	sarl	$2, %ecx	#, D.14562
	addl	%ecx, %ebx	# D.14562, tmp4590
	movb	%bl, 2(%rsi)	# tmp4590, MEM[base: src_3574, offset: 2B]
	leal	(%rdx,%rdx,2), %edi	#, D.14562
	leal	7(%rdi), %edx	#, tmp4596
	testl	%edi, %edi	# D.14562
	cmovns	%edi, %edx	# tmp4596,, D.14562, D.14562
	sarl	$3, %edx	#, D.14562
	addl	%edx, %r11d	# D.14562, tmp4598
	movb	%r11b, 3(%rsi)	# tmp4598, MEM[base: src_3574, offset: 3B]
	subl	%edx, %r10d	# D.14562, tmp4599
	movb	%r10b, 4(%rsi)	# tmp4599, MEM[base: src_3574, offset: 4B]
	subl	%ecx, %r9d	# D.14562, tmp4600
	movb	%r9b, 5(%rsi)	# tmp4600, MEM[base: src_3574, offset: 5B]
	subl	%eax, %r8d	# D.14562, tmp4601
	movb	%r8b, 6(%rsi)	# tmp4601, MEM[base: src_3574, offset: 6B]
.L283:
	addq	8(%rsp), %rsi	# %sfp, src
	subl	$1, %r12d	#, D.14563
	jne	.L285	#,
	jmp	.L286	#
.L278:
	testb	$2, 32(%rsp)	#, %sfp
	je	.L287	#,
	movq	24(%rsp), %rax	# %sfp, dstBlock
	leaq	-4(%rax), %r15	#, dst
	movl	176(%rsp), %ecx	# %sfp, D.14562
	imull	408(%rsp), %ecx	# %sfp, D.14562
	sarl	$8, %ecx	#, D.14562
	addl	$1, %ecx	#, dcOffset
	leal	1(%rcx,%rcx), %esi	#, dcThreshold
	movq	%r15, %rdx	# dst, dst
	movl	$8, %edi	#, D.14563
	movl	$0, %eax	#, numEq
	movq	8(%rsp), %r9	# %sfp, D.14566
.L288:
	movzbl	1(%rdx), %r8d	# MEM[base: dst_3576, offset: 1B], D.14562
	movzbl	(%rdx), %r10d	# MEM[base: dst_3576, offset: 0B], D.14562
	subl	%r8d, %r10d	# D.14562, D.14562
	addl	%ecx, %r10d	# dcOffset, D.14562
	cmpl	%esi, %r10d	# dcThreshold, D.14562
	setb	%r10b	#, D.14581
	movzbl	%r10b, %r10d	# D.14581, D.14581
	addl	%eax, %r10d	# numEq, numEq
	movzbl	2(%rdx), %eax	# MEM[base: dst_3576, offset: 2B], D.14562
	subl	%eax, %r8d	# D.14562, D.14562
	addl	%ecx, %r8d	# dcOffset, D.14562
	cmpl	%r8d, %esi	# D.14562, dcThreshold
	seta	%r8b	#, D.14581
	movzbl	%r8b, %r8d	# D.14581, D.14581
	addl	%r8d, %r10d	# D.14581, numEq
	movzbl	3(%rdx), %r8d	# MEM[base: dst_3576, offset: 3B], D.14562
	subl	%r8d, %eax	# D.14562, D.14562
	addl	%ecx, %eax	# dcOffset, D.14562
	cmpl	%eax, %esi	# D.14562, dcThreshold
	seta	%r11b	#, D.14581
	movzbl	%r11b, %r11d	# D.14581, D.14581
	addl	%r11d, %r10d	# D.14581, numEq
	movzbl	4(%rdx), %eax	# MEM[base: dst_3576, offset: 4B], D.14562
	subl	%eax, %r8d	# D.14562, D.14562
	addl	%ecx, %r8d	# dcOffset, D.14562
	cmpl	%r8d, %esi	# D.14562, dcThreshold
	seta	%r8b	#, D.14581
	movzbl	%r8b, %r8d	# D.14581, D.14581
	addl	%r8d, %r10d	# D.14581, numEq
	movzbl	5(%rdx), %r8d	# MEM[base: dst_3576, offset: 5B], D.14562
	subl	%r8d, %eax	# D.14562, D.14562
	addl	%ecx, %eax	# dcOffset, D.14562
	cmpl	%eax, %esi	# D.14562, dcThreshold
	seta	%r11b	#, D.14581
	movzbl	%r11b, %r11d	# D.14581, D.14581
	addl	%r11d, %r10d	# D.14581, numEq
	movzbl	6(%rdx), %eax	# MEM[base: dst_3576, offset: 6B], D.14562
	subl	%eax, %r8d	# D.14562, D.14562
	addl	%ecx, %r8d	# dcOffset, D.14562
	cmpl	%r8d, %esi	# D.14562, dcThreshold
	seta	%r8b	#, D.14581
	movzbl	%r8b, %r8d	# D.14581, D.14581
	addl	%r10d, %r8d	# numEq, numEq
	movzbl	7(%rdx), %r10d	# MEM[base: dst_3576, offset: 7B], D.14562
	subl	%r10d, %eax	# D.14562, D.14562
	addl	%ecx, %eax	# dcOffset, D.14562
	cmpl	%eax, %esi	# D.14562, dcThreshold
	seta	%al	#, D.14581
	movzbl	%al, %eax	# D.14581, D.14581
	addl	%r8d, %eax	# numEq, numEq
	addq	%r9, %rdx	# D.14566, dst
	subl	$1, %edi	#, D.14563
	jne	.L288	#,
	movl	36(%rsp), %ebx	# %sfp, c$QP
	leal	0(,%rbx,8), %r12d	#, D.14562
	movl	$8, %ebp	#, D.14563
	movl	$0, %r13d	#, tmp5186
	movq	8(%rsp), %r8	# %sfp, D.14566
	cmpl	60(%rsp), %eax	# %sfp, numEq
	jle	.L416	#,
	movl	%ebx, %eax	# c$QP, c$QP
	addl	%eax, %eax	# D.14562
	leal	0(,%rbx,4), %edx	#, D.14564
	movq	24(%rsp), %rbx	# %sfp, dstBlock
	movzbl	-4(%rbx), %ecx	# MEM[base: dstBlock_3501, offset: -4B], D.14562
	movzbl	1(%rbx), %esi	# MEM[base: dstBlock_3501, offset: 1B], D.14562
	subl	%esi, %ecx	# D.14562, D.14562
	addl	%eax, %ecx	# D.14562, D.14562
	cmpl	%edx, %ecx	# D.14564, D.14562
	ja	.L286	#,
	movq	256(%rsp), %rsi	# %sfp, ivtmp.1193
	movzbl	-2(%rsi), %ecx	# MEM[base: _3941, offset: -2B], D.14562
	movzbl	3(%rsi), %esi	# MEM[base: _3941, offset: 3B], D.14562
	subl	%esi, %ecx	# D.14562, D.14562
	addl	%eax, %ecx	# D.14562, D.14562
	cmpl	%ecx, %edx	# D.14562, D.14564
	jb	.L286	#,
	movq	264(%rsp), %rsi	# %sfp, ivtmp.1212
	movzbl	(%rsi), %ecx	# MEM[base: _3958, offset: 0B], D.14562
	movzbl	-3(%rsi), %esi	# MEM[base: _3958, offset: -3B], D.14562
	subl	%esi, %ecx	# D.14562, D.14562
	addl	%eax, %ecx	# D.14562, D.14562
	cmpl	%ecx, %edx	# D.14562, D.14564
	jb	.L286	#,
	movq	272(%rsp), %rsi	# %sfp, ivtmp.1215
	movzbl	2(%rsi), %ecx	# MEM[base: _3960, offset: 2B], D.14562
	movzbl	-1(%rsi), %esi	# MEM[base: _3960, offset: -1B], D.14562
	subl	%esi, %ecx	# D.14562, D.14562
	addl	%eax, %ecx	# D.14562, D.14562
	cmpl	%ecx, %edx	# D.14562, D.14564
	jb	.L286	#,
	movq	8(%rsp), %rsi	# %sfp, D.14566
	movzbl	-4(%rbx,%rsi,4), %ecx	# MEM[base: dstBlock_3501, index: _165, step: 4, offset: -4B], D.14562
	movzbl	1(%rbx,%rsi,4), %esi	# MEM[base: dstBlock_3501, index: _165, step: 4, offset: 1B], D.14562
	subl	%esi, %ecx	# D.14562, D.14562
	addl	%eax, %ecx	# D.14562, D.14562
	cmpl	%edx, %ecx	# D.14564, D.14562
	ja	.L286	#,
	movq	248(%rsp), %rbx	# %sfp, ivtmp.1198
	movzbl	-2(%rbx), %ecx	# MEM[base: _3943, offset: -2B], D.14562
	movzbl	3(%rbx), %esi	# MEM[base: _3943, offset: 3B], D.14562
	subl	%esi, %ecx	# D.14562, D.14562
	addl	%eax, %ecx	# D.14562, D.14562
	cmpl	%edx, %ecx	# D.14564, D.14562
	ja	.L286	#,
	movq	344(%rsp), %rbx	# %sfp, ivtmp.1209
	movzbl	(%rbx), %ecx	# MEM[base: _3953, offset: 0B], D.14562
	movzbl	-3(%rbx), %esi	# MEM[base: _3953, offset: -3B], D.14562
	subl	%esi, %ecx	# D.14562, D.14562
	addl	%eax, %ecx	# D.14562, D.14562
	cmpl	%edx, %ecx	# D.14564, D.14562
	ja	.L286	#,
	movq	336(%rsp), %rbx	# %sfp, ivtmp.1205
	movzbl	2(%rbx), %ecx	# MEM[base: _3947, offset: 2B], D.14562
	movzbl	-1(%rbx), %esi	# MEM[base: _3947, offset: -1B], D.14562
	subl	%esi, %ecx	# D.14562, D.14562
	addl	%ecx, %eax	# D.14562, D.14562
	cmpl	%edx, %eax	# D.14564, D.14562
	ja	.L286	#,
	movl	$8, 120(%rsp)	#, %sfp
.L292:
	movzbl	-1(%r15), %edx	# MEM[base: dst_3584, offset: -1B], D.14562
	movzbl	(%r15), %r10d	# MEM[base: dst_3584, offset: 0B], D.14562
	movl	%edx, %eax	# D.14562, D.14562
	subl	%r10d, %eax	# D.14562, D.14562
	movl	%eax, %ecx	# D.14562, tmp4669
	sarl	$31, %ecx	#, tmp4669
	xorl	%ecx, %eax	# tmp4669, tmp4670
	subl	%ecx, %eax	# tmp4669, D.14562
	movl	36(%rsp), %ebx	# %sfp, c$QP
	cmpl	%eax, %ebx	# D.14562, c$QP
	cmovle	%r10d, %edx	# D.14562,, D.14562, D.14562
	movzbl	8(%r15), %eax	# MEM[base: dst_3584, offset: 8B], D.14562
	movzbl	7(%r15), %r11d	# MEM[base: dst_3584, offset: 7B], D.14562
	movl	%eax, %ecx	# D.14562, D.14562
	subl	%r11d, %ecx	# D.14562, D.14562
	movl	%ecx, %esi	# D.14562, tmp4673
	sarl	$31, %esi	#, tmp4673
	xorl	%esi, %ecx	# tmp4673, tmp4674
	subl	%esi, %ecx	# tmp4673, D.14562
	cmpl	%ecx, %ebx	# D.14562, c$QP
	cmovle	%r11d, %eax	# D.14562,, D.14562, D.14562
	movzbl	1(%r15), %r13d	# MEM[base: dst_3584, offset: 1B], D.14562
	movzbl	2(%r15), %r12d	# MEM[base: dst_3584, offset: 2B], D.14562
	leal	(%r10,%rdx,4), %ecx	#, D.14562
	addl	%r13d, %ecx	# D.14562, D.14562
	leal	4(%r12,%rcx), %ebx	#, D.14562
	movzbl	3(%r15), %ebp	# MEM[base: dst_3584, offset: 3B], D.14562
	movl	%ebx, %r9d	# D.14562, D.14562
	subl	%edx, %r9d	# D.14562, D.14562
	addl	%ebp, %r9d	# D.14562, D.14562
	movzbl	4(%r15), %esi	# MEM[base: dst_3584, offset: 4B], D.14562
	movl	%r9d, %r8d	# D.14562, D.14562
	subl	%edx, %r8d	# D.14562, D.14562
	movl	%esi, 40(%rsp)	# D.14562, %sfp
	addl	%esi, %r8d	# D.14562, D.14562
	movzbl	5(%r15), %ecx	# MEM[base: dst_3584, offset: 5B], D.14562
	movl	%r8d, %r14d	# D.14562, D.14562
	subl	%edx, %r14d	# D.14562, D.14562
	movl	%r14d, %edi	# D.14562, D.14562
	movl	%ecx, 48(%rsp)	# D.14562, %sfp
	addl	%ecx, %edi	# D.14562, D.14562
	movzbl	6(%r15), %r14d	# MEM[base: dst_3584, offset: 6B], D.14562
	movl	%edi, %esi	# D.14562, D.14562
	subl	%edx, %esi	# D.14562, D.14562
	movl	%r14d, 72(%rsp)	# D.14562, %sfp
	addl	%r14d, %esi	# D.14562, D.14562
	movl	%esi, %edx	# D.14562, D.14562
	subl	%r10d, %edx	# D.14562, D.14562
	movl	%edx, %ecx	# D.14562, D.14562
	addl	%r11d, %ecx	# D.14562, D.14562
	movl	%ecx, %edx	# D.14562, D.14562
	subl	%r13d, %edx	# D.14562, D.14562
	addl	%eax, %edx	# D.14562, D.14562
	movl	%edx, %r14d	# D.14562, D.14562
	subl	%r12d, %r14d	# D.14562, D.14562
	addl	%eax, %r14d	# D.14562, D.14562
	movl	%r14d, 80(%rsp)	# D.14562, %sfp
	subl	%ebp, %r14d	# D.14562, D.14562
	addl	%eax, %r14d	# D.14562, D.14562
	addl	%r8d, %ebx	# D.14562, D.14562
	leal	(%rbx,%r10,2), %r10d	#, D.14562
	sarl	$4, %r10d	#, D.14562
	movb	%r10b, (%r15)	# D.14562, MEM[base: dst_3584, offset: 0B]
	addl	%edi, %r9d	# D.14562, D.14562
	leal	(%r9,%r13,2), %r9d	#, D.14562
	sarl	$4, %r9d	#, D.14562
	movb	%r9b, 1(%r15)	# D.14562, MEM[base: dst_3584, offset: 1B]
	addl	%esi, %r8d	# D.14562, D.14562
	leal	(%r8,%r12,2), %r8d	#, D.14562
	sarl	$4, %r8d	#, D.14562
	movb	%r8b, 2(%r15)	# D.14562, MEM[base: dst_3584, offset: 2B]
	addl	%ecx, %edi	# D.14562, D.14562
	leal	(%rdi,%rbp,2), %edi	#, D.14562
	sarl	$4, %edi	#, D.14562
	movb	%dil, 3(%r15)	# D.14562, MEM[base: dst_3584, offset: 3B]
	addl	%edx, %esi	# D.14562, D.14562
	movl	40(%rsp), %edi	# %sfp, D.14562
	leal	(%rsi,%rdi,2), %esi	#, D.14562
	sarl	$4, %esi	#, D.14562
	movb	%sil, 4(%r15)	# D.14562, MEM[base: dst_3584, offset: 4B]
	movl	80(%rsp), %ebx	# %sfp, D.14562
	addl	%ebx, %ecx	# D.14562, D.14562
	movl	48(%rsp), %esi	# %sfp, D.14562
	leal	(%rcx,%rsi,2), %ecx	#, D.14562
	sarl	$4, %ecx	#, D.14562
	movb	%cl, 5(%r15)	# D.14562, MEM[base: dst_3584, offset: 5B]
	addl	%r14d, %edx	# D.14562, D.14562
	movl	72(%rsp), %esi	# %sfp, D.14562
	leal	(%rdx,%rsi,2), %edx	#, D.14562
	sarl	$4, %edx	#, D.14562
	movb	%dl, 6(%r15)	# D.14562, MEM[base: dst_3584, offset: 6B]
	subl	%edi, %r14d	# D.14562, D.14562
	addl	%r14d, %eax	# D.14562, D.14562
	leal	(%rbx,%rax), %r14d	#, D.14562
	leal	(%r14,%r11,2), %eax	#, D.14562
	sarl	$4, %eax	#, D.14562
	movb	%al, 7(%r15)	# D.14562, MEM[base: dst_3584, offset: 7B]
	addq	8(%rsp), %r15	# %sfp, dst
	subl	$1, 120(%rsp)	#, %sfp
	jne	.L292	#,
	jmp	.L286	#
.L416:
	movzbl	4(%r15), %edi	# MEM[base: dst_3518, offset: 4B], D.14575
	movzbl	%dil, %ecx	# D.14575, D.14562
	movzbl	3(%r15), %esi	# MEM[base: dst_3518, offset: 3B], D.14575
	movzbl	%sil, %r11d	# D.14575, D.14562
	movzbl	2(%r15), %r10d	# MEM[base: dst_3518, offset: 2B], D.14562
	movzbl	5(%r15), %ebx	# MEM[base: dst_3518, offset: 5B], D.14562
	movl	%ecx, %eax	# D.14562, D.14562
	subl	%r11d, %eax	# D.14562, D.14562
	leal	(%rax,%rax,4), %eax	#, D.14562
	movl	%r10d, %edx	# D.14562, D.14562
	subl	%ebx, %edx	# D.14562, D.14562
	leal	(%rax,%rdx,2), %r9d	#, middleEnergy
	movl	%r9d, %edx	# middleEnergy, tmp4728
	sarl	$31, %edx	#, tmp4728
	movl	%edx, %eax	# tmp4728, D.14562
	xorl	%r9d, %eax	# middleEnergy, D.14562
	subl	%edx, %eax	# tmp4728, D.14562
	cmpl	%r12d, %eax	# D.14562, D.14562
	jge	.L293	#,
	movl	%r11d, %edx	# D.14562, D.14562
	subl	%ecx, %edx	# D.14562, D.14562
	movl	%edx, %r14d	# D.14562, tmp4730
	shrl	$31, %r14d	#, tmp4730
	addl	%r14d, %edx	# tmp4730, tmp4731
	sarl	%edx	# tmp4732
	movzbl	6(%r15), %r14d	# MEM[base: dst_3518, offset: 6B], D.14562
	subl	%ebx, %r14d	# D.14562, D.14562
	leal	(%r14,%r14,4), %ebx	#, D.14562
	movzbl	7(%r15), %r14d	# MEM[base: dst_3518, offset: 7B], D.14562
	subl	%r14d, %ecx	# D.14562, D.14562
	leal	(%rbx,%rcx,2), %ecx	#, rightEnergy
	movl	%ecx, %ebx	# rightEnergy, tmp4743
	sarl	$31, %ebx	#, tmp4743
	xorl	%ebx, %ecx	# tmp4743, tmp4744
	subl	%ebx, %ecx	# tmp4743, D.14562
	movzbl	1(%r15), %ebx	# MEM[base: dst_3518, offset: 1B], D.14562
	subl	%ebx, %r10d	# D.14562, D.14562
	leal	(%r10,%r10,4), %ebx	#, D.14562
	movzbl	(%r15), %r10d	# MEM[base: dst_3518, offset: 0B], D.14562
	subl	%r11d, %r10d	# D.14562, D.14562
	leal	(%rbx,%r10,2), %r10d	#, leftEnergy
	movl	%r10d, %r11d	# leftEnergy, tmp4755
	sarl	$31, %r11d	#, tmp4755
	xorl	%r11d, %r10d	# tmp4755, tmp4756
	subl	%r11d, %r10d	# tmp4755, D.14562
	cmpl	%r10d, %ecx	# D.14562, D.14562
	cmovg	%r10d, %ecx	# D.14562,, D.14562, D.14562
	subl	%ecx, %eax	# D.14562, d
	cmovs	%r13d, %eax	# d,, tmp5186, d
	leal	32(%rax,%rax,4), %eax	#, D.14562
	sarl	$6, %eax	#, d
	movl	%eax, %ecx	# d, d
	negl	%r9d	# D.14562
	testl	%r9d, %r9d	# D.14562
	setg	%al	#, D.14562
	movzbl	%al, %eax	# D.14562, D.14562
	leal	-1(%rax,%rax), %eax	#, D.14562
	imull	%ecx, %eax	# d, d
	testl	%edx, %edx	# tmp4732
	jle	.L295	#,
	testl	%eax, %eax	# d
	cmovs	%r13d, %eax	# d,, tmp5186, d
	cmpl	%edx, %eax	# tmp4732, d
	cmovg	%edx, %eax	# d,, tmp4732, d
	jmp	.L296	#
.L295:
	testl	%eax, %eax	# d
	cmovg	%r13d, %eax	# d,, tmp5186, d
	cmpl	%edx, %eax	# tmp4732, d
	cmovl	%edx, %eax	# d,, tmp4732, d
.L296:
	subl	%eax, %esi	# d, tmp4766
	movb	%sil, 3(%r15)	# tmp4766, MEM[base: dst_3518, offset: 3B]
	addl	%edi, %eax	# D.14575, tmp4767
	movb	%al, 4(%r15)	# tmp4767, MEM[base: dst_3518, offset: 4B]
.L293:
	addq	%r8, %r15	# D.14566, dst
	subl	$1, %ebp	#, D.14563
	jne	.L416	#,
	jmp	.L286	#
.L287:
	testl	$16384, 32(%rsp)	#, %sfp
	je	.L286	#,
	movl	176(%rsp), %eax	# %sfp, D.14562
	imull	408(%rsp), %eax	# %sfp, D.14562
	sarl	$8, %eax	#, D.14562
	leal	1(%rax), %r11d	#, dcOffset
	leal	1(%r11,%r11), %r14d	#, dcThreshold
	movl	36(%rsp), %eax	# %sfp, c$QP
	leal	(%rax,%rax), %ebx	#, D.14562
	movl	%ebx, 200(%rsp)	# D.14562, %sfp
	sall	$3, %eax	#, D.14562
	movl	%eax, 180(%rsp)	# D.14562, %sfp
	movq	24(%rsp), %rax	# %sfp, dstBlock
	leaq	-2(%rax), %r15	#, ivtmp.944
	movl	$8, 136(%rsp)	#, %sfp
.L316:
	movq	%r15, %rbp	# ivtmp.944, D.14574
	movzbl	-3(%r15), %eax	# MEM[base: _790, offset: -3B], D.14562
	movl	%eax, 120(%rsp)	# D.14562, %sfp
	movzbl	-2(%r15), %edi	# MEM[base: _790, offset: -2B], D.14575
	movzbl	%dil, %r9d	# D.14575, max
	subl	%r9d, %eax	# max, D.14562
	movl	%eax, 128(%rsp)	# D.14562, %sfp
	movzbl	-1(%r15), %esi	# MEM[base: _790, offset: -1B], D.14575
	movzbl	%sil, %r12d	# D.14575, max
	addl	%r11d, %eax	# dcOffset, D.14562
	cmpl	%r14d, %eax	# dcThreshold, D.14562
	setb	%al	#, D.14581
	movzbl	%al, %eax	# D.14581, D.14581
	movl	%r9d, %ecx	# max, D.14562
	subl	%r12d, %ecx	# max, D.14562
	addl	%r11d, %ecx	# dcOffset, D.14562
	cmpl	%ecx, %r14d	# D.14562, dcThreshold
	seta	%dl	#, D.14581
	movzbl	%dl, %edx	# D.14581, D.14581
	addl	%edx, %eax	# D.14581, numEq
	movzbl	(%r15), %r8d	# MEM[base: _790, offset: 0B], D.14562
	movl	%r12d, %edx	# max, D.14562
	subl	%r8d, %edx	# D.14562, D.14562
	addl	%r11d, %edx	# dcOffset, D.14562
	cmpl	%edx, %r14d	# D.14562, dcThreshold
	seta	%dl	#, D.14581
	movzbl	%dl, %edx	# D.14581, D.14581
	addl	%edx, %eax	# D.14581, numEq
	movzbl	1(%r15), %r13d	# MEM[base: _790, offset: 1B], D.14562
	movl	%r8d, %edx	# D.14562, D.14562
	subl	%r13d, %edx	# D.14562, D.14562
	addl	%r11d, %edx	# dcOffset, D.14562
	cmpl	%edx, %r14d	# D.14562, dcThreshold
	seta	%dl	#, D.14581
	movzbl	%dl, %edx	# D.14581, D.14581
	addl	%eax, %edx	# numEq, numEq
	movzbl	2(%r15), %eax	# MEM[base: _790, offset: 2B], D.14562
	movl	%eax, %r10d	# D.14562, D.14562
	movl	%eax, 48(%rsp)	# D.14562, %sfp
	movl	%r13d, %eax	# D.14562, D.14562
	subl	%r10d, %eax	# D.14562, D.14562
	movl	%eax, 72(%rsp)	# D.14562, %sfp
	leal	(%r11,%rax), %ecx	#, D.14562
	cmpl	%ecx, %r14d	# D.14562, dcThreshold
	seta	%cl	#, D.14581
	movzbl	%cl, %ecx	# D.14581, D.14581
	addl	%ecx, %edx	# D.14581, numEq
	movzbl	3(%r15), %ebx	# MEM[base: _790, offset: 3B], D.14562
	movl	%r10d, %ecx	# D.14562, D.14562
	subl	%ebx, %ecx	# D.14562, D.14562
	addl	%r11d, %ecx	# dcOffset, D.14562
	cmpl	%ecx, %r14d	# D.14562, dcThreshold
	seta	%cl	#, D.14581
	movzbl	%cl, %ecx	# D.14581, D.14581
	addl	%ecx, %edx	# D.14581, numEq
	movzbl	4(%r15), %r10d	# MEM[base: _790, offset: 4B], D.14562
	movl	%ebx, %ecx	# D.14562, D.14562
	subl	%r10d, %ecx	# D.14562, D.14562
	addl	%r11d, %ecx	# dcOffset, D.14562
	cmpl	%ecx, %r14d	# D.14562, dcThreshold
	seta	%cl	#, D.14581
	movzbl	%cl, %ecx	# D.14581, D.14581
	addl	%ecx, %edx	# D.14581, numEq
	movzbl	5(%r15), %ecx	# MEM[base: _790, offset: 5B], D.14562
	movl	%r10d, %eax	# D.14562, D.14562
	movl	%ecx, 40(%rsp)	# D.14562, %sfp
	subl	%ecx, %eax	# D.14562, D.14562
	movl	%eax, %ecx	# D.14562, D.14562
	addl	%r11d, %ecx	# dcOffset, D.14562
	cmpl	%ecx, %r14d	# D.14562, dcThreshold
	seta	%cl	#, D.14581
	movzbl	%cl, %ecx	# D.14581, D.14581
	addl	%edx, %ecx	# numEq, numEq
	movzbl	6(%r15), %eax	# MEM[base: _790, offset: 6B], D.14562
	movl	%eax, 80(%rsp)	# D.14562, %sfp
	movl	40(%rsp), %edx	# %sfp, D.14562
	subl	%eax, %edx	# D.14562, D.14562
	addl	%r11d, %edx	# dcOffset, D.14562
	cmpl	%edx, %r14d	# D.14562, dcThreshold
	seta	%dl	#, D.14581
	movzbl	%dl, %edx	# D.14581, D.14581
	addl	%ecx, %edx	# numEq, numEq
	cmpl	60(%rsp), %edx	# %sfp, numEq
	jle	.L298	#,
	cmpb	%sil, %dil	# D.14575, D.14575
	ja	.L411	#,
	movl	%r12d, %esi	# max, max
	movl	%r9d, %edi	# max, max
	jmp	.L299	#
.L302:
	movzbl	(%rcx), %eax	# MEM[base: _3585, offset: 0B], D.14575
	movzbl	1(%rcx), %edx	# MEM[base: _3585, offset: 1B], D.14575
	cmpb	%dl, %al	# D.14575, D.14575
	jbe	.L300	#,
	movzbl	%al, %eax	# D.14575, max
	cmpl	%eax, %esi	# max, max
	cmovl	%eax, %esi	# max,, max, max
	movzbl	%dl, %edx	# D.14575, max
	cmpl	%edx, %edi	# max, max
	cmovg	%edx, %edi	# max,, max, max
	jmp	.L301	#
.L300:
	movzbl	%dl, %edx	# D.14575, max
	cmpl	%edx, %esi	# max, max
	cmovl	%edx, %esi	# max,, max, max
	movzbl	%al, %eax	# D.14575, max
	cmpl	%eax, %edi	# max, max
	cmovg	%eax, %edi	# max,, max, max
.L301:
	addq	$2, %rcx	#, ivtmp.929
	cmpq	72(%rsp), %rcx	# %sfp, ivtmp.929
	jne	.L302	#,
	subl	%edi, %esi	# max, D.14562
	cmpl	200(%rsp), %esi	# %sfp, D.14562
	jge	.L303	#,
	movl	128(%rsp), %eax	# %sfp, D.14562
	movl	%eax, %edx	# D.14562, tmp4808
	sarl	$31, %edx	#, tmp4808
	xorl	%edx, %eax	# tmp4808, tmp4809
	subl	%edx, %eax	# tmp4808, D.14562
	cmpl	36(%rsp), %eax	# %sfp, D.14562
	movl	120(%rsp), %eax	# %sfp, D.14562
	cmovge	%r9d, %eax	# D.14562,, max, D.14562
	movl	%eax, %edi	# D.14562, D.14562
	movl	80(%rsp), %esi	# %sfp, D.14562
	movl	%esi, %eax	# D.14562, D.14562
	movl	40(%rsp), %ecx	# %sfp, D.14562
	subl	%ecx, %eax	# D.14562, D.14562
	cltd
	xorl	%edx, %eax	# tmp4812, tmp4813
	subl	%edx, %eax	# tmp4812, D.14562
	cmpl	36(%rsp), %eax	# %sfp, D.14562
	movl	%esi, %eax	# D.14562, D.14562
	cmovge	%ecx, %eax	# D.14562,, D.14562, D.14562
	movl	%eax, 80(%rsp)	# D.14562, %sfp
	leal	(%r9,%rdi,4), %eax	#, D.14562
	addl	%r12d, %eax	# max, D.14562
	leal	4(%r8,%rax), %esi	#, D.14562
	movl	%esi, %edx	# D.14562, D.14562
	subl	%edi, %edx	# D.14562, D.14562
	leal	0(%r13,%rdx), %edx	#, D.14562
	movl	%edx, 72(%rsp)	# D.14562, %sfp
	subl	%edi, %edx	# D.14562, D.14562
	addl	48(%rsp), %edx	# %sfp, D.14562
	movl	%edx, %eax	# D.14562, D.14562
	subl	%edi, %eax	# D.14562, D.14562
	addl	%ebx, %eax	# D.14562, D.14562
	movl	%eax, %ecx	# D.14562, D.14562
	subl	%edi, %ecx	# D.14562, D.14562
	addl	%r10d, %ecx	# D.14562, D.14562
	movl	%ecx, %edi	# D.14562, D.14562
	subl	%r9d, %edi	# max, D.14562
	addl	40(%rsp), %edi	# %sfp, D.14562
	movl	%edi, %r9d	# D.14562, D.14562
	subl	%r12d, %r9d	# max, D.14562
	movl	80(%rsp), %r12d	# %sfp, D.14562
	movl	%r12d, %ebx	# D.14562, D.14562
	addl	%r12d, %r9d	# D.14562, D.14562
	movl	%r9d, %r10d	# D.14562, D.14562
	subl	%r8d, %r10d	# D.14562, D.14562
	movl	%r10d, %r8d	# D.14562, D.14562
	addl	%r12d, %r8d	# D.14562, D.14562
	movl	%r8d, %r10d	# D.14562, D.14562
	subl	%r13d, %r10d	# D.14562, D.14562
	addl	%r10d, %ebx	# D.14562, D.14562
	movl	%ebx, %r10d	# D.14562, D.14562
	subl	48(%rsp), %r10d	# %sfp, D.14562
	addl	%r10d, %r12d	# D.14562, D.14562
	cmpl	$0, 244(%rsp)	#, %sfp
	je	.L306	#,
	movb	$-128, 5(%rbp)	#, MEM[base: _790, offset: 5B]
	movb	$-128, 4(%rbp)	#, MEM[base: _790, offset: 4B]
	movb	$-128, 3(%rbp)	#, MEM[base: _790, offset: 3B]
	movb	$-128, 2(%rbp)	#, MEM[base: _790, offset: 2B]
	movb	$-128, 1(%rbp)	#, MEM[base: _790, offset: 1B]
	movb	$-128, 0(%rbp)	#, MEM[base: _790, offset: 0B]
	movb	$-128, -1(%rbp)	#, MEM[base: _790, offset: -1B]
	movb	$-128, -2(%rbp)	#, MEM[base: _790, offset: -2B]
.L306:
	addl	%edx, %esi	# D.14562, D.14562
	movzbl	-2(%rbp), %r10d	# MEM[base: _790, offset: -2B], D.14562
	leal	(%rsi,%r10,2), %esi	#, D.14562
	sarl	$4, %esi	#, D.14562
	movb	%sil, -2(%rbp)	# D.14562, MEM[base: _790, offset: -2B]
	movl	72(%rsp), %r13d	# %sfp, D.14562
	addl	%eax, %r13d	# D.14562, D.14562
	movzbl	-1(%rbp), %esi	# MEM[base: _790, offset: -1B], D.14562
	leal	0(%r13,%rsi,2), %esi	#, D.14562
	sarl	$4, %esi	#, D.14562
	movb	%sil, -1(%rbp)	# D.14562, MEM[base: _790, offset: -1B]
	addl	%ecx, %edx	# D.14562, D.14562
	movzbl	0(%rbp), %esi	# MEM[base: _790, offset: 0B], D.14562
	leal	(%rdx,%rsi,2), %edx	#, D.14562
	sarl	$4, %edx	#, D.14562
	movb	%dl, 0(%rbp)	# D.14562, MEM[base: _790, offset: 0B]
	addl	%edi, %eax	# D.14562, D.14562
	movzbl	1(%rbp), %edx	# MEM[base: _790, offset: 1B], D.14562
	leal	(%rax,%rdx,2), %eax	#, D.14562
	sarl	$4, %eax	#, D.14562
	movb	%al, 1(%rbp)	# D.14562, MEM[base: _790, offset: 1B]
	addl	%r9d, %ecx	# D.14562, D.14562
	movzbl	2(%rbp), %eax	# MEM[base: _790, offset: 2B], D.14562
	leal	(%rcx,%rax,2), %eax	#, D.14562
	sarl	$4, %eax	#, D.14562
	movb	%al, 2(%rbp)	# D.14562, MEM[base: _790, offset: 2B]
	addl	%r8d, %edi	# D.14562, D.14562
	movzbl	3(%rbp), %eax	# MEM[base: _790, offset: 3B], D.14562
	leal	(%rdi,%rax,2), %eax	#, D.14562
	sarl	$4, %eax	#, D.14562
	movb	%al, 3(%rbp)	# D.14562, MEM[base: _790, offset: 3B]
	addl	%ebx, %r9d	# D.14562, D.14562
	movzbl	4(%rbp), %eax	# MEM[base: _790, offset: 4B], D.14562
	leal	(%r9,%rax,2), %eax	#, D.14562
	sarl	$4, %eax	#, D.14562
	movb	%al, 4(%rbp)	# D.14562, MEM[base: _790, offset: 4B]
	addl	%r8d, %r12d	# D.14562, D.14562
	movzbl	5(%rbp), %eax	# MEM[base: _790, offset: 5B], D.14562
	leal	(%r12,%rax,2), %eax	#, D.14562
	sarl	$4, %eax	#, D.14562
	movb	%al, 5(%rbp)	# D.14562, MEM[base: _790, offset: 5B]
	jmp	.L303	#
.L298:
	movl	48(%rsp), %edx	# %sfp, D.14562
	subl	%r13d, %edx	# D.14562, D.14562
	leal	(%rdx,%rdx,4), %edx	#, D.14562
	movl	%r8d, %ecx	# D.14562, D.14562
	subl	%ebx, %ecx	# D.14562, D.14562
	leal	(%rdx,%rcx,2), %ecx	#, middleEnergy
	movl	%ecx, %esi	# middleEnergy, tmp4874
	sarl	$31, %esi	#, tmp4874
	movl	%esi, %edx	# tmp4874, D.14562
	xorl	%ecx, %edx	# middleEnergy, D.14562
	subl	%esi, %edx	# tmp4874, D.14562
	cmpl	180(%rsp), %edx	# %sfp, D.14562
	jge	.L303	#,
	movl	72(%rsp), %eax	# %sfp, D.14562
	movl	%eax, %esi	# D.14562, tmp4875
	shrl	$31, %esi	#, tmp4875
	addl	%esi, %eax	# tmp4875, tmp4876
	sarl	%eax	# tmp4877
	subl	%ebx, %r10d	# D.14562, D.14562
	leal	(%r10,%r10,4), %esi	#, D.14562
	movl	48(%rsp), %edi	# %sfp, D.14562
	subl	40(%rsp), %edi	# %sfp, D.14562
	leal	(%rsi,%rdi,2), %esi	#, rightEnergy
	movl	%esi, %edi	# rightEnergy, tmp4886
	sarl	$31, %edi	#, tmp4886
	xorl	%edi, %esi	# tmp4886, tmp4887
	subl	%edi, %esi	# tmp4886, D.14562
	subl	%r12d, %r8d	# max, D.14562
	leal	(%r8,%r8,4), %edi	#, D.14562
	subl	%r13d, %r9d	# D.14562, D.14562
	leal	(%rdi,%r9,2), %edi	#, leftEnergy
	movl	%edi, %r8d	# leftEnergy, tmp4896
	sarl	$31, %r8d	#, tmp4896
	xorl	%r8d, %edi	# tmp4896, tmp4897
	subl	%r8d, %edi	# tmp4896, D.14562
	cmpl	%edi, %esi	# D.14562, D.14562
	cmovg	%edi, %esi	# D.14562,, D.14562, D.14562
	subl	%esi, %edx	# D.14562, d
	movl	$0, %ebx	#, tmp6538
	cmovs	%ebx, %edx	# d,, tmp6538, d
	leal	32(%rdx,%rdx,4), %edx	#, D.14562
	sarl	$6, %edx	#, d
	negl	%ecx	# D.14562
	testl	%ecx, %ecx	# D.14562
	setg	%cl	#, D.14562
	movzbl	%cl, %ecx	# D.14562, D.14562
	leal	-1(%rcx,%rcx), %ecx	#, D.14562
	imull	%ecx, %edx	# D.14562, d
	testl	%eax, %eax	# tmp4877
	jle	.L308	#,
	testl	%edx, %edx	# d
	cmovs	%ebx, %edx	# d,, tmp6539, d
	cmpl	%eax, %edx	# tmp4877, d
	cmovle	%edx, %eax	# d,, d
	jmp	.L309	#
.L308:
	testl	%edx, %edx	# d
	movl	$0, %ebx	#, tmp6540
	cmovg	%ebx, %edx	# d,, tmp6540, d
	cmpl	%eax, %edx	# tmp4877, d
	cmovge	%edx, %eax	# d,, d
.L309:
	testl	%eax, %eax	# d
	je	.L310	#,
	cmpl	$0, 244(%rsp)	#, %sfp
	je	.L310	#,
	sarl	$31, %eax	#, d
	andl	$64, %eax	#, d
	subl	$32, %eax	#, d
	movl	%r13d, %edx	# D.14562, D.14562
	subl	%eax, %edx	# d, D.14562
	movl	%edx, %ecx	# D.14562, D.14582
	testl	$-256, %edx	#, D.14562
	je	.L313	#,
	negl	%edx	# D.14562
	sarl	$31, %edx	#, tmp6543
	movl	%edx, %ecx	# tmp6543, D.14582
.L313:
	movb	%cl, 1(%rbp)	# D.14582, MEM[base: _790, offset: 1B]
	addl	48(%rsp), %eax	# %sfp, D.14562
	movl	%eax, %edx	# D.14562, D.14582
	testl	$-256, %eax	#, D.14562
	je	.L315	#,
	negl	%eax	# D.14562
	cltd
.L315:
	movb	%dl, 2(%rbp)	# D.14582, MEM[base: _790, offset: 2B]
	movl	$0, %eax	#, d
.L310:
	subb	%al, 1(%rbp)	# d, MEM[base: _790, offset: 1B]
	addb	%al, 2(%rbp)	# d, MEM[base: _790, offset: 2B]
.L303:
	addq	8(%rsp), %r15	# %sfp, ivtmp.944
	subl	$1, 136(%rsp)	#, %sfp
	jne	.L316	#,
.L286:
	cmpl	$0, 204(%rsp)	#, %sfp
	jle	.L317	#,
	testb	$4, 32(%rsp)	#, %sfp
	je	.L317	#,
	movq	24(%rsp), %rax	# %sfp, D.14566
	subq	8(%rsp), %rax	# %sfp, D.14566
	leaq	-9(%rax), %r9	#, D.14565
	movl	56(%rsp), %r8d	# %sfp, dstStride
	movl	%r8d, %ebp	# dstStride, D.14563
	movl	$8, %esi	#, D.14563
	movl	$0, %ecx	#, min
	movl	$255, %edi	#, min
.L319:
	movslq	%r8d, %rdx	# ivtmp.844, D.14566
	addq	%r9, %rdx	# D.14565, p
	leaq	8(%rdx), %r10	#, D.14565
.L318:
	addq	$1, %rdx	#, p
	movzbl	(%rdx), %eax	# MEM[base: p_2529, offset: 0B], min
	cmpl	%eax, %ecx	# min, min
	cmovl	%eax, %ecx	# min,, min, min
	cmpl	%eax, %edi	# min, min
	cmovg	%eax, %edi	# min,, min, min
	cmpq	%r10, %rdx	# D.14565, p
	jne	.L318	#,
	addl	%ebp, %r8d	# D.14563, ivtmp.844
	subl	$1, %esi	#, D.14563
	jne	.L319	#,
	movl	%ecx, %eax	# min, D.14562
	subl	%edi, %eax	# min, D.14562
	cmpl	$19, %eax	#, D.14562
	jle	.L317	#,
	leal	1(%rdi,%rcx), %ecx	#, D.14562
	sarl	%ecx	# avg
	leaq	768(%rsp), %r12	#, ivtmp.804
	leaq	40(%r12), %r11	#, D.14572
	movq	%r12, %rdi	# ivtmp.804, ivtmp.826
.L330:
	movslq	%esi, %rdx	# D.14563, D.14566
	movzbl	(%r9,%rdx), %eax	# *_2546, D.14562
	cmpl	%eax, %ecx	# D.14562, avg
	setl	%al	#, t
	movzbl	%al, %eax	# t, t
	movzbl	1(%r9,%rdx), %r10d	# *_2550, D.14562
	leal	2(%rax), %r8d	#, tmp5162
	cmpl	%r10d, %ecx	# D.14562, avg
	cmovl	%r8d, %eax	# tmp5162,, t
	movzbl	2(%r9,%rdx), %r10d	# *_2556, D.14562
	leal	4(%rax), %r8d	#, tmp5172
	cmpl	%r10d, %ecx	# D.14562, avg
	cmovl	%r8d, %eax	# tmp5172,, t
	movzbl	3(%r9,%rdx), %r10d	# *_2562, D.14562
	leal	8(%rax), %r8d	#, tmp5163
	cmpl	%r10d, %ecx	# D.14562, avg
	cmovl	%r8d, %eax	# tmp5163,, t
	movzbl	4(%r9,%rdx), %r10d	# *_2568, D.14562
	leal	16(%rax), %r8d	#, tmp5176
	cmpl	%r10d, %ecx	# D.14562, avg
	cmovl	%r8d, %eax	# tmp5176,, t
	movzbl	5(%r9,%rdx), %r10d	# *_2574, D.14562
	leal	32(%rax), %r8d	#, tmp5164
	cmpl	%r10d, %ecx	# D.14562, avg
	cmovl	%r8d, %eax	# tmp5164,, t
	movzbl	6(%r9,%rdx), %r10d	# *_2580, D.14562
	leal	64(%rax), %r8d	#, tmp5173
	cmpl	%r10d, %ecx	# D.14562, avg
	cmovl	%r8d, %eax	# tmp5173,, t
	movzbl	7(%r9,%rdx), %r10d	# *_2586, D.14562
	leal	128(%rax), %r8d	#, tmp5165
	cmpl	%r10d, %ecx	# D.14562, avg
	cmovl	%r8d, %eax	# tmp5165,, t
	movzbl	8(%r9,%rdx), %r10d	# *_2592, D.14562
	leal	256(%rax), %r8d	#, tmp5178
	cmpl	%r10d, %ecx	# D.14562, avg
	cmovl	%r8d, %eax	# tmp5178,, t
	movzbl	9(%r9,%rdx), %r8d	# *_2598, D.14562
	leal	512(%rax), %edx	#, tmp5166
	cmpl	%r8d, %ecx	# D.14562, avg
	cmovl	%edx, %eax	# tmp5166,, t
	movl	%eax, %edx	# t, D.14562
	notl	%edx	# D.14562
	sall	$16, %edx	#, D.14562
	orl	%edx, %eax	# D.14562, t
	leal	(%rax,%rax), %r8d	#, D.14562
	movl	%eax, %edx	# t, D.14562
	sarl	%edx	# D.14562
	andl	%r8d, %edx	# D.14562, D.14562
	andl	%edx, %eax	# D.14562, tmp4943
	movl	%eax, (%rdi)	# tmp4943, MEM[base: _3083, offset: 0B]
	addl	%ebp, %esi	# D.14563, D.14563
	addq	$4, %rdi	#, ivtmp.826
	cmpq	%r11, %rdi	# D.14572, ivtmp.826
	jne	.L330	#,
	leaq	800(%rsp), %r14	#, D.14572
	movq	%r12, %rcx	# ivtmp.804, ivtmp.815
.L331:
	movl	4(%rcx), %eax	# MEM[base: _2686, offset: 4B], MEM[base: _2686, offset: 4B]
	andl	(%rcx), %eax	# MEM[base: _2686, offset: 0B], D.14562
	andl	8(%rcx), %eax	# MEM[base: _2686, offset: 8B], t
	movl	%eax, %edx	# t, D.14562
	sarl	$16, %edx	#, D.14562
	orl	%edx, %eax	# D.14562, tmp4947
	movl	%eax, (%rcx)	# tmp4947, MEM[base: _2686, offset: 0B]
	addq	$4, %rcx	#, ivtmp.815
	cmpq	%r14, %rcx	# D.14572, ivtmp.815
	jne	.L331	#,
	movl	36(%rsp), %eax	# %sfp, c$QP
	movl	%eax, %r11d	# c$QP, tmp4949
	shrl	$31, %r11d	#, tmp4949
	addl	%eax, %r11d	# c$QP, tmp4950
	sarl	%r11d	# D.14562
	addl	$1, %r11d	#, QP2
	movl	56(%rsp), %r13d	# %sfp, ivtmp.806
	movq	8(%rsp), %r10	# %sfp, D.14566
	negq	%r10	# D.14566
.L336:
	movl	(%r12), %ebx	# MEM[base: _2327, offset: 0B], t
	movslq	%r13d, %rax	# ivtmp.806, D.14566
	addq	%r9, %rax	# D.14565, p
	movq	8(%rsp), %rsi	# %sfp, D.14566
	leaq	(%rax,%rsi), %rcx	#, ivtmp.794
	movl	$1, %edx	#, x
.L335:
	addq	$1, %rax	#, p
	btl	%edx, %ebx	# x, t
	jnc	.L332	#,
	movzbl	(%rax), %r8d	# MEM[base: p_2629, offset: 0B], D.14575
	movzbl	%r8b, %edi	# D.14575, D.14562
	movzbl	-1(%rax,%r10), %esi	# MEM[base: p_2629, index: _800, offset: -1B], D.14562
	movzbl	(%rax,%r10), %r15d	# MEM[base: p_2629, index: _800, offset: 0B], D.14562
	leal	(%rsi,%r15,2), %r15d	#, D.14562
	movzbl	1(%rax,%r10), %esi	# MEM[base: p_2629, index: _800, offset: 1B], D.14562
	addl	%r15d, %esi	# D.14562, D.14562
	movzbl	-1(%rax), %r15d	# MEM[base: p_2629, offset: -1B], D.14562
	leal	(%rsi,%r15,2), %esi	#, D.14562
	leal	(%rsi,%rdi,4), %esi	#, D.14562
	movzbl	1(%rax), %r15d	# MEM[base: p_2629, offset: 1B], D.14562
	leal	(%rsi,%r15,2), %r15d	#, D.14562
	movzbl	(%rcx), %esi	# MEM[base: _3292, offset: 0B], D.14562
	addl	%r15d, %esi	# D.14562, D.14562
	movzbl	1(%rcx), %r15d	# MEM[base: _3292, offset: 1B], D.14562
	leal	(%rsi,%r15,2), %r15d	#, D.14562
	movzbl	2(%rcx), %esi	# MEM[base: _3292, offset: 2B], D.14562
	leal	8(%r15,%rsi), %esi	#, D.14562
	sarl	$4, %esi	#, f
	leal	(%r11,%rdi), %r15d	#, D.14562
	cmpl	%r15d, %esi	# D.14562, f
	jle	.L333	#,
	addl	%r11d, %r8d	# QP2, tmp4977
	movb	%r8b, (%rax)	# tmp4977, MEM[base: p_2629, offset: 0B]
	jmp	.L332	#
.L333:
	subl	%r11d, %edi	# QP2, D.14562
	cmpl	%edi, %esi	# D.14562, f
	jge	.L334	#,
	subl	%r11d, %r8d	# QP2, tmp4979
	movb	%r8b, (%rax)	# tmp4979, MEM[base: p_2629, offset: 0B]
	jmp	.L332	#
.L334:
	movb	%sil, (%rax)	# f, MEM[base: p_2629, offset: 0B]
.L332:
	addl	$1, %edx	#, x
	addq	$1, %rcx	#, ivtmp.794
	cmpl	$9, %edx	#, x
	jne	.L335	#,
	addq	$4, %r12	#, ivtmp.804
	addl	%ebp, %r13d	# D.14563, ivtmp.806
	cmpq	%r14, %r12	# D.14572, ivtmp.804
	jne	.L336	#,
.L317:
	testl	$1048576, 32(%rsp)	#, %sfp
	je	.L277	#,
	movslq	2160(%rsp), %rdx	# isColor, isColor
	movl	16(%rsp), %ebx	# %sfp, x
	movl	%ebx, %eax	# x, D.14562
	sarl	$3, %eax	#, D.14562
	cltq
	addq	496(%rsp), %rax	# %sfp, D.14566
	movq	880(%rsp,%rdx,8), %rcx	# c.tempBlurredPast, tmp4987
	leaq	(%rcx,%rax,4), %rbp	#, D.14580
	movslq	%ebx, %rsi	# x, D.14566
	addq	488(%rsp), %rsi	# %sfp, D.14566
	addq	856(%rsp,%rdx,8), %rsi	# c.tempBlurred, D.14565
	movq	24(%rsp), %rax	# %sfp, dstBlock
	leaq	-8(%rax), %r8	#, D.14565
	movl	460(%rsp), %eax	# %sfp, c$1232
	movl	%eax, 508(%rbp)	# c$1232, MEM[(uint32_t *)_378 + 508B]
	movl	420(%rsp), %eax	# %sfp, c$1236
	movl	%eax, 512(%rbp)	# c$1236, MEM[(uint32_t *)_378 + 512B]
	movl	456(%rsp), %eax	# %sfp, c$1240
	movl	%eax, 516(%rbp)	# c$1240, MEM[(uint32_t *)_378 + 516B]
	movl	56(%rsp), %ebx	# %sfp, D.14563
	movl	$0, %r11d	#, ivtmp.779
	movl	$8, %r10d	#, D.14563
	movl	$0, %edi	#, d
	jmp	.L337	#
.L338:
	movslq	%edx, %rcx	# ivtmp.772, D.14566
	movzbl	(%rsi,%rcx), %eax	# *_2702, ref
	movzbl	(%r8,%rcx), %ecx	# *_2705, cur
	subl	%ecx, %eax	# cur, d1
	imull	%eax, %eax	# d1, D.14562
	addl	%eax, %edi	# D.14562, d
	addl	$1, %edx	#, ivtmp.772
	cmpl	%r9d, %edx	# D.14563, ivtmp.772
	jne	.L338	#,
	addl	%ebx, %r11d	# D.14563, ivtmp.779
	subl	$1, %r10d	#, D.14563
	je	.L339	#,
.L337:
	leal	8(%r11), %r9d	#, D.14563
	movl	%r11d, %edx	# ivtmp.779, ivtmp.772
	jmp	.L338	#
.L339:
	movl	-4(%rbp), %eax	# MEM[(uint32_t *)_378 + -4B], MEM[(uint32_t *)_378 + -4B]
	addl	-1024(%rbp), %eax	# MEM[(uint32_t *)_378 + -1024B], D.14564
	movl	4(%rbp), %edx	# MEM[(uint32_t *)_378 + 4B], MEM[(uint32_t *)_378 + 4B]
	leal	4(%rax,%rdx), %eax	#, D.14564
	addl	1024(%rbp), %eax	# MEM[(uint32_t *)_378 + 1024B], D.14564
	leal	(%rax,%rdi,4), %eax	#, D.14564
	shrl	$3, %eax	#, D.14564
	movl	%edi, 0(%rbp)	# d, MEM[(uint32_t *)_378]
	cmpl	420(%rsp), %eax	# %sfp, D.14564
	jle	.L340	#,
	movl	$8, %r9d	#, D.14563
	movl	$8, %ebp	#, D.14563
	cmpl	456(%rsp), %eax	# %sfp, D.14564
	jl	.L343	#,
	jmp	.L342	#
.L344:
	movslq	%ecx, %rax	# ivtmp.712, D.14566
	leaq	(%rsi,%rax), %rdi	#, D.14565
	addq	%r8, %rax	# D.14565, D.14565
	movzbl	(%rdi), %r9d	# *_2737, ref
	movzbl	(%rax), %edx	# *_2740, cur
	leal	1(%r9,%rdx), %edx	#, D.14562
	sarl	%edx	# D.14562
	movb	%dl, (%rax)	# D.14562, *_2740
	movb	%dl, (%rdi)	# D.14562, *_2737
	addl	$1, %ecx	#, ivtmp.712
	cmpl	%r11d, %ecx	# D.14563, ivtmp.712
	jne	.L344	#,
	addl	%ebx, %r10d	# D.14563, D.14563
	subl	$1, %ebp	#, D.14563
	je	.L277	#,
.L343:
	leal	8(%r10), %r11d	#, D.14563
	movl	%r10d, %ecx	# D.14563, ivtmp.712
	jmp	.L344	#
.L345:
	movslq	%eax, %rdx	# ivtmp.727, D.14566
	movzbl	(%r8,%rdx), %ecx	# *_2755, D.14575
	movb	%cl, (%rsi,%rdx)	# D.14575, *_2754
	addl	$1, %eax	#, ivtmp.727
	cmpl	%edi, %eax	# D.14563, ivtmp.727
	jne	.L345	#,
	addl	%ebx, %r10d	# D.14563, D.14563
	subl	$1, %r9d	#, D.14563
	je	.L277	#,
.L342:
	leal	8(%r10), %edi	#, D.14563
	movl	%r10d, %eax	# D.14563, ivtmp.727
	jmp	.L345	#
.L340:
	movl	$8, %ebp	#, D.14563
	cmpl	460(%rsp), %eax	# %sfp, D.14564
	jge	.L347	#,
.L346:
	movl	$8, %ebp	#, D.14563
	jmp	.L348	#
.L349:
	movslq	%ecx, %rdx	# ivtmp.742, D.14566
	leaq	(%rsi,%rdx), %rdi	#, D.14565
	addq	%r8, %rdx	# D.14565, D.14565
	movzbl	(%rdx), %eax	# *_2768, cur
	movzbl	(%rdi), %r9d	# *_2765, ref
	leal	(%rax,%r9,8), %eax	#, D.14562
	subl	%r9d, %eax	# ref, D.14562
	addl	$4, %eax	#, D.14562
	sarl	$3, %eax	#, D.14562
	movb	%al, (%rdx)	# D.14562, *_2768
	movb	%al, (%rdi)	# D.14562, *_2765
	addl	$1, %ecx	#, ivtmp.742
	cmpl	%r11d, %ecx	# D.14563, ivtmp.742
	jne	.L349	#,
	addl	%ebx, %r10d	# D.14563, D.14563
	subl	$1, %ebp	#, D.14563
	je	.L277	#,
.L348:
	leal	8(%r10), %r11d	#, D.14563
	movl	%r10d, %ecx	# D.14563, ivtmp.742
	jmp	.L349	#
.L350:
	movslq	%ecx, %rax	# ivtmp.757, D.14566
	leaq	(%rsi,%rax), %rdi	#, D.14565
	addq	%r8, %rax	# D.14565, D.14565
	movzbl	(%rax), %r9d	# *_2786, cur
	movzbl	(%rdi), %edx	# *_2783, ref
	leal	(%rdx,%rdx,2), %edx	#, D.14562
	leal	2(%r9,%rdx), %edx	#, D.14562
	sarl	$2, %edx	#, D.14562
	movb	%dl, (%rax)	# D.14562, *_2786
	movb	%dl, (%rdi)	# D.14562, *_2783
	addl	$1, %ecx	#, ivtmp.757
	cmpl	%r11d, %ecx	# D.14563, ivtmp.757
	jne	.L350	#,
	addl	%ebx, %r10d	# D.14563, D.14563
	subl	$1, %ebp	#, D.14563
	je	.L277	#,
.L347:
	leal	8(%r10), %r11d	#, D.14563
	movl	%r10d, %ecx	# D.14563, ivtmp.757
	jmp	.L350	#
.L277:
	addq	$8, 24(%rsp)	#, %sfp
	addq	$8, 104(%rsp)	#, %sfp
	addl	$8, 16(%rsp)	#, %sfp
	movl	16(%rsp), %eax	# %sfp, x
	addq	$8, 144(%rsp)	#, %sfp
	addq	$8, 256(%rsp)	#, %sfp
	addq	$8, 248(%rsp)	#, %sfp
	addq	$8, 336(%rsp)	#, %sfp
	addq	$8, 344(%rsp)	#, %sfp
	addq	$8, 264(%rsp)	#, %sfp
	addq	$8, 272(%rsp)	#, %sfp
	addq	$8, 232(%rsp)	#, %sfp
	addq	$8, 352(%rsp)	#, %sfp
	addq	$8, 280(%rsp)	#, %sfp
	addq	$8, 288(%rsp)	#, %sfp
	addq	$8, 296(%rsp)	#, %sfp
	addq	$8, 304(%rsp)	#, %sfp
	addq	$8, 312(%rsp)	#, %sfp
	addq	$8, 320(%rsp)	#, %sfp
	addq	$8, 328(%rsp)	#, %sfp
	cmpl	%eax, 224(%rsp)	# x, %sfp
	jg	.L351	#,
.L201:
	cmpl	$0, 204(%rsp)	#, %sfp
	jle	.L352	#,
	testb	$4, 32(%rsp)	#, %sfp
	je	.L352	#,
	movq	24(%rsp), %rax	# %sfp, D.14566
	subq	8(%rsp), %rax	# %sfp, D.14566
	leaq	-9(%rax), %rbp	#, src
	movl	56(%rsp), %r8d	# %sfp, dstStride
	movl	%r8d, %r13d	# dstStride, D.14563
	movl	$8, %esi	#, D.14563
	movl	$0, %ecx	#, min
	movl	$255, %edi	#, min
.L354:
	movslq	%r8d, %rdx	# ivtmp.704, D.14566
	addq	%rbp, %rdx	# src, p
	leaq	8(%rdx), %r9	#, D.14565
.L353:
	addq	$1, %rdx	#, p
	movzbl	(%rdx), %eax	# MEM[base: p_2814, offset: 0B], min
	cmpl	%eax, %ecx	# min, min
	cmovl	%eax, %ecx	# min,, min, min
	cmpl	%eax, %edi	# min, min
	cmovg	%eax, %edi	# min,, min, min
	cmpq	%r9, %rdx	# D.14565, p
	jne	.L353	#,
	addl	%r13d, %r8d	# D.14563, ivtmp.704
	subl	$1, %esi	#, D.14563
	jne	.L354	#,
	movl	%ecx, %eax	# min, D.14562
	subl	%edi, %eax	# min, D.14562
	cmpl	$19, %eax	#, D.14562
	jle	.L352	#,
	leal	1(%rdi,%rcx), %ecx	#, D.14562
	sarl	%ecx	# avg
	leaq	768(%rsp), %r12	#, ivtmp.664
	leaq	40(%r12), %r10	#, D.14572
	movq	%r12, %rdi	# ivtmp.664, ivtmp.686
.L365:
	movslq	%esi, %rdx	# D.14563, D.14566
	movzbl	0(%rbp,%rdx), %eax	# *_2831, D.14562
	cmpl	%eax, %ecx	# D.14562, avg
	setl	%al	#, t
	movzbl	%al, %eax	# t, t
	movzbl	1(%rbp,%rdx), %r9d	# *_2835, D.14562
	leal	2(%rax), %r8d	#, tmp5167
	cmpl	%r9d, %ecx	# D.14562, avg
	cmovl	%r8d, %eax	# tmp5167,, t
	movzbl	2(%rbp,%rdx), %r9d	# *_2841, D.14562
	leal	4(%rax), %r8d	#, tmp5174
	cmpl	%r9d, %ecx	# D.14562, avg
	cmovl	%r8d, %eax	# tmp5174,, t
	movzbl	3(%rbp,%rdx), %r9d	# *_2847, D.14562
	leal	8(%rax), %r8d	#, tmp5168
	cmpl	%r9d, %ecx	# D.14562, avg
	cmovl	%r8d, %eax	# tmp5168,, t
	movzbl	4(%rbp,%rdx), %r9d	# *_2853, D.14562
	leal	16(%rax), %r8d	#, tmp5177
	cmpl	%r9d, %ecx	# D.14562, avg
	cmovl	%r8d, %eax	# tmp5177,, t
	movzbl	5(%rbp,%rdx), %r9d	# *_2859, D.14562
	leal	32(%rax), %r8d	#, tmp5169
	cmpl	%r9d, %ecx	# D.14562, avg
	cmovl	%r8d, %eax	# tmp5169,, t
	movzbl	6(%rbp,%rdx), %r9d	# *_2865, D.14562
	leal	64(%rax), %r8d	#, tmp5175
	cmpl	%r9d, %ecx	# D.14562, avg
	cmovl	%r8d, %eax	# tmp5175,, t
	movzbl	7(%rbp,%rdx), %r9d	# *_2871, D.14562
	leal	128(%rax), %r8d	#, tmp5170
	cmpl	%r9d, %ecx	# D.14562, avg
	cmovl	%r8d, %eax	# tmp5170,, t
	movzbl	8(%rbp,%rdx), %r9d	# *_2877, D.14562
	leal	256(%rax), %r8d	#, tmp5179
	cmpl	%r9d, %ecx	# D.14562, avg
	cmovl	%r8d, %eax	# tmp5179,, t
	movzbl	9(%rbp,%rdx), %r8d	# *_2883, D.14562
	leal	512(%rax), %edx	#, tmp5171
	cmpl	%r8d, %ecx	# D.14562, avg
	cmovl	%edx, %eax	# tmp5171,, t
	movl	%eax, %edx	# t, D.14562
	notl	%edx	# D.14562
	sall	$16, %edx	#, D.14562
	orl	%edx, %eax	# D.14562, t
	leal	(%rax,%rax), %r8d	#, D.14562
	movl	%eax, %edx	# t, D.14562
	sarl	%edx	# D.14562
	andl	%r8d, %edx	# D.14562, D.14562
	andl	%edx, %eax	# D.14562, tmp5053
	movl	%eax, (%rdi)	# tmp5053, MEM[base: _2338, offset: 0B]
	addl	%r13d, %esi	# D.14563, D.14563
	addq	$4, %rdi	#, ivtmp.686
	cmpq	%r10, %rdi	# D.14572, ivtmp.686
	jne	.L365	#,
	leaq	800(%rsp), %r15	#, D.14572
	movq	%r12, %rcx	# ivtmp.664, ivtmp.675
.L366:
	movl	4(%rcx), %eax	# MEM[base: _2908, offset: 4B], MEM[base: _2908, offset: 4B]
	andl	(%rcx), %eax	# MEM[base: _2908, offset: 0B], D.14562
	andl	8(%rcx), %eax	# MEM[base: _2908, offset: 8B], t
	movl	%eax, %edx	# t, D.14562
	sarl	$16, %edx	#, D.14562
	orl	%edx, %eax	# D.14562, tmp5057
	movl	%eax, (%rcx)	# tmp5057, MEM[base: _2908, offset: 0B]
	addq	$4, %rcx	#, ivtmp.675
	cmpq	%r15, %rcx	# D.14572, ivtmp.675
	jne	.L366	#,
	movl	36(%rsp), %eax	# %sfp, c$QP
	movl	%eax, %r10d	# c$QP, tmp5059
	shrl	$31, %r10d	#, tmp5059
	addl	%eax, %r10d	# c$QP, tmp5060
	sarl	%r10d	# D.14562
	addl	$1, %r10d	#, QP2
	movl	56(%rsp), %r14d	# %sfp, ivtmp.666
	movq	8(%rsp), %r9	# %sfp, D.14566
	negq	%r9	# D.14566
.L371:
	movl	(%r12), %r11d	# MEM[base: _352, offset: 0B], t
	movslq	%r14d, %rax	# ivtmp.666, D.14566
	addq	%rbp, %rax	# src, p
	movq	8(%rsp), %rbx	# %sfp, D.14566
	leaq	(%rax,%rbx), %rcx	#, ivtmp.654
	movl	$1, %edx	#, x
.L370:
	addq	$1, %rax	#, p
	btl	%edx, %r11d	# x, t
	jnc	.L367	#,
	movzbl	(%rax), %r8d	# MEM[base: p_2914, offset: 0B], D.14575
	movzbl	%r8b, %edi	# D.14575, D.14562
	movzbl	-1(%rax,%r9), %esi	# MEM[base: p_2914, index: _2343, offset: -1B], D.14562
	movzbl	(%rax,%r9), %ebx	# MEM[base: p_2914, index: _2343, offset: 0B], D.14562
	leal	(%rsi,%rbx,2), %ebx	#, D.14562
	movzbl	1(%rax,%r9), %esi	# MEM[base: p_2914, index: _2343, offset: 1B], D.14562
	addl	%ebx, %esi	# D.14562, D.14562
	movzbl	-1(%rax), %ebx	# MEM[base: p_2914, offset: -1B], D.14562
	leal	(%rsi,%rbx,2), %esi	#, D.14562
	leal	(%rsi,%rdi,4), %esi	#, D.14562
	movzbl	1(%rax), %ebx	# MEM[base: p_2914, offset: 1B], D.14562
	leal	(%rsi,%rbx,2), %ebx	#, D.14562
	movzbl	(%rcx), %esi	# MEM[base: _2365, offset: 0B], D.14562
	addl	%ebx, %esi	# D.14562, D.14562
	movzbl	1(%rcx), %ebx	# MEM[base: _2365, offset: 1B], D.14562
	leal	(%rsi,%rbx,2), %ebx	#, D.14562
	movzbl	2(%rcx), %esi	# MEM[base: _2365, offset: 2B], D.14562
	leal	8(%rbx,%rsi), %esi	#, D.14562
	sarl	$4, %esi	#, f
	leal	(%r10,%rdi), %ebx	#, D.14562
	cmpl	%ebx, %esi	# D.14562, f
	jle	.L368	#,
	addl	%r10d, %r8d	# QP2, tmp5087
	movb	%r8b, (%rax)	# tmp5087, MEM[base: p_2914, offset: 0B]
	jmp	.L367	#
.L368:
	subl	%r10d, %edi	# QP2, D.14562
	cmpl	%edi, %esi	# D.14562, f
	jge	.L369	#,
	subl	%r10d, %r8d	# QP2, tmp5089
	movb	%r8b, (%rax)	# tmp5089, MEM[base: p_2914, offset: 0B]
	jmp	.L367	#
.L369:
	movb	%sil, (%rax)	# f, MEM[base: p_2914, offset: 0B]
.L367:
	addl	$1, %edx	#, x
	addq	$1, %rcx	#, ivtmp.654
	cmpl	$9, %edx	#, x
	jne	.L370	#,
	addq	$4, %r12	#, ivtmp.664
	addl	%r13d, %r14d	# D.14563, ivtmp.666
	cmpq	%r12, %r15	# ivtmp.664, D.14572
	jne	.L371	#,
.L352:
	testl	$1048576, 32(%rsp)	#, %sfp
	je	.L372	#,
	movslq	2160(%rsp), %rcx	# isColor, isColor
	movl	204(%rsp), %eax	# %sfp, D.14562
	sarl	$3, %eax	#, D.14562
	sall	$8, %eax	#, D.14562
	cltq
	movl	16(%rsp), %esi	# %sfp, x
	movl	%esi, %edx	# x, D.14562
	sarl	$3, %edx	#, D.14562
	movslq	%edx, %rdx	# D.14562, D.14566
	leaq	256(%rax,%rdx), %rdx	#, D.14566
	movq	880(%rsp,%rcx,8), %rax	# c.tempBlurredPast, tmp5101
	leaq	(%rax,%rdx,4), %rbx	#, D.14580
	movslq	%esi, %rax	# x, D.14566
	addq	488(%rsp), %rax	# %sfp, D.14566
	addq	856(%rsp,%rcx,8), %rax	# c.tempBlurred, D.14565
	movq	%rax, %rsi	# D.14565, D.14565
	movq	24(%rsp), %r8	# %sfp, D.14565
	subq	$8, %r8	#, D.14565
	movl	460(%rsp), %eax	# %sfp, c$1232
	movl	%eax, 508(%rbx)	# c$1232, MEM[(uint32_t *)_402 + 508B]
	movl	420(%rsp), %eax	# %sfp, c$1236
	movl	%eax, 512(%rbx)	# c$1236, MEM[(uint32_t *)_402 + 512B]
	movl	456(%rsp), %eax	# %sfp, c$1240
	movl	%eax, 516(%rbx)	# c$1240, MEM[(uint32_t *)_402 + 516B]
	movl	56(%rsp), %ebp	# %sfp, D.14563
	movl	$0, %r11d	#, ivtmp.639
	movl	$8, %r10d	#, D.14563
	movl	$0, %edi	#, d
	jmp	.L373	#
.L374:
	movslq	%edx, %rcx	# ivtmp.632, D.14566
	movzbl	(%rsi,%rcx), %eax	# *_2987, ref
	movzbl	(%r8,%rcx), %ecx	# *_2990, cur
	subl	%ecx, %eax	# cur, d1
	imull	%eax, %eax	# d1, D.14562
	addl	%eax, %edi	# D.14562, d
	addl	$1, %edx	#, ivtmp.632
	cmpl	%r9d, %edx	# D.14563, ivtmp.632
	jne	.L374	#,
	addl	%ebp, %r11d	# D.14563, ivtmp.639
	subl	$1, %r10d	#, D.14563
	je	.L375	#,
.L373:
	leal	8(%r11), %r9d	#, D.14563
	movl	%r11d, %edx	# ivtmp.639, ivtmp.632
	jmp	.L374	#
.L375:
	movl	-4(%rbx), %eax	# MEM[(uint32_t *)_402 + -4B], MEM[(uint32_t *)_402 + -4B]
	addl	-1024(%rbx), %eax	# MEM[(uint32_t *)_402 + -1024B], D.14564
	movl	4(%rbx), %edx	# MEM[(uint32_t *)_402 + 4B], MEM[(uint32_t *)_402 + 4B]
	leal	4(%rax,%rdx), %eax	#, D.14564
	addl	1024(%rbx), %eax	# MEM[(uint32_t *)_402 + 1024B], D.14564
	leal	(%rax,%rdi,4), %eax	#, D.14564
	shrl	$3, %eax	#, D.14564
	movl	%edi, (%rbx)	# d, MEM[(uint32_t *)_402]
	cmpl	420(%rsp), %eax	# %sfp, D.14564
	jle	.L376	#,
	movl	$8, %r9d	#, D.14563
	movl	$8, %ebx	#, D.14563
	cmpl	456(%rsp), %eax	# %sfp, D.14564
	jl	.L379	#,
	jmp	.L378	#
.L380:
	movslq	%ecx, %rax	# ivtmp.572, D.14566
	leaq	(%rsi,%rax), %rdi	#, D.14565
	addq	%r8, %rax	# D.14565, D.14565
	movzbl	(%rdi), %r9d	# *_3022, ref
	movzbl	(%rax), %edx	# *_3025, cur
	leal	1(%r9,%rdx), %edx	#, D.14562
	sarl	%edx	# D.14562
	movb	%dl, (%rax)	# D.14562, *_3025
	movb	%dl, (%rdi)	# D.14562, *_3022
	addl	$1, %ecx	#, ivtmp.572
	cmpl	%r11d, %ecx	# D.14563, ivtmp.572
	jne	.L380	#,
	addl	%ebp, %r10d	# D.14563, D.14563
	subl	$1, %ebx	#, D.14563
	je	.L372	#,
.L379:
	leal	8(%r10), %r11d	#, D.14563
	movl	%r10d, %ecx	# D.14563, ivtmp.572
	jmp	.L380	#
.L381:
	movslq	%eax, %rdx	# ivtmp.587, D.14566
	movzbl	(%r8,%rdx), %ecx	# *_3040, D.14575
	movb	%cl, (%rsi,%rdx)	# D.14575, *_3039
	addl	$1, %eax	#, ivtmp.587
	cmpl	%edi, %eax	# D.14563, ivtmp.587
	jne	.L381	#,
	addl	%ebp, %r10d	# D.14563, D.14563
	subl	$1, %r9d	#, D.14563
	je	.L372	#,
.L378:
	leal	8(%r10), %edi	#, D.14563
	movl	%r10d, %eax	# D.14563, ivtmp.587
	jmp	.L381	#
.L376:
	movl	$8, %ebx	#, D.14563
	cmpl	460(%rsp), %eax	# %sfp, D.14564
	jge	.L383	#,
.L382:
	movl	$8, %ebx	#, D.14563
	jmp	.L384	#
.L385:
	movslq	%ecx, %rdx	# ivtmp.602, D.14566
	leaq	(%rsi,%rdx), %rdi	#, D.14565
	addq	%r8, %rdx	# D.14565, D.14565
	movzbl	(%rdx), %eax	# *_3053, cur
	movzbl	(%rdi), %r9d	# *_3050, ref
	leal	(%rax,%r9,8), %eax	#, D.14562
	subl	%r9d, %eax	# ref, D.14562
	addl	$4, %eax	#, D.14562
	sarl	$3, %eax	#, D.14562
	movb	%al, (%rdx)	# D.14562, *_3053
	movb	%al, (%rdi)	# D.14562, *_3050
	addl	$1, %ecx	#, ivtmp.602
	cmpl	%r11d, %ecx	# D.14563, ivtmp.602
	jne	.L385	#,
	addl	%ebp, %r10d	# D.14563, D.14563
	subl	$1, %ebx	#, D.14563
	je	.L372	#,
.L384:
	leal	8(%r10), %r11d	#, D.14563
	movl	%r10d, %ecx	# D.14563, ivtmp.602
	jmp	.L385	#
.L386:
	movslq	%ecx, %rax	# ivtmp.617, D.14566
	leaq	(%rsi,%rax), %rdi	#, D.14565
	addq	%r8, %rax	# D.14565, D.14565
	movzbl	(%rax), %r9d	# *_3071, cur
	movzbl	(%rdi), %edx	# *_3068, ref
	leal	(%rdx,%rdx,2), %edx	#, D.14562
	leal	2(%r9,%rdx), %edx	#, D.14562
	sarl	$2, %edx	#, D.14562
	movb	%dl, (%rax)	# D.14562, *_3071
	movb	%dl, (%rdi)	# D.14562, *_3068
	addl	$1, %ecx	#, ivtmp.617
	cmpl	%r11d, %ecx	# D.14563, ivtmp.617
	jne	.L386	#,
	addl	%ebp, %r10d	# D.14563, D.14563
	subl	$1, %ebx	#, D.14563
	je	.L372	#,
.L383:
	leal	8(%r10), %r11d	#, D.14563
	movl	%r10d, %ecx	# D.14563, ivtmp.617
	jmp	.L386	#
.L372:
	movl	664(%rsp), %ebx	# %sfp, D.14562
	cmpl	%ebx, 228(%rsp)	# D.14562, %sfp
	jg	.L387	#,
	movl	224(%rsp), %ebx	# %sfp, width
	movl	668(%rsp), %esi	# %sfp, D.14562
	cmpl	%esi, %ebx	# D.14562, width
	je	.L388	#,
	movl	548(%rsp), %esi	# %sfp, ivtmp.1273
	movl	%esi, %r14d	# ivtmp.1273, D.14562
	movl	56(%rsp), %r13d	# %sfp, D.14563
	movl	$0, %eax	#, ivtmp.565
	movl	$0, %ebp	#, i
	movslq	%ebx, %r12	# width, D.14567
	testl	%esi, %esi	# ivtmp.1273
	jg	.L415	#,
	jmp	.L387	#
.L388:
	movl	56(%rsp), %ecx	# %sfp,
	movl	548(%rsp), %edx	# %sfp,
	movq	520(%rsp), %rsi	# %sfp,
	movq	560(%rsp), %rdi	# %sfp,
	call	linecpy	#
	jmp	.L387	#
.L415:
	addl	$1, %ebp	#, i
	leal	0(%r13,%rax), %ebx	#, D.14563
	cltq
	movq	560(%rsp), %rsi	# %sfp, dstBlock
	leaq	(%rsi,%rax), %rdi	#, D.14565
	movslq	%ebx, %rsi	# D.14563, D.14566
	addq	504(%rsp), %rsi	# %sfp, D.14565
	movq	%r12, %rdx	# D.14567,
	call	memcpy	#
	movl	%ebx, %eax	# D.14563, ivtmp.565
	cmpl	%r14d, %ebp	# D.14562, i
	jne	.L415	#,
.L387:
	addl	$8, 204(%rsp)	#, %sfp
	movl	204(%rsp), %eax	# %sfp, y
	movl	692(%rsp), %esi	# %sfp, D.14563
	addl	%esi, 608(%rsp)	# D.14563, %sfp
	movl	688(%rsp), %esi	# %sfp, D.14563
	addl	%esi, 580(%rsp)	# D.14563, %sfp
	subl	$8, 548(%rsp)	#, %sfp
	subl	$8, 628(%rsp)	#, %sfp
	movl	724(%rsp), %esi	# %sfp, D.14563
	addl	%esi, 644(%rsp)	# D.14563, %sfp
	cmpl	%eax, 228(%rsp)	# y, %sfp
	jg	.L391	#,
.L197:
	movq	424(%rsp), %rax	# %sfp, c$yHistogram
	movq	%rax, 832(%rsp)	# c$yHistogram, MEM[(struct PPContext *)&c + 16B]
	movq	648(%rsp), %rax	# %sfp, c$packedYOffset
	movq	%rax, 840(%rsp)	# c$packedYOffset, MEM[(struct PPContext *)&c + 24B]
	movq	656(%rsp), %rax	# %sfp, c$packedYScale
	movq	%rax, 848(%rsp)	# c$packedYScale, MEM[(struct PPContext *)&c + 32B]
	movq	616(%rsp), %rax	# %sfp, c$tempDst
	movq	%rax, 904(%rsp)	# c$tempDst, MEM[(struct PPContext *)&c + 88B]
	movq	592(%rsp), %rax	# %sfp, srcBlock
	movq	%rax, 912(%rsp)	# srcBlock, MEM[(struct PPContext *)&c + 96B]
	movq	472(%rsp), %rax	# %sfp, c$deintTemp
	movq	%rax, 920(%rsp)	# c$deintTemp, MEM[(struct PPContext *)&c + 104B]
	movq	600(%rsp), %rax	# %sfp, c$nonBQPTable
	movq	%rax, 1976(%rsp)	# c$nonBQPTable, MEM[(struct PPContext *)&c + 1160B]
	movl	36(%rsp), %eax	# %sfp, c$QP
	movl	%eax, 1992(%rsp)	# c$QP, MEM[(struct PPContext *)&c + 1176B]
	movl	176(%rsp), %eax	# %sfp, c$nonBQP
	movl	%eax, 1996(%rsp)	# c$nonBQP, MEM[(struct PPContext *)&c + 1180B]
	movl	576(%rsp), %eax	# %sfp, c$frameNum
	movl	%eax, 2000(%rsp)	# c$frameNum, MEM[(struct PPContext *)&c + 1184B]
	movl	720(%rsp), %eax	# %sfp, c$hChromaSubSample
	movl	%eax, 2016(%rsp)	# c$hChromaSubSample, MEM[(struct PPContext *)&c + 1200B]
	movl	708(%rsp), %eax	# %sfp, c$vChromaSubSample
	movl	%eax, 2020(%rsp)	# c$vChromaSubSample, MEM[(struct PPContext *)&c + 1204B]
	movl	696(%rsp), %eax	# %sfp, c$ppMode$lumMode
	movl	%eax, 2024(%rsp)	# c$ppMode$lumMode, MEM[(struct PPContext *)&c + 1208B]
	movl	704(%rsp), %eax	# %sfp, c$ppMode$chromMode
	movl	%eax, 2028(%rsp)	# c$ppMode$chromMode, MEM[(struct PPContext *)&c + 1212B]
	movl	700(%rsp), %eax	# %sfp, c$ppMode$minAllowedY
	movl	%eax, 2036(%rsp)	# c$ppMode$minAllowedY, MEM[(struct PPContext *)&c + 1220B]
	movl	716(%rsp), %eax	# %sfp, c$ppMode$maxAllowedY
	movl	%eax, 2040(%rsp)	# c$ppMode$maxAllowedY, MEM[(struct PPContext *)&c + 1224B]
	movss	712(%rsp), %xmm4	# %sfp, c$ppMode$maxClippedThreshold
	movss	%xmm4, 2044(%rsp)	# c$ppMode$maxClippedThreshold, MEM[(struct PPContext *)&c + 1228B]
	movl	460(%rsp), %eax	# %sfp, c$1232
	movl	%eax, 2048(%rsp)	# c$1232, MEM[(struct PPContext *)&c + 1232B]
	movl	420(%rsp), %eax	# %sfp, c$1236
	movl	%eax, 2052(%rsp)	# c$1236, MEM[(struct PPContext *)&c + 1236B]
	movl	456(%rsp), %eax	# %sfp, c$1240
	movl	%eax, 2056(%rsp)	# c$1240, MEM[(struct PPContext *)&c + 1240B]
	movl	408(%rsp), %eax	# %sfp, c$ppMode$baseDcDiff
	movl	%eax, 2060(%rsp)	# c$ppMode$baseDcDiff, MEM[(struct PPContext *)&c + 1244B]
	movl	60(%rsp), %eax	# %sfp, c$ppMode$flatnessThreshold
	movl	%eax, 2064(%rsp)	# c$ppMode$flatnessThreshold, MEM[(struct PPContext *)&c + 1248B]
	leaq	816(%rsp), %rsi	#, tmp5152
	movl	$157, %ecx	#, tmp5153
	movq	2168(%rsp), %rdi	# c2, c2
	rep movsq
	jmp	.L475	#
.L180:
	movq	%rax, %rcx	# src, src
	movl	$5, %edx	#, D.14563
	jmp	.L182	#
.L207:
	movq	104(%rsp), %rsi	# %sfp, D.14571
	addq	360(%rsp), %rsi	# %sfp, D.14571
	movq	24(%rsp), %rdi	# %sfp, D.14565
	addq	392(%rsp), %rdi	# %sfp, D.14565
	testb	$8, 32(%rsp)	#, %sfp
	jne	.L392	#,
	movl	240(%rsp), %r11d	# %sfp, D.14563
	movl	56(%rsp), %r10d	# %sfp, D.14563
	movl	$0, %ecx	#, ivtmp.1178
	movl	$0, %edx	#, ivtmp.1177
	movl	$8, %eax	#, D.14563
	jmp	.L210	#
.L392:
	movl	240(%rsp), %r11d	# %sfp, D.14563
	movl	56(%rsp), %r10d	# %sfp, D.14563
	movl	$0, %ecx	#, ivtmp.1170
	movl	$0, %edx	#, ivtmp.1169
	movl	$8, %eax	#, D.14563
	jmp	.L208	#
.L474:
	movl	56(%rsp), %eax	# %sfp, l9
	addl	752(%rsp), %eax	# %sfp, l9
	movq	352(%rsp), %rbx	# %sfp, ivtmp.1225
	leaq	8(%rbx), %rsi	#, D.14572
	movq	%rsi, 120(%rsp)	# D.14572, %sfp
	movq	%rbx, %r8	# ivtmp.1225, ivtmp.974
	movq	168(%rsp), %rbx	# %sfp, D.14566
	negq	%rbx	# D.14566
	movq	%rbx, 128(%rsp)	# D.14566, %sfp
	cltq
	movq	%rax, 136(%rsp)	# D.14566, %sfp
	movslq	436(%rsp), %rax	# %sfp, D.14566
	movq	%rax, 40(%rsp)	# D.14566, %sfp
	movslq	440(%rsp), %rax	# %sfp, D.14566
	movq	%rax, 48(%rsp)	# D.14566, %sfp
	movslq	444(%rsp), %rax	# %sfp, D.14566
	movq	%rax, 72(%rsp)	# D.14566, %sfp
	movslq	448(%rsp), %rbp	# %sfp, D.14566
	jmp	.L252	#
.L246:
	movl	36(%rsp), %eax	# %sfp, c$QP
	sall	$3, %eax	#, D.14562
	movl	%eax, 40(%rsp)	# D.14562, %sfp
	movq	232(%rsp), %rax	# %sfp, ivtmp.1223
	leaq	8(%rax), %rbp	#, D.14572
	movq	%rax, %rcx	# ivtmp.1223, ivtmp.1014
	movslq	440(%rsp), %r13	# %sfp, D.14566
	movslq	436(%rsp), %r14	# %sfp, D.14566
	movslq	444(%rsp), %r15	# %sfp, D.14566
	movslq	448(%rsp), %rax	# %sfp, D.14566
	movq	%rax, 72(%rsp)	# D.14566, %sfp
	jmp	.L257	#
.L406:
	movl	%r12d, %esi	# max, max
	movl	%ebp, %edi	# max, max
.L259:
	movl	412(%rsp), %eax	# %sfp, ivtmp.1053
	movl	$3, %ecx	#, D.14563
	movl	%r8d, 200(%rsp)	# D.14562, %sfp
	movl	%r9d, 368(%rsp)	# D.14562, %sfp
	movl	%ecx, %r8d	# D.14563, D.14563
	movl	56(%rsp), %r9d	# %sfp, dstStride
	jmp	.L262	#
.L279:
	movl	$8, %r12d	#, D.14563
	movl	$0, %r14d	#, tmp5185
	movl	36(%rsp), %r15d	# %sfp, c$QP
	jmp	.L285	#
.L411:
	movl	%r9d, %esi	# max, max
	movl	%r12d, %edi	# max, max
.L299:
	leaq	6(%r15), %rax	#, D.14572
	movq	%rax, 72(%rsp)	# D.14572, %sfp
	movq	%r15, %rcx	# ivtmp.944, ivtmp.929
	jmp	.L302	#
.L475:
	addq	$2088, %rsp	#,
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
.LFE103:
	.size	postProcess_C, .-postProcess_C
	.type	postProcess_MMX, @function
postProcess_MMX:
.LFB121:
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
	subq	$3112, %rsp	#,
	.cfi_def_cfa_offset 3168
	movq	%rdi, 368(%rsp)	# src, %sfp
	movl	%esi, 176(%rsp)	# srcStride, %sfp
	movq	%rdx, 336(%rsp)	# dst, %sfp
	movl	%ecx, 36(%rsp)	# dstStride, %sfp
	movl	%r8d, 96(%rsp)	# width, %sfp
	movl	%r9d, 100(%rsp)	# height, %sfp
	leaq	1840(%rsp), %rdi	#, tmp1978
	movl	$157, %ecx	#, tmp1980
	movq	3192(%rsp), %rsi	# c2, c2
	rep movsq
	cmpl	$0, 3184(%rsp)	#, isColor
	je	.L482	#,
	movl	3052(%rsp), %ebp	# c.ppMode.chromMode, D.15188
	movl	$4, %eax	#, tmp1981
	movl	%eax, %ebx	# tmp1981, D.15188
	subl	3040(%rsp), %ebx	# c.hChromaSubSample, D.15188
	movl	%ebx, 276(%rsp)	# D.15188, %sfp
	subl	3044(%rsp), %eax	# c.vChromaSubSample, D.15188
	movl	%eax, 408(%rsp)	# D.15188, %sfp
	jmp	.L483	#
.L482:
	movl	3048(%rsp), %ebp	# c.ppMode.lumMode, D.15188
	movl	$4, 276(%rsp)	#, %sfp
	movl	$4, 408(%rsp)	#, %sfp
.L483:
	movq	1856(%rsp), %rax	# c.yHistogram, yHistogram
	movq	%rax, 264(%rsp)	# yHistogram, %sfp
	cmpl	$0, 176(%rsp)	#, %sfp
	jle	.L484	#,
	movq	1936(%rsp), %rax	# c.tempSrc, srcBlock
	movq	%rax, 344(%rsp)	# srcBlock, %sfp
	jmp	.L485	#
.L484:
	imull	$23, 176(%rsp), %eax	#, %sfp, D.15188
	cltq
	movq	1936(%rsp), %rbx	# c.tempSrc, srcBlock
	subq	%rax, %rbx	# D.15185, srcBlock
	movq	%rbx, 344(%rsp)	# srcBlock, %sfp
.L485:
	cmpl	$0, 36(%rsp)	#, %sfp
	jle	.L486	#,
	movq	1928(%rsp), %rax	# c.tempDst, tmp3215
	addq	$32, %rax	#, D.15186
	movq	%rax, 328(%rsp)	# D.15186, %sfp
	jmp	.L487	#
.L486:
	imull	$23, 36(%rsp), %edx	#, %sfp, D.15188
	movslq	%edx, %rdx	# D.15188, D.15185
	movl	$32, %eax	#, tmp1990
	subq	%rdx, %rax	# D.15185, D.15185
	addq	1928(%rsp), %rax	# c.tempDst, D.15186
	movq	%rax, 328(%rsp)	# D.15186, %sfp
.L487:
	testl	$33554432, %ebp	#, D.15188
	je	.L488	#,
	movl	$.LC1, %edx	#,
	movl	$24, %esi	#,
	movq	3192(%rsp), %rdi	# c2,
	movl	$0, %eax	#,
	call	av_log	#
	jmp	.L488	#
.L489:
	movl	%ecx, %r10d	# ivtmp.1821, D.15188
	sarl	$8, %r10d	#, D.15188
	movl	%edi, %eax	# tmp2894, D.15188
	subl	%r10d, %eax	# D.15188, D.15188
	cltq
	imulq	%rsi, %rax	# tmp2895, D.15192
	movq	%rax, (%rdx)	# D.15192, MEM[base: _120, offset: 0B]
	leal	2(%r10,%r10), %eax	#, D.15188
	movl	%edi, %ebx	# tmp2894, D.15188
	subl	%eax, %ebx	# D.15188, D.15188
	movl	%ebx, %eax	# D.15188, D.15188
	cltq
	imulq	%rsi, %rax	# tmp2895, D.15192
	movq	%rax, 512(%rdx)	# D.15192, MEM[base: _120, offset: 512B]
	addl	%r9d, %ecx	# D.15189, ivtmp.1821
	addq	$8, %rdx	#, ivtmp.1822
	cmpq	%r8, %rdx	# D.15201, ivtmp.1822
	jne	.L489	#,
	movl	$16, 300(%rsp)	#, %sfp
	movl	%ebp, %eax	# D.15188, D.15188
	andl	$262144, %eax	#, D.15188
	movl	%eax, 296(%rsp)	# D.15188, %sfp
	jne	.L490	#,
	movl	$14, 300(%rsp)	#, %sfp
	testl	$12713984, %ebp	#, D.15188
	jne	.L490	#,
	movl	$13, 300(%rsp)	#, %sfp
	testl	$590849, %ebp	#, D.15188
	jne	.L490	#,
	movl	$11, 300(%rsp)	#, %sfp
	testl	$512, %ebp	#, D.15188
	jne	.L490	#,
	movl	%ebp, %eax	# D.15188, D.15188
	andl	$4, %eax	#, D.15188
	cmpl	$1, %eax	#, D.15188
	sbbl	%eax, %eax	# copyAhead
	addl	$9, %eax	#, copyAhead
	movl	%eax, 300(%rsp)	# copyAhead, %sfp
.L490:
	movl	300(%rsp), %eax	# %sfp, copyAhead
	subl	$8, %eax	#, copyAhead
	movl	%eax, 360(%rsp)	# copyAhead, %sfp
	cmpl	$0, 3184(%rsp)	#, isColor
	jne	.L491	#,
	movl	3024(%rsp), %eax	# c.frameNum, tmp3227
	addl	$1, %eax	#, D.15188
	movl	%eax, 3024(%rsp)	# D.15188, c.frameNum
	cmpl	$1, %eax	#, D.15188
	jne	.L492	#,
	movslq	96(%rsp), %rdx	# %sfp, D.15191
	movslq	100(%rsp), %rax	# %sfp, D.15191
	imulq	%rdx, %rax	# D.15191, D.15191
	shrq	$6, %rax	#, D.15191
	movq	%rax, %rdx	# D.15191, tmp2015
	salq	$4, %rdx	#, tmp2015
	subq	%rax, %rdx	# D.15191, D.15191
	movq	%rdx, %rax	# D.15191, D.15191
	shrq	$8, %rax	#, tmp2017
	movq	264(%rsp), %rbx	# %sfp, yHistogram
	movq	%rax, (%rbx)	# tmp2017, *yHistogram_63
.L492:
	movq	264(%rsp), %rax	# %sfp, yHistogram
	movq	%rax, %rdi	# yHistogram, ivtmp.1797
	leaq	2048(%rax), %rcx	#, D.15201
	movl	$0, %edx	#, clipped
.L493:
	addq	(%rax), %rdx	# MEM[base: _2153, offset: 0B], clipped
	addq	$8, %rax	#, ivtmp.1813
	cmpq	%rcx, %rax	# D.15201, ivtmp.1813
	jne	.L493	#,
	movq	%rdx, %rcx	# clipped, clipped
	testq	%rdx, %rdx	# clipped
	js	.L494	#,
	pxor	%xmm0, %xmm0	# D.15194
	cvtsi2ssq	%rdx, %xmm0	# clipped, D.15194
	jmp	.L495	#
.L494:
	movq	%rdx, %rax	# clipped, tmp2023
	shrq	%rax	# tmp2023
	movq	%rdx, %rsi	# clipped, tmp2024
	andl	$1, %esi	#, tmp2024
	orq	%rsi, %rax	# tmp2024, tmp2023
	pxor	%xmm0, %xmm0	# tmp2022
	cvtsi2ssq	%rax, %xmm0	# tmp2023, tmp2022
	addss	%xmm0, %xmm0	# tmp2022, D.15194
.L495:
	mulss	3068(%rsp), %xmm0	# c.ppMode.maxClippedThreshold, D.15194
	ucomiss	.LC2(%rip), %xmm0	#, D.15194
	jnb	.L496	#,
	cvttss2siq	%xmm0, %rax	# D.15194, maxClipped
	jmp	.L497	#
.L496:
	subss	.LC2(%rip), %xmm0	#, tmp2026
	cvttss2siq	%xmm0, %rax	# tmp2026, maxClipped
	movabsq	$-9223372036854775808, %rsi	#, tmp2028
	xorq	%rsi, %rax	# tmp2028, maxClipped
.L497:
	cmpq	%rcx, %rax	# clipped, maxClipped
	ja	.L645	#,
	movq	264(%rsp), %rbx	# %sfp, yHistogram
	leaq	2040(%rbx), %r8	#, ivtmp.1804
	movq	%rdx, %rcx	# clipped, clipped
	movl	$255, %esi	#, black
.L499:
	subq	(%r8), %rcx	# MEM[base: _2152, offset: 0B], clipped
	subl	$1, %esi	#, black
	subq	$8, %r8	#, ivtmp.1804
	cmpq	%rcx, %rax	# clipped, maxClipped
	ja	.L651	#,
	testl	%esi, %esi	# black
	jg	.L499	#,
.L651:
	movl	$0, %ecx	#, white
.L501:
	subq	(%rdi), %rdx	# MEM[base: _214, offset: 0B], clipped
	addl	$1, %ecx	#, white
	addq	$8, %rdi	#, ivtmp.1797
	cmpq	%rdx, %rax	# clipped, maxClipped
	ja	.L498	#,
	cmpl	$255, %ecx	#, white
	jle	.L501	#,
	jmp	.L498	#
.L645:
	movl	$255, %esi	#, black
	movl	$0, %ecx	#, white
.L498:
	movl	3060(%rsp), %edi	# c.ppMode.minAllowedY, D.15188
	movl	3064(%rsp), %eax	# c.ppMode.maxAllowedY, D.15188
	subl	%edi, %eax	# D.15188, D.15188
	pxor	%xmm1, %xmm1	# D.15195
	cvtsi2sd	%eax, %xmm1	# D.15188, D.15195
	subl	%esi, %ecx	# black, D.15188
	pxor	%xmm0, %xmm0	# D.15195
	cvtsi2sd	%ecx, %xmm0	# D.15188, D.15195
	divsd	%xmm0, %xmm1	# D.15195, scale
	movapd	%xmm1, %xmm0	# scale, scale
	mulsd	.LC3(%rip), %xmm1	#, D.15195
	addsd	.LC4(%rip), %xmm1	#, D.15195
	cvttsd2si	%xmm1, %eax	# D.15195, D.15196
	movzwl	%ax, %edx	# D.15196, D.15191
	movl	%esi, %eax	# black, D.15188
	subl	%edi, %eax	# D.15188, D.15188
	movzwl	%ax, %eax	# D.15188, D.15191
	movq	%rax, %rcx	# D.15191, D.15191
	salq	$32, %rcx	#, D.15191
	orq	%rcx, %rax	# D.15191, D.15191
	movq	%rax, %rcx	# D.15191, D.15191
	salq	$16, %rcx	#, D.15191
	orq	%rcx, %rax	# D.15191, tmp2052
	movq	%rax, 1864(%rsp)	# tmp2052, c.packedYOffset
	movq	%rdx, %rax	# D.15191, D.15191
	salq	$32, %rax	#, D.15191
	orq	%rdx, %rax	# D.15191, D.15191
	movq	%rax, %rdx	# D.15191, D.15191
	salq	$16, %rdx	#, D.15191
	orq	%rdx, %rax	# D.15191, tmp2055
	movq	%rax, 1872(%rsp)	# tmp2055, c.packedYScale
	movl	$65536, 272(%rsp)	#, %sfp
	testb	$8, %bpl	#, D.15188
	je	.L503	#,
	movsd	.LC5(%rip), %xmm1	#, tmp2058
	mulsd	%xmm1, %xmm0	# tmp2058, D.15195
	mulsd	%xmm1, %xmm0	# tmp2058, D.15195
	addsd	.LC4(%rip), %xmm0	#, D.15195
	cvttsd2si	%xmm0, %eax	# D.15195, QPCorrecture
	movl	%eax, 272(%rsp)	# QPCorrecture, %sfp
	jmp	.L503	#
.L491:
	movabsq	$72058693566333184, %rax	#, tmp3246
	movq	%rax, 1872(%rsp)	# tmp3246, c.packedYScale
	movq	$0, 1864(%rsp)	#, c.packedYOffset
	movl	$65536, 272(%rsp)	#, %sfp
.L503:
	movl	176(%rsp), %ebx	# %sfp, srcStride
	movl	%ebx, %eax	# srcStride, D.15188
	negl	%eax	# D.15188
	sall	$3, %eax	#, tmp2066
	cltq
	movq	%rax, 8(%rsp)	# D.15185, %sfp
	movl	36(%rsp), %eax	# %sfp, dstStride
	movslq	%eax, %rdi	# dstStride, D.15185
	movq	%rdi, %rsi	# D.15185, D.15185
	movq	%rdi, 64(%rsp)	# D.15185, %sfp
	movq	328(%rsp), %rdi	# %sfp, dstBlock
	addq	%rsi, %rdi	# D.15185, dstBlock
	movq	%rdi, 352(%rsp)	# dstBlock, %sfp
	cmpl	$0, 96(%rsp)	#, %sfp
	jle	.L504	#,
	leal	0(,%rbx,8), %r13d	#, D.15188
	movslq	%r13d, %rbx	# D.15188, D.15185
	movq	%rbx, 16(%rsp)	# D.15185, %sfp
	leal	0(,%rax,8), %edx	#, D.15188
	movslq	%edx, %r12	# D.15188, D.15185
	movl	%eax, %ebx	# dstStride, dstStride
	leal	0(,%rax,4), %ecx	#, D.15188
	movslq	%ecx, %r14	# D.15188, D.15185
	addl	%eax, %eax	# D.15188
	movslq	%eax, %rsi	# D.15188, D.15185
	movq	%rsi, 40(%rsp)	# D.15185, %sfp
	addl	%ebx, %eax	# dstStride, D.15188
	movslq	%eax, %rsi	# D.15188, D.15185
	movq	%rsi, 48(%rsp)	# D.15185, %sfp
	addl	%eax, %eax	# tmp2077
	cltq
	movq	%rax, 56(%rsp)	# D.15185, %sfp
	movl	%ebx, %eax	# dstStride, dstStride
	addl	%ebx, %ecx	# dstStride, D.15188
	movslq	%ecx, %rbx	# D.15188, D.15185
	movq	%rbx, 72(%rsp)	# D.15185, %sfp
	subl	%eax, %edx	# dstStride, D.15188
	movslq	%edx, %rbx	# D.15188, D.15185
	movq	%rbx, 80(%rsp)	# D.15185, %sfp
	leaq	(%rdi,%r14), %r13	#, ivtmp.1792
	movl	$0, %ebx	#, ivtmp.1784
	movl	%ebp, %edi	# D.15188, D.15188
	andl	$8, %edi	#, D.15188
	movl	%edi, 24(%rsp)	# D.15188, %sfp
	movl	%eax, %r15d	# dstStride, D.15188
	negl	%r15d	# D.15188
	movslq	%r15d, %r15	# D.15188, D.15197
	movq	%r14, 88(%rsp)	# D.15185, %sfp
	movl	%eax, %r14d	# dstStride, dstStride
.L514:
	movq	352(%rsp), %rax	# %sfp, dstBlock
	leaq	(%rax,%rbx), %rdi	#, dstBlock
	movl	%ebx, %ecx	# ivtmp.1784, x
	movq	16(%rsp), %rsi	# %sfp, D.15185
	leaq	(%rsi,%rbx), %r8	#, D.15185
	addq	8(%rsp), %r8	# %sfp, D.15185
	addq	368(%rsp), %r8	# %sfp, D.15200
	leaq	(%r12,%rbx), %rsi	#, D.15185
	addq	%rax, %rsi	# dstBlock, D.15186
	cmpl	$0, 24(%rsp)	#, %sfp
	je	.L505	#,
	movslq	176(%rsp), %r9	# %sfp, D.15197
	movslq	%r14d, %r10	# dstStride, D.15197
	leaq	1864(%rsp), %rax	#, tmp3277
#APP
# 3102 "postprocess_template.c" 1
	movq (%rax), %mm2        
	movq 8(%rax), %mm3       
	lea (%r8,%r9), %rax         	# D.15200, D.15197
	lea (%rsi,%r10), %rdx         	# D.15186, D.15197
	pxor %mm4, %mm4              
	movq (%r8), %mm0          	# D.15200
	movq (%r8), %mm5          	# D.15200
	punpcklbw %mm4, %mm0         
	punpckhbw %mm4, %mm5         
	psubw %mm2, %mm0             
	psubw %mm2, %mm5             
	movq (%r8, %r9), %mm1          	# D.15200, D.15197
	psllw $6, %mm0                
	psllw $6, %mm5                
	pmulhw %mm3, %mm0            
	movq (%r8, %r9), %mm6          	# D.15200, D.15197
	pmulhw %mm3, %mm5            
	punpcklbw %mm4, %mm1         
	punpckhbw %mm4, %mm6         
	psubw %mm2, %mm1             
	psubw %mm2, %mm6             
	psllw $6, %mm1                
	psllw $6, %mm6                
	pmulhw %mm3, %mm1            
	pmulhw %mm3, %mm6            
	packuswb %mm5, %mm0          
	packuswb %mm6, %mm1          
	movq %mm0, (%rsi)          	# D.15186
	movq %mm1, (%rsi, %r10)          	# D.15186, D.15197
	movq (%r8, %r9, 2), %mm0          	# D.15200, D.15197
	movq (%r8, %r9, 2), %mm5          	# D.15200, D.15197
	punpcklbw %mm4, %mm0         
	punpckhbw %mm4, %mm5         
	psubw %mm2, %mm0             
	psubw %mm2, %mm5             
	movq (%rax, %r9, 2), %mm1          	# D.15197
	psllw $6, %mm0                
	psllw $6, %mm5                
	pmulhw %mm3, %mm0            
	movq (%rax, %r9, 2), %mm6          	# D.15197
	pmulhw %mm3, %mm5            
	punpcklbw %mm4, %mm1         
	punpckhbw %mm4, %mm6         
	psubw %mm2, %mm1             
	psubw %mm2, %mm6             
	psllw $6, %mm1                
	psllw $6, %mm6                
	pmulhw %mm3, %mm1            
	pmulhw %mm3, %mm6            
	packuswb %mm5, %mm0          
	packuswb %mm6, %mm1          
	movq %mm0, (%rsi, %r10, 2)          	# D.15186, D.15197
	movq %mm1, (%rdx, %r10, 2)          	# D.15197
	movq (%r8, %r9, 4), %mm0          	# D.15200, D.15197
	movq (%r8, %r9, 4), %mm5          	# D.15200, D.15197
	punpcklbw %mm4, %mm0         
	punpckhbw %mm4, %mm5         
	psubw %mm2, %mm0             
	psubw %mm2, %mm5             
	movq (%rax, %r9, 4), %mm1          	# D.15197
	psllw $6, %mm0                
	psllw $6, %mm5                
	pmulhw %mm3, %mm0            
	movq (%rax, %r9, 4), %mm6          	# D.15197
	pmulhw %mm3, %mm5            
	punpcklbw %mm4, %mm1         
	punpckhbw %mm4, %mm6         
	psubw %mm2, %mm1             
	psubw %mm2, %mm6             
	psllw $6, %mm1                
	psllw $6, %mm6                
	pmulhw %mm3, %mm1            
	pmulhw %mm3, %mm6            
	packuswb %mm5, %mm0          
	packuswb %mm6, %mm1          
	movq %mm0, (%rsi, %r10, 4)          	# D.15186, D.15197
	movq %mm1, (%rdx, %r10, 4)          	# D.15197
	lea (%rax,%r9,4), %rax        	# D.15197
	lea (%rdx,%r10,4), %rdx        	# D.15197
	movq (%rax, %r9), %mm0          	# D.15197
	movq (%rax, %r9), %mm5          	# D.15197
	punpcklbw %mm4, %mm0         
	punpckhbw %mm4, %mm5         
	psubw %mm2, %mm0             
	psubw %mm2, %mm5             
	movq (%rax, %r9, 2), %mm1          	# D.15197
	psllw $6, %mm0                
	psllw $6, %mm5                
	pmulhw %mm3, %mm0            
	movq (%rax, %r9, 2), %mm6          	# D.15197
	pmulhw %mm3, %mm5            
	punpcklbw %mm4, %mm1         
	punpckhbw %mm4, %mm6         
	psubw %mm2, %mm1             
	psubw %mm2, %mm6             
	psllw $6, %mm1                
	psllw $6, %mm6                
	pmulhw %mm3, %mm1            
	pmulhw %mm3, %mm6            
	packuswb %mm5, %mm0          
	packuswb %mm6, %mm1          
	movq %mm0, (%rdx, %r10)          	# D.15197
	movq %mm1, (%rdx, %r10, 2)          	# D.15197
	
# 0 "" 2
#NO_APP
	jmp	.L506	#
.L505:
	movslq	176(%rsp), %r9	# %sfp, D.15197
	movslq	%r14d, %r10	# dstStride, D.15197
#APP
# 3185 "postprocess_template.c" 1
	lea (%r8,%r9), %rax                 	# D.15200, D.15197
	lea (%rsi,%r10), %rdx                 	# D.15186, D.15197
	movq (%r8), %mm0          	# D.15200
	movq (%r8, %r9), %mm1          	# D.15200, D.15197
	movq %mm0, (%rsi)          	# D.15186
	movq %mm1, (%rsi, %r10)          	# D.15186, D.15197
	movq (%r8, %r9, 2), %mm0          	# D.15200, D.15197
	movq (%rax, %r9, 2), %mm1          	# D.15197
	movq %mm0, (%rsi, %r10, 2)          	# D.15186, D.15197
	movq %mm1, (%rdx, %r10, 2)          	# D.15197
	movq (%r8, %r9, 4), %mm0          	# D.15200, D.15197
	movq (%rax, %r9, 4), %mm1          	# D.15197
	movq %mm0, (%rsi, %r10, 4)          	# D.15186, D.15197
	movq %mm1, (%rdx, %r10, 4)          	# D.15197
	lea (%rax,%r9,4), %rax        	# D.15197
	lea (%rdx,%r10,4), %rdx        	# D.15197
	movq (%rax, %r9), %mm0          	# D.15197
	movq (%rax, %r9, 2), %mm1          	# D.15197
	movq %mm0, (%rdx, %r10)          	# D.15197
	movq %mm1, (%rdx, %r10, 2)          	# D.15197
	
# 0 "" 2
#NO_APP
.L506:
#APP
# 3225 "postprocess_template.c" 1
	movq (%rsi), %mm0               	# src
	movq %mm0, (%rsi, %r15, 4)        	# src, D.15197
	add %r15, %rsi                     	# D.15197, src
	movq %mm0, (%rsi)               	# src
	movq %mm0, (%rsi, %r15)           	# src, D.15197
	movq %mm0, (%rsi, %r15, 2)        	# src, D.15197
	movq %mm0, (%rsi, %r15, 4)        	# src, D.15197
	
# 0 "" 2
#NO_APP
	testl	$65536, %ebp	#, D.15188
	je	.L507	#,
	movq	%r13, %rcx	# ivtmp.1792, src
	leaq	8(%r13), %rsi	#, D.15186
.L508:
	movl	(%rcx), %edx	# MEM[base: src_1908, offset: 0B], a
	movq	40(%rsp), %rax	# %sfp, D.15185
	movl	(%rcx,%rax), %eax	# MEM[base: src_1908, index: _511, offset: 0B], b
	movl	%eax, %edi	# b, D.15188
	orl	%edx, %edi	# a, D.15188
	xorl	%eax, %edx	# b, D.15188
	andl	$-16843010, %edx	#, D.15191
	shrq	%rdx	# D.15191
	subl	%edx, %edi	# D.15191, tmp2104
	movl	%edi, %edx	# tmp2104, tmp2104
	movq	64(%rsp), %rdi	# %sfp, D.15185
	movl	%edx, (%rcx,%rdi)	# tmp2104, MEM[base: src_1908, index: _180, offset: 0B]
	movq	88(%rsp), %rdi	# %sfp, D.15185
	movl	(%rcx,%rdi), %edx	# MEM[base: src_1908, index: _505, offset: 0B], a
	movl	%edx, %edi	# a, D.15188
	orl	%eax, %edi	# b, D.15188
	xorl	%edx, %eax	# a, D.15188
	andl	$-16843010, %eax	#, D.15191
	shrq	%rax	# D.15191
	subl	%eax, %edi	# D.15191, tmp2110
	movl	%edi, %eax	# tmp2110, tmp2110
	movq	48(%rsp), %rdi	# %sfp, D.15185
	movl	%eax, (%rcx,%rdi)	# tmp2110, MEM[base: src_1908, index: _529, offset: 0B]
	movq	56(%rsp), %rax	# %sfp, D.15185
	movl	(%rcx,%rax), %eax	# MEM[base: src_1908, index: _540, offset: 0B], b
	movl	%eax, %edi	# b, D.15188
	orl	%edx, %edi	# a, D.15188
	xorl	%eax, %edx	# b, D.15188
	andl	$-16843010, %edx	#, D.15191
	shrq	%rdx	# D.15191
	subl	%edx, %edi	# D.15191, tmp2116
	movl	%edi, %edx	# tmp2116, tmp2116
	movq	72(%rsp), %rdi	# %sfp, D.15185
	movl	%edx, (%rcx,%rdi)	# tmp2116, MEM[base: src_1908, index: _545, offset: 0B]
	movl	(%rcx,%r12), %edi	# MEM[base: src_1908, index: _187, offset: 0B], a
	movl	%edi, %edx	# a, D.15188
	orl	%eax, %edx	# b, D.15188
	xorl	%edi, %eax	# a, D.15188
	andl	$-16843010, %eax	#, D.15191
	shrq	%rax	# D.15191
	subl	%eax, %edx	# D.15191, tmp2122
	movq	80(%rsp), %rdi	# %sfp, D.15185
	movl	%edx, (%rcx,%rdi)	# tmp2122, MEM[base: src_1908, index: _561, offset: 0B]
	addq	$4, %rcx	#, src
	cmpq	%rsi, %rcx	# D.15186, src
	jne	.L508	#,
	jmp	.L509	#
.L507:
	testl	$131072, %ebp	#, D.15188
	je	.L510	#,
	movslq	%ecx, %rdx	# x, D.15185
	addq	1944(%rsp), %rdx	# c.deintTemp, D.15186
	movl	%r14d, %esi	# dstStride,
	call	deInterlaceBlendLinear_MMX	#
	jmp	.L509	#
.L510:
	testl	$524288, %ebp	#, D.15188
	je	.L511	#,
	movl	%r14d, %esi	# dstStride,
	call	deInterlaceMedian_MMX	#
	jmp	.L509	#
.L511:
	cmpl	$0, 296(%rsp)	#, %sfp
	je	.L512	#,
	movl	%r14d, %esi	# dstStride,
	call	deInterlaceInterpolateCubic_MMX	#
	jmp	.L509	#
.L512:
	testl	$4194304, %ebp	#, D.15188
	je	.L513	#,
	movslq	%ecx, %rdx	# x, D.15185
	addq	1944(%rsp), %rdx	# c.deintTemp, D.15186
	movl	%r14d, %esi	# dstStride,
	call	deInterlaceFF_MMX	#
	jmp	.L509	#
.L513:
	testl	$8388608, %ebp	#, D.15188
	je	.L509	#,
	movq	1944(%rsp), %rdx	# c.deintTemp, D.15186
	movslq	%ecx, %rax	# x, D.15185
	movslq	96(%rsp), %rcx	# %sfp, D.15185
	addq	%rax, %rcx	# D.15185, D.15185
	addq	%rdx, %rcx	# D.15186, D.15186
	addq	%rax, %rdx	# D.15185, D.15186
	movl	%r14d, %esi	# dstStride,
	call	deInterlaceL5_MMX	#
.L509:
	addq	$8, %rbx	#, ivtmp.1784
	addq	$8, %r13	#, ivtmp.1792
	cmpl	%ebx, 96(%rsp)	# ivtmp.1784, %sfp
	jg	.L514	#,
.L504:
	movl	36(%rsp), %ebx	# %sfp, dstStride
	movl	%ebx, %eax	# dstStride, tmp2137
	sarl	$31, %eax	#, tmp2137
	xorl	%eax, %ebx	# tmp2137, D.15188
	subl	%eax, %ebx	# tmp2137, D.15188
	movl	%ebx, 440(%rsp)	# D.15188, %sfp
	cmpl	96(%rsp), %ebx	# %sfp, D.15188
	je	.L515	#,
	cmpl	$0, 360(%rsp)	#, %sfp
	jg	.L516	#,
	jmp	.L517	#
.L515:
	movl	36(%rsp), %eax	# %sfp, dstStride
	leal	(%rax,%rax,8), %esi	#, D.15188
	movslq	%esi, %rsi	# D.15188, D.15185
	addq	328(%rsp), %rsi	# %sfp, D.15198
	movl	%eax, %ecx	# dstStride,
	movl	360(%rsp), %edx	# %sfp,
	movq	336(%rsp), %rdi	# %sfp,
	call	linecpy	#
.L517:
	cmpl	$0, 100(%rsp)	#, %sfp
	jg	.L518	#,
	jmp	.L519	#
.L516:
	movl	36(%rsp), %eax	# %sfp, dstStride
	movl	%eax, 8(%rsp)	# dstStride, %sfp
	movl	300(%rsp), %ebx	# %sfp, copyAhead
	leal	-8(%rbx), %r13d	#, D.15188
	leal	(%rax,%rax,8), %r12d	#, D.15189
	movl	$0, %ebx	#, ivtmp.1763
	movl	$0, %r15d	#, i
	movslq	96(%rsp), %r14	# %sfp, D.15191
.L520:
	movslq	%ebx, %rdi	# ivtmp.1763, D.15185
	addq	336(%rsp), %rdi	# %sfp, D.15186
	leal	(%rbx,%r12), %esi	#, D.15189
	movslq	%esi, %rsi	# D.15189, D.15185
	addq	328(%rsp), %rsi	# %sfp, D.15186
	movq	%r14, %rdx	# D.15191,
	call	memcpy	#
	addl	$1, %r15d	#, i
	addl	8(%rsp), %ebx	# %sfp, ivtmp.1763
	cmpl	%r13d, %r15d	# D.15188, i
	jne	.L520	#,
	jmp	.L517	#
.L518:
	movl	176(%rsp), %r14d	# %sfp, srcStride
	movl	%r14d, %eax	# srcStride, D.15188
	movl	360(%rsp), %esi	# %sfp, copyAhead
	imull	%esi, %eax	# copyAhead, D.15188
	cltq
	movq	%rax, 160(%rsp)	# D.15185, %sfp
	movq	344(%rsp), %rdi	# %sfp, D.15187
	addq	%rax, %rdi	# D.15185, D.15187
	movq	%rdi, 456(%rsp)	# D.15187, %sfp
	movl	100(%rsp), %r11d	# %sfp, height
	leal	-1(%r11), %edx	#, D.15188
	movl	%r14d, %eax	# srcStride, D.15188
	imull	%edx, %eax	# D.15188, D.15188
	cltq
	addq	368(%rsp), %rax	# %sfp, D.15200
	movq	%rax, 464(%rsp)	# D.15200, %sfp
	movl	36(%rsp), %ebx	# %sfp, dstStride
	imull	%ebx, %edx	# dstStride, D.15188
	movslq	%edx, %rax	# D.15188, D.15185
	addq	336(%rsp), %rax	# %sfp, D.15186
	movq	%rax, 472(%rsp)	# D.15186, %sfp
	leal	(%r14,%r14,2), %eax	#, D.15188
	sall	$2, %eax	#, tmp2164
	cltq
	movq	%rax, 280(%rsp)	# D.15185, %sfp
	movl	%ebx, %eax	# dstStride, D.15188
	imull	%esi, %eax	# copyAhead, D.15188
	cltq
	movq	%rax, 200(%rsp)	# D.15185, %sfp
	movl	%ebx, %eax	# dstStride, dstStride
	sall	$2, %eax	#, D.15188
	movslq	%eax, %rdi	# D.15188, D.15185
	movq	%rdi, 168(%rsp)	# D.15185, %sfp
	leal	(%rbx,%rbx), %esi	#, D.15188
	movslq	%esi, %rdi	# D.15188, D.15185
	movq	%rdi, 216(%rsp)	# D.15185, %sfp
	leal	(%rsi,%rbx), %ecx	#, D.15188
	movslq	%ecx, %rdi	# D.15188, D.15185
	movq	%rdi, 56(%rsp)	# D.15185, %sfp
	leal	(%rcx,%rcx), %edi	#, tmp2173
	movslq	%edi, %rdi	# tmp2173, D.15185
	movq	%rdi, 88(%rsp)	# D.15185, %sfp
	addl	%ebx, %eax	# dstStride, D.15188
	movslq	%eax, %rdi	# D.15188, D.15185
	movq	%rdi, 224(%rsp)	# D.15185, %sfp
	leal	0(,%rbx,8), %edi	#, D.15188
	movslq	%edi, %r9	# D.15188, D.15185
	movq	%r9, 208(%rsp)	# D.15185, %sfp
	movl	%edi, %r8d	# D.15188, D.15188
	subl	%ebx, %r8d	# dstStride, D.15188
	movslq	%r8d, %r9	# D.15188, D.15185
	movq	%r9, 232(%rsp)	# D.15185, %sfp
	addl	%eax, %eax	# tmp2183
	cltq
	movq	%rax, 424(%rsp)	# D.15185, %sfp
	leal	(%rdi,%rbx), %eax	#, D.15188
	cltq
	movq	%rax, 384(%rsp)	# D.15185, %sfp
	leal	0(,%rcx,4), %eax	#, tmp2190
	cltq
	movq	%rax, 432(%rsp)	# D.15185, %sfp
	negl	%ebx	# tmp2192
	movl	%ebx, %eax	# tmp2192, tmp2192
	sall	$3, %eax	#, tmp2193
	movslq	%eax, %rbx	# tmp2193, offset
	movq	%rbx, 400(%rsp)	# offset, %sfp
	leal	0(,%r14,8), %ecx	#, D.15189
	movl	%ecx, 444(%rsp)	# D.15189, %sfp
	movl	%edi, 448(%rsp)	# D.15188, %sfp
	movl	%r11d, 484(%rsp)	# height, %sfp
	leal	1(%r11), %ecx	#, ivtmp.1754
	movl	%ecx, 412(%rsp)	# ivtmp.1754, %sfp
	movl	%eax, 452(%rsp)	# tmp2193, %sfp
	leal	(%rdx,%rsi), %eax	#, ivtmp.1755
	movl	%eax, 416(%rsp)	# ivtmp.1755, %sfp
	movl	%r11d, 364(%rsp)	# height, %sfp
	movl	$0, 396(%rsp)	#, %sfp
	movl	$0, 392(%rsp)	#, %sfp
	movl	$0, 80(%rsp)	#, %sfp
	movl	%r14d, %edx	# srcStride, tmp2884
	sarl	$31, %edx	#, tmp2884
	movl	%r14d, %eax	# srcStride, tmp2885
	xorl	%edx, %eax	# tmp2884, tmp2885
	subl	%edx, %eax	# tmp2884, D.15188
	cltq
	movq	%rax, 488(%rsp)	# D.15191, %sfp
	movl	%ebp, %r14d	# D.15188, D.15188
.L639:
	movslq	392(%rsp), %rax	# %sfp, D.15185
	addq	368(%rsp), %rax	# %sfp, srcBlock
	movq	%rax, 48(%rsp)	# srcBlock, %sfp
	movslq	396(%rsp), %rax	# %sfp, D.15185
	movq	%rax, %rbx	# D.15185, D.15185
	movq	%rax, 288(%rsp)	# D.15185, %sfp
	movq	336(%rsp), %rax	# %sfp, dstBlock
	addq	%rbx, %rax	# D.15185, dstBlock
	movq	%rax, %rdi	# dstBlock, dstBlock
	movq	%rax, 376(%rsp)	# dstBlock, %sfp
	movq	1848(%rsp), %rax	# c.tempBlocks, tempBlock2
	movq	%rax, 24(%rsp)	# tempBlock2, %sfp
	addq	$8, %rax	#, tempBlock2
	movq	%rax, 40(%rsp)	# tempBlock2, %sfp
	movl	80(%rsp), %ebx	# %sfp, y
	movl	%ebx, %eax	# y, D.15188
	movzbl	408(%rsp), %ecx	# %sfp, tmp3371
	sarl	%cl, %eax	# tmp3371, D.15188
	movl	%eax, %ecx	# D.15188, D.15188
	movl	%eax, %edx	# D.15188, D.15188
	imull	3176(%rsp), %edx	# QPStride, D.15188
	movslq	%edx, %rdx	# D.15188, D.15185
	addq	3168(%rsp), %rdx	# QPs, QPptr
	movq	%rdx, 184(%rsp)	# QPptr, %sfp
	movl	3176(%rsp), %edx	# QPStride, tmp2197
	sarl	$31, %edx	#, tmp2197
	movl	%edx, %eax	# tmp2197, tmp2198
	xorl	3176(%rsp), %eax	# QPStride, tmp2198
	subl	%edx, %eax	# tmp2197, D.15188
	imull	%ecx, %eax	# D.15188, D.15188
	cltq
	addq	3000(%rsp), %rax	# c.nonBQPTable, nonBQPptr
	movq	%rax, 192(%rsp)	# nonBQPptr, %sfp
	movl	%ebx, %eax	# y, y
	movl	%ebx, 180(%rsp)	# y, %sfp
	addl	$15, %eax	#, D.15188
	movl	%eax, 420(%rsp)	# D.15188, %sfp
	cmpl	%eax, 100(%rsp)	# D.15188, %sfp
	jle	.L521	#,
	movq	%rdi, 8(%rsp)	# dstBlock, %sfp
	jmp	.L526	#
.L647:
	movq	352(%rsp), %rax	# %sfp, dstBlock
	movq	%rax, 8(%rsp)	# dstBlock, %sfp
	movq	344(%rsp), %rax	# %sfp, srcBlock
	movq	%rax, 48(%rsp)	# srcBlock, %sfp
.L526:
	cmpl	$0, 96(%rsp)	#, %sfp
	jg	.L522	#,
	movl	$0, 16(%rsp)	#, %sfp
	jmp	.L523	#
.L521:
	movl	484(%rsp), %edx	# %sfp, D.15189
	movl	300(%rsp), %r15d	# %sfp, copyAhead
	subl	%r15d, %edx	# copyAhead, D.15189
	addl	$8, %edx	#, D.15189
	subl	80(%rsp), %edx	# %sfp, D.15189
	movl	$0, %eax	#, tmp2206
	cmovs	%eax, %edx	# D.15189,, tmp2206, D.15189
	movq	48(%rsp), %rsi	# %sfp, D.15198
	addq	160(%rsp), %rsi	# %sfp, D.15198
	movl	176(%rsp), %ebp	# %sfp, srcStride
	movl	%ebp, %ecx	# srcStride,
	movq	456(%rsp), %rdi	# %sfp,
	call	linecpy	#
	movl	364(%rsp), %eax	# %sfp, ivtmp.1752
	cmpl	$8, %eax	#, ivtmp.1752
	movl	$8, %ebx	#, tmp2208
	cmovge	%eax, %ebx	# ivtmp.1752,, i
	cmpl	%ebx, %r15d	# i, copyAhead
	jle	.L524	#,
	movl	%ebp, %r12d	# srcStride, D.15189
	imull	%ebx, %ebp	# i, ivtmp.1743
.L525:
	movslq	%ebp, %rdi	# ivtmp.1743, D.15185
	addq	344(%rsp), %rdi	# %sfp, D.15186
	movq	488(%rsp), %rdx	# %sfp,
	movq	464(%rsp), %rsi	# %sfp,
	call	memcpy	#
	addl	$1, %ebx	#, i
	addl	%r12d, %ebp	# D.15189, ivtmp.1743
	cmpl	%ebx, 300(%rsp)	# i, %sfp
	jg	.L525	#,
.L524:
	movl	412(%rsp), %r15d	# %sfp, ivtmp.1754
	movl	%r15d, %ebx	# ivtmp.1754, i
	movl	300(%rsp), %eax	# %sfp, copyAhead
	subl	$7, %eax	#, D.15188
	cmpl	%eax, %r15d	# D.15188, ivtmp.1754
	cmovle	%r15d, %eax	# ivtmp.1754,, D.15188
	movl	%eax, %edx	# D.15188, D.15188
	movq	376(%rsp), %rsi	# %sfp, D.15186
	subq	64(%rsp), %rsi	# %sfp, D.15186
	movl	36(%rsp), %r13d	# %sfp, dstStride
	movl	%r13d, %ecx	# dstStride,
	movq	328(%rsp), %rdi	# %sfp,
	call	linecpy	#
	cmpl	%r15d, 360(%rsp)	# ivtmp.1754, %sfp
	jl	.L647	#,
	movl	416(%rsp), %ebp	# %sfp, ivtmp.1736
	movslq	440(%rsp), %r12	# %sfp, D.15191
.L527:
	movslq	%ebp, %rdi	# ivtmp.1736, D.15185
	addq	328(%rsp), %rdi	# %sfp, D.15186
	movq	%r12, %rdx	# D.15191,
	movq	472(%rsp), %rsi	# %sfp,
	call	memcpy	#
	addl	$1, %ebx	#, i
	addl	%r13d, %ebp	# D.15189, ivtmp.1736
	cmpl	%ebx, 360(%rsp)	# i, %sfp
	jge	.L527	#,
	movq	352(%rsp), %rax	# %sfp, dstBlock
	movq	%rax, 8(%rsp)	# dstBlock, %sfp
	movq	344(%rsp), %rax	# %sfp, srcBlock
	movq	%rax, 48(%rsp)	# srcBlock, %sfp
	jmp	.L526	#
.L522:
	movl	80(%rsp), %eax	# %sfp, D.15188
	sarl	$3, %eax	#, D.15188
	sall	$8, %eax	#, D.15188
	cltq
	addq	$256, %rax	#, D.15185
	movq	%rax, 304(%rsp)	# D.15185, %sfp
	movq	8(%rsp), %rdi	# %sfp, dstBlock
	movq	%rdi, %rax	# dstBlock, dstBlock
	addq	$8, %rax	#, ivtmp.1695
	movq	%rax, 72(%rsp)	# ivtmp.1695, %sfp
	movq	168(%rsp), %rbx	# %sfp, D.15185
	movq	%rbx, %rax	# D.15185, D.15185
	addq	232(%rsp), %rax	# %sfp, D.15185
	addq	%rdi, %rax	# dstBlock, ivtmp.1717
	movq	%rax, 104(%rsp)	# ivtmp.1717, %sfp
	movq	%rbx, %rax	# D.15185, D.15185
	addq	224(%rsp), %rax	# %sfp, D.15185
	addq	%rdi, %rax	# dstBlock, ivtmp.1718
	movq	%rax, 112(%rsp)	# ivtmp.1718, %sfp
	movq	%rbx, %rax	# D.15185, D.15185
	addq	88(%rsp), %rax	# %sfp, D.15185
	addq	%rdi, %rax	# dstBlock, ivtmp.1720
	movq	%rax, 120(%rsp)	# ivtmp.1720, %sfp
	movq	%rbx, %rax	# D.15185, D.15185
	addq	56(%rsp), %rax	# %sfp, D.15185
	addq	%rdi, %rax	# dstBlock, ivtmp.1722
	movq	%rax, 128(%rsp)	# ivtmp.1722, %sfp
	movq	%rbx, %rax	# D.15185, D.15185
	leaq	(%rdi,%rbx,2), %rbx	#, ivtmp.1723
	movq	%rbx, 136(%rsp)	# ivtmp.1723, %sfp
	movq	64(%rsp), %rsi	# %sfp, D.15185
	movq	%rax, %rbx	# D.15185, D.15185
	addq	%rax, %rsi	# D.15185, D.15185
	movq	%rsi, %rax	# D.15185, D.15185
	addq	%rdi, %rax	# dstBlock, ivtmp.1724
	movq	%rax, 144(%rsp)	# ivtmp.1724, %sfp
	movq	%rbx, %rax	# D.15185, D.15185
	addq	216(%rsp), %rax	# %sfp, D.15185
	addq	%rdi, %rax	# dstBlock, ivtmp.1725
	movq	%rax, 152(%rsp)	# ivtmp.1725, %sfp
	movl	$0, 16(%rsp)	#, %sfp
	movl	36(%rsp), %edi	# %sfp, dstStride
	movl	%edi, %eax	# dstStride, dstStride
	leal	(%rdi,%rdi), %ebx	#, l2
	movl	%ebx, 312(%rsp)	# l2, %sfp
	addl	%ebx, %eax	# l2, l3
	movl	%eax, 316(%rsp)	# l3, %sfp
	movl	%edi, %ebx	# dstStride, l4
	addl	%eax, %ebx	# l3, l4
	movl	%ebx, 320(%rsp)	# l4, %sfp
	addl	%ebx, %edi	# l4, l5
	movl	%edi, 324(%rsp)	# l5, %sfp
	jmp	.L613	#
.L650:
	movq	%rax, 24(%rsp)	# tempBlock2, %sfp
.L613:
	cmpl	$0, 3184(%rsp)	#, isColor
	je	.L528	#,
	movl	16(%rsp), %edx	# %sfp, D.15188
	movzbl	276(%rsp), %ecx	# %sfp, tmp3450
	sarl	%cl, %edx	# tmp3450, D.15188
	movslq	%edx, %rdx	# D.15188, D.15185
	movq	184(%rsp), %rax	# %sfp, QPptr
	movsbl	(%rax,%rdx), %eax	# *_295, QP
	movq	192(%rsp), %rbx	# %sfp, nonBQPptr
	movsbl	(%rbx,%rdx), %edx	# *_298, *_298
	movl	%edx, 3020(%rsp)	# *_298, c.nonBQP
	jmp	.L529	#
.L528:
	movl	16(%rsp), %edx	# %sfp, D.15188
	sarl	$4, %edx	#, D.15188
	movslq	%edx, %rdx	# D.15188, D.15185
	movq	184(%rsp), %rax	# %sfp, QPptr
	movsbl	(%rax,%rdx), %eax	# *_304, QP
	movl	272(%rsp), %ebx	# %sfp, QPCorrecture
	imull	%ebx, %eax	# QPCorrecture, D.15188
	addl	$32768, %eax	#, D.15188
	sarl	$16, %eax	#, QP
	movq	192(%rsp), %rdi	# %sfp, nonBQPptr
	movsbl	(%rdi,%rdx), %edx	# *_310, D.15188
	imull	%ebx, %edx	# QPCorrecture, D.15188
	addl	$32768, %edx	#, D.15188
	sarl	$16, %edx	#, tmp2249
	movl	%edx, 3020(%rsp)	# tmp2249, c.nonBQP
	movq	48(%rsp), %rbx	# %sfp, srcBlock
	movq	280(%rsp), %rdi	# %sfp, D.15185
	movzbl	4(%rbx,%rdi), %edx	# MEM[base: srcBlock_1886, index: _319, offset: 4B], D.15191
	movq	264(%rsp), %rbx	# %sfp, yHistogram
	addq	$1, (%rbx,%rdx,8)	#, *_325
.L529:
	movl	%eax, 3016(%rsp)	# QP, c.QP
#APP
# 3497 "postprocess_template.c" 1
	movd %eax, %mm7         	# QP
	packuswb %mm7, %mm7  
	packuswb %mm7, %mm7  
	packuswb %mm7, %mm7  
	movq %mm7, 1952(%rsp)         	# c.pQPb
	
# 0 "" 2
#NO_APP
	movq	48(%rsp), %rsi	# %sfp, D.15200
	addq	160(%rsp), %rsi	# %sfp, D.15200
	movq	8(%rsp), %rdi	# %sfp, D.15186
	addq	200(%rsp), %rdi	# %sfp, D.15186
	testb	$8, %r14b	#, D.15188
	je	.L530	#,
	movslq	176(%rsp), %rcx	# %sfp, D.15197
	movslq	36(%rsp), %r8	# %sfp, D.15197
	leaq	1864(%rsp), %rax	#, tmp3467
#APP
# 3102 "postprocess_template.c" 1
	movq (%rax), %mm2        
	movq 8(%rax), %mm3       
	lea (%rsi,%rcx), %rax         	# D.15200, D.15197
	lea (%rdi,%r8), %rdx         	# D.15186, D.15197
	pxor %mm4, %mm4              
	movq (%rsi), %mm0          	# D.15200
	movq (%rsi), %mm5          	# D.15200
	punpcklbw %mm4, %mm0         
	punpckhbw %mm4, %mm5         
	psubw %mm2, %mm0             
	psubw %mm2, %mm5             
	movq (%rsi, %rcx), %mm1          	# D.15200, D.15197
	psllw $6, %mm0                
	psllw $6, %mm5                
	pmulhw %mm3, %mm0            
	movq (%rsi, %rcx), %mm6          	# D.15200, D.15197
	pmulhw %mm3, %mm5            
	punpcklbw %mm4, %mm1         
	punpckhbw %mm4, %mm6         
	psubw %mm2, %mm1             
	psubw %mm2, %mm6             
	psllw $6, %mm1                
	psllw $6, %mm6                
	pmulhw %mm3, %mm1            
	pmulhw %mm3, %mm6            
	packuswb %mm5, %mm0          
	packuswb %mm6, %mm1          
	movq %mm0, (%rdi)          	# D.15186
	movq %mm1, (%rdi, %r8)          	# D.15186, D.15197
	movq (%rsi, %rcx, 2), %mm0          	# D.15200, D.15197
	movq (%rsi, %rcx, 2), %mm5          	# D.15200, D.15197
	punpcklbw %mm4, %mm0         
	punpckhbw %mm4, %mm5         
	psubw %mm2, %mm0             
	psubw %mm2, %mm5             
	movq (%rax, %rcx, 2), %mm1          	# D.15197
	psllw $6, %mm0                
	psllw $6, %mm5                
	pmulhw %mm3, %mm0            
	movq (%rax, %rcx, 2), %mm6          	# D.15197
	pmulhw %mm3, %mm5            
	punpcklbw %mm4, %mm1         
	punpckhbw %mm4, %mm6         
	psubw %mm2, %mm1             
	psubw %mm2, %mm6             
	psllw $6, %mm1                
	psllw $6, %mm6                
	pmulhw %mm3, %mm1            
	pmulhw %mm3, %mm6            
	packuswb %mm5, %mm0          
	packuswb %mm6, %mm1          
	movq %mm0, (%rdi, %r8, 2)          	# D.15186, D.15197
	movq %mm1, (%rdx, %r8, 2)          	# D.15197
	movq (%rsi, %rcx, 4), %mm0          	# D.15200, D.15197
	movq (%rsi, %rcx, 4), %mm5          	# D.15200, D.15197
	punpcklbw %mm4, %mm0         
	punpckhbw %mm4, %mm5         
	psubw %mm2, %mm0             
	psubw %mm2, %mm5             
	movq (%rax, %rcx, 4), %mm1          	# D.15197
	psllw $6, %mm0                
	psllw $6, %mm5                
	pmulhw %mm3, %mm0            
	movq (%rax, %rcx, 4), %mm6          	# D.15197
	pmulhw %mm3, %mm5            
	punpcklbw %mm4, %mm1         
	punpckhbw %mm4, %mm6         
	psubw %mm2, %mm1             
	psubw %mm2, %mm6             
	psllw $6, %mm1                
	psllw $6, %mm6                
	pmulhw %mm3, %mm1            
	pmulhw %mm3, %mm6            
	packuswb %mm5, %mm0          
	packuswb %mm6, %mm1          
	movq %mm0, (%rdi, %r8, 4)          	# D.15186, D.15197
	movq %mm1, (%rdx, %r8, 4)          	# D.15197
	lea (%rax,%rcx,4), %rax        	# D.15197
	lea (%rdx,%r8,4), %rdx        	# D.15197
	movq (%rax, %rcx), %mm0          	# D.15197
	movq (%rax, %rcx), %mm5          	# D.15197
	punpcklbw %mm4, %mm0         
	punpckhbw %mm4, %mm5         
	psubw %mm2, %mm0             
	psubw %mm2, %mm5             
	movq (%rax, %rcx, 2), %mm1          	# D.15197
	psllw $6, %mm0                
	psllw $6, %mm5                
	pmulhw %mm3, %mm0            
	movq (%rax, %rcx, 2), %mm6          	# D.15197
	pmulhw %mm3, %mm5            
	punpcklbw %mm4, %mm1         
	punpckhbw %mm4, %mm6         
	psubw %mm2, %mm1             
	psubw %mm2, %mm6             
	psllw $6, %mm1                
	psllw $6, %mm6                
	pmulhw %mm3, %mm1            
	pmulhw %mm3, %mm6            
	packuswb %mm5, %mm0          
	packuswb %mm6, %mm1          
	movq %mm0, (%rdx, %r8)          	# D.15197
	movq %mm1, (%rdx, %r8, 2)          	# D.15197
	
# 0 "" 2
#NO_APP
	jmp	.L531	#
.L530:
	movslq	176(%rsp), %rcx	# %sfp, D.15197
	movslq	36(%rsp), %r8	# %sfp, D.15197
#APP
# 3185 "postprocess_template.c" 1
	lea (%rsi,%rcx), %rax                 	# D.15200, D.15197
	lea (%rdi,%r8), %rdx                 	# D.15186, D.15197
	movq (%rsi), %mm0          	# D.15200
	movq (%rsi, %rcx), %mm1          	# D.15200, D.15197
	movq %mm0, (%rdi)          	# D.15186
	movq %mm1, (%rdi, %r8)          	# D.15186, D.15197
	movq (%rsi, %rcx, 2), %mm0          	# D.15200, D.15197
	movq (%rax, %rcx, 2), %mm1          	# D.15197
	movq %mm0, (%rdi, %r8, 2)          	# D.15186, D.15197
	movq %mm1, (%rdx, %r8, 2)          	# D.15197
	movq (%rsi, %rcx, 4), %mm0          	# D.15200, D.15197
	movq (%rax, %rcx, 4), %mm1          	# D.15197
	movq %mm0, (%rdi, %r8, 4)          	# D.15186, D.15197
	movq %mm1, (%rdx, %r8, 4)          	# D.15197
	lea (%rax,%rcx,4), %rax        	# D.15197
	lea (%rdx,%r8,4), %rdx        	# D.15197
	movq (%rax, %rcx), %mm0          	# D.15197
	movq (%rax, %rcx, 2), %mm1          	# D.15197
	movq %mm0, (%rdx, %r8)          	# D.15197
	movq %mm1, (%rdx, %r8, 2)          	# D.15197
	
# 0 "" 2
#NO_APP
.L531:
	testl	$65536, %r14d	#, D.15188
	je	.L532	#,
	movq	8(%rsp), %rcx	# %sfp, src
	movq	168(%rsp), %r8	# %sfp, D.15185
	addq	%r8, %rcx	# D.15185, src
	movq	%r8, %rsi	# D.15185, D.15186
	addq	72(%rsp), %rsi	# %sfp, D.15186
	movq	64(%rsp), %rdi	# %sfp, D.15185
	movq	216(%rsp), %r9	# %sfp, D.15185
	movq	56(%rsp), %r10	# %sfp, D.15185
	movq	88(%rsp), %r11	# %sfp, D.15185
	movq	224(%rsp), %rbx	# %sfp, D.15185
	movq	208(%rsp), %rbp	# %sfp, D.15185
	movq	232(%rsp), %r12	# %sfp, D.15185
.L533:
	movl	(%rcx), %edx	# MEM[base: src_1910, offset: 0B], a
	movl	(%rcx,%r9), %eax	# MEM[base: src_1910, index: _584, offset: 0B], b
	movl	%eax, %r13d	# b, D.15188
	orl	%edx, %r13d	# a, D.15188
	xorl	%eax, %edx	# b, D.15188
	andl	$-16843010, %edx	#, D.15191
	shrq	%rdx	# D.15191
	subl	%edx, %r13d	# D.15191, tmp2266
	movl	%r13d, (%rcx,%rdi)	# tmp2266, MEM[base: src_1910, index: _180, offset: 0B]
	movl	(%rcx,%r8), %edx	# MEM[base: src_1910, index: _578, offset: 0B], a
	movl	%edx, %r13d	# a, D.15188
	orl	%eax, %r13d	# b, D.15188
	xorl	%edx, %eax	# a, D.15188
	andl	$-16843010, %eax	#, D.15191
	shrq	%rax	# D.15191
	subl	%eax, %r13d	# D.15191, tmp2272
	movl	%r13d, (%rcx,%r10)	# tmp2272, MEM[base: src_1910, index: _602, offset: 0B]
	movl	(%rcx,%r11), %eax	# MEM[base: src_1910, index: _613, offset: 0B], b
	movl	%eax, %r13d	# b, D.15188
	orl	%edx, %r13d	# a, D.15188
	xorl	%eax, %edx	# b, D.15188
	andl	$-16843010, %edx	#, D.15191
	shrq	%rdx	# D.15191
	subl	%edx, %r13d	# D.15191, tmp2278
	movl	%r13d, (%rcx,%rbx)	# tmp2278, MEM[base: src_1910, index: _618, offset: 0B]
	movl	(%rcx,%rbp), %r13d	# MEM[base: src_1910, index: _629, offset: 0B], a
	movl	%r13d, %edx	# a, D.15188
	orl	%eax, %edx	# b, D.15188
	xorl	%r13d, %eax	# a, D.15188
	andl	$-16843010, %eax	#, D.15191
	shrq	%rax	# D.15191
	subl	%eax, %edx	# D.15191, tmp2284
	movl	%edx, (%rcx,%r12)	# tmp2284, MEM[base: src_1910, index: _634, offset: 0B]
	addq	$4, %rcx	#, src
	cmpq	%rsi, %rcx	# D.15186, src
	jne	.L533	#,
	jmp	.L534	#
.L532:
	testl	$131072, %r14d	#, D.15188
	je	.L535	#,
	movslq	16(%rsp), %rsi	# %sfp, D.15185
	addq	1944(%rsp), %rsi	# c.deintTemp, tmp
	movq	8(%rsp), %rcx	# %sfp, src
	movq	168(%rsp), %rax	# %sfp, D.15185
	addq	%rax, %rcx	# D.15185, src
	leaq	8(%rsi), %r8	#, D.15186
	movq	%rax, %r9	# D.15185, D.15185
	movq	216(%rsp), %r10	# %sfp, D.15185
	movq	56(%rsp), %r11	# %sfp, D.15185
	movq	88(%rsp), %rbx	# %sfp, D.15185
	movq	224(%rsp), %rbp	# %sfp, D.15185
	movq	208(%rsp), %r13	# %sfp, D.15185
	movq	232(%rsp), %r12	# %sfp, D.15185
	movl	%r14d, 240(%rsp)	# D.15188, %sfp
	movq	64(%rsp), %r14	# %sfp, D.15185
.L536:
	movl	(%rsi), %r15d	# MEM[base: tmp_1912, offset: 0B], a
	movl	(%rcx), %edx	# MEM[base: src_1913, offset: 0B], b
	movl	(%rcx,%r14), %edi	# MEM[base: src_1913, index: _180, offset: 0B], c
	movl	%edi, %eax	# c, D.15188
	xorl	%r15d, %eax	# a, D.15188
	andl	$-16843010, %eax	#, D.15191
	shrq	%rax	# D.15191
	andl	%edi, %r15d	# c, D.15188
	addl	%r15d, %eax	# D.15188, D.15189
	movl	%eax, %r15d	# D.15189, D.15188
	orl	%edx, %r15d	# b, D.15188
	xorl	%edx, %eax	# b, D.15188
	andl	$-16843010, %eax	#, D.15191
	shrq	%rax	# D.15191
	subl	%eax, %r15d	# D.15191, tmp2298
	movl	%r15d, (%rcx)	# tmp2298, MEM[base: src_1913, offset: 0B]
	movl	(%rcx,%r10), %eax	# MEM[base: src_1913, index: _584, offset: 0B], a
	movl	%eax, %r15d	# a, D.15188
	xorl	%edx, %r15d	# b, D.15188
	andl	$-16843010, %r15d	#, D.15191
	shrq	%r15	# D.15191
	andl	%eax, %edx	# a, D.15188
	addl	%r15d, %edx	# D.15191, D.15189
	movl	%edx, %r15d	# D.15189, D.15188
	orl	%edi, %r15d	# c, D.15188
	xorl	%edi, %edx	# c, D.15188
	andl	$-16843010, %edx	#, D.15191
	shrq	%rdx	# D.15191
	subl	%edx, %r15d	# D.15191, tmp2309
	movl	%r15d, (%rcx,%r14)	# tmp2309, MEM[base: src_1913, index: _180, offset: 0B]
	movl	(%rcx,%r11), %edx	# MEM[base: src_1913, index: _602, offset: 0B], b
	movl	%edx, %r15d	# b, D.15188
	xorl	%edi, %r15d	# c, D.15188
	andl	$-16843010, %r15d	#, D.15191
	shrq	%r15	# D.15191
	andl	%edx, %edi	# b, D.15188
	addl	%r15d, %edi	# D.15191, D.15189
	movl	%edi, %r15d	# D.15189, D.15188
	orl	%eax, %r15d	# a, D.15188
	xorl	%eax, %edi	# a, D.15188
	andl	$-16843010, %edi	#, D.15191
	shrq	%rdi	# D.15191
	subl	%edi, %r15d	# D.15191, tmp2320
	movl	%r15d, (%rcx,%r10)	# tmp2320, MEM[base: src_1913, index: _584, offset: 0B]
	movl	(%rcx,%r9), %edi	# MEM[base: src_1913, index: _578, offset: 0B], c
	movl	%edi, %r15d	# c, D.15188
	xorl	%eax, %r15d	# a, D.15188
	andl	$-16843010, %r15d	#, D.15191
	shrq	%r15	# D.15191
	andl	%edi, %eax	# c, D.15188
	addl	%r15d, %eax	# D.15191, D.15189
	movl	%eax, %r15d	# D.15189, D.15188
	orl	%edx, %r15d	# b, D.15188
	xorl	%edx, %eax	# b, D.15188
	andl	$-16843010, %eax	#, D.15191
	shrq	%rax	# D.15191
	subl	%eax, %r15d	# D.15191, tmp2331
	movl	%r15d, (%rcx,%r11)	# tmp2331, MEM[base: src_1913, index: _602, offset: 0B]
	movl	(%rcx,%rbp), %eax	# MEM[base: src_1913, index: _618, offset: 0B], a
	movl	%eax, %r15d	# a, D.15188
	xorl	%edx, %r15d	# b, D.15188
	andl	$-16843010, %r15d	#, D.15191
	shrq	%r15	# D.15191
	andl	%eax, %edx	# a, D.15188
	addl	%r15d, %edx	# D.15191, D.15189
	movl	%edx, %r15d	# D.15189, D.15188
	orl	%edi, %r15d	# c, D.15188
	xorl	%edi, %edx	# c, D.15188
	andl	$-16843010, %edx	#, D.15191
	shrq	%rdx	# D.15191
	subl	%edx, %r15d	# D.15191, tmp2342
	movl	%r15d, (%rcx,%r9)	# tmp2342, MEM[base: src_1913, index: _578, offset: 0B]
	movl	(%rcx,%rbx), %edx	# MEM[base: src_1913, index: _613, offset: 0B], b
	movl	%edx, %r15d	# b, D.15188
	xorl	%edi, %r15d	# c, D.15188
	andl	$-16843010, %r15d	#, D.15191
	shrq	%r15	# D.15191
	andl	%edx, %edi	# b, D.15188
	addl	%r15d, %edi	# D.15191, D.15189
	movl	%edi, %r15d	# D.15189, D.15188
	orl	%eax, %r15d	# a, D.15188
	xorl	%eax, %edi	# a, D.15188
	andl	$-16843010, %edi	#, D.15191
	shrq	%rdi	# D.15191
	subl	%edi, %r15d	# D.15191, tmp2353
	movl	%r15d, (%rcx,%rbp)	# tmp2353, MEM[base: src_1913, index: _618, offset: 0B]
	movl	(%rcx,%r12), %edi	# MEM[base: src_1913, index: _634, offset: 0B], D.15193
	movl	%edi, %r15d	# D.15193, D.15188
	xorl	%eax, %r15d	# a, D.15188
	andl	$-16843010, %r15d	#, D.15191
	shrq	%r15	# D.15191
	andl	%edi, %eax	# D.15193, D.15188
	addl	%r15d, %eax	# D.15191, D.15189
	movl	%eax, %r15d	# D.15189, D.15188
	orl	%edx, %r15d	# b, D.15188
	xorl	%edx, %eax	# b, D.15188
	andl	$-16843010, %eax	#, D.15191
	shrq	%rax	# D.15191
	subl	%eax, %r15d	# D.15191, tmp2364
	movl	%r15d, (%rcx,%rbx)	# tmp2364, MEM[base: src_1913, index: _613, offset: 0B]
	movl	(%rcx,%r13), %r15d	# MEM[base: src_1913, index: _629, offset: 0B], a
	movl	%r15d, %eax	# a, D.15188
	xorl	%edx, %eax	# b, D.15188
	andl	$-16843010, %eax	#, D.15191
	shrq	%rax	# D.15191
	andl	%r15d, %edx	# a, D.15188
	addl	%edx, %eax	# D.15188, D.15189
	movl	%eax, %edx	# D.15189, D.15188
	orl	%edi, %edx	# D.15193, D.15188
	xorl	%edi, %eax	# D.15193, D.15188
	andl	$-16843010, %eax	#, D.15191
	shrq	%rax	# D.15191
	subl	%eax, %edx	# D.15191, tmp2375
	movl	%edx, (%rcx,%r12)	# tmp2375, MEM[base: src_1913, index: _634, offset: 0B]
	movl	%edi, (%rsi)	# D.15193, MEM[base: tmp_1912, offset: 0B]
	addq	$4, %rcx	#, src
	addq	$4, %rsi	#, tmp
	cmpq	%r8, %rsi	# D.15186, tmp
	jne	.L536	#,
	movl	240(%rsp), %r14d	# %sfp, D.15188
	jmp	.L534	#
.L535:
	testl	$524288, %r14d	#, D.15188
	je	.L537	#,
	movq	8(%rsp), %rcx	# %sfp, D.15186
	addq	168(%rsp), %rcx	# %sfp, D.15186
	movslq	36(%rsp), %rsi	# %sfp, D.15197
#APP
# 1926 "postprocess_template.c" 1
	lea (%rcx, %rsi), %rax                	# D.15186, D.15197
	lea (%rax, %rsi, 4), %rdx      	# D.15197
	pxor %mm7, %mm7                      
	movq (%rcx), %mm0                     	# D.15186
	movq (%rax), %mm2                     
	movq (%rax, %rsi), %mm1                     	# D.15197
	movq %mm0, %mm3                      
	movq %mm1, %mm4                      
	movq %mm2, %mm5                      
	psubusb %mm1, %mm3                   
	psubusb %mm2, %mm4                   
	psubusb %mm0, %mm5                   
	pcmpeqb %mm7, %mm3                   
	pcmpeqb %mm7, %mm4                   
	pcmpeqb %mm7, %mm5                   
	movq %mm3, %mm6                      
	pxor %mm4, %mm3                      
	pxor %mm5, %mm4                      
	pxor %mm6, %mm5                      
	por %mm3, %mm1                       
	por %mm4, %mm2                       
	por %mm5, %mm0                       
	pand %mm2, %mm0                      
	pand %mm1, %mm0                      
	movq %mm0, (%rax)                     
	movq (%rax, %rsi), %mm0                     	# D.15197
	movq (%rax, %rsi, 2), %mm2                     	# D.15197
	movq (%rcx, %rsi, 4), %mm1                     	# D.15186, D.15197
	movq %mm0, %mm3                      
	movq %mm1, %mm4                      
	movq %mm2, %mm5                      
	psubusb %mm1, %mm3                   
	psubusb %mm2, %mm4                   
	psubusb %mm0, %mm5                   
	pcmpeqb %mm7, %mm3                   
	pcmpeqb %mm7, %mm4                   
	pcmpeqb %mm7, %mm5                   
	movq %mm3, %mm6                      
	pxor %mm4, %mm3                      
	pxor %mm5, %mm4                      
	pxor %mm6, %mm5                      
	por %mm3, %mm1                       
	por %mm4, %mm2                       
	por %mm5, %mm0                       
	pand %mm2, %mm0                      
	pand %mm1, %mm0                      
	movq %mm0, (%rax, %rsi, 2)                     	# D.15197
	movq (%rcx, %rsi, 4), %mm0                     	# D.15186, D.15197
	movq (%rdx), %mm2                     
	movq (%rdx, %rsi), %mm1                     	# D.15197
	movq %mm0, %mm3                      
	movq %mm1, %mm4                      
	movq %mm2, %mm5                      
	psubusb %mm1, %mm3                   
	psubusb %mm2, %mm4                   
	psubusb %mm0, %mm5                   
	pcmpeqb %mm7, %mm3                   
	pcmpeqb %mm7, %mm4                   
	pcmpeqb %mm7, %mm5                   
	movq %mm3, %mm6                      
	pxor %mm4, %mm3                      
	pxor %mm5, %mm4                      
	pxor %mm6, %mm5                      
	por %mm3, %mm1                       
	por %mm4, %mm2                       
	por %mm5, %mm0                       
	pand %mm2, %mm0                      
	pand %mm1, %mm0                      
	movq %mm0, (%rdx)                     
	movq (%rdx, %rsi), %mm0                     	# D.15197
	movq (%rdx, %rsi, 2), %mm2                     	# D.15197
	movq (%rcx, %rsi, 8), %mm1                     	# D.15186, D.15197
	movq %mm0, %mm3                      
	movq %mm1, %mm4                      
	movq %mm2, %mm5                      
	psubusb %mm1, %mm3                   
	psubusb %mm2, %mm4                   
	psubusb %mm0, %mm5                   
	pcmpeqb %mm7, %mm3                   
	pcmpeqb %mm7, %mm4                   
	pcmpeqb %mm7, %mm5                   
	movq %mm3, %mm6                      
	pxor %mm4, %mm3                      
	pxor %mm5, %mm4                      
	pxor %mm6, %mm5                      
	por %mm3, %mm1                       
	por %mm4, %mm2                       
	por %mm5, %mm0                       
	pand %mm2, %mm0                      
	pand %mm1, %mm0                      
	movq %mm0, (%rdx, %rsi, 2)                     	# D.15197
	
# 0 "" 2
#NO_APP
	jmp	.L534	#
.L537:
	cmpl	$0, 296(%rsp)	#, %sfp
	je	.L538	#,
	movq	56(%rsp), %rdi	# %sfp, D.15201
	movq	88(%rsp), %rax	# %sfp, D.15185
	addq	%rax, %rdi	# D.15185, D.15201
	movq	8(%rsp), %rbx	# %sfp, dstBlock
	leaq	(%rbx,%rdi), %rsi	#, ivtmp.1667
	addq	72(%rsp), %rdi	# %sfp, D.15201
	movq	%rax, %r8	# D.15185, D.15185
	negq	%r8	# D.15185
	movq	168(%rsp), %r9	# %sfp, D.15185
	movq	216(%rsp), %r11	# %sfp, D.15185
	movq	224(%rsp), %rbp	# %sfp, D.15185
	movq	208(%rsp), %r10	# %sfp, D.15185
	movq	232(%rsp), %r12	# %sfp, D.15185
	movq	424(%rsp), %rbx	# %sfp, D.15185
	movq	384(%rsp), %r13	# %sfp, D.15185
	movl	%r14d, 240(%rsp)	# D.15188, %sfp
	movq	432(%rsp), %r14	# %sfp, D.15185
.L547:
	movq	%rsi, %rax	# ivtmp.1667, D.15187
	subq	88(%rsp), %rax	# %sfp, D.15187
	movq	%rsi, %rcx	# ivtmp.1667, D.15187
	movzbl	(%rax,%r11), %edx	# MEM[base: _742, index: _584, offset: 0B], D.15188
	leal	(%rdx,%rdx,8), %edx	#, D.15188
	movzbl	(%rsi,%r8), %r15d	# MEM[base: _700, index: _656, offset: 0B], D.15188
	subl	%r15d, %edx	# D.15188, D.15188
	movzbl	(%rax,%r9), %r15d	# MEM[base: _742, index: _578, offset: 0B], D.15188
	leal	(%r15,%r15,8), %r15d	#, D.15188
	addl	%r15d, %edx	# D.15188, D.15188
	movzbl	(%rsi), %r15d	# MEM[base: _700, offset: 0B], D.15188
	subl	%r15d, %edx	# D.15188, D.15188
	sarl	$4, %edx	#, D.15188
	movl	%edx, %r15d	# D.15188, D.15207
	testl	$-256, %edx	#, D.15188
	je	.L540	#,
	negl	%edx	# D.15188
	sarl	$31, %edx	#, tmp3508
	movl	%edx, %r15d	# tmp3508, D.15207
.L540:
	movq	56(%rsp), %rdx	# %sfp, D.15185
	movb	%r15b, (%rax,%rdx)	# D.15207, MEM[base: _742, index: _602, offset: 0B]
	movzbl	(%rax,%r9), %edx	# MEM[base: _742, index: _578, offset: 0B], D.15188
	leal	(%rdx,%rdx,8), %edx	#, D.15188
	movzbl	(%rax,%r11), %r15d	# MEM[base: _742, index: _584, offset: 0B], D.15188
	subl	%r15d, %edx	# D.15188, D.15188
	movzbl	(%rcx), %r15d	# MEM[base: _700, offset: 0B], D.15188
	leal	(%r15,%r15,8), %r15d	#, D.15188
	addl	%r15d, %edx	# D.15188, D.15188
	movzbl	(%rax,%r10), %r15d	# MEM[base: _742, index: _629, offset: 0B], D.15188
	subl	%r15d, %edx	# D.15188, D.15188
	sarl	$4, %edx	#, D.15188
	movl	%edx, %r15d	# D.15188, D.15207
	testl	$-256, %edx	#, D.15188
	je	.L542	#,
	negl	%edx	# D.15188
	sarl	$31, %edx	#, tmp3510
	movl	%edx, %r15d	# tmp3510, D.15207
.L542:
	movb	%r15b, (%rax,%rbp)	# D.15207, MEM[base: _742, index: _618, offset: 0B]
	movzbl	(%rcx), %edx	# MEM[base: _700, offset: 0B], D.15188
	leal	(%rdx,%rdx,8), %edx	#, D.15188
	movzbl	(%rax,%r9), %r15d	# MEM[base: _742, index: _578, offset: 0B], D.15188
	subl	%r15d, %edx	# D.15188, D.15188
	movzbl	(%rax,%r10), %r15d	# MEM[base: _742, index: _629, offset: 0B], D.15188
	leal	(%r15,%r15,8), %r15d	#, D.15188
	addl	%r15d, %edx	# D.15188, D.15188
	movzbl	(%rax,%rbx), %r15d	# MEM[base: _742, index: _908, offset: 0B], D.15188
	subl	%r15d, %edx	# D.15188, D.15188
	sarl	$4, %edx	#, D.15188
	movl	%edx, %r15d	# D.15188, D.15207
	testl	$-256, %edx	#, D.15188
	je	.L544	#,
	negl	%edx	# D.15188
	sarl	$31, %edx	#, tmp3511
	movl	%edx, %r15d	# tmp3511, D.15207
.L544:
	movb	%r15b, (%rax,%r12)	# D.15207, MEM[base: _742, index: _634, offset: 0B]
	movzbl	(%rax,%r10), %edx	# MEM[base: _742, index: _629, offset: 0B], D.15188
	leal	(%rdx,%rdx,8), %edx	#, D.15188
	movzbl	(%rcx), %ecx	# MEM[base: _700, offset: 0B], D.15188
	subl	%ecx, %edx	# D.15188, D.15188
	movzbl	(%rax,%rbx), %ecx	# MEM[base: _742, index: _908, offset: 0B], D.15188
	leal	(%rcx,%rcx,8), %ecx	#, D.15188
	addl	%ecx, %edx	# D.15188, D.15188
	movzbl	(%rax,%r14), %ecx	# MEM[base: _742, index: _934, offset: 0B], D.15188
	subl	%ecx, %edx	# D.15188, D.15188
	sarl	$4, %edx	#, D.15188
	movl	%edx, %ecx	# D.15188, D.15207
	testl	$-256, %edx	#, D.15188
	je	.L546	#,
	negl	%edx	# D.15188
	sarl	$31, %edx	#, tmp3512
	movl	%edx, %ecx	# tmp3512, D.15207
.L546:
	movb	%cl, (%rax,%r13)	# D.15207, MEM[base: _742, index: _921, offset: 0B]
	addq	$1, %rsi	#, ivtmp.1667
	cmpq	%rdi, %rsi	# D.15201, ivtmp.1667
	jne	.L547	#,
	movl	240(%rsp), %r14d	# %sfp, D.15188
	jmp	.L534	#
.L538:
	testl	$4194304, %r14d	#, D.15188
	je	.L548	#,
	movslq	16(%rsp), %rdi	# %sfp, D.15185
	movq	8(%rsp), %rsi	# %sfp, dstBlock
	movq	%rsi, %r11	# dstBlock, D.15186
	movq	168(%rsp), %rbp	# %sfp, D.15185
	addq	%rbp, %r11	# D.15185, D.15186
	movl	$0, %eax	#, ivtmp.1676
	addq	1944(%rsp), %rdi	# c.deintTemp, D.15186
	movq	%rbp, %rbx	# D.15185, D.15201
	addq	208(%rsp), %rbx	# %sfp, D.15201
	addq	%rsi, %rbx	# dstBlock, D.15186
	addq	384(%rsp), %rbp	# %sfp, D.15201
	addq	%rsi, %rbp	# dstBlock, D.15186
	movl	%r14d, 480(%rsp)	# D.15188, %sfp
	movq	128(%rsp), %rsi	# %sfp, ivtmp.1722
	movq	136(%rsp), %r8	# %sfp, ivtmp.1723
	movq	144(%rsp), %r9	# %sfp, ivtmp.1724
	movq	152(%rsp), %r10	# %sfp, ivtmp.1725
	movq	%r11, 240(%rsp)	# D.15186, %sfp
	movq	104(%rsp), %r11	# %sfp, ivtmp.1717
	movq	%rbx, 248(%rsp)	# D.15186, %sfp
	movq	112(%rsp), %rbx	# %sfp, ivtmp.1718
	movq	%rbp, 256(%rsp)	# D.15186, %sfp
	movq	120(%rsp), %rbp	# %sfp, ivtmp.1720
.L557:
	movq	%r9, %r14	# ivtmp.1724, D.15186
	movzbl	(%r9,%rax), %ecx	# MEM[base: _946, index: ivtmp.1676_1613, offset: 0B], t2
	movq	%r10, %r13	# ivtmp.1725, D.15186
	movq	%rsi, %r12	# ivtmp.1722, D.15186
	movq	240(%rsp), %rdx	# %sfp, D.15186
	movzbl	(%rdx,%rax), %edx	# MEM[base: _2073, index: ivtmp.1676_1613, offset: 0B], D.15188
	sall	$2, %edx	#, D.15188
	movzbl	(%rdi,%rax), %r15d	# MEM[base: _1915, index: ivtmp.1676_1613, offset: 0B], t1
	subl	%r15d, %edx	# t1, D.15188
	leal	(%rdx,%rcx,2), %edx	#, D.15188
	movzbl	(%r10,%rax), %r15d	# MEM[base: _922, index: ivtmp.1676_1613, offset: 0B], D.15188
	leal	(%rdx,%r15,4), %edx	#, D.15188
	movzbl	(%rsi,%rax), %r15d	# MEM[base: _896, index: ivtmp.1676_1613, offset: 0B], D.15188
	subl	%r15d, %edx	# D.15188, D.15188
	addl	$4, %edx	#, D.15188
	sarl	$3, %edx	#, D.15188
	movl	%edx, %r15d	# D.15188, D.15207
	testl	$-256, %edx	#, D.15188
	je	.L550	#,
	negl	%edx	# D.15188
	sarl	$31, %edx	#, tmp3523
	movl	%edx, %r15d	# tmp3523, D.15207
.L550:
	movb	%r15b, (%r14,%rax)	# D.15207, MEM[base: _946, index: ivtmp.1676_1613, offset: 0B]
	movq	%r8, %r15	# ivtmp.1723, D.15186
	movzbl	(%r8,%rax), %edx	# MEM[base: _30, index: ivtmp.1676_1613, offset: 0B], t1
	movq	%rbx, %r14	# ivtmp.1718, D.15186
	movzbl	0(%r13,%rax), %r13d	# MEM[base: _922, index: ivtmp.1676_1613, offset: 0B], D.15188
	sall	$2, %r13d	#, D.15188
	subl	%ecx, %r13d	# t2, D.15188
	leal	0(%r13,%rdx,2), %ecx	#, D.15188
	leal	(%rcx,%rdx,4), %ecx	#, D.15188
	movzbl	(%rbx,%rax), %r13d	# MEM[base: _1994, index: ivtmp.1676_1613, offset: 0B], D.15188
	subl	%r13d, %ecx	# D.15188, D.15188
	addl	$4, %ecx	#, D.15188
	sarl	$3, %ecx	#, D.15188
	movl	%ecx, %r13d	# D.15188, D.15207
	testl	$-256, %ecx	#, D.15188
	je	.L552	#,
	negl	%ecx	# D.15188
	sarl	$31, %ecx	#, tmp3524
	movl	%ecx, %r13d	# tmp3524, D.15207
.L552:
	movb	%r13b, (%r12,%rax)	# D.15207, MEM[base: _896, index: ivtmp.1676_1613, offset: 0B]
	movq	%rbp, %r13	# ivtmp.1720, D.15186
	movzbl	0(%rbp,%rax), %ecx	# MEM[base: _11, index: ivtmp.1676_1613, offset: 0B], t2
	movq	%r11, %r12	# ivtmp.1717, D.15186
	movzbl	(%r15,%rax), %r15d	# MEM[base: _30, index: ivtmp.1676_1613, offset: 0B], D.15188
	sall	$2, %r15d	#, D.15188
	subl	%edx, %r15d	# t1, D.15188
	leal	(%r15,%rcx,2), %edx	#, D.15188
	leal	(%rdx,%rcx,4), %edx	#, D.15188
	movzbl	(%r11,%rax), %r15d	# MEM[base: _10, index: ivtmp.1676_1613, offset: 0B], D.15188
	subl	%r15d, %edx	# D.15188, D.15188
	addl	$4, %edx	#, D.15188
	sarl	$3, %edx	#, D.15188
	movl	%edx, %r15d	# D.15188, D.15207
	testl	$-256, %edx	#, D.15188
	je	.L554	#,
	negl	%edx	# D.15188
	sarl	$31, %edx	#, tmp3525
	movl	%edx, %r15d	# tmp3525, D.15207
.L554:
	movb	%r15b, (%r14,%rax)	# D.15207, MEM[base: _1994, index: ivtmp.1676_1613, offset: 0B]
	movq	248(%rsp), %rdx	# %sfp, D.15186
	movzbl	(%rdx,%rax), %edx	# MEM[base: _2067, index: ivtmp.1676_1613, offset: 0B], D.15204
	movzbl	%dl, %r14d	# D.15204, t1
	movzbl	0(%r13,%rax), %r13d	# MEM[base: _11, index: ivtmp.1676_1613, offset: 0B], D.15188
	sall	$2, %r13d	#, D.15188
	subl	%ecx, %r13d	# t2, D.15188
	leal	0(%r13,%r14,2), %ecx	#, D.15188
	leal	(%rcx,%r14,4), %ecx	#, D.15188
	movq	256(%rsp), %r14	# %sfp, D.15186
	movzbl	(%r14,%rax), %r13d	# MEM[base: _2071, index: ivtmp.1676_1613, offset: 0B], D.15188
	subl	%r13d, %ecx	# D.15188, D.15188
	addl	$4, %ecx	#, D.15188
	sarl	$3, %ecx	#, D.15188
	movl	%ecx, %r13d	# D.15188, D.15207
	testl	$-256, %ecx	#, D.15188
	je	.L556	#,
	negl	%ecx	# D.15188
	sarl	$31, %ecx	#, tmp3528
	movl	%ecx, %r13d	# tmp3528, D.15207
.L556:
	movb	%r13b, (%r12,%rax)	# D.15207, MEM[base: _10, index: ivtmp.1676_1613, offset: 0B]
	movb	%dl, (%rdi,%rax)	# D.15204, MEM[base: _1915, index: ivtmp.1676_1613, offset: 0B]
	addq	$1, %rax	#, ivtmp.1676
	cmpq	$8, %rax	#, ivtmp.1676
	jne	.L557	#,
	movl	480(%rsp), %r14d	# %sfp, D.15188
	jmp	.L534	#
.L548:
	testl	$8388608, %r14d	#, D.15188
	je	.L534	#,
	movq	1944(%rsp), %rdx	# c.deintTemp, D.15186
	movslq	16(%rsp), %rax	# %sfp, D.15185
	movslq	96(%rsp), %rcx	# %sfp, D.15185
	addq	%rax, %rcx	# D.15185, D.15185
	addq	%rdx, %rcx	# D.15186, D.15186
	addq	%rax, %rdx	# D.15185, D.15186
	movl	36(%rsp), %esi	# %sfp,
	movq	8(%rsp), %rdi	# %sfp,
	call	deInterlaceL5_MMX	#
.L534:
	movl	180(%rsp), %eax	# %sfp, D.15189
	addl	$8, %eax	#, D.15189
	cmpl	%eax, 100(%rsp)	# D.15189, %sfp
	jle	.L558	#,
	testl	$512, %r14d	#, D.15188
	je	.L559	#,
	movl	36(%rsp), %eax	# %sfp, dstStride
	movl	%eax, %r9d	# dstStride, l6
	movl	324(%rsp), %ebx	# %sfp, l5
	addl	%ebx, %r9d	# l5, l6
	leal	(%rax,%r9), %r12d	#, l7
	movslq	316(%rsp), %r13	# %sfp, D.15185
	movq	56(%rsp), %rax	# %sfp, D.15185
	leaq	(%rax,%r13), %rbp	#, D.15201
	movq	8(%rsp), %rax	# %sfp, dstBlock
	leaq	(%rax,%rbp), %rdi	#, ivtmp.1615
	addq	72(%rsp), %rbp	# %sfp, D.15201
	movq	%rbp, %rax	# D.15201, D.15201
	movslq	320(%rsp), %r11	# %sfp, D.15185
	movslq	%ebx, %r10	# l5, D.15185
	movslq	%r9d, %r9	# l6, D.15185
	movslq	312(%rsp), %rbp	# %sfp, D.15185
	movslq	%r12d, %r12	# l7, D.15185
	movl	%r14d, 240(%rsp)	# D.15188, %sfp
	movq	%rax, %r14	# D.15201, D.15201
.L562:
	movq	%rdi, %rcx	# ivtmp.1615, D.15187
	subq	%r13, %rcx	# D.15185, D.15187
	movzbl	(%rcx,%r11), %r15d	# MEM[base: _1701, index: _1082, offset: 0B], D.15188
	movzbl	(%rcx,%r10), %eax	# MEM[base: _1701, index: _1087, offset: 0B], D.15188
	movl	%r15d, %r8d	# D.15188, b
	subl	%eax, %r8d	# D.15188, b
	movl	%r8d, %esi	# b, tmp2510
	sarl	$31, %esi	#, tmp2510
	movl	%esi, %edx	# tmp2510, tmp2511
	xorl	%r8d, %edx	# b, tmp2511
	subl	%esi, %edx	# tmp2510, D.15188
	movzbl	(%rdi), %esi	# MEM[base: _1934, offset: 0B], D.15188
	subl	%r15d, %esi	# D.15188, a
	movl	%esi, %r15d	# a, tmp2515
	sarl	$31, %r15d	#, tmp2515
	xorl	%r15d, %esi	# tmp2515, tmp2516
	subl	%r15d, %esi	# tmp2515, D.15188
	movzbl	(%rcx,%r9), %r15d	# MEM[base: _1701, index: _1092, offset: 0B], D.15188
	subl	%r15d, %eax	# D.15188, c
	movl	%eax, %r15d	# c, tmp2520
	sarl	$31, %r15d	#, tmp2520
	xorl	%r15d, %eax	# tmp2520, tmp2521
	subl	%r15d, %eax	# tmp2520, D.15188
	addl	%eax, %esi	# D.15188, D.15188
	sarl	%esi	# D.15188
	subl	%esi, %edx	# D.15188, d
	movl	$0, %eax	#, tmp3550
	cmovs	%eax, %edx	# d,, tmp3550, d
	movl	3016(%rsp), %eax	# c.QP, tmp3551
	addl	%eax, %eax	# D.15188
	cmpl	%eax, %edx	# D.15188, d
	jge	.L560	#,
	negl	%r8d	# D.15188
	testl	%r8d, %r8d	# D.15188
	setg	%al	#, D.15188
	movzbl	%al, %eax	# D.15188, D.15188
	leal	-1(%rax,%rax), %eax	#, D.15188
	imull	%eax, %edx	# D.15188, v
	movl	%edx, %esi	# v, D.15188
	sarl	$3, %esi	#, D.15188
	addb	%sil, (%rcx,%rbp)	# D.15188, MEM[base: _1701, index: _1109, offset: 0B]
	movl	%edx, %r8d	# v, D.15188
	sarl	$2, %r8d	#, D.15188
	addb	%r8b, (%rdi)	# D.15188, MEM[base: _1934, offset: 0B]
	leal	(%rdx,%rdx,2), %eax	#, D.15188
	sarl	$3, %eax	#, D.15188
	addb	%al, (%rcx,%r11)	# D.15188, MEM[base: _1701, index: _1082, offset: 0B]
	subb	%al, (%rcx,%r10)	# D.15188, MEM[base: _1701, index: _1087, offset: 0B]
	subb	%r8b, (%rcx,%r9)	# D.15188, MEM[base: _1701, index: _1092, offset: 0B]
	subb	%sil, (%rcx,%r12)	# D.15188, MEM[base: _1701, index: _1128, offset: 0B]
.L560:
	addq	$1, %rdi	#, ivtmp.1615
	cmpq	%r14, %rdi	# D.15201, ivtmp.1615
	jne	.L562	#,
	movl	240(%rsp), %r14d	# %sfp, D.15188
	jmp	.L558	#
.L559:
	testb	$1, %r14b	#, D.15188
	je	.L563	#,
	movslq	3020(%rsp), %rax	# c.nonBQP, D.15188
#APP
# 114 "postprocess_template.c" 1
	movq 1968(%rsp,%rax,8), %mm7                         	# c.mmxDcOffset
	movq 2480(%rsp,%rax,8), %mm6                         	# c.mmxDcThreshold
	
# 0 "" 2
#NO_APP
	movq	8(%rsp), %rdx	# %sfp, D.15200
	addq	168(%rsp), %rdx	# %sfp, D.15200
	movslq	36(%rsp), %rcx	# %sfp, D.15197
#APP
# 120 "postprocess_template.c" 1
	lea (%rdx, %rcx), %rax                	# D.15200, D.15197
	movq (%rdx), %mm0                       	# D.15200
	movq (%rax), %mm1                
	movq %mm0, %mm3                      
	movq %mm0, %mm4                      
	psubusb %mm1, %mm4 
	paddb %mm1, %mm4 
	movq %mm3, %mm5 
	psubusb %mm1, %mm5 
	psubb %mm5, %mm3 
	psubb %mm1, %mm0                     
	paddb %mm7, %mm0                     
	pcmpgtb %mm6, %mm0                   
	movq (%rax,%rcx), %mm2             	# D.15197
	psubusb %mm2, %mm4 
	paddb %mm2, %mm4 
	movq %mm3, %mm5 
	psubusb %mm2, %mm5 
	psubb %mm5, %mm3 
	psubb %mm2, %mm1                     
	paddb %mm7, %mm1                     
	pcmpgtb %mm6, %mm1                   
	paddb %mm1, %mm0                     
	movq (%rax, %rcx, 2), %mm1         	# D.15197
	psubusb %mm1, %mm4 
	paddb %mm1, %mm4 
	movq %mm3, %mm5 
	psubusb %mm1, %mm5 
	psubb %mm5, %mm3 
	psubb %mm1, %mm2                     
	paddb %mm7, %mm2                     
	pcmpgtb %mm6, %mm2                   
	paddb %mm2, %mm0                     
	lea (%rax, %rcx, 4), %rax      	# D.15197
	movq (%rdx, %rcx, 4), %mm2                	# D.15200, D.15197
	psubusb %mm2, %mm4 
	paddb %mm2, %mm4 
	movq %mm3, %mm5 
	psubusb %mm2, %mm5 
	psubb %mm5, %mm3 
	psubb %mm2, %mm1                     
	paddb %mm7, %mm1                     
	pcmpgtb %mm6, %mm1                   
	paddb %mm1, %mm0                     
	movq (%rax), %mm1                
	psubusb %mm1, %mm4 
	paddb %mm1, %mm4 
	movq %mm3, %mm5 
	psubusb %mm1, %mm5 
	psubb %mm5, %mm3 
	psubb %mm1, %mm2                     
	paddb %mm7, %mm2                     
	pcmpgtb %mm6, %mm2                   
	paddb %mm2, %mm0                     
	movq (%rax, %rcx), %mm2            	# D.15197
	psubusb %mm2, %mm4 
	paddb %mm2, %mm4 
	movq %mm3, %mm5 
	psubusb %mm2, %mm5 
	psubb %mm5, %mm3 
	psubb %mm2, %mm1                     
	paddb %mm7, %mm1                     
	pcmpgtb %mm6, %mm1                   
	paddb %mm1, %mm0                     
	movq (%rax, %rcx, 2), %mm1         	# D.15197
	psubusb %mm1, %mm4 
	paddb %mm1, %mm4 
	movq %mm3, %mm5 
	psubusb %mm1, %mm5 
	psubb %mm5, %mm3 
	psubb %mm1, %mm2                     
	paddb %mm7, %mm2                     
	pcmpgtb %mm6, %mm2                   
	paddb %mm2, %mm0                     
	psubusb %mm3, %mm4                   
	                                       
	movq %mm0, %mm1                      
	psrlw $8, %mm0                        
	paddb %mm1, %mm0                     
	movq %mm0, %mm1                      
	psrlq $16, %mm0                       
	paddb %mm1, %mm0                     
	movq %mm0, %mm1                      
	psrlq $32, %mm0                       
	paddb %mm1, %mm0                     
	movq 1952(%rsp), %mm7                         	# c.pQPb
	paddusb %mm7, %mm7                   
	psubusb %mm7, %mm4                   
	packssdw %mm4, %mm4                  
	movd %mm0, %edx                         	# numEq
	movd %mm4, %ecx                         	# dcOk
	
# 0 "" 2
#NO_APP
	negl	%edx	# D.15188
	movzbl	%dl, %eax	# D.15188, numEq
	cmpl	3088(%rsp), %eax	# c.ppMode.flatnessThreshold, numEq
	jle	.L564	#,
	testl	%ecx, %ecx	# dcOk
	jne	.L558	#,
	leaq	1840(%rsp), %rdx	#, tmp3557
	movl	36(%rsp), %esi	# %sfp,
	movq	8(%rsp), %rdi	# %sfp,
	call	doVertLowPass_MMX	#
	jmp	.L558	#
.L564:
	leaq	1840(%rsp), %rdx	#, tmp3558
	movl	36(%rsp), %esi	# %sfp,
	movq	8(%rsp), %rdi	# %sfp,
	call	doVertDefFilter_MMX	#
	jmp	.L558	#
.L563:
	testl	$1024, %r14d	#, D.15188
	je	.L558	#,
	movq	8(%rsp), %rdx	# %sfp, D.15186
	addq	56(%rsp), %rdx	# %sfp, D.15186
	movslq	3020(%rsp), %rax	# c.nonBQP, D.15188
#APP
# 2553 "postprocess_template.c" 1
	movq 1968(%rsp,%rax,8), %mm7                         	# c.mmxDcOffset
	movq 2480(%rsp,%rax,8), %mm6                         	# c.mmxDcThreshold
	
# 0 "" 2
#NO_APP
	movslq	36(%rsp), %rcx	# %sfp, D.15197
#APP
# 2559 "postprocess_template.c" 1
	lea (%rdx, %rcx), %rax                	# D.15186, D.15197
	movq (%rdx), %mm0                       	# D.15186
	movq (%rax), %mm1                
	movq %mm1, %mm3                      
	movq %mm1, %mm4                      
	psubb %mm1, %mm0                     
	paddb %mm7, %mm0                     
	pcmpgtb %mm6, %mm0                   
	movq (%rax,%rcx), %mm2             	# D.15197
	psubusb %mm2, %mm4 
	paddb %mm2, %mm4 
	movq %mm3, %mm5 
	psubusb %mm2, %mm5 
	psubb %mm5, %mm3 
	psubb %mm2, %mm1                     
	paddb %mm7, %mm1                     
	pcmpgtb %mm6, %mm1                   
	paddb %mm1, %mm0                     
	movq (%rax, %rcx, 2), %mm1         	# D.15197
	psubusb %mm1, %mm4 
	paddb %mm1, %mm4 
	movq %mm3, %mm5 
	psubusb %mm1, %mm5 
	psubb %mm5, %mm3 
	psubb %mm1, %mm2                     
	paddb %mm7, %mm2                     
	pcmpgtb %mm6, %mm2                   
	paddb %mm2, %mm0                     
	lea (%rax, %rcx, 4), %rax      	# D.15197
	movq (%rdx, %rcx, 4), %mm2                	# D.15186, D.15197
	psubusb %mm2, %mm4 
	paddb %mm2, %mm4 
	movq %mm3, %mm5 
	psubusb %mm2, %mm5 
	psubb %mm5, %mm3 
	psubb %mm2, %mm1                     
	paddb %mm7, %mm1                     
	pcmpgtb %mm6, %mm1                   
	paddb %mm1, %mm0                     
	movq (%rax), %mm1                
	psubusb %mm1, %mm4 
	paddb %mm1, %mm4 
	movq %mm3, %mm5 
	psubusb %mm1, %mm5 
	psubb %mm5, %mm3 
	psubb %mm1, %mm2                     
	paddb %mm7, %mm2                     
	pcmpgtb %mm6, %mm2                   
	paddb %mm2, %mm0                     
	movq (%rax, %rcx), %mm2            	# D.15197
	psubusb %mm2, %mm4 
	paddb %mm2, %mm4 
	movq %mm3, %mm5 
	psubusb %mm2, %mm5 
	psubb %mm5, %mm3 
	psubb %mm2, %mm1                     
	paddb %mm7, %mm1                     
	pcmpgtb %mm6, %mm1                   
	paddb %mm1, %mm0                     
	movq (%rax, %rcx, 2), %mm1         	# D.15197
	psubusb %mm1, %mm4 
	paddb %mm1, %mm4 
	movq %mm3, %mm5 
	psubusb %mm1, %mm5 
	psubb %mm5, %mm3 
	psubb %mm1, %mm2                     
	paddb %mm7, %mm2                     
	pcmpgtb %mm6, %mm2                   
	paddb %mm2, %mm0                     
	movq (%rdx, %rcx, 8), %mm2                	# D.15186, D.15197
	psubusb %mm2, %mm4 
	paddb %mm2, %mm4 
	movq %mm3, %mm5 
	psubusb %mm2, %mm5 
	psubb %mm5, %mm3 
	psubb %mm2, %mm1                     
	paddb %mm7, %mm1                     
	pcmpgtb %mm6, %mm1                   
	paddb %mm1, %mm0                     
	movq (%rax, %rcx, 4), %mm1         	# D.15197
	psubb %mm1, %mm2                     
	paddb %mm7, %mm2                     
	pcmpgtb %mm6, %mm2                   
	paddb %mm2, %mm0                     
	psubusb %mm3, %mm4                   
	pxor %mm6, %mm6                      
	movq 1952(%rsp), %mm7                         	# c.pQPb
	paddusb %mm7, %mm7                   
	psubusb %mm4, %mm7                   
	pcmpeqb %mm6, %mm7                   
	pcmpeqb %mm6, %mm7                   
	movq %mm7, 504(%rsp)                         	# dc_mask
	movq 3088(%rsp), %mm7                         	# c.ppMode.flatnessThreshold
	punpcklbw %mm7, %mm7                 
	punpcklbw %mm7, %mm7                 
	punpcklbw %mm7, %mm7                 
	psubb %mm0, %mm6                     
	pcmpgtb %mm7, %mm6                   
	movq %mm6, 512(%rsp)                         	# eq_mask
	
# 0 "" 2
#NO_APP
	movq	512(%rsp), %rax	# eq_mask, D.15197
	movq	%rax, %rsi	# D.15197, D.15197
	andq	504(%rsp), %rsi	# dc_mask, D.15197
	movq	%rsi, 520(%rsp)	# D.15197, both_masks
	testq	%rsi, %rsi	# D.15197
	je	.L565	#,
	leaq	560(%rsp), %rsi	#, tmp2553
	movq	%rdx, %rbx	# src, src
#APP
# 2664 "postprocess_template.c" 1
	movq 1952(%rsp), %mm0                         	# c.pQPb
	pxor %mm4, %mm4                      
	movq (%rdx), %mm6                       	# src
	movq (%rdx, %rcx), %mm5                   	# src, D.15197
	movq %mm5, %mm1                      
	movq %mm6, %mm2                      
	psubusb %mm6, %mm5                   
	psubusb %mm1, %mm2                   
	por %mm5, %mm2                       
	psubusb %mm2, %mm0                   
	pcmpeqb %mm4, %mm0                   
	pxor %mm6, %mm1                      
	pand %mm0, %mm1                      
	pxor %mm1, %mm6                      
	movq (%rdx, %rcx, 8), %mm5                	# src, D.15197
	add %rcx, %rdx                             	# D.15197, src
	movq (%rdx, %rcx, 8), %mm7                	# src, D.15197
	movq %mm5, %mm1                      
	movq %mm7, %mm2                      
	psubusb %mm7, %mm5                   
	psubusb %mm1, %mm2                   
	por %mm5, %mm2                       
	movq 1952(%rsp), %mm0                         	# c.pQPb
	psubusb %mm2, %mm0                   
	pcmpeqb %mm4, %mm0                   
	pxor %mm7, %mm1                      
	pand %mm0, %mm1                      
	pxor %mm1, %mm7                      
	movq %mm6, %mm5                      
	punpckhbw %mm4, %mm6                 
	punpcklbw %mm4, %mm5                 
	movq %mm5, %mm0                      
	movq %mm6, %mm1                      
	psllw $2, %mm0                        
	psllw $2, %mm1                        
	paddw w04, %mm0             
	paddw w04, %mm1             
	movq (%rdx), %mm2                       	# src
	movq (%rdx), %mm3                       	# src
	add %rcx, %rdx                             	# D.15197, src
	punpcklbw %mm4, %mm2                 
	punpckhbw %mm4, %mm3                 
	paddw %mm2, %mm0                     
	paddw %mm3, %mm1                     
	movq (%rdx), %mm2                       	# src
	movq (%rdx), %mm3                       	# src
	add %rcx, %rdx                             	# D.15197, src
	punpcklbw %mm4, %mm2                 
	punpckhbw %mm4, %mm3                 
	paddw %mm2, %mm0                     
	paddw %mm3, %mm1                     
	movq (%rdx), %mm2                       	# src
	movq (%rdx), %mm3                       	# src
	add %rcx, %rdx                             	# D.15197, src
	punpcklbw %mm4, %mm2                 
	punpckhbw %mm4, %mm3                 
	paddw %mm2, %mm0                     
	paddw %mm3, %mm1                     
	movq %mm0, (%rsi)                       	# tmp2553
	movq %mm1, 8(%rsi)                      	# tmp2553
	movq (%rdx), %mm2                       	# src
	movq (%rdx), %mm3                       	# src
	add %rcx, %rdx                             	# D.15197, src
	punpcklbw %mm4, %mm2                 
	punpckhbw %mm4, %mm3                 
	paddw %mm2, %mm0                     
	paddw %mm3, %mm1                     
	psubw %mm5, %mm0                     
	psubw %mm6, %mm1                     
	movq %mm0, 16(%rsi)                     	# tmp2553
	movq %mm1, 24(%rsi)                     	# tmp2553
	movq (%rdx), %mm2                       	# src
	movq (%rdx), %mm3                       	# src
	add %rcx, %rdx                             	# D.15197, src
	punpcklbw %mm4, %mm2                 
	punpckhbw %mm4, %mm3                 
	paddw %mm2, %mm0                     
	paddw %mm3, %mm1                     
	psubw %mm5, %mm0                     
	psubw %mm6, %mm1                     
	movq %mm0, 32(%rsi)                     	# tmp2553
	movq %mm1, 40(%rsi)                     	# tmp2553
	movq (%rdx), %mm2                       	# src
	movq (%rdx), %mm3                       	# src
	add %rcx, %rdx                             	# D.15197, src
	punpcklbw %mm4, %mm2                 
	punpckhbw %mm4, %mm3                 
	paddw %mm2, %mm0                     
	paddw %mm3, %mm1                     
	psubw %mm5, %mm0                     
	psubw %mm6, %mm1                     
	movq %mm0, 48(%rsi)                     	# tmp2553
	movq %mm1, 56(%rsi)                     	# tmp2553
	movq (%rdx), %mm2                       	# src
	movq (%rdx), %mm3                       	# src
	add %rcx, %rdx                             	# D.15197, src
	punpcklbw %mm4, %mm2                 
	punpckhbw %mm4, %mm3                 
	paddw %mm2, %mm0                     
	paddw %mm3, %mm1                     
	psubw %mm5, %mm0                     
	psubw %mm6, %mm1                     
	movq %mm0, 64(%rsi)                     	# tmp2553
	movq %mm1, 72(%rsi)                     	# tmp2553
	movq %mm7, %mm6                      
	punpckhbw %mm4, %mm7                 
	punpcklbw %mm4, %mm6                 
	movq (%rdx), %mm2                       	# src
	movq (%rdx), %mm3                       	# src
	add %rcx, %rdx                             	# D.15197, src
	punpcklbw %mm4, %mm2                 
	punpckhbw %mm4, %mm3                 
	paddw %mm2, %mm0                     
	paddw %mm3, %mm1                     
	mov %rbx, %rdx                             	# src, src
	add %rcx, %rdx                             	# D.15197, src
	movq (%rdx), %mm2                       	# src
	movq (%rdx), %mm3                       	# src
	add %rcx, %rdx                             	# D.15197, src
	punpcklbw %mm4, %mm2                 
	punpckhbw %mm4, %mm3                 
	psubw %mm2, %mm0                     
	psubw %mm3, %mm1                     
	movq %mm0, 80(%rsi)                     	# tmp2553
	movq %mm1, 88(%rsi)                     	# tmp2553
	movq (%rdx), %mm2                       	# src
	movq (%rdx), %mm3                       	# src
	add %rcx, %rdx                             	# D.15197, src
	punpcklbw %mm4, %mm2                 
	punpckhbw %mm4, %mm3                 
	psubw %mm2, %mm0                     
	psubw %mm3, %mm1                     
	paddw %mm6, %mm0                     
	paddw %mm7, %mm1                     
	movq %mm0, 96(%rsi)                     	# tmp2553
	movq %mm1, 104(%rsi)                    	# tmp2553
	movq (%rdx), %mm2                       	# src
	movq (%rdx), %mm3                       	# src
	add %rcx, %rdx                             	# D.15197, src
	punpcklbw %mm4, %mm2                 
	punpckhbw %mm4, %mm3                 
	psubw %mm2, %mm0                     
	psubw %mm3, %mm1                     
	paddw %mm6, %mm0                     
	paddw %mm7, %mm1                     
	movq %mm0, 112(%rsi)                    	# tmp2553
	movq %mm1, 120(%rsi)                    	# tmp2553
	movq (%rdx), %mm2                       	# src
	movq (%rdx), %mm3                       	# src
	add %rcx, %rdx                             	# D.15197, src
	punpcklbw %mm4, %mm2                 
	punpckhbw %mm4, %mm3                 
	psubw %mm2, %mm0                     
	psubw %mm3, %mm1                     
	paddw %mm6, %mm0                     
	paddw %mm7, %mm1                     
	movq %mm0, 128(%rsi)                    	# tmp2553
	movq %mm1, 136(%rsi)                    	# tmp2553
	movq (%rdx), %mm2                       	# src
	movq (%rdx), %mm3                       	# src
	add %rcx, %rdx                             	# D.15197, src
	punpcklbw %mm4, %mm2                 
	punpckhbw %mm4, %mm3                 
	psubw %mm2, %mm0                     
	psubw %mm3, %mm1                     
	paddw %mm6, %mm0                     
	paddw %mm7, %mm1                     
	movq %mm0, 144(%rsi)                    	# tmp2553
	movq %mm1, 152(%rsi)                    	# tmp2553
	mov %rbx, %rdx                             	# src, src
	
# 0 "" 2
#NO_APP
	addq	64(%rsp), %rdx	# %sfp, src
	movq	208(%rsp), %rbx	# %sfp, D.15185
	leaq	(%rdx,%rbx), %r8	#, D.15186
	movq	400(%rsp), %rdi	# %sfp, offset
#APP
# 2804 "postprocess_template.c" 1
	movq 520(%rsp), %mm6                         	# both_masks
	pcmpeqb %mm5, %mm5                   
	pxor %mm6, %mm5                      
	pxor %mm7, %mm7                      
	1:                                     
	movq (%rsi), %mm0                       	# temp_sums
	movq 8(%rsi), %mm1                      	# temp_sums
	paddw 32(%rsi), %mm0                    	# temp_sums
	paddw 40(%rsi), %mm1                    	# temp_sums
	movq (%rdi, %r8), %mm2                   	# offset, D.15186
	movq %mm2, %mm3                      
	movq %mm2, %mm4                      
	punpcklbw %mm7, %mm2                 
	punpckhbw %mm7, %mm3                 
	paddw %mm2, %mm0                     
	paddw %mm3, %mm1                     
	paddw %mm2, %mm0                     
	paddw %mm3, %mm1                     
	psrlw $4, %mm0                        
	psrlw $4, %mm1                        
	packuswb %mm1, %mm0                  
	pand %mm6, %mm0                      
	pand %mm5, %mm4                      
	por %mm4, %mm0                       
	movq %mm0, (%rdi, %r8)                   	# offset, D.15186
	add $16, %rsi                            	# temp_sums
	add %rcx, %rdi                             	# D.15197, offset
	 js 1b                                 
	
# 0 "" 2
#NO_APP
	jmp	.L566	#
.L565:
	movq	64(%rsp), %rdx	# %sfp, D.15201
	addq	56(%rsp), %rdx	# %sfp, D.15201
	addq	8(%rsp), %rdx	# %sfp, src
.L566:
	cmpq	$-1, %rax	#, D.15197
	je	.L558	#,
	leaq	528(%rsp), %rsi	#, tmp2560
#APP
# 2844 "postprocess_template.c" 1
	pxor %mm7, %mm7                      
	movq (%rdx), %mm0                       	# temp_src
	movq %mm0, %mm1                      
	punpcklbw %mm7, %mm0                 
	punpckhbw %mm7, %mm1                 
	movq (%rdx, %rcx), %mm2                   	# temp_src, D.15197
	lea (%rdx, %rcx, 2), %rax             	# temp_src, D.15197
	movq %mm2, %mm3                      
	punpcklbw %mm7, %mm2                 
	punpckhbw %mm7, %mm3                 
	movq (%rax), %mm4                
	movq %mm4, %mm5                      
	punpcklbw %mm7, %mm4                 
	punpckhbw %mm7, %mm5                 
	paddw %mm0, %mm0                     
	paddw %mm1, %mm1                     
	psubw %mm4, %mm2                     
	psubw %mm5, %mm3                     
	psubw %mm2, %mm0                     
	psubw %mm3, %mm1                     
	psllw $2, %mm2                        
	psllw $2, %mm3                        
	psubw %mm2, %mm0                     
	psubw %mm3, %mm1                     
	movq (%rax, %rcx), %mm2            	# D.15197
	movq %mm2, %mm3                      
	punpcklbw %mm7, %mm2                 
	punpckhbw %mm7, %mm3                 
	psubw %mm2, %mm0                     
	psubw %mm3, %mm1                     
	psubw %mm2, %mm0                     
	psubw %mm3, %mm1                     
	movq %mm0, (%rsi)                       	# tmp2560
	movq %mm1, 8(%rsi)                      	# tmp2560
	movq (%rax, %rcx, 2), %mm0         	# D.15197
	movq %mm0, %mm1                      
	punpcklbw %mm7, %mm0                 
	punpckhbw %mm7, %mm1                 
	psubw %mm0, %mm2                     
	psubw %mm1, %mm3                     
	movq %mm2, 16(%rsi)                     	# tmp2560
	movq %mm3, 24(%rsi)                     	# tmp2560
	paddw %mm4, %mm4                     
	paddw %mm5, %mm5                     
	psubw %mm2, %mm4                     
	psubw %mm3, %mm5                     
	lea (%rax, %rcx), %rdx                	# D.15197, temp_src
	psllw $2, %mm2                        
	psllw $2, %mm3                        
	psubw %mm2, %mm4                     
	psubw %mm3, %mm5                     
	movq (%rdx, %rcx, 2), %mm2                	# temp_src, D.15197
	movq %mm2, %mm3                      
	punpcklbw %mm7, %mm2                 
	punpckhbw %mm7, %mm3                 
	psubw %mm2, %mm4                     
	psubw %mm3, %mm5                     
	psubw %mm2, %mm4                     
	psubw %mm3, %mm5                     
	movq (%rax, %rcx, 4), %mm6         	# D.15197
	punpcklbw %mm7, %mm6                 
	psubw %mm6, %mm2                     
	movq (%rax, %rcx, 4), %mm6         	# D.15197
	punpckhbw %mm7, %mm6                 
	psubw %mm6, %mm3                     
	paddw %mm0, %mm0                     
	paddw %mm1, %mm1                     
	psubw %mm2, %mm0                     
	psubw %mm3, %mm1                     
	psllw $2, %mm2                        
	psllw $2, %mm3                        
	psubw %mm2, %mm0                     
	psubw %mm3, %mm1                     
	movq (%rdx, %rcx, 4), %mm2                	# temp_src, D.15197
	movq %mm2, %mm3                      
	punpcklbw %mm7, %mm2                 
	punpckhbw %mm7, %mm3                 
	paddw %mm2, %mm2                     
	paddw %mm3, %mm3                     
	psubw %mm2, %mm0                     
	psubw %mm3, %mm1                     
	movq (%rsi), %mm2                       	# tmp2560
	movq 8(%rsi), %mm3                      	# tmp2560
	movq %mm7, %mm6                      
	pcmpgtw %mm0, %mm6                   
	pxor %mm6, %mm0                      
	psubw %mm6, %mm0                     
	movq %mm7, %mm6                      
	pcmpgtw %mm1, %mm6                   
	pxor %mm6, %mm1                      
	psubw %mm6, %mm1                     
	movq %mm7, %mm6                      
	pcmpgtw %mm2, %mm6                   
	pxor %mm6, %mm2                      
	psubw %mm6, %mm2                     
	movq %mm7, %mm6                      
	pcmpgtw %mm3, %mm6                   
	pxor %mm6, %mm3                      
	psubw %mm6, %mm3                     
	movq %mm0, %mm6                      
	psubusw %mm2, %mm6                   
	psubw %mm6, %mm0                     
	movq %mm1, %mm6                      
	psubusw %mm3, %mm6                   
	psubw %mm6, %mm1                     
	movd 1952(%rsp), %mm2                         	# c.pQPb
	punpcklbw %mm7, %mm2                 
	movq %mm7, %mm6                      
	pcmpgtw %mm4, %mm6                   
	pxor %mm6, %mm4                      
	psubw %mm6, %mm4                     
	pcmpgtw %mm5, %mm7                   
	pxor %mm7, %mm5                      
	psubw %mm7, %mm5                     
	psllw $3, %mm2                        
	movq %mm2, %mm3                      
	pcmpgtw %mm4, %mm2                   
	pcmpgtw %mm5, %mm3                   
	pand %mm2, %mm4                      
	pand %mm3, %mm5                      
	psubusw %mm0, %mm4                   
	psubusw %mm1, %mm5                   
	movq w05, %mm2              
	pmullw %mm2, %mm4                    
	pmullw %mm2, %mm5                    
	movq w20, %mm2              
	paddw %mm2, %mm4                     
	paddw %mm2, %mm5                     
	psrlw $6, %mm4                        
	psrlw $6, %mm5                        
	movq 16(%rsi), %mm0                     	# tmp2560
	movq 24(%rsi), %mm1                     	# tmp2560
	pxor %mm2, %mm2                      
	pxor %mm3, %mm3                      
	pcmpgtw %mm0, %mm2                   
	pcmpgtw %mm1, %mm3                   
	pxor %mm2, %mm0                      
	pxor %mm3, %mm1                      
	psubw %mm2, %mm0                     
	psubw %mm3, %mm1                     
	psrlw $1, %mm0                        
	psrlw $1, %mm1                        
	pxor %mm6, %mm2                      
	pxor %mm7, %mm3                      
	pand %mm2, %mm4                      
	pand %mm3, %mm5                      
	movq %mm4, %mm2                      
	psubusw %mm0, %mm2                   
	psubw %mm2, %mm4                     
	movq %mm5, %mm2                      
	psubusw %mm1, %mm2                   
	psubw %mm2, %mm5                     
	pxor %mm6, %mm4                      
	pxor %mm7, %mm5                      
	psubw %mm6, %mm4                     
	psubw %mm7, %mm5                     
	packsswb %mm5, %mm4                  
	movq 512(%rsp), %mm1                         	# eq_mask
	pandn %mm4, %mm1                     
	movq (%rdx), %mm0                       	# temp_src
	paddb   %mm1, %mm0                   
	movq %mm0, (%rdx)                       	# temp_src
	movq (%rdx, %rcx), %mm0                   	# temp_src, D.15197
	psubb %mm1, %mm0                     
	movq %mm0, (%rdx, %rcx)                   	# temp_src, D.15197
	
# 0 "" 2
#NO_APP
.L558:
	movslq	36(%rsp), %rbx	# %sfp, D.15197
	movq	8(%rsp), %rdi	# %sfp, dstBlock
	movq	24(%rsp), %rsi	# %sfp, tempBlock2
	movq	40(%rsp), %rcx	# %sfp, tempBlock2
#APP
# 1995 "postprocess_template.c" 1
	lea (%rdi, %rbx), %rax                	# dstBlock, D.15197
	movq (%rdi), %mm0                       	# dstBlock
	movq (%rax), %mm1                
	movq %mm0, %mm2                      
	punpcklbw %mm1, %mm0                 
	punpckhbw %mm1, %mm2                 
	movq (%rax, %rbx), %mm1            	# D.15197
	movq (%rax, %rbx, 2), %mm3         	# D.15197
	movq %mm1, %mm4                      
	punpcklbw %mm3, %mm1                 
	punpckhbw %mm3, %mm4                 
	movq %mm0, %mm3                      
	punpcklwd %mm1, %mm0                 
	punpckhwd %mm1, %mm3                 
	movq %mm2, %mm1                      
	punpcklwd %mm4, %mm2                 
	punpckhwd %mm4, %mm1                 
	movd %mm0, 128(%rsi)                    	# tempBlock2
	psrlq $32, %mm0                       
	movd %mm0, 144(%rsi)                    	# tempBlock2
	movd %mm3, 160(%rsi)                    	# tempBlock2
	psrlq $32, %mm3                       
	movd %mm3, 176(%rsi)                    	# tempBlock2
	movd %mm3, 48(%rcx)                     	# tempBlock2
	movd %mm2, 192(%rsi)                    	# tempBlock2
	movd %mm2, 64(%rcx)                     	# tempBlock2
	psrlq $32, %mm2                       
	movd %mm2, 80(%rcx)                     	# tempBlock2
	movd %mm1, 96(%rcx)                     	# tempBlock2
	psrlq $32, %mm1                       
	movd %mm1, 112(%rcx)                    	# tempBlock2
	lea (%rax, %rbx, 4), %rax      	# D.15197
	movq (%rdi, %rbx, 4), %mm0                	# dstBlock, D.15197
	movq (%rax), %mm1                
	movq %mm0, %mm2                      
	punpcklbw %mm1, %mm0                 
	punpckhbw %mm1, %mm2                 
	movq (%rax, %rbx), %mm1            	# D.15197
	movq (%rax, %rbx, 2), %mm3         	# D.15197
	movq %mm1, %mm4                      
	punpcklbw %mm3, %mm1                 
	punpckhbw %mm3, %mm4                 
	movq %mm0, %mm3                      
	punpcklwd %mm1, %mm0                 
	punpckhwd %mm1, %mm3                 
	movq %mm2, %mm1                      
	punpcklwd %mm4, %mm2                 
	punpckhwd %mm4, %mm1                 
	movd %mm0, 132(%rsi)                    	# tempBlock2
	psrlq $32, %mm0                       
	movd %mm0, 148(%rsi)                    	# tempBlock2
	movd %mm3, 164(%rsi)                    	# tempBlock2
	psrlq $32, %mm3                       
	movd %mm3, 180(%rsi)                    	# tempBlock2
	movd %mm3, 52(%rcx)                     	# tempBlock2
	movd %mm2, 196(%rsi)                    	# tempBlock2
	movd %mm2, 68(%rcx)                     	# tempBlock2
	psrlq $32, %mm2                       
	movd %mm2, 84(%rcx)                     	# tempBlock2
	movd %mm1, 100(%rcx)                    	# tempBlock2
	psrlq $32, %mm1                       
	movd %mm1, 116(%rcx)                    	# tempBlock2
	
# 0 "" 2
#NO_APP
	movl	16(%rsp), %eax	# %sfp, tmp3204
	subl	$8, %eax	#, tmp3204
	js	.L568	#,
	testl	$8192, %r14d	#, D.15188
	je	.L569	#,
	movq	%rsi, %rax	# tempBlock2, tempBlock2
	leaq	48(%rsi), %rsi	#, src
	leaq	56(%rax), %r12	#, D.15186
	movl	$0, %ebp	#, tmp2862
.L572:
	movzbl	48(%rsi), %r11d	# MEM[base: src_1921, offset: 48B], D.15204
	movzbl	64(%rsi), %r10d	# MEM[base: src_1921, offset: 64B], D.15204
	movzbl	%r10b, %r13d	# D.15204, D.15188
	movzbl	80(%rsi), %r9d	# MEM[base: src_1921, offset: 80B], D.15204
	movzbl	%r9b, %edx	# D.15204, D.15188
	movl	%r13d, %edi	# D.15188, b
	subl	%edx, %edi	# D.15188, b
	movzbl	96(%rsi), %r8d	# MEM[base: src_1921, offset: 96B], D.15204
	movl	%edi, %eax	# b, tmp2563
	sarl	$31, %eax	#, tmp2563
	movl	%eax, %ecx	# tmp2563, tmp2564
	xorl	%edi, %ecx	# b, tmp2564
	subl	%eax, %ecx	# tmp2563, D.15188
	movzbl	%r11b, %eax	# D.15204, D.15188
	subl	%r13d, %eax	# D.15188, a
	movl	%eax, %r13d	# a, tmp2568
	sarl	$31, %r13d	#, tmp2568
	xorl	%r13d, %eax	# tmp2568, tmp2569
	subl	%r13d, %eax	# tmp2568, D.15188
	movl	%eax, %r13d	# D.15188, D.15188
	movzbl	%r8b, %eax	# D.15204, D.15188
	subl	%eax, %edx	# D.15188, c
	movl	%edx, %eax	# c, tmp2573
	sarl	$31, %eax	#, tmp2573
	xorl	%eax, %edx	# tmp2573, tmp2574
	subl	%eax, %edx	# tmp2573, D.15188
	leal	0(%r13,%rdx), %eax	#, D.15188
	sarl	%eax	# D.15188
	subl	%eax, %ecx	# D.15188, d
	cmovs	%ebp, %ecx	# d,, tmp2862, d
	movl	3016(%rsp), %eax	# c.QP, tmp3582
	addl	%eax, %eax	# D.15188
	cmpl	%eax, %ecx	# D.15188, d
	jge	.L570	#,
	negl	%edi	# D.15188
	testl	%edi, %edi	# D.15188
	setg	%al	#, D.15188
	movzbl	%al, %eax	# D.15188, D.15188
	leal	-1(%rax,%rax), %eax	#, D.15188
	imull	%eax, %ecx	# D.15188, v
	movl	%ecx, %eax	# v, D.15188
	sarl	$3, %eax	#, D.15188
	addb	%al, 32(%rsi)	# D.15188, MEM[base: src_1921, offset: 32B]
	movl	%ecx, %edx	# v, D.15188
	sarl	$2, %edx	#, D.15188
	addl	%edx, %r11d	# D.15188, tmp2584
	movb	%r11b, 48(%rsi)	# tmp2584, MEM[base: src_1921, offset: 48B]
	leal	(%rcx,%rcx,2), %ecx	#, D.15188
	sarl	$3, %ecx	#, D.15188
	addl	%ecx, %r10d	# D.15188, tmp2589
	movb	%r10b, 64(%rsi)	# tmp2589, MEM[base: src_1921, offset: 64B]
	subl	%ecx, %r9d	# D.15188, tmp2590
	movb	%r9b, 80(%rsi)	# tmp2590, MEM[base: src_1921, offset: 80B]
	subl	%edx, %r8d	# D.15188, tmp2591
	movb	%r8b, 96(%rsi)	# tmp2591, MEM[base: src_1921, offset: 96B]
	subb	%al, 112(%rsi)	# D.15188, MEM[base: src_1921, offset: 112B]
.L570:
	addq	$1, %rsi	#, src
	cmpq	%r12, %rsi	# D.15186, src
	jne	.L572	#,
	jmp	.L573	#
.L569:
	testb	$2, %r14b	#, D.15188
	je	.L574	#,
	movslq	3020(%rsp), %rax	# c.nonBQP, D.15188
#APP
# 114 "postprocess_template.c" 1
	movq 1968(%rsp,%rax,8), %mm7                         	# c.mmxDcOffset
	movq 2480(%rsp,%rax,8), %mm6                         	# c.mmxDcThreshold
	
# 0 "" 2
#NO_APP
	movq	24(%rsp), %rax	# %sfp, tempBlock2
	leaq	64(%rax), %rdx	#, src
	movl	$16, %ecx	#, tmp2600
#APP
# 120 "postprocess_template.c" 1
	lea (%rdx, %rcx), %rax                	# src, tmp2600
	movq (%rdx), %mm0                       	# src
	movq (%rax), %mm1                
	movq %mm0, %mm3                      
	movq %mm0, %mm4                      
	psubusb %mm1, %mm4 
	paddb %mm1, %mm4 
	movq %mm3, %mm5 
	psubusb %mm1, %mm5 
	psubb %mm5, %mm3 
	psubb %mm1, %mm0                     
	paddb %mm7, %mm0                     
	pcmpgtb %mm6, %mm0                   
	movq (%rax,%rcx), %mm2             	# tmp2600
	psubusb %mm2, %mm4 
	paddb %mm2, %mm4 
	movq %mm3, %mm5 
	psubusb %mm2, %mm5 
	psubb %mm5, %mm3 
	psubb %mm2, %mm1                     
	paddb %mm7, %mm1                     
	pcmpgtb %mm6, %mm1                   
	paddb %mm1, %mm0                     
	movq (%rax, %rcx, 2), %mm1         	# tmp2600
	psubusb %mm1, %mm4 
	paddb %mm1, %mm4 
	movq %mm3, %mm5 
	psubusb %mm1, %mm5 
	psubb %mm5, %mm3 
	psubb %mm1, %mm2                     
	paddb %mm7, %mm2                     
	pcmpgtb %mm6, %mm2                   
	paddb %mm2, %mm0                     
	lea (%rax, %rcx, 4), %rax      	# tmp2600
	movq (%rdx, %rcx, 4), %mm2                	# src, tmp2600
	psubusb %mm2, %mm4 
	paddb %mm2, %mm4 
	movq %mm3, %mm5 
	psubusb %mm2, %mm5 
	psubb %mm5, %mm3 
	psubb %mm2, %mm1                     
	paddb %mm7, %mm1                     
	pcmpgtb %mm6, %mm1                   
	paddb %mm1, %mm0                     
	movq (%rax), %mm1                
	psubusb %mm1, %mm4 
	paddb %mm1, %mm4 
	movq %mm3, %mm5 
	psubusb %mm1, %mm5 
	psubb %mm5, %mm3 
	psubb %mm1, %mm2                     
	paddb %mm7, %mm2                     
	pcmpgtb %mm6, %mm2                   
	paddb %mm2, %mm0                     
	movq (%rax, %rcx), %mm2            	# tmp2600
	psubusb %mm2, %mm4 
	paddb %mm2, %mm4 
	movq %mm3, %mm5 
	psubusb %mm2, %mm5 
	psubb %mm5, %mm3 
	psubb %mm2, %mm1                     
	paddb %mm7, %mm1                     
	pcmpgtb %mm6, %mm1                   
	paddb %mm1, %mm0                     
	movq (%rax, %rcx, 2), %mm1         	# tmp2600
	psubusb %mm1, %mm4 
	paddb %mm1, %mm4 
	movq %mm3, %mm5 
	psubusb %mm1, %mm5 
	psubb %mm5, %mm3 
	psubb %mm1, %mm2                     
	paddb %mm7, %mm2                     
	pcmpgtb %mm6, %mm2                   
	paddb %mm2, %mm0                     
	psubusb %mm3, %mm4                   
	                                       
	movq %mm0, %mm1                      
	psrlw $8, %mm0                        
	paddb %mm1, %mm0                     
	movq %mm0, %mm1                      
	psrlq $16, %mm0                       
	paddb %mm1, %mm0                     
	movq %mm0, %mm1                      
	psrlq $32, %mm0                       
	paddb %mm1, %mm0                     
	movq 1952(%rsp), %mm7                         	# c.pQPb
	paddusb %mm7, %mm7                   
	psubusb %mm7, %mm4                   
	packssdw %mm4, %mm4                  
	movd %mm0, %edx                         	# numEq
	movd %mm4, %ecx                         	# dcOk
	
# 0 "" 2
#NO_APP
	negl	%edx	# D.15188
	movzbl	%dl, %eax	# D.15188, numEq
	cmpl	3088(%rsp), %eax	# c.ppMode.flatnessThreshold, numEq
	jle	.L575	#,
	testl	%ecx, %ecx	# dcOk
	jne	.L573	#,
	leaq	1840(%rsp), %rdx	#, tmp3586
	movl	$16, %esi	#,
	movq	24(%rsp), %rdi	# %sfp,
	call	doVertLowPass_MMX	#
	jmp	.L573	#
.L575:
	leaq	1840(%rsp), %rdx	#, tmp3587
	movl	$16, %esi	#,
	movq	24(%rsp), %rdi	# %sfp,
	call	doVertDefFilter_MMX	#
	jmp	.L573	#
.L574:
	testl	$16384, %r14d	#, D.15188
	je	.L573	#,
	movq	24(%rsp), %rdi	# %sfp, tempBlock2
	leaq	48(%rdi), %rdx	#, src
	movslq	3020(%rsp), %rax	# c.nonBQP, D.15188
#APP
# 2553 "postprocess_template.c" 1
	movq 1968(%rsp,%rax,8), %mm7                         	# c.mmxDcOffset
	movq 2480(%rsp,%rax,8), %mm6                         	# c.mmxDcThreshold
	
# 0 "" 2
#NO_APP
	movl	$16, %ecx	#, tmp2609
#APP
# 2559 "postprocess_template.c" 1
	lea (%rdx, %rcx), %rax                	# src, tmp2609
	movq (%rdx), %mm0                       	# src
	movq (%rax), %mm1                
	movq %mm1, %mm3                      
	movq %mm1, %mm4                      
	psubb %mm1, %mm0                     
	paddb %mm7, %mm0                     
	pcmpgtb %mm6, %mm0                   
	movq (%rax,%rcx), %mm2             	# tmp2609
	psubusb %mm2, %mm4 
	paddb %mm2, %mm4 
	movq %mm3, %mm5 
	psubusb %mm2, %mm5 
	psubb %mm5, %mm3 
	psubb %mm2, %mm1                     
	paddb %mm7, %mm1                     
	pcmpgtb %mm6, %mm1                   
	paddb %mm1, %mm0                     
	movq (%rax, %rcx, 2), %mm1         	# tmp2609
	psubusb %mm1, %mm4 
	paddb %mm1, %mm4 
	movq %mm3, %mm5 
	psubusb %mm1, %mm5 
	psubb %mm5, %mm3 
	psubb %mm1, %mm2                     
	paddb %mm7, %mm2                     
	pcmpgtb %mm6, %mm2                   
	paddb %mm2, %mm0                     
	lea (%rax, %rcx, 4), %rax      	# tmp2609
	movq (%rdx, %rcx, 4), %mm2                	# src, tmp2609
	psubusb %mm2, %mm4 
	paddb %mm2, %mm4 
	movq %mm3, %mm5 
	psubusb %mm2, %mm5 
	psubb %mm5, %mm3 
	psubb %mm2, %mm1                     
	paddb %mm7, %mm1                     
	pcmpgtb %mm6, %mm1                   
	paddb %mm1, %mm0                     
	movq (%rax), %mm1                
	psubusb %mm1, %mm4 
	paddb %mm1, %mm4 
	movq %mm3, %mm5 
	psubusb %mm1, %mm5 
	psubb %mm5, %mm3 
	psubb %mm1, %mm2                     
	paddb %mm7, %mm2                     
	pcmpgtb %mm6, %mm2                   
	paddb %mm2, %mm0                     
	movq (%rax, %rcx), %mm2            	# tmp2609
	psubusb %mm2, %mm4 
	paddb %mm2, %mm4 
	movq %mm3, %mm5 
	psubusb %mm2, %mm5 
	psubb %mm5, %mm3 
	psubb %mm2, %mm1                     
	paddb %mm7, %mm1                     
	pcmpgtb %mm6, %mm1                   
	paddb %mm1, %mm0                     
	movq (%rax, %rcx, 2), %mm1         	# tmp2609
	psubusb %mm1, %mm4 
	paddb %mm1, %mm4 
	movq %mm3, %mm5 
	psubusb %mm1, %mm5 
	psubb %mm5, %mm3 
	psubb %mm1, %mm2                     
	paddb %mm7, %mm2                     
	pcmpgtb %mm6, %mm2                   
	paddb %mm2, %mm0                     
	movq (%rdx, %rcx, 8), %mm2                	# src, tmp2609
	psubusb %mm2, %mm4 
	paddb %mm2, %mm4 
	movq %mm3, %mm5 
	psubusb %mm2, %mm5 
	psubb %mm5, %mm3 
	psubb %mm2, %mm1                     
	paddb %mm7, %mm1                     
	pcmpgtb %mm6, %mm1                   
	paddb %mm1, %mm0                     
	movq (%rax, %rcx, 4), %mm1         	# tmp2609
	psubb %mm1, %mm2                     
	paddb %mm7, %mm2                     
	pcmpgtb %mm6, %mm2                   
	paddb %mm2, %mm0                     
	psubusb %mm3, %mm4                   
	pxor %mm6, %mm6                      
	movq 1952(%rsp), %mm7                         	# c.pQPb
	paddusb %mm7, %mm7                   
	psubusb %mm4, %mm7                   
	pcmpeqb %mm6, %mm7                   
	pcmpeqb %mm6, %mm7                   
	movq %mm7, 504(%rsp)                         	# dc_mask
	movq 3088(%rsp), %mm7                         	# c.ppMode.flatnessThreshold
	punpcklbw %mm7, %mm7                 
	punpcklbw %mm7, %mm7                 
	punpcklbw %mm7, %mm7                 
	psubb %mm0, %mm6                     
	pcmpgtb %mm7, %mm6                   
	movq %mm6, 512(%rsp)                         	# eq_mask
	
# 0 "" 2
#NO_APP
	movq	512(%rsp), %rcx	# eq_mask, D.15197
	movq	%rcx, %rsi	# D.15197, D.15197
	andq	504(%rsp), %rsi	# dc_mask, D.15197
	movq	%rsi, 520(%rsp)	# D.15197, both_masks
	movq	%rdi, %rax	# tempBlock2, tempBlock2
	addq	$64, %rax	#, src
	testq	%rsi, %rsi	# D.15197
	je	.L577	#,
	movl	$16, %r8d	#, tmp2611
	leaq	560(%rsp), %rsi	#, tmp2612
	movq	%rdx, %rax	# src, src
#APP
# 2664 "postprocess_template.c" 1
	movq 1952(%rsp), %mm0                         	# c.pQPb
	pxor %mm4, %mm4                      
	movq (%rdx), %mm6                       	# src
	movq (%rdx, %r8), %mm5                   	# src, tmp2611
	movq %mm5, %mm1                      
	movq %mm6, %mm2                      
	psubusb %mm6, %mm5                   
	psubusb %mm1, %mm2                   
	por %mm5, %mm2                       
	psubusb %mm2, %mm0                   
	pcmpeqb %mm4, %mm0                   
	pxor %mm6, %mm1                      
	pand %mm0, %mm1                      
	pxor %mm1, %mm6                      
	movq (%rdx, %r8, 8), %mm5                	# src, tmp2611
	add %r8, %rdx                             	# tmp2611, src
	movq (%rdx, %r8, 8), %mm7                	# src, tmp2611
	movq %mm5, %mm1                      
	movq %mm7, %mm2                      
	psubusb %mm7, %mm5                   
	psubusb %mm1, %mm2                   
	por %mm5, %mm2                       
	movq 1952(%rsp), %mm0                         	# c.pQPb
	psubusb %mm2, %mm0                   
	pcmpeqb %mm4, %mm0                   
	pxor %mm7, %mm1                      
	pand %mm0, %mm1                      
	pxor %mm1, %mm7                      
	movq %mm6, %mm5                      
	punpckhbw %mm4, %mm6                 
	punpcklbw %mm4, %mm5                 
	movq %mm5, %mm0                      
	movq %mm6, %mm1                      
	psllw $2, %mm0                        
	psllw $2, %mm1                        
	paddw w04, %mm0             
	paddw w04, %mm1             
	movq (%rdx), %mm2                       	# src
	movq (%rdx), %mm3                       	# src
	add %r8, %rdx                             	# tmp2611, src
	punpcklbw %mm4, %mm2                 
	punpckhbw %mm4, %mm3                 
	paddw %mm2, %mm0                     
	paddw %mm3, %mm1                     
	movq (%rdx), %mm2                       	# src
	movq (%rdx), %mm3                       	# src
	add %r8, %rdx                             	# tmp2611, src
	punpcklbw %mm4, %mm2                 
	punpckhbw %mm4, %mm3                 
	paddw %mm2, %mm0                     
	paddw %mm3, %mm1                     
	movq (%rdx), %mm2                       	# src
	movq (%rdx), %mm3                       	# src
	add %r8, %rdx                             	# tmp2611, src
	punpcklbw %mm4, %mm2                 
	punpckhbw %mm4, %mm3                 
	paddw %mm2, %mm0                     
	paddw %mm3, %mm1                     
	movq %mm0, (%rsi)                       	# tmp2612
	movq %mm1, 8(%rsi)                      	# tmp2612
	movq (%rdx), %mm2                       	# src
	movq (%rdx), %mm3                       	# src
	add %r8, %rdx                             	# tmp2611, src
	punpcklbw %mm4, %mm2                 
	punpckhbw %mm4, %mm3                 
	paddw %mm2, %mm0                     
	paddw %mm3, %mm1                     
	psubw %mm5, %mm0                     
	psubw %mm6, %mm1                     
	movq %mm0, 16(%rsi)                     	# tmp2612
	movq %mm1, 24(%rsi)                     	# tmp2612
	movq (%rdx), %mm2                       	# src
	movq (%rdx), %mm3                       	# src
	add %r8, %rdx                             	# tmp2611, src
	punpcklbw %mm4, %mm2                 
	punpckhbw %mm4, %mm3                 
	paddw %mm2, %mm0                     
	paddw %mm3, %mm1                     
	psubw %mm5, %mm0                     
	psubw %mm6, %mm1                     
	movq %mm0, 32(%rsi)                     	# tmp2612
	movq %mm1, 40(%rsi)                     	# tmp2612
	movq (%rdx), %mm2                       	# src
	movq (%rdx), %mm3                       	# src
	add %r8, %rdx                             	# tmp2611, src
	punpcklbw %mm4, %mm2                 
	punpckhbw %mm4, %mm3                 
	paddw %mm2, %mm0                     
	paddw %mm3, %mm1                     
	psubw %mm5, %mm0                     
	psubw %mm6, %mm1                     
	movq %mm0, 48(%rsi)                     	# tmp2612
	movq %mm1, 56(%rsi)                     	# tmp2612
	movq (%rdx), %mm2                       	# src
	movq (%rdx), %mm3                       	# src
	add %r8, %rdx                             	# tmp2611, src
	punpcklbw %mm4, %mm2                 
	punpckhbw %mm4, %mm3                 
	paddw %mm2, %mm0                     
	paddw %mm3, %mm1                     
	psubw %mm5, %mm0                     
	psubw %mm6, %mm1                     
	movq %mm0, 64(%rsi)                     	# tmp2612
	movq %mm1, 72(%rsi)                     	# tmp2612
	movq %mm7, %mm6                      
	punpckhbw %mm4, %mm7                 
	punpcklbw %mm4, %mm6                 
	movq (%rdx), %mm2                       	# src
	movq (%rdx), %mm3                       	# src
	add %r8, %rdx                             	# tmp2611, src
	punpcklbw %mm4, %mm2                 
	punpckhbw %mm4, %mm3                 
	paddw %mm2, %mm0                     
	paddw %mm3, %mm1                     
	mov %rax, %rdx                             	# src, src
	add %r8, %rdx                             	# tmp2611, src
	movq (%rdx), %mm2                       	# src
	movq (%rdx), %mm3                       	# src
	add %r8, %rdx                             	# tmp2611, src
	punpcklbw %mm4, %mm2                 
	punpckhbw %mm4, %mm3                 
	psubw %mm2, %mm0                     
	psubw %mm3, %mm1                     
	movq %mm0, 80(%rsi)                     	# tmp2612
	movq %mm1, 88(%rsi)                     	# tmp2612
	movq (%rdx), %mm2                       	# src
	movq (%rdx), %mm3                       	# src
	add %r8, %rdx                             	# tmp2611, src
	punpcklbw %mm4, %mm2                 
	punpckhbw %mm4, %mm3                 
	psubw %mm2, %mm0                     
	psubw %mm3, %mm1                     
	paddw %mm6, %mm0                     
	paddw %mm7, %mm1                     
	movq %mm0, 96(%rsi)                     	# tmp2612
	movq %mm1, 104(%rsi)                    	# tmp2612
	movq (%rdx), %mm2                       	# src
	movq (%rdx), %mm3                       	# src
	add %r8, %rdx                             	# tmp2611, src
	punpcklbw %mm4, %mm2                 
	punpckhbw %mm4, %mm3                 
	psubw %mm2, %mm0                     
	psubw %mm3, %mm1                     
	paddw %mm6, %mm0                     
	paddw %mm7, %mm1                     
	movq %mm0, 112(%rsi)                    	# tmp2612
	movq %mm1, 120(%rsi)                    	# tmp2612
	movq (%rdx), %mm2                       	# src
	movq (%rdx), %mm3                       	# src
	add %r8, %rdx                             	# tmp2611, src
	punpcklbw %mm4, %mm2                 
	punpckhbw %mm4, %mm3                 
	psubw %mm2, %mm0                     
	psubw %mm3, %mm1                     
	paddw %mm6, %mm0                     
	paddw %mm7, %mm1                     
	movq %mm0, 128(%rsi)                    	# tmp2612
	movq %mm1, 136(%rsi)                    	# tmp2612
	movq (%rdx), %mm2                       	# src
	movq (%rdx), %mm3                       	# src
	add %r8, %rdx                             	# tmp2611, src
	punpcklbw %mm4, %mm2                 
	punpckhbw %mm4, %mm3                 
	psubw %mm2, %mm0                     
	psubw %mm3, %mm1                     
	paddw %mm6, %mm0                     
	paddw %mm7, %mm1                     
	movq %mm0, 144(%rsi)                    	# tmp2612
	movq %mm1, 152(%rsi)                    	# tmp2612
	mov %rax, %rdx                             	# src, src
	
# 0 "" 2
#NO_APP
	leaq	16(%rdx), %rax	#, src
	addq	$144, %rdx	#, D.15186
	movq	$-128, %rdi	#, offset
#APP
# 2804 "postprocess_template.c" 1
	movq 520(%rsp), %mm6                         	# both_masks
	pcmpeqb %mm5, %mm5                   
	pxor %mm6, %mm5                      
	pxor %mm7, %mm7                      
	1:                                     
	movq (%rsi), %mm0                       	# temp_sums
	movq 8(%rsi), %mm1                      	# temp_sums
	paddw 32(%rsi), %mm0                    	# temp_sums
	paddw 40(%rsi), %mm1                    	# temp_sums
	movq (%rdi, %rdx), %mm2                   	# offset, D.15186
	movq %mm2, %mm3                      
	movq %mm2, %mm4                      
	punpcklbw %mm7, %mm2                 
	punpckhbw %mm7, %mm3                 
	paddw %mm2, %mm0                     
	paddw %mm3, %mm1                     
	paddw %mm2, %mm0                     
	paddw %mm3, %mm1                     
	psrlw $4, %mm0                        
	psrlw $4, %mm1                        
	packuswb %mm1, %mm0                  
	pand %mm6, %mm0                      
	pand %mm5, %mm4                      
	por %mm4, %mm0                       
	movq %mm0, (%rdi, %rdx)                   	# offset, D.15186
	add $16, %rsi                            	# temp_sums
	add %r8, %rdi                             	# tmp2611, offset
	 js 1b                                 
	
# 0 "" 2
#NO_APP
.L577:
	cmpq	$-1, %rcx	#, D.15197
	je	.L573	#,
	leaq	528(%rsp), %rsi	#, tmp2621
	movq	%rax, %rdx	# src, temp_src
	movl	$16, %ecx	#, tmp2620
#APP
# 2844 "postprocess_template.c" 1
	pxor %mm7, %mm7                      
	movq (%rdx), %mm0                       	# temp_src
	movq %mm0, %mm1                      
	punpcklbw %mm7, %mm0                 
	punpckhbw %mm7, %mm1                 
	movq (%rdx, %rcx), %mm2                   	# temp_src, tmp2620
	lea (%rdx, %rcx, 2), %rax             	# temp_src, tmp2620
	movq %mm2, %mm3                      
	punpcklbw %mm7, %mm2                 
	punpckhbw %mm7, %mm3                 
	movq (%rax), %mm4                
	movq %mm4, %mm5                      
	punpcklbw %mm7, %mm4                 
	punpckhbw %mm7, %mm5                 
	paddw %mm0, %mm0                     
	paddw %mm1, %mm1                     
	psubw %mm4, %mm2                     
	psubw %mm5, %mm3                     
	psubw %mm2, %mm0                     
	psubw %mm3, %mm1                     
	psllw $2, %mm2                        
	psllw $2, %mm3                        
	psubw %mm2, %mm0                     
	psubw %mm3, %mm1                     
	movq (%rax, %rcx), %mm2            	# tmp2620
	movq %mm2, %mm3                      
	punpcklbw %mm7, %mm2                 
	punpckhbw %mm7, %mm3                 
	psubw %mm2, %mm0                     
	psubw %mm3, %mm1                     
	psubw %mm2, %mm0                     
	psubw %mm3, %mm1                     
	movq %mm0, (%rsi)                       	# tmp2621
	movq %mm1, 8(%rsi)                      	# tmp2621
	movq (%rax, %rcx, 2), %mm0         	# tmp2620
	movq %mm0, %mm1                      
	punpcklbw %mm7, %mm0                 
	punpckhbw %mm7, %mm1                 
	psubw %mm0, %mm2                     
	psubw %mm1, %mm3                     
	movq %mm2, 16(%rsi)                     	# tmp2621
	movq %mm3, 24(%rsi)                     	# tmp2621
	paddw %mm4, %mm4                     
	paddw %mm5, %mm5                     
	psubw %mm2, %mm4                     
	psubw %mm3, %mm5                     
	lea (%rax, %rcx), %rdx                	# tmp2620, temp_src
	psllw $2, %mm2                        
	psllw $2, %mm3                        
	psubw %mm2, %mm4                     
	psubw %mm3, %mm5                     
	movq (%rdx, %rcx, 2), %mm2                	# temp_src, tmp2620
	movq %mm2, %mm3                      
	punpcklbw %mm7, %mm2                 
	punpckhbw %mm7, %mm3                 
	psubw %mm2, %mm4                     
	psubw %mm3, %mm5                     
	psubw %mm2, %mm4                     
	psubw %mm3, %mm5                     
	movq (%rax, %rcx, 4), %mm6         	# tmp2620
	punpcklbw %mm7, %mm6                 
	psubw %mm6, %mm2                     
	movq (%rax, %rcx, 4), %mm6         	# tmp2620
	punpckhbw %mm7, %mm6                 
	psubw %mm6, %mm3                     
	paddw %mm0, %mm0                     
	paddw %mm1, %mm1                     
	psubw %mm2, %mm0                     
	psubw %mm3, %mm1                     
	psllw $2, %mm2                        
	psllw $2, %mm3                        
	psubw %mm2, %mm0                     
	psubw %mm3, %mm1                     
	movq (%rdx, %rcx, 4), %mm2                	# temp_src, tmp2620
	movq %mm2, %mm3                      
	punpcklbw %mm7, %mm2                 
	punpckhbw %mm7, %mm3                 
	paddw %mm2, %mm2                     
	paddw %mm3, %mm3                     
	psubw %mm2, %mm0                     
	psubw %mm3, %mm1                     
	movq (%rsi), %mm2                       	# tmp2621
	movq 8(%rsi), %mm3                      	# tmp2621
	movq %mm7, %mm6                      
	pcmpgtw %mm0, %mm6                   
	pxor %mm6, %mm0                      
	psubw %mm6, %mm0                     
	movq %mm7, %mm6                      
	pcmpgtw %mm1, %mm6                   
	pxor %mm6, %mm1                      
	psubw %mm6, %mm1                     
	movq %mm7, %mm6                      
	pcmpgtw %mm2, %mm6                   
	pxor %mm6, %mm2                      
	psubw %mm6, %mm2                     
	movq %mm7, %mm6                      
	pcmpgtw %mm3, %mm6                   
	pxor %mm6, %mm3                      
	psubw %mm6, %mm3                     
	movq %mm0, %mm6                      
	psubusw %mm2, %mm6                   
	psubw %mm6, %mm0                     
	movq %mm1, %mm6                      
	psubusw %mm3, %mm6                   
	psubw %mm6, %mm1                     
	movd 1952(%rsp), %mm2                         	# c.pQPb
	punpcklbw %mm7, %mm2                 
	movq %mm7, %mm6                      
	pcmpgtw %mm4, %mm6                   
	pxor %mm6, %mm4                      
	psubw %mm6, %mm4                     
	pcmpgtw %mm5, %mm7                   
	pxor %mm7, %mm5                      
	psubw %mm7, %mm5                     
	psllw $3, %mm2                        
	movq %mm2, %mm3                      
	pcmpgtw %mm4, %mm2                   
	pcmpgtw %mm5, %mm3                   
	pand %mm2, %mm4                      
	pand %mm3, %mm5                      
	psubusw %mm0, %mm4                   
	psubusw %mm1, %mm5                   
	movq w05, %mm2              
	pmullw %mm2, %mm4                    
	pmullw %mm2, %mm5                    
	movq w20, %mm2              
	paddw %mm2, %mm4                     
	paddw %mm2, %mm5                     
	psrlw $6, %mm4                        
	psrlw $6, %mm5                        
	movq 16(%rsi), %mm0                     	# tmp2621
	movq 24(%rsi), %mm1                     	# tmp2621
	pxor %mm2, %mm2                      
	pxor %mm3, %mm3                      
	pcmpgtw %mm0, %mm2                   
	pcmpgtw %mm1, %mm3                   
	pxor %mm2, %mm0                      
	pxor %mm3, %mm1                      
	psubw %mm2, %mm0                     
	psubw %mm3, %mm1                     
	psrlw $1, %mm0                        
	psrlw $1, %mm1                        
	pxor %mm6, %mm2                      
	pxor %mm7, %mm3                      
	pand %mm2, %mm4                      
	pand %mm3, %mm5                      
	movq %mm4, %mm2                      
	psubusw %mm0, %mm2                   
	psubw %mm2, %mm4                     
	movq %mm5, %mm2                      
	psubusw %mm1, %mm2                   
	psubw %mm2, %mm5                     
	pxor %mm6, %mm4                      
	pxor %mm7, %mm5                      
	psubw %mm6, %mm4                     
	psubw %mm7, %mm5                     
	packsswb %mm5, %mm4                  
	movq 512(%rsp), %mm1                         	# eq_mask
	pandn %mm4, %mm1                     
	movq (%rdx), %mm0                       	# temp_src
	paddb   %mm1, %mm0                   
	movq %mm0, (%rdx)                       	# temp_src
	movq (%rdx, %rcx), %mm0                   	# temp_src, tmp2620
	psubb %mm1, %mm0                     
	movq %mm0, (%rdx, %rcx)                   	# temp_src, tmp2620
	
# 0 "" 2
#NO_APP
.L573:
	movq	8(%rsp), %rdi	# %sfp, dstBlock
	leaq	-4(%rdi), %rcx	#, D.15186
	movq	24(%rsp), %rax	# %sfp, tempBlock2
	leaq	64(%rax), %rsi	#, D.15200
#APP
# 2080 "postprocess_template.c" 1
	lea (%rcx, %rbx), %rax                	# D.15186, D.15197
	lea (%rax,%rbx,4), %rdx        	# D.15197
	movq (%rsi), %mm0                       	# D.15200
	movq 16(%rsi), %mm1                     	# D.15200
	movq %mm0, %mm2                      
	punpcklbw %mm1, %mm0                 
	punpckhbw %mm1, %mm2                 
	movq 32(%rsi), %mm1                     	# D.15200
	movq 48(%rsi), %mm3                     	# D.15200
	movq %mm1, %mm4                      
	punpcklbw %mm3, %mm1                 
	punpckhbw %mm3, %mm4                 
	movq %mm0, %mm3                      
	punpcklwd %mm1, %mm0                 
	punpckhwd %mm1, %mm3                 
	movq %mm2, %mm1                      
	punpcklwd %mm4, %mm2                 
	punpckhwd %mm4, %mm1                 
	movd %mm0, (%rcx)                       	# D.15186
	psrlq $32, %mm0                       
	movd %mm0, (%rax)                
	movd %mm3, (%rax, %rbx)            	# D.15197
	psrlq $32, %mm3                       
	movd %mm3, (%rax, %rbx, 2)         	# D.15197
	movd %mm2, (%rcx, %rbx, 4)                	# D.15186, D.15197
	psrlq $32, %mm2                       
	movd %mm2, (%rdx)                
	movd %mm1, (%rdx, %rbx)            	# D.15197
	psrlq $32, %mm1                       
	movd %mm1, (%rdx, %rbx, 2)         	# D.15197
	movq 64(%rsi), %mm0                     	# D.15200
	movq 80(%rsi), %mm1                     	# D.15200
	movq %mm0, %mm2                      
	punpcklbw %mm1, %mm0                 
	punpckhbw %mm1, %mm2                 
	movq 96(%rsi), %mm1                     	# D.15200
	movq 112(%rsi), %mm3                    	# D.15200
	movq %mm1, %mm4                      
	punpcklbw %mm3, %mm1                 
	punpckhbw %mm3, %mm4                 
	movq %mm0, %mm3                      
	punpcklwd %mm1, %mm0                 
	punpckhwd %mm1, %mm3                 
	movq %mm2, %mm1                      
	punpcklwd %mm4, %mm2                 
	punpckhwd %mm4, %mm1                 
	movd %mm0, 4(%rcx)                      	# D.15186
	psrlq $32, %mm0                       
	movd %mm0, 4(%rax)               
	movd %mm3, 4(%rax, %rbx)           	# D.15197
	psrlq $32, %mm3                       
	movd %mm3, 4(%rax, %rbx, 2)        	# D.15197
	movd %mm2, 4(%rcx, %rbx, 4)               	# D.15186, D.15197
	psrlq $32, %mm2                       
	movd %mm2, 4(%rdx)               
	movd %mm1, 4(%rdx, %rbx)           	# D.15197
	psrlq $32, %mm1                       
	movd %mm1, 4(%rdx, %rbx, 2)        	# D.15197
	
# 0 "" 2
#NO_APP
	cmpl	$0, 80(%rsp)	#, %sfp
	jle	.L579	#,
	testb	$4, %r14b	#, D.15188
	je	.L579	#,
	movl	3016(%rsp), %ebx	# c.QP, D.15188
	movq	%rdi, %rax	# dstBlock, D.15185
	subq	64(%rsp), %rax	# %sfp, D.15185
	leaq	-9(%rax), %r9	#, D.15186
	movl	36(%rsp), %r10d	# %sfp, dstStride
	movl	%r10d, %ebp	# dstStride, D.15189
	movl	$8, %esi	#, D.15189
	movl	$0, %ecx	#, min
	movl	$255, %edi	#, min
.L581:
	movslq	%r10d, %rdx	# ivtmp.1590, D.15185
	addq	%r9, %rdx	# D.15186, p
	leaq	8(%rdx), %r8	#, D.15186
.L580:
	addq	$1, %rdx	#, p
	movzbl	(%rdx), %eax	# MEM[base: p_1229, offset: 0B], min
	cmpl	%eax, %ecx	# min, min
	cmovl	%eax, %ecx	# min,, min, min
	cmpl	%eax, %edi	# min, min
	cmovg	%eax, %edi	# min,, min, min
	cmpq	%r8, %rdx	# D.15186, p
	jne	.L580	#,
	addl	%ebp, %r10d	# D.15189, ivtmp.1590
	subl	$1, %esi	#, D.15189
	jne	.L581	#,
	movl	%ecx, %eax	# min, D.15188
	subl	%edi, %eax	# min, D.15188
	cmpl	$19, %eax	#, D.15188
	jle	.L579	#,
	leal	1(%rdi,%rcx), %ecx	#, D.15188
	sarl	%ecx	# avg
	leaq	560(%rsp), %r12	#, ivtmp.1550
	leaq	40(%r12), %r11	#, D.15201
	movq	%r12, %rdi	# ivtmp.1550, ivtmp.1572
.L592:
	movslq	%esi, %rdx	# D.15189, D.15185
	movzbl	(%r9,%rdx), %eax	# *_1246, D.15188
	cmpl	%eax, %ecx	# D.15188, avg
	setl	%al	#, t
	movzbl	%al, %eax	# t, t
	movzbl	1(%r9,%rdx), %r10d	# *_1250, D.15188
	leal	2(%rax), %r8d	#, tmp2839
	cmpl	%r10d, %ecx	# D.15188, avg
	cmovl	%r8d, %eax	# tmp2839,, t
	movzbl	2(%r9,%rdx), %r10d	# *_1256, D.15188
	leal	4(%rax), %r8d	#, tmp2849
	cmpl	%r10d, %ecx	# D.15188, avg
	cmovl	%r8d, %eax	# tmp2849,, t
	movzbl	3(%r9,%rdx), %r10d	# *_1262, D.15188
	leal	8(%rax), %r8d	#, tmp2840
	cmpl	%r10d, %ecx	# D.15188, avg
	cmovl	%r8d, %eax	# tmp2840,, t
	movzbl	4(%r9,%rdx), %r10d	# *_1268, D.15188
	leal	16(%rax), %r8d	#, tmp2853
	cmpl	%r10d, %ecx	# D.15188, avg
	cmovl	%r8d, %eax	# tmp2853,, t
	movzbl	5(%r9,%rdx), %r10d	# *_1274, D.15188
	leal	32(%rax), %r8d	#, tmp2841
	cmpl	%r10d, %ecx	# D.15188, avg
	cmovl	%r8d, %eax	# tmp2841,, t
	movzbl	6(%r9,%rdx), %r10d	# *_1280, D.15188
	leal	64(%rax), %r8d	#, tmp2850
	cmpl	%r10d, %ecx	# D.15188, avg
	cmovl	%r8d, %eax	# tmp2850,, t
	movzbl	7(%r9,%rdx), %r10d	# *_1286, D.15188
	leal	128(%rax), %r8d	#, tmp2842
	cmpl	%r10d, %ecx	# D.15188, avg
	cmovl	%r8d, %eax	# tmp2842,, t
	movzbl	8(%r9,%rdx), %r10d	# *_1292, D.15188
	leal	256(%rax), %r8d	#, tmp2855
	cmpl	%r10d, %ecx	# D.15188, avg
	cmovl	%r8d, %eax	# tmp2855,, t
	movzbl	9(%r9,%rdx), %r8d	# *_1298, D.15188
	leal	512(%rax), %edx	#, tmp2843
	cmpl	%r8d, %ecx	# D.15188, avg
	cmovl	%edx, %eax	# tmp2843,, t
	movl	%eax, %edx	# t, D.15188
	notl	%edx	# D.15188
	sall	$16, %edx	#, D.15188
	orl	%edx, %eax	# D.15188, t
	leal	(%rax,%rax), %r8d	#, D.15188
	movl	%eax, %edx	# t, D.15188
	sarl	%edx	# D.15188
	andl	%r8d, %edx	# D.15188, D.15188
	andl	%edx, %eax	# D.15188, tmp2650
	movl	%eax, (%rdi)	# tmp2650, MEM[base: _1770, offset: 0B]
	addl	%ebp, %esi	# D.15189, D.15189
	addq	$4, %rdi	#, ivtmp.1572
	cmpq	%r11, %rdi	# D.15201, ivtmp.1572
	jne	.L592	#,
	movq	%r12, %rcx	# ivtmp.1550, ivtmp.1561
.L593:
	movl	4(%rcx), %eax	# MEM[base: _1323, offset: 4B], MEM[base: _1323, offset: 4B]
	andl	(%rcx), %eax	# MEM[base: _1323, offset: 0B], D.15188
	andl	8(%rcx), %eax	# MEM[base: _1323, offset: 8B], t
	movl	%eax, %edx	# t, D.15188
	sarl	$16, %edx	#, D.15188
	orl	%edx, %eax	# D.15188, tmp2654
	movl	%eax, (%rcx)	# tmp2654, MEM[base: _1323, offset: 0B]
	addq	$4, %rcx	#, ivtmp.1561
	leaq	592(%rsp), %rax	#, tmp3601
	cmpq	%rax, %rcx	# tmp3601, ivtmp.1561
	jne	.L593	#,
	movl	%ebx, %eax	# D.15188, tmp2656
	shrl	$31, %eax	#, tmp2656
	addl	%eax, %ebx	# tmp2656, tmp2657
	sarl	%ebx	# D.15188
	leal	1(%rbx), %r11d	#, QP2
	movl	36(%rsp), %r13d	# %sfp, ivtmp.1552
	movq	64(%rsp), %rax	# %sfp, D.15185
	movq	%rax, %r10	# D.15185, D.15185
	negq	%r10	# D.15185
	movl	%r14d, 240(%rsp)	# D.15188, %sfp
	movq	%rax, %r14	# D.15185, D.15185
.L598:
	movl	(%r12), %ebx	# MEM[base: _1205, offset: 0B], t
	movslq	%r13d, %rax	# ivtmp.1552, D.15185
	addq	%r9, %rax	# D.15186, p
	leaq	(%rax,%r14), %rcx	#, ivtmp.1540
	movl	$1, %edx	#, x
.L597:
	addq	$1, %rax	#, p
	btl	%edx, %ebx	# x, t
	jnc	.L594	#,
	movzbl	(%rax), %r8d	# MEM[base: p_1329, offset: 0B], D.15204
	movzbl	%r8b, %edi	# D.15204, D.15188
	movzbl	-1(%rax,%r10), %esi	# MEM[base: p_1329, index: _24, offset: -1B], D.15188
	movzbl	(%rax,%r10), %r15d	# MEM[base: p_1329, index: _24, offset: 0B], D.15188
	leal	(%rsi,%r15,2), %r15d	#, D.15188
	movzbl	1(%rax,%r10), %esi	# MEM[base: p_1329, index: _24, offset: 1B], D.15188
	addl	%r15d, %esi	# D.15188, D.15188
	movzbl	-1(%rax), %r15d	# MEM[base: p_1329, offset: -1B], D.15188
	leal	(%rsi,%r15,2), %esi	#, D.15188
	leal	(%rsi,%rdi,4), %esi	#, D.15188
	movzbl	1(%rax), %r15d	# MEM[base: p_1329, offset: 1B], D.15188
	leal	(%rsi,%r15,2), %r15d	#, D.15188
	movzbl	(%rcx), %esi	# MEM[base: _1793, offset: 0B], D.15188
	addl	%r15d, %esi	# D.15188, D.15188
	movzbl	1(%rcx), %r15d	# MEM[base: _1793, offset: 1B], D.15188
	leal	(%rsi,%r15,2), %r15d	#, D.15188
	movzbl	2(%rcx), %esi	# MEM[base: _1793, offset: 2B], D.15188
	leal	8(%r15,%rsi), %esi	#, D.15188
	sarl	$4, %esi	#, f
	leal	(%r11,%rdi), %r15d	#, D.15188
	cmpl	%r15d, %esi	# D.15188, f
	jle	.L595	#,
	addl	%r11d, %r8d	# QP2, tmp2684
	movb	%r8b, (%rax)	# tmp2684, MEM[base: p_1329, offset: 0B]
	jmp	.L594	#
.L595:
	subl	%r11d, %edi	# QP2, D.15188
	cmpl	%edi, %esi	# D.15188, f
	jge	.L596	#,
	subl	%r11d, %r8d	# QP2, tmp2686
	movb	%r8b, (%rax)	# tmp2686, MEM[base: p_1329, offset: 0B]
	jmp	.L594	#
.L596:
	movb	%sil, (%rax)	# f, MEM[base: p_1329, offset: 0B]
.L594:
	addl	$1, %edx	#, x
	addq	$1, %rcx	#, ivtmp.1540
	cmpl	$9, %edx	#, x
	jne	.L597	#,
	addq	$4, %r12	#, ivtmp.1550
	addl	%ebp, %r13d	# D.15189, ivtmp.1552
	leaq	592(%rsp), %rax	#, tmp3604
	cmpq	%rax, %r12	# tmp3604, ivtmp.1550
	jne	.L598	#,
	movl	240(%rsp), %r14d	# %sfp, D.15188
.L579:
	testl	$1048576, %r14d	#, D.15188
	je	.L568	#,
	movslq	3184(%rsp), %rdx	# isColor, isColor
	movl	16(%rsp), %ebx	# %sfp, x
	movl	%ebx, %eax	# x, D.15188
	sarl	$3, %eax	#, D.15188
	cltq
	addq	304(%rsp), %rax	# %sfp, D.15185
	movq	1904(%rsp,%rdx,8), %rcx	# c.tempBlurredPast, tmp2694
	leaq	(%rcx,%rax,4), %rbp	#, D.15206
	movslq	%ebx, %rsi	# x, D.15185
	addq	288(%rsp), %rsi	# %sfp, D.15185
	addq	1880(%rsp,%rdx,8), %rsi	# c.tempBlurred, D.15186
	movq	8(%rsp), %rax	# %sfp, dstBlock
	leaq	-8(%rax), %r8	#, D.15186
	movl	3072(%rsp), %eax	# MEM[(const int *)&c + 1232B], MEM[(const int *)&c + 1232B]
	movl	%eax, 508(%rbp)	# MEM[(const int *)&c + 1232B], MEM[(uint32_t *)_399 + 508B]
	movl	3076(%rsp), %eax	# MEM[(const int *)&c + 1236B], MEM[(const int *)&c + 1236B]
	movl	%eax, 512(%rbp)	# MEM[(const int *)&c + 1236B], MEM[(uint32_t *)_399 + 512B]
	movl	3080(%rsp), %eax	# MEM[(const int *)&c + 1240B], MEM[(const int *)&c + 1240B]
	movl	%eax, 516(%rbp)	# MEM[(const int *)&c + 1240B], MEM[(uint32_t *)_399 + 516B]
	movl	36(%rsp), %ebx	# %sfp, D.15189
	movl	$0, %r11d	#, ivtmp.1525
	movl	$8, %r10d	#, D.15189
	movl	$0, %edi	#, d
	jmp	.L599	#
.L600:
	movslq	%edx, %rcx	# ivtmp.1518, D.15185
	movzbl	(%rsi,%rcx), %eax	# *_1402, ref
	movzbl	(%r8,%rcx), %ecx	# *_1405, cur
	subl	%ecx, %eax	# cur, d1
	imull	%eax, %eax	# d1, D.15188
	addl	%eax, %edi	# D.15188, d
	addl	$1, %edx	#, ivtmp.1518
	cmpl	%r9d, %edx	# D.15189, ivtmp.1518
	jne	.L600	#,
	addl	%ebx, %r11d	# D.15189, ivtmp.1525
	subl	$1, %r10d	#, D.15189
	je	.L601	#,
.L599:
	leal	8(%r11), %r9d	#, D.15189
	movl	%r11d, %edx	# ivtmp.1525, ivtmp.1518
	jmp	.L600	#
.L601:
	movl	-4(%rbp), %eax	# MEM[(uint32_t *)_399 + -4B], MEM[(uint32_t *)_399 + -4B]
	addl	-1024(%rbp), %eax	# MEM[(uint32_t *)_399 + -1024B], D.15193
	movl	4(%rbp), %edx	# MEM[(uint32_t *)_399 + 4B], MEM[(uint32_t *)_399 + 4B]
	leal	4(%rax,%rdx), %eax	#, D.15193
	addl	1024(%rbp), %eax	# MEM[(uint32_t *)_399 + 1024B], D.15193
	leal	(%rax,%rdi,4), %eax	#, D.15193
	shrl	$3, %eax	#, D.15193
	movl	%edi, 0(%rbp)	# d, MEM[(uint32_t *)_399]
	cmpl	3076(%rsp), %eax	# MEM[(const int *)&c + 1236B], D.15193
	jle	.L602	#,
	movl	$8, %r9d	#, D.15189
	movl	$8, %ebp	#, D.15189
	cmpl	3080(%rsp), %eax	# MEM[(const int *)&c + 1240B], D.15193
	jl	.L605	#,
	jmp	.L604	#
.L606:
	movslq	%ecx, %rax	# ivtmp.1458, D.15185
	leaq	(%rsi,%rax), %rdi	#, D.15186
	addq	%r8, %rax	# D.15186, D.15186
	movzbl	(%rdi), %r9d	# *_1437, ref
	movzbl	(%rax), %edx	# *_1440, cur
	leal	1(%r9,%rdx), %edx	#, D.15188
	sarl	%edx	# D.15188
	movb	%dl, (%rax)	# D.15188, *_1440
	movb	%dl, (%rdi)	# D.15188, *_1437
	addl	$1, %ecx	#, ivtmp.1458
	cmpl	%r11d, %ecx	# D.15189, ivtmp.1458
	jne	.L606	#,
	addl	%ebx, %r10d	# D.15189, D.15189
	subl	$1, %ebp	#, D.15189
	je	.L568	#,
.L605:
	leal	8(%r10), %r11d	#, D.15189
	movl	%r10d, %ecx	# D.15189, ivtmp.1458
	jmp	.L606	#
.L607:
	movslq	%eax, %rdx	# ivtmp.1473, D.15185
	movzbl	(%r8,%rdx), %ecx	# *_1455, D.15204
	movb	%cl, (%rsi,%rdx)	# D.15204, *_1454
	addl	$1, %eax	#, ivtmp.1473
	cmpl	%edi, %eax	# D.15189, ivtmp.1473
	jne	.L607	#,
	addl	%ebx, %r10d	# D.15189, D.15189
	subl	$1, %r9d	#, D.15189
	je	.L568	#,
.L604:
	leal	8(%r10), %edi	#, D.15189
	movl	%r10d, %eax	# D.15189, ivtmp.1473
	jmp	.L607	#
.L602:
	movl	$8, %ebp	#, D.15189
	cmpl	3072(%rsp), %eax	# MEM[(const int *)&c + 1232B], D.15193
	jge	.L609	#,
.L608:
	movl	$8, %ebp	#, D.15189
	jmp	.L610	#
.L611:
	movslq	%ecx, %rdx	# ivtmp.1488, D.15185
	leaq	(%rsi,%rdx), %rdi	#, D.15186
	addq	%r8, %rdx	# D.15186, D.15186
	movzbl	(%rdx), %eax	# *_1468, cur
	movzbl	(%rdi), %r9d	# *_1465, ref
	leal	(%rax,%r9,8), %eax	#, D.15188
	subl	%r9d, %eax	# ref, D.15188
	addl	$4, %eax	#, D.15188
	sarl	$3, %eax	#, D.15188
	movb	%al, (%rdx)	# D.15188, *_1468
	movb	%al, (%rdi)	# D.15188, *_1465
	addl	$1, %ecx	#, ivtmp.1488
	cmpl	%r11d, %ecx	# D.15189, ivtmp.1488
	jne	.L611	#,
	addl	%ebx, %r10d	# D.15189, D.15189
	subl	$1, %ebp	#, D.15189
	je	.L568	#,
.L610:
	leal	8(%r10), %r11d	#, D.15189
	movl	%r10d, %ecx	# D.15189, ivtmp.1488
	jmp	.L611	#
.L612:
	movslq	%ecx, %rax	# ivtmp.1503, D.15185
	leaq	(%rsi,%rax), %rdi	#, D.15186
	addq	%r8, %rax	# D.15186, D.15186
	movzbl	(%rax), %r11d	# *_1486, cur
	movzbl	(%rdi), %edx	# *_1483, ref
	leal	(%rdx,%rdx,2), %edx	#, D.15188
	leal	2(%r11,%rdx), %edx	#, D.15188
	sarl	$2, %edx	#, D.15188
	movb	%dl, (%rax)	# D.15188, *_1486
	movb	%dl, (%rdi)	# D.15188, *_1483
	addl	$1, %ecx	#, ivtmp.1503
	cmpl	%r9d, %ecx	# D.15189, ivtmp.1503
	jne	.L612	#,
	addl	%ebx, %r10d	# D.15189, D.15189
	subl	$1, %ebp	#, D.15189
	je	.L568	#,
.L609:
	leal	8(%r10), %r9d	#, D.15189
	movl	%r10d, %ecx	# D.15189, ivtmp.1503
	jmp	.L612	#
.L568:
	addq	$8, 8(%rsp)	#, %sfp
	addq	$8, 48(%rsp)	#, %sfp
	addl	$8, 16(%rsp)	#, %sfp
	movl	16(%rsp), %ebx	# %sfp, x
	addq	$8, 72(%rsp)	#, %sfp
	addq	$8, 104(%rsp)	#, %sfp
	addq	$8, 112(%rsp)	#, %sfp
	addq	$8, 120(%rsp)	#, %sfp
	addq	$8, 128(%rsp)	#, %sfp
	addq	$8, 136(%rsp)	#, %sfp
	addq	$8, 144(%rsp)	#, %sfp
	addq	$8, 152(%rsp)	#, %sfp
	movq	40(%rsp), %rax	# %sfp, tempBlock2
	movq	24(%rsp), %rdi	# %sfp, tempBlock2
	movq	%rdi, 40(%rsp)	# tempBlock2, %sfp
	cmpl	%ebx, 96(%rsp)	# x, %sfp
	jg	.L650	#,
.L523:
	cmpl	$0, 80(%rsp)	#, %sfp
	jle	.L614	#,
	testb	$4, %r14b	#, D.15188
	je	.L614	#,
	movl	3016(%rsp), %r11d	# c.QP, D.15188
	movq	8(%rsp), %rax	# %sfp, D.15185
	subq	64(%rsp), %rax	# %sfp, D.15185
	leaq	-9(%rax), %rbp	#, src
	movl	36(%rsp), %r9d	# %sfp, dstStride
	movl	%r9d, %r13d	# dstStride, D.15189
	movl	$8, %esi	#, D.15189
	movl	$0, %ecx	#, min
	movl	$255, %edi	#, min
.L616:
	movslq	%r9d, %rdx	# ivtmp.1450, D.15185
	addq	%rbp, %rdx	# src, p
	leaq	8(%rdx), %r8	#, D.15186
.L615:
	addq	$1, %rdx	#, p
	movzbl	(%rdx), %eax	# MEM[base: p_1514, offset: 0B], min
	cmpl	%eax, %ecx	# min, min
	cmovl	%eax, %ecx	# min,, min, min
	cmpl	%eax, %edi	# min, min
	cmovg	%eax, %edi	# min,, min, min
	cmpq	%r8, %rdx	# D.15186, p
	jne	.L615	#,
	addl	%r13d, %r9d	# D.15189, ivtmp.1450
	subl	$1, %esi	#, D.15189
	jne	.L616	#,
	movl	%ecx, %eax	# min, D.15188
	subl	%edi, %eax	# min, D.15188
	cmpl	$19, %eax	#, D.15188
	jle	.L614	#,
	leal	1(%rdi,%rcx), %ecx	#, D.15188
	sarl	%ecx	# avg
	leaq	560(%rsp), %r12	#, ivtmp.1410
	leaq	40(%r12), %r10	#, D.15201
	movq	%r12, %rdi	# ivtmp.1410, ivtmp.1432
.L627:
	movslq	%esi, %rdx	# D.15189, D.15185
	movzbl	0(%rbp,%rdx), %eax	# *_1531, D.15188
	cmpl	%eax, %ecx	# D.15188, avg
	setl	%al	#, t
	movzbl	%al, %eax	# t, t
	movzbl	1(%rbp,%rdx), %r9d	# *_1535, D.15188
	leal	2(%rax), %r8d	#, tmp2844
	cmpl	%r9d, %ecx	# D.15188, avg
	cmovl	%r8d, %eax	# tmp2844,, t
	movzbl	2(%rbp,%rdx), %r9d	# *_1541, D.15188
	leal	4(%rax), %r8d	#, tmp2851
	cmpl	%r9d, %ecx	# D.15188, avg
	cmovl	%r8d, %eax	# tmp2851,, t
	movzbl	3(%rbp,%rdx), %r9d	# *_1547, D.15188
	leal	8(%rax), %r8d	#, tmp2845
	cmpl	%r9d, %ecx	# D.15188, avg
	cmovl	%r8d, %eax	# tmp2845,, t
	movzbl	4(%rbp,%rdx), %r9d	# *_1553, D.15188
	leal	16(%rax), %r8d	#, tmp2854
	cmpl	%r9d, %ecx	# D.15188, avg
	cmovl	%r8d, %eax	# tmp2854,, t
	movzbl	5(%rbp,%rdx), %r9d	# *_1559, D.15188
	leal	32(%rax), %r8d	#, tmp2846
	cmpl	%r9d, %ecx	# D.15188, avg
	cmovl	%r8d, %eax	# tmp2846,, t
	movzbl	6(%rbp,%rdx), %r9d	# *_1565, D.15188
	leal	64(%rax), %r8d	#, tmp2852
	cmpl	%r9d, %ecx	# D.15188, avg
	cmovl	%r8d, %eax	# tmp2852,, t
	movzbl	7(%rbp,%rdx), %r9d	# *_1571, D.15188
	leal	128(%rax), %r8d	#, tmp2847
	cmpl	%r9d, %ecx	# D.15188, avg
	cmovl	%r8d, %eax	# tmp2847,, t
	movzbl	8(%rbp,%rdx), %r9d	# *_1577, D.15188
	leal	256(%rax), %r8d	#, tmp2856
	cmpl	%r9d, %ecx	# D.15188, avg
	cmovl	%r8d, %eax	# tmp2856,, t
	movzbl	9(%rbp,%rdx), %r8d	# *_1583, D.15188
	leal	512(%rax), %edx	#, tmp2848
	cmpl	%r8d, %ecx	# D.15188, avg
	cmovl	%edx, %eax	# tmp2848,, t
	movl	%eax, %edx	# t, D.15188
	notl	%edx	# D.15188
	sall	$16, %edx	#, D.15188
	orl	%edx, %eax	# D.15188, t
	leal	(%rax,%rax), %r8d	#, D.15188
	movl	%eax, %edx	# t, D.15188
	sarl	%edx	# D.15188
	andl	%r8d, %edx	# D.15188, D.15188
	andl	%edx, %eax	# D.15188, tmp2763
	movl	%eax, (%rdi)	# tmp2763, MEM[base: _292, offset: 0B]
	addl	%r13d, %esi	# D.15189, D.15189
	addq	$4, %rdi	#, ivtmp.1432
	cmpq	%r10, %rdi	# D.15201, ivtmp.1432
	jne	.L627	#,
	movq	%r12, %rcx	# ivtmp.1410, ivtmp.1421
.L628:
	movl	4(%rcx), %eax	# MEM[base: _1671, offset: 4B], MEM[base: _1671, offset: 4B]
	andl	(%rcx), %eax	# MEM[base: _1671, offset: 0B], D.15188
	andl	8(%rcx), %eax	# MEM[base: _1671, offset: 8B], t
	movl	%eax, %edx	# t, D.15188
	sarl	$16, %edx	#, D.15188
	orl	%edx, %eax	# D.15188, tmp2767
	movl	%eax, (%rcx)	# tmp2767, MEM[base: _1671, offset: 0B]
	addq	$4, %rcx	#, ivtmp.1421
	leaq	592(%rsp), %rax	#, tmp3631
	cmpq	%rax, %rcx	# tmp3631, ivtmp.1421
	jne	.L628	#,
	movl	%r11d, %r10d	# D.15188, tmp2769
	shrl	$31, %r10d	#, tmp2769
	addl	%r11d, %r10d	# D.15188, tmp2770
	sarl	%r10d	# D.15188
	addl	$1, %r10d	#, QP2
	movl	36(%rsp), %r15d	# %sfp, ivtmp.1412
	movq	64(%rsp), %r9	# %sfp, D.15185
	negq	%r9	# D.15185
.L633:
	movl	(%r12), %r11d	# MEM[base: _25, offset: 0B], t
	movslq	%r15d, %rax	# ivtmp.1412, D.15185
	addq	%rbp, %rax	# src, p
	movq	64(%rsp), %rbx	# %sfp, D.15185
	leaq	(%rax,%rbx), %rcx	#, ivtmp.1400
	movl	$1, %edx	#, x
.L632:
	addq	$1, %rax	#, p
	btl	%edx, %r11d	# x, t
	jnc	.L629	#,
	movzbl	(%rax), %r8d	# MEM[base: p_1614, offset: 0B], D.15204
	movzbl	%r8b, %edi	# D.15204, D.15188
	movzbl	-1(%rax,%r9), %esi	# MEM[base: p_1614, index: _3, offset: -1B], D.15188
	movzbl	(%rax,%r9), %ebx	# MEM[base: p_1614, index: _3, offset: 0B], D.15188
	leal	(%rsi,%rbx,2), %ebx	#, D.15188
	movzbl	1(%rax,%r9), %esi	# MEM[base: p_1614, index: _3, offset: 1B], D.15188
	addl	%ebx, %esi	# D.15188, D.15188
	movzbl	-1(%rax), %ebx	# MEM[base: p_1614, offset: -1B], D.15188
	leal	(%rsi,%rbx,2), %esi	#, D.15188
	leal	(%rsi,%rdi,4), %esi	#, D.15188
	movzbl	1(%rax), %ebx	# MEM[base: p_1614, offset: 1B], D.15188
	leal	(%rsi,%rbx,2), %ebx	#, D.15188
	movzbl	(%rcx), %esi	# MEM[base: _1790, offset: 0B], D.15188
	addl	%ebx, %esi	# D.15188, D.15188
	movzbl	1(%rcx), %ebx	# MEM[base: _1790, offset: 1B], D.15188
	leal	(%rsi,%rbx,2), %ebx	#, D.15188
	movzbl	2(%rcx), %esi	# MEM[base: _1790, offset: 2B], D.15188
	leal	8(%rbx,%rsi), %esi	#, D.15188
	sarl	$4, %esi	#, f
	leal	(%r10,%rdi), %ebx	#, D.15188
	cmpl	%ebx, %esi	# D.15188, f
	jle	.L630	#,
	addl	%r10d, %r8d	# QP2, tmp2797
	movb	%r8b, (%rax)	# tmp2797, MEM[base: p_1614, offset: 0B]
	jmp	.L629	#
.L630:
	subl	%r10d, %edi	# QP2, D.15188
	cmpl	%edi, %esi	# D.15188, f
	jge	.L631	#,
	subl	%r10d, %r8d	# QP2, tmp2799
	movb	%r8b, (%rax)	# tmp2799, MEM[base: p_1614, offset: 0B]
	jmp	.L629	#
.L631:
	movb	%sil, (%rax)	# f, MEM[base: p_1614, offset: 0B]
.L629:
	addl	$1, %edx	#, x
	addq	$1, %rcx	#, ivtmp.1400
	cmpl	$9, %edx	#, x
	jne	.L632	#,
	addq	$4, %r12	#, ivtmp.1410
	addl	%r13d, %r15d	# D.15189, ivtmp.1412
	leaq	592(%rsp), %rax	#, tmp3635
	cmpq	%r12, %rax	# ivtmp.1410, tmp3635
	jne	.L633	#,
.L614:
	testl	$1048576, %r14d	#, D.15188
	je	.L634	#,
	movslq	3184(%rsp), %rsi	# isColor, isColor
	movl	80(%rsp), %eax	# %sfp, D.15188
	sarl	$3, %eax	#, D.15188
	sall	$8, %eax	#, D.15188
	cltq
	movl	16(%rsp), %ebx	# %sfp, x
	movl	%ebx, %edx	# x, D.15188
	sarl	$3, %edx	#, D.15188
	movslq	%edx, %rdx	# D.15188, D.15185
	leaq	256(%rax,%rdx), %rdx	#, D.15185
	movq	1904(%rsp,%rsi,8), %rax	# c.tempBlurredPast, tmp2814
	leaq	(%rax,%rdx,4), %rcx	#, D.15206
	movslq	%ebx, %rax	# x, D.15185
	addq	288(%rsp), %rax	# %sfp, D.15185
	addq	1880(%rsp,%rsi,8), %rax	# c.tempBlurred, D.15186
	movq	%rax, %rdx	# D.15186, D.15186
	movq	8(%rsp), %rdi	# %sfp, D.15186
	subq	$8, %rdi	#, D.15186
	leaq	3072(%rsp), %r8	#,
	movl	36(%rsp), %esi	# %sfp,
	call	tempNoiseReducer_MMX	#
.L634:
	movl	420(%rsp), %ebx	# %sfp, D.15188
	cmpl	%ebx, 100(%rsp)	# D.15188, %sfp
	jg	.L635	#,
	movl	96(%rsp), %ebx	# %sfp, width
	movl	440(%rsp), %edi	# %sfp, D.15188
	cmpl	%edi, %ebx	# D.15188, width
	je	.L636	#,
	movl	364(%rsp), %edi	# %sfp, ivtmp.1752
	movl	%edi, %r15d	# ivtmp.1752, D.15188
	movl	36(%rsp), %r13d	# %sfp, D.15189
	movl	$0, %eax	#, ivtmp.1386
	movl	$0, %ebp	#, i
	movslq	%ebx, %r12	# width, D.15191
	testl	%edi, %edi	# ivtmp.1752
	jg	.L652	#,
	jmp	.L635	#
.L636:
	movl	36(%rsp), %ecx	# %sfp,
	movl	364(%rsp), %edx	# %sfp,
	movq	352(%rsp), %rsi	# %sfp,
	movq	376(%rsp), %rdi	# %sfp,
	call	linecpy	#
	jmp	.L635	#
.L652:
	addl	$1, %ebp	#, i
	leal	0(%r13,%rax), %ebx	#, D.15189
	cltq
	movq	376(%rsp), %rdi	# %sfp, dstBlock
	addq	%rax, %rdi	# D.15185, D.15186
	movslq	%ebx, %rsi	# D.15189, D.15185
	addq	328(%rsp), %rsi	# %sfp, D.15186
	movq	%r12, %rdx	# D.15191,
	call	memcpy	#
	movl	%ebx, %eax	# D.15189, ivtmp.1386
	cmpl	%r15d, %ebp	# D.15188, i
	jne	.L652	#,
.L635:
	addl	$8, 80(%rsp)	#, %sfp
	movl	80(%rsp), %eax	# %sfp, y
	movl	444(%rsp), %edi	# %sfp, D.15189
	addl	%edi, 392(%rsp)	# D.15189, %sfp
	movl	448(%rsp), %edi	# %sfp, D.15189
	addl	%edi, 396(%rsp)	# D.15189, %sfp
	subl	$8, 364(%rsp)	#, %sfp
	subl	$8, 412(%rsp)	#, %sfp
	movl	452(%rsp), %edi	# %sfp, D.15189
	addl	%edi, 416(%rsp)	# D.15189, %sfp
	cmpl	%eax, 100(%rsp)	# y, %sfp
	jg	.L639	#,
.L519:
#APP
# 3692 "postprocess_template.c" 1
	emms
# 0 "" 2
#NO_APP
	leaq	1840(%rsp), %rsi	#, tmp2833
	movl	$157, %ecx	#, tmp2834
	movq	3192(%rsp), %rdi	# c2, c2
	rep movsq
	jmp	.L687	#
.L488:
	movl	3084(%rsp), %r9d	# c.ppMode.baseDcDiff, D.15189
	leaq	1840(%rsp), %rax	#, tmp3662
	leaq	1968(%rsp), %rdx	#, ivtmp.1822
	leaq	584(%rax), %r8	#, D.15201
	movl	$0, %ecx	#, ivtmp.1821
	movl	$126, %edi	#, tmp2894
	movabsq	$72340172838076673, %rsi	#, tmp2895
	jmp	.L489	#
.L687:
	addq	$3112, %rsp	#,
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
.LFE121:
	.size	postProcess_MMX, .-postProcess_MMX
	.type	postProcess_SSE2, @function
postProcess_SSE2:
.LFB175:
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
	subq	$2920, %rsp	#,
	.cfi_def_cfa_offset 2976
	movq	%rdi, 192(%rsp)	# src, %sfp
	movl	%esi, 60(%rsp)	# srcStride, %sfp
	movq	%rdx, 160(%rsp)	# dst, %sfp
	movl	%ecx, 12(%rsp)	# dstStride, %sfp
	movl	%r8d, 28(%rsp)	# width, %sfp
	movl	%r9d, 24(%rsp)	# height, %sfp
	leaq	1648(%rsp), %rdi	#, tmp677
	movl	$157, %ecx	#, tmp679
	movq	3000(%rsp), %rsi	# c2, c2
	rep movsq
	cmpl	$0, 2992(%rsp)	#, isColor
	je	.L696	#,
	movl	2860(%rsp), %r15d	# c.ppMode.chromMode, D.15422
	movl	$4, %eax	#, tmp680
	movl	%eax, %ebx	# tmp680, D.15422
	subl	2848(%rsp), %ebx	# c.hChromaSubSample, D.15422
	movl	%ebx, 116(%rsp)	# D.15422, %sfp
	subl	2852(%rsp), %eax	# c.vChromaSubSample, D.15422
	movl	%eax, 216(%rsp)	# D.15422, %sfp
	jmp	.L697	#
.L696:
	movl	2856(%rsp), %r15d	# c.ppMode.lumMode, D.15422
	movl	$4, 116(%rsp)	#, %sfp
	movl	$4, 216(%rsp)	#, %sfp
.L697:
	movq	1664(%rsp), %rax	# c.yHistogram, yHistogram
	movq	%rax, 104(%rsp)	# yHistogram, %sfp
	cmpl	$0, 60(%rsp)	#, %sfp
	jle	.L698	#,
	movq	1744(%rsp), %rax	# c.tempSrc, srcBlock
	movq	%rax, 248(%rsp)	# srcBlock, %sfp
	jmp	.L699	#
.L698:
	imull	$23, 60(%rsp), %eax	#, %sfp, D.15422
	cltq
	movq	1744(%rsp), %rbx	# c.tempSrc, srcBlock
	subq	%rax, %rbx	# D.15426, srcBlock
	movq	%rbx, 248(%rsp)	# srcBlock, %sfp
.L699:
	cmpl	$0, 12(%rsp)	#, %sfp
	jle	.L700	#,
	movq	1736(%rsp), %rax	# c.tempDst, tmp1145
	addq	$32, %rax	#, D.15423
	movq	%rax, 224(%rsp)	# D.15423, %sfp
	jmp	.L701	#
.L700:
	imull	$23, 12(%rsp), %edx	#, %sfp, D.15422
	movslq	%edx, %rdx	# D.15422, D.15426
	movl	$32, %eax	#, tmp689
	subq	%rdx, %rax	# D.15426, D.15426
	addq	1736(%rsp), %rax	# c.tempDst, D.15423
	movq	%rax, 224(%rsp)	# D.15423, %sfp
.L701:
	testl	$33554432, %r15d	#, D.15422
	je	.L702	#,
	movl	$.LC1, %edx	#,
	movl	$24, %esi	#,
	movq	3000(%rsp), %rdi	# c2,
	movl	$0, %eax	#,
	call	av_log	#
	jmp	.L702	#
.L703:
	movl	%ecx, %r10d	# ivtmp.1934, D.15422
	sarl	$8, %r10d	#, D.15422
	movl	%edi, %eax	# tmp1069, D.15422
	subl	%r10d, %eax	# D.15422, D.15422
	cltq
	imulq	%rsi, %rax	# tmp1070, D.15428
	movq	%rax, (%rdx)	# D.15428, MEM[base: _119, offset: 0B]
	leal	2(%r10,%r10), %eax	#, D.15422
	movl	%edi, %ebx	# tmp1069, D.15422
	subl	%eax, %ebx	# D.15422, D.15422
	movl	%ebx, %eax	# D.15422, D.15422
	cltq
	imulq	%rsi, %rax	# tmp1070, D.15428
	movq	%rax, 512(%rdx)	# D.15428, MEM[base: _119, offset: 512B]
	addl	%r9d, %ecx	# D.15421, ivtmp.1934
	addq	$8, %rdx	#, ivtmp.1935
	cmpq	%r8, %rdx	# D.15424, ivtmp.1935
	jne	.L703	#,
	movl	$16, 244(%rsp)	#, %sfp
	movl	%r15d, %eax	# D.15422, D.15422
	andl	$262144, %eax	#, D.15422
	movl	%eax, 152(%rsp)	# D.15422, %sfp
	jne	.L704	#,
	movl	$14, 244(%rsp)	#, %sfp
	testl	$12713984, %r15d	#, D.15422
	jne	.L704	#,
	movl	$13, 244(%rsp)	#, %sfp
	testl	$590849, %r15d	#, D.15422
	jne	.L704	#,
	movl	$11, 244(%rsp)	#, %sfp
	testl	$512, %r15d	#, D.15422
	jne	.L704	#,
	movl	%r15d, %eax	# D.15422, D.15422
	andl	$4, %eax	#, D.15422
	cmpl	$1, %eax	#, D.15422
	sbbl	%eax, %eax	# copyAhead
	addl	$9, %eax	#, copyAhead
	movl	%eax, 244(%rsp)	# copyAhead, %sfp
.L704:
	movl	244(%rsp), %eax	# %sfp, copyAhead
	subl	$8, %eax	#, copyAhead
	movl	%eax, 48(%rsp)	# copyAhead, %sfp
	cmpl	$0, 2992(%rsp)	#, isColor
	jne	.L705	#,
	movl	2832(%rsp), %eax	# c.frameNum, tmp1157
	addl	$1, %eax	#, D.15422
	movl	%eax, 2832(%rsp)	# D.15422, c.frameNum
	cmpl	$1, %eax	#, D.15422
	jne	.L706	#,
	movslq	28(%rsp), %rdx	# %sfp, D.15427
	movslq	24(%rsp), %rax	# %sfp, D.15427
	imulq	%rdx, %rax	# D.15427, D.15427
	shrq	$6, %rax	#, D.15427
	movq	%rax, %rdx	# D.15427, tmp714
	salq	$4, %rdx	#, tmp714
	subq	%rax, %rdx	# D.15427, D.15427
	movq	%rdx, %rax	# D.15427, D.15427
	shrq	$8, %rax	#, tmp716
	movq	104(%rsp), %rbx	# %sfp, yHistogram
	movq	%rax, (%rbx)	# tmp716, *yHistogram_62
.L706:
	movq	104(%rsp), %rax	# %sfp, yHistogram
	movq	%rax, %rsi	# yHistogram, ivtmp.1910
	leaq	2048(%rax), %rcx	#, D.15424
	movl	$0, %edx	#, clipped
.L707:
	addq	(%rax), %rdx	# MEM[base: _688, offset: 0B], clipped
	addq	$8, %rax	#, ivtmp.1926
	cmpq	%rcx, %rax	# D.15424, ivtmp.1926
	jne	.L707	#,
	movq	%rdx, %rcx	# clipped, clipped
	testq	%rdx, %rdx	# clipped
	js	.L708	#,
	pxor	%xmm0, %xmm0	# D.15431
	cvtsi2ssq	%rdx, %xmm0	# clipped, D.15431
	jmp	.L709	#
.L708:
	movq	%rdx, %rax	# clipped, tmp722
	shrq	%rax	# tmp722
	movq	%rdx, %rdi	# clipped, tmp723
	andl	$1, %edi	#, tmp723
	orq	%rdi, %rax	# tmp723, tmp722
	pxor	%xmm0, %xmm0	# tmp721
	cvtsi2ssq	%rax, %xmm0	# tmp722, tmp721
	addss	%xmm0, %xmm0	# tmp721, D.15431
.L709:
	mulss	2876(%rsp), %xmm0	# c.ppMode.maxClippedThreshold, D.15431
	ucomiss	.LC2(%rip), %xmm0	#, D.15431
	jnb	.L710	#,
	cvttss2siq	%xmm0, %r8	# D.15431, maxClipped
	jmp	.L711	#
.L710:
	subss	.LC2(%rip), %xmm0	#, tmp725
	cvttss2siq	%xmm0, %r8	# tmp725, maxClipped
	movabsq	$-9223372036854775808, %rax	#, tmp727
	xorq	%rax, %r8	# tmp727, maxClipped
.L711:
	cmpq	%rcx, %r8	# clipped, maxClipped
	ja	.L780	#,
	movq	104(%rsp), %rax	# %sfp, yHistogram
	leaq	2040(%rax), %rdi	#, ivtmp.1917
	movq	%rdx, %rcx	# clipped, clipped
	movl	$255, %eax	#, black
.L713:
	subq	(%rdi), %rcx	# MEM[base: _687, offset: 0B], clipped
	subl	$1, %eax	#, black
	subq	$8, %rdi	#, ivtmp.1917
	cmpq	%rcx, %r8	# clipped, maxClipped
	ja	.L784	#,
	testl	%eax, %eax	# black
	jg	.L713	#,
.L784:
	movl	$0, %ecx	#, white
.L715:
	subq	(%rsi), %rdx	# MEM[base: _685, offset: 0B], clipped
	addl	$1, %ecx	#, white
	addq	$8, %rsi	#, ivtmp.1910
	cmpq	%rdx, %r8	# clipped, maxClipped
	ja	.L712	#,
	cmpl	$255, %ecx	#, white
	jle	.L715	#,
	jmp	.L712	#
.L780:
	movl	$255, %eax	#, black
	movl	$0, %ecx	#, white
.L712:
	movl	2868(%rsp), %esi	# c.ppMode.minAllowedY, D.15422
	movl	2872(%rsp), %edx	# c.ppMode.maxAllowedY, D.15422
	subl	%esi, %edx	# D.15422, D.15422
	pxor	%xmm1, %xmm1	# D.15432
	cvtsi2sd	%edx, %xmm1	# D.15422, D.15432
	subl	%eax, %ecx	# black, D.15422
	pxor	%xmm0, %xmm0	# D.15432
	cvtsi2sd	%ecx, %xmm0	# D.15422, D.15432
	divsd	%xmm0, %xmm1	# D.15432, scale
	movapd	%xmm1, %xmm0	# scale, scale
	mulsd	.LC5(%rip), %xmm0	#, D.15432
	movapd	%xmm0, %xmm1	# D.15432, D.15432
	addsd	.LC4(%rip), %xmm1	#, D.15432
	cvttsd2si	%xmm1, %edx	# D.15432, D.15433
	movzwl	%dx, %edx	# D.15433, D.15427
	cltq
	imulq	%rdx, %rax	# D.15427, D.15427
	shrq	$8, %rax	#, D.15427
	subl	%esi, %eax	# D.15422, D.15427
	movzwl	%ax, %eax	# D.15427, D.15427
	movq	%rax, %rcx	# D.15427, D.15427
	salq	$32, %rcx	#, D.15427
	orq	%rcx, %rax	# D.15427, D.15427
	movq	%rax, %rcx	# D.15427, D.15427
	salq	$16, %rcx	#, D.15427
	orq	%rcx, %rax	# D.15427, tmp754
	movq	%rax, 1672(%rsp)	# tmp754, c.packedYOffset
	movq	%rdx, %rax	# D.15427, D.15427
	salq	$32, %rax	#, D.15427
	orq	%rax, %rdx	# D.15427, D.15427
	movq	%rdx, %rax	# D.15427, D.15427
	salq	$16, %rax	#, D.15427
	orq	%rax, %rdx	# D.15427, tmp757
	movq	%rdx, 1680(%rsp)	# tmp757, c.packedYScale
	movl	$65536, 112(%rsp)	#, %sfp
	testb	$8, %r15b	#, D.15422
	je	.L717	#,
	mulsd	.LC5(%rip), %xmm0	#, D.15432
	addsd	.LC4(%rip), %xmm0	#, D.15432
	cvttsd2si	%xmm0, %eax	# D.15432, QPCorrecture
	movl	%eax, 112(%rsp)	# QPCorrecture, %sfp
	jmp	.L717	#
.L705:
	movabsq	$72058693566333184, %rax	#, tmp1175
	movq	%rax, 1680(%rsp)	# tmp1175, c.packedYScale
	movq	$0, 1672(%rsp)	#, c.packedYOffset
	movl	$65536, 112(%rsp)	#, %sfp
.L717:
	movl	60(%rsp), %ecx	# %sfp, srcStride
	movl	%ecx, %r12d	# srcStride, D.15422
	negl	%r12d	# D.15422
	sall	$3, %r12d	#, tmp766
	movslq	%r12d, %r12	# tmp766, D.15426
	addq	192(%rsp), %r12	# %sfp, srcBlock
	movl	12(%rsp), %edi	# %sfp, dstStride
	movl	%edi, %eax	# dstStride, dstStride
	movslq	%edi, %rbx	# dstStride, D.15426
	movq	%rbx, %rsi	# D.15426, D.15426
	movq	%rbx, 88(%rsp)	# D.15426, %sfp
	movq	224(%rsp), %rbx	# %sfp, dstBlock
	addq	%rsi, %rbx	# D.15426, dstBlock
	movq	%rbx, %rbp	# dstBlock, dstBlock
	movq	%rbx, 256(%rsp)	# dstBlock, %sfp
	cmpl	$0, 28(%rsp)	#, %sfp
	jle	.L718	#,
	leal	0(,%rcx,8), %r14d	#, D.15422
	movslq	%r14d, %r14	# D.15422, D.15426
	sall	$3, %eax	#, D.15422
	cltq
	movq	%rax, (%rsp)	# D.15426, %sfp
	movl	%edi, %eax	# dstStride, dstStride
	sall	$2, %eax	#, D.15422
	cltq
	movq	%rax, 32(%rsp)	# D.15426, %sfp
	leal	(%rdi,%rdi,2), %eax	#, D.15422
	cltq
	movq	%rax, 40(%rsp)	# D.15426, %sfp
	movq	%r12, %rbx	# srcBlock, srcBlock
	movslq	%ecx, %r13	# srcStride, D.15434
	movl	%edi, %eax	# dstStride, D.15422
	negl	%eax	# D.15422
	cltq
	movq	%rax, 16(%rsp)	# D.15434, %sfp
.L727:
	movl	%ebx, %edi	# srcBlock, D.15422
	subl	%r12d, %edi	# srcBlock, D.15422
	movslq	12(%rsp), %rsi	# %sfp, D.15434
	movslq	%edi, %rcx	# D.15422, D.15434
	movslq	48(%rsp), %r8	# %sfp, D.15434
#APP
# 3381 "postprocess_template.c" 1
	mov %rcx, %rax              	# D.15434
	shr $2, %rax              
	and $6, %rax              
	add %r8, %rax              	# D.15434
	mov %rax, %rdx       
	imul %r13, %rax             	# D.15434
	imul %rsi, %rdx             	# D.15434
	prefetchnta 32(%rax, %rbx)  	# srcBlock
	prefetcht0 32(%rdx, %rbp)   	# dstBlock
	add %r13, %rax              	# D.15434
	add %rsi, %rdx              	# D.15434
	prefetchnta 32(%rax, %rbx)  	# srcBlock
	prefetcht0 32(%rdx, %rbp)   	# dstBlock
	
# 0 "" 2
#NO_APP
	leaq	(%rbx,%r14), %r8	#, D.15425
	movq	(%rsp), %rax	# %sfp, D.15426
	leaq	0(%rbp,%rax), %rcx	#, D.15423
	testb	$8, %r15b	#, D.15422
	je	.L719	#,
	leaq	1672(%rsp), %rax	#, tmp1197
#APP
# 3102 "postprocess_template.c" 1
	movq (%rax), %mm2        
	movq 8(%rax), %mm3       
	lea (%r8,%r13), %rax         	# D.15425, D.15434
	lea (%rcx,%rsi), %rdx         	# D.15423, D.15434
	pxor %mm4, %mm4              
	movq (%r8), %mm0          	# D.15425
	movq (%r8), %mm5          	# D.15425
	movq (%r8, %r13), %mm1          	# D.15425, D.15434
	movq (%r8, %r13), %mm6          	# D.15425, D.15434
	punpcklbw %mm0, %mm0         
	punpckhbw %mm5, %mm5         
	punpcklbw %mm1, %mm1         
	punpckhbw %mm6, %mm6         
	pmulhuw %mm3, %mm0           
	pmulhuw %mm3, %mm5           
	pmulhuw %mm3, %mm1           
	pmulhuw %mm3, %mm6           
	psubw %mm2, %mm0             
	psubw %mm2, %mm5             
	psubw %mm2, %mm1             
	psubw %mm2, %mm6             
	packuswb %mm5, %mm0          
	packuswb %mm6, %mm1          
	movq %mm0, (%rcx)          	# D.15423
	movq %mm1, (%rcx, %rsi)          	# D.15423, D.15434
	movq (%r8, %r13, 2), %mm0          	# D.15425, D.15434
	movq (%r8, %r13, 2), %mm5          	# D.15425, D.15434
	movq (%rax, %r13, 2), %mm1          	# D.15434
	movq (%rax, %r13, 2), %mm6          	# D.15434
	punpcklbw %mm0, %mm0         
	punpckhbw %mm5, %mm5         
	punpcklbw %mm1, %mm1         
	punpckhbw %mm6, %mm6         
	pmulhuw %mm3, %mm0           
	pmulhuw %mm3, %mm5           
	pmulhuw %mm3, %mm1           
	pmulhuw %mm3, %mm6           
	psubw %mm2, %mm0             
	psubw %mm2, %mm5             
	psubw %mm2, %mm1             
	psubw %mm2, %mm6             
	packuswb %mm5, %mm0          
	packuswb %mm6, %mm1          
	movq %mm0, (%rcx, %rsi, 2)          	# D.15423, D.15434
	movq %mm1, (%rdx, %rsi, 2)          	# D.15434
	movq (%r8, %r13, 4), %mm0          	# D.15425, D.15434
	movq (%r8, %r13, 4), %mm5          	# D.15425, D.15434
	movq (%rax, %r13, 4), %mm1          	# D.15434
	movq (%rax, %r13, 4), %mm6          	# D.15434
	punpcklbw %mm0, %mm0         
	punpckhbw %mm5, %mm5         
	punpcklbw %mm1, %mm1         
	punpckhbw %mm6, %mm6         
	pmulhuw %mm3, %mm0           
	pmulhuw %mm3, %mm5           
	pmulhuw %mm3, %mm1           
	pmulhuw %mm3, %mm6           
	psubw %mm2, %mm0             
	psubw %mm2, %mm5             
	psubw %mm2, %mm1             
	psubw %mm2, %mm6             
	packuswb %mm5, %mm0          
	packuswb %mm6, %mm1          
	movq %mm0, (%rcx, %rsi, 4)          	# D.15423, D.15434
	movq %mm1, (%rdx, %rsi, 4)          	# D.15434
	lea (%rax,%r13,4), %rax        	# D.15434
	lea (%rdx,%rsi,4), %rdx        	# D.15434
	movq (%rax, %r13), %mm0          	# D.15434
	movq (%rax, %r13), %mm5          	# D.15434
	movq (%rax, %r13, 2), %mm1          	# D.15434
	movq (%rax, %r13, 2), %mm6          	# D.15434
	punpcklbw %mm0, %mm0         
	punpckhbw %mm5, %mm5         
	punpcklbw %mm1, %mm1         
	punpckhbw %mm6, %mm6         
	pmulhuw %mm3, %mm0           
	pmulhuw %mm3, %mm5           
	pmulhuw %mm3, %mm1           
	pmulhuw %mm3, %mm6           
	psubw %mm2, %mm0             
	psubw %mm2, %mm5             
	psubw %mm2, %mm1             
	psubw %mm2, %mm6             
	packuswb %mm5, %mm0          
	packuswb %mm6, %mm1          
	movq %mm0, (%rdx, %rsi)          	# D.15434
	movq %mm1, (%rdx, %rsi, 2)          	# D.15434
	
# 0 "" 2
#NO_APP
	jmp	.L720	#
.L719:
#APP
# 3185 "postprocess_template.c" 1
	lea (%r8,%r13), %rax                 	# D.15425, D.15434
	lea (%rcx,%rsi), %rdx                 	# D.15423, D.15434
	movq (%r8), %mm0          	# D.15425
	movq (%r8, %r13), %mm1          	# D.15425, D.15434
	movq %mm0, (%rcx)          	# D.15423
	movq %mm1, (%rcx, %rsi)          	# D.15423, D.15434
	movq (%r8, %r13, 2), %mm0          	# D.15425, D.15434
	movq (%rax, %r13, 2), %mm1          	# D.15434
	movq %mm0, (%rcx, %rsi, 2)          	# D.15423, D.15434
	movq %mm1, (%rdx, %rsi, 2)          	# D.15434
	movq (%r8, %r13, 4), %mm0          	# D.15425, D.15434
	movq (%rax, %r13, 4), %mm1          	# D.15434
	movq %mm0, (%rcx, %rsi, 4)          	# D.15423, D.15434
	movq %mm1, (%rdx, %rsi, 4)          	# D.15434
	lea (%rax,%r13,4), %rax        	# D.15434
	lea (%rdx,%rsi,4), %rdx        	# D.15434
	movq (%rax, %r13), %mm0          	# D.15434
	movq (%rax, %r13, 2), %mm1          	# D.15434
	movq %mm0, (%rdx, %rsi)          	# D.15434
	movq %mm1, (%rdx, %rsi, 2)          	# D.15434
	
# 0 "" 2
#NO_APP
.L720:
	movq	%rcx, %rax	# D.15423, src
	movq	16(%rsp), %rcx	# %sfp, D.15434
#APP
# 3225 "postprocess_template.c" 1
	movq (%rax), %mm0               	# src
	movq %mm0, (%rax, %rcx, 4)        	# src, D.15434
	add %rcx, %rax                     	# D.15434, src
	movq %mm0, (%rax)               	# src
	movq %mm0, (%rax, %rcx)           	# src, D.15434
	movq %mm0, (%rax, %rcx, 2)        	# src, D.15434
	movq %mm0, (%rax, %rcx, 4)        	# src, D.15434
	
# 0 "" 2
#NO_APP
	testl	$65536, %r15d	#, D.15422
	je	.L721	#,
	movq	32(%rsp), %rax	# %sfp, D.15426
	leaq	0(%rbp,%rax), %rdx	#, D.15423
#APP
# 1455 "postprocess_template.c" 1
	lea (%rdx, %rsi), %rax                	# D.15423, D.15434
	lea (%rax, %rsi, 4), %rcx      	# D.15434
	movq (%rdx), %mm0                       	# D.15423
	movq (%rax, %rsi), %mm1            	# D.15434
	pavgb %mm1, %mm0 
	movq %mm0, (%rax)                
	movq (%rdx, %rsi, 4), %mm0                	# D.15423, D.15434
	pavgb %mm0, %mm1 
	movq %mm1, (%rax, %rsi, 2)         	# D.15434
	movq (%rcx, %rsi), %mm1            	# D.15434
	pavgb %mm1, %mm0 
	movq %mm0, (%rcx)                
	movq (%rdx, %rsi, 8), %mm0                	# D.15423, D.15434
	pavgb %mm0, %mm1 
	movq %mm1, (%rcx, %rsi, 2)         	# D.15434
	
# 0 "" 2
#NO_APP
	jmp	.L722	#
.L721:
	testl	$131072, %r15d	#, D.15422
	je	.L723	#,
	movq	32(%rsp), %rax	# %sfp, D.15426
	leaq	0(%rbp,%rax), %rcx	#, D.15423
	movslq	%edi, %rdi	# D.15422, D.15426
	addq	1752(%rsp), %rdi	# c.deintTemp, D.15423
#APP
# 1775 "postprocess_template.c" 1
	lea (%rcx, %rsi), %rax                	# D.15423, D.15434
	lea (%rax, %rsi, 4), %rdx      	# D.15434
	movq (%rdi), %mm0                       	# D.15423
	movq (%rax), %mm1                
	pavgb %mm1, %mm0 
	movq (%rcx), %mm2                       	# D.15423
	pavgb %mm2, %mm0 
	movq %mm0, (%rcx)                       	# D.15423
	movq (%rax, %rsi), %mm0            	# D.15434
	pavgb %mm0, %mm2 
	pavgb %mm1, %mm2 
	movq %mm2, (%rax)                
	movq (%rax, %rsi, 2), %mm2         	# D.15434
	pavgb %mm2, %mm1 
	pavgb %mm0, %mm1 
	movq %mm1, (%rax, %rsi)            	# D.15434
	movq (%rcx, %rsi, 4), %mm1                	# D.15423, D.15434
	pavgb %mm1, %mm0 
	pavgb %mm2, %mm0 
	movq %mm0, (%rax, %rsi, 2)         	# D.15434
	movq (%rdx), %mm0                
	pavgb %mm0, %mm2 
	pavgb %mm1, %mm2 
	movq %mm2, (%rcx, %rsi, 4)                	# D.15423, D.15434
	movq (%rdx, %rsi), %mm2            	# D.15434
	pavgb %mm2, %mm1 
	pavgb %mm0, %mm1 
	movq %mm1, (%rdx)                
	movq (%rdx, %rsi, 2), %mm1         	# D.15434
	pavgb %mm1, %mm0 
	pavgb %mm2, %mm0 
	movq %mm0, (%rdx, %rsi)            	# D.15434
	movq (%rcx, %rsi, 8), %mm0                	# D.15423, D.15434
	pavgb %mm0, %mm2 
	pavgb %mm1, %mm2 
	movq %mm2, (%rdx, %rsi, 2)         	# D.15434
	movq %mm1, (%rdi)                       	# D.15423
	
# 0 "" 2
#NO_APP
	jmp	.L722	#
.L723:
	testl	$524288, %r15d	#, D.15422
	je	.L724	#,
	movq	32(%rsp), %rax	# %sfp, D.15426
	leaq	0(%rbp,%rax), %rcx	#, D.15423
#APP
# 1877 "postprocess_template.c" 1
	lea (%rcx, %rsi), %rax                	# D.15423, D.15434
	lea (%rax, %rsi, 4), %rdx      	# D.15434
	movq (%rcx), %mm0                       	# D.15423
	movq (%rax, %rsi), %mm2            	# D.15434
	movq (%rax), %mm1                
	movq %mm0, %mm3                      
	pmaxub %mm1, %mm0                    
	pminub %mm3, %mm1                    
	pmaxub %mm2, %mm1                    
	pminub %mm1, %mm0                    
	movq %mm0, (%rax)                
	movq (%rcx, %rsi, 4), %mm0                	# D.15423, D.15434
	movq (%rax, %rsi, 2), %mm1         	# D.15434
	movq %mm2, %mm3                      
	pmaxub %mm1, %mm2                    
	pminub %mm3, %mm1                    
	pmaxub %mm0, %mm1                    
	pminub %mm1, %mm2                    
	movq %mm2, (%rax, %rsi, 2)         	# D.15434
	movq (%rdx), %mm2                
	movq (%rdx, %rsi), %mm1            	# D.15434
	movq %mm2, %mm3                      
	pmaxub %mm0, %mm2                    
	pminub %mm3, %mm0                    
	pmaxub %mm1, %mm0                    
	pminub %mm0, %mm2                    
	movq %mm2, (%rdx)                
	movq (%rdx, %rsi, 2), %mm2         	# D.15434
	movq (%rcx, %rsi, 8), %mm0                	# D.15423, D.15434
	movq %mm2, %mm3                      
	pmaxub %mm0, %mm2                    
	pminub %mm3, %mm0                    
	pmaxub %mm1, %mm0                    
	pminub %mm0, %mm2                    
	movq %mm2, (%rdx, %rsi, 2)         	# D.15434
	
# 0 "" 2
#NO_APP
	jmp	.L722	#
.L724:
	cmpl	$0, 152(%rsp)	#, %sfp
	je	.L725	#,
	movq	40(%rsp), %rax	# %sfp, D.15426
	leaq	0(%rbp,%rax), %rdi	#, D.15423
#APP
# 1508 "postprocess_template.c" 1
	lea (%rdi, %rsi), %rax                	# D.15423, D.15434
	lea (%rax, %rsi, 4), %rdx      	# D.15434
	lea (%rdx, %rsi, 4), %rcx      	# D.15434
	add %rsi, %rcx                      	# D.15434
	pxor %xmm7, %xmm7                    
	movq (%rdi), %xmm0                    	# D.15423
	movq (%rax, %rsi), %xmm1                    	# D.15434
	movq (%rdi, %rsi, 4), %xmm2                    	# D.15423, D.15434
	movq (%rdx, %rsi), %xmm3                    	# D.15434
	pavgb %xmm2, %xmm1                   
	pavgb %xmm3, %xmm0                   
	punpcklbw %xmm7, %xmm0               
	punpcklbw %xmm7, %xmm1               
	psubw %xmm1, %xmm0                   
	psraw $3, %xmm0                       
	psubw %xmm0, %xmm1                   
	packuswb %xmm1, %xmm1                
	movlps %xmm1, (%rax, %rsi, 2)                  	# D.15434
	movq (%rax, %rsi), %xmm0                    	# D.15434
	movq (%rdi, %rsi, 4), %xmm1                    	# D.15423, D.15434
	movq (%rdx, %rsi), %xmm2                    	# D.15434
	movq (%rdi, %rsi, 8), %xmm3                    	# D.15423, D.15434
	pavgb %xmm2, %xmm1                   
	pavgb %xmm3, %xmm0                   
	punpcklbw %xmm7, %xmm0               
	punpcklbw %xmm7, %xmm1               
	psubw %xmm1, %xmm0                   
	psraw $3, %xmm0                       
	psubw %xmm0, %xmm1                   
	packuswb %xmm1, %xmm1                
	movlps %xmm1, (%rdx)                  
	movq (%rdi, %rsi, 4), %xmm0                    	# D.15423, D.15434
	movq (%rdx, %rsi), %xmm1                    	# D.15434
	movq (%rdi, %rsi, 8), %xmm2                    	# D.15423, D.15434
	movq (%rcx), %xmm3                    
	pavgb %xmm2, %xmm1                   
	pavgb %xmm3, %xmm0                   
	punpcklbw %xmm7, %xmm0               
	punpcklbw %xmm7, %xmm1               
	psubw %xmm1, %xmm0                   
	psraw $3, %xmm0                       
	psubw %xmm0, %xmm1                   
	packuswb %xmm1, %xmm1                
	movlps %xmm1, (%rdx, %rsi, 2)                  	# D.15434
	movq (%rdx, %rsi), %xmm0                    	# D.15434
	movq (%rdi, %rsi, 8), %xmm1                    	# D.15423, D.15434
	movq (%rcx), %xmm2                    
	movq (%rcx, %rsi, 2), %xmm3                    	# D.15434
	pavgb %xmm2, %xmm1                   
	pavgb %xmm3, %xmm0                   
	punpcklbw %xmm7, %xmm0               
	punpcklbw %xmm7, %xmm1               
	psubw %xmm1, %xmm0                   
	psraw $3, %xmm0                       
	psubw %xmm0, %xmm1                   
	packuswb %xmm1, %xmm1                
	movlps %xmm1, (%rdx, %rsi, 4)                  	# D.15434
	
# 0 "" 2
#NO_APP
	jmp	.L722	#
.L725:
	testl	$4194304, %r15d	#, D.15422
	je	.L726	#,
	movslq	%edi, %rdx	# D.15422, D.15426
	addq	1752(%rsp), %rdx	# c.deintTemp, D.15423
	movl	12(%rsp), %esi	# %sfp,
	movq	%rbp, %rdi	# dstBlock,
	call	deInterlaceFF_SSE2	#
	jmp	.L722	#
.L726:
	testl	$8388608, %r15d	#, D.15422
	je	.L722	#,
	movq	1752(%rsp), %rdx	# c.deintTemp, D.15423
	movslq	%edi, %rdi	# D.15422, D.15426
	movslq	28(%rsp), %rcx	# %sfp, D.15426
	addq	%rdi, %rcx	# D.15426, D.15426
	addq	%rdx, %rcx	# D.15423, D.15423
	addq	%rdi, %rdx	# D.15426, D.15423
	movl	12(%rsp), %esi	# %sfp,
	movq	%rbp, %rdi	# dstBlock,
	call	deInterlaceL5_SSE2	#
.L722:
	addq	$8, %rbp	#, dstBlock
	addq	$8, %rbx	#, srcBlock
	movl	%ebx, %eax	# srcBlock, D.15421
	subl	%r12d, %eax	# srcBlock, D.15421
	cmpl	%eax, 28(%rsp)	# D.15421, %sfp
	jg	.L727	#,
.L718:
	movl	12(%rsp), %ebx	# %sfp, dstStride
	movl	%ebx, %eax	# dstStride, tmp803
	sarl	$31, %eax	#, tmp803
	xorl	%eax, %ebx	# tmp803, D.15422
	subl	%eax, %ebx	# tmp803, D.15422
	movl	%ebx, 264(%rsp)	# D.15422, %sfp
	cmpl	28(%rsp), %ebx	# %sfp, D.15422
	je	.L728	#,
	cmpl	$0, 48(%rsp)	#, %sfp
	jg	.L729	#,
	jmp	.L730	#
.L728:
	movl	12(%rsp), %eax	# %sfp, dstStride
	leal	(%rax,%rax,8), %esi	#, D.15422
	movslq	%esi, %rsi	# D.15422, D.15426
	addq	224(%rsp), %rsi	# %sfp, D.15435
	movl	%eax, %ecx	# dstStride,
	movl	48(%rsp), %edx	# %sfp,
	movq	160(%rsp), %rdi	# %sfp,
	call	linecpy	#
.L730:
	cmpl	$0, 24(%rsp)	#, %sfp
	jg	.L731	#,
	jmp	.L732	#
.L729:
	movl	12(%rsp), %ebx	# %sfp, dstStride
	movl	%ebx, %eax	# dstStride, dstStride
	movl	244(%rsp), %ecx	# %sfp, copyAhead
	leal	-8(%rcx), %r14d	#, D.15422
	leal	(%rbx,%rbx,8), %r13d	#, D.15421
	movl	$0, %ebx	#, ivtmp.1891
	movl	$0, %ebp	#, i
	movslq	28(%rsp), %r12	# %sfp, D.15427
	movl	%r15d, (%rsp)	# D.15422, %sfp
	movl	%eax, %r15d	# D.15421, D.15421
.L733:
	movslq	%ebx, %rdi	# ivtmp.1891, D.15426
	addq	160(%rsp), %rdi	# %sfp, D.15423
	leal	(%rbx,%r13), %esi	#, D.15421
	movslq	%esi, %rsi	# D.15421, D.15426
	addq	224(%rsp), %rsi	# %sfp, D.15423
	movq	%r12, %rdx	# D.15427,
	call	memcpy	#
	addl	$1, %ebp	#, i
	addl	%r15d, %ebx	# D.15421, ivtmp.1891
	cmpl	%r14d, %ebp	# D.15422, i
	jne	.L733	#,
	movl	(%rsp), %r15d	# %sfp, D.15422
	jmp	.L730	#
.L731:
	movl	60(%rsp), %ebx	# %sfp, srcStride
	movl	%ebx, %eax	# srcStride, D.15422
	movl	48(%rsp), %ecx	# %sfp, copyAhead
	imull	%ecx, %eax	# copyAhead, D.15422
	cltq
	movq	%rax, 64(%rsp)	# D.15426, %sfp
	movq	248(%rsp), %rsi	# %sfp, D.15430
	addq	%rax, %rsi	# D.15426, D.15430
	movq	%rsi, 272(%rsp)	# D.15430, %sfp
	movl	24(%rsp), %edi	# %sfp, height
	leal	-1(%rdi), %edx	#, D.15422
	movl	%ebx, %eax	# srcStride, D.15422
	imull	%edx, %eax	# D.15422, D.15422
	cltq
	addq	192(%rsp), %rax	# %sfp, D.15425
	movq	%rax, 280(%rsp)	# D.15425, %sfp
	movl	12(%rsp), %esi	# %sfp, dstStride
	imull	%esi, %edx	# dstStride, D.15422
	movslq	%edx, %rax	# D.15422, D.15426
	addq	160(%rsp), %rax	# %sfp, D.15423
	movq	%rax, 288(%rsp)	# D.15423, %sfp
	leal	(%rbx,%rbx,2), %eax	#, D.15422
	sall	$2, %eax	#, tmp830
	cltq
	movq	%rax, 120(%rsp)	# D.15426, %sfp
	movl	%esi, %eax	# dstStride, D.15422
	imull	%ecx, %eax	# copyAhead, D.15422
	cltq
	movq	%rax, 72(%rsp)	# D.15426, %sfp
	movl	%esi, %eax	# dstStride, dstStride
	sall	$2, %eax	#, D.15422
	cltq
	movq	%rax, 80(%rsp)	# D.15426, %sfp
	movl	%esi, %eax	# dstStride, dstStride
	leal	(%rsi,%rsi), %ecx	#, tmp834
	addl	%ecx, %eax	# tmp834, D.15422
	cltq
	movq	%rax, 96(%rsp)	# D.15426, %sfp
	movl	%esi, %eax	# dstStride, tmp837
	negl	%eax	# tmp837
	sall	$3, %eax	#, tmp838
	movslq	%eax, %r11	# tmp838, offset
	movq	%r11, 176(%rsp)	# offset, %sfp
	negq	%r11	# D.15426
	movq	%r11, 184(%rsp)	# D.15426, %sfp
	leal	0(,%rbx,8), %r11d	#, D.15421
	movl	%r11d, 240(%rsp)	# D.15421, %sfp
	sall	$3, %esi	#, D.15421
	movl	%esi, 212(%rsp)	# D.15421, %sfp
	movl	%edi, 268(%rsp)	# height, %sfp
	leal	1(%rdi), %esi	#, ivtmp.1882
	movl	%esi, 204(%rsp)	# ivtmp.1882, %sfp
	movl	%eax, 220(%rsp)	# tmp838, %sfp
	leal	(%rdx,%rcx), %eax	#, ivtmp.1883
	movl	%eax, 208(%rsp)	# ivtmp.1883, %sfp
	movl	%edi, 200(%rsp)	# height, %sfp
	movl	$0, 168(%rsp)	#, %sfp
	movl	$0, 156(%rsp)	#, %sfp
	movl	$0, 16(%rsp)	#, %sfp
	movl	%ebx, %edx	# srcStride, tmp1059
	sarl	$31, %edx	#, tmp1059
	movl	%ebx, %eax	# srcStride, tmp1060
	xorl	%edx, %eax	# tmp1059, tmp1060
	subl	%edx, %eax	# tmp1059, D.15422
	cltq
	movq	%rax, 296(%rsp)	# D.15427, %sfp
.L774:
	movslq	156(%rsp), %r14	# %sfp, D.15426
	addq	192(%rsp), %r14	# %sfp, srcBlock
	movslq	168(%rsp), %rax	# %sfp, D.15426
	movq	%rax, %rbx	# D.15426, D.15426
	movq	%rax, 136(%rsp)	# D.15426, %sfp
	movq	160(%rsp), %rax	# %sfp, dstBlock
	addq	%rbx, %rax	# D.15426, dstBlock
	movq	%rax, %rbx	# dstBlock, dstBlock
	movq	%rax, 232(%rsp)	# dstBlock, %sfp
	movq	1656(%rsp), %r13	# c.tempBlocks, tempBlock2
	leaq	8(%r13), %rax	#, tempBlock2
	movq	%rax, (%rsp)	# tempBlock2, %sfp
	movl	16(%rsp), %esi	# %sfp, y
	movl	%esi, %eax	# y, D.15422
	movzbl	216(%rsp), %ecx	# %sfp, tmp1267
	sarl	%cl, %eax	# tmp1267, D.15422
	movl	%eax, %ecx	# D.15422, D.15422
	movl	%eax, %edx	# D.15422, D.15422
	imull	2984(%rsp), %edx	# QPStride, D.15422
	movslq	%edx, %rdx	# D.15422, D.15426
	addq	2976(%rsp), %rdx	# QPs, QPptr
	movq	%rdx, 32(%rsp)	# QPptr, %sfp
	movl	2984(%rsp), %edx	# QPStride, tmp844
	sarl	$31, %edx	#, tmp844
	movl	%edx, %eax	# tmp844, tmp845
	xorl	2984(%rsp), %eax	# QPStride, tmp845
	subl	%edx, %eax	# tmp844, D.15422
	imull	%ecx, %eax	# D.15422, D.15422
	cltq
	addq	2808(%rsp), %rax	# c.nonBQPTable, nonBQPptr
	movq	%rax, 40(%rsp)	# nonBQPptr, %sfp
	movl	%esi, %eax	# y, y
	movl	%esi, 52(%rsp)	# y, %sfp
	addl	$15, %eax	#, D.15422
	movl	%eax, 172(%rsp)	# D.15422, %sfp
	cmpl	%eax, 24(%rsp)	# D.15422, %sfp
	jle	.L734	#,
	jmp	.L739	#
.L782:
	movq	256(%rsp), %rbx	# %sfp, dstBlock
	movq	248(%rsp), %r14	# %sfp, srcBlock
.L739:
	movl	$0, %r12d	#, x
	cmpl	$0, 28(%rsp)	#, %sfp
	jle	.L736	#,
	jmp	.L735	#
.L734:
	movl	268(%rsp), %edx	# %sfp, D.15421
	movl	244(%rsp), %r12d	# %sfp, copyAhead
	subl	%r12d, %edx	# copyAhead, D.15421
	addl	$8, %edx	#, D.15421
	subl	16(%rsp), %edx	# %sfp, D.15421
	movl	$0, %eax	#, tmp853
	cmovs	%eax, %edx	# D.15421,, tmp853, D.15421
	movq	64(%rsp), %rax	# %sfp, D.15426
	leaq	(%r14,%rax), %rsi	#, D.15435
	movl	60(%rsp), %ebp	# %sfp, srcStride
	movl	%ebp, %ecx	# srcStride,
	movq	272(%rsp), %rdi	# %sfp,
	call	linecpy	#
	movl	200(%rsp), %eax	# %sfp, ivtmp.1880
	cmpl	$8, %eax	#, ivtmp.1880
	movl	$8, %ebx	#, tmp855
	cmovge	%eax, %ebx	# ivtmp.1880,, i
	cmpl	%ebx, %r12d	# i, copyAhead
	jle	.L737	#,
	movl	%ebp, %r12d	# srcStride, D.15421
	imull	%ebx, %ebp	# i, ivtmp.1871
.L738:
	movslq	%ebp, %rdi	# ivtmp.1871, D.15426
	addq	248(%rsp), %rdi	# %sfp, D.15423
	movq	296(%rsp), %rdx	# %sfp,
	movq	280(%rsp), %rsi	# %sfp,
	call	memcpy	#
	addl	$1, %ebx	#, i
	addl	%r12d, %ebp	# D.15421, ivtmp.1871
	cmpl	%ebx, 244(%rsp)	# i, %sfp
	jg	.L738	#,
.L737:
	movl	204(%rsp), %ebp	# %sfp, ivtmp.1882
	movl	%ebp, %ebx	# ivtmp.1882, i
	movl	244(%rsp), %eax	# %sfp, copyAhead
	subl	$7, %eax	#, D.15422
	cmpl	%eax, %ebp	# D.15422, ivtmp.1882
	cmovle	%ebp, %eax	# ivtmp.1882,, D.15422
	movl	%eax, %edx	# D.15422, D.15422
	movq	232(%rsp), %rsi	# %sfp, D.15423
	subq	88(%rsp), %rsi	# %sfp, D.15423
	movl	12(%rsp), %r14d	# %sfp, dstStride
	movl	%r14d, %ecx	# dstStride,
	movq	224(%rsp), %rdi	# %sfp,
	call	linecpy	#
	cmpl	%ebp, 48(%rsp)	# ivtmp.1882, %sfp
	jl	.L782	#,
	movl	208(%rsp), %ebp	# %sfp, ivtmp.1864
	movslq	264(%rsp), %r12	# %sfp, D.15427
.L740:
	movslq	%ebp, %rdi	# ivtmp.1864, D.15426
	addq	224(%rsp), %rdi	# %sfp, D.15423
	movq	%r12, %rdx	# D.15427,
	movq	288(%rsp), %rsi	# %sfp,
	call	memcpy	#
	addl	$1, %ebx	#, i
	addl	%r14d, %ebp	# D.15421, ivtmp.1864
	cmpl	%ebx, 48(%rsp)	# i, %sfp
	jge	.L740	#,
	movq	256(%rsp), %rbx	# %sfp, dstBlock
	movq	248(%rsp), %r14	# %sfp, srcBlock
	jmp	.L739	#
.L735:
	movl	16(%rsp), %eax	# %sfp, D.15422
	sarl	$3, %eax	#, D.15422
	sall	$8, %eax	#, D.15422
	cltq
	addq	$256, %rax	#, D.15426
	movq	%rax, 144(%rsp)	# D.15426, %sfp
	movl	$0, %r12d	#, x
	leaq	1672(%rsp), %rax	#, tmp1051
	movq	%rax, 128(%rsp)	# tmp1051, %sfp
	movl	%r15d, %eax	# D.15422, tmp1052
	shrl	$2, %eax	#, tmp1052
	andl	$1, %eax	#, D.15441
	movb	%al, 59(%rsp)	# D.15441, %sfp
	jmp	.L767	#
.L783:
	movq	%rax, %r13	# tempBlock2, tempBlock2
.L767:
	cmpl	$0, 2992(%rsp)	#, isColor
	je	.L741	#,
	movl	%r12d, %edx	# x, D.15422
	movzbl	116(%rsp), %ecx	# %sfp, tmp1307
	sarl	%cl, %edx	# tmp1307, D.15422
	movslq	%edx, %rdx	# D.15422, D.15426
	movq	32(%rsp), %rax	# %sfp, QPptr
	movsbl	(%rax,%rdx), %eax	# *_301, QP
	movq	40(%rsp), %rdi	# %sfp, nonBQPptr
	movsbl	(%rdi,%rdx), %edx	# *_304, *_304
	movl	%edx, 2828(%rsp)	# *_304, c.nonBQP
	jmp	.L742	#
.L741:
	movl	%r12d, %edx	# x, D.15422
	sarl	$4, %edx	#, D.15422
	movslq	%edx, %rdx	# D.15422, D.15426
	movq	32(%rsp), %rax	# %sfp, QPptr
	movsbl	(%rax,%rdx), %eax	# *_310, QP
	movl	112(%rsp), %esi	# %sfp, QPCorrecture
	imull	%esi, %eax	# QPCorrecture, D.15422
	addl	$32768, %eax	#, D.15422
	sarl	$16, %eax	#, QP
	movq	40(%rsp), %rdi	# %sfp, nonBQPptr
	movsbl	(%rdi,%rdx), %edx	# *_316, D.15422
	imull	%esi, %edx	# QPCorrecture, D.15422
	addl	$32768, %edx	#, D.15422
	sarl	$16, %edx	#, tmp889
	movl	%edx, 2828(%rsp)	# tmp889, c.nonBQP
	movq	120(%rsp), %rsi	# %sfp, D.15426
	movzbl	4(%r14,%rsi), %edx	# MEM[base: srcBlock_619, index: _325, offset: 4B], D.15427
	movq	104(%rsp), %rdi	# %sfp, yHistogram
	addq	$1, (%rdi,%rdx,8)	#, *_331
.L742:
	movl	%eax, 2824(%rsp)	# QP, c.QP
#APP
# 3497 "postprocess_template.c" 1
	movd %eax, %mm7         	# QP
	packuswb %mm7, %mm7  
	packuswb %mm7, %mm7  
	packuswb %mm7, %mm7  
	movq %mm7, 1760(%rsp)         	# c.pQPb
	
# 0 "" 2
#NO_APP
	movslq	60(%rsp), %rsi	# %sfp, D.15434
	movslq	12(%rsp), %rbp	# %sfp, D.15434
	movslq	%r12d, %rcx	# x, D.15434
	movslq	48(%rsp), %rdi	# %sfp, D.15434
#APP
# 3517 "postprocess_template.c" 1
	mov %rcx, %rax              	# D.15434
	shr $2, %rax              
	and $6, %rax              
	add %rdi, %rax              	# D.15434
	mov %rax, %rdx       
	imul %rsi, %rax             	# D.15434
	imul %rbp, %rdx             	# D.15434
	prefetchnta 32(%rax, %r14)  	# srcBlock
	prefetcht0 32(%rdx, %rbx)   	# dstBlock
	add %rsi, %rax              	# D.15434
	add %rbp, %rdx              	# D.15434
	prefetchnta 32(%rax, %r14)  	# srcBlock
	prefetcht0 32(%rdx, %rbx)   	# dstBlock
	
# 0 "" 2
#NO_APP
	movq	64(%rsp), %rax	# %sfp, D.15426
	leaq	(%r14,%rax), %rcx	#, D.15425
	movq	72(%rsp), %rax	# %sfp, D.15426
	leaq	(%rbx,%rax), %rdi	#, D.15423
	testb	$8, %r15b	#, D.15422
	je	.L743	#,
	movq	128(%rsp), %rax	# %sfp, packedOffsetAndScale
#APP
# 3102 "postprocess_template.c" 1
	movq (%rax), %mm2        
	movq 8(%rax), %mm3       
	lea (%rcx,%rsi), %rax         	# D.15425, D.15434
	lea (%rdi,%rbp), %rdx         	# D.15423, D.15434
	pxor %mm4, %mm4              
	movq (%rcx), %mm0          	# D.15425
	movq (%rcx), %mm5          	# D.15425
	movq (%rcx, %rsi), %mm1          	# D.15425, D.15434
	movq (%rcx, %rsi), %mm6          	# D.15425, D.15434
	punpcklbw %mm0, %mm0         
	punpckhbw %mm5, %mm5         
	punpcklbw %mm1, %mm1         
	punpckhbw %mm6, %mm6         
	pmulhuw %mm3, %mm0           
	pmulhuw %mm3, %mm5           
	pmulhuw %mm3, %mm1           
	pmulhuw %mm3, %mm6           
	psubw %mm2, %mm0             
	psubw %mm2, %mm5             
	psubw %mm2, %mm1             
	psubw %mm2, %mm6             
	packuswb %mm5, %mm0          
	packuswb %mm6, %mm1          
	movq %mm0, (%rdi)          	# D.15423
	movq %mm1, (%rdi, %rbp)          	# D.15423, D.15434
	movq (%rcx, %rsi, 2), %mm0          	# D.15425, D.15434
	movq (%rcx, %rsi, 2), %mm5          	# D.15425, D.15434
	movq (%rax, %rsi, 2), %mm1          	# D.15434
	movq (%rax, %rsi, 2), %mm6          	# D.15434
	punpcklbw %mm0, %mm0         
	punpckhbw %mm5, %mm5         
	punpcklbw %mm1, %mm1         
	punpckhbw %mm6, %mm6         
	pmulhuw %mm3, %mm0           
	pmulhuw %mm3, %mm5           
	pmulhuw %mm3, %mm1           
	pmulhuw %mm3, %mm6           
	psubw %mm2, %mm0             
	psubw %mm2, %mm5             
	psubw %mm2, %mm1             
	psubw %mm2, %mm6             
	packuswb %mm5, %mm0          
	packuswb %mm6, %mm1          
	movq %mm0, (%rdi, %rbp, 2)          	# D.15423, D.15434
	movq %mm1, (%rdx, %rbp, 2)          	# D.15434
	movq (%rcx, %rsi, 4), %mm0          	# D.15425, D.15434
	movq (%rcx, %rsi, 4), %mm5          	# D.15425, D.15434
	movq (%rax, %rsi, 4), %mm1          	# D.15434
	movq (%rax, %rsi, 4), %mm6          	# D.15434
	punpcklbw %mm0, %mm0         
	punpckhbw %mm5, %mm5         
	punpcklbw %mm1, %mm1         
	punpckhbw %mm6, %mm6         
	pmulhuw %mm3, %mm0           
	pmulhuw %mm3, %mm5           
	pmulhuw %mm3, %mm1           
	pmulhuw %mm3, %mm6           
	psubw %mm2, %mm0             
	psubw %mm2, %mm5             
	psubw %mm2, %mm1             
	psubw %mm2, %mm6             
	packuswb %mm5, %mm0          
	packuswb %mm6, %mm1          
	movq %mm0, (%rdi, %rbp, 4)          	# D.15423, D.15434
	movq %mm1, (%rdx, %rbp, 4)          	# D.15434
	lea (%rax,%rsi,4), %rax        	# D.15434
	lea (%rdx,%rbp,4), %rdx        	# D.15434
	movq (%rax, %rsi), %mm0          	# D.15434
	movq (%rax, %rsi), %mm5          	# D.15434
	movq (%rax, %rsi, 2), %mm1          	# D.15434
	movq (%rax, %rsi, 2), %mm6          	# D.15434
	punpcklbw %mm0, %mm0         
	punpckhbw %mm5, %mm5         
	punpcklbw %mm1, %mm1         
	punpckhbw %mm6, %mm6         
	pmulhuw %mm3, %mm0           
	pmulhuw %mm3, %mm5           
	pmulhuw %mm3, %mm1           
	pmulhuw %mm3, %mm6           
	psubw %mm2, %mm0             
	psubw %mm2, %mm5             
	psubw %mm2, %mm1             
	psubw %mm2, %mm6             
	packuswb %mm5, %mm0          
	packuswb %mm6, %mm1          
	movq %mm0, (%rdx, %rbp)          	# D.15434
	movq %mm1, (%rdx, %rbp, 2)          	# D.15434
	
# 0 "" 2
#NO_APP
	jmp	.L744	#
.L743:
#APP
# 3185 "postprocess_template.c" 1
	lea (%rcx,%rsi), %rax                 	# D.15425, D.15434
	lea (%rdi,%rbp), %rdx                 	# D.15423, D.15434
	movq (%rcx), %mm0          	# D.15425
	movq (%rcx, %rsi), %mm1          	# D.15425, D.15434
	movq %mm0, (%rdi)          	# D.15423
	movq %mm1, (%rdi, %rbp)          	# D.15423, D.15434
	movq (%rcx, %rsi, 2), %mm0          	# D.15425, D.15434
	movq (%rax, %rsi, 2), %mm1          	# D.15434
	movq %mm0, (%rdi, %rbp, 2)          	# D.15423, D.15434
	movq %mm1, (%rdx, %rbp, 2)          	# D.15434
	movq (%rcx, %rsi, 4), %mm0          	# D.15425, D.15434
	movq (%rax, %rsi, 4), %mm1          	# D.15434
	movq %mm0, (%rdi, %rbp, 4)          	# D.15423, D.15434
	movq %mm1, (%rdx, %rbp, 4)          	# D.15434
	lea (%rax,%rsi,4), %rax        	# D.15434
	lea (%rdx,%rbp,4), %rdx        	# D.15434
	movq (%rax, %rsi), %mm0          	# D.15434
	movq (%rax, %rsi, 2), %mm1          	# D.15434
	movq %mm0, (%rdx, %rbp)          	# D.15434
	movq %mm1, (%rdx, %rbp, 2)          	# D.15434
	
# 0 "" 2
#NO_APP
.L744:
	testl	$65536, %r15d	#, D.15422
	je	.L745	#,
	movq	80(%rsp), %rax	# %sfp, D.15426
	leaq	(%rbx,%rax), %rdx	#, D.15423
#APP
# 1455 "postprocess_template.c" 1
	lea (%rdx, %rbp), %rax                	# D.15423, D.15434
	lea (%rax, %rbp, 4), %rcx      	# D.15434
	movq (%rdx), %mm0                       	# D.15423
	movq (%rax, %rbp), %mm1            	# D.15434
	pavgb %mm1, %mm0 
	movq %mm0, (%rax)                
	movq (%rdx, %rbp, 4), %mm0                	# D.15423, D.15434
	pavgb %mm0, %mm1 
	movq %mm1, (%rax, %rbp, 2)         	# D.15434
	movq (%rcx, %rbp), %mm1            	# D.15434
	pavgb %mm1, %mm0 
	movq %mm0, (%rcx)                
	movq (%rdx, %rbp, 8), %mm0                	# D.15423, D.15434
	pavgb %mm0, %mm1 
	movq %mm1, (%rcx, %rbp, 2)         	# D.15434
	
# 0 "" 2
#NO_APP
	jmp	.L746	#
.L745:
	testl	$131072, %r15d	#, D.15422
	je	.L747	#,
	movq	80(%rsp), %rax	# %sfp, D.15426
	leaq	(%rbx,%rax), %rsi	#, D.15423
	movslq	%r12d, %rcx	# x, D.15426
	addq	1752(%rsp), %rcx	# c.deintTemp, D.15423
#APP
# 1775 "postprocess_template.c" 1
	lea (%rsi, %rbp), %rax                	# D.15423, D.15434
	lea (%rax, %rbp, 4), %rdx      	# D.15434
	movq (%rcx), %mm0                       	# D.15423
	movq (%rax), %mm1                
	pavgb %mm1, %mm0 
	movq (%rsi), %mm2                       	# D.15423
	pavgb %mm2, %mm0 
	movq %mm0, (%rsi)                       	# D.15423
	movq (%rax, %rbp), %mm0            	# D.15434
	pavgb %mm0, %mm2 
	pavgb %mm1, %mm2 
	movq %mm2, (%rax)                
	movq (%rax, %rbp, 2), %mm2         	# D.15434
	pavgb %mm2, %mm1 
	pavgb %mm0, %mm1 
	movq %mm1, (%rax, %rbp)            	# D.15434
	movq (%rsi, %rbp, 4), %mm1                	# D.15423, D.15434
	pavgb %mm1, %mm0 
	pavgb %mm2, %mm0 
	movq %mm0, (%rax, %rbp, 2)         	# D.15434
	movq (%rdx), %mm0                
	pavgb %mm0, %mm2 
	pavgb %mm1, %mm2 
	movq %mm2, (%rsi, %rbp, 4)                	# D.15423, D.15434
	movq (%rdx, %rbp), %mm2            	# D.15434
	pavgb %mm2, %mm1 
	pavgb %mm0, %mm1 
	movq %mm1, (%rdx)                
	movq (%rdx, %rbp, 2), %mm1         	# D.15434
	pavgb %mm1, %mm0 
	pavgb %mm2, %mm0 
	movq %mm0, (%rdx, %rbp)            	# D.15434
	movq (%rsi, %rbp, 8), %mm0                	# D.15423, D.15434
	pavgb %mm0, %mm2 
	pavgb %mm1, %mm2 
	movq %mm2, (%rdx, %rbp, 2)         	# D.15434
	movq %mm1, (%rcx)                       	# D.15423
	
# 0 "" 2
#NO_APP
	jmp	.L746	#
.L747:
	testl	$524288, %r15d	#, D.15422
	je	.L748	#,
	movq	80(%rsp), %rax	# %sfp, D.15426
	leaq	(%rbx,%rax), %rcx	#, D.15423
#APP
# 1877 "postprocess_template.c" 1
	lea (%rcx, %rbp), %rax                	# D.15423, D.15434
	lea (%rax, %rbp, 4), %rdx      	# D.15434
	movq (%rcx), %mm0                       	# D.15423
	movq (%rax, %rbp), %mm2            	# D.15434
	movq (%rax), %mm1                
	movq %mm0, %mm3                      
	pmaxub %mm1, %mm0                    
	pminub %mm3, %mm1                    
	pmaxub %mm2, %mm1                    
	pminub %mm1, %mm0                    
	movq %mm0, (%rax)                
	movq (%rcx, %rbp, 4), %mm0                	# D.15423, D.15434
	movq (%rax, %rbp, 2), %mm1         	# D.15434
	movq %mm2, %mm3                      
	pmaxub %mm1, %mm2                    
	pminub %mm3, %mm1                    
	pmaxub %mm0, %mm1                    
	pminub %mm1, %mm2                    
	movq %mm2, (%rax, %rbp, 2)         	# D.15434
	movq (%rdx), %mm2                
	movq (%rdx, %rbp), %mm1            	# D.15434
	movq %mm2, %mm3                      
	pmaxub %mm0, %mm2                    
	pminub %mm3, %mm0                    
	pmaxub %mm1, %mm0                    
	pminub %mm0, %mm2                    
	movq %mm2, (%rdx)                
	movq (%rdx, %rbp, 2), %mm2         	# D.15434
	movq (%rcx, %rbp, 8), %mm0                	# D.15423, D.15434
	movq %mm2, %mm3                      
	pmaxub %mm0, %mm2                    
	pminub %mm3, %mm0                    
	pmaxub %mm1, %mm0                    
	pminub %mm0, %mm2                    
	movq %mm2, (%rdx, %rbp, 2)         	# D.15434
	
# 0 "" 2
#NO_APP
	jmp	.L746	#
.L748:
	cmpl	$0, 152(%rsp)	#, %sfp
	je	.L749	#,
	movq	96(%rsp), %rax	# %sfp, D.15426
	leaq	(%rbx,%rax), %rsi	#, D.15423
#APP
# 1508 "postprocess_template.c" 1
	lea (%rsi, %rbp), %rax                	# D.15423, D.15434
	lea (%rax, %rbp, 4), %rdx      	# D.15434
	lea (%rdx, %rbp, 4), %rcx      	# D.15434
	add %rbp, %rcx                      	# D.15434
	pxor %xmm7, %xmm7                    
	movq (%rsi), %xmm0                    	# D.15423
	movq (%rax, %rbp), %xmm1                    	# D.15434
	movq (%rsi, %rbp, 4), %xmm2                    	# D.15423, D.15434
	movq (%rdx, %rbp), %xmm3                    	# D.15434
	pavgb %xmm2, %xmm1                   
	pavgb %xmm3, %xmm0                   
	punpcklbw %xmm7, %xmm0               
	punpcklbw %xmm7, %xmm1               
	psubw %xmm1, %xmm0                   
	psraw $3, %xmm0                       
	psubw %xmm0, %xmm1                   
	packuswb %xmm1, %xmm1                
	movlps %xmm1, (%rax, %rbp, 2)                  	# D.15434
	movq (%rax, %rbp), %xmm0                    	# D.15434
	movq (%rsi, %rbp, 4), %xmm1                    	# D.15423, D.15434
	movq (%rdx, %rbp), %xmm2                    	# D.15434
	movq (%rsi, %rbp, 8), %xmm3                    	# D.15423, D.15434
	pavgb %xmm2, %xmm1                   
	pavgb %xmm3, %xmm0                   
	punpcklbw %xmm7, %xmm0               
	punpcklbw %xmm7, %xmm1               
	psubw %xmm1, %xmm0                   
	psraw $3, %xmm0                       
	psubw %xmm0, %xmm1                   
	packuswb %xmm1, %xmm1                
	movlps %xmm1, (%rdx)                  
	movq (%rsi, %rbp, 4), %xmm0                    	# D.15423, D.15434
	movq (%rdx, %rbp), %xmm1                    	# D.15434
	movq (%rsi, %rbp, 8), %xmm2                    	# D.15423, D.15434
	movq (%rcx), %xmm3                    
	pavgb %xmm2, %xmm1                   
	pavgb %xmm3, %xmm0                   
	punpcklbw %xmm7, %xmm0               
	punpcklbw %xmm7, %xmm1               
	psubw %xmm1, %xmm0                   
	psraw $3, %xmm0                       
	psubw %xmm0, %xmm1                   
	packuswb %xmm1, %xmm1                
	movlps %xmm1, (%rdx, %rbp, 2)                  	# D.15434
	movq (%rdx, %rbp), %xmm0                    	# D.15434
	movq (%rsi, %rbp, 8), %xmm1                    	# D.15423, D.15434
	movq (%rcx), %xmm2                    
	movq (%rcx, %rbp, 2), %xmm3                    	# D.15434
	pavgb %xmm2, %xmm1                   
	pavgb %xmm3, %xmm0                   
	punpcklbw %xmm7, %xmm0               
	punpcklbw %xmm7, %xmm1               
	psubw %xmm1, %xmm0                   
	psraw $3, %xmm0                       
	psubw %xmm0, %xmm1                   
	packuswb %xmm1, %xmm1                
	movlps %xmm1, (%rdx, %rbp, 4)                  	# D.15434
	
# 0 "" 2
#NO_APP
	jmp	.L746	#
.L749:
	testl	$4194304, %r15d	#, D.15422
	je	.L750	#,
	movq	80(%rsp), %rax	# %sfp, D.15426
	leaq	(%rbx,%rax), %rsi	#, D.15423
	movslq	%r12d, %rcx	# x, D.15426
	addq	1752(%rsp), %rcx	# c.deintTemp, D.15423
#APP
# 1595 "postprocess_template.c" 1
	lea (%rsi, %rbp), %rax                	# D.15423, D.15434
	lea (%rax, %rbp, 4), %rdx      	# D.15434
	pxor %mm7, %mm7                      
	movq (%rcx), %mm0                       	# D.15423
	movq (%rsi), %mm1                     	# D.15423
	movq (%rax), %mm2                     
	movq (%rax, %rbp), %mm3                     	# D.15434
	movq (%rax, %rbp, 2), %mm4                     	# D.15434
	pavgb %mm3, %mm1 
	pavgb %mm4, %mm0 
	movq %mm0, %mm3                      
	punpcklbw %mm7, %mm0                 
	punpckhbw %mm7, %mm3                 
	movq %mm1, %mm4                      
	punpcklbw %mm7, %mm1                 
	punpckhbw %mm7, %mm4                 
	psllw $2, %mm1                        
	psllw $2, %mm4                        
	psubw %mm0, %mm1                     
	psubw %mm3, %mm4                     
	movq %mm2, %mm5                      
	movq %mm2, %mm0                      
	punpcklbw %mm7, %mm2                 
	punpckhbw %mm7, %mm5                 
	paddw %mm2, %mm1                     
	paddw %mm5, %mm4                     
	psraw $2, %mm1                        
	psraw $2, %mm4                        
	packuswb %mm4, %mm1                  
	movq %mm1, (%rax)                     
	movq (%rax, %rbp), %mm1                     	# D.15434
	movq (%rax, %rbp, 2), %mm2                     	# D.15434
	movq (%rsi, %rbp, 4), %mm3                     	# D.15423, D.15434
	movq (%rdx), %mm4                     
	pavgb %mm3, %mm1 
	pavgb %mm4, %mm0 
	movq %mm0, %mm3                      
	punpcklbw %mm7, %mm0                 
	punpckhbw %mm7, %mm3                 
	movq %mm1, %mm4                      
	punpcklbw %mm7, %mm1                 
	punpckhbw %mm7, %mm4                 
	psllw $2, %mm1                        
	psllw $2, %mm4                        
	psubw %mm0, %mm1                     
	psubw %mm3, %mm4                     
	movq %mm2, %mm5                      
	movq %mm2, %mm0                      
	punpcklbw %mm7, %mm2                 
	punpckhbw %mm7, %mm5                 
	paddw %mm2, %mm1                     
	paddw %mm5, %mm4                     
	psraw $2, %mm1                        
	psraw $2, %mm4                        
	packuswb %mm4, %mm1                  
	movq %mm1, (%rax, %rbp, 2)                     	# D.15434
	movq (%rsi, %rbp, 4), %mm1                     	# D.15423, D.15434
	movq (%rdx), %mm2                     
	movq (%rdx, %rbp), %mm3                     	# D.15434
	movq (%rdx, %rbp, 2), %mm4                     	# D.15434
	pavgb %mm3, %mm1 
	pavgb %mm4, %mm0 
	movq %mm0, %mm3                      
	punpcklbw %mm7, %mm0                 
	punpckhbw %mm7, %mm3                 
	movq %mm1, %mm4                      
	punpcklbw %mm7, %mm1                 
	punpckhbw %mm7, %mm4                 
	psllw $2, %mm1                        
	psllw $2, %mm4                        
	psubw %mm0, %mm1                     
	psubw %mm3, %mm4                     
	movq %mm2, %mm5                      
	movq %mm2, %mm0                      
	punpcklbw %mm7, %mm2                 
	punpckhbw %mm7, %mm5                 
	paddw %mm2, %mm1                     
	paddw %mm5, %mm4                     
	psraw $2, %mm1                        
	psraw $2, %mm4                        
	packuswb %mm4, %mm1                  
	movq %mm1, (%rdx)                     
	movq (%rdx, %rbp), %mm1                     	# D.15434
	movq (%rdx, %rbp, 2), %mm2                     	# D.15434
	movq (%rsi, %rbp, 8), %mm3                     	# D.15423, D.15434
	movq (%rdx, %rbp, 4), %mm4                     	# D.15434
	pavgb %mm3, %mm1 
	pavgb %mm4, %mm0 
	movq %mm0, %mm3                      
	punpcklbw %mm7, %mm0                 
	punpckhbw %mm7, %mm3                 
	movq %mm1, %mm4                      
	punpcklbw %mm7, %mm1                 
	punpckhbw %mm7, %mm4                 
	psllw $2, %mm1                        
	psllw $2, %mm4                        
	psubw %mm0, %mm1                     
	psubw %mm3, %mm4                     
	movq %mm2, %mm5                      
	movq %mm2, %mm0                      
	punpcklbw %mm7, %mm2                 
	punpckhbw %mm7, %mm5                 
	paddw %mm2, %mm1                     
	paddw %mm5, %mm4                     
	psraw $2, %mm1                        
	psraw $2, %mm4                        
	packuswb %mm4, %mm1                  
	movq %mm1, (%rdx, %rbp, 2)                     	# D.15434
	movq %mm0, (%rcx)                       	# D.15423
	
# 0 "" 2
#NO_APP
	jmp	.L746	#
.L750:
	testl	$8388608, %r15d	#, D.15422
	je	.L746	#,
	movq	1752(%rsp), %rdx	# c.deintTemp, D.15423
	movslq	%r12d, %rax	# x, D.15426
	movslq	28(%rsp), %rcx	# %sfp, D.15426
	addq	%rax, %rcx	# D.15426, D.15426
	addq	%rdx, %rcx	# D.15423, D.15423
	addq	%rax, %rdx	# D.15426, D.15423
	movl	12(%rsp), %esi	# %sfp,
	movq	%rbx, %rdi	# dstBlock,
	call	deInterlaceL5_SSE2	#
.L746:
	movl	52(%rsp), %eax	# %sfp, D.15421
	addl	$8, %eax	#, D.15421
	cmpl	%eax, 24(%rsp)	# D.15421, %sfp
	jle	.L751	#,
	testl	$512, %r15d	#, D.15422
	je	.L752	#,
	movq	96(%rsp), %rax	# %sfp, D.15426
	leaq	(%rbx,%rax), %rdx	#, D.15423
#APP
# 412 "postprocess_template.c" 1
	pxor %mm7, %mm7                      
	lea (%rdx, %rbp), %rax                	# D.15423, D.15434
	lea (%rax, %rbp, 4), %rcx      	# D.15434
	movq (%rax, %rbp, 2), %mm0         	# D.15434
	movq (%rdx, %rbp, 4), %mm1                	# D.15423, D.15434
	movq %mm1, %mm2                      
	psubusb %mm0, %mm1                   
	psubusb %mm2, %mm0                   
	por %mm1, %mm0                       
	movq (%rcx), %mm3                
	movq (%rcx, %rbp), %mm4            	# D.15434
	movq %mm3, %mm5                      
	psubusb %mm4, %mm3                   
	psubusb %mm5, %mm4                   
	por %mm4, %mm3                       
	pavgb %mm3, %mm0 
	movq %mm2, %mm1                      
	psubusb %mm5, %mm2                   
	movq %mm2, %mm4                      
	pcmpeqb %mm7, %mm2                   
	psubusb %mm1, %mm5                   
	por %mm5, %mm4                       
	psubusb %mm0, %mm4                   
	movq %mm4, %mm3                      
	movq 1760(%rsp), %mm0                         	# c.pQPb
	paddusb %mm0, %mm0                   
	psubusb %mm0, %mm4                   
	pcmpeqb %mm7, %mm4                   
	psubusb b01, %mm3           
	pand %mm4, %mm3                      
	pavgb %mm7, %mm3 
	movq %mm3, %mm1                      
	pavgb %mm7, %mm3 
	pavgb %mm1, %mm3 
	movq (%rdx, %rbp, 4), %mm0                	# D.15423, D.15434
	pxor %mm2, %mm0                      
	psubusb %mm3, %mm0                   
	pxor %mm2, %mm0                      
	movq %mm0, (%rdx, %rbp, 4)                	# D.15423, D.15434
	movq (%rcx), %mm0                
	pxor %mm2, %mm0                      
	paddusb %mm3, %mm0                   
	pxor %mm2, %mm0                      
	movq %mm0, (%rcx)                
	pavgb %mm7, %mm1 
	movq (%rax, %rbp, 2), %mm0         	# D.15434
	pxor %mm2, %mm0                      
	psubusb %mm1, %mm0                   
	pxor %mm2, %mm0                      
	movq %mm0, (%rax, %rbp, 2)         	# D.15434
	movq (%rcx, %rbp), %mm0            	# D.15434
	pxor %mm2, %mm0                      
	paddusb %mm1, %mm0                   
	pxor %mm2, %mm0                      
	movq %mm0, (%rcx, %rbp)            	# D.15434
	pavgb %mm7, %mm1 
	movq (%rax, %rbp), %mm0            	# D.15434
	pxor %mm2, %mm0                      
	psubusb %mm1, %mm0                   
	pxor %mm2, %mm0                      
	movq %mm0, (%rax, %rbp)            	# D.15434
	movq (%rcx, %rbp, 2), %mm0         	# D.15434
	pxor %mm2, %mm0                      
	paddusb %mm1, %mm0                   
	pxor %mm2, %mm0                      
	movq %mm0, (%rcx, %rbp, 2)         	# D.15434
	
# 0 "" 2
#NO_APP
	jmp	.L751	#
.L752:
	testb	$1, %r15b	#, D.15422
	je	.L753	#,
	movq	80(%rsp), %rax	# %sfp, D.15426
	leaq	(%rbx,%rax), %rsi	#, D.15425
	movslq	2828(%rsp), %rax	# c.nonBQP, D.15422
#APP
# 114 "postprocess_template.c" 1
	movq 1776(%rsp,%rax,8), %mm7                         	# c.mmxDcOffset
	movq 2288(%rsp,%rax,8), %mm6                         	# c.mmxDcThreshold
	
# 0 "" 2
# 120 "postprocess_template.c" 1
	lea (%rsi, %rbp), %rax                	# D.15425, D.15434
	movq (%rsi), %mm0                       	# D.15425
	movq (%rax), %mm1                
	movq %mm0, %mm3                      
	movq %mm0, %mm4                      
	pmaxub %mm1, %mm4 
	pminub %mm1, %mm3 
	psubb %mm1, %mm0                     
	paddb %mm7, %mm0                     
	pcmpgtb %mm6, %mm0                   
	movq (%rax,%rbp), %mm2             	# D.15434
	pmaxub %mm2, %mm4 
	pminub %mm2, %mm3 
	psubb %mm2, %mm1                     
	paddb %mm7, %mm1                     
	pcmpgtb %mm6, %mm1                   
	paddb %mm1, %mm0                     
	movq (%rax, %rbp, 2), %mm1         	# D.15434
	pmaxub %mm1, %mm4 
	pminub %mm1, %mm3 
	psubb %mm1, %mm2                     
	paddb %mm7, %mm2                     
	pcmpgtb %mm6, %mm2                   
	paddb %mm2, %mm0                     
	lea (%rax, %rbp, 4), %rax      	# D.15434
	movq (%rsi, %rbp, 4), %mm2                	# D.15425, D.15434
	pmaxub %mm2, %mm4 
	pminub %mm2, %mm3 
	psubb %mm2, %mm1                     
	paddb %mm7, %mm1                     
	pcmpgtb %mm6, %mm1                   
	paddb %mm1, %mm0                     
	movq (%rax), %mm1                
	pmaxub %mm1, %mm4 
	pminub %mm1, %mm3 
	psubb %mm1, %mm2                     
	paddb %mm7, %mm2                     
	pcmpgtb %mm6, %mm2                   
	paddb %mm2, %mm0                     
	movq (%rax, %rbp), %mm2            	# D.15434
	pmaxub %mm2, %mm4 
	pminub %mm2, %mm3 
	psubb %mm2, %mm1                     
	paddb %mm7, %mm1                     
	pcmpgtb %mm6, %mm1                   
	paddb %mm1, %mm0                     
	movq (%rax, %rbp, 2), %mm1         	# D.15434
	pmaxub %mm1, %mm4 
	pminub %mm1, %mm3 
	psubb %mm1, %mm2                     
	paddb %mm7, %mm2                     
	pcmpgtb %mm6, %mm2                   
	paddb %mm2, %mm0                     
	psubusb %mm3, %mm4                   
	                                       
	pxor %mm7, %mm7                      
	psadbw %mm7, %mm0                    
	movq 1760(%rsp), %mm7                         	# c.pQPb
	paddusb %mm7, %mm7                   
	psubusb %mm7, %mm4                   
	packssdw %mm4, %mm4                  
	movd %mm0, %edx                         	# numEq
	movd %mm4, %ecx                         	# dcOk
	
# 0 "" 2
#NO_APP
	negl	%edx	# D.15422
	movzbl	%dl, %edx	# D.15422, numEq
	cmpl	2896(%rsp), %edx	# c.ppMode.flatnessThreshold, numEq
	jle	.L754	#,
	testl	%ecx, %ecx	# dcOk
	jne	.L751	#,
	movq	96(%rsp), %rax	# %sfp, D.15426
	leaq	(%rbx,%rax), %rdx	#, D.15423
#APP
# 232 "postprocess_template.c" 1
	movq 1760(%rsp), %mm0                         	# c.pQPb
	pxor %mm4, %mm4                      
	movq (%rdx), %mm6                       	# D.15423
	movq (%rdx, %rbp), %mm5                   	# D.15423, D.15434
	movq %mm5, %mm1                      
	movq %mm6, %mm2                      
	psubusb %mm6, %mm5                   
	psubusb %mm1, %mm2                   
	por %mm5, %mm2                       
	psubusb %mm0, %mm2                   
	pcmpeqb %mm4, %mm2                   
	pand %mm2, %mm6                      
	pandn %mm1, %mm2                     
	por %mm2, %mm6                       
	movq (%rdx, %rbp, 8), %mm5                	# D.15423, D.15434
	lea (%rdx, %rbp, 4), %rax             	# D.15423, D.15434
	lea (%rdx, %rbp, 8), %rcx             	# D.15423, D.15434
	sub %rbp, %rcx                      	# D.15434
	add %rbp, %rdx                             	# D.15434, D.15423
	movq (%rdx, %rbp, 8), %mm7                	# D.15423, D.15434
	movq %mm5, %mm1                      
	movq %mm7, %mm2                      
	psubusb %mm7, %mm5                   
	psubusb %mm1, %mm2                   
	por %mm5, %mm2                       
	psubusb %mm0, %mm2                   
	pcmpeqb %mm4, %mm2                   
	pand %mm2, %mm7                      
	pandn %mm1, %mm2                     
	por %mm2, %mm7                       
	movq (%rdx, %rbp), %mm0                   	# D.15423, D.15434
	movq %mm0, %mm1                      
	pavgb %mm6, %mm0 
	pavgb %mm6, %mm0 
	movq (%rdx, %rbp, 4), %mm2                	# D.15423, D.15434
	movq %mm2, %mm5                      
	pavgb (%rax), %mm2 
	pavgb (%rdx, %rbp, 2), %mm2 	# D.15423, D.15434
	movq %mm2, %mm3                      
	movq (%rdx), %mm4                       	# D.15423
	pavgb %mm4, %mm3 
	pavgb %mm0, %mm3 
	movq %mm3, (%rdx)                       	# D.15423
	movq %mm1, %mm0                      
	pavgb %mm6, %mm0 
	movq %mm4, %mm3                      
	pavgb (%rdx,%rbp,2), %mm3 	# D.15423, D.15434
	pavgb (%rax,%rbp,2), %mm5 	# D.15434
	pavgb (%rax), %mm5 
	pavgb %mm5, %mm3 
	pavgb %mm0, %mm3 
	movq %mm3, (%rdx,%rbp)                    	# D.15423, D.15434
	pavgb %mm4, %mm6 
	movq (%rcx), %mm0                
	pavgb (%rax, %rbp, 2), %mm0 	# D.15434
	movq %mm0, %mm3                      
	pavgb %mm1, %mm0 
	pavgb %mm6, %mm0 
	pavgb %mm2, %mm0 
	movq (%rdx, %rbp, 2), %mm2                	# D.15423, D.15434
	movq %mm0, (%rdx, %rbp, 2)                	# D.15423, D.15434
	movq (%rax, %rbp, 4), %mm0         	# D.15434
	pavgb (%rcx), %mm0 
	pavgb %mm0, %mm6 
	pavgb %mm1, %mm4 
	pavgb %mm2, %mm1 
	pavgb %mm1, %mm6 
	pavgb %mm5, %mm6 
	movq (%rax), %mm5                
	movq %mm6, (%rax)                
	movq (%rax, %rbp, 4), %mm6         	# D.15434
	pavgb %mm7, %mm6 
	pavgb %mm4, %mm6 
	pavgb %mm3, %mm6 
	pavgb %mm5, %mm2 
	movq (%rdx, %rbp, 4), %mm4                	# D.15423, D.15434
	pavgb %mm4, %mm2 
	pavgb %mm2, %mm6 
	movq %mm6, (%rdx, %rbp, 4)                	# D.15423, D.15434
	pavgb %mm7, %mm1 
	pavgb %mm4, %mm5 
	pavgb %mm5, %mm0 
	movq (%rax, %rbp, 2), %mm6         	# D.15434
	pavgb %mm6, %mm1 
	pavgb %mm0, %mm1 
	movq %mm1, (%rax, %rbp, 2)         	# D.15434
	pavgb (%rcx), %mm2 
	movq (%rax, %rbp, 4), %mm0         	# D.15434
	pavgb %mm0, %mm6 
	pavgb %mm7, %mm6 
	pavgb %mm2, %mm6 
	movq %mm6, (%rcx)                
	pavgb %mm7, %mm5 
	pavgb %mm7, %mm5 
	pavgb %mm3, %mm0 
	pavgb %mm0, %mm5 
	movq %mm5, (%rax, %rbp, 4)         	# D.15434
	sub %rbp, %rdx                             	# D.15434, D.15423
	
# 0 "" 2
#NO_APP
	jmp	.L751	#
.L754:
#APP
# 552 "postprocess_template.c" 1
	lea (%rsi, %rbp), %rax                	# D.15425, D.15434
	pcmpeqb %mm6, %mm6                   
	movq (%rax, %rbp, 2), %mm1         	# D.15434
	movq (%rsi, %rbp, 4), %mm0                	# D.15425, D.15434
	pxor %mm6, %mm1                      
	pavgb %mm1, %mm0 
	movq (%rax, %rbp, 4), %mm2         	# D.15434
	movq (%rax, %rbp), %mm3            	# D.15434
	pxor %mm6, %mm2                      
	movq %mm2, %mm5                      
	movq b80, %mm4              
	lea (%rax, %rbp, 4), %rcx      	# D.15434
	pavgb %mm3, %mm2 
	pavgb %mm0, %mm4 
	pavgb %mm2, %mm4 
	pavgb %mm0, %mm4 
	movq (%rax), %mm2                
	pxor %mm6, %mm2                      
	pavgb %mm3, %mm2 
	pavgb (%rsi), %mm1 	# D.15425
	movq b80, %mm3              
	pavgb %mm2, %mm3 
	pavgb %mm1, %mm3 
	pavgb %mm2, %mm3 
	pavgb (%rcx, %rbp), %mm5 	# D.15434
	movq (%rcx, %rbp, 2), %mm1         	# D.15434
	pxor %mm6, %mm1                      
	pavgb (%rsi, %rbp, 4), %mm1 	# D.15425, D.15434
	movq b80, %mm2              
	pavgb %mm5, %mm2 
	pavgb %mm1, %mm2 
	pavgb %mm5, %mm2 
	movq b00, %mm1              
	movq b00, %mm5              
	psubb %mm2, %mm1                     
	psubb %mm3, %mm5                     
	pmaxub %mm1, %mm2 
	pmaxub %mm5, %mm3 
	pminub %mm2, %mm3 
	movq b00, %mm7              
	movq 1760(%rsp), %mm2                         	# c.pQPb
	pavgb %mm6, %mm2 
	psubb %mm6, %mm2                     
	movq %mm4, %mm1                      
	pcmpgtb %mm7, %mm1                   
	pxor %mm1, %mm4                      
	psubb %mm1, %mm4                     
	pcmpgtb %mm4, %mm2                   
	psubusb %mm3, %mm4                   
	movq %mm4, %mm3                      
	psubusb b01, %mm4           
	pavgb %mm7, %mm4 
	pavgb %mm7, %mm4 
	paddb %mm3, %mm4                     
	pand %mm2, %mm4                      
	movq b80, %mm5              
	psubb %mm0, %mm5                     
	paddsb %mm6, %mm5                    
	pcmpgtb %mm5, %mm7                   
	pxor %mm7, %mm5                      
	pminub %mm5, %mm4 
	pxor %mm1, %mm7                      
	pand %mm7, %mm4                      
	movq (%rax, %rbp, 2), %mm0         	# D.15434
	movq (%rsi, %rbp, 4), %mm2                	# D.15425, D.15434
	pxor %mm1, %mm0                      
	pxor %mm1, %mm2                      
	paddb %mm4, %mm0                     
	psubb %mm4, %mm2                     
	pxor %mm1, %mm0                      
	pxor %mm1, %mm2                      
	movq %mm0, (%rax, %rbp, 2)         	# D.15434
	movq %mm2, (%rsi, %rbp, 4)                	# D.15425, D.15434
	
# 0 "" 2
#NO_APP
	jmp	.L751	#
.L753:
	testl	$1024, %r15d	#, D.15422
	je	.L751	#,
	movq	96(%rsp), %rax	# %sfp, D.15426
	leaq	(%rbx,%rax), %rdx	#, D.15423
	movslq	2828(%rsp), %rax	# c.nonBQP, D.15422
#APP
# 2553 "postprocess_template.c" 1
	movq 1776(%rsp,%rax,8), %mm7                         	# c.mmxDcOffset
	movq 2288(%rsp,%rax,8), %mm6                         	# c.mmxDcThreshold
	
# 0 "" 2
# 2559 "postprocess_template.c" 1
	lea (%rdx, %rbp), %rax                	# D.15423, D.15434
	movq (%rdx), %mm0                       	# D.15423
	movq (%rax), %mm1                
	movq %mm1, %mm3                      
	movq %mm1, %mm4                      
	psubb %mm1, %mm0                     
	paddb %mm7, %mm0                     
	pcmpgtb %mm6, %mm0                   
	movq (%rax,%rbp), %mm2             	# D.15434
	pmaxub %mm2, %mm4 
	pminub %mm2, %mm3 
	psubb %mm2, %mm1                     
	paddb %mm7, %mm1                     
	pcmpgtb %mm6, %mm1                   
	paddb %mm1, %mm0                     
	movq (%rax, %rbp, 2), %mm1         	# D.15434
	pmaxub %mm1, %mm4 
	pminub %mm1, %mm3 
	psubb %mm1, %mm2                     
	paddb %mm7, %mm2                     
	pcmpgtb %mm6, %mm2                   
	paddb %mm2, %mm0                     
	lea (%rax, %rbp, 4), %rax      	# D.15434
	movq (%rdx, %rbp, 4), %mm2                	# D.15423, D.15434
	pmaxub %mm2, %mm4 
	pminub %mm2, %mm3 
	psubb %mm2, %mm1                     
	paddb %mm7, %mm1                     
	pcmpgtb %mm6, %mm1                   
	paddb %mm1, %mm0                     
	movq (%rax), %mm1                
	pmaxub %mm1, %mm4 
	pminub %mm1, %mm3 
	psubb %mm1, %mm2                     
	paddb %mm7, %mm2                     
	pcmpgtb %mm6, %mm2                   
	paddb %mm2, %mm0                     
	movq (%rax, %rbp), %mm2            	# D.15434
	pmaxub %mm2, %mm4 
	pminub %mm2, %mm3 
	psubb %mm2, %mm1                     
	paddb %mm7, %mm1                     
	pcmpgtb %mm6, %mm1                   
	paddb %mm1, %mm0                     
	movq (%rax, %rbp, 2), %mm1         	# D.15434
	pmaxub %mm1, %mm4 
	pminub %mm1, %mm3 
	psubb %mm1, %mm2                     
	paddb %mm7, %mm2                     
	pcmpgtb %mm6, %mm2                   
	paddb %mm2, %mm0                     
	movq (%rdx, %rbp, 8), %mm2                	# D.15423, D.15434
	pmaxub %mm2, %mm4 
	pminub %mm2, %mm3 
	psubb %mm2, %mm1                     
	paddb %mm7, %mm1                     
	pcmpgtb %mm6, %mm1                   
	paddb %mm1, %mm0                     
	movq (%rax, %rbp, 4), %mm1         	# D.15434
	psubb %mm1, %mm2                     
	paddb %mm7, %mm2                     
	pcmpgtb %mm6, %mm2                   
	paddb %mm2, %mm0                     
	psubusb %mm3, %mm4                   
	pxor %mm6, %mm6                      
	movq 1760(%rsp), %mm7                         	# c.pQPb
	paddusb %mm7, %mm7                   
	psubusb %mm4, %mm7                   
	pcmpeqb %mm6, %mm7                   
	pcmpeqb %mm6, %mm7                   
	movq %mm7, 304(%rsp)                         	# dc_mask
	movq 2896(%rsp), %mm7                         	# c.ppMode.flatnessThreshold
	punpcklbw %mm7, %mm7                 
	punpcklbw %mm7, %mm7                 
	punpcklbw %mm7, %mm7                 
	psubb %mm0, %mm6                     
	pcmpgtb %mm7, %mm6                   
	movq %mm6, 312(%rsp)                         	# eq_mask
	
# 0 "" 2
#NO_APP
	movq	312(%rsp), %rcx	# eq_mask, D.15434
	movq	%rcx, %rax	# D.15434, D.15434
	andq	304(%rsp), %rax	# dc_mask, D.15434
	movq	%rax, 320(%rsp)	# D.15434, both_masks
	testq	%rax, %rax	# D.15434
	je	.L755	#,
	leaq	368(%rsp), %rax	#, tmp936
	movq	%rdx, %rsi	# src, src
#APP
# 2664 "postprocess_template.c" 1
	movq 1760(%rsp), %mm0                         	# c.pQPb
	pxor %mm4, %mm4                      
	movq (%rdx), %mm6                       	# src
	movq (%rdx, %rbp), %mm5                   	# src, D.15434
	movq %mm5, %mm1                      
	movq %mm6, %mm2                      
	psubusb %mm6, %mm5                   
	psubusb %mm1, %mm2                   
	por %mm5, %mm2                       
	psubusb %mm2, %mm0                   
	pcmpeqb %mm4, %mm0                   
	pxor %mm6, %mm1                      
	pand %mm0, %mm1                      
	pxor %mm1, %mm6                      
	movq (%rdx, %rbp, 8), %mm5                	# src, D.15434
	add %rbp, %rdx                             	# D.15434, src
	movq (%rdx, %rbp, 8), %mm7                	# src, D.15434
	movq %mm5, %mm1                      
	movq %mm7, %mm2                      
	psubusb %mm7, %mm5                   
	psubusb %mm1, %mm2                   
	por %mm5, %mm2                       
	movq 1760(%rsp), %mm0                         	# c.pQPb
	psubusb %mm2, %mm0                   
	pcmpeqb %mm4, %mm0                   
	pxor %mm7, %mm1                      
	pand %mm0, %mm1                      
	pxor %mm1, %mm7                      
	movq %mm6, %mm5                      
	punpckhbw %mm4, %mm6                 
	punpcklbw %mm4, %mm5                 
	movq %mm5, %mm0                      
	movq %mm6, %mm1                      
	psllw $2, %mm0                        
	psllw $2, %mm1                        
	paddw w04, %mm0             
	paddw w04, %mm1             
	movq (%rdx), %mm2                       	# src
	movq (%rdx), %mm3                       	# src
	add %rbp, %rdx                             	# D.15434, src
	punpcklbw %mm4, %mm2                 
	punpckhbw %mm4, %mm3                 
	paddw %mm2, %mm0                     
	paddw %mm3, %mm1                     
	movq (%rdx), %mm2                       	# src
	movq (%rdx), %mm3                       	# src
	add %rbp, %rdx                             	# D.15434, src
	punpcklbw %mm4, %mm2                 
	punpckhbw %mm4, %mm3                 
	paddw %mm2, %mm0                     
	paddw %mm3, %mm1                     
	movq (%rdx), %mm2                       	# src
	movq (%rdx), %mm3                       	# src
	add %rbp, %rdx                             	# D.15434, src
	punpcklbw %mm4, %mm2                 
	punpckhbw %mm4, %mm3                 
	paddw %mm2, %mm0                     
	paddw %mm3, %mm1                     
	movq %mm0, (%rax)                       	# tmp936
	movq %mm1, 8(%rax)                      	# tmp936
	movq (%rdx), %mm2                       	# src
	movq (%rdx), %mm3                       	# src
	add %rbp, %rdx                             	# D.15434, src
	punpcklbw %mm4, %mm2                 
	punpckhbw %mm4, %mm3                 
	paddw %mm2, %mm0                     
	paddw %mm3, %mm1                     
	psubw %mm5, %mm0                     
	psubw %mm6, %mm1                     
	movq %mm0, 16(%rax)                     	# tmp936
	movq %mm1, 24(%rax)                     	# tmp936
	movq (%rdx), %mm2                       	# src
	movq (%rdx), %mm3                       	# src
	add %rbp, %rdx                             	# D.15434, src
	punpcklbw %mm4, %mm2                 
	punpckhbw %mm4, %mm3                 
	paddw %mm2, %mm0                     
	paddw %mm3, %mm1                     
	psubw %mm5, %mm0                     
	psubw %mm6, %mm1                     
	movq %mm0, 32(%rax)                     	# tmp936
	movq %mm1, 40(%rax)                     	# tmp936
	movq (%rdx), %mm2                       	# src
	movq (%rdx), %mm3                       	# src
	add %rbp, %rdx                             	# D.15434, src
	punpcklbw %mm4, %mm2                 
	punpckhbw %mm4, %mm3                 
	paddw %mm2, %mm0                     
	paddw %mm3, %mm1                     
	psubw %mm5, %mm0                     
	psubw %mm6, %mm1                     
	movq %mm0, 48(%rax)                     	# tmp936
	movq %mm1, 56(%rax)                     	# tmp936
	movq (%rdx), %mm2                       	# src
	movq (%rdx), %mm3                       	# src
	add %rbp, %rdx                             	# D.15434, src
	punpcklbw %mm4, %mm2                 
	punpckhbw %mm4, %mm3                 
	paddw %mm2, %mm0                     
	paddw %mm3, %mm1                     
	psubw %mm5, %mm0                     
	psubw %mm6, %mm1                     
	movq %mm0, 64(%rax)                     	# tmp936
	movq %mm1, 72(%rax)                     	# tmp936
	movq %mm7, %mm6                      
	punpckhbw %mm4, %mm7                 
	punpcklbw %mm4, %mm6                 
	movq (%rdx), %mm2                       	# src
	movq (%rdx), %mm3                       	# src
	add %rbp, %rdx                             	# D.15434, src
	punpcklbw %mm4, %mm2                 
	punpckhbw %mm4, %mm3                 
	paddw %mm2, %mm0                     
	paddw %mm3, %mm1                     
	mov %rsi, %rdx                             	# src, src
	add %rbp, %rdx                             	# D.15434, src
	movq (%rdx), %mm2                       	# src
	movq (%rdx), %mm3                       	# src
	add %rbp, %rdx                             	# D.15434, src
	punpcklbw %mm4, %mm2                 
	punpckhbw %mm4, %mm3                 
	psubw %mm2, %mm0                     
	psubw %mm3, %mm1                     
	movq %mm0, 80(%rax)                     	# tmp936
	movq %mm1, 88(%rax)                     	# tmp936
	movq (%rdx), %mm2                       	# src
	movq (%rdx), %mm3                       	# src
	add %rbp, %rdx                             	# D.15434, src
	punpcklbw %mm4, %mm2                 
	punpckhbw %mm4, %mm3                 
	psubw %mm2, %mm0                     
	psubw %mm3, %mm1                     
	paddw %mm6, %mm0                     
	paddw %mm7, %mm1                     
	movq %mm0, 96(%rax)                     	# tmp936
	movq %mm1, 104(%rax)                    	# tmp936
	movq (%rdx), %mm2                       	# src
	movq (%rdx), %mm3                       	# src
	add %rbp, %rdx                             	# D.15434, src
	punpcklbw %mm4, %mm2                 
	punpckhbw %mm4, %mm3                 
	psubw %mm2, %mm0                     
	psubw %mm3, %mm1                     
	paddw %mm6, %mm0                     
	paddw %mm7, %mm1                     
	movq %mm0, 112(%rax)                    	# tmp936
	movq %mm1, 120(%rax)                    	# tmp936
	movq (%rdx), %mm2                       	# src
	movq (%rdx), %mm3                       	# src
	add %rbp, %rdx                             	# D.15434, src
	punpcklbw %mm4, %mm2                 
	punpckhbw %mm4, %mm3                 
	psubw %mm2, %mm0                     
	psubw %mm3, %mm1                     
	paddw %mm6, %mm0                     
	paddw %mm7, %mm1                     
	movq %mm0, 128(%rax)                    	# tmp936
	movq %mm1, 136(%rax)                    	# tmp936
	movq (%rdx), %mm2                       	# src
	movq (%rdx), %mm3                       	# src
	add %rbp, %rdx                             	# D.15434, src
	punpcklbw %mm4, %mm2                 
	punpckhbw %mm4, %mm3                 
	psubw %mm2, %mm0                     
	psubw %mm3, %mm1                     
	paddw %mm6, %mm0                     
	paddw %mm7, %mm1                     
	movq %mm0, 144(%rax)                    	# tmp936
	movq %mm1, 152(%rax)                    	# tmp936
	mov %rsi, %rdx                             	# src, src
	
# 0 "" 2
#NO_APP
	addq	88(%rsp), %rdx	# %sfp, src
	movq	184(%rsp), %rsi	# %sfp, D.15426
	leaq	(%rdx,%rsi), %rdi	#, D.15423
	movq	176(%rsp), %rsi	# %sfp, offset
#APP
# 2804 "postprocess_template.c" 1
	movq 320(%rsp), %mm6                         	# both_masks
	pcmpeqb %mm5, %mm5                   
	pxor %mm6, %mm5                      
	pxor %mm7, %mm7                      
	1:                                     
	movq (%rax), %mm0                       	# temp_sums
	movq 8(%rax), %mm1                      	# temp_sums
	paddw 32(%rax), %mm0                    	# temp_sums
	paddw 40(%rax), %mm1                    	# temp_sums
	movq (%rsi, %rdi), %mm2                   	# offset, D.15423
	movq %mm2, %mm3                      
	movq %mm2, %mm4                      
	punpcklbw %mm7, %mm2                 
	punpckhbw %mm7, %mm3                 
	paddw %mm2, %mm0                     
	paddw %mm3, %mm1                     
	paddw %mm2, %mm0                     
	paddw %mm3, %mm1                     
	psrlw $4, %mm0                        
	psrlw $4, %mm1                        
	packuswb %mm1, %mm0                  
	pand %mm6, %mm0                      
	pand %mm5, %mm4                      
	por %mm4, %mm0                       
	movq %mm0, (%rsi, %rdi)                   	# offset, D.15423
	add $16, %rax                            	# temp_sums
	add %rbp, %rsi                             	# D.15434, offset
	 js 1b                                 
	
# 0 "" 2
#NO_APP
	jmp	.L756	#
.L755:
	movq	88(%rsp), %rdx	# %sfp, D.15424
	addq	96(%rsp), %rdx	# %sfp, D.15424
	addq	%rbx, %rdx	# dstBlock, src
.L756:
	cmpq	$-1, %rcx	#, D.15434
	je	.L751	#,
	leaq	336(%rsp), %rcx	#, tmp943
#APP
# 2844 "postprocess_template.c" 1
	pxor %mm7, %mm7                      
	movq (%rdx), %mm0                       	# temp_src
	movq %mm0, %mm1                      
	punpcklbw %mm7, %mm0                 
	punpckhbw %mm7, %mm1                 
	movq (%rdx, %rbp), %mm2                   	# temp_src, D.15434
	lea (%rdx, %rbp, 2), %rax             	# temp_src, D.15434
	movq %mm2, %mm3                      
	punpcklbw %mm7, %mm2                 
	punpckhbw %mm7, %mm3                 
	movq (%rax), %mm4                
	movq %mm4, %mm5                      
	punpcklbw %mm7, %mm4                 
	punpckhbw %mm7, %mm5                 
	paddw %mm0, %mm0                     
	paddw %mm1, %mm1                     
	psubw %mm4, %mm2                     
	psubw %mm5, %mm3                     
	psubw %mm2, %mm0                     
	psubw %mm3, %mm1                     
	psllw $2, %mm2                        
	psllw $2, %mm3                        
	psubw %mm2, %mm0                     
	psubw %mm3, %mm1                     
	movq (%rax, %rbp), %mm2            	# D.15434
	movq %mm2, %mm3                      
	punpcklbw %mm7, %mm2                 
	punpckhbw %mm7, %mm3                 
	psubw %mm2, %mm0                     
	psubw %mm3, %mm1                     
	psubw %mm2, %mm0                     
	psubw %mm3, %mm1                     
	movq %mm0, (%rcx)                       	# tmp943
	movq %mm1, 8(%rcx)                      	# tmp943
	movq (%rax, %rbp, 2), %mm0         	# D.15434
	movq %mm0, %mm1                      
	punpcklbw %mm7, %mm0                 
	punpckhbw %mm7, %mm1                 
	psubw %mm0, %mm2                     
	psubw %mm1, %mm3                     
	movq %mm2, 16(%rcx)                     	# tmp943
	movq %mm3, 24(%rcx)                     	# tmp943
	paddw %mm4, %mm4                     
	paddw %mm5, %mm5                     
	psubw %mm2, %mm4                     
	psubw %mm3, %mm5                     
	lea (%rax, %rbp), %rdx                	# D.15434, temp_src
	psllw $2, %mm2                        
	psllw $2, %mm3                        
	psubw %mm2, %mm4                     
	psubw %mm3, %mm5                     
	movq (%rdx, %rbp, 2), %mm2                	# temp_src, D.15434
	movq %mm2, %mm3                      
	punpcklbw %mm7, %mm2                 
	punpckhbw %mm7, %mm3                 
	psubw %mm2, %mm4                     
	psubw %mm3, %mm5                     
	psubw %mm2, %mm4                     
	psubw %mm3, %mm5                     
	movq (%rax, %rbp, 4), %mm6         	# D.15434
	punpcklbw %mm7, %mm6                 
	psubw %mm6, %mm2                     
	movq (%rax, %rbp, 4), %mm6         	# D.15434
	punpckhbw %mm7, %mm6                 
	psubw %mm6, %mm3                     
	paddw %mm0, %mm0                     
	paddw %mm1, %mm1                     
	psubw %mm2, %mm0                     
	psubw %mm3, %mm1                     
	psllw $2, %mm2                        
	psllw $2, %mm3                        
	psubw %mm2, %mm0                     
	psubw %mm3, %mm1                     
	movq (%rdx, %rbp, 4), %mm2                	# temp_src, D.15434
	movq %mm2, %mm3                      
	punpcklbw %mm7, %mm2                 
	punpckhbw %mm7, %mm3                 
	paddw %mm2, %mm2                     
	paddw %mm3, %mm3                     
	psubw %mm2, %mm0                     
	psubw %mm3, %mm1                     
	movq (%rcx), %mm2                       	# tmp943
	movq 8(%rcx), %mm3                      	# tmp943
	movq %mm7, %mm6                      
	psubw %mm0, %mm6                     
	pmaxsw %mm6, %mm0                    
	movq %mm7, %mm6                      
	psubw %mm1, %mm6                     
	pmaxsw %mm6, %mm1                    
	movq %mm7, %mm6                      
	psubw %mm2, %mm6                     
	pmaxsw %mm6, %mm2                    
	movq %mm7, %mm6                      
	psubw %mm3, %mm6                     
	pmaxsw %mm6, %mm3                    
	pminsw %mm2, %mm0                    
	pminsw %mm3, %mm1                    
	movd 1760(%rsp), %mm2                         	# c.pQPb
	punpcklbw %mm7, %mm2                 
	movq %mm7, %mm6                      
	pcmpgtw %mm4, %mm6                   
	pxor %mm6, %mm4                      
	psubw %mm6, %mm4                     
	pcmpgtw %mm5, %mm7                   
	pxor %mm7, %mm5                      
	psubw %mm7, %mm5                     
	psllw $3, %mm2                        
	movq %mm2, %mm3                      
	pcmpgtw %mm4, %mm2                   
	pcmpgtw %mm5, %mm3                   
	pand %mm2, %mm4                      
	pand %mm3, %mm5                      
	psubusw %mm0, %mm4                   
	psubusw %mm1, %mm5                   
	movq w05, %mm2              
	pmullw %mm2, %mm4                    
	pmullw %mm2, %mm5                    
	movq w20, %mm2              
	paddw %mm2, %mm4                     
	paddw %mm2, %mm5                     
	psrlw $6, %mm4                        
	psrlw $6, %mm5                        
	movq 16(%rcx), %mm0                     	# tmp943
	movq 24(%rcx), %mm1                     	# tmp943
	pxor %mm2, %mm2                      
	pxor %mm3, %mm3                      
	pcmpgtw %mm0, %mm2                   
	pcmpgtw %mm1, %mm3                   
	pxor %mm2, %mm0                      
	pxor %mm3, %mm1                      
	psubw %mm2, %mm0                     
	psubw %mm3, %mm1                     
	psrlw $1, %mm0                        
	psrlw $1, %mm1                        
	pxor %mm6, %mm2                      
	pxor %mm7, %mm3                      
	pand %mm2, %mm4                      
	pand %mm3, %mm5                      
	pminsw %mm0, %mm4                    
	pminsw %mm1, %mm5                    
	pxor %mm6, %mm4                      
	pxor %mm7, %mm5                      
	psubw %mm6, %mm4                     
	psubw %mm7, %mm5                     
	packsswb %mm5, %mm4                  
	movq 312(%rsp), %mm1                         	# eq_mask
	pandn %mm4, %mm1                     
	movq (%rdx), %mm0                       	# temp_src
	paddb   %mm1, %mm0                   
	movq %mm0, (%rdx)                       	# temp_src
	movq (%rdx, %rbp), %mm0                   	# temp_src, D.15434
	psubb %mm1, %mm0                     
	movq %mm0, (%rdx, %rbp)                   	# temp_src, D.15434
	
# 0 "" 2
#NO_APP
.L751:
	movq	(%rsp), %rdi	# %sfp, tempBlock2
#APP
# 1995 "postprocess_template.c" 1
	lea (%rbx, %rbp), %rax                	# dstBlock, D.15434
	movq (%rbx), %mm0                       	# dstBlock
	movq (%rax), %mm1                
	movq %mm0, %mm2                      
	punpcklbw %mm1, %mm0                 
	punpckhbw %mm1, %mm2                 
	movq (%rax, %rbp), %mm1            	# D.15434
	movq (%rax, %rbp, 2), %mm3         	# D.15434
	movq %mm1, %mm4                      
	punpcklbw %mm3, %mm1                 
	punpckhbw %mm3, %mm4                 
	movq %mm0, %mm3                      
	punpcklwd %mm1, %mm0                 
	punpckhwd %mm1, %mm3                 
	movq %mm2, %mm1                      
	punpcklwd %mm4, %mm2                 
	punpckhwd %mm4, %mm1                 
	movd %mm0, 128(%r13)                    	# tempBlock2
	psrlq $32, %mm0                       
	movd %mm0, 144(%r13)                    	# tempBlock2
	movd %mm3, 160(%r13)                    	# tempBlock2
	psrlq $32, %mm3                       
	movd %mm3, 176(%r13)                    	# tempBlock2
	movd %mm3, 48(%rdi)                     	# tempBlock2
	movd %mm2, 192(%r13)                    	# tempBlock2
	movd %mm2, 64(%rdi)                     	# tempBlock2
	psrlq $32, %mm2                       
	movd %mm2, 80(%rdi)                     	# tempBlock2
	movd %mm1, 96(%rdi)                     	# tempBlock2
	psrlq $32, %mm1                       
	movd %mm1, 112(%rdi)                    	# tempBlock2
	lea (%rax, %rbp, 4), %rax      	# D.15434
	movq (%rbx, %rbp, 4), %mm0                	# dstBlock, D.15434
	movq (%rax), %mm1                
	movq %mm0, %mm2                      
	punpcklbw %mm1, %mm0                 
	punpckhbw %mm1, %mm2                 
	movq (%rax, %rbp), %mm1            	# D.15434
	movq (%rax, %rbp, 2), %mm3         	# D.15434
	movq %mm1, %mm4                      
	punpcklbw %mm3, %mm1                 
	punpckhbw %mm3, %mm4                 
	movq %mm0, %mm3                      
	punpcklwd %mm1, %mm0                 
	punpckhwd %mm1, %mm3                 
	movq %mm2, %mm1                      
	punpcklwd %mm4, %mm2                 
	punpckhwd %mm4, %mm1                 
	movd %mm0, 132(%r13)                    	# tempBlock2
	psrlq $32, %mm0                       
	movd %mm0, 148(%r13)                    	# tempBlock2
	movd %mm3, 164(%r13)                    	# tempBlock2
	psrlq $32, %mm3                       
	movd %mm3, 180(%r13)                    	# tempBlock2
	movd %mm3, 52(%rdi)                     	# tempBlock2
	movd %mm2, 196(%r13)                    	# tempBlock2
	movd %mm2, 68(%rdi)                     	# tempBlock2
	psrlq $32, %mm2                       
	movd %mm2, 84(%rdi)                     	# tempBlock2
	movd %mm1, 100(%rdi)                    	# tempBlock2
	psrlq $32, %mm1                       
	movd %mm1, 116(%rdi)                    	# tempBlock2
	
# 0 "" 2
#NO_APP
	movl	%r12d, %eax	# x, tmp1134
	subl	$8, %eax	#, tmp1134
	js	.L758	#,
	testl	$8192, %r15d	#, D.15422
	je	.L759	#,
	leaq	48(%r13), %rdx	#, src
	movl	$16, %esi	#, tmp947
#APP
# 412 "postprocess_template.c" 1
	pxor %mm7, %mm7                      
	lea (%rdx, %rsi), %rax                	# src, tmp947
	lea (%rax, %rsi, 4), %rcx      	# tmp947
	movq (%rax, %rsi, 2), %mm0         	# tmp947
	movq (%rdx, %rsi, 4), %mm1                	# src, tmp947
	movq %mm1, %mm2                      
	psubusb %mm0, %mm1                   
	psubusb %mm2, %mm0                   
	por %mm1, %mm0                       
	movq (%rcx), %mm3                
	movq (%rcx, %rsi), %mm4            	# tmp947
	movq %mm3, %mm5                      
	psubusb %mm4, %mm3                   
	psubusb %mm5, %mm4                   
	por %mm4, %mm3                       
	pavgb %mm3, %mm0 
	movq %mm2, %mm1                      
	psubusb %mm5, %mm2                   
	movq %mm2, %mm4                      
	pcmpeqb %mm7, %mm2                   
	psubusb %mm1, %mm5                   
	por %mm5, %mm4                       
	psubusb %mm0, %mm4                   
	movq %mm4, %mm3                      
	movq 1760(%rsp), %mm0                         	# c.pQPb
	paddusb %mm0, %mm0                   
	psubusb %mm0, %mm4                   
	pcmpeqb %mm7, %mm4                   
	psubusb b01, %mm3           
	pand %mm4, %mm3                      
	pavgb %mm7, %mm3 
	movq %mm3, %mm1                      
	pavgb %mm7, %mm3 
	pavgb %mm1, %mm3 
	movq (%rdx, %rsi, 4), %mm0                	# src, tmp947
	pxor %mm2, %mm0                      
	psubusb %mm3, %mm0                   
	pxor %mm2, %mm0                      
	movq %mm0, (%rdx, %rsi, 4)                	# src, tmp947
	movq (%rcx), %mm0                
	pxor %mm2, %mm0                      
	paddusb %mm3, %mm0                   
	pxor %mm2, %mm0                      
	movq %mm0, (%rcx)                
	pavgb %mm7, %mm1 
	movq (%rax, %rsi, 2), %mm0         	# tmp947
	pxor %mm2, %mm0                      
	psubusb %mm1, %mm0                   
	pxor %mm2, %mm0                      
	movq %mm0, (%rax, %rsi, 2)         	# tmp947
	movq (%rcx, %rsi), %mm0            	# tmp947
	pxor %mm2, %mm0                      
	paddusb %mm1, %mm0                   
	pxor %mm2, %mm0                      
	movq %mm0, (%rcx, %rsi)            	# tmp947
	pavgb %mm7, %mm1 
	movq (%rax, %rsi), %mm0            	# tmp947
	pxor %mm2, %mm0                      
	psubusb %mm1, %mm0                   
	pxor %mm2, %mm0                      
	movq %mm0, (%rax, %rsi)            	# tmp947
	movq (%rcx, %rsi, 2), %mm0         	# tmp947
	pxor %mm2, %mm0                      
	paddusb %mm1, %mm0                   
	pxor %mm2, %mm0                      
	movq %mm0, (%rcx, %rsi, 2)         	# tmp947
	
# 0 "" 2
#NO_APP
	jmp	.L760	#
.L759:
	testb	$2, %r15b	#, D.15422
	je	.L761	#,
	leaq	64(%r13), %rsi	#, src
	movslq	2828(%rsp), %rax	# c.nonBQP, D.15422
#APP
# 114 "postprocess_template.c" 1
	movq 1776(%rsp,%rax,8), %mm7                         	# c.mmxDcOffset
	movq 2288(%rsp,%rax,8), %mm6                         	# c.mmxDcThreshold
	
# 0 "" 2
#NO_APP
	movl	$16, %edx	#, tmp955
#APP
# 120 "postprocess_template.c" 1
	lea (%rsi, %rdx), %rax                	# src, tmp955
	movq (%rsi), %mm0                       	# src
	movq (%rax), %mm1                
	movq %mm0, %mm3                      
	movq %mm0, %mm4                      
	pmaxub %mm1, %mm4 
	pminub %mm1, %mm3 
	psubb %mm1, %mm0                     
	paddb %mm7, %mm0                     
	pcmpgtb %mm6, %mm0                   
	movq (%rax,%rdx), %mm2             	# tmp955
	pmaxub %mm2, %mm4 
	pminub %mm2, %mm3 
	psubb %mm2, %mm1                     
	paddb %mm7, %mm1                     
	pcmpgtb %mm6, %mm1                   
	paddb %mm1, %mm0                     
	movq (%rax, %rdx, 2), %mm1         	# tmp955
	pmaxub %mm1, %mm4 
	pminub %mm1, %mm3 
	psubb %mm1, %mm2                     
	paddb %mm7, %mm2                     
	pcmpgtb %mm6, %mm2                   
	paddb %mm2, %mm0                     
	lea (%rax, %rdx, 4), %rax      	# tmp955
	movq (%rsi, %rdx, 4), %mm2                	# src, tmp955
	pmaxub %mm2, %mm4 
	pminub %mm2, %mm3 
	psubb %mm2, %mm1                     
	paddb %mm7, %mm1                     
	pcmpgtb %mm6, %mm1                   
	paddb %mm1, %mm0                     
	movq (%rax), %mm1                
	pmaxub %mm1, %mm4 
	pminub %mm1, %mm3 
	psubb %mm1, %mm2                     
	paddb %mm7, %mm2                     
	pcmpgtb %mm6, %mm2                   
	paddb %mm2, %mm0                     
	movq (%rax, %rdx), %mm2            	# tmp955
	pmaxub %mm2, %mm4 
	pminub %mm2, %mm3 
	psubb %mm2, %mm1                     
	paddb %mm7, %mm1                     
	pcmpgtb %mm6, %mm1                   
	paddb %mm1, %mm0                     
	movq (%rax, %rdx, 2), %mm1         	# tmp955
	pmaxub %mm1, %mm4 
	pminub %mm1, %mm3 
	psubb %mm1, %mm2                     
	paddb %mm7, %mm2                     
	pcmpgtb %mm6, %mm2                   
	paddb %mm2, %mm0                     
	psubusb %mm3, %mm4                   
	                                       
	pxor %mm7, %mm7                      
	psadbw %mm7, %mm0                    
	movq 1760(%rsp), %mm7                         	# c.pQPb
	paddusb %mm7, %mm7                   
	psubusb %mm7, %mm4                   
	packssdw %mm4, %mm4                  
	movd %mm0, %edx                         	# numEq
	movd %mm4, %ecx                         	# dcOk
	
# 0 "" 2
#NO_APP
	negl	%edx	# D.15422
	movzbl	%dl, %eax	# D.15422, numEq
	cmpl	2896(%rsp), %eax	# c.ppMode.flatnessThreshold, numEq
	jle	.L762	#,
	testl	%ecx, %ecx	# dcOk
	jne	.L760	#,
	leaq	1648(%rsp), %rdx	#, tmp1343
	movl	$16, %esi	#,
	movq	%r13, %rdi	# tempBlock2,
	call	doVertLowPass_SSE2	#
	jmp	.L760	#
.L762:
	movl	$16, %edx	#, tmp958
#APP
# 552 "postprocess_template.c" 1
	lea (%rsi, %rdx), %rax                	# src, tmp958
	pcmpeqb %mm6, %mm6                   
	movq (%rax, %rdx, 2), %mm1         	# tmp958
	movq (%rsi, %rdx, 4), %mm0                	# src, tmp958
	pxor %mm6, %mm1                      
	pavgb %mm1, %mm0 
	movq (%rax, %rdx, 4), %mm2         	# tmp958
	movq (%rax, %rdx), %mm3            	# tmp958
	pxor %mm6, %mm2                      
	movq %mm2, %mm5                      
	movq b80, %mm4              
	lea (%rax, %rdx, 4), %rcx      	# tmp958
	pavgb %mm3, %mm2 
	pavgb %mm0, %mm4 
	pavgb %mm2, %mm4 
	pavgb %mm0, %mm4 
	movq (%rax), %mm2                
	pxor %mm6, %mm2                      
	pavgb %mm3, %mm2 
	pavgb (%rsi), %mm1 	# src
	movq b80, %mm3              
	pavgb %mm2, %mm3 
	pavgb %mm1, %mm3 
	pavgb %mm2, %mm3 
	pavgb (%rcx, %rdx), %mm5 	# tmp958
	movq (%rcx, %rdx, 2), %mm1         	# tmp958
	pxor %mm6, %mm1                      
	pavgb (%rsi, %rdx, 4), %mm1 	# src, tmp958
	movq b80, %mm2              
	pavgb %mm5, %mm2 
	pavgb %mm1, %mm2 
	pavgb %mm5, %mm2 
	movq b00, %mm1              
	movq b00, %mm5              
	psubb %mm2, %mm1                     
	psubb %mm3, %mm5                     
	pmaxub %mm1, %mm2 
	pmaxub %mm5, %mm3 
	pminub %mm2, %mm3 
	movq b00, %mm7              
	movq 1760(%rsp), %mm2                         	# c.pQPb
	pavgb %mm6, %mm2 
	psubb %mm6, %mm2                     
	movq %mm4, %mm1                      
	pcmpgtb %mm7, %mm1                   
	pxor %mm1, %mm4                      
	psubb %mm1, %mm4                     
	pcmpgtb %mm4, %mm2                   
	psubusb %mm3, %mm4                   
	movq %mm4, %mm3                      
	psubusb b01, %mm4           
	pavgb %mm7, %mm4 
	pavgb %mm7, %mm4 
	paddb %mm3, %mm4                     
	pand %mm2, %mm4                      
	movq b80, %mm5              
	psubb %mm0, %mm5                     
	paddsb %mm6, %mm5                    
	pcmpgtb %mm5, %mm7                   
	pxor %mm7, %mm5                      
	pminub %mm5, %mm4 
	pxor %mm1, %mm7                      
	pand %mm7, %mm4                      
	movq (%rax, %rdx, 2), %mm0         	# tmp958
	movq (%rsi, %rdx, 4), %mm2                	# src, tmp958
	pxor %mm1, %mm0                      
	pxor %mm1, %mm2                      
	paddb %mm4, %mm0                     
	psubb %mm4, %mm2                     
	pxor %mm1, %mm0                      
	pxor %mm1, %mm2                      
	movq %mm0, (%rax, %rdx, 2)         	# tmp958
	movq %mm2, (%rsi, %rdx, 4)                	# src, tmp958
	
# 0 "" 2
#NO_APP
	jmp	.L760	#
.L761:
	testl	$16384, %r15d	#, D.15422
	je	.L760	#,
	leaq	48(%r13), %rdx	#, src
	movslq	2828(%rsp), %rax	# c.nonBQP, D.15422
#APP
# 2553 "postprocess_template.c" 1
	movq 1776(%rsp,%rax,8), %mm7                         	# c.mmxDcOffset
	movq 2288(%rsp,%rax,8), %mm6                         	# c.mmxDcThreshold
	
# 0 "" 2
#NO_APP
	movl	$16, %ecx	#, tmp964
#APP
# 2559 "postprocess_template.c" 1
	lea (%rdx, %rcx), %rax                	# src, tmp964
	movq (%rdx), %mm0                       	# src
	movq (%rax), %mm1                
	movq %mm1, %mm3                      
	movq %mm1, %mm4                      
	psubb %mm1, %mm0                     
	paddb %mm7, %mm0                     
	pcmpgtb %mm6, %mm0                   
	movq (%rax,%rcx), %mm2             	# tmp964
	pmaxub %mm2, %mm4 
	pminub %mm2, %mm3 
	psubb %mm2, %mm1                     
	paddb %mm7, %mm1                     
	pcmpgtb %mm6, %mm1                   
	paddb %mm1, %mm0                     
	movq (%rax, %rcx, 2), %mm1         	# tmp964
	pmaxub %mm1, %mm4 
	pminub %mm1, %mm3 
	psubb %mm1, %mm2                     
	paddb %mm7, %mm2                     
	pcmpgtb %mm6, %mm2                   
	paddb %mm2, %mm0                     
	lea (%rax, %rcx, 4), %rax      	# tmp964
	movq (%rdx, %rcx, 4), %mm2                	# src, tmp964
	pmaxub %mm2, %mm4 
	pminub %mm2, %mm3 
	psubb %mm2, %mm1                     
	paddb %mm7, %mm1                     
	pcmpgtb %mm6, %mm1                   
	paddb %mm1, %mm0                     
	movq (%rax), %mm1                
	pmaxub %mm1, %mm4 
	pminub %mm1, %mm3 
	psubb %mm1, %mm2                     
	paddb %mm7, %mm2                     
	pcmpgtb %mm6, %mm2                   
	paddb %mm2, %mm0                     
	movq (%rax, %rcx), %mm2            	# tmp964
	pmaxub %mm2, %mm4 
	pminub %mm2, %mm3 
	psubb %mm2, %mm1                     
	paddb %mm7, %mm1                     
	pcmpgtb %mm6, %mm1                   
	paddb %mm1, %mm0                     
	movq (%rax, %rcx, 2), %mm1         	# tmp964
	pmaxub %mm1, %mm4 
	pminub %mm1, %mm3 
	psubb %mm1, %mm2                     
	paddb %mm7, %mm2                     
	pcmpgtb %mm6, %mm2                   
	paddb %mm2, %mm0                     
	movq (%rdx, %rcx, 8), %mm2                	# src, tmp964
	pmaxub %mm2, %mm4 
	pminub %mm2, %mm3 
	psubb %mm2, %mm1                     
	paddb %mm7, %mm1                     
	pcmpgtb %mm6, %mm1                   
	paddb %mm1, %mm0                     
	movq (%rax, %rcx, 4), %mm1         	# tmp964
	psubb %mm1, %mm2                     
	paddb %mm7, %mm2                     
	pcmpgtb %mm6, %mm2                   
	paddb %mm2, %mm0                     
	psubusb %mm3, %mm4                   
	pxor %mm6, %mm6                      
	movq 1760(%rsp), %mm7                         	# c.pQPb
	paddusb %mm7, %mm7                   
	psubusb %mm4, %mm7                   
	pcmpeqb %mm6, %mm7                   
	pcmpeqb %mm6, %mm7                   
	movq %mm7, 304(%rsp)                         	# dc_mask
	movq 2896(%rsp), %mm7                         	# c.ppMode.flatnessThreshold
	punpcklbw %mm7, %mm7                 
	punpcklbw %mm7, %mm7                 
	punpcklbw %mm7, %mm7                 
	psubb %mm0, %mm6                     
	pcmpgtb %mm7, %mm6                   
	movq %mm6, 312(%rsp)                         	# eq_mask
	
# 0 "" 2
#NO_APP
	movq	312(%rsp), %rsi	# eq_mask, D.15434
	movq	%rsi, %rcx	# D.15434, D.15434
	andq	304(%rsp), %rcx	# dc_mask, D.15434
	movq	%rcx, 320(%rsp)	# D.15434, both_masks
	leaq	64(%r13), %rax	#, src
	testq	%rcx, %rcx	# D.15434
	je	.L764	#,
	movl	$16, %r8d	#, tmp966
	leaq	368(%rsp), %rcx	#, tmp967
	movq	%rdx, %rax	# src, src
#APP
# 2664 "postprocess_template.c" 1
	movq 1760(%rsp), %mm0                         	# c.pQPb
	pxor %mm4, %mm4                      
	movq (%rdx), %mm6                       	# src
	movq (%rdx, %r8), %mm5                   	# src, tmp966
	movq %mm5, %mm1                      
	movq %mm6, %mm2                      
	psubusb %mm6, %mm5                   
	psubusb %mm1, %mm2                   
	por %mm5, %mm2                       
	psubusb %mm2, %mm0                   
	pcmpeqb %mm4, %mm0                   
	pxor %mm6, %mm1                      
	pand %mm0, %mm1                      
	pxor %mm1, %mm6                      
	movq (%rdx, %r8, 8), %mm5                	# src, tmp966
	add %r8, %rdx                             	# tmp966, src
	movq (%rdx, %r8, 8), %mm7                	# src, tmp966
	movq %mm5, %mm1                      
	movq %mm7, %mm2                      
	psubusb %mm7, %mm5                   
	psubusb %mm1, %mm2                   
	por %mm5, %mm2                       
	movq 1760(%rsp), %mm0                         	# c.pQPb
	psubusb %mm2, %mm0                   
	pcmpeqb %mm4, %mm0                   
	pxor %mm7, %mm1                      
	pand %mm0, %mm1                      
	pxor %mm1, %mm7                      
	movq %mm6, %mm5                      
	punpckhbw %mm4, %mm6                 
	punpcklbw %mm4, %mm5                 
	movq %mm5, %mm0                      
	movq %mm6, %mm1                      
	psllw $2, %mm0                        
	psllw $2, %mm1                        
	paddw w04, %mm0             
	paddw w04, %mm1             
	movq (%rdx), %mm2                       	# src
	movq (%rdx), %mm3                       	# src
	add %r8, %rdx                             	# tmp966, src
	punpcklbw %mm4, %mm2                 
	punpckhbw %mm4, %mm3                 
	paddw %mm2, %mm0                     
	paddw %mm3, %mm1                     
	movq (%rdx), %mm2                       	# src
	movq (%rdx), %mm3                       	# src
	add %r8, %rdx                             	# tmp966, src
	punpcklbw %mm4, %mm2                 
	punpckhbw %mm4, %mm3                 
	paddw %mm2, %mm0                     
	paddw %mm3, %mm1                     
	movq (%rdx), %mm2                       	# src
	movq (%rdx), %mm3                       	# src
	add %r8, %rdx                             	# tmp966, src
	punpcklbw %mm4, %mm2                 
	punpckhbw %mm4, %mm3                 
	paddw %mm2, %mm0                     
	paddw %mm3, %mm1                     
	movq %mm0, (%rcx)                       	# tmp967
	movq %mm1, 8(%rcx)                      	# tmp967
	movq (%rdx), %mm2                       	# src
	movq (%rdx), %mm3                       	# src
	add %r8, %rdx                             	# tmp966, src
	punpcklbw %mm4, %mm2                 
	punpckhbw %mm4, %mm3                 
	paddw %mm2, %mm0                     
	paddw %mm3, %mm1                     
	psubw %mm5, %mm0                     
	psubw %mm6, %mm1                     
	movq %mm0, 16(%rcx)                     	# tmp967
	movq %mm1, 24(%rcx)                     	# tmp967
	movq (%rdx), %mm2                       	# src
	movq (%rdx), %mm3                       	# src
	add %r8, %rdx                             	# tmp966, src
	punpcklbw %mm4, %mm2                 
	punpckhbw %mm4, %mm3                 
	paddw %mm2, %mm0                     
	paddw %mm3, %mm1                     
	psubw %mm5, %mm0                     
	psubw %mm6, %mm1                     
	movq %mm0, 32(%rcx)                     	# tmp967
	movq %mm1, 40(%rcx)                     	# tmp967
	movq (%rdx), %mm2                       	# src
	movq (%rdx), %mm3                       	# src
	add %r8, %rdx                             	# tmp966, src
	punpcklbw %mm4, %mm2                 
	punpckhbw %mm4, %mm3                 
	paddw %mm2, %mm0                     
	paddw %mm3, %mm1                     
	psubw %mm5, %mm0                     
	psubw %mm6, %mm1                     
	movq %mm0, 48(%rcx)                     	# tmp967
	movq %mm1, 56(%rcx)                     	# tmp967
	movq (%rdx), %mm2                       	# src
	movq (%rdx), %mm3                       	# src
	add %r8, %rdx                             	# tmp966, src
	punpcklbw %mm4, %mm2                 
	punpckhbw %mm4, %mm3                 
	paddw %mm2, %mm0                     
	paddw %mm3, %mm1                     
	psubw %mm5, %mm0                     
	psubw %mm6, %mm1                     
	movq %mm0, 64(%rcx)                     	# tmp967
	movq %mm1, 72(%rcx)                     	# tmp967
	movq %mm7, %mm6                      
	punpckhbw %mm4, %mm7                 
	punpcklbw %mm4, %mm6                 
	movq (%rdx), %mm2                       	# src
	movq (%rdx), %mm3                       	# src
	add %r8, %rdx                             	# tmp966, src
	punpcklbw %mm4, %mm2                 
	punpckhbw %mm4, %mm3                 
	paddw %mm2, %mm0                     
	paddw %mm3, %mm1                     
	mov %rax, %rdx                             	# src, src
	add %r8, %rdx                             	# tmp966, src
	movq (%rdx), %mm2                       	# src
	movq (%rdx), %mm3                       	# src
	add %r8, %rdx                             	# tmp966, src
	punpcklbw %mm4, %mm2                 
	punpckhbw %mm4, %mm3                 
	psubw %mm2, %mm0                     
	psubw %mm3, %mm1                     
	movq %mm0, 80(%rcx)                     	# tmp967
	movq %mm1, 88(%rcx)                     	# tmp967
	movq (%rdx), %mm2                       	# src
	movq (%rdx), %mm3                       	# src
	add %r8, %rdx                             	# tmp966, src
	punpcklbw %mm4, %mm2                 
	punpckhbw %mm4, %mm3                 
	psubw %mm2, %mm0                     
	psubw %mm3, %mm1                     
	paddw %mm6, %mm0                     
	paddw %mm7, %mm1                     
	movq %mm0, 96(%rcx)                     	# tmp967
	movq %mm1, 104(%rcx)                    	# tmp967
	movq (%rdx), %mm2                       	# src
	movq (%rdx), %mm3                       	# src
	add %r8, %rdx                             	# tmp966, src
	punpcklbw %mm4, %mm2                 
	punpckhbw %mm4, %mm3                 
	psubw %mm2, %mm0                     
	psubw %mm3, %mm1                     
	paddw %mm6, %mm0                     
	paddw %mm7, %mm1                     
	movq %mm0, 112(%rcx)                    	# tmp967
	movq %mm1, 120(%rcx)                    	# tmp967
	movq (%rdx), %mm2                       	# src
	movq (%rdx), %mm3                       	# src
	add %r8, %rdx                             	# tmp966, src
	punpcklbw %mm4, %mm2                 
	punpckhbw %mm4, %mm3                 
	psubw %mm2, %mm0                     
	psubw %mm3, %mm1                     
	paddw %mm6, %mm0                     
	paddw %mm7, %mm1                     
	movq %mm0, 128(%rcx)                    	# tmp967
	movq %mm1, 136(%rcx)                    	# tmp967
	movq (%rdx), %mm2                       	# src
	movq (%rdx), %mm3                       	# src
	add %r8, %rdx                             	# tmp966, src
	punpcklbw %mm4, %mm2                 
	punpckhbw %mm4, %mm3                 
	psubw %mm2, %mm0                     
	psubw %mm3, %mm1                     
	paddw %mm6, %mm0                     
	paddw %mm7, %mm1                     
	movq %mm0, 144(%rcx)                    	# tmp967
	movq %mm1, 152(%rcx)                    	# tmp967
	mov %rax, %rdx                             	# src, src
	
# 0 "" 2
#NO_APP
	leaq	16(%rdx), %rax	#, src
	addq	$144, %rdx	#, D.15423
	movq	$-128, %rdi	#, offset
#APP
# 2804 "postprocess_template.c" 1
	movq 320(%rsp), %mm6                         	# both_masks
	pcmpeqb %mm5, %mm5                   
	pxor %mm6, %mm5                      
	pxor %mm7, %mm7                      
	1:                                     
	movq (%rcx), %mm0                       	# temp_sums
	movq 8(%rcx), %mm1                      	# temp_sums
	paddw 32(%rcx), %mm0                    	# temp_sums
	paddw 40(%rcx), %mm1                    	# temp_sums
	movq (%rdi, %rdx), %mm2                   	# offset, D.15423
	movq %mm2, %mm3                      
	movq %mm2, %mm4                      
	punpcklbw %mm7, %mm2                 
	punpckhbw %mm7, %mm3                 
	paddw %mm2, %mm0                     
	paddw %mm3, %mm1                     
	paddw %mm2, %mm0                     
	paddw %mm3, %mm1                     
	psrlw $4, %mm0                        
	psrlw $4, %mm1                        
	packuswb %mm1, %mm0                  
	pand %mm6, %mm0                      
	pand %mm5, %mm4                      
	por %mm4, %mm0                       
	movq %mm0, (%rdi, %rdx)                   	# offset, D.15423
	add $16, %rcx                            	# temp_sums
	add %r8, %rdi                             	# tmp966, offset
	 js 1b                                 
	
# 0 "" 2
#NO_APP
.L764:
	cmpq	$-1, %rsi	#, D.15434
	je	.L760	#,
	leaq	336(%rsp), %rsi	#, tmp976
	movq	%rax, %rdx	# src, temp_src
	movl	$16, %ecx	#, tmp975
#APP
# 2844 "postprocess_template.c" 1
	pxor %mm7, %mm7                      
	movq (%rdx), %mm0                       	# temp_src
	movq %mm0, %mm1                      
	punpcklbw %mm7, %mm0                 
	punpckhbw %mm7, %mm1                 
	movq (%rdx, %rcx), %mm2                   	# temp_src, tmp975
	lea (%rdx, %rcx, 2), %rax             	# temp_src, tmp975
	movq %mm2, %mm3                      
	punpcklbw %mm7, %mm2                 
	punpckhbw %mm7, %mm3                 
	movq (%rax), %mm4                
	movq %mm4, %mm5                      
	punpcklbw %mm7, %mm4                 
	punpckhbw %mm7, %mm5                 
	paddw %mm0, %mm0                     
	paddw %mm1, %mm1                     
	psubw %mm4, %mm2                     
	psubw %mm5, %mm3                     
	psubw %mm2, %mm0                     
	psubw %mm3, %mm1                     
	psllw $2, %mm2                        
	psllw $2, %mm3                        
	psubw %mm2, %mm0                     
	psubw %mm3, %mm1                     
	movq (%rax, %rcx), %mm2            	# tmp975
	movq %mm2, %mm3                      
	punpcklbw %mm7, %mm2                 
	punpckhbw %mm7, %mm3                 
	psubw %mm2, %mm0                     
	psubw %mm3, %mm1                     
	psubw %mm2, %mm0                     
	psubw %mm3, %mm1                     
	movq %mm0, (%rsi)                       	# tmp976
	movq %mm1, 8(%rsi)                      	# tmp976
	movq (%rax, %rcx, 2), %mm0         	# tmp975
	movq %mm0, %mm1                      
	punpcklbw %mm7, %mm0                 
	punpckhbw %mm7, %mm1                 
	psubw %mm0, %mm2                     
	psubw %mm1, %mm3                     
	movq %mm2, 16(%rsi)                     	# tmp976
	movq %mm3, 24(%rsi)                     	# tmp976
	paddw %mm4, %mm4                     
	paddw %mm5, %mm5                     
	psubw %mm2, %mm4                     
	psubw %mm3, %mm5                     
	lea (%rax, %rcx), %rdx                	# tmp975, temp_src
	psllw $2, %mm2                        
	psllw $2, %mm3                        
	psubw %mm2, %mm4                     
	psubw %mm3, %mm5                     
	movq (%rdx, %rcx, 2), %mm2                	# temp_src, tmp975
	movq %mm2, %mm3                      
	punpcklbw %mm7, %mm2                 
	punpckhbw %mm7, %mm3                 
	psubw %mm2, %mm4                     
	psubw %mm3, %mm5                     
	psubw %mm2, %mm4                     
	psubw %mm3, %mm5                     
	movq (%rax, %rcx, 4), %mm6         	# tmp975
	punpcklbw %mm7, %mm6                 
	psubw %mm6, %mm2                     
	movq (%rax, %rcx, 4), %mm6         	# tmp975
	punpckhbw %mm7, %mm6                 
	psubw %mm6, %mm3                     
	paddw %mm0, %mm0                     
	paddw %mm1, %mm1                     
	psubw %mm2, %mm0                     
	psubw %mm3, %mm1                     
	psllw $2, %mm2                        
	psllw $2, %mm3                        
	psubw %mm2, %mm0                     
	psubw %mm3, %mm1                     
	movq (%rdx, %rcx, 4), %mm2                	# temp_src, tmp975
	movq %mm2, %mm3                      
	punpcklbw %mm7, %mm2                 
	punpckhbw %mm7, %mm3                 
	paddw %mm2, %mm2                     
	paddw %mm3, %mm3                     
	psubw %mm2, %mm0                     
	psubw %mm3, %mm1                     
	movq (%rsi), %mm2                       	# tmp976
	movq 8(%rsi), %mm3                      	# tmp976
	movq %mm7, %mm6                      
	psubw %mm0, %mm6                     
	pmaxsw %mm6, %mm0                    
	movq %mm7, %mm6                      
	psubw %mm1, %mm6                     
	pmaxsw %mm6, %mm1                    
	movq %mm7, %mm6                      
	psubw %mm2, %mm6                     
	pmaxsw %mm6, %mm2                    
	movq %mm7, %mm6                      
	psubw %mm3, %mm6                     
	pmaxsw %mm6, %mm3                    
	pminsw %mm2, %mm0                    
	pminsw %mm3, %mm1                    
	movd 1760(%rsp), %mm2                         	# c.pQPb
	punpcklbw %mm7, %mm2                 
	movq %mm7, %mm6                      
	pcmpgtw %mm4, %mm6                   
	pxor %mm6, %mm4                      
	psubw %mm6, %mm4                     
	pcmpgtw %mm5, %mm7                   
	pxor %mm7, %mm5                      
	psubw %mm7, %mm5                     
	psllw $3, %mm2                        
	movq %mm2, %mm3                      
	pcmpgtw %mm4, %mm2                   
	pcmpgtw %mm5, %mm3                   
	pand %mm2, %mm4                      
	pand %mm3, %mm5                      
	psubusw %mm0, %mm4                   
	psubusw %mm1, %mm5                   
	movq w05, %mm2              
	pmullw %mm2, %mm4                    
	pmullw %mm2, %mm5                    
	movq w20, %mm2              
	paddw %mm2, %mm4                     
	paddw %mm2, %mm5                     
	psrlw $6, %mm4                        
	psrlw $6, %mm5                        
	movq 16(%rsi), %mm0                     	# tmp976
	movq 24(%rsi), %mm1                     	# tmp976
	pxor %mm2, %mm2                      
	pxor %mm3, %mm3                      
	pcmpgtw %mm0, %mm2                   
	pcmpgtw %mm1, %mm3                   
	pxor %mm2, %mm0                      
	pxor %mm3, %mm1                      
	psubw %mm2, %mm0                     
	psubw %mm3, %mm1                     
	psrlw $1, %mm0                        
	psrlw $1, %mm1                        
	pxor %mm6, %mm2                      
	pxor %mm7, %mm3                      
	pand %mm2, %mm4                      
	pand %mm3, %mm5                      
	pminsw %mm0, %mm4                    
	pminsw %mm1, %mm5                    
	pxor %mm6, %mm4                      
	pxor %mm7, %mm5                      
	psubw %mm6, %mm4                     
	psubw %mm7, %mm5                     
	packsswb %mm5, %mm4                  
	movq 312(%rsp), %mm1                         	# eq_mask
	pandn %mm4, %mm1                     
	movq (%rdx), %mm0                       	# temp_src
	paddb   %mm1, %mm0                   
	movq %mm0, (%rdx)                       	# temp_src
	movq (%rdx, %rcx), %mm0                   	# temp_src, tmp975
	psubb %mm1, %mm0                     
	movq %mm0, (%rdx, %rcx)                   	# temp_src, tmp975
	
# 0 "" 2
#NO_APP
.L760:
	leaq	-4(%rbx), %rcx	#, D.15423
	leaq	64(%r13), %rsi	#, D.15425
#APP
# 2080 "postprocess_template.c" 1
	lea (%rcx, %rbp), %rax                	# D.15423, D.15434
	lea (%rax,%rbp,4), %rdx        	# D.15434
	movq (%rsi), %mm0                       	# D.15425
	movq 16(%rsi), %mm1                     	# D.15425
	movq %mm0, %mm2                      
	punpcklbw %mm1, %mm0                 
	punpckhbw %mm1, %mm2                 
	movq 32(%rsi), %mm1                     	# D.15425
	movq 48(%rsi), %mm3                     	# D.15425
	movq %mm1, %mm4                      
	punpcklbw %mm3, %mm1                 
	punpckhbw %mm3, %mm4                 
	movq %mm0, %mm3                      
	punpcklwd %mm1, %mm0                 
	punpckhwd %mm1, %mm3                 
	movq %mm2, %mm1                      
	punpcklwd %mm4, %mm2                 
	punpckhwd %mm4, %mm1                 
	movd %mm0, (%rcx)                       	# D.15423
	psrlq $32, %mm0                       
	movd %mm0, (%rax)                
	movd %mm3, (%rax, %rbp)            	# D.15434
	psrlq $32, %mm3                       
	movd %mm3, (%rax, %rbp, 2)         	# D.15434
	movd %mm2, (%rcx, %rbp, 4)                	# D.15423, D.15434
	psrlq $32, %mm2                       
	movd %mm2, (%rdx)                
	movd %mm1, (%rdx, %rbp)            	# D.15434
	psrlq $32, %mm1                       
	movd %mm1, (%rdx, %rbp, 2)         	# D.15434
	movq 64(%rsi), %mm0                     	# D.15425
	movq 80(%rsi), %mm1                     	# D.15425
	movq %mm0, %mm2                      
	punpcklbw %mm1, %mm0                 
	punpckhbw %mm1, %mm2                 
	movq 96(%rsi), %mm1                     	# D.15425
	movq 112(%rsi), %mm3                    	# D.15425
	movq %mm1, %mm4                      
	punpcklbw %mm3, %mm1                 
	punpckhbw %mm3, %mm4                 
	movq %mm0, %mm3                      
	punpcklwd %mm1, %mm0                 
	punpckhwd %mm1, %mm3                 
	movq %mm2, %mm1                      
	punpcklwd %mm4, %mm2                 
	punpckhwd %mm4, %mm1                 
	movd %mm0, 4(%rcx)                      	# D.15423
	psrlq $32, %mm0                       
	movd %mm0, 4(%rax)               
	movd %mm3, 4(%rax, %rbp)           	# D.15434
	psrlq $32, %mm3                       
	movd %mm3, 4(%rax, %rbp, 2)        	# D.15434
	movd %mm2, 4(%rcx, %rbp, 4)               	# D.15423, D.15434
	psrlq $32, %mm2                       
	movd %mm2, 4(%rdx)               
	movd %mm1, 4(%rdx, %rbp)           	# D.15434
	psrlq $32, %mm1                       
	movd %mm1, 4(%rdx, %rbp, 2)        	# D.15434
	
# 0 "" 2
#NO_APP
	cmpl	$0, 16(%rsp)	#, %sfp
	jle	.L766	#,
	cmpb	$0, 59(%rsp)	#, %sfp
	je	.L766	#,
	movq	%rbx, %rax	# dstBlock, D.15426
	subq	88(%rsp), %rax	# %sfp, D.15426
	leaq	-8(%rax), %rdi	#, D.15423
	leaq	1648(%rsp), %rdx	#, tmp1350
	movl	12(%rsp), %esi	# %sfp,
	call	dering_SSE2	#
.L766:
	testl	$1048576, %r15d	#, D.15422
	je	.L758	#,
	movslq	2992(%rsp), %rdx	# isColor, isColor
	movl	%r12d, %eax	# x, D.15422
	sarl	$3, %eax	#, D.15422
	cltq
	addq	144(%rsp), %rax	# %sfp, D.15426
	movq	1712(%rsp,%rdx,8), %rcx	# c.tempBlurredPast, tmp994
	leaq	(%rcx,%rax,4), %rax	#, D.15442
	movslq	%r12d, %rsi	# x, D.15426
	addq	136(%rsp), %rsi	# %sfp, D.15426
	addq	1688(%rsp,%rdx,8), %rsi	# c.tempBlurred, D.15423
	movq	%rax, 328(%rsp)	# D.15442, tempBlurredPast
	movl	2880(%rsp), %edx	# MEM[(const int *)&c + 1232B], MEM[(const int *)&c + 1232B]
	movl	%edx, 508(%rax)	# MEM[(const int *)&c + 1232B], MEM[(uint32_t *)_409 + 508B]
	movl	2884(%rsp), %edx	# MEM[(const int *)&c + 1236B], MEM[(const int *)&c + 1236B]
	movl	%edx, 512(%rax)	# MEM[(const int *)&c + 1236B], MEM[(uint32_t *)_409 + 512B]
	movl	2888(%rsp), %edx	# MEM[(const int *)&c + 1240B], MEM[(const int *)&c + 1240B]
	movl	%edx, 516(%rax)	# MEM[(const int *)&c + 1240B], MEM[(uint32_t *)_409 + 516B]
	leaq	-8(%rbx), %rdi	#, D.15423
#APP
# 2169 "postprocess_template.c" 1
	lea (%rbp, %rbp, 2), %rax             	# D.15434
	lea (%rbp, %rbp, 4), %rdx             	# D.15434
	lea (%rdx, %rbp, 2), %rcx      	# D.15434
	pcmpeqb %mm7, %mm7                   
	movq b80, %mm6              
	pxor %mm0, %mm0                      
	movq (%rdi), %mm5                     	# D.15423
	movq (%rsi), %mm2                     	# D.15423
	pxor %mm7, %mm2                      
	pavgb %mm2, %mm5 
	paddb %mm6, %mm5                     
	movq %mm5, %mm2                      
	psllw $8, %mm5                        
	pmaddwd %mm5, %mm5                   
	pmaddwd %mm2, %mm2                   
	paddd %mm2, %mm5                     
	psrld $14, %mm5                       
	paddd %mm5, %mm0                     
	movq (%rdi, %rbp), %mm5                     	# D.15423, D.15434
	movq (%rsi, %rbp), %mm2                     	# D.15423, D.15434
	pxor %mm7, %mm2                      
	pavgb %mm2, %mm5 
	paddb %mm6, %mm5                     
	movq %mm5, %mm2                      
	psllw $8, %mm5                        
	pmaddwd %mm5, %mm5                   
	pmaddwd %mm2, %mm2                   
	paddd %mm2, %mm5                     
	psrld $14, %mm5                       
	paddd %mm5, %mm0                     
	movq (%rdi, %rbp, 2), %mm5                     	# D.15423, D.15434
	movq (%rsi, %rbp, 2), %mm2                     	# D.15423, D.15434
	pxor %mm7, %mm2                      
	pavgb %mm2, %mm5 
	paddb %mm6, %mm5                     
	movq %mm5, %mm2                      
	psllw $8, %mm5                        
	pmaddwd %mm5, %mm5                   
	pmaddwd %mm2, %mm2                   
	paddd %mm2, %mm5                     
	psrld $14, %mm5                       
	paddd %mm5, %mm0                     
	movq (%rdi, %rax), %mm5                     	# D.15423
	movq (%rsi, %rax), %mm2                     	# D.15423
	pxor %mm7, %mm2                      
	pavgb %mm2, %mm5 
	paddb %mm6, %mm5                     
	movq %mm5, %mm2                      
	psllw $8, %mm5                        
	pmaddwd %mm5, %mm5                   
	pmaddwd %mm2, %mm2                   
	paddd %mm2, %mm5                     
	psrld $14, %mm5                       
	paddd %mm5, %mm0                     
	movq (%rdi, %rbp, 4), %mm5                     	# D.15423, D.15434
	movq (%rsi, %rbp, 4), %mm2                     	# D.15423, D.15434
	pxor %mm7, %mm2                      
	pavgb %mm2, %mm5 
	paddb %mm6, %mm5                     
	movq %mm5, %mm2                      
	psllw $8, %mm5                        
	pmaddwd %mm5, %mm5                   
	pmaddwd %mm2, %mm2                   
	paddd %mm2, %mm5                     
	psrld $14, %mm5                       
	paddd %mm5, %mm0                     
	movq (%rdi, %rdx), %mm5                     	# D.15423
	movq (%rsi, %rdx), %mm2                     	# D.15423
	pxor %mm7, %mm2                      
	pavgb %mm2, %mm5 
	paddb %mm6, %mm5                     
	movq %mm5, %mm2                      
	psllw $8, %mm5                        
	pmaddwd %mm5, %mm5                   
	pmaddwd %mm2, %mm2                   
	paddd %mm2, %mm5                     
	psrld $14, %mm5                       
	paddd %mm5, %mm0                     
	movq (%rdi, %rax,2), %mm5                     	# D.15423
	movq (%rsi, %rax,2), %mm2                     	# D.15423
	pxor %mm7, %mm2                      
	pavgb %mm2, %mm5 
	paddb %mm6, %mm5                     
	movq %mm5, %mm2                      
	psllw $8, %mm5                        
	pmaddwd %mm5, %mm5                   
	pmaddwd %mm2, %mm2                   
	paddd %mm2, %mm5                     
	psrld $14, %mm5                       
	paddd %mm5, %mm0                     
	movq (%rdi, %rcx), %mm5                     	# D.15423
	movq (%rsi, %rcx), %mm2                     	# D.15423
	pxor %mm7, %mm2                      
	pavgb %mm2, %mm5 
	paddb %mm6, %mm5                     
	movq %mm5, %mm2                      
	psllw $8, %mm5                        
	pmaddwd %mm5, %mm5                   
	pmaddwd %mm2, %mm2                   
	paddd %mm2, %mm5                     
	psrld $14, %mm5                       
	paddd %mm5, %mm0                     
	movq %mm0, %mm4                      
	psrlq $32, %mm0                       
	paddd %mm0, %mm4                     
	movd %mm4, %ecx                      
	shll $2, %ecx                         
	mov 328(%rsp), %rdx                      	# tempBlurredPast
	addl -4(%rdx), %ecx              
	addl 4(%rdx), %ecx               
	addl -1024(%rdx), %ecx           
	addl $4, %ecx                         
	addl 1024(%rdx), %ecx            
	shrl $3, %ecx                         
	movl %ecx, (%rdx)                
	cmpl 512(%rdx), %ecx             
	 jb 2f                                 
	cmpl 516(%rdx), %ecx             
	 jb 1f                                 
	lea (%rax, %rbp, 2), %rdx      	# D.15434
	lea (%rdx, %rbp, 2), %rcx      	# D.15434
	movq (%rdi), %mm0                       	# D.15423
	movq (%rdi, %rbp), %mm1                   	# D.15423, D.15434
	movq (%rdi, %rbp, 2), %mm2                	# D.15423, D.15434
	movq (%rdi, %rax), %mm3            	# D.15423
	movq (%rdi, %rbp, 4), %mm4                	# D.15423, D.15434
	movq (%rdi, %rdx), %mm5            	# D.15423
	movq (%rdi, %rax, 2), %mm6         	# D.15423
	movq (%rdi, %rcx), %mm7            	# D.15423
	movq %mm0, (%rsi)                       	# D.15423
	movq %mm1, (%rsi, %rbp)                   	# D.15423, D.15434
	movq %mm2, (%rsi, %rbp, 2)                	# D.15423, D.15434
	movq %mm3, (%rsi, %rax)            	# D.15423
	movq %mm4, (%rsi, %rbp, 4)                	# D.15423, D.15434
	movq %mm5, (%rsi, %rdx)            	# D.15423
	movq %mm6, (%rsi, %rax, 2)         	# D.15423
	movq %mm7, (%rsi, %rcx)            	# D.15423
	jmp 4f                                 
	1:                                     
	lea (%rax, %rbp, 2), %rdx      	# D.15434
	lea (%rdx, %rbp, 2), %rcx      	# D.15434
	movq (%rdi), %mm0                       	# D.15423
	pavgb (%rsi), %mm0 	# D.15423
	movq (%rdi, %rbp), %mm1                   	# D.15423, D.15434
	pavgb (%rsi, %rbp), %mm1 	# D.15423, D.15434
	movq (%rdi, %rbp, 2), %mm2                	# D.15423, D.15434
	pavgb (%rsi, %rbp, 2), %mm2 	# D.15423, D.15434
	movq (%rdi, %rax), %mm3            	# D.15423
	pavgb (%rsi, %rax), %mm3 	# D.15423
	movq (%rdi, %rbp, 4), %mm4                	# D.15423, D.15434
	pavgb (%rsi, %rbp, 4), %mm4 	# D.15423, D.15434
	movq (%rdi, %rdx), %mm5            	# D.15423
	pavgb (%rsi, %rdx), %mm5 	# D.15423
	movq (%rdi, %rax, 2), %mm6         	# D.15423
	pavgb (%rsi, %rax, 2), %mm6 	# D.15423
	movq (%rdi, %rcx), %mm7            	# D.15423
	pavgb (%rsi, %rcx), %mm7 	# D.15423
	movq %mm0, (%rsi)                       	# D.15423
	movq %mm1, (%rsi, %rbp)                   	# D.15423, D.15434
	movq %mm2, (%rsi, %rbp, 2)                	# D.15423, D.15434
	movq %mm3, (%rsi, %rax)            	# D.15423
	movq %mm4, (%rsi, %rbp, 4)                	# D.15423, D.15434
	movq %mm5, (%rsi, %rdx)            	# D.15423
	movq %mm6, (%rsi, %rax, 2)         	# D.15423
	movq %mm7, (%rsi, %rcx)            	# D.15423
	movq %mm0, (%rdi)                       	# D.15423
	movq %mm1, (%rdi, %rbp)                   	# D.15423, D.15434
	movq %mm2, (%rdi, %rbp, 2)                	# D.15423, D.15434
	movq %mm3, (%rdi, %rax)            	# D.15423
	movq %mm4, (%rdi, %rbp, 4)                	# D.15423, D.15434
	movq %mm5, (%rdi, %rdx)            	# D.15423
	movq %mm6, (%rdi, %rax, 2)         	# D.15423
	movq %mm7, (%rdi, %rcx)            	# D.15423
	jmp 4f                                 
	2:                                     
	cmpl 508(%rdx), %ecx             
	 jb 3f                                 
	lea (%rax, %rbp, 2), %rdx      	# D.15434
	lea (%rdx, %rbp, 2), %rcx      	# D.15434
	movq (%rdi), %mm0                       	# D.15423
	movq (%rdi, %rbp), %mm1                   	# D.15423, D.15434
	movq (%rdi, %rbp, 2), %mm2                	# D.15423, D.15434
	movq (%rdi, %rax), %mm3            	# D.15423
	movq (%rsi), %mm4                       	# D.15423
	movq (%rsi, %rbp), %mm5                   	# D.15423, D.15434
	movq (%rsi, %rbp, 2), %mm6                	# D.15423, D.15434
	movq (%rsi, %rax), %mm7            	# D.15423
	pavgb %mm4, %mm0 
	pavgb %mm5, %mm1 
	pavgb %mm6, %mm2 
	pavgb %mm7, %mm3 
	pavgb %mm4, %mm0 
	pavgb %mm5, %mm1 
	pavgb %mm6, %mm2 
	pavgb %mm7, %mm3 
	movq %mm0, (%rsi)                       	# D.15423
	movq %mm1, (%rsi, %rbp)                   	# D.15423, D.15434
	movq %mm2, (%rsi, %rbp, 2)                	# D.15423, D.15434
	movq %mm3, (%rsi, %rax)            	# D.15423
	movq %mm0, (%rdi)                       	# D.15423
	movq %mm1, (%rdi, %rbp)                   	# D.15423, D.15434
	movq %mm2, (%rdi, %rbp, 2)                	# D.15423, D.15434
	movq %mm3, (%rdi, %rax)            	# D.15423
	movq (%rdi, %rbp, 4), %mm0                	# D.15423, D.15434
	movq (%rdi, %rdx), %mm1            	# D.15423
	movq (%rdi, %rax, 2), %mm2         	# D.15423
	movq (%rdi, %rcx), %mm3            	# D.15423
	movq (%rsi, %rbp, 4), %mm4                	# D.15423, D.15434
	movq (%rsi, %rdx), %mm5            	# D.15423
	movq (%rsi, %rax, 2), %mm6         	# D.15423
	movq (%rsi, %rcx), %mm7            	# D.15423
	pavgb %mm4, %mm0 
	pavgb %mm5, %mm1 
	pavgb %mm6, %mm2 
	pavgb %mm7, %mm3 
	pavgb %mm4, %mm0 
	pavgb %mm5, %mm1 
	pavgb %mm6, %mm2 
	pavgb %mm7, %mm3 
	movq %mm0, (%rsi, %rbp, 4)                	# D.15423, D.15434
	movq %mm1, (%rsi, %rdx)            	# D.15423
	movq %mm2, (%rsi, %rax, 2)         	# D.15423
	movq %mm3, (%rsi, %rcx)            	# D.15423
	movq %mm0, (%rdi, %rbp, 4)                	# D.15423, D.15434
	movq %mm1, (%rdi, %rdx)            	# D.15423
	movq %mm2, (%rdi, %rax, 2)         	# D.15423
	movq %mm3, (%rdi, %rcx)            	# D.15423
	jmp 4f                                 
	3:                                     
	lea (%rax, %rbp, 2), %rdx      	# D.15434
	lea (%rdx, %rbp, 2), %rcx      	# D.15434
	movq (%rdi), %mm0                       	# D.15423
	movq (%rdi, %rbp), %mm1                   	# D.15423, D.15434
	movq (%rdi, %rbp, 2), %mm2                	# D.15423, D.15434
	movq (%rdi, %rax), %mm3            	# D.15423
	movq (%rsi), %mm4                       	# D.15423
	movq (%rsi, %rbp), %mm5                   	# D.15423, D.15434
	movq (%rsi, %rbp, 2), %mm6                	# D.15423, D.15434
	movq (%rsi, %rax), %mm7            	# D.15423
	pavgb %mm4, %mm0 
	pavgb %mm5, %mm1 
	pavgb %mm6, %mm2 
	pavgb %mm7, %mm3 
	pavgb %mm4, %mm0 
	pavgb %mm5, %mm1 
	pavgb %mm6, %mm2 
	pavgb %mm7, %mm3 
	pavgb %mm4, %mm0 
	pavgb %mm5, %mm1 
	pavgb %mm6, %mm2 
	pavgb %mm7, %mm3 
	movq %mm0, (%rsi)                       	# D.15423
	movq %mm1, (%rsi, %rbp)                   	# D.15423, D.15434
	movq %mm2, (%rsi, %rbp, 2)                	# D.15423, D.15434
	movq %mm3, (%rsi, %rax)            	# D.15423
	movq %mm0, (%rdi)                       	# D.15423
	movq %mm1, (%rdi, %rbp)                   	# D.15423, D.15434
	movq %mm2, (%rdi, %rbp, 2)                	# D.15423, D.15434
	movq %mm3, (%rdi, %rax)            	# D.15423
	movq (%rdi, %rbp, 4), %mm0                	# D.15423, D.15434
	movq (%rdi, %rdx), %mm1            	# D.15423
	movq (%rdi, %rax, 2), %mm2         	# D.15423
	movq (%rdi, %rcx), %mm3            	# D.15423
	movq (%rsi, %rbp, 4), %mm4                	# D.15423, D.15434
	movq (%rsi, %rdx), %mm5            	# D.15423
	movq (%rsi, %rax, 2), %mm6         	# D.15423
	movq (%rsi, %rcx), %mm7            	# D.15423
	pavgb %mm4, %mm0 
	pavgb %mm5, %mm1 
	pavgb %mm6, %mm2 
	pavgb %mm7, %mm3 
	pavgb %mm4, %mm0 
	pavgb %mm5, %mm1 
	pavgb %mm6, %mm2 
	pavgb %mm7, %mm3 
	pavgb %mm4, %mm0 
	pavgb %mm5, %mm1 
	pavgb %mm6, %mm2 
	pavgb %mm7, %mm3 
	movq %mm0, (%rsi, %rbp, 4)                	# D.15423, D.15434
	movq %mm1, (%rsi, %rdx)            	# D.15423
	movq %mm2, (%rsi, %rax, 2)         	# D.15423
	movq %mm3, (%rsi, %rcx)            	# D.15423
	movq %mm0, (%rdi, %rbp, 4)                	# D.15423, D.15434
	movq %mm1, (%rdi, %rdx)            	# D.15423
	movq %mm2, (%rdi, %rax, 2)         	# D.15423
	movq %mm3, (%rdi, %rcx)            	# D.15423
	4:                                     
	
# 0 "" 2
#NO_APP
.L758:
	addq	$8, %rbx	#, dstBlock
	addq	$8, %r14	#, srcBlock
	addl	$8, %r12d	#, x
	movq	(%rsp), %rax	# %sfp, tempBlock2
	movq	%r13, (%rsp)	# tempBlock2, %sfp
	cmpl	%r12d, 28(%rsp)	# x, %sfp
	jg	.L783	#,
.L736:
	cmpl	$0, 16(%rsp)	#, %sfp
	jle	.L768	#,
	testb	$4, %r15b	#, D.15422
	je	.L768	#,
	movq	%rbx, %rax	# dstBlock, D.15426
	subq	88(%rsp), %rax	# %sfp, D.15426
	leaq	-8(%rax), %rdi	#, D.15423
	leaq	1648(%rsp), %rdx	#, tmp1358
	movl	12(%rsp), %esi	# %sfp,
	call	dering_SSE2	#
.L768:
	testl	$1048576, %r15d	#, D.15422
	je	.L769	#,
	movslq	2992(%rsp), %rsi	# isColor, isColor
	movl	16(%rsp), %eax	# %sfp, D.15422
	sarl	$3, %eax	#, D.15422
	sall	$8, %eax	#, D.15422
	cltq
	movl	%r12d, %edx	# x, D.15422
	sarl	$3, %edx	#, D.15422
	movslq	%edx, %rdx	# D.15422, D.15426
	leaq	256(%rax,%rdx), %rdx	#, D.15426
	movq	1712(%rsp,%rsi,8), %rax	# c.tempBlurredPast, tmp1026
	leaq	(%rax,%rdx,4), %rcx	#, D.15442
	movslq	%r12d, %r12	# x, D.15426
	addq	136(%rsp), %r12	# %sfp, D.15426
	movq	%r12, %rdx	# D.15426, D.15423
	addq	1688(%rsp,%rsi,8), %rdx	# c.tempBlurred, D.15423
	leaq	-8(%rbx), %rdi	#, D.15423
	leaq	2880(%rsp), %r8	#,
	movl	12(%rsp), %esi	# %sfp,
	call	tempNoiseReducer_SSE2	#
.L769:
	movl	172(%rsp), %ebx	# %sfp, D.15422
	cmpl	%ebx, 24(%rsp)	# D.15422, %sfp
	jg	.L770	#,
	movl	28(%rsp), %ebx	# %sfp, width
	movl	264(%rsp), %ecx	# %sfp, D.15422
	cmpl	%ecx, %ebx	# D.15422, width
	je	.L771	#,
	movl	200(%rsp), %ecx	# %sfp, ivtmp.1880
	movl	%ecx, %r14d	# ivtmp.1880, D.15422
	movl	12(%rsp), %r13d	# %sfp, D.15421
	movl	$0, %eax	#, ivtmp.1831
	movl	$0, %ebp	#, i
	movslq	%ebx, %r12	# width, D.15427
	testl	%ecx, %ecx	# ivtmp.1880
	jg	.L785	#,
	jmp	.L770	#
.L771:
	movl	12(%rsp), %ecx	# %sfp,
	movl	200(%rsp), %edx	# %sfp,
	movq	256(%rsp), %rsi	# %sfp,
	movq	232(%rsp), %rdi	# %sfp,
	call	linecpy	#
	jmp	.L770	#
.L785:
	addl	$1, %ebp	#, i
	leal	0(%r13,%rax), %ebx	#, D.15421
	cltq
	movq	232(%rsp), %rcx	# %sfp, dstBlock
	leaq	(%rcx,%rax), %rdi	#, D.15423
	movslq	%ebx, %rsi	# D.15421, D.15426
	addq	224(%rsp), %rsi	# %sfp, D.15423
	movq	%r12, %rdx	# D.15427,
	call	memcpy	#
	movl	%ebx, %eax	# D.15421, ivtmp.1831
	cmpl	%r14d, %ebp	# D.15422, i
	jne	.L785	#,
.L770:
	addl	$8, 16(%rsp)	#, %sfp
	movl	16(%rsp), %eax	# %sfp, y
	movl	240(%rsp), %edi	# %sfp, D.15421
	addl	%edi, 156(%rsp)	# D.15421, %sfp
	movl	212(%rsp), %esi	# %sfp, D.15421
	addl	%esi, 168(%rsp)	# D.15421, %sfp
	subl	$8, 200(%rsp)	#, %sfp
	subl	$8, 204(%rsp)	#, %sfp
	movl	220(%rsp), %ecx	# %sfp, D.15421
	addl	%ecx, 208(%rsp)	# D.15421, %sfp
	cmpl	%eax, 24(%rsp)	# y, %sfp
	jg	.L774	#,
.L732:
#APP
# 3692 "postprocess_template.c" 1
	emms
# 0 "" 2
#NO_APP
	leaq	1648(%rsp), %rsi	#, tmp1045
	movl	$157, %ecx	#, tmp1046
	movq	3000(%rsp), %rdi	# c2, c2
	rep movsq
	jmp	.L796	#
.L702:
	movl	2892(%rsp), %r9d	# c.ppMode.baseDcDiff, D.15421
	leaq	1648(%rsp), %rax	#, tmp1383
	leaq	1776(%rsp), %rdx	#, ivtmp.1935
	leaq	584(%rax), %r8	#, D.15424
	movl	$0, %ecx	#, ivtmp.1934
	movl	$126, %edi	#, tmp1069
	movabsq	$72340172838076673, %rsi	#, tmp1070
	jmp	.L703	#
.L796:
	addq	$2920, %rsp	#,
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
.LFE175:
	.size	postProcess_SSE2, .-postProcess_SSE2
	.type	postProcess_MMX2, @function
postProcess_MMX2:
.LFB139:
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
	subq	$2920, %rsp	#,
	.cfi_def_cfa_offset 2976
	movq	%rdi, 192(%rsp)	# src, %sfp
	movl	%esi, 60(%rsp)	# srcStride, %sfp
	movq	%rdx, 160(%rsp)	# dst, %sfp
	movl	%ecx, 12(%rsp)	# dstStride, %sfp
	movl	%r8d, 28(%rsp)	# width, %sfp
	movl	%r9d, 24(%rsp)	# height, %sfp
	leaq	1648(%rsp), %rdi	#, tmp678
	movl	$157, %ecx	#, tmp680
	movq	3000(%rsp), %rsi	# c2, c2
	rep movsq
	cmpl	$0, 2992(%rsp)	#, isColor
	je	.L801	#,
	movl	2860(%rsp), %r15d	# c.ppMode.chromMode, D.15658
	movl	$4, %eax	#, tmp681
	movl	%eax, %ebx	# tmp681, D.15658
	subl	2848(%rsp), %ebx	# c.hChromaSubSample, D.15658
	movl	%ebx, 116(%rsp)	# D.15658, %sfp
	subl	2852(%rsp), %eax	# c.vChromaSubSample, D.15658
	movl	%eax, 216(%rsp)	# D.15658, %sfp
	jmp	.L802	#
.L801:
	movl	2856(%rsp), %r15d	# c.ppMode.lumMode, D.15658
	movl	$4, 116(%rsp)	#, %sfp
	movl	$4, 216(%rsp)	#, %sfp
.L802:
	movq	1664(%rsp), %rax	# c.yHistogram, yHistogram
	movq	%rax, 104(%rsp)	# yHistogram, %sfp
	cmpl	$0, 60(%rsp)	#, %sfp
	jle	.L803	#,
	movq	1744(%rsp), %rax	# c.tempSrc, srcBlock
	movq	%rax, 248(%rsp)	# srcBlock, %sfp
	jmp	.L804	#
.L803:
	imull	$23, 60(%rsp), %eax	#, %sfp, D.15658
	cltq
	movq	1744(%rsp), %rbx	# c.tempSrc, srcBlock
	subq	%rax, %rbx	# D.15662, srcBlock
	movq	%rbx, 248(%rsp)	# srcBlock, %sfp
.L804:
	cmpl	$0, 12(%rsp)	#, %sfp
	jle	.L805	#,
	movq	1736(%rsp), %rax	# c.tempDst, tmp1160
	addq	$32, %rax	#, D.15659
	movq	%rax, 224(%rsp)	# D.15659, %sfp
	jmp	.L806	#
.L805:
	imull	$23, 12(%rsp), %edx	#, %sfp, D.15658
	movslq	%edx, %rdx	# D.15658, D.15662
	movl	$32, %eax	#, tmp690
	subq	%rdx, %rax	# D.15662, D.15662
	addq	1736(%rsp), %rax	# c.tempDst, D.15659
	movq	%rax, 224(%rsp)	# D.15659, %sfp
.L806:
	testl	$33554432, %r15d	#, D.15658
	je	.L807	#,
	movl	$.LC1, %edx	#,
	movl	$24, %esi	#,
	movq	3000(%rsp), %rdi	# c2,
	movl	$0, %eax	#,
	call	av_log	#
	jmp	.L807	#
.L808:
	movl	%ecx, %r10d	# ivtmp.2046, D.15658
	sarl	$8, %r10d	#, D.15658
	movl	%edi, %eax	# tmp1084, D.15658
	subl	%r10d, %eax	# D.15658, D.15658
	cltq
	imulq	%rsi, %rax	# tmp1085, D.15664
	movq	%rax, (%rdx)	# D.15664, MEM[base: _119, offset: 0B]
	leal	2(%r10,%r10), %eax	#, D.15658
	movl	%edi, %ebx	# tmp1084, D.15658
	subl	%eax, %ebx	# D.15658, D.15658
	movl	%ebx, %eax	# D.15658, D.15658
	cltq
	imulq	%rsi, %rax	# tmp1085, D.15664
	movq	%rax, 512(%rdx)	# D.15664, MEM[base: _119, offset: 512B]
	addl	%r9d, %ecx	# D.15657, ivtmp.2046
	addq	$8, %rdx	#, ivtmp.2047
	cmpq	%r8, %rdx	# D.15660, ivtmp.2047
	jne	.L808	#,
	movl	$16, 244(%rsp)	#, %sfp
	movl	%r15d, %eax	# D.15658, D.15658
	andl	$262144, %eax	#, D.15658
	movl	%eax, 152(%rsp)	# D.15658, %sfp
	jne	.L809	#,
	movl	$14, 244(%rsp)	#, %sfp
	testl	$12713984, %r15d	#, D.15658
	jne	.L809	#,
	movl	$13, 244(%rsp)	#, %sfp
	testl	$590849, %r15d	#, D.15658
	jne	.L809	#,
	movl	$11, 244(%rsp)	#, %sfp
	testl	$512, %r15d	#, D.15658
	jne	.L809	#,
	movl	%r15d, %eax	# D.15658, D.15658
	andl	$4, %eax	#, D.15658
	cmpl	$1, %eax	#, D.15658
	sbbl	%eax, %eax	# copyAhead
	addl	$9, %eax	#, copyAhead
	movl	%eax, 244(%rsp)	# copyAhead, %sfp
.L809:
	movl	244(%rsp), %eax	# %sfp, copyAhead
	subl	$8, %eax	#, copyAhead
	movl	%eax, 48(%rsp)	# copyAhead, %sfp
	cmpl	$0, 2992(%rsp)	#, isColor
	jne	.L810	#,
	movl	2832(%rsp), %eax	# c.frameNum, tmp1172
	addl	$1, %eax	#, D.15658
	movl	%eax, 2832(%rsp)	# D.15658, c.frameNum
	cmpl	$1, %eax	#, D.15658
	jne	.L811	#,
	movslq	28(%rsp), %rdx	# %sfp, D.15663
	movslq	24(%rsp), %rax	# %sfp, D.15663
	imulq	%rdx, %rax	# D.15663, D.15663
	shrq	$6, %rax	#, D.15663
	movq	%rax, %rdx	# D.15663, tmp715
	salq	$4, %rdx	#, tmp715
	subq	%rax, %rdx	# D.15663, D.15663
	movq	%rdx, %rax	# D.15663, D.15663
	shrq	$8, %rax	#, tmp717
	movq	104(%rsp), %rbx	# %sfp, yHistogram
	movq	%rax, (%rbx)	# tmp717, *yHistogram_62
.L811:
	movq	104(%rsp), %rax	# %sfp, yHistogram
	movq	%rax, %rsi	# yHistogram, ivtmp.2022
	leaq	2048(%rax), %rcx	#, D.15660
	movl	$0, %edx	#, clipped
.L812:
	addq	(%rax), %rdx	# MEM[base: _695, offset: 0B], clipped
	addq	$8, %rax	#, ivtmp.2038
	cmpq	%rcx, %rax	# D.15660, ivtmp.2038
	jne	.L812	#,
	movq	%rdx, %rcx	# clipped, clipped
	testq	%rdx, %rdx	# clipped
	js	.L813	#,
	pxor	%xmm0, %xmm0	# D.15667
	cvtsi2ssq	%rdx, %xmm0	# clipped, D.15667
	jmp	.L814	#
.L813:
	movq	%rdx, %rax	# clipped, tmp723
	shrq	%rax	# tmp723
	movq	%rdx, %rdi	# clipped, tmp724
	andl	$1, %edi	#, tmp724
	orq	%rdi, %rax	# tmp724, tmp723
	pxor	%xmm0, %xmm0	# tmp722
	cvtsi2ssq	%rax, %xmm0	# tmp723, tmp722
	addss	%xmm0, %xmm0	# tmp722, D.15667
.L814:
	mulss	2876(%rsp), %xmm0	# c.ppMode.maxClippedThreshold, D.15667
	ucomiss	.LC2(%rip), %xmm0	#, D.15667
	jnb	.L815	#,
	cvttss2siq	%xmm0, %r8	# D.15667, maxClipped
	jmp	.L816	#
.L815:
	subss	.LC2(%rip), %xmm0	#, tmp726
	cvttss2siq	%xmm0, %r8	# tmp726, maxClipped
	movabsq	$-9223372036854775808, %rax	#, tmp728
	xorq	%rax, %r8	# tmp728, maxClipped
.L816:
	cmpq	%rcx, %r8	# clipped, maxClipped
	ja	.L886	#,
	movq	104(%rsp), %rax	# %sfp, yHistogram
	leaq	2040(%rax), %rdi	#, ivtmp.2029
	movq	%rdx, %rcx	# clipped, clipped
	movl	$255, %eax	#, black
.L818:
	subq	(%rdi), %rcx	# MEM[base: _694, offset: 0B], clipped
	subl	$1, %eax	#, black
	subq	$8, %rdi	#, ivtmp.2029
	cmpq	%rcx, %r8	# clipped, maxClipped
	ja	.L890	#,
	testl	%eax, %eax	# black
	jg	.L818	#,
.L890:
	movl	$0, %ecx	#, white
.L820:
	subq	(%rsi), %rdx	# MEM[base: _692, offset: 0B], clipped
	addl	$1, %ecx	#, white
	addq	$8, %rsi	#, ivtmp.2022
	cmpq	%rdx, %r8	# clipped, maxClipped
	ja	.L817	#,
	cmpl	$255, %ecx	#, white
	jle	.L820	#,
	jmp	.L817	#
.L886:
	movl	$255, %eax	#, black
	movl	$0, %ecx	#, white
.L817:
	movl	2868(%rsp), %esi	# c.ppMode.minAllowedY, D.15658
	movl	2872(%rsp), %edx	# c.ppMode.maxAllowedY, D.15658
	subl	%esi, %edx	# D.15658, D.15658
	pxor	%xmm1, %xmm1	# D.15668
	cvtsi2sd	%edx, %xmm1	# D.15658, D.15668
	subl	%eax, %ecx	# black, D.15658
	pxor	%xmm0, %xmm0	# D.15668
	cvtsi2sd	%ecx, %xmm0	# D.15658, D.15668
	divsd	%xmm0, %xmm1	# D.15668, scale
	movapd	%xmm1, %xmm0	# scale, scale
	mulsd	.LC5(%rip), %xmm0	#, D.15668
	movapd	%xmm0, %xmm1	# D.15668, D.15668
	addsd	.LC4(%rip), %xmm1	#, D.15668
	cvttsd2si	%xmm1, %edx	# D.15668, D.15669
	movzwl	%dx, %edx	# D.15669, D.15663
	cltq
	imulq	%rdx, %rax	# D.15663, D.15663
	shrq	$8, %rax	#, D.15663
	subl	%esi, %eax	# D.15658, D.15663
	movzwl	%ax, %eax	# D.15663, D.15663
	movq	%rax, %rcx	# D.15663, D.15663
	salq	$32, %rcx	#, D.15663
	orq	%rcx, %rax	# D.15663, D.15663
	movq	%rax, %rcx	# D.15663, D.15663
	salq	$16, %rcx	#, D.15663
	orq	%rcx, %rax	# D.15663, tmp755
	movq	%rax, 1672(%rsp)	# tmp755, c.packedYOffset
	movq	%rdx, %rax	# D.15663, D.15663
	salq	$32, %rax	#, D.15663
	orq	%rax, %rdx	# D.15663, D.15663
	movq	%rdx, %rax	# D.15663, D.15663
	salq	$16, %rax	#, D.15663
	orq	%rax, %rdx	# D.15663, tmp758
	movq	%rdx, 1680(%rsp)	# tmp758, c.packedYScale
	movl	$65536, 112(%rsp)	#, %sfp
	testb	$8, %r15b	#, D.15658
	je	.L822	#,
	mulsd	.LC5(%rip), %xmm0	#, D.15668
	addsd	.LC4(%rip), %xmm0	#, D.15668
	cvttsd2si	%xmm0, %eax	# D.15668, QPCorrecture
	movl	%eax, 112(%rsp)	# QPCorrecture, %sfp
	jmp	.L822	#
.L810:
	movabsq	$72058693566333184, %rax	#, tmp1190
	movq	%rax, 1680(%rsp)	# tmp1190, c.packedYScale
	movq	$0, 1672(%rsp)	#, c.packedYOffset
	movl	$65536, 112(%rsp)	#, %sfp
.L822:
	movl	60(%rsp), %ecx	# %sfp, srcStride
	movl	%ecx, %r12d	# srcStride, D.15658
	negl	%r12d	# D.15658
	sall	$3, %r12d	#, tmp767
	movslq	%r12d, %r12	# tmp767, D.15662
	addq	192(%rsp), %r12	# %sfp, srcBlock
	movl	12(%rsp), %esi	# %sfp, dstStride
	movl	%esi, %eax	# dstStride, dstStride
	movslq	%esi, %rbx	# dstStride, D.15662
	movq	%rbx, %rdi	# D.15662, D.15662
	movq	%rbx, 88(%rsp)	# D.15662, %sfp
	movq	224(%rsp), %rbx	# %sfp, dstBlock
	addq	%rdi, %rbx	# D.15662, dstBlock
	movq	%rbx, %rbp	# dstBlock, dstBlock
	movq	%rbx, 256(%rsp)	# dstBlock, %sfp
	cmpl	$0, 28(%rsp)	#, %sfp
	jle	.L823	#,
	leal	0(,%rcx,8), %r14d	#, D.15658
	movslq	%r14d, %r14	# D.15658, D.15662
	sall	$3, %eax	#, D.15658
	cltq
	movq	%rax, (%rsp)	# D.15662, %sfp
	movl	%esi, %eax	# dstStride, dstStride
	sall	$2, %eax	#, D.15658
	cltq
	movq	%rax, 32(%rsp)	# D.15662, %sfp
	movq	%r12, %rbx	# srcBlock, srcBlock
	movslq	%ecx, %r13	# srcStride, D.15670
	movl	%esi, %eax	# dstStride, D.15658
	negl	%eax	# D.15658
	cltq
	movq	%rax, 16(%rsp)	# D.15670, %sfp
.L832:
	movl	%ebx, %edi	# srcBlock, D.15658
	subl	%r12d, %edi	# srcBlock, D.15658
	movslq	12(%rsp), %rsi	# %sfp, D.15670
	movslq	%edi, %rcx	# D.15658, D.15670
	movslq	48(%rsp), %r8	# %sfp, D.15670
#APP
# 3381 "postprocess_template.c" 1
	mov %rcx, %rax              	# D.15670
	shr $2, %rax              
	and $6, %rax              
	add %r8, %rax              	# D.15670
	mov %rax, %rdx       
	imul %r13, %rax             	# D.15670
	imul %rsi, %rdx             	# D.15670
	prefetchnta 32(%rax, %rbx)  	# srcBlock
	prefetcht0 32(%rdx, %rbp)   	# dstBlock
	add %r13, %rax              	# D.15670
	add %rsi, %rdx              	# D.15670
	prefetchnta 32(%rax, %rbx)  	# srcBlock
	prefetcht0 32(%rdx, %rbp)   	# dstBlock
	
# 0 "" 2
#NO_APP
	leaq	(%rbx,%r14), %r8	#, D.15661
	movq	(%rsp), %rax	# %sfp, D.15662
	leaq	0(%rbp,%rax), %rcx	#, D.15659
	testb	$8, %r15b	#, D.15658
	je	.L824	#,
	leaq	1672(%rsp), %rax	#, tmp1210
#APP
# 3102 "postprocess_template.c" 1
	movq (%rax), %mm2        
	movq 8(%rax), %mm3       
	lea (%r8,%r13), %rax         	# D.15661, D.15670
	lea (%rcx,%rsi), %rdx         	# D.15659, D.15670
	pxor %mm4, %mm4              
	movq (%r8), %mm0          	# D.15661
	movq (%r8), %mm5          	# D.15661
	movq (%r8, %r13), %mm1          	# D.15661, D.15670
	movq (%r8, %r13), %mm6          	# D.15661, D.15670
	punpcklbw %mm0, %mm0         
	punpckhbw %mm5, %mm5         
	punpcklbw %mm1, %mm1         
	punpckhbw %mm6, %mm6         
	pmulhuw %mm3, %mm0           
	pmulhuw %mm3, %mm5           
	pmulhuw %mm3, %mm1           
	pmulhuw %mm3, %mm6           
	psubw %mm2, %mm0             
	psubw %mm2, %mm5             
	psubw %mm2, %mm1             
	psubw %mm2, %mm6             
	packuswb %mm5, %mm0          
	packuswb %mm6, %mm1          
	movq %mm0, (%rcx)          	# D.15659
	movq %mm1, (%rcx, %rsi)          	# D.15659, D.15670
	movq (%r8, %r13, 2), %mm0          	# D.15661, D.15670
	movq (%r8, %r13, 2), %mm5          	# D.15661, D.15670
	movq (%rax, %r13, 2), %mm1          	# D.15670
	movq (%rax, %r13, 2), %mm6          	# D.15670
	punpcklbw %mm0, %mm0         
	punpckhbw %mm5, %mm5         
	punpcklbw %mm1, %mm1         
	punpckhbw %mm6, %mm6         
	pmulhuw %mm3, %mm0           
	pmulhuw %mm3, %mm5           
	pmulhuw %mm3, %mm1           
	pmulhuw %mm3, %mm6           
	psubw %mm2, %mm0             
	psubw %mm2, %mm5             
	psubw %mm2, %mm1             
	psubw %mm2, %mm6             
	packuswb %mm5, %mm0          
	packuswb %mm6, %mm1          
	movq %mm0, (%rcx, %rsi, 2)          	# D.15659, D.15670
	movq %mm1, (%rdx, %rsi, 2)          	# D.15670
	movq (%r8, %r13, 4), %mm0          	# D.15661, D.15670
	movq (%r8, %r13, 4), %mm5          	# D.15661, D.15670
	movq (%rax, %r13, 4), %mm1          	# D.15670
	movq (%rax, %r13, 4), %mm6          	# D.15670
	punpcklbw %mm0, %mm0         
	punpckhbw %mm5, %mm5         
	punpcklbw %mm1, %mm1         
	punpckhbw %mm6, %mm6         
	pmulhuw %mm3, %mm0           
	pmulhuw %mm3, %mm5           
	pmulhuw %mm3, %mm1           
	pmulhuw %mm3, %mm6           
	psubw %mm2, %mm0             
	psubw %mm2, %mm5             
	psubw %mm2, %mm1             
	psubw %mm2, %mm6             
	packuswb %mm5, %mm0          
	packuswb %mm6, %mm1          
	movq %mm0, (%rcx, %rsi, 4)          	# D.15659, D.15670
	movq %mm1, (%rdx, %rsi, 4)          	# D.15670
	lea (%rax,%r13,4), %rax        	# D.15670
	lea (%rdx,%rsi,4), %rdx        	# D.15670
	movq (%rax, %r13), %mm0          	# D.15670
	movq (%rax, %r13), %mm5          	# D.15670
	movq (%rax, %r13, 2), %mm1          	# D.15670
	movq (%rax, %r13, 2), %mm6          	# D.15670
	punpcklbw %mm0, %mm0         
	punpckhbw %mm5, %mm5         
	punpcklbw %mm1, %mm1         
	punpckhbw %mm6, %mm6         
	pmulhuw %mm3, %mm0           
	pmulhuw %mm3, %mm5           
	pmulhuw %mm3, %mm1           
	pmulhuw %mm3, %mm6           
	psubw %mm2, %mm0             
	psubw %mm2, %mm5             
	psubw %mm2, %mm1             
	psubw %mm2, %mm6             
	packuswb %mm5, %mm0          
	packuswb %mm6, %mm1          
	movq %mm0, (%rdx, %rsi)          	# D.15670
	movq %mm1, (%rdx, %rsi, 2)          	# D.15670
	
# 0 "" 2
#NO_APP
	jmp	.L825	#
.L824:
#APP
# 3185 "postprocess_template.c" 1
	lea (%r8,%r13), %rax                 	# D.15661, D.15670
	lea (%rcx,%rsi), %rdx                 	# D.15659, D.15670
	movq (%r8), %mm0          	# D.15661
	movq (%r8, %r13), %mm1          	# D.15661, D.15670
	movq %mm0, (%rcx)          	# D.15659
	movq %mm1, (%rcx, %rsi)          	# D.15659, D.15670
	movq (%r8, %r13, 2), %mm0          	# D.15661, D.15670
	movq (%rax, %r13, 2), %mm1          	# D.15670
	movq %mm0, (%rcx, %rsi, 2)          	# D.15659, D.15670
	movq %mm1, (%rdx, %rsi, 2)          	# D.15670
	movq (%r8, %r13, 4), %mm0          	# D.15661, D.15670
	movq (%rax, %r13, 4), %mm1          	# D.15670
	movq %mm0, (%rcx, %rsi, 4)          	# D.15659, D.15670
	movq %mm1, (%rdx, %rsi, 4)          	# D.15670
	lea (%rax,%r13,4), %rax        	# D.15670
	lea (%rdx,%rsi,4), %rdx        	# D.15670
	movq (%rax, %r13), %mm0          	# D.15670
	movq (%rax, %r13, 2), %mm1          	# D.15670
	movq %mm0, (%rdx, %rsi)          	# D.15670
	movq %mm1, (%rdx, %rsi, 2)          	# D.15670
	
# 0 "" 2
#NO_APP
.L825:
	movq	%rcx, %rax	# D.15659, src
	movq	16(%rsp), %rcx	# %sfp, D.15670
#APP
# 3225 "postprocess_template.c" 1
	movq (%rax), %mm0               	# src
	movq %mm0, (%rax, %rcx, 4)        	# src, D.15670
	add %rcx, %rax                     	# D.15670, src
	movq %mm0, (%rax)               	# src
	movq %mm0, (%rax, %rcx)           	# src, D.15670
	movq %mm0, (%rax, %rcx, 2)        	# src, D.15670
	movq %mm0, (%rax, %rcx, 4)        	# src, D.15670
	
# 0 "" 2
#NO_APP
	testl	$65536, %r15d	#, D.15658
	je	.L826	#,
	movq	32(%rsp), %rax	# %sfp, D.15662
	leaq	0(%rbp,%rax), %rdx	#, D.15659
#APP
# 1455 "postprocess_template.c" 1
	lea (%rdx, %rsi), %rax                	# D.15659, D.15670
	lea (%rax, %rsi, 4), %rcx      	# D.15670
	movq (%rdx), %mm0                       	# D.15659
	movq (%rax, %rsi), %mm1            	# D.15670
	pavgb %mm1, %mm0 
	movq %mm0, (%rax)                
	movq (%rdx, %rsi, 4), %mm0                	# D.15659, D.15670
	pavgb %mm0, %mm1 
	movq %mm1, (%rax, %rsi, 2)         	# D.15670
	movq (%rcx, %rsi), %mm1            	# D.15670
	pavgb %mm1, %mm0 
	movq %mm0, (%rcx)                
	movq (%rdx, %rsi, 8), %mm0                	# D.15659, D.15670
	pavgb %mm0, %mm1 
	movq %mm1, (%rcx, %rsi, 2)         	# D.15670
	
# 0 "" 2
#NO_APP
	jmp	.L827	#
.L826:
	testl	$131072, %r15d	#, D.15658
	je	.L828	#,
	movq	32(%rsp), %rax	# %sfp, D.15662
	leaq	0(%rbp,%rax), %rcx	#, D.15659
	movslq	%edi, %rdi	# D.15658, D.15662
	addq	1752(%rsp), %rdi	# c.deintTemp, D.15659
#APP
# 1775 "postprocess_template.c" 1
	lea (%rcx, %rsi), %rax                	# D.15659, D.15670
	lea (%rax, %rsi, 4), %rdx      	# D.15670
	movq (%rdi), %mm0                       	# D.15659
	movq (%rax), %mm1                
	pavgb %mm1, %mm0 
	movq (%rcx), %mm2                       	# D.15659
	pavgb %mm2, %mm0 
	movq %mm0, (%rcx)                       	# D.15659
	movq (%rax, %rsi), %mm0            	# D.15670
	pavgb %mm0, %mm2 
	pavgb %mm1, %mm2 
	movq %mm2, (%rax)                
	movq (%rax, %rsi, 2), %mm2         	# D.15670
	pavgb %mm2, %mm1 
	pavgb %mm0, %mm1 
	movq %mm1, (%rax, %rsi)            	# D.15670
	movq (%rcx, %rsi, 4), %mm1                	# D.15659, D.15670
	pavgb %mm1, %mm0 
	pavgb %mm2, %mm0 
	movq %mm0, (%rax, %rsi, 2)         	# D.15670
	movq (%rdx), %mm0                
	pavgb %mm0, %mm2 
	pavgb %mm1, %mm2 
	movq %mm2, (%rcx, %rsi, 4)                	# D.15659, D.15670
	movq (%rdx, %rsi), %mm2            	# D.15670
	pavgb %mm2, %mm1 
	pavgb %mm0, %mm1 
	movq %mm1, (%rdx)                
	movq (%rdx, %rsi, 2), %mm1         	# D.15670
	pavgb %mm1, %mm0 
	pavgb %mm2, %mm0 
	movq %mm0, (%rdx, %rsi)            	# D.15670
	movq (%rcx, %rsi, 8), %mm0                	# D.15659, D.15670
	pavgb %mm0, %mm2 
	pavgb %mm1, %mm2 
	movq %mm2, (%rdx, %rsi, 2)         	# D.15670
	movq %mm1, (%rdi)                       	# D.15659
	
# 0 "" 2
#NO_APP
	jmp	.L827	#
.L828:
	testl	$524288, %r15d	#, D.15658
	je	.L829	#,
	movq	32(%rsp), %rax	# %sfp, D.15662
	leaq	0(%rbp,%rax), %rcx	#, D.15659
#APP
# 1877 "postprocess_template.c" 1
	lea (%rcx, %rsi), %rax                	# D.15659, D.15670
	lea (%rax, %rsi, 4), %rdx      	# D.15670
	movq (%rcx), %mm0                       	# D.15659
	movq (%rax, %rsi), %mm2            	# D.15670
	movq (%rax), %mm1                
	movq %mm0, %mm3                      
	pmaxub %mm1, %mm0                    
	pminub %mm3, %mm1                    
	pmaxub %mm2, %mm1                    
	pminub %mm1, %mm0                    
	movq %mm0, (%rax)                
	movq (%rcx, %rsi, 4), %mm0                	# D.15659, D.15670
	movq (%rax, %rsi, 2), %mm1         	# D.15670
	movq %mm2, %mm3                      
	pmaxub %mm1, %mm2                    
	pminub %mm3, %mm1                    
	pmaxub %mm0, %mm1                    
	pminub %mm1, %mm2                    
	movq %mm2, (%rax, %rsi, 2)         	# D.15670
	movq (%rdx), %mm2                
	movq (%rdx, %rsi), %mm1            	# D.15670
	movq %mm2, %mm3                      
	pmaxub %mm0, %mm2                    
	pminub %mm3, %mm0                    
	pmaxub %mm1, %mm0                    
	pminub %mm0, %mm2                    
	movq %mm2, (%rdx)                
	movq (%rdx, %rsi, 2), %mm2         	# D.15670
	movq (%rcx, %rsi, 8), %mm0                	# D.15659, D.15670
	movq %mm2, %mm3                      
	pmaxub %mm0, %mm2                    
	pminub %mm3, %mm0                    
	pmaxub %mm1, %mm0                    
	pminub %mm0, %mm2                    
	movq %mm2, (%rdx, %rsi, 2)         	# D.15670
	
# 0 "" 2
#NO_APP
	jmp	.L827	#
.L829:
	cmpl	$0, 152(%rsp)	#, %sfp
	je	.L830	#,
	movl	12(%rsp), %esi	# %sfp,
	movq	%rbp, %rdi	# dstBlock,
	call	deInterlaceInterpolateCubic_MMX2	#
	jmp	.L827	#
.L830:
	testl	$4194304, %r15d	#, D.15658
	je	.L831	#,
	movslq	%edi, %rdx	# D.15658, D.15662
	addq	1752(%rsp), %rdx	# c.deintTemp, D.15659
	movl	12(%rsp), %esi	# %sfp,
	movq	%rbp, %rdi	# dstBlock,
	call	deInterlaceFF_MMX2	#
	jmp	.L827	#
.L831:
	testl	$8388608, %r15d	#, D.15658
	je	.L827	#,
	movq	1752(%rsp), %rdx	# c.deintTemp, D.15659
	movslq	%edi, %rdi	# D.15658, D.15662
	movslq	28(%rsp), %rcx	# %sfp, D.15662
	addq	%rdi, %rcx	# D.15662, D.15662
	addq	%rdx, %rcx	# D.15659, D.15659
	addq	%rdi, %rdx	# D.15662, D.15659
	movl	12(%rsp), %esi	# %sfp,
	movq	%rbp, %rdi	# dstBlock,
	call	deInterlaceL5_MMX2	#
.L827:
	addq	$8, %rbp	#, dstBlock
	addq	$8, %rbx	#, srcBlock
	movl	%ebx, %eax	# srcBlock, D.15657
	subl	%r12d, %eax	# srcBlock, D.15657
	cmpl	%eax, 28(%rsp)	# D.15657, %sfp
	jg	.L832	#,
.L823:
	movl	12(%rsp), %ebx	# %sfp, dstStride
	movl	%ebx, %eax	# dstStride, tmp800
	sarl	$31, %eax	#, tmp800
	xorl	%eax, %ebx	# tmp800, D.15658
	subl	%eax, %ebx	# tmp800, D.15658
	movl	%ebx, 264(%rsp)	# D.15658, %sfp
	cmpl	28(%rsp), %ebx	# %sfp, D.15658
	je	.L833	#,
	cmpl	$0, 48(%rsp)	#, %sfp
	jg	.L834	#,
	jmp	.L835	#
.L833:
	movl	12(%rsp), %eax	# %sfp, dstStride
	leal	(%rax,%rax,8), %esi	#, D.15658
	movslq	%esi, %rsi	# D.15658, D.15662
	addq	224(%rsp), %rsi	# %sfp, D.15671
	testl	%eax, %eax	# dstStride
	jle	.L836	#,
	imull	48(%rsp), %eax	# %sfp, D.15658
	movslq	%eax, %rdx	# D.15658, D.15663
	movq	160(%rsp), %rdi	# %sfp,
	call	memcpy	#
.L835:
	cmpl	$0, 24(%rsp)	#, %sfp
	jg	.L837	#,
	jmp	.L838	#
.L836:
	movl	244(%rsp), %eax	# %sfp, copyAhead
	subl	$9, %eax	#, D.15658
	movl	12(%rsp), %ebx	# %sfp, dstStride
	imull	%ebx, %eax	# dstStride, D.15658
	cltq
	movq	160(%rsp), %rcx	# %sfp, dst
	leaq	(%rcx,%rax), %rdi	#, D.15659
	movl	48(%rsp), %edx	# %sfp, D.15658
	negl	%edx	# D.15658
	imull	%ebx, %edx	# dstStride, D.15658
	movslq	%edx, %rdx	# D.15658, D.15663
	addq	%rax, %rsi	# D.15662, D.15661
	call	memcpy	#
	jmp	.L835	#
.L834:
	movl	12(%rsp), %ebx	# %sfp, dstStride
	movl	%ebx, %eax	# dstStride, dstStride
	movl	244(%rsp), %ecx	# %sfp, copyAhead
	leal	-8(%rcx), %r14d	#, D.15658
	leal	(%rbx,%rbx,8), %r13d	#, D.15657
	movl	$0, %ebx	#, ivtmp.2004
	movl	$0, %ebp	#, i
	movslq	28(%rsp), %r12	# %sfp, D.15663
	movl	%r15d, (%rsp)	# D.15658, %sfp
	movl	%eax, %r15d	# D.15657, D.15657
.L839:
	movslq	%ebx, %rdi	# ivtmp.2004, D.15662
	addq	160(%rsp), %rdi	# %sfp, D.15659
	leal	(%rbx,%r13), %esi	#, D.15657
	movslq	%esi, %rsi	# D.15657, D.15662
	addq	224(%rsp), %rsi	# %sfp, D.15659
	movq	%r12, %rdx	# D.15663,
	call	memcpy	#
	addl	$1, %ebp	#, i
	addl	%r15d, %ebx	# D.15657, ivtmp.2004
	cmpl	%r14d, %ebp	# D.15658, i
	jne	.L839	#,
	movl	(%rsp), %r15d	# %sfp, D.15658
	jmp	.L835	#
.L837:
	movl	60(%rsp), %ebx	# %sfp, srcStride
	movl	%ebx, %eax	# srcStride, D.15658
	movl	48(%rsp), %ecx	# %sfp, copyAhead
	imull	%ecx, %eax	# copyAhead, D.15658
	cltq
	movq	%rax, 64(%rsp)	# D.15662, %sfp
	movq	248(%rsp), %rsi	# %sfp, D.15666
	addq	%rax, %rsi	# D.15662, D.15666
	movq	%rsi, 272(%rsp)	# D.15666, %sfp
	movl	24(%rsp), %esi	# %sfp, height
	leal	-1(%rsi), %edx	#, D.15658
	movl	%ebx, %eax	# srcStride, D.15658
	imull	%edx, %eax	# D.15658, D.15658
	cltq
	addq	192(%rsp), %rax	# %sfp, D.15661
	movq	%rax, 280(%rsp)	# D.15661, %sfp
	movl	12(%rsp), %edi	# %sfp, dstStride
	imull	%edi, %edx	# dstStride, D.15658
	movslq	%edx, %rax	# D.15658, D.15662
	addq	160(%rsp), %rax	# %sfp, D.15659
	movq	%rax, 288(%rsp)	# D.15659, %sfp
	leal	(%rbx,%rbx,2), %eax	#, D.15658
	sall	$2, %eax	#, tmp845
	cltq
	movq	%rax, 120(%rsp)	# D.15662, %sfp
	movl	%edi, %eax	# dstStride, D.15658
	imull	%ecx, %eax	# copyAhead, D.15658
	cltq
	movq	%rax, 72(%rsp)	# D.15662, %sfp
	movl	%edi, %eax	# dstStride, dstStride
	sall	$2, %eax	#, D.15658
	cltq
	movq	%rax, 80(%rsp)	# D.15662, %sfp
	movl	%edi, %eax	# dstStride, dstStride
	leal	(%rdi,%rdi), %ecx	#, tmp849
	addl	%ecx, %eax	# tmp849, D.15658
	cltq
	movq	%rax, 96(%rsp)	# D.15662, %sfp
	movl	%edi, %eax	# dstStride, tmp852
	negl	%eax	# tmp852
	sall	$3, %eax	#, tmp853
	movslq	%eax, %r11	# tmp853, offset
	movq	%r11, 176(%rsp)	# offset, %sfp
	negq	%r11	# D.15662
	movq	%r11, 184(%rsp)	# D.15662, %sfp
	leal	0(,%rbx,8), %r11d	#, D.15657
	movl	%r11d, 240(%rsp)	# D.15657, %sfp
	sall	$3, %edi	#, D.15657
	movl	%edi, 212(%rsp)	# D.15657, %sfp
	movl	%esi, 268(%rsp)	# height, %sfp
	leal	1(%rsi), %edi	#, ivtmp.1995
	movl	%edi, 204(%rsp)	# ivtmp.1995, %sfp
	movl	%eax, 220(%rsp)	# tmp853, %sfp
	leal	(%rdx,%rcx), %eax	#, ivtmp.1996
	movl	%eax, 208(%rsp)	# ivtmp.1996, %sfp
	movl	%esi, 200(%rsp)	# height, %sfp
	movl	$0, 168(%rsp)	#, %sfp
	movl	$0, 156(%rsp)	#, %sfp
	movl	$0, 16(%rsp)	#, %sfp
	movl	%ebx, %edx	# srcStride, tmp1074
	sarl	$31, %edx	#, tmp1074
	movl	%ebx, %eax	# srcStride, tmp1075
	xorl	%edx, %eax	# tmp1074, tmp1075
	subl	%edx, %eax	# tmp1074, D.15658
	cltq
	movq	%rax, 296(%rsp)	# D.15663, %sfp
.L880:
	movslq	156(%rsp), %r14	# %sfp, D.15662
	addq	192(%rsp), %r14	# %sfp, srcBlock
	movslq	168(%rsp), %rax	# %sfp, D.15662
	movq	%rax, %rbx	# D.15662, D.15662
	movq	%rax, 136(%rsp)	# D.15662, %sfp
	movq	160(%rsp), %rax	# %sfp, dstBlock
	addq	%rbx, %rax	# D.15662, dstBlock
	movq	%rax, %rbx	# dstBlock, dstBlock
	movq	%rax, 232(%rsp)	# dstBlock, %sfp
	movq	1656(%rsp), %r13	# c.tempBlocks, tempBlock2
	leaq	8(%r13), %rax	#, tempBlock2
	movq	%rax, (%rsp)	# tempBlock2, %sfp
	movl	16(%rsp), %esi	# %sfp, y
	movl	%esi, %eax	# y, D.15658
	movzbl	216(%rsp), %ecx	# %sfp, tmp1287
	sarl	%cl, %eax	# tmp1287, D.15658
	movl	%eax, %ecx	# D.15658, D.15658
	movl	%eax, %edx	# D.15658, D.15658
	imull	2984(%rsp), %edx	# QPStride, D.15658
	movslq	%edx, %rdx	# D.15658, D.15662
	addq	2976(%rsp), %rdx	# QPs, QPptr
	movq	%rdx, 32(%rsp)	# QPptr, %sfp
	movl	2984(%rsp), %edx	# QPStride, tmp859
	sarl	$31, %edx	#, tmp859
	movl	%edx, %eax	# tmp859, tmp860
	xorl	2984(%rsp), %eax	# QPStride, tmp860
	subl	%edx, %eax	# tmp859, D.15658
	imull	%ecx, %eax	# D.15658, D.15658
	cltq
	addq	2808(%rsp), %rax	# c.nonBQPTable, nonBQPptr
	movq	%rax, 40(%rsp)	# nonBQPptr, %sfp
	movl	%esi, %eax	# y, y
	movl	%esi, 52(%rsp)	# y, %sfp
	addl	$15, %eax	#, D.15658
	movl	%eax, 172(%rsp)	# D.15658, %sfp
	cmpl	%eax, 24(%rsp)	# D.15658, %sfp
	jle	.L840	#,
	jmp	.L845	#
.L888:
	movq	256(%rsp), %rbx	# %sfp, dstBlock
	movq	248(%rsp), %r14	# %sfp, srcBlock
.L845:
	movl	$0, %r12d	#, x
	cmpl	$0, 28(%rsp)	#, %sfp
	jle	.L842	#,
	jmp	.L841	#
.L840:
	movl	268(%rsp), %edx	# %sfp, D.15657
	movl	244(%rsp), %r12d	# %sfp, copyAhead
	subl	%r12d, %edx	# copyAhead, D.15657
	addl	$8, %edx	#, D.15657
	subl	16(%rsp), %edx	# %sfp, D.15657
	movl	$0, %eax	#, tmp868
	cmovs	%eax, %edx	# D.15657,, tmp868, D.15657
	movq	64(%rsp), %rax	# %sfp, D.15662
	leaq	(%r14,%rax), %rsi	#, D.15671
	movl	60(%rsp), %ebp	# %sfp, srcStride
	movl	%ebp, %ecx	# srcStride,
	movq	272(%rsp), %rdi	# %sfp,
	call	linecpy	#
	movl	200(%rsp), %eax	# %sfp, ivtmp.1993
	cmpl	$8, %eax	#, ivtmp.1993
	movl	$8, %ebx	#, tmp870
	cmovge	%eax, %ebx	# ivtmp.1993,, i
	cmpl	%ebx, %r12d	# i, copyAhead
	jle	.L843	#,
	movl	%ebp, %r12d	# srcStride, D.15657
	imull	%ebx, %ebp	# i, ivtmp.1984
.L844:
	movslq	%ebp, %rdi	# ivtmp.1984, D.15662
	addq	248(%rsp), %rdi	# %sfp, D.15659
	movq	296(%rsp), %rdx	# %sfp,
	movq	280(%rsp), %rsi	# %sfp,
	call	memcpy	#
	addl	$1, %ebx	#, i
	addl	%r12d, %ebp	# D.15657, ivtmp.1984
	cmpl	%ebx, 244(%rsp)	# i, %sfp
	jg	.L844	#,
.L843:
	movl	204(%rsp), %ebp	# %sfp, ivtmp.1995
	movl	%ebp, %ebx	# ivtmp.1995, i
	movl	244(%rsp), %eax	# %sfp, copyAhead
	subl	$7, %eax	#, D.15658
	cmpl	%eax, %ebp	# D.15658, ivtmp.1995
	cmovle	%ebp, %eax	# ivtmp.1995,, D.15658
	movl	%eax, %edx	# D.15658, D.15658
	movq	232(%rsp), %rsi	# %sfp, D.15659
	subq	88(%rsp), %rsi	# %sfp, D.15659
	movl	12(%rsp), %r14d	# %sfp, dstStride
	movl	%r14d, %ecx	# dstStride,
	movq	224(%rsp), %rdi	# %sfp,
	call	linecpy	#
	cmpl	%ebp, 48(%rsp)	# ivtmp.1995, %sfp
	jl	.L888	#,
	movl	208(%rsp), %ebp	# %sfp, ivtmp.1977
	movslq	264(%rsp), %r12	# %sfp, D.15663
.L846:
	movslq	%ebp, %rdi	# ivtmp.1977, D.15662
	addq	224(%rsp), %rdi	# %sfp, D.15659
	movq	%r12, %rdx	# D.15663,
	movq	288(%rsp), %rsi	# %sfp,
	call	memcpy	#
	addl	$1, %ebx	#, i
	addl	%r14d, %ebp	# D.15657, ivtmp.1977
	cmpl	%ebx, 48(%rsp)	# i, %sfp
	jge	.L846	#,
	movq	256(%rsp), %rbx	# %sfp, dstBlock
	movq	248(%rsp), %r14	# %sfp, srcBlock
	jmp	.L845	#
.L841:
	movl	16(%rsp), %eax	# %sfp, D.15658
	sarl	$3, %eax	#, D.15658
	sall	$8, %eax	#, D.15658
	cltq
	addq	$256, %rax	#, D.15662
	movq	%rax, 144(%rsp)	# D.15662, %sfp
	movl	$0, %r12d	#, x
	leaq	1672(%rsp), %rax	#, tmp1066
	movq	%rax, 128(%rsp)	# tmp1066, %sfp
	movl	%r15d, %eax	# D.15658, tmp1067
	shrl	$2, %eax	#, tmp1067
	andl	$1, %eax	#, D.15677
	movb	%al, 59(%rsp)	# D.15677, %sfp
	jmp	.L873	#
.L889:
	movq	%rax, %r13	# tempBlock2, tempBlock2
.L873:
	cmpl	$0, 2992(%rsp)	#, isColor
	je	.L847	#,
	movl	%r12d, %edx	# x, D.15658
	movzbl	116(%rsp), %ecx	# %sfp, tmp1327
	sarl	%cl, %edx	# tmp1327, D.15658
	movslq	%edx, %rdx	# D.15658, D.15662
	movq	32(%rsp), %rax	# %sfp, QPptr
	movsbl	(%rax,%rdx), %eax	# *_301, QP
	movq	40(%rsp), %rdi	# %sfp, nonBQPptr
	movsbl	(%rdi,%rdx), %edx	# *_304, *_304
	movl	%edx, 2828(%rsp)	# *_304, c.nonBQP
	jmp	.L848	#
.L847:
	movl	%r12d, %edx	# x, D.15658
	sarl	$4, %edx	#, D.15658
	movslq	%edx, %rdx	# D.15658, D.15662
	movq	32(%rsp), %rax	# %sfp, QPptr
	movsbl	(%rax,%rdx), %eax	# *_310, QP
	movl	112(%rsp), %esi	# %sfp, QPCorrecture
	imull	%esi, %eax	# QPCorrecture, D.15658
	addl	$32768, %eax	#, D.15658
	sarl	$16, %eax	#, QP
	movq	40(%rsp), %rdi	# %sfp, nonBQPptr
	movsbl	(%rdi,%rdx), %edx	# *_316, D.15658
	imull	%esi, %edx	# QPCorrecture, D.15658
	addl	$32768, %edx	#, D.15658
	sarl	$16, %edx	#, tmp904
	movl	%edx, 2828(%rsp)	# tmp904, c.nonBQP
	movq	120(%rsp), %rsi	# %sfp, D.15662
	movzbl	4(%r14,%rsi), %edx	# MEM[base: srcBlock_628, index: _325, offset: 4B], D.15663
	movq	104(%rsp), %rdi	# %sfp, yHistogram
	addq	$1, (%rdi,%rdx,8)	#, *_331
.L848:
	movl	%eax, 2824(%rsp)	# QP, c.QP
#APP
# 3497 "postprocess_template.c" 1
	movd %eax, %mm7         	# QP
	packuswb %mm7, %mm7  
	packuswb %mm7, %mm7  
	packuswb %mm7, %mm7  
	movq %mm7, 1760(%rsp)         	# c.pQPb
	
# 0 "" 2
#NO_APP
	movslq	60(%rsp), %rsi	# %sfp, D.15670
	movslq	12(%rsp), %rbp	# %sfp, D.15670
	movslq	%r12d, %rcx	# x, D.15670
	movslq	48(%rsp), %rdi	# %sfp, D.15670
#APP
# 3517 "postprocess_template.c" 1
	mov %rcx, %rax              	# D.15670
	shr $2, %rax              
	and $6, %rax              
	add %rdi, %rax              	# D.15670
	mov %rax, %rdx       
	imul %rsi, %rax             	# D.15670
	imul %rbp, %rdx             	# D.15670
	prefetchnta 32(%rax, %r14)  	# srcBlock
	prefetcht0 32(%rdx, %rbx)   	# dstBlock
	add %rsi, %rax              	# D.15670
	add %rbp, %rdx              	# D.15670
	prefetchnta 32(%rax, %r14)  	# srcBlock
	prefetcht0 32(%rdx, %rbx)   	# dstBlock
	
# 0 "" 2
#NO_APP
	movq	64(%rsp), %rax	# %sfp, D.15662
	leaq	(%r14,%rax), %rcx	#, D.15661
	movq	72(%rsp), %rax	# %sfp, D.15662
	leaq	(%rbx,%rax), %rdi	#, D.15659
	testb	$8, %r15b	#, D.15658
	je	.L849	#,
	movq	128(%rsp), %rax	# %sfp, packedOffsetAndScale
#APP
# 3102 "postprocess_template.c" 1
	movq (%rax), %mm2        
	movq 8(%rax), %mm3       
	lea (%rcx,%rsi), %rax         	# D.15661, D.15670
	lea (%rdi,%rbp), %rdx         	# D.15659, D.15670
	pxor %mm4, %mm4              
	movq (%rcx), %mm0          	# D.15661
	movq (%rcx), %mm5          	# D.15661
	movq (%rcx, %rsi), %mm1          	# D.15661, D.15670
	movq (%rcx, %rsi), %mm6          	# D.15661, D.15670
	punpcklbw %mm0, %mm0         
	punpckhbw %mm5, %mm5         
	punpcklbw %mm1, %mm1         
	punpckhbw %mm6, %mm6         
	pmulhuw %mm3, %mm0           
	pmulhuw %mm3, %mm5           
	pmulhuw %mm3, %mm1           
	pmulhuw %mm3, %mm6           
	psubw %mm2, %mm0             
	psubw %mm2, %mm5             
	psubw %mm2, %mm1             
	psubw %mm2, %mm6             
	packuswb %mm5, %mm0          
	packuswb %mm6, %mm1          
	movq %mm0, (%rdi)          	# D.15659
	movq %mm1, (%rdi, %rbp)          	# D.15659, D.15670
	movq (%rcx, %rsi, 2), %mm0          	# D.15661, D.15670
	movq (%rcx, %rsi, 2), %mm5          	# D.15661, D.15670
	movq (%rax, %rsi, 2), %mm1          	# D.15670
	movq (%rax, %rsi, 2), %mm6          	# D.15670
	punpcklbw %mm0, %mm0         
	punpckhbw %mm5, %mm5         
	punpcklbw %mm1, %mm1         
	punpckhbw %mm6, %mm6         
	pmulhuw %mm3, %mm0           
	pmulhuw %mm3, %mm5           
	pmulhuw %mm3, %mm1           
	pmulhuw %mm3, %mm6           
	psubw %mm2, %mm0             
	psubw %mm2, %mm5             
	psubw %mm2, %mm1             
	psubw %mm2, %mm6             
	packuswb %mm5, %mm0          
	packuswb %mm6, %mm1          
	movq %mm0, (%rdi, %rbp, 2)          	# D.15659, D.15670
	movq %mm1, (%rdx, %rbp, 2)          	# D.15670
	movq (%rcx, %rsi, 4), %mm0          	# D.15661, D.15670
	movq (%rcx, %rsi, 4), %mm5          	# D.15661, D.15670
	movq (%rax, %rsi, 4), %mm1          	# D.15670
	movq (%rax, %rsi, 4), %mm6          	# D.15670
	punpcklbw %mm0, %mm0         
	punpckhbw %mm5, %mm5         
	punpcklbw %mm1, %mm1         
	punpckhbw %mm6, %mm6         
	pmulhuw %mm3, %mm0           
	pmulhuw %mm3, %mm5           
	pmulhuw %mm3, %mm1           
	pmulhuw %mm3, %mm6           
	psubw %mm2, %mm0             
	psubw %mm2, %mm5             
	psubw %mm2, %mm1             
	psubw %mm2, %mm6             
	packuswb %mm5, %mm0          
	packuswb %mm6, %mm1          
	movq %mm0, (%rdi, %rbp, 4)          	# D.15659, D.15670
	movq %mm1, (%rdx, %rbp, 4)          	# D.15670
	lea (%rax,%rsi,4), %rax        	# D.15670
	lea (%rdx,%rbp,4), %rdx        	# D.15670
	movq (%rax, %rsi), %mm0          	# D.15670
	movq (%rax, %rsi), %mm5          	# D.15670
	movq (%rax, %rsi, 2), %mm1          	# D.15670
	movq (%rax, %rsi, 2), %mm6          	# D.15670
	punpcklbw %mm0, %mm0         
	punpckhbw %mm5, %mm5         
	punpcklbw %mm1, %mm1         
	punpckhbw %mm6, %mm6         
	pmulhuw %mm3, %mm0           
	pmulhuw %mm3, %mm5           
	pmulhuw %mm3, %mm1           
	pmulhuw %mm3, %mm6           
	psubw %mm2, %mm0             
	psubw %mm2, %mm5             
	psubw %mm2, %mm1             
	psubw %mm2, %mm6             
	packuswb %mm5, %mm0          
	packuswb %mm6, %mm1          
	movq %mm0, (%rdx, %rbp)          	# D.15670
	movq %mm1, (%rdx, %rbp, 2)          	# D.15670
	
# 0 "" 2
#NO_APP
	jmp	.L850	#
.L849:
#APP
# 3185 "postprocess_template.c" 1
	lea (%rcx,%rsi), %rax                 	# D.15661, D.15670
	lea (%rdi,%rbp), %rdx                 	# D.15659, D.15670
	movq (%rcx), %mm0          	# D.15661
	movq (%rcx, %rsi), %mm1          	# D.15661, D.15670
	movq %mm0, (%rdi)          	# D.15659
	movq %mm1, (%rdi, %rbp)          	# D.15659, D.15670
	movq (%rcx, %rsi, 2), %mm0          	# D.15661, D.15670
	movq (%rax, %rsi, 2), %mm1          	# D.15670
	movq %mm0, (%rdi, %rbp, 2)          	# D.15659, D.15670
	movq %mm1, (%rdx, %rbp, 2)          	# D.15670
	movq (%rcx, %rsi, 4), %mm0          	# D.15661, D.15670
	movq (%rax, %rsi, 4), %mm1          	# D.15670
	movq %mm0, (%rdi, %rbp, 4)          	# D.15659, D.15670
	movq %mm1, (%rdx, %rbp, 4)          	# D.15670
	lea (%rax,%rsi,4), %rax        	# D.15670
	lea (%rdx,%rbp,4), %rdx        	# D.15670
	movq (%rax, %rsi), %mm0          	# D.15670
	movq (%rax, %rsi, 2), %mm1          	# D.15670
	movq %mm0, (%rdx, %rbp)          	# D.15670
	movq %mm1, (%rdx, %rbp, 2)          	# D.15670
	
# 0 "" 2
#NO_APP
.L850:
	testl	$65536, %r15d	#, D.15658
	je	.L851	#,
	movq	80(%rsp), %rax	# %sfp, D.15662
	leaq	(%rbx,%rax), %rdx	#, D.15659
#APP
# 1455 "postprocess_template.c" 1
	lea (%rdx, %rbp), %rax                	# D.15659, D.15670
	lea (%rax, %rbp, 4), %rcx      	# D.15670
	movq (%rdx), %mm0                       	# D.15659
	movq (%rax, %rbp), %mm1            	# D.15670
	pavgb %mm1, %mm0 
	movq %mm0, (%rax)                
	movq (%rdx, %rbp, 4), %mm0                	# D.15659, D.15670
	pavgb %mm0, %mm1 
	movq %mm1, (%rax, %rbp, 2)         	# D.15670
	movq (%rcx, %rbp), %mm1            	# D.15670
	pavgb %mm1, %mm0 
	movq %mm0, (%rcx)                
	movq (%rdx, %rbp, 8), %mm0                	# D.15659, D.15670
	pavgb %mm0, %mm1 
	movq %mm1, (%rcx, %rbp, 2)         	# D.15670
	
# 0 "" 2
#NO_APP
	jmp	.L852	#
.L851:
	testl	$131072, %r15d	#, D.15658
	je	.L853	#,
	movq	80(%rsp), %rax	# %sfp, D.15662
	leaq	(%rbx,%rax), %rsi	#, D.15659
	movslq	%r12d, %rcx	# x, D.15662
	addq	1752(%rsp), %rcx	# c.deintTemp, D.15659
#APP
# 1775 "postprocess_template.c" 1
	lea (%rsi, %rbp), %rax                	# D.15659, D.15670
	lea (%rax, %rbp, 4), %rdx      	# D.15670
	movq (%rcx), %mm0                       	# D.15659
	movq (%rax), %mm1                
	pavgb %mm1, %mm0 
	movq (%rsi), %mm2                       	# D.15659
	pavgb %mm2, %mm0 
	movq %mm0, (%rsi)                       	# D.15659
	movq (%rax, %rbp), %mm0            	# D.15670
	pavgb %mm0, %mm2 
	pavgb %mm1, %mm2 
	movq %mm2, (%rax)                
	movq (%rax, %rbp, 2), %mm2         	# D.15670
	pavgb %mm2, %mm1 
	pavgb %mm0, %mm1 
	movq %mm1, (%rax, %rbp)            	# D.15670
	movq (%rsi, %rbp, 4), %mm1                	# D.15659, D.15670
	pavgb %mm1, %mm0 
	pavgb %mm2, %mm0 
	movq %mm0, (%rax, %rbp, 2)         	# D.15670
	movq (%rdx), %mm0                
	pavgb %mm0, %mm2 
	pavgb %mm1, %mm2 
	movq %mm2, (%rsi, %rbp, 4)                	# D.15659, D.15670
	movq (%rdx, %rbp), %mm2            	# D.15670
	pavgb %mm2, %mm1 
	pavgb %mm0, %mm1 
	movq %mm1, (%rdx)                
	movq (%rdx, %rbp, 2), %mm1         	# D.15670
	pavgb %mm1, %mm0 
	pavgb %mm2, %mm0 
	movq %mm0, (%rdx, %rbp)            	# D.15670
	movq (%rsi, %rbp, 8), %mm0                	# D.15659, D.15670
	pavgb %mm0, %mm2 
	pavgb %mm1, %mm2 
	movq %mm2, (%rdx, %rbp, 2)         	# D.15670
	movq %mm1, (%rcx)                       	# D.15659
	
# 0 "" 2
#NO_APP
	jmp	.L852	#
.L853:
	testl	$524288, %r15d	#, D.15658
	je	.L854	#,
	movq	80(%rsp), %rax	# %sfp, D.15662
	leaq	(%rbx,%rax), %rcx	#, D.15659
#APP
# 1877 "postprocess_template.c" 1
	lea (%rcx, %rbp), %rax                	# D.15659, D.15670
	lea (%rax, %rbp, 4), %rdx      	# D.15670
	movq (%rcx), %mm0                       	# D.15659
	movq (%rax, %rbp), %mm2            	# D.15670
	movq (%rax), %mm1                
	movq %mm0, %mm3                      
	pmaxub %mm1, %mm0                    
	pminub %mm3, %mm1                    
	pmaxub %mm2, %mm1                    
	pminub %mm1, %mm0                    
	movq %mm0, (%rax)                
	movq (%rcx, %rbp, 4), %mm0                	# D.15659, D.15670
	movq (%rax, %rbp, 2), %mm1         	# D.15670
	movq %mm2, %mm3                      
	pmaxub %mm1, %mm2                    
	pminub %mm3, %mm1                    
	pmaxub %mm0, %mm1                    
	pminub %mm1, %mm2                    
	movq %mm2, (%rax, %rbp, 2)         	# D.15670
	movq (%rdx), %mm2                
	movq (%rdx, %rbp), %mm1            	# D.15670
	movq %mm2, %mm3                      
	pmaxub %mm0, %mm2                    
	pminub %mm3, %mm0                    
	pmaxub %mm1, %mm0                    
	pminub %mm0, %mm2                    
	movq %mm2, (%rdx)                
	movq (%rdx, %rbp, 2), %mm2         	# D.15670
	movq (%rcx, %rbp, 8), %mm0                	# D.15659, D.15670
	movq %mm2, %mm3                      
	pmaxub %mm0, %mm2                    
	pminub %mm3, %mm0                    
	pmaxub %mm1, %mm0                    
	pminub %mm0, %mm2                    
	movq %mm2, (%rdx, %rbp, 2)         	# D.15670
	
# 0 "" 2
#NO_APP
	jmp	.L852	#
.L854:
	cmpl	$0, 152(%rsp)	#, %sfp
	je	.L855	#,
	movq	96(%rsp), %rax	# %sfp, D.15662
	leaq	(%rbx,%rax), %rsi	#, D.15659
#APP
# 1508 "postprocess_template.c" 1
	lea (%rsi, %rbp), %rax                	# D.15659, D.15670
	lea (%rax, %rbp, 4), %rdx      	# D.15670
	lea (%rdx, %rbp, 4), %rcx      	# D.15670
	add %rbp, %rcx                      	# D.15670
	pxor %mm7, %mm7                      
	movq (%rsi), %mm0                     	# D.15659
	movq (%rax, %rbp), %mm1                     	# D.15670
	movq (%rsi, %rbp, 4), %mm2                     	# D.15659, D.15670
	movq (%rdx, %rbp), %mm3                     	# D.15670
	pavgb %mm2, %mm1 
	pavgb %mm3, %mm0 
	movq %mm0, %mm2                      
	punpcklbw %mm7, %mm0                 
	punpckhbw %mm7, %mm2                 
	movq %mm1, %mm3                      
	punpcklbw %mm7, %mm1                 
	punpckhbw %mm7, %mm3                 
	psubw %mm1, %mm0                     
	psubw %mm3, %mm2                     
	psraw $3, %mm0                        
	psraw $3, %mm2                        
	psubw %mm0, %mm1                     
	psubw %mm2, %mm3                     
	packuswb %mm3, %mm1                  
	movq %mm1, (%rax, %rbp, 2)                     	# D.15670
	movq (%rax, %rbp), %mm0                     	# D.15670
	movq (%rsi, %rbp, 4), %mm1                     	# D.15659, D.15670
	movq (%rdx, %rbp), %mm2                     	# D.15670
	movq (%rsi, %rbp, 8), %mm3                     	# D.15659, D.15670
	pavgb %mm2, %mm1 
	pavgb %mm3, %mm0 
	movq %mm0, %mm2                      
	punpcklbw %mm7, %mm0                 
	punpckhbw %mm7, %mm2                 
	movq %mm1, %mm3                      
	punpcklbw %mm7, %mm1                 
	punpckhbw %mm7, %mm3                 
	psubw %mm1, %mm0                     
	psubw %mm3, %mm2                     
	psraw $3, %mm0                        
	psraw $3, %mm2                        
	psubw %mm0, %mm1                     
	psubw %mm2, %mm3                     
	packuswb %mm3, %mm1                  
	movq %mm1, (%rdx)                     
	movq (%rsi, %rbp, 4), %mm0                     	# D.15659, D.15670
	movq (%rdx, %rbp), %mm1                     	# D.15670
	movq (%rsi, %rbp, 8), %mm2                     	# D.15659, D.15670
	movq (%rcx), %mm3                     
	pavgb %mm2, %mm1 
	pavgb %mm3, %mm0 
	movq %mm0, %mm2                      
	punpcklbw %mm7, %mm0                 
	punpckhbw %mm7, %mm2                 
	movq %mm1, %mm3                      
	punpcklbw %mm7, %mm1                 
	punpckhbw %mm7, %mm3                 
	psubw %mm1, %mm0                     
	psubw %mm3, %mm2                     
	psraw $3, %mm0                        
	psraw $3, %mm2                        
	psubw %mm0, %mm1                     
	psubw %mm2, %mm3                     
	packuswb %mm3, %mm1                  
	movq %mm1, (%rdx, %rbp, 2)                     	# D.15670
	movq (%rdx, %rbp), %mm0                     	# D.15670
	movq (%rsi, %rbp, 8), %mm1                     	# D.15659, D.15670
	movq (%rcx), %mm2                     
	movq (%rcx, %rbp, 2), %mm3                     	# D.15670
	pavgb %mm2, %mm1 
	pavgb %mm3, %mm0 
	movq %mm0, %mm2                      
	punpcklbw %mm7, %mm0                 
	punpckhbw %mm7, %mm2                 
	movq %mm1, %mm3                      
	punpcklbw %mm7, %mm1                 
	punpckhbw %mm7, %mm3                 
	psubw %mm1, %mm0                     
	psubw %mm3, %mm2                     
	psraw $3, %mm0                        
	psraw $3, %mm2                        
	psubw %mm0, %mm1                     
	psubw %mm2, %mm3                     
	packuswb %mm3, %mm1                  
	movq %mm1, (%rdx, %rbp, 4)                     	# D.15670
	
# 0 "" 2
#NO_APP
	jmp	.L852	#
.L855:
	testl	$4194304, %r15d	#, D.15658
	je	.L856	#,
	movq	80(%rsp), %rax	# %sfp, D.15662
	leaq	(%rbx,%rax), %rsi	#, D.15659
	movslq	%r12d, %rcx	# x, D.15662
	addq	1752(%rsp), %rcx	# c.deintTemp, D.15659
#APP
# 1595 "postprocess_template.c" 1
	lea (%rsi, %rbp), %rax                	# D.15659, D.15670
	lea (%rax, %rbp, 4), %rdx      	# D.15670
	pxor %mm7, %mm7                      
	movq (%rcx), %mm0                       	# D.15659
	movq (%rsi), %mm1                     	# D.15659
	movq (%rax), %mm2                     
	movq (%rax, %rbp), %mm3                     	# D.15670
	movq (%rax, %rbp, 2), %mm4                     	# D.15670
	pavgb %mm3, %mm1 
	pavgb %mm4, %mm0 
	movq %mm0, %mm3                      
	punpcklbw %mm7, %mm0                 
	punpckhbw %mm7, %mm3                 
	movq %mm1, %mm4                      
	punpcklbw %mm7, %mm1                 
	punpckhbw %mm7, %mm4                 
	psllw $2, %mm1                        
	psllw $2, %mm4                        
	psubw %mm0, %mm1                     
	psubw %mm3, %mm4                     
	movq %mm2, %mm5                      
	movq %mm2, %mm0                      
	punpcklbw %mm7, %mm2                 
	punpckhbw %mm7, %mm5                 
	paddw %mm2, %mm1                     
	paddw %mm5, %mm4                     
	psraw $2, %mm1                        
	psraw $2, %mm4                        
	packuswb %mm4, %mm1                  
	movq %mm1, (%rax)                     
	movq (%rax, %rbp), %mm1                     	# D.15670
	movq (%rax, %rbp, 2), %mm2                     	# D.15670
	movq (%rsi, %rbp, 4), %mm3                     	# D.15659, D.15670
	movq (%rdx), %mm4                     
	pavgb %mm3, %mm1 
	pavgb %mm4, %mm0 
	movq %mm0, %mm3                      
	punpcklbw %mm7, %mm0                 
	punpckhbw %mm7, %mm3                 
	movq %mm1, %mm4                      
	punpcklbw %mm7, %mm1                 
	punpckhbw %mm7, %mm4                 
	psllw $2, %mm1                        
	psllw $2, %mm4                        
	psubw %mm0, %mm1                     
	psubw %mm3, %mm4                     
	movq %mm2, %mm5                      
	movq %mm2, %mm0                      
	punpcklbw %mm7, %mm2                 
	punpckhbw %mm7, %mm5                 
	paddw %mm2, %mm1                     
	paddw %mm5, %mm4                     
	psraw $2, %mm1                        
	psraw $2, %mm4                        
	packuswb %mm4, %mm1                  
	movq %mm1, (%rax, %rbp, 2)                     	# D.15670
	movq (%rsi, %rbp, 4), %mm1                     	# D.15659, D.15670
	movq (%rdx), %mm2                     
	movq (%rdx, %rbp), %mm3                     	# D.15670
	movq (%rdx, %rbp, 2), %mm4                     	# D.15670
	pavgb %mm3, %mm1 
	pavgb %mm4, %mm0 
	movq %mm0, %mm3                      
	punpcklbw %mm7, %mm0                 
	punpckhbw %mm7, %mm3                 
	movq %mm1, %mm4                      
	punpcklbw %mm7, %mm1                 
	punpckhbw %mm7, %mm4                 
	psllw $2, %mm1                        
	psllw $2, %mm4                        
	psubw %mm0, %mm1                     
	psubw %mm3, %mm4                     
	movq %mm2, %mm5                      
	movq %mm2, %mm0                      
	punpcklbw %mm7, %mm2                 
	punpckhbw %mm7, %mm5                 
	paddw %mm2, %mm1                     
	paddw %mm5, %mm4                     
	psraw $2, %mm1                        
	psraw $2, %mm4                        
	packuswb %mm4, %mm1                  
	movq %mm1, (%rdx)                     
	movq (%rdx, %rbp), %mm1                     	# D.15670
	movq (%rdx, %rbp, 2), %mm2                     	# D.15670
	movq (%rsi, %rbp, 8), %mm3                     	# D.15659, D.15670
	movq (%rdx, %rbp, 4), %mm4                     	# D.15670
	pavgb %mm3, %mm1 
	pavgb %mm4, %mm0 
	movq %mm0, %mm3                      
	punpcklbw %mm7, %mm0                 
	punpckhbw %mm7, %mm3                 
	movq %mm1, %mm4                      
	punpcklbw %mm7, %mm1                 
	punpckhbw %mm7, %mm4                 
	psllw $2, %mm1                        
	psllw $2, %mm4                        
	psubw %mm0, %mm1                     
	psubw %mm3, %mm4                     
	movq %mm2, %mm5                      
	movq %mm2, %mm0                      
	punpcklbw %mm7, %mm2                 
	punpckhbw %mm7, %mm5                 
	paddw %mm2, %mm1                     
	paddw %mm5, %mm4                     
	psraw $2, %mm1                        
	psraw $2, %mm4                        
	packuswb %mm4, %mm1                  
	movq %mm1, (%rdx, %rbp, 2)                     	# D.15670
	movq %mm0, (%rcx)                       	# D.15659
	
# 0 "" 2
#NO_APP
	jmp	.L852	#
.L856:
	testl	$8388608, %r15d	#, D.15658
	je	.L852	#,
	movq	1752(%rsp), %rdx	# c.deintTemp, D.15659
	movslq	%r12d, %rax	# x, D.15662
	movslq	28(%rsp), %rcx	# %sfp, D.15662
	addq	%rax, %rcx	# D.15662, D.15662
	addq	%rdx, %rcx	# D.15659, D.15659
	addq	%rax, %rdx	# D.15662, D.15659
	movl	12(%rsp), %esi	# %sfp,
	movq	%rbx, %rdi	# dstBlock,
	call	deInterlaceL5_MMX2	#
.L852:
	movl	52(%rsp), %eax	# %sfp, D.15657
	addl	$8, %eax	#, D.15657
	cmpl	%eax, 24(%rsp)	# D.15657, %sfp
	jle	.L857	#,
	testl	$512, %r15d	#, D.15658
	je	.L858	#,
	movq	96(%rsp), %rax	# %sfp, D.15662
	leaq	(%rbx,%rax), %rdx	#, D.15659
#APP
# 412 "postprocess_template.c" 1
	pxor %mm7, %mm7                      
	lea (%rdx, %rbp), %rax                	# D.15659, D.15670
	lea (%rax, %rbp, 4), %rcx      	# D.15670
	movq (%rax, %rbp, 2), %mm0         	# D.15670
	movq (%rdx, %rbp, 4), %mm1                	# D.15659, D.15670
	movq %mm1, %mm2                      
	psubusb %mm0, %mm1                   
	psubusb %mm2, %mm0                   
	por %mm1, %mm0                       
	movq (%rcx), %mm3                
	movq (%rcx, %rbp), %mm4            	# D.15670
	movq %mm3, %mm5                      
	psubusb %mm4, %mm3                   
	psubusb %mm5, %mm4                   
	por %mm4, %mm3                       
	pavgb %mm3, %mm0 
	movq %mm2, %mm1                      
	psubusb %mm5, %mm2                   
	movq %mm2, %mm4                      
	pcmpeqb %mm7, %mm2                   
	psubusb %mm1, %mm5                   
	por %mm5, %mm4                       
	psubusb %mm0, %mm4                   
	movq %mm4, %mm3                      
	movq 1760(%rsp), %mm0                         	# c.pQPb
	paddusb %mm0, %mm0                   
	psubusb %mm0, %mm4                   
	pcmpeqb %mm7, %mm4                   
	psubusb b01, %mm3           
	pand %mm4, %mm3                      
	pavgb %mm7, %mm3 
	movq %mm3, %mm1                      
	pavgb %mm7, %mm3 
	pavgb %mm1, %mm3 
	movq (%rdx, %rbp, 4), %mm0                	# D.15659, D.15670
	pxor %mm2, %mm0                      
	psubusb %mm3, %mm0                   
	pxor %mm2, %mm0                      
	movq %mm0, (%rdx, %rbp, 4)                	# D.15659, D.15670
	movq (%rcx), %mm0                
	pxor %mm2, %mm0                      
	paddusb %mm3, %mm0                   
	pxor %mm2, %mm0                      
	movq %mm0, (%rcx)                
	pavgb %mm7, %mm1 
	movq (%rax, %rbp, 2), %mm0         	# D.15670
	pxor %mm2, %mm0                      
	psubusb %mm1, %mm0                   
	pxor %mm2, %mm0                      
	movq %mm0, (%rax, %rbp, 2)         	# D.15670
	movq (%rcx, %rbp), %mm0            	# D.15670
	pxor %mm2, %mm0                      
	paddusb %mm1, %mm0                   
	pxor %mm2, %mm0                      
	movq %mm0, (%rcx, %rbp)            	# D.15670
	pavgb %mm7, %mm1 
	movq (%rax, %rbp), %mm0            	# D.15670
	pxor %mm2, %mm0                      
	psubusb %mm1, %mm0                   
	pxor %mm2, %mm0                      
	movq %mm0, (%rax, %rbp)            	# D.15670
	movq (%rcx, %rbp, 2), %mm0         	# D.15670
	pxor %mm2, %mm0                      
	paddusb %mm1, %mm0                   
	pxor %mm2, %mm0                      
	movq %mm0, (%rcx, %rbp, 2)         	# D.15670
	
# 0 "" 2
#NO_APP
	jmp	.L857	#
.L858:
	testb	$1, %r15b	#, D.15658
	je	.L859	#,
	movq	80(%rsp), %rax	# %sfp, D.15662
	leaq	(%rbx,%rax), %rsi	#, D.15661
	movslq	2828(%rsp), %rax	# c.nonBQP, D.15658
#APP
# 114 "postprocess_template.c" 1
	movq 1776(%rsp,%rax,8), %mm7                         	# c.mmxDcOffset
	movq 2288(%rsp,%rax,8), %mm6                         	# c.mmxDcThreshold
	
# 0 "" 2
# 120 "postprocess_template.c" 1
	lea (%rsi, %rbp), %rax                	# D.15661, D.15670
	movq (%rsi), %mm0                       	# D.15661
	movq (%rax), %mm1                
	movq %mm0, %mm3                      
	movq %mm0, %mm4                      
	pmaxub %mm1, %mm4 
	pminub %mm1, %mm3 
	psubb %mm1, %mm0                     
	paddb %mm7, %mm0                     
	pcmpgtb %mm6, %mm0                   
	movq (%rax,%rbp), %mm2             	# D.15670
	pmaxub %mm2, %mm4 
	pminub %mm2, %mm3 
	psubb %mm2, %mm1                     
	paddb %mm7, %mm1                     
	pcmpgtb %mm6, %mm1                   
	paddb %mm1, %mm0                     
	movq (%rax, %rbp, 2), %mm1         	# D.15670
	pmaxub %mm1, %mm4 
	pminub %mm1, %mm3 
	psubb %mm1, %mm2                     
	paddb %mm7, %mm2                     
	pcmpgtb %mm6, %mm2                   
	paddb %mm2, %mm0                     
	lea (%rax, %rbp, 4), %rax      	# D.15670
	movq (%rsi, %rbp, 4), %mm2                	# D.15661, D.15670
	pmaxub %mm2, %mm4 
	pminub %mm2, %mm3 
	psubb %mm2, %mm1                     
	paddb %mm7, %mm1                     
	pcmpgtb %mm6, %mm1                   
	paddb %mm1, %mm0                     
	movq (%rax), %mm1                
	pmaxub %mm1, %mm4 
	pminub %mm1, %mm3 
	psubb %mm1, %mm2                     
	paddb %mm7, %mm2                     
	pcmpgtb %mm6, %mm2                   
	paddb %mm2, %mm0                     
	movq (%rax, %rbp), %mm2            	# D.15670
	pmaxub %mm2, %mm4 
	pminub %mm2, %mm3 
	psubb %mm2, %mm1                     
	paddb %mm7, %mm1                     
	pcmpgtb %mm6, %mm1                   
	paddb %mm1, %mm0                     
	movq (%rax, %rbp, 2), %mm1         	# D.15670
	pmaxub %mm1, %mm4 
	pminub %mm1, %mm3 
	psubb %mm1, %mm2                     
	paddb %mm7, %mm2                     
	pcmpgtb %mm6, %mm2                   
	paddb %mm2, %mm0                     
	psubusb %mm3, %mm4                   
	                                       
	pxor %mm7, %mm7                      
	psadbw %mm7, %mm0                    
	movq 1760(%rsp), %mm7                         	# c.pQPb
	paddusb %mm7, %mm7                   
	psubusb %mm7, %mm4                   
	packssdw %mm4, %mm4                  
	movd %mm0, %edx                         	# numEq
	movd %mm4, %ecx                         	# dcOk
	
# 0 "" 2
#NO_APP
	negl	%edx	# D.15658
	movzbl	%dl, %edx	# D.15658, numEq
	cmpl	2896(%rsp), %edx	# c.ppMode.flatnessThreshold, numEq
	jle	.L860	#,
	testl	%ecx, %ecx	# dcOk
	jne	.L857	#,
	movq	96(%rsp), %rax	# %sfp, D.15662
	leaq	(%rbx,%rax), %rdx	#, D.15659
#APP
# 232 "postprocess_template.c" 1
	movq 1760(%rsp), %mm0                         	# c.pQPb
	pxor %mm4, %mm4                      
	movq (%rdx), %mm6                       	# D.15659
	movq (%rdx, %rbp), %mm5                   	# D.15659, D.15670
	movq %mm5, %mm1                      
	movq %mm6, %mm2                      
	psubusb %mm6, %mm5                   
	psubusb %mm1, %mm2                   
	por %mm5, %mm2                       
	psubusb %mm0, %mm2                   
	pcmpeqb %mm4, %mm2                   
	pand %mm2, %mm6                      
	pandn %mm1, %mm2                     
	por %mm2, %mm6                       
	movq (%rdx, %rbp, 8), %mm5                	# D.15659, D.15670
	lea (%rdx, %rbp, 4), %rax             	# D.15659, D.15670
	lea (%rdx, %rbp, 8), %rcx             	# D.15659, D.15670
	sub %rbp, %rcx                      	# D.15670
	add %rbp, %rdx                             	# D.15670, D.15659
	movq (%rdx, %rbp, 8), %mm7                	# D.15659, D.15670
	movq %mm5, %mm1                      
	movq %mm7, %mm2                      
	psubusb %mm7, %mm5                   
	psubusb %mm1, %mm2                   
	por %mm5, %mm2                       
	psubusb %mm0, %mm2                   
	pcmpeqb %mm4, %mm2                   
	pand %mm2, %mm7                      
	pandn %mm1, %mm2                     
	por %mm2, %mm7                       
	movq (%rdx, %rbp), %mm0                   	# D.15659, D.15670
	movq %mm0, %mm1                      
	pavgb %mm6, %mm0 
	pavgb %mm6, %mm0 
	movq (%rdx, %rbp, 4), %mm2                	# D.15659, D.15670
	movq %mm2, %mm5                      
	pavgb (%rax), %mm2 
	pavgb (%rdx, %rbp, 2), %mm2 	# D.15659, D.15670
	movq %mm2, %mm3                      
	movq (%rdx), %mm4                       	# D.15659
	pavgb %mm4, %mm3 
	pavgb %mm0, %mm3 
	movq %mm3, (%rdx)                       	# D.15659
	movq %mm1, %mm0                      
	pavgb %mm6, %mm0 
	movq %mm4, %mm3                      
	pavgb (%rdx,%rbp,2), %mm3 	# D.15659, D.15670
	pavgb (%rax,%rbp,2), %mm5 	# D.15670
	pavgb (%rax), %mm5 
	pavgb %mm5, %mm3 
	pavgb %mm0, %mm3 
	movq %mm3, (%rdx,%rbp)                    	# D.15659, D.15670
	pavgb %mm4, %mm6 
	movq (%rcx), %mm0                
	pavgb (%rax, %rbp, 2), %mm0 	# D.15670
	movq %mm0, %mm3                      
	pavgb %mm1, %mm0 
	pavgb %mm6, %mm0 
	pavgb %mm2, %mm0 
	movq (%rdx, %rbp, 2), %mm2                	# D.15659, D.15670
	movq %mm0, (%rdx, %rbp, 2)                	# D.15659, D.15670
	movq (%rax, %rbp, 4), %mm0         	# D.15670
	pavgb (%rcx), %mm0 
	pavgb %mm0, %mm6 
	pavgb %mm1, %mm4 
	pavgb %mm2, %mm1 
	pavgb %mm1, %mm6 
	pavgb %mm5, %mm6 
	movq (%rax), %mm5                
	movq %mm6, (%rax)                
	movq (%rax, %rbp, 4), %mm6         	# D.15670
	pavgb %mm7, %mm6 
	pavgb %mm4, %mm6 
	pavgb %mm3, %mm6 
	pavgb %mm5, %mm2 
	movq (%rdx, %rbp, 4), %mm4                	# D.15659, D.15670
	pavgb %mm4, %mm2 
	pavgb %mm2, %mm6 
	movq %mm6, (%rdx, %rbp, 4)                	# D.15659, D.15670
	pavgb %mm7, %mm1 
	pavgb %mm4, %mm5 
	pavgb %mm5, %mm0 
	movq (%rax, %rbp, 2), %mm6         	# D.15670
	pavgb %mm6, %mm1 
	pavgb %mm0, %mm1 
	movq %mm1, (%rax, %rbp, 2)         	# D.15670
	pavgb (%rcx), %mm2 
	movq (%rax, %rbp, 4), %mm0         	# D.15670
	pavgb %mm0, %mm6 
	pavgb %mm7, %mm6 
	pavgb %mm2, %mm6 
	movq %mm6, (%rcx)                
	pavgb %mm7, %mm5 
	pavgb %mm7, %mm5 
	pavgb %mm3, %mm0 
	pavgb %mm0, %mm5 
	movq %mm5, (%rax, %rbp, 4)         	# D.15670
	sub %rbp, %rdx                             	# D.15670, D.15659
	
# 0 "" 2
#NO_APP
	jmp	.L857	#
.L860:
#APP
# 552 "postprocess_template.c" 1
	lea (%rsi, %rbp), %rax                	# D.15661, D.15670
	pcmpeqb %mm6, %mm6                   
	movq (%rax, %rbp, 2), %mm1         	# D.15670
	movq (%rsi, %rbp, 4), %mm0                	# D.15661, D.15670
	pxor %mm6, %mm1                      
	pavgb %mm1, %mm0 
	movq (%rax, %rbp, 4), %mm2         	# D.15670
	movq (%rax, %rbp), %mm3            	# D.15670
	pxor %mm6, %mm2                      
	movq %mm2, %mm5                      
	movq b80, %mm4              
	lea (%rax, %rbp, 4), %rcx      	# D.15670
	pavgb %mm3, %mm2 
	pavgb %mm0, %mm4 
	pavgb %mm2, %mm4 
	pavgb %mm0, %mm4 
	movq (%rax), %mm2                
	pxor %mm6, %mm2                      
	pavgb %mm3, %mm2 
	pavgb (%rsi), %mm1 	# D.15661
	movq b80, %mm3              
	pavgb %mm2, %mm3 
	pavgb %mm1, %mm3 
	pavgb %mm2, %mm3 
	pavgb (%rcx, %rbp), %mm5 	# D.15670
	movq (%rcx, %rbp, 2), %mm1         	# D.15670
	pxor %mm6, %mm1                      
	pavgb (%rsi, %rbp, 4), %mm1 	# D.15661, D.15670
	movq b80, %mm2              
	pavgb %mm5, %mm2 
	pavgb %mm1, %mm2 
	pavgb %mm5, %mm2 
	movq b00, %mm1              
	movq b00, %mm5              
	psubb %mm2, %mm1                     
	psubb %mm3, %mm5                     
	pmaxub %mm1, %mm2 
	pmaxub %mm5, %mm3 
	pminub %mm2, %mm3 
	movq b00, %mm7              
	movq 1760(%rsp), %mm2                         	# c.pQPb
	pavgb %mm6, %mm2 
	psubb %mm6, %mm2                     
	movq %mm4, %mm1                      
	pcmpgtb %mm7, %mm1                   
	pxor %mm1, %mm4                      
	psubb %mm1, %mm4                     
	pcmpgtb %mm4, %mm2                   
	psubusb %mm3, %mm4                   
	movq %mm4, %mm3                      
	psubusb b01, %mm4           
	pavgb %mm7, %mm4 
	pavgb %mm7, %mm4 
	paddb %mm3, %mm4                     
	pand %mm2, %mm4                      
	movq b80, %mm5              
	psubb %mm0, %mm5                     
	paddsb %mm6, %mm5                    
	pcmpgtb %mm5, %mm7                   
	pxor %mm7, %mm5                      
	pminub %mm5, %mm4 
	pxor %mm1, %mm7                      
	pand %mm7, %mm4                      
	movq (%rax, %rbp, 2), %mm0         	# D.15670
	movq (%rsi, %rbp, 4), %mm2                	# D.15661, D.15670
	pxor %mm1, %mm0                      
	pxor %mm1, %mm2                      
	paddb %mm4, %mm0                     
	psubb %mm4, %mm2                     
	pxor %mm1, %mm0                      
	pxor %mm1, %mm2                      
	movq %mm0, (%rax, %rbp, 2)         	# D.15670
	movq %mm2, (%rsi, %rbp, 4)                	# D.15661, D.15670
	
# 0 "" 2
#NO_APP
	jmp	.L857	#
.L859:
	testl	$1024, %r15d	#, D.15658
	je	.L857	#,
	movq	96(%rsp), %rax	# %sfp, D.15662
	leaq	(%rbx,%rax), %rdx	#, D.15659
	movslq	2828(%rsp), %rax	# c.nonBQP, D.15658
#APP
# 2553 "postprocess_template.c" 1
	movq 1776(%rsp,%rax,8), %mm7                         	# c.mmxDcOffset
	movq 2288(%rsp,%rax,8), %mm6                         	# c.mmxDcThreshold
	
# 0 "" 2
# 2559 "postprocess_template.c" 1
	lea (%rdx, %rbp), %rax                	# D.15659, D.15670
	movq (%rdx), %mm0                       	# D.15659
	movq (%rax), %mm1                
	movq %mm1, %mm3                      
	movq %mm1, %mm4                      
	psubb %mm1, %mm0                     
	paddb %mm7, %mm0                     
	pcmpgtb %mm6, %mm0                   
	movq (%rax,%rbp), %mm2             	# D.15670
	pmaxub %mm2, %mm4 
	pminub %mm2, %mm3 
	psubb %mm2, %mm1                     
	paddb %mm7, %mm1                     
	pcmpgtb %mm6, %mm1                   
	paddb %mm1, %mm0                     
	movq (%rax, %rbp, 2), %mm1         	# D.15670
	pmaxub %mm1, %mm4 
	pminub %mm1, %mm3 
	psubb %mm1, %mm2                     
	paddb %mm7, %mm2                     
	pcmpgtb %mm6, %mm2                   
	paddb %mm2, %mm0                     
	lea (%rax, %rbp, 4), %rax      	# D.15670
	movq (%rdx, %rbp, 4), %mm2                	# D.15659, D.15670
	pmaxub %mm2, %mm4 
	pminub %mm2, %mm3 
	psubb %mm2, %mm1                     
	paddb %mm7, %mm1                     
	pcmpgtb %mm6, %mm1                   
	paddb %mm1, %mm0                     
	movq (%rax), %mm1                
	pmaxub %mm1, %mm4 
	pminub %mm1, %mm3 
	psubb %mm1, %mm2                     
	paddb %mm7, %mm2                     
	pcmpgtb %mm6, %mm2                   
	paddb %mm2, %mm0                     
	movq (%rax, %rbp), %mm2            	# D.15670
	pmaxub %mm2, %mm4 
	pminub %mm2, %mm3 
	psubb %mm2, %mm1                     
	paddb %mm7, %mm1                     
	pcmpgtb %mm6, %mm1                   
	paddb %mm1, %mm0                     
	movq (%rax, %rbp, 2), %mm1         	# D.15670
	pmaxub %mm1, %mm4 
	pminub %mm1, %mm3 
	psubb %mm1, %mm2                     
	paddb %mm7, %mm2                     
	pcmpgtb %mm6, %mm2                   
	paddb %mm2, %mm0                     
	movq (%rdx, %rbp, 8), %mm2                	# D.15659, D.15670
	pmaxub %mm2, %mm4 
	pminub %mm2, %mm3 
	psubb %mm2, %mm1                     
	paddb %mm7, %mm1                     
	pcmpgtb %mm6, %mm1                   
	paddb %mm1, %mm0                     
	movq (%rax, %rbp, 4), %mm1         	# D.15670
	psubb %mm1, %mm2                     
	paddb %mm7, %mm2                     
	pcmpgtb %mm6, %mm2                   
	paddb %mm2, %mm0                     
	psubusb %mm3, %mm4                   
	pxor %mm6, %mm6                      
	movq 1760(%rsp), %mm7                         	# c.pQPb
	paddusb %mm7, %mm7                   
	psubusb %mm4, %mm7                   
	pcmpeqb %mm6, %mm7                   
	pcmpeqb %mm6, %mm7                   
	movq %mm7, 304(%rsp)                         	# dc_mask
	movq 2896(%rsp), %mm7                         	# c.ppMode.flatnessThreshold
	punpcklbw %mm7, %mm7                 
	punpcklbw %mm7, %mm7                 
	punpcklbw %mm7, %mm7                 
	psubb %mm0, %mm6                     
	pcmpgtb %mm7, %mm6                   
	movq %mm6, 312(%rsp)                         	# eq_mask
	
# 0 "" 2
#NO_APP
	movq	312(%rsp), %rcx	# eq_mask, D.15670
	movq	%rcx, %rax	# D.15670, D.15670
	andq	304(%rsp), %rax	# dc_mask, D.15670
	movq	%rax, 320(%rsp)	# D.15670, both_masks
	testq	%rax, %rax	# D.15670
	je	.L861	#,
	leaq	368(%rsp), %rax	#, tmp951
	movq	%rdx, %rsi	# src, src
#APP
# 2664 "postprocess_template.c" 1
	movq 1760(%rsp), %mm0                         	# c.pQPb
	pxor %mm4, %mm4                      
	movq (%rdx), %mm6                       	# src
	movq (%rdx, %rbp), %mm5                   	# src, D.15670
	movq %mm5, %mm1                      
	movq %mm6, %mm2                      
	psubusb %mm6, %mm5                   
	psubusb %mm1, %mm2                   
	por %mm5, %mm2                       
	psubusb %mm2, %mm0                   
	pcmpeqb %mm4, %mm0                   
	pxor %mm6, %mm1                      
	pand %mm0, %mm1                      
	pxor %mm1, %mm6                      
	movq (%rdx, %rbp, 8), %mm5                	# src, D.15670
	add %rbp, %rdx                             	# D.15670, src
	movq (%rdx, %rbp, 8), %mm7                	# src, D.15670
	movq %mm5, %mm1                      
	movq %mm7, %mm2                      
	psubusb %mm7, %mm5                   
	psubusb %mm1, %mm2                   
	por %mm5, %mm2                       
	movq 1760(%rsp), %mm0                         	# c.pQPb
	psubusb %mm2, %mm0                   
	pcmpeqb %mm4, %mm0                   
	pxor %mm7, %mm1                      
	pand %mm0, %mm1                      
	pxor %mm1, %mm7                      
	movq %mm6, %mm5                      
	punpckhbw %mm4, %mm6                 
	punpcklbw %mm4, %mm5                 
	movq %mm5, %mm0                      
	movq %mm6, %mm1                      
	psllw $2, %mm0                        
	psllw $2, %mm1                        
	paddw w04, %mm0             
	paddw w04, %mm1             
	movq (%rdx), %mm2                       	# src
	movq (%rdx), %mm3                       	# src
	add %rbp, %rdx                             	# D.15670, src
	punpcklbw %mm4, %mm2                 
	punpckhbw %mm4, %mm3                 
	paddw %mm2, %mm0                     
	paddw %mm3, %mm1                     
	movq (%rdx), %mm2                       	# src
	movq (%rdx), %mm3                       	# src
	add %rbp, %rdx                             	# D.15670, src
	punpcklbw %mm4, %mm2                 
	punpckhbw %mm4, %mm3                 
	paddw %mm2, %mm0                     
	paddw %mm3, %mm1                     
	movq (%rdx), %mm2                       	# src
	movq (%rdx), %mm3                       	# src
	add %rbp, %rdx                             	# D.15670, src
	punpcklbw %mm4, %mm2                 
	punpckhbw %mm4, %mm3                 
	paddw %mm2, %mm0                     
	paddw %mm3, %mm1                     
	movq %mm0, (%rax)                       	# tmp951
	movq %mm1, 8(%rax)                      	# tmp951
	movq (%rdx), %mm2                       	# src
	movq (%rdx), %mm3                       	# src
	add %rbp, %rdx                             	# D.15670, src
	punpcklbw %mm4, %mm2                 
	punpckhbw %mm4, %mm3                 
	paddw %mm2, %mm0                     
	paddw %mm3, %mm1                     
	psubw %mm5, %mm0                     
	psubw %mm6, %mm1                     
	movq %mm0, 16(%rax)                     	# tmp951
	movq %mm1, 24(%rax)                     	# tmp951
	movq (%rdx), %mm2                       	# src
	movq (%rdx), %mm3                       	# src
	add %rbp, %rdx                             	# D.15670, src
	punpcklbw %mm4, %mm2                 
	punpckhbw %mm4, %mm3                 
	paddw %mm2, %mm0                     
	paddw %mm3, %mm1                     
	psubw %mm5, %mm0                     
	psubw %mm6, %mm1                     
	movq %mm0, 32(%rax)                     	# tmp951
	movq %mm1, 40(%rax)                     	# tmp951
	movq (%rdx), %mm2                       	# src
	movq (%rdx), %mm3                       	# src
	add %rbp, %rdx                             	# D.15670, src
	punpcklbw %mm4, %mm2                 
	punpckhbw %mm4, %mm3                 
	paddw %mm2, %mm0                     
	paddw %mm3, %mm1                     
	psubw %mm5, %mm0                     
	psubw %mm6, %mm1                     
	movq %mm0, 48(%rax)                     	# tmp951
	movq %mm1, 56(%rax)                     	# tmp951
	movq (%rdx), %mm2                       	# src
	movq (%rdx), %mm3                       	# src
	add %rbp, %rdx                             	# D.15670, src
	punpcklbw %mm4, %mm2                 
	punpckhbw %mm4, %mm3                 
	paddw %mm2, %mm0                     
	paddw %mm3, %mm1                     
	psubw %mm5, %mm0                     
	psubw %mm6, %mm1                     
	movq %mm0, 64(%rax)                     	# tmp951
	movq %mm1, 72(%rax)                     	# tmp951
	movq %mm7, %mm6                      
	punpckhbw %mm4, %mm7                 
	punpcklbw %mm4, %mm6                 
	movq (%rdx), %mm2                       	# src
	movq (%rdx), %mm3                       	# src
	add %rbp, %rdx                             	# D.15670, src
	punpcklbw %mm4, %mm2                 
	punpckhbw %mm4, %mm3                 
	paddw %mm2, %mm0                     
	paddw %mm3, %mm1                     
	mov %rsi, %rdx                             	# src, src
	add %rbp, %rdx                             	# D.15670, src
	movq (%rdx), %mm2                       	# src
	movq (%rdx), %mm3                       	# src
	add %rbp, %rdx                             	# D.15670, src
	punpcklbw %mm4, %mm2                 
	punpckhbw %mm4, %mm3                 
	psubw %mm2, %mm0                     
	psubw %mm3, %mm1                     
	movq %mm0, 80(%rax)                     	# tmp951
	movq %mm1, 88(%rax)                     	# tmp951
	movq (%rdx), %mm2                       	# src
	movq (%rdx), %mm3                       	# src
	add %rbp, %rdx                             	# D.15670, src
	punpcklbw %mm4, %mm2                 
	punpckhbw %mm4, %mm3                 
	psubw %mm2, %mm0                     
	psubw %mm3, %mm1                     
	paddw %mm6, %mm0                     
	paddw %mm7, %mm1                     
	movq %mm0, 96(%rax)                     	# tmp951
	movq %mm1, 104(%rax)                    	# tmp951
	movq (%rdx), %mm2                       	# src
	movq (%rdx), %mm3                       	# src
	add %rbp, %rdx                             	# D.15670, src
	punpcklbw %mm4, %mm2                 
	punpckhbw %mm4, %mm3                 
	psubw %mm2, %mm0                     
	psubw %mm3, %mm1                     
	paddw %mm6, %mm0                     
	paddw %mm7, %mm1                     
	movq %mm0, 112(%rax)                    	# tmp951
	movq %mm1, 120(%rax)                    	# tmp951
	movq (%rdx), %mm2                       	# src
	movq (%rdx), %mm3                       	# src
	add %rbp, %rdx                             	# D.15670, src
	punpcklbw %mm4, %mm2                 
	punpckhbw %mm4, %mm3                 
	psubw %mm2, %mm0                     
	psubw %mm3, %mm1                     
	paddw %mm6, %mm0                     
	paddw %mm7, %mm1                     
	movq %mm0, 128(%rax)                    	# tmp951
	movq %mm1, 136(%rax)                    	# tmp951
	movq (%rdx), %mm2                       	# src
	movq (%rdx), %mm3                       	# src
	add %rbp, %rdx                             	# D.15670, src
	punpcklbw %mm4, %mm2                 
	punpckhbw %mm4, %mm3                 
	psubw %mm2, %mm0                     
	psubw %mm3, %mm1                     
	paddw %mm6, %mm0                     
	paddw %mm7, %mm1                     
	movq %mm0, 144(%rax)                    	# tmp951
	movq %mm1, 152(%rax)                    	# tmp951
	mov %rsi, %rdx                             	# src, src
	
# 0 "" 2
#NO_APP
	addq	88(%rsp), %rdx	# %sfp, src
	movq	184(%rsp), %rsi	# %sfp, D.15662
	leaq	(%rdx,%rsi), %rdi	#, D.15659
	movq	176(%rsp), %rsi	# %sfp, offset
#APP
# 2804 "postprocess_template.c" 1
	movq 320(%rsp), %mm6                         	# both_masks
	pcmpeqb %mm5, %mm5                   
	pxor %mm6, %mm5                      
	pxor %mm7, %mm7                      
	1:                                     
	movq (%rax), %mm0                       	# temp_sums
	movq 8(%rax), %mm1                      	# temp_sums
	paddw 32(%rax), %mm0                    	# temp_sums
	paddw 40(%rax), %mm1                    	# temp_sums
	movq (%rsi, %rdi), %mm2                   	# offset, D.15659
	movq %mm2, %mm3                      
	movq %mm2, %mm4                      
	punpcklbw %mm7, %mm2                 
	punpckhbw %mm7, %mm3                 
	paddw %mm2, %mm0                     
	paddw %mm3, %mm1                     
	paddw %mm2, %mm0                     
	paddw %mm3, %mm1                     
	psrlw $4, %mm0                        
	psrlw $4, %mm1                        
	packuswb %mm1, %mm0                  
	pand %mm6, %mm0                      
	pand %mm5, %mm4                      
	por %mm4, %mm0                       
	movq %mm0, (%rsi, %rdi)                   	# offset, D.15659
	add $16, %rax                            	# temp_sums
	add %rbp, %rsi                             	# D.15670, offset
	 js 1b                                 
	
# 0 "" 2
#NO_APP
	jmp	.L862	#
.L861:
	movq	88(%rsp), %rdx	# %sfp, D.15660
	addq	96(%rsp), %rdx	# %sfp, D.15660
	addq	%rbx, %rdx	# dstBlock, src
.L862:
	cmpq	$-1, %rcx	#, D.15670
	je	.L857	#,
	leaq	336(%rsp), %rcx	#, tmp958
#APP
# 2844 "postprocess_template.c" 1
	pxor %mm7, %mm7                      
	movq (%rdx), %mm0                       	# temp_src
	movq %mm0, %mm1                      
	punpcklbw %mm7, %mm0                 
	punpckhbw %mm7, %mm1                 
	movq (%rdx, %rbp), %mm2                   	# temp_src, D.15670
	lea (%rdx, %rbp, 2), %rax             	# temp_src, D.15670
	movq %mm2, %mm3                      
	punpcklbw %mm7, %mm2                 
	punpckhbw %mm7, %mm3                 
	movq (%rax), %mm4                
	movq %mm4, %mm5                      
	punpcklbw %mm7, %mm4                 
	punpckhbw %mm7, %mm5                 
	paddw %mm0, %mm0                     
	paddw %mm1, %mm1                     
	psubw %mm4, %mm2                     
	psubw %mm5, %mm3                     
	psubw %mm2, %mm0                     
	psubw %mm3, %mm1                     
	psllw $2, %mm2                        
	psllw $2, %mm3                        
	psubw %mm2, %mm0                     
	psubw %mm3, %mm1                     
	movq (%rax, %rbp), %mm2            	# D.15670
	movq %mm2, %mm3                      
	punpcklbw %mm7, %mm2                 
	punpckhbw %mm7, %mm3                 
	psubw %mm2, %mm0                     
	psubw %mm3, %mm1                     
	psubw %mm2, %mm0                     
	psubw %mm3, %mm1                     
	movq %mm0, (%rcx)                       	# tmp958
	movq %mm1, 8(%rcx)                      	# tmp958
	movq (%rax, %rbp, 2), %mm0         	# D.15670
	movq %mm0, %mm1                      
	punpcklbw %mm7, %mm0                 
	punpckhbw %mm7, %mm1                 
	psubw %mm0, %mm2                     
	psubw %mm1, %mm3                     
	movq %mm2, 16(%rcx)                     	# tmp958
	movq %mm3, 24(%rcx)                     	# tmp958
	paddw %mm4, %mm4                     
	paddw %mm5, %mm5                     
	psubw %mm2, %mm4                     
	psubw %mm3, %mm5                     
	lea (%rax, %rbp), %rdx                	# D.15670, temp_src
	psllw $2, %mm2                        
	psllw $2, %mm3                        
	psubw %mm2, %mm4                     
	psubw %mm3, %mm5                     
	movq (%rdx, %rbp, 2), %mm2                	# temp_src, D.15670
	movq %mm2, %mm3                      
	punpcklbw %mm7, %mm2                 
	punpckhbw %mm7, %mm3                 
	psubw %mm2, %mm4                     
	psubw %mm3, %mm5                     
	psubw %mm2, %mm4                     
	psubw %mm3, %mm5                     
	movq (%rax, %rbp, 4), %mm6         	# D.15670
	punpcklbw %mm7, %mm6                 
	psubw %mm6, %mm2                     
	movq (%rax, %rbp, 4), %mm6         	# D.15670
	punpckhbw %mm7, %mm6                 
	psubw %mm6, %mm3                     
	paddw %mm0, %mm0                     
	paddw %mm1, %mm1                     
	psubw %mm2, %mm0                     
	psubw %mm3, %mm1                     
	psllw $2, %mm2                        
	psllw $2, %mm3                        
	psubw %mm2, %mm0                     
	psubw %mm3, %mm1                     
	movq (%rdx, %rbp, 4), %mm2                	# temp_src, D.15670
	movq %mm2, %mm3                      
	punpcklbw %mm7, %mm2                 
	punpckhbw %mm7, %mm3                 
	paddw %mm2, %mm2                     
	paddw %mm3, %mm3                     
	psubw %mm2, %mm0                     
	psubw %mm3, %mm1                     
	movq (%rcx), %mm2                       	# tmp958
	movq 8(%rcx), %mm3                      	# tmp958
	movq %mm7, %mm6                      
	psubw %mm0, %mm6                     
	pmaxsw %mm6, %mm0                    
	movq %mm7, %mm6                      
	psubw %mm1, %mm6                     
	pmaxsw %mm6, %mm1                    
	movq %mm7, %mm6                      
	psubw %mm2, %mm6                     
	pmaxsw %mm6, %mm2                    
	movq %mm7, %mm6                      
	psubw %mm3, %mm6                     
	pmaxsw %mm6, %mm3                    
	pminsw %mm2, %mm0                    
	pminsw %mm3, %mm1                    
	movd 1760(%rsp), %mm2                         	# c.pQPb
	punpcklbw %mm7, %mm2                 
	movq %mm7, %mm6                      
	pcmpgtw %mm4, %mm6                   
	pxor %mm6, %mm4                      
	psubw %mm6, %mm4                     
	pcmpgtw %mm5, %mm7                   
	pxor %mm7, %mm5                      
	psubw %mm7, %mm5                     
	psllw $3, %mm2                        
	movq %mm2, %mm3                      
	pcmpgtw %mm4, %mm2                   
	pcmpgtw %mm5, %mm3                   
	pand %mm2, %mm4                      
	pand %mm3, %mm5                      
	psubusw %mm0, %mm4                   
	psubusw %mm1, %mm5                   
	movq w05, %mm2              
	pmullw %mm2, %mm4                    
	pmullw %mm2, %mm5                    
	movq w20, %mm2              
	paddw %mm2, %mm4                     
	paddw %mm2, %mm5                     
	psrlw $6, %mm4                        
	psrlw $6, %mm5                        
	movq 16(%rcx), %mm0                     	# tmp958
	movq 24(%rcx), %mm1                     	# tmp958
	pxor %mm2, %mm2                      
	pxor %mm3, %mm3                      
	pcmpgtw %mm0, %mm2                   
	pcmpgtw %mm1, %mm3                   
	pxor %mm2, %mm0                      
	pxor %mm3, %mm1                      
	psubw %mm2, %mm0                     
	psubw %mm3, %mm1                     
	psrlw $1, %mm0                        
	psrlw $1, %mm1                        
	pxor %mm6, %mm2                      
	pxor %mm7, %mm3                      
	pand %mm2, %mm4                      
	pand %mm3, %mm5                      
	pminsw %mm0, %mm4                    
	pminsw %mm1, %mm5                    
	pxor %mm6, %mm4                      
	pxor %mm7, %mm5                      
	psubw %mm6, %mm4                     
	psubw %mm7, %mm5                     
	packsswb %mm5, %mm4                  
	movq 312(%rsp), %mm1                         	# eq_mask
	pandn %mm4, %mm1                     
	movq (%rdx), %mm0                       	# temp_src
	paddb   %mm1, %mm0                   
	movq %mm0, (%rdx)                       	# temp_src
	movq (%rdx, %rbp), %mm0                   	# temp_src, D.15670
	psubb %mm1, %mm0                     
	movq %mm0, (%rdx, %rbp)                   	# temp_src, D.15670
	
# 0 "" 2
#NO_APP
.L857:
	movq	(%rsp), %rdi	# %sfp, tempBlock2
#APP
# 1995 "postprocess_template.c" 1
	lea (%rbx, %rbp), %rax                	# dstBlock, D.15670
	movq (%rbx), %mm0                       	# dstBlock
	movq (%rax), %mm1                
	movq %mm0, %mm2                      
	punpcklbw %mm1, %mm0                 
	punpckhbw %mm1, %mm2                 
	movq (%rax, %rbp), %mm1            	# D.15670
	movq (%rax, %rbp, 2), %mm3         	# D.15670
	movq %mm1, %mm4                      
	punpcklbw %mm3, %mm1                 
	punpckhbw %mm3, %mm4                 
	movq %mm0, %mm3                      
	punpcklwd %mm1, %mm0                 
	punpckhwd %mm1, %mm3                 
	movq %mm2, %mm1                      
	punpcklwd %mm4, %mm2                 
	punpckhwd %mm4, %mm1                 
	movd %mm0, 128(%r13)                    	# tempBlock2
	psrlq $32, %mm0                       
	movd %mm0, 144(%r13)                    	# tempBlock2
	movd %mm3, 160(%r13)                    	# tempBlock2
	psrlq $32, %mm3                       
	movd %mm3, 176(%r13)                    	# tempBlock2
	movd %mm3, 48(%rdi)                     	# tempBlock2
	movd %mm2, 192(%r13)                    	# tempBlock2
	movd %mm2, 64(%rdi)                     	# tempBlock2
	psrlq $32, %mm2                       
	movd %mm2, 80(%rdi)                     	# tempBlock2
	movd %mm1, 96(%rdi)                     	# tempBlock2
	psrlq $32, %mm1                       
	movd %mm1, 112(%rdi)                    	# tempBlock2
	lea (%rax, %rbp, 4), %rax      	# D.15670
	movq (%rbx, %rbp, 4), %mm0                	# dstBlock, D.15670
	movq (%rax), %mm1                
	movq %mm0, %mm2                      
	punpcklbw %mm1, %mm0                 
	punpckhbw %mm1, %mm2                 
	movq (%rax, %rbp), %mm1            	# D.15670
	movq (%rax, %rbp, 2), %mm3         	# D.15670
	movq %mm1, %mm4                      
	punpcklbw %mm3, %mm1                 
	punpckhbw %mm3, %mm4                 
	movq %mm0, %mm3                      
	punpcklwd %mm1, %mm0                 
	punpckhwd %mm1, %mm3                 
	movq %mm2, %mm1                      
	punpcklwd %mm4, %mm2                 
	punpckhwd %mm4, %mm1                 
	movd %mm0, 132(%r13)                    	# tempBlock2
	psrlq $32, %mm0                       
	movd %mm0, 148(%r13)                    	# tempBlock2
	movd %mm3, 164(%r13)                    	# tempBlock2
	psrlq $32, %mm3                       
	movd %mm3, 180(%r13)                    	# tempBlock2
	movd %mm3, 52(%rdi)                     	# tempBlock2
	movd %mm2, 196(%r13)                    	# tempBlock2
	movd %mm2, 68(%rdi)                     	# tempBlock2
	psrlq $32, %mm2                       
	movd %mm2, 84(%rdi)                     	# tempBlock2
	movd %mm1, 100(%rdi)                    	# tempBlock2
	psrlq $32, %mm1                       
	movd %mm1, 116(%rdi)                    	# tempBlock2
	
# 0 "" 2
#NO_APP
	movl	%r12d, %eax	# x, tmp1149
	subl	$8, %eax	#, tmp1149
	js	.L864	#,
	testl	$8192, %r15d	#, D.15658
	je	.L865	#,
	leaq	48(%r13), %rdx	#, src
	movl	$16, %esi	#, tmp962
#APP
# 412 "postprocess_template.c" 1
	pxor %mm7, %mm7                      
	lea (%rdx, %rsi), %rax                	# src, tmp962
	lea (%rax, %rsi, 4), %rcx      	# tmp962
	movq (%rax, %rsi, 2), %mm0         	# tmp962
	movq (%rdx, %rsi, 4), %mm1                	# src, tmp962
	movq %mm1, %mm2                      
	psubusb %mm0, %mm1                   
	psubusb %mm2, %mm0                   
	por %mm1, %mm0                       
	movq (%rcx), %mm3                
	movq (%rcx, %rsi), %mm4            	# tmp962
	movq %mm3, %mm5                      
	psubusb %mm4, %mm3                   
	psubusb %mm5, %mm4                   
	por %mm4, %mm3                       
	pavgb %mm3, %mm0 
	movq %mm2, %mm1                      
	psubusb %mm5, %mm2                   
	movq %mm2, %mm4                      
	pcmpeqb %mm7, %mm2                   
	psubusb %mm1, %mm5                   
	por %mm5, %mm4                       
	psubusb %mm0, %mm4                   
	movq %mm4, %mm3                      
	movq 1760(%rsp), %mm0                         	# c.pQPb
	paddusb %mm0, %mm0                   
	psubusb %mm0, %mm4                   
	pcmpeqb %mm7, %mm4                   
	psubusb b01, %mm3           
	pand %mm4, %mm3                      
	pavgb %mm7, %mm3 
	movq %mm3, %mm1                      
	pavgb %mm7, %mm3 
	pavgb %mm1, %mm3 
	movq (%rdx, %rsi, 4), %mm0                	# src, tmp962
	pxor %mm2, %mm0                      
	psubusb %mm3, %mm0                   
	pxor %mm2, %mm0                      
	movq %mm0, (%rdx, %rsi, 4)                	# src, tmp962
	movq (%rcx), %mm0                
	pxor %mm2, %mm0                      
	paddusb %mm3, %mm0                   
	pxor %mm2, %mm0                      
	movq %mm0, (%rcx)                
	pavgb %mm7, %mm1 
	movq (%rax, %rsi, 2), %mm0         	# tmp962
	pxor %mm2, %mm0                      
	psubusb %mm1, %mm0                   
	pxor %mm2, %mm0                      
	movq %mm0, (%rax, %rsi, 2)         	# tmp962
	movq (%rcx, %rsi), %mm0            	# tmp962
	pxor %mm2, %mm0                      
	paddusb %mm1, %mm0                   
	pxor %mm2, %mm0                      
	movq %mm0, (%rcx, %rsi)            	# tmp962
	pavgb %mm7, %mm1 
	movq (%rax, %rsi), %mm0            	# tmp962
	pxor %mm2, %mm0                      
	psubusb %mm1, %mm0                   
	pxor %mm2, %mm0                      
	movq %mm0, (%rax, %rsi)            	# tmp962
	movq (%rcx, %rsi, 2), %mm0         	# tmp962
	pxor %mm2, %mm0                      
	paddusb %mm1, %mm0                   
	pxor %mm2, %mm0                      
	movq %mm0, (%rcx, %rsi, 2)         	# tmp962
	
# 0 "" 2
#NO_APP
	jmp	.L866	#
.L865:
	testb	$2, %r15b	#, D.15658
	je	.L867	#,
	leaq	64(%r13), %rsi	#, src
	movslq	2828(%rsp), %rax	# c.nonBQP, D.15658
#APP
# 114 "postprocess_template.c" 1
	movq 1776(%rsp,%rax,8), %mm7                         	# c.mmxDcOffset
	movq 2288(%rsp,%rax,8), %mm6                         	# c.mmxDcThreshold
	
# 0 "" 2
#NO_APP
	movl	$16, %edx	#, tmp970
#APP
# 120 "postprocess_template.c" 1
	lea (%rsi, %rdx), %rax                	# src, tmp970
	movq (%rsi), %mm0                       	# src
	movq (%rax), %mm1                
	movq %mm0, %mm3                      
	movq %mm0, %mm4                      
	pmaxub %mm1, %mm4 
	pminub %mm1, %mm3 
	psubb %mm1, %mm0                     
	paddb %mm7, %mm0                     
	pcmpgtb %mm6, %mm0                   
	movq (%rax,%rdx), %mm2             	# tmp970
	pmaxub %mm2, %mm4 
	pminub %mm2, %mm3 
	psubb %mm2, %mm1                     
	paddb %mm7, %mm1                     
	pcmpgtb %mm6, %mm1                   
	paddb %mm1, %mm0                     
	movq (%rax, %rdx, 2), %mm1         	# tmp970
	pmaxub %mm1, %mm4 
	pminub %mm1, %mm3 
	psubb %mm1, %mm2                     
	paddb %mm7, %mm2                     
	pcmpgtb %mm6, %mm2                   
	paddb %mm2, %mm0                     
	lea (%rax, %rdx, 4), %rax      	# tmp970
	movq (%rsi, %rdx, 4), %mm2                	# src, tmp970
	pmaxub %mm2, %mm4 
	pminub %mm2, %mm3 
	psubb %mm2, %mm1                     
	paddb %mm7, %mm1                     
	pcmpgtb %mm6, %mm1                   
	paddb %mm1, %mm0                     
	movq (%rax), %mm1                
	pmaxub %mm1, %mm4 
	pminub %mm1, %mm3 
	psubb %mm1, %mm2                     
	paddb %mm7, %mm2                     
	pcmpgtb %mm6, %mm2                   
	paddb %mm2, %mm0                     
	movq (%rax, %rdx), %mm2            	# tmp970
	pmaxub %mm2, %mm4 
	pminub %mm2, %mm3 
	psubb %mm2, %mm1                     
	paddb %mm7, %mm1                     
	pcmpgtb %mm6, %mm1                   
	paddb %mm1, %mm0                     
	movq (%rax, %rdx, 2), %mm1         	# tmp970
	pmaxub %mm1, %mm4 
	pminub %mm1, %mm3 
	psubb %mm1, %mm2                     
	paddb %mm7, %mm2                     
	pcmpgtb %mm6, %mm2                   
	paddb %mm2, %mm0                     
	psubusb %mm3, %mm4                   
	                                       
	pxor %mm7, %mm7                      
	psadbw %mm7, %mm0                    
	movq 1760(%rsp), %mm7                         	# c.pQPb
	paddusb %mm7, %mm7                   
	psubusb %mm7, %mm4                   
	packssdw %mm4, %mm4                  
	movd %mm0, %edx                         	# numEq
	movd %mm4, %ecx                         	# dcOk
	
# 0 "" 2
#NO_APP
	negl	%edx	# D.15658
	movzbl	%dl, %eax	# D.15658, numEq
	cmpl	2896(%rsp), %eax	# c.ppMode.flatnessThreshold, numEq
	jle	.L868	#,
	testl	%ecx, %ecx	# dcOk
	jne	.L866	#,
	leaq	1648(%rsp), %rdx	#, tmp1363
	movl	$16, %esi	#,
	movq	%r13, %rdi	# tempBlock2,
	call	doVertLowPass_MMX2	#
	jmp	.L866	#
.L868:
	movl	$16, %edx	#, tmp973
#APP
# 552 "postprocess_template.c" 1
	lea (%rsi, %rdx), %rax                	# src, tmp973
	pcmpeqb %mm6, %mm6                   
	movq (%rax, %rdx, 2), %mm1         	# tmp973
	movq (%rsi, %rdx, 4), %mm0                	# src, tmp973
	pxor %mm6, %mm1                      
	pavgb %mm1, %mm0 
	movq (%rax, %rdx, 4), %mm2         	# tmp973
	movq (%rax, %rdx), %mm3            	# tmp973
	pxor %mm6, %mm2                      
	movq %mm2, %mm5                      
	movq b80, %mm4              
	lea (%rax, %rdx, 4), %rcx      	# tmp973
	pavgb %mm3, %mm2 
	pavgb %mm0, %mm4 
	pavgb %mm2, %mm4 
	pavgb %mm0, %mm4 
	movq (%rax), %mm2                
	pxor %mm6, %mm2                      
	pavgb %mm3, %mm2 
	pavgb (%rsi), %mm1 	# src
	movq b80, %mm3              
	pavgb %mm2, %mm3 
	pavgb %mm1, %mm3 
	pavgb %mm2, %mm3 
	pavgb (%rcx, %rdx), %mm5 	# tmp973
	movq (%rcx, %rdx, 2), %mm1         	# tmp973
	pxor %mm6, %mm1                      
	pavgb (%rsi, %rdx, 4), %mm1 	# src, tmp973
	movq b80, %mm2              
	pavgb %mm5, %mm2 
	pavgb %mm1, %mm2 
	pavgb %mm5, %mm2 
	movq b00, %mm1              
	movq b00, %mm5              
	psubb %mm2, %mm1                     
	psubb %mm3, %mm5                     
	pmaxub %mm1, %mm2 
	pmaxub %mm5, %mm3 
	pminub %mm2, %mm3 
	movq b00, %mm7              
	movq 1760(%rsp), %mm2                         	# c.pQPb
	pavgb %mm6, %mm2 
	psubb %mm6, %mm2                     
	movq %mm4, %mm1                      
	pcmpgtb %mm7, %mm1                   
	pxor %mm1, %mm4                      
	psubb %mm1, %mm4                     
	pcmpgtb %mm4, %mm2                   
	psubusb %mm3, %mm4                   
	movq %mm4, %mm3                      
	psubusb b01, %mm4           
	pavgb %mm7, %mm4 
	pavgb %mm7, %mm4 
	paddb %mm3, %mm4                     
	pand %mm2, %mm4                      
	movq b80, %mm5              
	psubb %mm0, %mm5                     
	paddsb %mm6, %mm5                    
	pcmpgtb %mm5, %mm7                   
	pxor %mm7, %mm5                      
	pminub %mm5, %mm4 
	pxor %mm1, %mm7                      
	pand %mm7, %mm4                      
	movq (%rax, %rdx, 2), %mm0         	# tmp973
	movq (%rsi, %rdx, 4), %mm2                	# src, tmp973
	pxor %mm1, %mm0                      
	pxor %mm1, %mm2                      
	paddb %mm4, %mm0                     
	psubb %mm4, %mm2                     
	pxor %mm1, %mm0                      
	pxor %mm1, %mm2                      
	movq %mm0, (%rax, %rdx, 2)         	# tmp973
	movq %mm2, (%rsi, %rdx, 4)                	# src, tmp973
	
# 0 "" 2
#NO_APP
	jmp	.L866	#
.L867:
	testl	$16384, %r15d	#, D.15658
	je	.L866	#,
	leaq	48(%r13), %rdx	#, src
	movslq	2828(%rsp), %rax	# c.nonBQP, D.15658
#APP
# 2553 "postprocess_template.c" 1
	movq 1776(%rsp,%rax,8), %mm7                         	# c.mmxDcOffset
	movq 2288(%rsp,%rax,8), %mm6                         	# c.mmxDcThreshold
	
# 0 "" 2
#NO_APP
	movl	$16, %ecx	#, tmp979
#APP
# 2559 "postprocess_template.c" 1
	lea (%rdx, %rcx), %rax                	# src, tmp979
	movq (%rdx), %mm0                       	# src
	movq (%rax), %mm1                
	movq %mm1, %mm3                      
	movq %mm1, %mm4                      
	psubb %mm1, %mm0                     
	paddb %mm7, %mm0                     
	pcmpgtb %mm6, %mm0                   
	movq (%rax,%rcx), %mm2             	# tmp979
	pmaxub %mm2, %mm4 
	pminub %mm2, %mm3 
	psubb %mm2, %mm1                     
	paddb %mm7, %mm1                     
	pcmpgtb %mm6, %mm1                   
	paddb %mm1, %mm0                     
	movq (%rax, %rcx, 2), %mm1         	# tmp979
	pmaxub %mm1, %mm4 
	pminub %mm1, %mm3 
	psubb %mm1, %mm2                     
	paddb %mm7, %mm2                     
	pcmpgtb %mm6, %mm2                   
	paddb %mm2, %mm0                     
	lea (%rax, %rcx, 4), %rax      	# tmp979
	movq (%rdx, %rcx, 4), %mm2                	# src, tmp979
	pmaxub %mm2, %mm4 
	pminub %mm2, %mm3 
	psubb %mm2, %mm1                     
	paddb %mm7, %mm1                     
	pcmpgtb %mm6, %mm1                   
	paddb %mm1, %mm0                     
	movq (%rax), %mm1                
	pmaxub %mm1, %mm4 
	pminub %mm1, %mm3 
	psubb %mm1, %mm2                     
	paddb %mm7, %mm2                     
	pcmpgtb %mm6, %mm2                   
	paddb %mm2, %mm0                     
	movq (%rax, %rcx), %mm2            	# tmp979
	pmaxub %mm2, %mm4 
	pminub %mm2, %mm3 
	psubb %mm2, %mm1                     
	paddb %mm7, %mm1                     
	pcmpgtb %mm6, %mm1                   
	paddb %mm1, %mm0                     
	movq (%rax, %rcx, 2), %mm1         	# tmp979
	pmaxub %mm1, %mm4 
	pminub %mm1, %mm3 
	psubb %mm1, %mm2                     
	paddb %mm7, %mm2                     
	pcmpgtb %mm6, %mm2                   
	paddb %mm2, %mm0                     
	movq (%rdx, %rcx, 8), %mm2                	# src, tmp979
	pmaxub %mm2, %mm4 
	pminub %mm2, %mm3 
	psubb %mm2, %mm1                     
	paddb %mm7, %mm1                     
	pcmpgtb %mm6, %mm1                   
	paddb %mm1, %mm0                     
	movq (%rax, %rcx, 4), %mm1         	# tmp979
	psubb %mm1, %mm2                     
	paddb %mm7, %mm2                     
	pcmpgtb %mm6, %mm2                   
	paddb %mm2, %mm0                     
	psubusb %mm3, %mm4                   
	pxor %mm6, %mm6                      
	movq 1760(%rsp), %mm7                         	# c.pQPb
	paddusb %mm7, %mm7                   
	psubusb %mm4, %mm7                   
	pcmpeqb %mm6, %mm7                   
	pcmpeqb %mm6, %mm7                   
	movq %mm7, 304(%rsp)                         	# dc_mask
	movq 2896(%rsp), %mm7                         	# c.ppMode.flatnessThreshold
	punpcklbw %mm7, %mm7                 
	punpcklbw %mm7, %mm7                 
	punpcklbw %mm7, %mm7                 
	psubb %mm0, %mm6                     
	pcmpgtb %mm7, %mm6                   
	movq %mm6, 312(%rsp)                         	# eq_mask
	
# 0 "" 2
#NO_APP
	movq	312(%rsp), %rsi	# eq_mask, D.15670
	movq	%rsi, %rcx	# D.15670, D.15670
	andq	304(%rsp), %rcx	# dc_mask, D.15670
	movq	%rcx, 320(%rsp)	# D.15670, both_masks
	leaq	64(%r13), %rax	#, src
	testq	%rcx, %rcx	# D.15670
	je	.L870	#,
	movl	$16, %r8d	#, tmp981
	leaq	368(%rsp), %rcx	#, tmp982
	movq	%rdx, %rax	# src, src
#APP
# 2664 "postprocess_template.c" 1
	movq 1760(%rsp), %mm0                         	# c.pQPb
	pxor %mm4, %mm4                      
	movq (%rdx), %mm6                       	# src
	movq (%rdx, %r8), %mm5                   	# src, tmp981
	movq %mm5, %mm1                      
	movq %mm6, %mm2                      
	psubusb %mm6, %mm5                   
	psubusb %mm1, %mm2                   
	por %mm5, %mm2                       
	psubusb %mm2, %mm0                   
	pcmpeqb %mm4, %mm0                   
	pxor %mm6, %mm1                      
	pand %mm0, %mm1                      
	pxor %mm1, %mm6                      
	movq (%rdx, %r8, 8), %mm5                	# src, tmp981
	add %r8, %rdx                             	# tmp981, src
	movq (%rdx, %r8, 8), %mm7                	# src, tmp981
	movq %mm5, %mm1                      
	movq %mm7, %mm2                      
	psubusb %mm7, %mm5                   
	psubusb %mm1, %mm2                   
	por %mm5, %mm2                       
	movq 1760(%rsp), %mm0                         	# c.pQPb
	psubusb %mm2, %mm0                   
	pcmpeqb %mm4, %mm0                   
	pxor %mm7, %mm1                      
	pand %mm0, %mm1                      
	pxor %mm1, %mm7                      
	movq %mm6, %mm5                      
	punpckhbw %mm4, %mm6                 
	punpcklbw %mm4, %mm5                 
	movq %mm5, %mm0                      
	movq %mm6, %mm1                      
	psllw $2, %mm0                        
	psllw $2, %mm1                        
	paddw w04, %mm0             
	paddw w04, %mm1             
	movq (%rdx), %mm2                       	# src
	movq (%rdx), %mm3                       	# src
	add %r8, %rdx                             	# tmp981, src
	punpcklbw %mm4, %mm2                 
	punpckhbw %mm4, %mm3                 
	paddw %mm2, %mm0                     
	paddw %mm3, %mm1                     
	movq (%rdx), %mm2                       	# src
	movq (%rdx), %mm3                       	# src
	add %r8, %rdx                             	# tmp981, src
	punpcklbw %mm4, %mm2                 
	punpckhbw %mm4, %mm3                 
	paddw %mm2, %mm0                     
	paddw %mm3, %mm1                     
	movq (%rdx), %mm2                       	# src
	movq (%rdx), %mm3                       	# src
	add %r8, %rdx                             	# tmp981, src
	punpcklbw %mm4, %mm2                 
	punpckhbw %mm4, %mm3                 
	paddw %mm2, %mm0                     
	paddw %mm3, %mm1                     
	movq %mm0, (%rcx)                       	# tmp982
	movq %mm1, 8(%rcx)                      	# tmp982
	movq (%rdx), %mm2                       	# src
	movq (%rdx), %mm3                       	# src
	add %r8, %rdx                             	# tmp981, src
	punpcklbw %mm4, %mm2                 
	punpckhbw %mm4, %mm3                 
	paddw %mm2, %mm0                     
	paddw %mm3, %mm1                     
	psubw %mm5, %mm0                     
	psubw %mm6, %mm1                     
	movq %mm0, 16(%rcx)                     	# tmp982
	movq %mm1, 24(%rcx)                     	# tmp982
	movq (%rdx), %mm2                       	# src
	movq (%rdx), %mm3                       	# src
	add %r8, %rdx                             	# tmp981, src
	punpcklbw %mm4, %mm2                 
	punpckhbw %mm4, %mm3                 
	paddw %mm2, %mm0                     
	paddw %mm3, %mm1                     
	psubw %mm5, %mm0                     
	psubw %mm6, %mm1                     
	movq %mm0, 32(%rcx)                     	# tmp982
	movq %mm1, 40(%rcx)                     	# tmp982
	movq (%rdx), %mm2                       	# src
	movq (%rdx), %mm3                       	# src
	add %r8, %rdx                             	# tmp981, src
	punpcklbw %mm4, %mm2                 
	punpckhbw %mm4, %mm3                 
	paddw %mm2, %mm0                     
	paddw %mm3, %mm1                     
	psubw %mm5, %mm0                     
	psubw %mm6, %mm1                     
	movq %mm0, 48(%rcx)                     	# tmp982
	movq %mm1, 56(%rcx)                     	# tmp982
	movq (%rdx), %mm2                       	# src
	movq (%rdx), %mm3                       	# src
	add %r8, %rdx                             	# tmp981, src
	punpcklbw %mm4, %mm2                 
	punpckhbw %mm4, %mm3                 
	paddw %mm2, %mm0                     
	paddw %mm3, %mm1                     
	psubw %mm5, %mm0                     
	psubw %mm6, %mm1                     
	movq %mm0, 64(%rcx)                     	# tmp982
	movq %mm1, 72(%rcx)                     	# tmp982
	movq %mm7, %mm6                      
	punpckhbw %mm4, %mm7                 
	punpcklbw %mm4, %mm6                 
	movq (%rdx), %mm2                       	# src
	movq (%rdx), %mm3                       	# src
	add %r8, %rdx                             	# tmp981, src
	punpcklbw %mm4, %mm2                 
	punpckhbw %mm4, %mm3                 
	paddw %mm2, %mm0                     
	paddw %mm3, %mm1                     
	mov %rax, %rdx                             	# src, src
	add %r8, %rdx                             	# tmp981, src
	movq (%rdx), %mm2                       	# src
	movq (%rdx), %mm3                       	# src
	add %r8, %rdx                             	# tmp981, src
	punpcklbw %mm4, %mm2                 
	punpckhbw %mm4, %mm3                 
	psubw %mm2, %mm0                     
	psubw %mm3, %mm1                     
	movq %mm0, 80(%rcx)                     	# tmp982
	movq %mm1, 88(%rcx)                     	# tmp982
	movq (%rdx), %mm2                       	# src
	movq (%rdx), %mm3                       	# src
	add %r8, %rdx                             	# tmp981, src
	punpcklbw %mm4, %mm2                 
	punpckhbw %mm4, %mm3                 
	psubw %mm2, %mm0                     
	psubw %mm3, %mm1                     
	paddw %mm6, %mm0                     
	paddw %mm7, %mm1                     
	movq %mm0, 96(%rcx)                     	# tmp982
	movq %mm1, 104(%rcx)                    	# tmp982
	movq (%rdx), %mm2                       	# src
	movq (%rdx), %mm3                       	# src
	add %r8, %rdx                             	# tmp981, src
	punpcklbw %mm4, %mm2                 
	punpckhbw %mm4, %mm3                 
	psubw %mm2, %mm0                     
	psubw %mm3, %mm1                     
	paddw %mm6, %mm0                     
	paddw %mm7, %mm1                     
	movq %mm0, 112(%rcx)                    	# tmp982
	movq %mm1, 120(%rcx)                    	# tmp982
	movq (%rdx), %mm2                       	# src
	movq (%rdx), %mm3                       	# src
	add %r8, %rdx                             	# tmp981, src
	punpcklbw %mm4, %mm2                 
	punpckhbw %mm4, %mm3                 
	psubw %mm2, %mm0                     
	psubw %mm3, %mm1                     
	paddw %mm6, %mm0                     
	paddw %mm7, %mm1                     
	movq %mm0, 128(%rcx)                    	# tmp982
	movq %mm1, 136(%rcx)                    	# tmp982
	movq (%rdx), %mm2                       	# src
	movq (%rdx), %mm3                       	# src
	add %r8, %rdx                             	# tmp981, src
	punpcklbw %mm4, %mm2                 
	punpckhbw %mm4, %mm3                 
	psubw %mm2, %mm0                     
	psubw %mm3, %mm1                     
	paddw %mm6, %mm0                     
	paddw %mm7, %mm1                     
	movq %mm0, 144(%rcx)                    	# tmp982
	movq %mm1, 152(%rcx)                    	# tmp982
	mov %rax, %rdx                             	# src, src
	
# 0 "" 2
#NO_APP
	leaq	16(%rdx), %rax	#, src
	addq	$144, %rdx	#, D.15659
	movq	$-128, %rdi	#, offset
#APP
# 2804 "postprocess_template.c" 1
	movq 320(%rsp), %mm6                         	# both_masks
	pcmpeqb %mm5, %mm5                   
	pxor %mm6, %mm5                      
	pxor %mm7, %mm7                      
	1:                                     
	movq (%rcx), %mm0                       	# temp_sums
	movq 8(%rcx), %mm1                      	# temp_sums
	paddw 32(%rcx), %mm0                    	# temp_sums
	paddw 40(%rcx), %mm1                    	# temp_sums
	movq (%rdi, %rdx), %mm2                   	# offset, D.15659
	movq %mm2, %mm3                      
	movq %mm2, %mm4                      
	punpcklbw %mm7, %mm2                 
	punpckhbw %mm7, %mm3                 
	paddw %mm2, %mm0                     
	paddw %mm3, %mm1                     
	paddw %mm2, %mm0                     
	paddw %mm3, %mm1                     
	psrlw $4, %mm0                        
	psrlw $4, %mm1                        
	packuswb %mm1, %mm0                  
	pand %mm6, %mm0                      
	pand %mm5, %mm4                      
	por %mm4, %mm0                       
	movq %mm0, (%rdi, %rdx)                   	# offset, D.15659
	add $16, %rcx                            	# temp_sums
	add %r8, %rdi                             	# tmp981, offset
	 js 1b                                 
	
# 0 "" 2
#NO_APP
.L870:
	cmpq	$-1, %rsi	#, D.15670
	je	.L866	#,
	leaq	336(%rsp), %rsi	#, tmp991
	movq	%rax, %rdx	# src, temp_src
	movl	$16, %ecx	#, tmp990
#APP
# 2844 "postprocess_template.c" 1
	pxor %mm7, %mm7                      
	movq (%rdx), %mm0                       	# temp_src
	movq %mm0, %mm1                      
	punpcklbw %mm7, %mm0                 
	punpckhbw %mm7, %mm1                 
	movq (%rdx, %rcx), %mm2                   	# temp_src, tmp990
	lea (%rdx, %rcx, 2), %rax             	# temp_src, tmp990
	movq %mm2, %mm3                      
	punpcklbw %mm7, %mm2                 
	punpckhbw %mm7, %mm3                 
	movq (%rax), %mm4                
	movq %mm4, %mm5                      
	punpcklbw %mm7, %mm4                 
	punpckhbw %mm7, %mm5                 
	paddw %mm0, %mm0                     
	paddw %mm1, %mm1                     
	psubw %mm4, %mm2                     
	psubw %mm5, %mm3                     
	psubw %mm2, %mm0                     
	psubw %mm3, %mm1                     
	psllw $2, %mm2                        
	psllw $2, %mm3                        
	psubw %mm2, %mm0                     
	psubw %mm3, %mm1                     
	movq (%rax, %rcx), %mm2            	# tmp990
	movq %mm2, %mm3                      
	punpcklbw %mm7, %mm2                 
	punpckhbw %mm7, %mm3                 
	psubw %mm2, %mm0                     
	psubw %mm3, %mm1                     
	psubw %mm2, %mm0                     
	psubw %mm3, %mm1                     
	movq %mm0, (%rsi)                       	# tmp991
	movq %mm1, 8(%rsi)                      	# tmp991
	movq (%rax, %rcx, 2), %mm0         	# tmp990
	movq %mm0, %mm1                      
	punpcklbw %mm7, %mm0                 
	punpckhbw %mm7, %mm1                 
	psubw %mm0, %mm2                     
	psubw %mm1, %mm3                     
	movq %mm2, 16(%rsi)                     	# tmp991
	movq %mm3, 24(%rsi)                     	# tmp991
	paddw %mm4, %mm4                     
	paddw %mm5, %mm5                     
	psubw %mm2, %mm4                     
	psubw %mm3, %mm5                     
	lea (%rax, %rcx), %rdx                	# tmp990, temp_src
	psllw $2, %mm2                        
	psllw $2, %mm3                        
	psubw %mm2, %mm4                     
	psubw %mm3, %mm5                     
	movq (%rdx, %rcx, 2), %mm2                	# temp_src, tmp990
	movq %mm2, %mm3                      
	punpcklbw %mm7, %mm2                 
	punpckhbw %mm7, %mm3                 
	psubw %mm2, %mm4                     
	psubw %mm3, %mm5                     
	psubw %mm2, %mm4                     
	psubw %mm3, %mm5                     
	movq (%rax, %rcx, 4), %mm6         	# tmp990
	punpcklbw %mm7, %mm6                 
	psubw %mm6, %mm2                     
	movq (%rax, %rcx, 4), %mm6         	# tmp990
	punpckhbw %mm7, %mm6                 
	psubw %mm6, %mm3                     
	paddw %mm0, %mm0                     
	paddw %mm1, %mm1                     
	psubw %mm2, %mm0                     
	psubw %mm3, %mm1                     
	psllw $2, %mm2                        
	psllw $2, %mm3                        
	psubw %mm2, %mm0                     
	psubw %mm3, %mm1                     
	movq (%rdx, %rcx, 4), %mm2                	# temp_src, tmp990
	movq %mm2, %mm3                      
	punpcklbw %mm7, %mm2                 
	punpckhbw %mm7, %mm3                 
	paddw %mm2, %mm2                     
	paddw %mm3, %mm3                     
	psubw %mm2, %mm0                     
	psubw %mm3, %mm1                     
	movq (%rsi), %mm2                       	# tmp991
	movq 8(%rsi), %mm3                      	# tmp991
	movq %mm7, %mm6                      
	psubw %mm0, %mm6                     
	pmaxsw %mm6, %mm0                    
	movq %mm7, %mm6                      
	psubw %mm1, %mm6                     
	pmaxsw %mm6, %mm1                    
	movq %mm7, %mm6                      
	psubw %mm2, %mm6                     
	pmaxsw %mm6, %mm2                    
	movq %mm7, %mm6                      
	psubw %mm3, %mm6                     
	pmaxsw %mm6, %mm3                    
	pminsw %mm2, %mm0                    
	pminsw %mm3, %mm1                    
	movd 1760(%rsp), %mm2                         	# c.pQPb
	punpcklbw %mm7, %mm2                 
	movq %mm7, %mm6                      
	pcmpgtw %mm4, %mm6                   
	pxor %mm6, %mm4                      
	psubw %mm6, %mm4                     
	pcmpgtw %mm5, %mm7                   
	pxor %mm7, %mm5                      
	psubw %mm7, %mm5                     
	psllw $3, %mm2                        
	movq %mm2, %mm3                      
	pcmpgtw %mm4, %mm2                   
	pcmpgtw %mm5, %mm3                   
	pand %mm2, %mm4                      
	pand %mm3, %mm5                      
	psubusw %mm0, %mm4                   
	psubusw %mm1, %mm5                   
	movq w05, %mm2              
	pmullw %mm2, %mm4                    
	pmullw %mm2, %mm5                    
	movq w20, %mm2              
	paddw %mm2, %mm4                     
	paddw %mm2, %mm5                     
	psrlw $6, %mm4                        
	psrlw $6, %mm5                        
	movq 16(%rsi), %mm0                     	# tmp991
	movq 24(%rsi), %mm1                     	# tmp991
	pxor %mm2, %mm2                      
	pxor %mm3, %mm3                      
	pcmpgtw %mm0, %mm2                   
	pcmpgtw %mm1, %mm3                   
	pxor %mm2, %mm0                      
	pxor %mm3, %mm1                      
	psubw %mm2, %mm0                     
	psubw %mm3, %mm1                     
	psrlw $1, %mm0                        
	psrlw $1, %mm1                        
	pxor %mm6, %mm2                      
	pxor %mm7, %mm3                      
	pand %mm2, %mm4                      
	pand %mm3, %mm5                      
	pminsw %mm0, %mm4                    
	pminsw %mm1, %mm5                    
	pxor %mm6, %mm4                      
	pxor %mm7, %mm5                      
	psubw %mm6, %mm4                     
	psubw %mm7, %mm5                     
	packsswb %mm5, %mm4                  
	movq 312(%rsp), %mm1                         	# eq_mask
	pandn %mm4, %mm1                     
	movq (%rdx), %mm0                       	# temp_src
	paddb   %mm1, %mm0                   
	movq %mm0, (%rdx)                       	# temp_src
	movq (%rdx, %rcx), %mm0                   	# temp_src, tmp990
	psubb %mm1, %mm0                     
	movq %mm0, (%rdx, %rcx)                   	# temp_src, tmp990
	
# 0 "" 2
#NO_APP
.L866:
	leaq	-4(%rbx), %rcx	#, D.15659
	leaq	64(%r13), %rsi	#, D.15661
#APP
# 2080 "postprocess_template.c" 1
	lea (%rcx, %rbp), %rax                	# D.15659, D.15670
	lea (%rax,%rbp,4), %rdx        	# D.15670
	movq (%rsi), %mm0                       	# D.15661
	movq 16(%rsi), %mm1                     	# D.15661
	movq %mm0, %mm2                      
	punpcklbw %mm1, %mm0                 
	punpckhbw %mm1, %mm2                 
	movq 32(%rsi), %mm1                     	# D.15661
	movq 48(%rsi), %mm3                     	# D.15661
	movq %mm1, %mm4                      
	punpcklbw %mm3, %mm1                 
	punpckhbw %mm3, %mm4                 
	movq %mm0, %mm3                      
	punpcklwd %mm1, %mm0                 
	punpckhwd %mm1, %mm3                 
	movq %mm2, %mm1                      
	punpcklwd %mm4, %mm2                 
	punpckhwd %mm4, %mm1                 
	movd %mm0, (%rcx)                       	# D.15659
	psrlq $32, %mm0                       
	movd %mm0, (%rax)                
	movd %mm3, (%rax, %rbp)            	# D.15670
	psrlq $32, %mm3                       
	movd %mm3, (%rax, %rbp, 2)         	# D.15670
	movd %mm2, (%rcx, %rbp, 4)                	# D.15659, D.15670
	psrlq $32, %mm2                       
	movd %mm2, (%rdx)                
	movd %mm1, (%rdx, %rbp)            	# D.15670
	psrlq $32, %mm1                       
	movd %mm1, (%rdx, %rbp, 2)         	# D.15670
	movq 64(%rsi), %mm0                     	# D.15661
	movq 80(%rsi), %mm1                     	# D.15661
	movq %mm0, %mm2                      
	punpcklbw %mm1, %mm0                 
	punpckhbw %mm1, %mm2                 
	movq 96(%rsi), %mm1                     	# D.15661
	movq 112(%rsi), %mm3                    	# D.15661
	movq %mm1, %mm4                      
	punpcklbw %mm3, %mm1                 
	punpckhbw %mm3, %mm4                 
	movq %mm0, %mm3                      
	punpcklwd %mm1, %mm0                 
	punpckhwd %mm1, %mm3                 
	movq %mm2, %mm1                      
	punpcklwd %mm4, %mm2                 
	punpckhwd %mm4, %mm1                 
	movd %mm0, 4(%rcx)                      	# D.15659
	psrlq $32, %mm0                       
	movd %mm0, 4(%rax)               
	movd %mm3, 4(%rax, %rbp)           	# D.15670
	psrlq $32, %mm3                       
	movd %mm3, 4(%rax, %rbp, 2)        	# D.15670
	movd %mm2, 4(%rcx, %rbp, 4)               	# D.15659, D.15670
	psrlq $32, %mm2                       
	movd %mm2, 4(%rdx)               
	movd %mm1, 4(%rdx, %rbp)           	# D.15670
	psrlq $32, %mm1                       
	movd %mm1, 4(%rdx, %rbp, 2)        	# D.15670
	
# 0 "" 2
#NO_APP
	cmpl	$0, 16(%rsp)	#, %sfp
	jle	.L872	#,
	cmpb	$0, 59(%rsp)	#, %sfp
	je	.L872	#,
	movq	%rbx, %rax	# dstBlock, D.15662
	subq	88(%rsp), %rax	# %sfp, D.15662
	leaq	-8(%rax), %rdi	#, D.15659
	leaq	1648(%rsp), %rdx	#, tmp1370
	movl	12(%rsp), %esi	# %sfp,
	call	dering_MMX2	#
.L872:
	testl	$1048576, %r15d	#, D.15658
	je	.L864	#,
	movslq	2992(%rsp), %rdx	# isColor, isColor
	movl	%r12d, %eax	# x, D.15658
	sarl	$3, %eax	#, D.15658
	cltq
	addq	144(%rsp), %rax	# %sfp, D.15662
	movq	1712(%rsp,%rdx,8), %rcx	# c.tempBlurredPast, tmp1009
	leaq	(%rcx,%rax,4), %rax	#, D.15678
	movslq	%r12d, %rsi	# x, D.15662
	addq	136(%rsp), %rsi	# %sfp, D.15662
	addq	1688(%rsp,%rdx,8), %rsi	# c.tempBlurred, D.15659
	movq	%rax, 328(%rsp)	# D.15678, tempBlurredPast
	movl	2880(%rsp), %edx	# MEM[(const int *)&c + 1232B], MEM[(const int *)&c + 1232B]
	movl	%edx, 508(%rax)	# MEM[(const int *)&c + 1232B], MEM[(uint32_t *)_409 + 508B]
	movl	2884(%rsp), %edx	# MEM[(const int *)&c + 1236B], MEM[(const int *)&c + 1236B]
	movl	%edx, 512(%rax)	# MEM[(const int *)&c + 1236B], MEM[(uint32_t *)_409 + 512B]
	movl	2888(%rsp), %edx	# MEM[(const int *)&c + 1240B], MEM[(const int *)&c + 1240B]
	movl	%edx, 516(%rax)	# MEM[(const int *)&c + 1240B], MEM[(uint32_t *)_409 + 516B]
	leaq	-8(%rbx), %rdi	#, D.15659
#APP
# 2169 "postprocess_template.c" 1
	lea (%rbp, %rbp, 2), %rax             	# D.15670
	lea (%rbp, %rbp, 4), %rdx             	# D.15670
	lea (%rdx, %rbp, 2), %rcx      	# D.15670
	pcmpeqb %mm7, %mm7                   
	movq b80, %mm6              
	pxor %mm0, %mm0                      
	movq (%rdi), %mm5                     	# D.15659
	movq (%rsi), %mm2                     	# D.15659
	pxor %mm7, %mm2                      
	pavgb %mm2, %mm5 
	paddb %mm6, %mm5                     
	movq %mm5, %mm2                      
	psllw $8, %mm5                        
	pmaddwd %mm5, %mm5                   
	pmaddwd %mm2, %mm2                   
	paddd %mm2, %mm5                     
	psrld $14, %mm5                       
	paddd %mm5, %mm0                     
	movq (%rdi, %rbp), %mm5                     	# D.15659, D.15670
	movq (%rsi, %rbp), %mm2                     	# D.15659, D.15670
	pxor %mm7, %mm2                      
	pavgb %mm2, %mm5 
	paddb %mm6, %mm5                     
	movq %mm5, %mm2                      
	psllw $8, %mm5                        
	pmaddwd %mm5, %mm5                   
	pmaddwd %mm2, %mm2                   
	paddd %mm2, %mm5                     
	psrld $14, %mm5                       
	paddd %mm5, %mm0                     
	movq (%rdi, %rbp, 2), %mm5                     	# D.15659, D.15670
	movq (%rsi, %rbp, 2), %mm2                     	# D.15659, D.15670
	pxor %mm7, %mm2                      
	pavgb %mm2, %mm5 
	paddb %mm6, %mm5                     
	movq %mm5, %mm2                      
	psllw $8, %mm5                        
	pmaddwd %mm5, %mm5                   
	pmaddwd %mm2, %mm2                   
	paddd %mm2, %mm5                     
	psrld $14, %mm5                       
	paddd %mm5, %mm0                     
	movq (%rdi, %rax), %mm5                     	# D.15659
	movq (%rsi, %rax), %mm2                     	# D.15659
	pxor %mm7, %mm2                      
	pavgb %mm2, %mm5 
	paddb %mm6, %mm5                     
	movq %mm5, %mm2                      
	psllw $8, %mm5                        
	pmaddwd %mm5, %mm5                   
	pmaddwd %mm2, %mm2                   
	paddd %mm2, %mm5                     
	psrld $14, %mm5                       
	paddd %mm5, %mm0                     
	movq (%rdi, %rbp, 4), %mm5                     	# D.15659, D.15670
	movq (%rsi, %rbp, 4), %mm2                     	# D.15659, D.15670
	pxor %mm7, %mm2                      
	pavgb %mm2, %mm5 
	paddb %mm6, %mm5                     
	movq %mm5, %mm2                      
	psllw $8, %mm5                        
	pmaddwd %mm5, %mm5                   
	pmaddwd %mm2, %mm2                   
	paddd %mm2, %mm5                     
	psrld $14, %mm5                       
	paddd %mm5, %mm0                     
	movq (%rdi, %rdx), %mm5                     	# D.15659
	movq (%rsi, %rdx), %mm2                     	# D.15659
	pxor %mm7, %mm2                      
	pavgb %mm2, %mm5 
	paddb %mm6, %mm5                     
	movq %mm5, %mm2                      
	psllw $8, %mm5                        
	pmaddwd %mm5, %mm5                   
	pmaddwd %mm2, %mm2                   
	paddd %mm2, %mm5                     
	psrld $14, %mm5                       
	paddd %mm5, %mm0                     
	movq (%rdi, %rax,2), %mm5                     	# D.15659
	movq (%rsi, %rax,2), %mm2                     	# D.15659
	pxor %mm7, %mm2                      
	pavgb %mm2, %mm5 
	paddb %mm6, %mm5                     
	movq %mm5, %mm2                      
	psllw $8, %mm5                        
	pmaddwd %mm5, %mm5                   
	pmaddwd %mm2, %mm2                   
	paddd %mm2, %mm5                     
	psrld $14, %mm5                       
	paddd %mm5, %mm0                     
	movq (%rdi, %rcx), %mm5                     	# D.15659
	movq (%rsi, %rcx), %mm2                     	# D.15659
	pxor %mm7, %mm2                      
	pavgb %mm2, %mm5 
	paddb %mm6, %mm5                     
	movq %mm5, %mm2                      
	psllw $8, %mm5                        
	pmaddwd %mm5, %mm5                   
	pmaddwd %mm2, %mm2                   
	paddd %mm2, %mm5                     
	psrld $14, %mm5                       
	paddd %mm5, %mm0                     
	movq %mm0, %mm4                      
	psrlq $32, %mm0                       
	paddd %mm0, %mm4                     
	movd %mm4, %ecx                      
	shll $2, %ecx                         
	mov 328(%rsp), %rdx                      	# tempBlurredPast
	addl -4(%rdx), %ecx              
	addl 4(%rdx), %ecx               
	addl -1024(%rdx), %ecx           
	addl $4, %ecx                         
	addl 1024(%rdx), %ecx            
	shrl $3, %ecx                         
	movl %ecx, (%rdx)                
	cmpl 512(%rdx), %ecx             
	 jb 2f                                 
	cmpl 516(%rdx), %ecx             
	 jb 1f                                 
	lea (%rax, %rbp, 2), %rdx      	# D.15670
	lea (%rdx, %rbp, 2), %rcx      	# D.15670
	movq (%rdi), %mm0                       	# D.15659
	movq (%rdi, %rbp), %mm1                   	# D.15659, D.15670
	movq (%rdi, %rbp, 2), %mm2                	# D.15659, D.15670
	movq (%rdi, %rax), %mm3            	# D.15659
	movq (%rdi, %rbp, 4), %mm4                	# D.15659, D.15670
	movq (%rdi, %rdx), %mm5            	# D.15659
	movq (%rdi, %rax, 2), %mm6         	# D.15659
	movq (%rdi, %rcx), %mm7            	# D.15659
	movq %mm0, (%rsi)                       	# D.15659
	movq %mm1, (%rsi, %rbp)                   	# D.15659, D.15670
	movq %mm2, (%rsi, %rbp, 2)                	# D.15659, D.15670
	movq %mm3, (%rsi, %rax)            	# D.15659
	movq %mm4, (%rsi, %rbp, 4)                	# D.15659, D.15670
	movq %mm5, (%rsi, %rdx)            	# D.15659
	movq %mm6, (%rsi, %rax, 2)         	# D.15659
	movq %mm7, (%rsi, %rcx)            	# D.15659
	jmp 4f                                 
	1:                                     
	lea (%rax, %rbp, 2), %rdx      	# D.15670
	lea (%rdx, %rbp, 2), %rcx      	# D.15670
	movq (%rdi), %mm0                       	# D.15659
	pavgb (%rsi), %mm0 	# D.15659
	movq (%rdi, %rbp), %mm1                   	# D.15659, D.15670
	pavgb (%rsi, %rbp), %mm1 	# D.15659, D.15670
	movq (%rdi, %rbp, 2), %mm2                	# D.15659, D.15670
	pavgb (%rsi, %rbp, 2), %mm2 	# D.15659, D.15670
	movq (%rdi, %rax), %mm3            	# D.15659
	pavgb (%rsi, %rax), %mm3 	# D.15659
	movq (%rdi, %rbp, 4), %mm4                	# D.15659, D.15670
	pavgb (%rsi, %rbp, 4), %mm4 	# D.15659, D.15670
	movq (%rdi, %rdx), %mm5            	# D.15659
	pavgb (%rsi, %rdx), %mm5 	# D.15659
	movq (%rdi, %rax, 2), %mm6         	# D.15659
	pavgb (%rsi, %rax, 2), %mm6 	# D.15659
	movq (%rdi, %rcx), %mm7            	# D.15659
	pavgb (%rsi, %rcx), %mm7 	# D.15659
	movq %mm0, (%rsi)                       	# D.15659
	movq %mm1, (%rsi, %rbp)                   	# D.15659, D.15670
	movq %mm2, (%rsi, %rbp, 2)                	# D.15659, D.15670
	movq %mm3, (%rsi, %rax)            	# D.15659
	movq %mm4, (%rsi, %rbp, 4)                	# D.15659, D.15670
	movq %mm5, (%rsi, %rdx)            	# D.15659
	movq %mm6, (%rsi, %rax, 2)         	# D.15659
	movq %mm7, (%rsi, %rcx)            	# D.15659
	movq %mm0, (%rdi)                       	# D.15659
	movq %mm1, (%rdi, %rbp)                   	# D.15659, D.15670
	movq %mm2, (%rdi, %rbp, 2)                	# D.15659, D.15670
	movq %mm3, (%rdi, %rax)            	# D.15659
	movq %mm4, (%rdi, %rbp, 4)                	# D.15659, D.15670
	movq %mm5, (%rdi, %rdx)            	# D.15659
	movq %mm6, (%rdi, %rax, 2)         	# D.15659
	movq %mm7, (%rdi, %rcx)            	# D.15659
	jmp 4f                                 
	2:                                     
	cmpl 508(%rdx), %ecx             
	 jb 3f                                 
	lea (%rax, %rbp, 2), %rdx      	# D.15670
	lea (%rdx, %rbp, 2), %rcx      	# D.15670
	movq (%rdi), %mm0                       	# D.15659
	movq (%rdi, %rbp), %mm1                   	# D.15659, D.15670
	movq (%rdi, %rbp, 2), %mm2                	# D.15659, D.15670
	movq (%rdi, %rax), %mm3            	# D.15659
	movq (%rsi), %mm4                       	# D.15659
	movq (%rsi, %rbp), %mm5                   	# D.15659, D.15670
	movq (%rsi, %rbp, 2), %mm6                	# D.15659, D.15670
	movq (%rsi, %rax), %mm7            	# D.15659
	pavgb %mm4, %mm0 
	pavgb %mm5, %mm1 
	pavgb %mm6, %mm2 
	pavgb %mm7, %mm3 
	pavgb %mm4, %mm0 
	pavgb %mm5, %mm1 
	pavgb %mm6, %mm2 
	pavgb %mm7, %mm3 
	movq %mm0, (%rsi)                       	# D.15659
	movq %mm1, (%rsi, %rbp)                   	# D.15659, D.15670
	movq %mm2, (%rsi, %rbp, 2)                	# D.15659, D.15670
	movq %mm3, (%rsi, %rax)            	# D.15659
	movq %mm0, (%rdi)                       	# D.15659
	movq %mm1, (%rdi, %rbp)                   	# D.15659, D.15670
	movq %mm2, (%rdi, %rbp, 2)                	# D.15659, D.15670
	movq %mm3, (%rdi, %rax)            	# D.15659
	movq (%rdi, %rbp, 4), %mm0                	# D.15659, D.15670
	movq (%rdi, %rdx), %mm1            	# D.15659
	movq (%rdi, %rax, 2), %mm2         	# D.15659
	movq (%rdi, %rcx), %mm3            	# D.15659
	movq (%rsi, %rbp, 4), %mm4                	# D.15659, D.15670
	movq (%rsi, %rdx), %mm5            	# D.15659
	movq (%rsi, %rax, 2), %mm6         	# D.15659
	movq (%rsi, %rcx), %mm7            	# D.15659
	pavgb %mm4, %mm0 
	pavgb %mm5, %mm1 
	pavgb %mm6, %mm2 
	pavgb %mm7, %mm3 
	pavgb %mm4, %mm0 
	pavgb %mm5, %mm1 
	pavgb %mm6, %mm2 
	pavgb %mm7, %mm3 
	movq %mm0, (%rsi, %rbp, 4)                	# D.15659, D.15670
	movq %mm1, (%rsi, %rdx)            	# D.15659
	movq %mm2, (%rsi, %rax, 2)         	# D.15659
	movq %mm3, (%rsi, %rcx)            	# D.15659
	movq %mm0, (%rdi, %rbp, 4)                	# D.15659, D.15670
	movq %mm1, (%rdi, %rdx)            	# D.15659
	movq %mm2, (%rdi, %rax, 2)         	# D.15659
	movq %mm3, (%rdi, %rcx)            	# D.15659
	jmp 4f                                 
	3:                                     
	lea (%rax, %rbp, 2), %rdx      	# D.15670
	lea (%rdx, %rbp, 2), %rcx      	# D.15670
	movq (%rdi), %mm0                       	# D.15659
	movq (%rdi, %rbp), %mm1                   	# D.15659, D.15670
	movq (%rdi, %rbp, 2), %mm2                	# D.15659, D.15670
	movq (%rdi, %rax), %mm3            	# D.15659
	movq (%rsi), %mm4                       	# D.15659
	movq (%rsi, %rbp), %mm5                   	# D.15659, D.15670
	movq (%rsi, %rbp, 2), %mm6                	# D.15659, D.15670
	movq (%rsi, %rax), %mm7            	# D.15659
	pavgb %mm4, %mm0 
	pavgb %mm5, %mm1 
	pavgb %mm6, %mm2 
	pavgb %mm7, %mm3 
	pavgb %mm4, %mm0 
	pavgb %mm5, %mm1 
	pavgb %mm6, %mm2 
	pavgb %mm7, %mm3 
	pavgb %mm4, %mm0 
	pavgb %mm5, %mm1 
	pavgb %mm6, %mm2 
	pavgb %mm7, %mm3 
	movq %mm0, (%rsi)                       	# D.15659
	movq %mm1, (%rsi, %rbp)                   	# D.15659, D.15670
	movq %mm2, (%rsi, %rbp, 2)                	# D.15659, D.15670
	movq %mm3, (%rsi, %rax)            	# D.15659
	movq %mm0, (%rdi)                       	# D.15659
	movq %mm1, (%rdi, %rbp)                   	# D.15659, D.15670
	movq %mm2, (%rdi, %rbp, 2)                	# D.15659, D.15670
	movq %mm3, (%rdi, %rax)            	# D.15659
	movq (%rdi, %rbp, 4), %mm0                	# D.15659, D.15670
	movq (%rdi, %rdx), %mm1            	# D.15659
	movq (%rdi, %rax, 2), %mm2         	# D.15659
	movq (%rdi, %rcx), %mm3            	# D.15659
	movq (%rsi, %rbp, 4), %mm4                	# D.15659, D.15670
	movq (%rsi, %rdx), %mm5            	# D.15659
	movq (%rsi, %rax, 2), %mm6         	# D.15659
	movq (%rsi, %rcx), %mm7            	# D.15659
	pavgb %mm4, %mm0 
	pavgb %mm5, %mm1 
	pavgb %mm6, %mm2 
	pavgb %mm7, %mm3 
	pavgb %mm4, %mm0 
	pavgb %mm5, %mm1 
	pavgb %mm6, %mm2 
	pavgb %mm7, %mm3 
	pavgb %mm4, %mm0 
	pavgb %mm5, %mm1 
	pavgb %mm6, %mm2 
	pavgb %mm7, %mm3 
	movq %mm0, (%rsi, %rbp, 4)                	# D.15659, D.15670
	movq %mm1, (%rsi, %rdx)            	# D.15659
	movq %mm2, (%rsi, %rax, 2)         	# D.15659
	movq %mm3, (%rsi, %rcx)            	# D.15659
	movq %mm0, (%rdi, %rbp, 4)                	# D.15659, D.15670
	movq %mm1, (%rdi, %rdx)            	# D.15659
	movq %mm2, (%rdi, %rax, 2)         	# D.15659
	movq %mm3, (%rdi, %rcx)            	# D.15659
	4:                                     
	
# 0 "" 2
#NO_APP
.L864:
	addq	$8, %rbx	#, dstBlock
	addq	$8, %r14	#, srcBlock
	addl	$8, %r12d	#, x
	movq	(%rsp), %rax	# %sfp, tempBlock2
	movq	%r13, (%rsp)	# tempBlock2, %sfp
	cmpl	%r12d, 28(%rsp)	# x, %sfp
	jg	.L889	#,
.L842:
	cmpl	$0, 16(%rsp)	#, %sfp
	jle	.L874	#,
	testb	$4, %r15b	#, D.15658
	je	.L874	#,
	movq	%rbx, %rax	# dstBlock, D.15662
	subq	88(%rsp), %rax	# %sfp, D.15662
	leaq	-8(%rax), %rdi	#, D.15659
	leaq	1648(%rsp), %rdx	#, tmp1378
	movl	12(%rsp), %esi	# %sfp,
	call	dering_MMX2	#
.L874:
	testl	$1048576, %r15d	#, D.15658
	je	.L875	#,
	movslq	2992(%rsp), %rsi	# isColor, isColor
	movl	16(%rsp), %eax	# %sfp, D.15658
	sarl	$3, %eax	#, D.15658
	sall	$8, %eax	#, D.15658
	cltq
	movl	%r12d, %edx	# x, D.15658
	sarl	$3, %edx	#, D.15658
	movslq	%edx, %rdx	# D.15658, D.15662
	leaq	256(%rax,%rdx), %rdx	#, D.15662
	movq	1712(%rsp,%rsi,8), %rax	# c.tempBlurredPast, tmp1041
	leaq	(%rax,%rdx,4), %rcx	#, D.15678
	movslq	%r12d, %r12	# x, D.15662
	addq	136(%rsp), %r12	# %sfp, D.15662
	movq	%r12, %rdx	# D.15662, D.15659
	addq	1688(%rsp,%rsi,8), %rdx	# c.tempBlurred, D.15659
	leaq	-8(%rbx), %rdi	#, D.15659
	leaq	2880(%rsp), %r8	#,
	movl	12(%rsp), %esi	# %sfp,
	call	tempNoiseReducer_MMX2	#
.L875:
	movl	172(%rsp), %ebx	# %sfp, D.15658
	cmpl	%ebx, 24(%rsp)	# D.15658, %sfp
	jg	.L876	#,
	movl	28(%rsp), %ebx	# %sfp, width
	movl	264(%rsp), %ecx	# %sfp, D.15658
	cmpl	%ecx, %ebx	# D.15658, width
	je	.L877	#,
	movl	200(%rsp), %ecx	# %sfp, ivtmp.1993
	movl	%ecx, %r14d	# ivtmp.1993, D.15658
	movl	12(%rsp), %r13d	# %sfp, D.15657
	movl	$0, %eax	#, ivtmp.1944
	movl	$0, %ebp	#, i
	movslq	%ebx, %r12	# width, D.15663
	testl	%ecx, %ecx	# ivtmp.1993
	jg	.L891	#,
	jmp	.L876	#
.L877:
	movl	12(%rsp), %ecx	# %sfp,
	movl	200(%rsp), %edx	# %sfp,
	movq	256(%rsp), %rsi	# %sfp,
	movq	232(%rsp), %rdi	# %sfp,
	call	linecpy	#
	jmp	.L876	#
.L891:
	addl	$1, %ebp	#, i
	leal	0(%r13,%rax), %ebx	#, D.15657
	cltq
	movq	232(%rsp), %rcx	# %sfp, dstBlock
	leaq	(%rcx,%rax), %rdi	#, D.15659
	movslq	%ebx, %rsi	# D.15657, D.15662
	addq	224(%rsp), %rsi	# %sfp, D.15659
	movq	%r12, %rdx	# D.15663,
	call	memcpy	#
	movl	%ebx, %eax	# D.15657, ivtmp.1944
	cmpl	%r14d, %ebp	# D.15658, i
	jne	.L891	#,
.L876:
	addl	$8, 16(%rsp)	#, %sfp
	movl	16(%rsp), %eax	# %sfp, y
	movl	240(%rsp), %edi	# %sfp, D.15657
	addl	%edi, 156(%rsp)	# D.15657, %sfp
	movl	212(%rsp), %esi	# %sfp, D.15657
	addl	%esi, 168(%rsp)	# D.15657, %sfp
	subl	$8, 200(%rsp)	#, %sfp
	subl	$8, 204(%rsp)	#, %sfp
	movl	220(%rsp), %ecx	# %sfp, D.15657
	addl	%ecx, 208(%rsp)	# D.15657, %sfp
	cmpl	%eax, 24(%rsp)	# y, %sfp
	jg	.L880	#,
.L838:
#APP
# 3692 "postprocess_template.c" 1
	emms
# 0 "" 2
#NO_APP
	leaq	1648(%rsp), %rsi	#, tmp1060
	movl	$157, %ecx	#, tmp1061
	movq	3000(%rsp), %rdi	# c2, c2
	rep movsq
	jmp	.L902	#
.L807:
	movl	2892(%rsp), %r9d	# c.ppMode.baseDcDiff, D.15657
	leaq	1648(%rsp), %rax	#, tmp1403
	leaq	1776(%rsp), %rdx	#, ivtmp.2047
	leaq	584(%rax), %r8	#, D.15660
	movl	$0, %ecx	#, ivtmp.2046
	movl	$126, %edi	#, tmp1084
	movabsq	$72340172838076673, %rsi	#, tmp1085
	jmp	.L808	#
.L902:
	addq	$2920, %rsp	#,
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
.LFE139:
	.size	postProcess_MMX2, .-postProcess_MMX2
	.type	postProcess_3DNow, @function
postProcess_3DNow:
.LFB157:
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
	subq	$2920, %rsp	#,
	.cfi_def_cfa_offset 2976
	movq	%rdi, 184(%rsp)	# src, %sfp
	movl	%esi, 48(%rsp)	# srcStride, %sfp
	movq	%rdx, 160(%rsp)	# dst, %sfp
	movl	%ecx, 4(%rsp)	# dstStride, %sfp
	movl	%r8d, 28(%rsp)	# width, %sfp
	movl	%r9d, 24(%rsp)	# height, %sfp
	leaq	1648(%rsp), %rdi	#, tmp939
	movl	$157, %ecx	#, tmp941
	movq	3000(%rsp), %rsi	# c2, c2
	rep movsq
	cmpl	$0, 2992(%rsp)	#, isColor
	je	.L907	#,
	movl	2860(%rsp), %r15d	# c.ppMode.chromMode, D.15893
	movl	$4, %eax	#, tmp942
	movl	%eax, %ebx	# tmp942, D.15893
	subl	2848(%rsp), %ebx	# c.hChromaSubSample, D.15893
	movl	%ebx, 136(%rsp)	# D.15893, %sfp
	subl	2852(%rsp), %eax	# c.vChromaSubSample, D.15893
	movl	%eax, 208(%rsp)	# D.15893, %sfp
	jmp	.L908	#
.L907:
	movl	2856(%rsp), %r15d	# c.ppMode.lumMode, D.15893
	movl	$4, 136(%rsp)	#, %sfp
	movl	$4, 208(%rsp)	#, %sfp
.L908:
	movq	1664(%rsp), %rax	# c.yHistogram, yHistogram
	movq	%rax, 96(%rsp)	# yHistogram, %sfp
	cmpl	$0, 48(%rsp)	#, %sfp
	jle	.L909	#,
	movq	1744(%rsp), %rax	# c.tempSrc, srcBlock
	movq	%rax, 248(%rsp)	# srcBlock, %sfp
	jmp	.L910	#
.L909:
	imull	$23, 48(%rsp), %eax	#, %sfp, D.15893
	cltq
	movq	1744(%rsp), %rbx	# c.tempSrc, srcBlock
	subq	%rax, %rbx	# D.15897, srcBlock
	movq	%rbx, 248(%rsp)	# srcBlock, %sfp
.L910:
	cmpl	$0, 4(%rsp)	#, %sfp
	jle	.L911	#,
	movq	1736(%rsp), %rax	# c.tempDst, tmp1443
	addq	$32, %rax	#, D.15895
	movq	%rax, 224(%rsp)	# D.15895, %sfp
	jmp	.L912	#
.L911:
	imull	$23, 4(%rsp), %edx	#, %sfp, D.15893
	movslq	%edx, %rdx	# D.15893, D.15897
	movl	$32, %eax	#, tmp951
	subq	%rdx, %rax	# D.15897, D.15897
	addq	1736(%rsp), %rax	# c.tempDst, D.15895
	movq	%rax, 224(%rsp)	# D.15895, %sfp
.L912:
	testl	$33554432, %r15d	#, D.15893
	je	.L913	#,
	movl	$.LC1, %edx	#,
	movl	$24, %esi	#,
	movq	3000(%rsp), %rdi	# c2,
	movl	$0, %eax	#,
	call	av_log	#
	jmp	.L913	#
.L914:
	movl	%ecx, %r10d	# ivtmp.2156, D.15893
	sarl	$8, %r10d	#, D.15893
	movl	%edi, %eax	# tmp1357, D.15893
	subl	%r10d, %eax	# D.15893, D.15893
	cltq
	imulq	%rsi, %rax	# tmp1358, D.15899
	movq	%rax, (%rdx)	# D.15899, MEM[base: _120, offset: 0B]
	leal	2(%r10,%r10), %eax	#, D.15893
	movl	%edi, %ebx	# tmp1357, D.15893
	subl	%eax, %ebx	# D.15893, D.15893
	movl	%ebx, %eax	# D.15893, D.15893
	cltq
	imulq	%rsi, %rax	# tmp1358, D.15899
	movq	%rax, 512(%rdx)	# D.15899, MEM[base: _120, offset: 512B]
	addl	%r9d, %ecx	# D.15892, ivtmp.2156
	addq	$8, %rdx	#, ivtmp.2157
	cmpq	%r8, %rdx	# D.15913, ivtmp.2157
	jne	.L914	#,
	movl	$16, 240(%rsp)	#, %sfp
	movl	%r15d, %eax	# D.15893, D.15893
	andl	$262144, %eax	#, D.15893
	movl	%eax, 140(%rsp)	# D.15893, %sfp
	jne	.L915	#,
	movl	$14, 240(%rsp)	#, %sfp
	testl	$12713984, %r15d	#, D.15893
	jne	.L915	#,
	movl	$13, 240(%rsp)	#, %sfp
	testl	$590849, %r15d	#, D.15893
	jne	.L915	#,
	movl	$11, 240(%rsp)	#, %sfp
	testl	$512, %r15d	#, D.15893
	jne	.L915	#,
	movl	%r15d, %eax	# D.15893, D.15893
	andl	$4, %eax	#, D.15893
	cmpl	$1, %eax	#, D.15893
	sbbl	%eax, %eax	# copyAhead
	addl	$9, %eax	#, copyAhead
	movl	%eax, 240(%rsp)	# copyAhead, %sfp
.L915:
	movl	240(%rsp), %eax	# %sfp, copyAhead
	subl	$8, %eax	#, copyAhead
	movl	%eax, 244(%rsp)	# copyAhead, %sfp
	cmpl	$0, 2992(%rsp)	#, isColor
	jne	.L916	#,
	movl	2832(%rsp), %eax	# c.frameNum, tmp1455
	addl	$1, %eax	#, D.15893
	movl	%eax, 2832(%rsp)	# D.15893, c.frameNum
	cmpl	$1, %eax	#, D.15893
	jne	.L917	#,
	movslq	28(%rsp), %rdx	# %sfp, D.15898
	movslq	24(%rsp), %rax	# %sfp, D.15898
	imulq	%rdx, %rax	# D.15898, D.15898
	shrq	$6, %rax	#, D.15898
	movq	%rax, %rdx	# D.15898, tmp976
	salq	$4, %rdx	#, tmp976
	subq	%rax, %rdx	# D.15898, D.15898
	movq	%rdx, %rax	# D.15898, D.15898
	shrq	$8, %rax	#, tmp978
	movq	96(%rsp), %rbx	# %sfp, yHistogram
	movq	%rax, (%rbx)	# tmp978, *yHistogram_63
.L917:
	movq	96(%rsp), %rax	# %sfp, yHistogram
	movq	%rax, %rsi	# yHistogram, ivtmp.2132
	leaq	2048(%rax), %rcx	#, D.15913
	movl	$0, %edx	#, clipped
.L918:
	addq	(%rax), %rdx	# MEM[base: _701, offset: 0B], clipped
	addq	$8, %rax	#, ivtmp.2148
	cmpq	%rcx, %rax	# D.15913, ivtmp.2148
	jne	.L918	#,
	movq	%rdx, %rcx	# clipped, clipped
	testq	%rdx, %rdx	# clipped
	js	.L919	#,
	pxor	%xmm0, %xmm0	# D.15902
	cvtsi2ssq	%rdx, %xmm0	# clipped, D.15902
	jmp	.L920	#
.L919:
	movq	%rdx, %rax	# clipped, tmp984
	shrq	%rax	# tmp984
	movq	%rdx, %rdi	# clipped, tmp985
	andl	$1, %edi	#, tmp985
	orq	%rdi, %rax	# tmp985, tmp984
	pxor	%xmm0, %xmm0	# tmp983
	cvtsi2ssq	%rax, %xmm0	# tmp984, tmp983
	addss	%xmm0, %xmm0	# tmp983, D.15902
.L920:
	mulss	2876(%rsp), %xmm0	# c.ppMode.maxClippedThreshold, D.15902
	ucomiss	.LC2(%rip), %xmm0	#, D.15902
	jnb	.L921	#,
	cvttss2siq	%xmm0, %r8	# D.15902, maxClipped
	jmp	.L922	#
.L921:
	subss	.LC2(%rip), %xmm0	#, tmp987
	cvttss2siq	%xmm0, %r8	# tmp987, maxClipped
	movabsq	$-9223372036854775808, %rax	#, tmp989
	xorq	%rax, %r8	# tmp989, maxClipped
.L922:
	cmpq	%rcx, %r8	# clipped, maxClipped
	ja	.L992	#,
	movq	96(%rsp), %rax	# %sfp, yHistogram
	leaq	2040(%rax), %rdi	#, ivtmp.2139
	movq	%rdx, %rcx	# clipped, clipped
	movl	$255, %eax	#, black
.L924:
	subq	(%rdi), %rcx	# MEM[base: _700, offset: 0B], clipped
	subl	$1, %eax	#, black
	subq	$8, %rdi	#, ivtmp.2139
	cmpq	%rcx, %r8	# clipped, maxClipped
	ja	.L996	#,
	testl	%eax, %eax	# black
	jg	.L924	#,
.L996:
	movl	$0, %ecx	#, white
.L926:
	subq	(%rsi), %rdx	# MEM[base: _698, offset: 0B], clipped
	addl	$1, %ecx	#, white
	addq	$8, %rsi	#, ivtmp.2132
	cmpq	%rdx, %r8	# clipped, maxClipped
	ja	.L923	#,
	cmpl	$255, %ecx	#, white
	jle	.L926	#,
	jmp	.L923	#
.L992:
	movl	$255, %eax	#, black
	movl	$0, %ecx	#, white
.L923:
	movl	2868(%rsp), %esi	# c.ppMode.minAllowedY, D.15893
	movl	2872(%rsp), %edx	# c.ppMode.maxAllowedY, D.15893
	subl	%esi, %edx	# D.15893, D.15893
	pxor	%xmm1, %xmm1	# D.15903
	cvtsi2sd	%edx, %xmm1	# D.15893, D.15903
	subl	%eax, %ecx	# black, D.15893
	pxor	%xmm0, %xmm0	# D.15903
	cvtsi2sd	%ecx, %xmm0	# D.15893, D.15903
	divsd	%xmm0, %xmm1	# D.15903, scale
	movapd	%xmm1, %xmm0	# scale, scale
	mulsd	.LC3(%rip), %xmm1	#, D.15903
	addsd	.LC4(%rip), %xmm1	#, D.15903
	cvttsd2si	%xmm1, %edx	# D.15903, D.15904
	movzwl	%dx, %edx	# D.15904, D.15898
	subl	%esi, %eax	# D.15893, D.15893
	movzwl	%ax, %eax	# D.15893, D.15898
	movq	%rax, %rcx	# D.15898, D.15898
	salq	$32, %rcx	#, D.15898
	orq	%rcx, %rax	# D.15898, D.15898
	movq	%rax, %rcx	# D.15898, D.15898
	salq	$16, %rcx	#, D.15898
	orq	%rcx, %rax	# D.15898, tmp1013
	movq	%rax, 1672(%rsp)	# tmp1013, c.packedYOffset
	movq	%rdx, %rax	# D.15898, D.15898
	salq	$32, %rax	#, D.15898
	orq	%rdx, %rax	# D.15898, D.15898
	movq	%rax, %rdx	# D.15898, D.15898
	salq	$16, %rdx	#, D.15898
	orq	%rdx, %rax	# D.15898, tmp1016
	movq	%rax, 1680(%rsp)	# tmp1016, c.packedYScale
	movl	$65536, 108(%rsp)	#, %sfp
	testb	$8, %r15b	#, D.15893
	je	.L928	#,
	movsd	.LC5(%rip), %xmm1	#, tmp1019
	mulsd	%xmm1, %xmm0	# tmp1019, D.15903
	mulsd	%xmm1, %xmm0	# tmp1019, D.15903
	addsd	.LC4(%rip), %xmm0	#, D.15903
	cvttsd2si	%xmm0, %eax	# D.15903, QPCorrecture
	movl	%eax, 108(%rsp)	# QPCorrecture, %sfp
	jmp	.L928	#
.L916:
	movabsq	$72058693566333184, %rax	#, tmp1473
	movq	%rax, 1680(%rsp)	# tmp1473, c.packedYScale
	movq	$0, 1672(%rsp)	#, c.packedYOffset
	movl	$65536, 108(%rsp)	#, %sfp
.L928:
	movl	48(%rsp), %esi	# %sfp, srcStride
	movl	%esi, %eax	# srcStride, D.15893
	negl	%eax	# D.15893
	sall	$3, %eax	#, tmp1027
	cltq
	movl	4(%rsp), %ebx	# %sfp, dstStride
	movslq	%ebx, %rcx	# dstStride, D.15897
	movq	%rcx, %rdi	# D.15897, D.15897
	movq	%rcx, 72(%rsp)	# D.15897, %sfp
	movq	224(%rsp), %rcx	# %sfp, dstBlock
	addq	%rdi, %rcx	# D.15897, dstBlock
	movq	%rcx, %rdi	# dstBlock, dstBlock
	movq	%rcx, 256(%rsp)	# dstBlock, %sfp
	cmpl	$0, 28(%rsp)	#, %sfp
	jle	.L929	#,
	leal	0(,%rbx,8), %r12d	#, D.15893
	movslq	%r12d, %r12	# D.15893, D.15897
	movl	%ebx, %ecx	# dstStride, dstStride
	leal	0(,%rbx,4), %edx	#, D.15893
	movslq	%edx, %rbx	# D.15893, D.15897
	movq	%rbx, 32(%rsp)	# D.15897, %sfp
	leal	0(,%rsi,8), %ebp	#, D.15893
	movslq	%ebp, %rbp	# D.15893, D.15897
	addq	%rax, %rbp	# D.15897, D.15897
	addq	184(%rsp), %rbp	# %sfp, ivtmp.2128
	movq	%rdi, %rbx	# dstBlock, dstBlock
	movl	%r15d, %r14d	# D.15893, D.15893
	andl	$8, %r14d	#, D.15893
	movl	%ecx, %eax	# dstStride, D.15893
	negl	%eax	# D.15893
	movslq	%eax, %r13	# D.15893, D.15906
	movq	%r12, 8(%rsp)	# D.15897, %sfp
	movq	%rdi, %r12	# dstBlock, dstBlock
	movq	%r13, 16(%rsp)	# D.15906, %sfp
	movl	%ecx, %r13d	# dstStride, dstStride
.L938:
	movl	%ebx, %esi	# dstBlock, D.15893
	subl	%r12d, %esi	# dstBlock, D.15893
	movq	8(%rsp), %rax	# %sfp, D.15897
	leaq	(%rbx,%rax), %rcx	#, D.15895
	testl	%r14d, %r14d	# D.15893
	je	.L930	#,
	movslq	48(%rsp), %rdi	# %sfp, D.15906
	movslq	%r13d, %r8	# dstStride, D.15906
	leaq	1672(%rsp), %rax	#, tmp1490
#APP
# 3102 "postprocess_template.c" 1
	movq (%rax), %mm2        
	movq 8(%rax), %mm3       
	lea (%rbp,%rdi), %rax         	# ivtmp.2128, D.15906
	lea (%rcx,%r8), %rdx         	# D.15895, D.15906
	pxor %mm4, %mm4              
	movq (%rbp), %mm0          	# ivtmp.2128
	movq (%rbp), %mm5          	# ivtmp.2128
	punpcklbw %mm4, %mm0         
	punpckhbw %mm4, %mm5         
	psubw %mm2, %mm0             
	psubw %mm2, %mm5             
	movq (%rbp, %rdi), %mm1          	# ivtmp.2128, D.15906
	psllw $6, %mm0                
	psllw $6, %mm5                
	pmulhw %mm3, %mm0            
	movq (%rbp, %rdi), %mm6          	# ivtmp.2128, D.15906
	pmulhw %mm3, %mm5            
	punpcklbw %mm4, %mm1         
	punpckhbw %mm4, %mm6         
	psubw %mm2, %mm1             
	psubw %mm2, %mm6             
	psllw $6, %mm1                
	psllw $6, %mm6                
	pmulhw %mm3, %mm1            
	pmulhw %mm3, %mm6            
	packuswb %mm5, %mm0          
	packuswb %mm6, %mm1          
	movq %mm0, (%rcx)          	# D.15895
	movq %mm1, (%rcx, %r8)          	# D.15895, D.15906
	movq (%rbp, %rdi, 2), %mm0          	# ivtmp.2128, D.15906
	movq (%rbp, %rdi, 2), %mm5          	# ivtmp.2128, D.15906
	punpcklbw %mm4, %mm0         
	punpckhbw %mm4, %mm5         
	psubw %mm2, %mm0             
	psubw %mm2, %mm5             
	movq (%rax, %rdi, 2), %mm1          	# D.15906
	psllw $6, %mm0                
	psllw $6, %mm5                
	pmulhw %mm3, %mm0            
	movq (%rax, %rdi, 2), %mm6          	# D.15906
	pmulhw %mm3, %mm5            
	punpcklbw %mm4, %mm1         
	punpckhbw %mm4, %mm6         
	psubw %mm2, %mm1             
	psubw %mm2, %mm6             
	psllw $6, %mm1                
	psllw $6, %mm6                
	pmulhw %mm3, %mm1            
	pmulhw %mm3, %mm6            
	packuswb %mm5, %mm0          
	packuswb %mm6, %mm1          
	movq %mm0, (%rcx, %r8, 2)          	# D.15895, D.15906
	movq %mm1, (%rdx, %r8, 2)          	# D.15906
	movq (%rbp, %rdi, 4), %mm0          	# ivtmp.2128, D.15906
	movq (%rbp, %rdi, 4), %mm5          	# ivtmp.2128, D.15906
	punpcklbw %mm4, %mm0         
	punpckhbw %mm4, %mm5         
	psubw %mm2, %mm0             
	psubw %mm2, %mm5             
	movq (%rax, %rdi, 4), %mm1          	# D.15906
	psllw $6, %mm0                
	psllw $6, %mm5                
	pmulhw %mm3, %mm0            
	movq (%rax, %rdi, 4), %mm6          	# D.15906
	pmulhw %mm3, %mm5            
	punpcklbw %mm4, %mm1         
	punpckhbw %mm4, %mm6         
	psubw %mm2, %mm1             
	psubw %mm2, %mm6             
	psllw $6, %mm1                
	psllw $6, %mm6                
	pmulhw %mm3, %mm1            
	pmulhw %mm3, %mm6            
	packuswb %mm5, %mm0          
	packuswb %mm6, %mm1          
	movq %mm0, (%rcx, %r8, 4)          	# D.15895, D.15906
	movq %mm1, (%rdx, %r8, 4)          	# D.15906
	lea (%rax,%rdi,4), %rax        	# D.15906
	lea (%rdx,%r8,4), %rdx        	# D.15906
	movq (%rax, %rdi), %mm0          	# D.15906
	movq (%rax, %rdi), %mm5          	# D.15906
	punpcklbw %mm4, %mm0         
	punpckhbw %mm4, %mm5         
	psubw %mm2, %mm0             
	psubw %mm2, %mm5             
	movq (%rax, %rdi, 2), %mm1          	# D.15906
	psllw $6, %mm0                
	psllw $6, %mm5                
	pmulhw %mm3, %mm0            
	movq (%rax, %rdi, 2), %mm6          	# D.15906
	pmulhw %mm3, %mm5            
	punpcklbw %mm4, %mm1         
	punpckhbw %mm4, %mm6         
	psubw %mm2, %mm1             
	psubw %mm2, %mm6             
	psllw $6, %mm1                
	psllw $6, %mm6                
	pmulhw %mm3, %mm1            
	pmulhw %mm3, %mm6            
	packuswb %mm5, %mm0          
	packuswb %mm6, %mm1          
	movq %mm0, (%rdx, %r8)          	# D.15906
	movq %mm1, (%rdx, %r8, 2)          	# D.15906
	
# 0 "" 2
#NO_APP
	jmp	.L931	#
.L930:
	movslq	48(%rsp), %rdi	# %sfp, D.15906
	movslq	%r13d, %r8	# dstStride, D.15906
#APP
# 3185 "postprocess_template.c" 1
	lea (%rbp,%rdi), %rax                 	# ivtmp.2128, D.15906
	lea (%rcx,%r8), %rdx                 	# D.15895, D.15906
	movq (%rbp), %mm0          	# ivtmp.2128
	movq (%rbp, %rdi), %mm1          	# ivtmp.2128, D.15906
	movq %mm0, (%rcx)          	# D.15895
	movq %mm1, (%rcx, %r8)          	# D.15895, D.15906
	movq (%rbp, %rdi, 2), %mm0          	# ivtmp.2128, D.15906
	movq (%rax, %rdi, 2), %mm1          	# D.15906
	movq %mm0, (%rcx, %r8, 2)          	# D.15895, D.15906
	movq %mm1, (%rdx, %r8, 2)          	# D.15906
	movq (%rbp, %rdi, 4), %mm0          	# ivtmp.2128, D.15906
	movq (%rax, %rdi, 4), %mm1          	# D.15906
	movq %mm0, (%rcx, %r8, 4)          	# D.15895, D.15906
	movq %mm1, (%rdx, %r8, 4)          	# D.15906
	lea (%rax,%rdi,4), %rax        	# D.15906
	lea (%rdx,%r8,4), %rdx        	# D.15906
	movq (%rax, %rdi), %mm0          	# D.15906
	movq (%rax, %rdi, 2), %mm1          	# D.15906
	movq %mm0, (%rdx, %r8)          	# D.15906
	movq %mm1, (%rdx, %r8, 2)          	# D.15906
	
# 0 "" 2
#NO_APP
.L931:
	movq	%rcx, %rax	# D.15895, src
	movq	16(%rsp), %rcx	# %sfp, D.15906
#APP
# 3225 "postprocess_template.c" 1
	movq (%rax), %mm0               	# src
	movq %mm0, (%rax, %rcx, 4)        	# src, D.15906
	add %rcx, %rax                     	# D.15906, src
	movq %mm0, (%rax)               	# src
	movq %mm0, (%rax, %rcx)           	# src, D.15906
	movq %mm0, (%rax, %rcx, 2)        	# src, D.15906
	movq %mm0, (%rax, %rcx, 4)        	# src, D.15906
	
# 0 "" 2
#NO_APP
	testl	$65536, %r15d	#, D.15893
	je	.L932	#,
	movq	32(%rsp), %rax	# %sfp, D.15897
	leaq	(%rbx,%rax), %rdx	#, D.15895
	movslq	%r13d, %rsi	# dstStride, D.15906
#APP
# 1455 "postprocess_template.c" 1
	lea (%rdx, %rsi), %rax                	# D.15895, D.15906
	lea (%rax, %rsi, 4), %rcx      	# D.15906
	movq (%rdx), %mm0                       	# D.15895
	movq (%rax, %rsi), %mm1            	# D.15906
	pavgusb %mm1, %mm0 
	movq %mm0, (%rax)                
	movq (%rdx, %rsi, 4), %mm0                	# D.15895, D.15906
	pavgusb %mm0, %mm1 
	movq %mm1, (%rax, %rsi, 2)         	# D.15906
	movq (%rcx, %rsi), %mm1            	# D.15906
	pavgusb %mm1, %mm0 
	movq %mm0, (%rcx)                
	movq (%rdx, %rsi, 8), %mm0                	# D.15895, D.15906
	pavgusb %mm0, %mm1 
	movq %mm1, (%rcx, %rsi, 2)         	# D.15906
	
# 0 "" 2
#NO_APP
	jmp	.L933	#
.L932:
	testl	$131072, %r15d	#, D.15893
	je	.L934	#,
	movq	32(%rsp), %rax	# %sfp, D.15897
	leaq	(%rbx,%rax), %rdi	#, D.15895
	movslq	%r13d, %rcx	# dstStride, D.15906
	movslq	%esi, %rsi	# D.15893, D.15897
	addq	1752(%rsp), %rsi	# c.deintTemp, D.15895
#APP
# 1775 "postprocess_template.c" 1
	lea (%rdi, %rcx), %rax                	# D.15895, D.15906
	lea (%rax, %rcx, 4), %rdx      	# D.15906
	movq (%rsi), %mm0                       	# D.15895
	movq (%rax), %mm1                
	pavgusb %mm1, %mm0 
	movq (%rdi), %mm2                       	# D.15895
	pavgusb %mm2, %mm0 
	movq %mm0, (%rdi)                       	# D.15895
	movq (%rax, %rcx), %mm0            	# D.15906
	pavgusb %mm0, %mm2 
	pavgusb %mm1, %mm2 
	movq %mm2, (%rax)                
	movq (%rax, %rcx, 2), %mm2         	# D.15906
	pavgusb %mm2, %mm1 
	pavgusb %mm0, %mm1 
	movq %mm1, (%rax, %rcx)            	# D.15906
	movq (%rdi, %rcx, 4), %mm1                	# D.15895, D.15906
	pavgusb %mm1, %mm0 
	pavgusb %mm2, %mm0 
	movq %mm0, (%rax, %rcx, 2)         	# D.15906
	movq (%rdx), %mm0                
	pavgusb %mm0, %mm2 
	pavgusb %mm1, %mm2 
	movq %mm2, (%rdi, %rcx, 4)                	# D.15895, D.15906
	movq (%rdx, %rcx), %mm2            	# D.15906
	pavgusb %mm2, %mm1 
	pavgusb %mm0, %mm1 
	movq %mm1, (%rdx)                
	movq (%rdx, %rcx, 2), %mm1         	# D.15906
	pavgusb %mm1, %mm0 
	pavgusb %mm2, %mm0 
	movq %mm0, (%rdx, %rcx)            	# D.15906
	movq (%rdi, %rcx, 8), %mm0                	# D.15895, D.15906
	pavgusb %mm0, %mm2 
	pavgusb %mm1, %mm2 
	movq %mm2, (%rdx, %rcx, 2)         	# D.15906
	movq %mm1, (%rsi)                       	# D.15895
	
# 0 "" 2
#NO_APP
	jmp	.L933	#
.L934:
	testl	$524288, %r15d	#, D.15893
	je	.L935	#,
	movl	%r13d, %esi	# dstStride,
	movq	%rbx, %rdi	# dstBlock,
	call	deInterlaceMedian_3DNow	#
	jmp	.L933	#
.L935:
	cmpl	$0, 140(%rsp)	#, %sfp
	je	.L936	#,
	movl	%r13d, %esi	# dstStride,
	movq	%rbx, %rdi	# dstBlock,
	call	deInterlaceInterpolateCubic_3DNow	#
	jmp	.L933	#
.L936:
	testl	$4194304, %r15d	#, D.15893
	je	.L937	#,
	movslq	%esi, %rdx	# D.15893, D.15897
	addq	1752(%rsp), %rdx	# c.deintTemp, D.15895
	movl	%r13d, %esi	# dstStride,
	movq	%rbx, %rdi	# dstBlock,
	call	deInterlaceFF_3DNow	#
	jmp	.L933	#
.L937:
	testl	$8388608, %r15d	#, D.15893
	je	.L933	#,
	movq	1752(%rsp), %rdx	# c.deintTemp, D.15895
	movslq	%esi, %rsi	# D.15893, D.15897
	movslq	28(%rsp), %rcx	# %sfp, D.15897
	addq	%rsi, %rcx	# D.15897, D.15897
	addq	%rdx, %rcx	# D.15895, D.15895
	addq	%rsi, %rdx	# D.15897, D.15895
	movl	%r13d, %esi	# dstStride,
	movq	%rbx, %rdi	# dstBlock,
	call	deInterlaceL5_3DNow	#
.L933:
	addq	$8, %rbx	#, dstBlock
	addq	$8, %rbp	#, ivtmp.2128
	movl	%ebx, %eax	# dstBlock, D.15892
	subl	%r12d, %eax	# dstBlock, D.15892
	cmpl	%eax, 28(%rsp)	# D.15892, %sfp
	jg	.L938	#,
.L929:
	movl	4(%rsp), %ebx	# %sfp, dstStride
	movl	%ebx, %eax	# dstStride, tmp1064
	sarl	$31, %eax	#, tmp1064
	xorl	%eax, %ebx	# tmp1064, D.15893
	subl	%eax, %ebx	# tmp1064, D.15893
	movl	%ebx, 288(%rsp)	# D.15893, %sfp
	cmpl	28(%rsp), %ebx	# %sfp, D.15893
	je	.L939	#,
	cmpl	$0, 244(%rsp)	#, %sfp
	jg	.L940	#,
	jmp	.L941	#
.L939:
	movl	4(%rsp), %eax	# %sfp, dstStride
	leal	(%rax,%rax,8), %esi	#, D.15893
	movslq	%esi, %rsi	# D.15893, D.15897
	addq	224(%rsp), %rsi	# %sfp, D.15905
	testl	%eax, %eax	# dstStride
	jle	.L942	#,
	imull	244(%rsp), %eax	# %sfp, D.15893
	movslq	%eax, %rdx	# D.15893, D.15898
	movq	160(%rsp), %rdi	# %sfp,
	call	memcpy	#
.L941:
	cmpl	$0, 24(%rsp)	#, %sfp
	jg	.L943	#,
	jmp	.L944	#
.L942:
	movl	240(%rsp), %eax	# %sfp, copyAhead
	subl	$9, %eax	#, D.15893
	movl	4(%rsp), %ebx	# %sfp, dstStride
	imull	%ebx, %eax	# dstStride, D.15893
	cltq
	movq	160(%rsp), %rcx	# %sfp, dst
	leaq	(%rcx,%rax), %rdi	#, D.15895
	movl	244(%rsp), %edx	# %sfp, D.15893
	negl	%edx	# D.15893
	imull	%ebx, %edx	# dstStride, D.15893
	movslq	%edx, %rdx	# D.15893, D.15898
	addq	%rax, %rsi	# D.15897, D.15894
	call	memcpy	#
	jmp	.L941	#
.L940:
	movl	4(%rsp), %ebx	# %sfp, dstStride
	movl	%ebx, %eax	# dstStride, dstStride
	movl	240(%rsp), %ecx	# %sfp, copyAhead
	leal	-8(%rcx), %r14d	#, D.15893
	leal	(%rbx,%rbx,8), %r13d	#, D.15892
	movl	$0, %ebx	#, ivtmp.2115
	movl	$0, %ebp	#, i
	movslq	28(%rsp), %r12	# %sfp, D.15898
	movl	%r15d, 8(%rsp)	# D.15893, %sfp
	movl	%eax, %r15d	# D.15892, D.15892
.L945:
	movslq	%ebx, %rdi	# ivtmp.2115, D.15897
	addq	160(%rsp), %rdi	# %sfp, D.15895
	leal	(%rbx,%r13), %esi	#, D.15892
	movslq	%esi, %rsi	# D.15892, D.15897
	addq	224(%rsp), %rsi	# %sfp, D.15895
	movq	%r12, %rdx	# D.15898,
	call	memcpy	#
	addl	$1, %ebp	#, i
	addl	%r15d, %ebx	# D.15892, ivtmp.2115
	cmpl	%r14d, %ebp	# D.15893, i
	jne	.L945	#,
	movl	8(%rsp), %r15d	# %sfp, D.15893
	jmp	.L941	#
.L943:
	movl	48(%rsp), %ebx	# %sfp, srcStride
	movl	%ebx, %eax	# srcStride, D.15893
	movl	244(%rsp), %r11d	# %sfp, copyAhead
	imull	%r11d, %eax	# copyAhead, D.15893
	cltq
	movq	%rax, 88(%rsp)	# D.15897, %sfp
	movq	248(%rsp), %rcx	# %sfp, D.15901
	addq	%rax, %rcx	# D.15897, D.15901
	movq	%rcx, 264(%rsp)	# D.15901, %sfp
	movl	24(%rsp), %edi	# %sfp, height
	leal	-1(%rdi), %edx	#, D.15893
	movl	%ebx, %eax	# srcStride, D.15893
	imull	%edx, %eax	# D.15893, D.15893
	cltq
	addq	184(%rsp), %rax	# %sfp, D.15894
	movq	%rax, 272(%rsp)	# D.15894, %sfp
	movl	%edx, %ecx	# D.15893, D.15893
	movl	4(%rsp), %esi	# %sfp, dstStride
	imull	%esi, %ecx	# dstStride, D.15893
	movslq	%ecx, %rax	# D.15893, D.15897
	addq	160(%rsp), %rax	# %sfp, D.15895
	movq	%rax, 280(%rsp)	# D.15895, %sfp
	leal	(%rbx,%rbx,2), %eax	#, D.15893
	sall	$2, %eax	#, tmp1109
	cltq
	movq	%rax, 112(%rsp)	# D.15897, %sfp
	movl	%esi, %eax	# dstStride, D.15893
	imull	%r11d, %eax	# copyAhead, D.15893
	cltq
	movq	%rax, 56(%rsp)	# D.15897, %sfp
	movl	%esi, %eax	# dstStride, dstStride
	sall	$2, %eax	#, D.15893
	cltq
	movq	%rax, 64(%rsp)	# D.15897, %sfp
	movl	%esi, %eax	# dstStride, dstStride
	leal	(%rsi,%rsi), %edx	#, tmp1113
	addl	%edx, %eax	# tmp1113, D.15893
	cltq
	movq	%rax, 80(%rsp)	# D.15897, %sfp
	movl	%esi, %eax	# dstStride, tmp1116
	negl	%eax	# tmp1116
	sall	$3, %eax	#, tmp1117
	movslq	%eax, %r11	# tmp1117, offset
	movq	%r11, 168(%rsp)	# offset, %sfp
	negq	%r11	# D.15897
	movq	%r11, 176(%rsp)	# D.15897, %sfp
	leal	0(,%rbx,8), %r11d	#, D.15892
	movl	%r11d, 212(%rsp)	# D.15892, %sfp
	sall	$3, %esi	#, D.15892
	movl	%esi, 216(%rsp)	# D.15892, %sfp
	movl	%edi, 292(%rsp)	# height, %sfp
	leal	1(%rdi), %esi	#, ivtmp.2106
	movl	%esi, 200(%rsp)	# ivtmp.2106, %sfp
	movl	%eax, 220(%rsp)	# tmp1117, %sfp
	leal	(%rcx,%rdx), %eax	#, ivtmp.2107
	movl	%eax, 204(%rsp)	# ivtmp.2107, %sfp
	movl	%edi, 196(%rsp)	# height, %sfp
	movl	$0, 152(%rsp)	#, %sfp
	movl	$0, 156(%rsp)	#, %sfp
	movl	$0, 16(%rsp)	#, %sfp
	movl	%ebx, %edx	# srcStride, tmp1347
	sarl	$31, %edx	#, tmp1347
	movl	%ebx, %eax	# srcStride, tmp1348
	xorl	%edx, %eax	# tmp1347, tmp1348
	subl	%edx, %eax	# tmp1347, D.15893
	cltq
	movq	%rax, 296(%rsp)	# D.15898, %sfp
.L986:
	movslq	156(%rsp), %r13	# %sfp, D.15897
	addq	184(%rsp), %r13	# %sfp, srcBlock
	movslq	152(%rsp), %rax	# %sfp, D.15897
	movq	%rax, %rbx	# D.15897, D.15897
	movq	%rax, 128(%rsp)	# D.15897, %sfp
	movq	160(%rsp), %rax	# %sfp, dstBlock
	addq	%rbx, %rax	# D.15897, dstBlock
	movq	%rax, %rbx	# dstBlock, dstBlock
	movq	%rax, 232(%rsp)	# dstBlock, %sfp
	movq	1656(%rsp), %r12	# c.tempBlocks, tempBlock2
	leaq	8(%r12), %rax	#, tempBlock2
	movq	%rax, 8(%rsp)	# tempBlock2, %sfp
	movl	16(%rsp), %esi	# %sfp, y
	movl	%esi, %eax	# y, D.15893
	movzbl	208(%rsp), %ecx	# %sfp, tmp1568
	sarl	%cl, %eax	# tmp1568, D.15893
	movl	%eax, %ecx	# D.15893, D.15893
	movl	%eax, %edx	# D.15893, D.15893
	imull	2984(%rsp), %edx	# QPStride, D.15893
	movslq	%edx, %rdx	# D.15893, D.15897
	addq	2976(%rsp), %rdx	# QPs, QPptr
	movq	%rdx, 32(%rsp)	# QPptr, %sfp
	movl	2984(%rsp), %edx	# QPStride, tmp1123
	sarl	$31, %edx	#, tmp1123
	movl	%edx, %eax	# tmp1123, tmp1124
	xorl	2984(%rsp), %eax	# QPStride, tmp1124
	subl	%edx, %eax	# tmp1123, D.15893
	imull	%ecx, %eax	# D.15893, D.15893
	cltq
	addq	2808(%rsp), %rax	# c.nonBQPTable, nonBQPptr
	movq	%rax, 40(%rsp)	# nonBQPptr, %sfp
	movl	%esi, %eax	# y, y
	movl	%esi, 52(%rsp)	# y, %sfp
	addl	$15, %eax	#, D.15893
	movl	%eax, 192(%rsp)	# D.15893, %sfp
	cmpl	%eax, 24(%rsp)	# D.15893, %sfp
	jle	.L946	#,
	jmp	.L951	#
.L994:
	movq	256(%rsp), %rbx	# %sfp, dstBlock
	movq	248(%rsp), %r13	# %sfp, srcBlock
.L951:
	movl	$0, %ebp	#, x
	cmpl	$0, 28(%rsp)	#, %sfp
	jle	.L948	#,
	jmp	.L947	#
.L946:
	movl	292(%rsp), %edx	# %sfp, D.15892
	movl	240(%rsp), %r14d	# %sfp, copyAhead
	subl	%r14d, %edx	# copyAhead, D.15892
	addl	$8, %edx	#, D.15892
	subl	16(%rsp), %edx	# %sfp, D.15892
	movl	$0, %eax	#, tmp1132
	cmovs	%eax, %edx	# D.15892,, tmp1132, D.15892
	movq	88(%rsp), %rax	# %sfp, D.15897
	leaq	0(%r13,%rax), %rsi	#, D.15905
	movl	48(%rsp), %ebp	# %sfp, srcStride
	movl	%ebp, %ecx	# srcStride,
	movq	264(%rsp), %rdi	# %sfp,
	call	linecpy	#
	movl	196(%rsp), %eax	# %sfp, ivtmp.2104
	cmpl	$8, %eax	#, ivtmp.2104
	movl	$8, %ebx	#, tmp1134
	cmovge	%eax, %ebx	# ivtmp.2104,, i
	cmpl	%ebx, %r14d	# i, copyAhead
	jle	.L949	#,
	movl	%ebp, %r13d	# srcStride, D.15892
	imull	%ebx, %ebp	# i, ivtmp.2095
.L950:
	movslq	%ebp, %rdi	# ivtmp.2095, D.15897
	addq	248(%rsp), %rdi	# %sfp, D.15895
	movq	296(%rsp), %rdx	# %sfp,
	movq	272(%rsp), %rsi	# %sfp,
	call	memcpy	#
	addl	$1, %ebx	#, i
	addl	%r13d, %ebp	# D.15892, ivtmp.2095
	cmpl	%ebx, 240(%rsp)	# i, %sfp
	jg	.L950	#,
.L949:
	movl	200(%rsp), %ebp	# %sfp, ivtmp.2106
	movl	%ebp, %ebx	# ivtmp.2106, i
	movl	240(%rsp), %eax	# %sfp, copyAhead
	subl	$7, %eax	#, D.15893
	cmpl	%eax, %ebp	# D.15893, ivtmp.2106
	cmovle	%ebp, %eax	# ivtmp.2106,, D.15893
	movl	%eax, %edx	# D.15893, D.15893
	movq	232(%rsp), %rsi	# %sfp, D.15895
	subq	72(%rsp), %rsi	# %sfp, D.15895
	movl	4(%rsp), %r14d	# %sfp, dstStride
	movl	%r14d, %ecx	# dstStride,
	movq	224(%rsp), %rdi	# %sfp,
	call	linecpy	#
	cmpl	%ebp, 244(%rsp)	# ivtmp.2106, %sfp
	jl	.L994	#,
	movl	204(%rsp), %ebp	# %sfp, ivtmp.2088
	movslq	288(%rsp), %r13	# %sfp, D.15898
.L952:
	movslq	%ebp, %rdi	# ivtmp.2088, D.15897
	addq	224(%rsp), %rdi	# %sfp, D.15895
	movq	%r13, %rdx	# D.15898,
	movq	280(%rsp), %rsi	# %sfp,
	call	memcpy	#
	addl	$1, %ebx	#, i
	addl	%r14d, %ebp	# D.15892, ivtmp.2088
	cmpl	%ebx, 244(%rsp)	# i, %sfp
	jge	.L952	#,
	movq	256(%rsp), %rbx	# %sfp, dstBlock
	movq	248(%rsp), %r13	# %sfp, srcBlock
	jmp	.L951	#
.L947:
	movl	16(%rsp), %eax	# %sfp, D.15893
	sarl	$3, %eax	#, D.15893
	sall	$8, %eax	#, D.15893
	cltq
	addq	$256, %rax	#, D.15897
	movq	%rax, 144(%rsp)	# D.15897, %sfp
	addq	88(%rsp), %r13	# %sfp, ivtmp.2080
	movl	$0, %ebp	#, x
	leaq	1672(%rsp), %rax	#, tmp1339
	movq	%rax, 120(%rsp)	# tmp1339, %sfp
	movl	%r15d, %eax	# D.15893, tmp1340
	shrl	$2, %eax	#, tmp1340
	andl	$1, %eax	#, D.15896
	movb	%al, 107(%rsp)	# D.15896, %sfp
	jmp	.L979	#
.L995:
	movq	%rax, %r12	# tempBlock2, tempBlock2
.L979:
	cmpl	$0, 2992(%rsp)	#, isColor
	je	.L953	#,
	movl	%ebp, %edx	# x, D.15893
	movzbl	136(%rsp), %ecx	# %sfp, tmp1609
	sarl	%cl, %edx	# tmp1609, D.15893
	movslq	%edx, %rdx	# D.15893, D.15897
	movq	32(%rsp), %rax	# %sfp, QPptr
	movsbl	(%rax,%rdx), %eax	# *_295, QP
	movq	40(%rsp), %rsi	# %sfp, nonBQPptr
	movsbl	(%rsi,%rdx), %edx	# *_298, *_298
	movl	%edx, 2828(%rsp)	# *_298, c.nonBQP
	jmp	.L954	#
.L953:
	movl	%ebp, %edx	# x, D.15893
	sarl	$4, %edx	#, D.15893
	movslq	%edx, %rdx	# D.15893, D.15897
	movq	32(%rsp), %rax	# %sfp, QPptr
	movsbl	(%rax,%rdx), %eax	# *_304, QP
	movl	108(%rsp), %edi	# %sfp, QPCorrecture
	imull	%edi, %eax	# QPCorrecture, D.15893
	addl	$32768, %eax	#, D.15893
	sarl	$16, %eax	#, QP
	movq	40(%rsp), %rcx	# %sfp, nonBQPptr
	movsbl	(%rcx,%rdx), %edx	# *_310, D.15893
	imull	%edi, %edx	# QPCorrecture, D.15893
	addl	$32768, %edx	#, D.15893
	sarl	$16, %edx	#, tmp1168
	movl	%edx, 2828(%rsp)	# tmp1168, c.nonBQP
	movq	%r13, %rdx	# ivtmp.2080, D.15897
	subq	88(%rsp), %rdx	# %sfp, D.15897
	movq	112(%rsp), %rcx	# %sfp, D.15897
	movzbl	4(%rdx,%rcx), %edx	# MEM[base: _384, index: _319, offset: 4B], D.15898
	movq	96(%rsp), %rsi	# %sfp, yHistogram
	addq	$1, (%rsi,%rdx,8)	#, *_325
.L954:
	movl	%eax, 2824(%rsp)	# QP, c.QP
#APP
# 3497 "postprocess_template.c" 1
	movd %eax, %mm7         	# QP
	packuswb %mm7, %mm7  
	packuswb %mm7, %mm7  
	packuswb %mm7, %mm7  
	movq %mm7, 1760(%rsp)         	# c.pQPb
	
# 0 "" 2
#NO_APP
	movq	56(%rsp), %rax	# %sfp, D.15897
	leaq	(%rbx,%rax), %rsi	#, D.15895
	testb	$8, %r15b	#, D.15893
	je	.L955	#,
	movslq	48(%rsp), %rcx	# %sfp, D.15906
	movslq	4(%rsp), %rdi	# %sfp, D.15906
	movq	120(%rsp), %rax	# %sfp, packedOffsetAndScale
#APP
# 3102 "postprocess_template.c" 1
	movq (%rax), %mm2        
	movq 8(%rax), %mm3       
	lea (%r13,%rcx), %rax         	# ivtmp.2080, D.15906
	lea (%rsi,%rdi), %rdx         	# D.15895, D.15906
	pxor %mm4, %mm4              
	movq (%r13), %mm0          	# ivtmp.2080
	movq (%r13), %mm5          	# ivtmp.2080
	punpcklbw %mm4, %mm0         
	punpckhbw %mm4, %mm5         
	psubw %mm2, %mm0             
	psubw %mm2, %mm5             
	movq (%r13, %rcx), %mm1          	# ivtmp.2080, D.15906
	psllw $6, %mm0                
	psllw $6, %mm5                
	pmulhw %mm3, %mm0            
	movq (%r13, %rcx), %mm6          	# ivtmp.2080, D.15906
	pmulhw %mm3, %mm5            
	punpcklbw %mm4, %mm1         
	punpckhbw %mm4, %mm6         
	psubw %mm2, %mm1             
	psubw %mm2, %mm6             
	psllw $6, %mm1                
	psllw $6, %mm6                
	pmulhw %mm3, %mm1            
	pmulhw %mm3, %mm6            
	packuswb %mm5, %mm0          
	packuswb %mm6, %mm1          
	movq %mm0, (%rsi)          	# D.15895
	movq %mm1, (%rsi, %rdi)          	# D.15895, D.15906
	movq (%r13, %rcx, 2), %mm0          	# ivtmp.2080, D.15906
	movq (%r13, %rcx, 2), %mm5          	# ivtmp.2080, D.15906
	punpcklbw %mm4, %mm0         
	punpckhbw %mm4, %mm5         
	psubw %mm2, %mm0             
	psubw %mm2, %mm5             
	movq (%rax, %rcx, 2), %mm1          	# D.15906
	psllw $6, %mm0                
	psllw $6, %mm5                
	pmulhw %mm3, %mm0            
	movq (%rax, %rcx, 2), %mm6          	# D.15906
	pmulhw %mm3, %mm5            
	punpcklbw %mm4, %mm1         
	punpckhbw %mm4, %mm6         
	psubw %mm2, %mm1             
	psubw %mm2, %mm6             
	psllw $6, %mm1                
	psllw $6, %mm6                
	pmulhw %mm3, %mm1            
	pmulhw %mm3, %mm6            
	packuswb %mm5, %mm0          
	packuswb %mm6, %mm1          
	movq %mm0, (%rsi, %rdi, 2)          	# D.15895, D.15906
	movq %mm1, (%rdx, %rdi, 2)          	# D.15906
	movq (%r13, %rcx, 4), %mm0          	# ivtmp.2080, D.15906
	movq (%r13, %rcx, 4), %mm5          	# ivtmp.2080, D.15906
	punpcklbw %mm4, %mm0         
	punpckhbw %mm4, %mm5         
	psubw %mm2, %mm0             
	psubw %mm2, %mm5             
	movq (%rax, %rcx, 4), %mm1          	# D.15906
	psllw $6, %mm0                
	psllw $6, %mm5                
	pmulhw %mm3, %mm0            
	movq (%rax, %rcx, 4), %mm6          	# D.15906
	pmulhw %mm3, %mm5            
	punpcklbw %mm4, %mm1         
	punpckhbw %mm4, %mm6         
	psubw %mm2, %mm1             
	psubw %mm2, %mm6             
	psllw $6, %mm1                
	psllw $6, %mm6                
	pmulhw %mm3, %mm1            
	pmulhw %mm3, %mm6            
	packuswb %mm5, %mm0          
	packuswb %mm6, %mm1          
	movq %mm0, (%rsi, %rdi, 4)          	# D.15895, D.15906
	movq %mm1, (%rdx, %rdi, 4)          	# D.15906
	lea (%rax,%rcx,4), %rax        	# D.15906
	lea (%rdx,%rdi,4), %rdx        	# D.15906
	movq (%rax, %rcx), %mm0          	# D.15906
	movq (%rax, %rcx), %mm5          	# D.15906
	punpcklbw %mm4, %mm0         
	punpckhbw %mm4, %mm5         
	psubw %mm2, %mm0             
	psubw %mm2, %mm5             
	movq (%rax, %rcx, 2), %mm1          	# D.15906
	psllw $6, %mm0                
	psllw $6, %mm5                
	pmulhw %mm3, %mm0            
	movq (%rax, %rcx, 2), %mm6          	# D.15906
	pmulhw %mm3, %mm5            
	punpcklbw %mm4, %mm1         
	punpckhbw %mm4, %mm6         
	psubw %mm2, %mm1             
	psubw %mm2, %mm6             
	psllw $6, %mm1                
	psllw $6, %mm6                
	pmulhw %mm3, %mm1            
	pmulhw %mm3, %mm6            
	packuswb %mm5, %mm0          
	packuswb %mm6, %mm1          
	movq %mm0, (%rdx, %rdi)          	# D.15906
	movq %mm1, (%rdx, %rdi, 2)          	# D.15906
	
# 0 "" 2
#NO_APP
	jmp	.L956	#
.L955:
	movslq	48(%rsp), %rcx	# %sfp, D.15906
	movslq	4(%rsp), %rdi	# %sfp, D.15906
#APP
# 3185 "postprocess_template.c" 1
	lea (%r13,%rcx), %rax                 	# ivtmp.2080, D.15906
	lea (%rsi,%rdi), %rdx                 	# D.15895, D.15906
	movq (%r13), %mm0          	# ivtmp.2080
	movq (%r13, %rcx), %mm1          	# ivtmp.2080, D.15906
	movq %mm0, (%rsi)          	# D.15895
	movq %mm1, (%rsi, %rdi)          	# D.15895, D.15906
	movq (%r13, %rcx, 2), %mm0          	# ivtmp.2080, D.15906
	movq (%rax, %rcx, 2), %mm1          	# D.15906
	movq %mm0, (%rsi, %rdi, 2)          	# D.15895, D.15906
	movq %mm1, (%rdx, %rdi, 2)          	# D.15906
	movq (%r13, %rcx, 4), %mm0          	# ivtmp.2080, D.15906
	movq (%rax, %rcx, 4), %mm1          	# D.15906
	movq %mm0, (%rsi, %rdi, 4)          	# D.15895, D.15906
	movq %mm1, (%rdx, %rdi, 4)          	# D.15906
	lea (%rax,%rcx,4), %rax        	# D.15906
	lea (%rdx,%rdi,4), %rdx        	# D.15906
	movq (%rax, %rcx), %mm0          	# D.15906
	movq (%rax, %rcx, 2), %mm1          	# D.15906
	movq %mm0, (%rdx, %rdi)          	# D.15906
	movq %mm1, (%rdx, %rdi, 2)          	# D.15906
	
# 0 "" 2
#NO_APP
.L956:
	testl	$65536, %r15d	#, D.15893
	je	.L957	#,
	movq	64(%rsp), %rax	# %sfp, D.15897
	leaq	(%rbx,%rax), %rdx	#, D.15895
	movslq	4(%rsp), %rsi	# %sfp, D.15906
#APP
# 1455 "postprocess_template.c" 1
	lea (%rdx, %rsi), %rax                	# D.15895, D.15906
	lea (%rax, %rsi, 4), %rcx      	# D.15906
	movq (%rdx), %mm0                       	# D.15895
	movq (%rax, %rsi), %mm1            	# D.15906
	pavgusb %mm1, %mm0 
	movq %mm0, (%rax)                
	movq (%rdx, %rsi, 4), %mm0                	# D.15895, D.15906
	pavgusb %mm0, %mm1 
	movq %mm1, (%rax, %rsi, 2)         	# D.15906
	movq (%rcx, %rsi), %mm1            	# D.15906
	pavgusb %mm1, %mm0 
	movq %mm0, (%rcx)                
	movq (%rdx, %rsi, 8), %mm0                	# D.15895, D.15906
	pavgusb %mm0, %mm1 
	movq %mm1, (%rcx, %rsi, 2)         	# D.15906
	
# 0 "" 2
#NO_APP
	jmp	.L958	#
.L957:
	testl	$131072, %r15d	#, D.15893
	je	.L959	#,
	movq	64(%rsp), %rax	# %sfp, D.15897
	leaq	(%rbx,%rax), %rdi	#, D.15895
	movslq	4(%rsp), %rcx	# %sfp, D.15906
	movslq	%ebp, %rsi	# x, D.15897
	addq	1752(%rsp), %rsi	# c.deintTemp, D.15895
#APP
# 1775 "postprocess_template.c" 1
	lea (%rdi, %rcx), %rax                	# D.15895, D.15906
	lea (%rax, %rcx, 4), %rdx      	# D.15906
	movq (%rsi), %mm0                       	# D.15895
	movq (%rax), %mm1                
	pavgusb %mm1, %mm0 
	movq (%rdi), %mm2                       	# D.15895
	pavgusb %mm2, %mm0 
	movq %mm0, (%rdi)                       	# D.15895
	movq (%rax, %rcx), %mm0            	# D.15906
	pavgusb %mm0, %mm2 
	pavgusb %mm1, %mm2 
	movq %mm2, (%rax)                
	movq (%rax, %rcx, 2), %mm2         	# D.15906
	pavgusb %mm2, %mm1 
	pavgusb %mm0, %mm1 
	movq %mm1, (%rax, %rcx)            	# D.15906
	movq (%rdi, %rcx, 4), %mm1                	# D.15895, D.15906
	pavgusb %mm1, %mm0 
	pavgusb %mm2, %mm0 
	movq %mm0, (%rax, %rcx, 2)         	# D.15906
	movq (%rdx), %mm0                
	pavgusb %mm0, %mm2 
	pavgusb %mm1, %mm2 
	movq %mm2, (%rdi, %rcx, 4)                	# D.15895, D.15906
	movq (%rdx, %rcx), %mm2            	# D.15906
	pavgusb %mm2, %mm1 
	pavgusb %mm0, %mm1 
	movq %mm1, (%rdx)                
	movq (%rdx, %rcx, 2), %mm1         	# D.15906
	pavgusb %mm1, %mm0 
	pavgusb %mm2, %mm0 
	movq %mm0, (%rdx, %rcx)            	# D.15906
	movq (%rdi, %rcx, 8), %mm0                	# D.15895, D.15906
	pavgusb %mm0, %mm2 
	pavgusb %mm1, %mm2 
	movq %mm2, (%rdx, %rcx, 2)         	# D.15906
	movq %mm1, (%rsi)                       	# D.15895
	
# 0 "" 2
#NO_APP
	jmp	.L958	#
.L959:
	testl	$524288, %r15d	#, D.15893
	je	.L960	#,
	movq	64(%rsp), %rax	# %sfp, D.15897
	leaq	(%rbx,%rax), %rcx	#, D.15895
	movslq	4(%rsp), %rsi	# %sfp, D.15906
#APP
# 1926 "postprocess_template.c" 1
	lea (%rcx, %rsi), %rax                	# D.15895, D.15906
	lea (%rax, %rsi, 4), %rdx      	# D.15906
	pxor %mm7, %mm7                      
	movq (%rcx), %mm0                     	# D.15895
	movq (%rax), %mm2                     
	movq (%rax, %rsi), %mm1                     	# D.15906
	movq %mm0, %mm3                      
	movq %mm1, %mm4                      
	movq %mm2, %mm5                      
	psubusb %mm1, %mm3                   
	psubusb %mm2, %mm4                   
	psubusb %mm0, %mm5                   
	pcmpeqb %mm7, %mm3                   
	pcmpeqb %mm7, %mm4                   
	pcmpeqb %mm7, %mm5                   
	movq %mm3, %mm6                      
	pxor %mm4, %mm3                      
	pxor %mm5, %mm4                      
	pxor %mm6, %mm5                      
	por %mm3, %mm1                       
	por %mm4, %mm2                       
	por %mm5, %mm0                       
	pand %mm2, %mm0                      
	pand %mm1, %mm0                      
	movq %mm0, (%rax)                     
	movq (%rax, %rsi), %mm0                     	# D.15906
	movq (%rax, %rsi, 2), %mm2                     	# D.15906
	movq (%rcx, %rsi, 4), %mm1                     	# D.15895, D.15906
	movq %mm0, %mm3                      
	movq %mm1, %mm4                      
	movq %mm2, %mm5                      
	psubusb %mm1, %mm3                   
	psubusb %mm2, %mm4                   
	psubusb %mm0, %mm5                   
	pcmpeqb %mm7, %mm3                   
	pcmpeqb %mm7, %mm4                   
	pcmpeqb %mm7, %mm5                   
	movq %mm3, %mm6                      
	pxor %mm4, %mm3                      
	pxor %mm5, %mm4                      
	pxor %mm6, %mm5                      
	por %mm3, %mm1                       
	por %mm4, %mm2                       
	por %mm5, %mm0                       
	pand %mm2, %mm0                      
	pand %mm1, %mm0                      
	movq %mm0, (%rax, %rsi, 2)                     	# D.15906
	movq (%rcx, %rsi, 4), %mm0                     	# D.15895, D.15906
	movq (%rdx), %mm2                     
	movq (%rdx, %rsi), %mm1                     	# D.15906
	movq %mm0, %mm3                      
	movq %mm1, %mm4                      
	movq %mm2, %mm5                      
	psubusb %mm1, %mm3                   
	psubusb %mm2, %mm4                   
	psubusb %mm0, %mm5                   
	pcmpeqb %mm7, %mm3                   
	pcmpeqb %mm7, %mm4                   
	pcmpeqb %mm7, %mm5                   
	movq %mm3, %mm6                      
	pxor %mm4, %mm3                      
	pxor %mm5, %mm4                      
	pxor %mm6, %mm5                      
	por %mm3, %mm1                       
	por %mm4, %mm2                       
	por %mm5, %mm0                       
	pand %mm2, %mm0                      
	pand %mm1, %mm0                      
	movq %mm0, (%rdx)                     
	movq (%rdx, %rsi), %mm0                     	# D.15906
	movq (%rdx, %rsi, 2), %mm2                     	# D.15906
	movq (%rcx, %rsi, 8), %mm1                     	# D.15895, D.15906
	movq %mm0, %mm3                      
	movq %mm1, %mm4                      
	movq %mm2, %mm5                      
	psubusb %mm1, %mm3                   
	psubusb %mm2, %mm4                   
	psubusb %mm0, %mm5                   
	pcmpeqb %mm7, %mm3                   
	pcmpeqb %mm7, %mm4                   
	pcmpeqb %mm7, %mm5                   
	movq %mm3, %mm6                      
	pxor %mm4, %mm3                      
	pxor %mm5, %mm4                      
	pxor %mm6, %mm5                      
	por %mm3, %mm1                       
	por %mm4, %mm2                       
	por %mm5, %mm0                       
	pand %mm2, %mm0                      
	pand %mm1, %mm0                      
	movq %mm0, (%rdx, %rsi, 2)                     	# D.15906
	
# 0 "" 2
#NO_APP
	jmp	.L958	#
.L960:
	cmpl	$0, 140(%rsp)	#, %sfp
	je	.L961	#,
	movq	80(%rsp), %rax	# %sfp, D.15897
	leaq	(%rbx,%rax), %rsi	#, D.15895
	movslq	4(%rsp), %rdi	# %sfp, D.15906
#APP
# 1508 "postprocess_template.c" 1
	lea (%rsi, %rdi), %rax                	# D.15895, D.15906
	lea (%rax, %rdi, 4), %rdx      	# D.15906
	lea (%rdx, %rdi, 4), %rcx      	# D.15906
	add %rdi, %rcx                      	# D.15906
	pxor %mm7, %mm7                      
	movq (%rsi), %mm0                     	# D.15895
	movq (%rax, %rdi), %mm1                     	# D.15906
	movq (%rsi, %rdi, 4), %mm2                     	# D.15895, D.15906
	movq (%rdx, %rdi), %mm3                     	# D.15906
	pavgusb %mm2, %mm1 
	pavgusb %mm3, %mm0 
	movq %mm0, %mm2                      
	punpcklbw %mm7, %mm0                 
	punpckhbw %mm7, %mm2                 
	movq %mm1, %mm3                      
	punpcklbw %mm7, %mm1                 
	punpckhbw %mm7, %mm3                 
	psubw %mm1, %mm0                     
	psubw %mm3, %mm2                     
	psraw $3, %mm0                        
	psraw $3, %mm2                        
	psubw %mm0, %mm1                     
	psubw %mm2, %mm3                     
	packuswb %mm3, %mm1                  
	movq %mm1, (%rax, %rdi, 2)                     	# D.15906
	movq (%rax, %rdi), %mm0                     	# D.15906
	movq (%rsi, %rdi, 4), %mm1                     	# D.15895, D.15906
	movq (%rdx, %rdi), %mm2                     	# D.15906
	movq (%rsi, %rdi, 8), %mm3                     	# D.15895, D.15906
	pavgusb %mm2, %mm1 
	pavgusb %mm3, %mm0 
	movq %mm0, %mm2                      
	punpcklbw %mm7, %mm0                 
	punpckhbw %mm7, %mm2                 
	movq %mm1, %mm3                      
	punpcklbw %mm7, %mm1                 
	punpckhbw %mm7, %mm3                 
	psubw %mm1, %mm0                     
	psubw %mm3, %mm2                     
	psraw $3, %mm0                        
	psraw $3, %mm2                        
	psubw %mm0, %mm1                     
	psubw %mm2, %mm3                     
	packuswb %mm3, %mm1                  
	movq %mm1, (%rdx)                     
	movq (%rsi, %rdi, 4), %mm0                     	# D.15895, D.15906
	movq (%rdx, %rdi), %mm1                     	# D.15906
	movq (%rsi, %rdi, 8), %mm2                     	# D.15895, D.15906
	movq (%rcx), %mm3                     
	pavgusb %mm2, %mm1 
	pavgusb %mm3, %mm0 
	movq %mm0, %mm2                      
	punpcklbw %mm7, %mm0                 
	punpckhbw %mm7, %mm2                 
	movq %mm1, %mm3                      
	punpcklbw %mm7, %mm1                 
	punpckhbw %mm7, %mm3                 
	psubw %mm1, %mm0                     
	psubw %mm3, %mm2                     
	psraw $3, %mm0                        
	psraw $3, %mm2                        
	psubw %mm0, %mm1                     
	psubw %mm2, %mm3                     
	packuswb %mm3, %mm1                  
	movq %mm1, (%rdx, %rdi, 2)                     	# D.15906
	movq (%rdx, %rdi), %mm0                     	# D.15906
	movq (%rsi, %rdi, 8), %mm1                     	# D.15895, D.15906
	movq (%rcx), %mm2                     
	movq (%rcx, %rdi, 2), %mm3                     	# D.15906
	pavgusb %mm2, %mm1 
	pavgusb %mm3, %mm0 
	movq %mm0, %mm2                      
	punpcklbw %mm7, %mm0                 
	punpckhbw %mm7, %mm2                 
	movq %mm1, %mm3                      
	punpcklbw %mm7, %mm1                 
	punpckhbw %mm7, %mm3                 
	psubw %mm1, %mm0                     
	psubw %mm3, %mm2                     
	psraw $3, %mm0                        
	psraw $3, %mm2                        
	psubw %mm0, %mm1                     
	psubw %mm2, %mm3                     
	packuswb %mm3, %mm1                  
	movq %mm1, (%rdx, %rdi, 4)                     	# D.15906
	
# 0 "" 2
#NO_APP
	jmp	.L958	#
.L961:
	testl	$4194304, %r15d	#, D.15893
	je	.L962	#,
	movq	64(%rsp), %rax	# %sfp, D.15897
	leaq	(%rbx,%rax), %rdi	#, D.15895
	movslq	4(%rsp), %rcx	# %sfp, D.15906
	movslq	%ebp, %rsi	# x, D.15897
	addq	1752(%rsp), %rsi	# c.deintTemp, D.15895
#APP
# 1595 "postprocess_template.c" 1
	lea (%rdi, %rcx), %rax                	# D.15895, D.15906
	lea (%rax, %rcx, 4), %rdx      	# D.15906
	pxor %mm7, %mm7                      
	movq (%rsi), %mm0                       	# D.15895
	movq (%rdi), %mm1                     	# D.15895
	movq (%rax), %mm2                     
	movq (%rax, %rcx), %mm3                     	# D.15906
	movq (%rax, %rcx, 2), %mm4                     	# D.15906
	pavgusb %mm3, %mm1 
	pavgusb %mm4, %mm0 
	movq %mm0, %mm3                      
	punpcklbw %mm7, %mm0                 
	punpckhbw %mm7, %mm3                 
	movq %mm1, %mm4                      
	punpcklbw %mm7, %mm1                 
	punpckhbw %mm7, %mm4                 
	psllw $2, %mm1                        
	psllw $2, %mm4                        
	psubw %mm0, %mm1                     
	psubw %mm3, %mm4                     
	movq %mm2, %mm5                      
	movq %mm2, %mm0                      
	punpcklbw %mm7, %mm2                 
	punpckhbw %mm7, %mm5                 
	paddw %mm2, %mm1                     
	paddw %mm5, %mm4                     
	psraw $2, %mm1                        
	psraw $2, %mm4                        
	packuswb %mm4, %mm1                  
	movq %mm1, (%rax)                     
	movq (%rax, %rcx), %mm1                     	# D.15906
	movq (%rax, %rcx, 2), %mm2                     	# D.15906
	movq (%rdi, %rcx, 4), %mm3                     	# D.15895, D.15906
	movq (%rdx), %mm4                     
	pavgusb %mm3, %mm1 
	pavgusb %mm4, %mm0 
	movq %mm0, %mm3                      
	punpcklbw %mm7, %mm0                 
	punpckhbw %mm7, %mm3                 
	movq %mm1, %mm4                      
	punpcklbw %mm7, %mm1                 
	punpckhbw %mm7, %mm4                 
	psllw $2, %mm1                        
	psllw $2, %mm4                        
	psubw %mm0, %mm1                     
	psubw %mm3, %mm4                     
	movq %mm2, %mm5                      
	movq %mm2, %mm0                      
	punpcklbw %mm7, %mm2                 
	punpckhbw %mm7, %mm5                 
	paddw %mm2, %mm1                     
	paddw %mm5, %mm4                     
	psraw $2, %mm1                        
	psraw $2, %mm4                        
	packuswb %mm4, %mm1                  
	movq %mm1, (%rax, %rcx, 2)                     	# D.15906
	movq (%rdi, %rcx, 4), %mm1                     	# D.15895, D.15906
	movq (%rdx), %mm2                     
	movq (%rdx, %rcx), %mm3                     	# D.15906
	movq (%rdx, %rcx, 2), %mm4                     	# D.15906
	pavgusb %mm3, %mm1 
	pavgusb %mm4, %mm0 
	movq %mm0, %mm3                      
	punpcklbw %mm7, %mm0                 
	punpckhbw %mm7, %mm3                 
	movq %mm1, %mm4                      
	punpcklbw %mm7, %mm1                 
	punpckhbw %mm7, %mm4                 
	psllw $2, %mm1                        
	psllw $2, %mm4                        
	psubw %mm0, %mm1                     
	psubw %mm3, %mm4                     
	movq %mm2, %mm5                      
	movq %mm2, %mm0                      
	punpcklbw %mm7, %mm2                 
	punpckhbw %mm7, %mm5                 
	paddw %mm2, %mm1                     
	paddw %mm5, %mm4                     
	psraw $2, %mm1                        
	psraw $2, %mm4                        
	packuswb %mm4, %mm1                  
	movq %mm1, (%rdx)                     
	movq (%rdx, %rcx), %mm1                     	# D.15906
	movq (%rdx, %rcx, 2), %mm2                     	# D.15906
	movq (%rdi, %rcx, 8), %mm3                     	# D.15895, D.15906
	movq (%rdx, %rcx, 4), %mm4                     	# D.15906
	pavgusb %mm3, %mm1 
	pavgusb %mm4, %mm0 
	movq %mm0, %mm3                      
	punpcklbw %mm7, %mm0                 
	punpckhbw %mm7, %mm3                 
	movq %mm1, %mm4                      
	punpcklbw %mm7, %mm1                 
	punpckhbw %mm7, %mm4                 
	psllw $2, %mm1                        
	psllw $2, %mm4                        
	psubw %mm0, %mm1                     
	psubw %mm3, %mm4                     
	movq %mm2, %mm5                      
	movq %mm2, %mm0                      
	punpcklbw %mm7, %mm2                 
	punpckhbw %mm7, %mm5                 
	paddw %mm2, %mm1                     
	paddw %mm5, %mm4                     
	psraw $2, %mm1                        
	psraw $2, %mm4                        
	packuswb %mm4, %mm1                  
	movq %mm1, (%rdx, %rcx, 2)                     	# D.15906
	movq %mm0, (%rsi)                       	# D.15895
	
# 0 "" 2
#NO_APP
	jmp	.L958	#
.L962:
	testl	$8388608, %r15d	#, D.15893
	je	.L958	#,
	movq	1752(%rsp), %rdx	# c.deintTemp, D.15895
	movslq	%ebp, %rax	# x, D.15897
	movslq	28(%rsp), %rcx	# %sfp, D.15897
	addq	%rax, %rcx	# D.15897, D.15897
	addq	%rdx, %rcx	# D.15895, D.15895
	addq	%rax, %rdx	# D.15897, D.15895
	movl	4(%rsp), %esi	# %sfp,
	movq	%rbx, %rdi	# dstBlock,
	call	deInterlaceL5_3DNow	#
.L958:
	movl	52(%rsp), %eax	# %sfp, D.15892
	addl	$8, %eax	#, D.15892
	cmpl	%eax, 24(%rsp)	# D.15892, %sfp
	jle	.L963	#,
	testl	$512, %r15d	#, D.15893
	je	.L964	#,
	movq	80(%rsp), %rax	# %sfp, D.15897
	leaq	(%rbx,%rax), %rdx	#, D.15895
	movslq	4(%rsp), %rsi	# %sfp, D.15906
#APP
# 412 "postprocess_template.c" 1
	pxor %mm7, %mm7                      
	lea (%rdx, %rsi), %rax                	# D.15895, D.15906
	lea (%rax, %rsi, 4), %rcx      	# D.15906
	movq (%rax, %rsi, 2), %mm0         	# D.15906
	movq (%rdx, %rsi, 4), %mm1                	# D.15895, D.15906
	movq %mm1, %mm2                      
	psubusb %mm0, %mm1                   
	psubusb %mm2, %mm0                   
	por %mm1, %mm0                       
	movq (%rcx), %mm3                
	movq (%rcx, %rsi), %mm4            	# D.15906
	movq %mm3, %mm5                      
	psubusb %mm4, %mm3                   
	psubusb %mm5, %mm4                   
	por %mm4, %mm3                       
	pavgusb %mm3, %mm0 
	movq %mm2, %mm1                      
	psubusb %mm5, %mm2                   
	movq %mm2, %mm4                      
	pcmpeqb %mm7, %mm2                   
	psubusb %mm1, %mm5                   
	por %mm5, %mm4                       
	psubusb %mm0, %mm4                   
	movq %mm4, %mm3                      
	movq 1760(%rsp), %mm0                         	# c.pQPb
	paddusb %mm0, %mm0                   
	psubusb %mm0, %mm4                   
	pcmpeqb %mm7, %mm4                   
	psubusb b01, %mm3           
	pand %mm4, %mm3                      
	pavgusb %mm7, %mm3 
	movq %mm3, %mm1                      
	pavgusb %mm7, %mm3 
	pavgusb %mm1, %mm3 
	movq (%rdx, %rsi, 4), %mm0                	# D.15895, D.15906
	pxor %mm2, %mm0                      
	psubusb %mm3, %mm0                   
	pxor %mm2, %mm0                      
	movq %mm0, (%rdx, %rsi, 4)                	# D.15895, D.15906
	movq (%rcx), %mm0                
	pxor %mm2, %mm0                      
	paddusb %mm3, %mm0                   
	pxor %mm2, %mm0                      
	movq %mm0, (%rcx)                
	pavgusb %mm7, %mm1 
	movq (%rax, %rsi, 2), %mm0         	# D.15906
	pxor %mm2, %mm0                      
	psubusb %mm1, %mm0                   
	pxor %mm2, %mm0                      
	movq %mm0, (%rax, %rsi, 2)         	# D.15906
	movq (%rcx, %rsi), %mm0            	# D.15906
	pxor %mm2, %mm0                      
	paddusb %mm1, %mm0                   
	pxor %mm2, %mm0                      
	movq %mm0, (%rcx, %rsi)            	# D.15906
	pavgusb %mm7, %mm1 
	movq (%rax, %rsi), %mm0            	# D.15906
	pxor %mm2, %mm0                      
	psubusb %mm1, %mm0                   
	pxor %mm2, %mm0                      
	movq %mm0, (%rax, %rsi)            	# D.15906
	movq (%rcx, %rsi, 2), %mm0         	# D.15906
	pxor %mm2, %mm0                      
	paddusb %mm1, %mm0                   
	pxor %mm2, %mm0                      
	movq %mm0, (%rcx, %rsi, 2)         	# D.15906
	
# 0 "" 2
#NO_APP
	jmp	.L963	#
.L964:
	testb	$1, %r15b	#, D.15893
	je	.L965	#,
	movq	64(%rsp), %rax	# %sfp, D.15897
	leaq	(%rbx,%rax), %rsi	#, D.15894
	movslq	2828(%rsp), %rax	# c.nonBQP, D.15893
#APP
# 114 "postprocess_template.c" 1
	movq 1776(%rsp,%rax,8), %mm7                         	# c.mmxDcOffset
	movq 2288(%rsp,%rax,8), %mm6                         	# c.mmxDcThreshold
	
# 0 "" 2
#NO_APP
	movslq	4(%rsp), %rdi	# %sfp, D.15906
#APP
# 120 "postprocess_template.c" 1
	lea (%rsi, %rdi), %rax                	# D.15894, D.15906
	movq (%rsi), %mm0                       	# D.15894
	movq (%rax), %mm1                
	movq %mm0, %mm3                      
	movq %mm0, %mm4                      
	psubusb %mm1, %mm4 
	paddb %mm1, %mm4 
	movq %mm3, %mm5 
	psubusb %mm1, %mm5 
	psubb %mm5, %mm3 
	psubb %mm1, %mm0                     
	paddb %mm7, %mm0                     
	pcmpgtb %mm6, %mm0                   
	movq (%rax,%rdi), %mm2             	# D.15906
	psubusb %mm2, %mm4 
	paddb %mm2, %mm4 
	movq %mm3, %mm5 
	psubusb %mm2, %mm5 
	psubb %mm5, %mm3 
	psubb %mm2, %mm1                     
	paddb %mm7, %mm1                     
	pcmpgtb %mm6, %mm1                   
	paddb %mm1, %mm0                     
	movq (%rax, %rdi, 2), %mm1         	# D.15906
	psubusb %mm1, %mm4 
	paddb %mm1, %mm4 
	movq %mm3, %mm5 
	psubusb %mm1, %mm5 
	psubb %mm5, %mm3 
	psubb %mm1, %mm2                     
	paddb %mm7, %mm2                     
	pcmpgtb %mm6, %mm2                   
	paddb %mm2, %mm0                     
	lea (%rax, %rdi, 4), %rax      	# D.15906
	movq (%rsi, %rdi, 4), %mm2                	# D.15894, D.15906
	psubusb %mm2, %mm4 
	paddb %mm2, %mm4 
	movq %mm3, %mm5 
	psubusb %mm2, %mm5 
	psubb %mm5, %mm3 
	psubb %mm2, %mm1                     
	paddb %mm7, %mm1                     
	pcmpgtb %mm6, %mm1                   
	paddb %mm1, %mm0                     
	movq (%rax), %mm1                
	psubusb %mm1, %mm4 
	paddb %mm1, %mm4 
	movq %mm3, %mm5 
	psubusb %mm1, %mm5 
	psubb %mm5, %mm3 
	psubb %mm1, %mm2                     
	paddb %mm7, %mm2                     
	pcmpgtb %mm6, %mm2                   
	paddb %mm2, %mm0                     
	movq (%rax, %rdi), %mm2            	# D.15906
	psubusb %mm2, %mm4 
	paddb %mm2, %mm4 
	movq %mm3, %mm5 
	psubusb %mm2, %mm5 
	psubb %mm5, %mm3 
	psubb %mm2, %mm1                     
	paddb %mm7, %mm1                     
	pcmpgtb %mm6, %mm1                   
	paddb %mm1, %mm0                     
	movq (%rax, %rdi, 2), %mm1         	# D.15906
	psubusb %mm1, %mm4 
	paddb %mm1, %mm4 
	movq %mm3, %mm5 
	psubusb %mm1, %mm5 
	psubb %mm5, %mm3 
	psubb %mm1, %mm2                     
	paddb %mm7, %mm2                     
	pcmpgtb %mm6, %mm2                   
	paddb %mm2, %mm0                     
	psubusb %mm3, %mm4                   
	                                       
	movq %mm0, %mm1                      
	psrlw $8, %mm0                        
	paddb %mm1, %mm0                     
	movq %mm0, %mm1                      
	psrlq $16, %mm0                       
	paddb %mm1, %mm0                     
	movq %mm0, %mm1                      
	psrlq $32, %mm0                       
	paddb %mm1, %mm0                     
	movq 1760(%rsp), %mm7                         	# c.pQPb
	paddusb %mm7, %mm7                   
	psubusb %mm7, %mm4                   
	packssdw %mm4, %mm4                  
	movd %mm0, %edx                         	# numEq
	movd %mm4, %ecx                         	# dcOk
	
# 0 "" 2
#NO_APP
	negl	%edx	# D.15893
	movzbl	%dl, %edx	# D.15893, numEq
	cmpl	2896(%rsp), %edx	# c.ppMode.flatnessThreshold, numEq
	jle	.L966	#,
	testl	%ecx, %ecx	# dcOk
	jne	.L963	#,
	leaq	1648(%rsp), %rdx	#, tmp1644
	movl	4(%rsp), %esi	# %sfp,
	movq	%rbx, %rdi	# dstBlock,
	call	doVertLowPass_3DNow	#
	jmp	.L963	#
.L966:
#APP
# 552 "postprocess_template.c" 1
	lea (%rsi, %rdi), %rax                	# D.15894, D.15906
	pcmpeqb %mm6, %mm6                   
	movq (%rax, %rdi, 2), %mm1         	# D.15906
	movq (%rsi, %rdi, 4), %mm0                	# D.15894, D.15906
	pxor %mm6, %mm1                      
	pavgusb %mm1, %mm0 
	movq (%rax, %rdi, 4), %mm2         	# D.15906
	movq (%rax, %rdi), %mm3            	# D.15906
	pxor %mm6, %mm2                      
	movq %mm2, %mm5                      
	movq b80, %mm4              
	lea (%rax, %rdi, 4), %rcx      	# D.15906
	pavgusb %mm3, %mm2 
	pavgusb %mm0, %mm4 
	pavgusb %mm2, %mm4 
	pavgusb %mm0, %mm4 
	movq (%rax), %mm2                
	pxor %mm6, %mm2                      
	pavgusb %mm3, %mm2 
	pavgusb (%rsi), %mm1 	# D.15894
	movq b80, %mm3              
	pavgusb %mm2, %mm3 
	pavgusb %mm1, %mm3 
	pavgusb %mm2, %mm3 
	pavgusb (%rcx, %rdi), %mm5 	# D.15906
	movq (%rcx, %rdi, 2), %mm1         	# D.15906
	pxor %mm6, %mm1                      
	pavgusb (%rsi, %rdi, 4), %mm1 	# D.15894, D.15906
	movq b80, %mm2              
	pavgusb %mm5, %mm2 
	pavgusb %mm1, %mm2 
	pavgusb %mm5, %mm2 
	movq b00, %mm1              
	movq b00, %mm5              
	psubb %mm2, %mm1                     
	psubb %mm3, %mm5                     
	psubusb %mm1, %mm2 
	paddb %mm1, %mm2 
	psubusb %mm5, %mm3 
	paddb %mm5, %mm3 
	movq %mm3, %mm1 
	psubusb %mm2, %mm1 
	psubb %mm1, %mm3 
	movq b00, %mm7              
	movq 1760(%rsp), %mm2                         	# c.pQPb
	pavgusb %mm6, %mm2 
	psubb %mm6, %mm2                     
	movq %mm4, %mm1                      
	pcmpgtb %mm7, %mm1                   
	pxor %mm1, %mm4                      
	psubb %mm1, %mm4                     
	pcmpgtb %mm4, %mm2                   
	psubusb %mm3, %mm4                   
	movq %mm4, %mm3                      
	psubusb b01, %mm4           
	pavgusb %mm7, %mm4 
	pavgusb %mm7, %mm4 
	paddb %mm3, %mm4                     
	pand %mm2, %mm4                      
	movq b80, %mm5              
	psubb %mm0, %mm5                     
	paddsb %mm6, %mm5                    
	pcmpgtb %mm5, %mm7                   
	pxor %mm7, %mm5                      
	movq %mm4, %mm3 
	psubusb %mm5, %mm3 
	psubb %mm3, %mm4 
	pxor %mm1, %mm7                      
	pand %mm7, %mm4                      
	movq (%rax, %rdi, 2), %mm0         	# D.15906
	movq (%rsi, %rdi, 4), %mm2                	# D.15894, D.15906
	pxor %mm1, %mm0                      
	pxor %mm1, %mm2                      
	paddb %mm4, %mm0                     
	psubb %mm4, %mm2                     
	pxor %mm1, %mm0                      
	pxor %mm1, %mm2                      
	movq %mm0, (%rax, %rdi, 2)         	# D.15906
	movq %mm2, (%rsi, %rdi, 4)                	# D.15894, D.15906
	
# 0 "" 2
#NO_APP
	jmp	.L963	#
.L965:
	testl	$1024, %r15d	#, D.15893
	je	.L963	#,
	movq	80(%rsp), %rax	# %sfp, D.15897
	leaq	(%rbx,%rax), %rdx	#, D.15895
	movslq	2828(%rsp), %rax	# c.nonBQP, D.15893
#APP
# 2553 "postprocess_template.c" 1
	movq 1776(%rsp,%rax,8), %mm7                         	# c.mmxDcOffset
	movq 2288(%rsp,%rax,8), %mm6                         	# c.mmxDcThreshold
	
# 0 "" 2
#NO_APP
	movslq	4(%rsp), %rcx	# %sfp, D.15906
#APP
# 2559 "postprocess_template.c" 1
	lea (%rdx, %rcx), %rax                	# D.15895, D.15906
	movq (%rdx), %mm0                       	# D.15895
	movq (%rax), %mm1                
	movq %mm1, %mm3                      
	movq %mm1, %mm4                      
	psubb %mm1, %mm0                     
	paddb %mm7, %mm0                     
	pcmpgtb %mm6, %mm0                   
	movq (%rax,%rcx), %mm2             	# D.15906
	psubusb %mm2, %mm4 
	paddb %mm2, %mm4 
	movq %mm3, %mm5 
	psubusb %mm2, %mm5 
	psubb %mm5, %mm3 
	psubb %mm2, %mm1                     
	paddb %mm7, %mm1                     
	pcmpgtb %mm6, %mm1                   
	paddb %mm1, %mm0                     
	movq (%rax, %rcx, 2), %mm1         	# D.15906
	psubusb %mm1, %mm4 
	paddb %mm1, %mm4 
	movq %mm3, %mm5 
	psubusb %mm1, %mm5 
	psubb %mm5, %mm3 
	psubb %mm1, %mm2                     
	paddb %mm7, %mm2                     
	pcmpgtb %mm6, %mm2                   
	paddb %mm2, %mm0                     
	lea (%rax, %rcx, 4), %rax      	# D.15906
	movq (%rdx, %rcx, 4), %mm2                	# D.15895, D.15906
	psubusb %mm2, %mm4 
	paddb %mm2, %mm4 
	movq %mm3, %mm5 
	psubusb %mm2, %mm5 
	psubb %mm5, %mm3 
	psubb %mm2, %mm1                     
	paddb %mm7, %mm1                     
	pcmpgtb %mm6, %mm1                   
	paddb %mm1, %mm0                     
	movq (%rax), %mm1                
	psubusb %mm1, %mm4 
	paddb %mm1, %mm4 
	movq %mm3, %mm5 
	psubusb %mm1, %mm5 
	psubb %mm5, %mm3 
	psubb %mm1, %mm2                     
	paddb %mm7, %mm2                     
	pcmpgtb %mm6, %mm2                   
	paddb %mm2, %mm0                     
	movq (%rax, %rcx), %mm2            	# D.15906
	psubusb %mm2, %mm4 
	paddb %mm2, %mm4 
	movq %mm3, %mm5 
	psubusb %mm2, %mm5 
	psubb %mm5, %mm3 
	psubb %mm2, %mm1                     
	paddb %mm7, %mm1                     
	pcmpgtb %mm6, %mm1                   
	paddb %mm1, %mm0                     
	movq (%rax, %rcx, 2), %mm1         	# D.15906
	psubusb %mm1, %mm4 
	paddb %mm1, %mm4 
	movq %mm3, %mm5 
	psubusb %mm1, %mm5 
	psubb %mm5, %mm3 
	psubb %mm1, %mm2                     
	paddb %mm7, %mm2                     
	pcmpgtb %mm6, %mm2                   
	paddb %mm2, %mm0                     
	movq (%rdx, %rcx, 8), %mm2                	# D.15895, D.15906
	psubusb %mm2, %mm4 
	paddb %mm2, %mm4 
	movq %mm3, %mm5 
	psubusb %mm2, %mm5 
	psubb %mm5, %mm3 
	psubb %mm2, %mm1                     
	paddb %mm7, %mm1                     
	pcmpgtb %mm6, %mm1                   
	paddb %mm1, %mm0                     
	movq (%rax, %rcx, 4), %mm1         	# D.15906
	psubb %mm1, %mm2                     
	paddb %mm7, %mm2                     
	pcmpgtb %mm6, %mm2                   
	paddb %mm2, %mm0                     
	psubusb %mm3, %mm4                   
	pxor %mm6, %mm6                      
	movq 1760(%rsp), %mm7                         	# c.pQPb
	paddusb %mm7, %mm7                   
	psubusb %mm4, %mm7                   
	pcmpeqb %mm6, %mm7                   
	pcmpeqb %mm6, %mm7                   
	movq %mm7, 304(%rsp)                         	# dc_mask
	movq 2896(%rsp), %mm7                         	# c.ppMode.flatnessThreshold
	punpcklbw %mm7, %mm7                 
	punpcklbw %mm7, %mm7                 
	punpcklbw %mm7, %mm7                 
	psubb %mm0, %mm6                     
	pcmpgtb %mm7, %mm6                   
	movq %mm6, 312(%rsp)                         	# eq_mask
	
# 0 "" 2
#NO_APP
	movq	312(%rsp), %rsi	# eq_mask, D.15906
	movq	%rsi, %rax	# D.15906, D.15906
	andq	304(%rsp), %rax	# dc_mask, D.15906
	movq	%rax, 320(%rsp)	# D.15906, both_masks
	testq	%rax, %rax	# D.15906
	je	.L967	#,
	leaq	368(%rsp), %rax	#, tmp1224
	movq	%rdx, %rdi	# src, src
#APP
# 2664 "postprocess_template.c" 1
	movq 1760(%rsp), %mm0                         	# c.pQPb
	pxor %mm4, %mm4                      
	movq (%rdx), %mm6                       	# src
	movq (%rdx, %rcx), %mm5                   	# src, D.15906
	movq %mm5, %mm1                      
	movq %mm6, %mm2                      
	psubusb %mm6, %mm5                   
	psubusb %mm1, %mm2                   
	por %mm5, %mm2                       
	psubusb %mm2, %mm0                   
	pcmpeqb %mm4, %mm0                   
	pxor %mm6, %mm1                      
	pand %mm0, %mm1                      
	pxor %mm1, %mm6                      
	movq (%rdx, %rcx, 8), %mm5                	# src, D.15906
	add %rcx, %rdx                             	# D.15906, src
	movq (%rdx, %rcx, 8), %mm7                	# src, D.15906
	movq %mm5, %mm1                      
	movq %mm7, %mm2                      
	psubusb %mm7, %mm5                   
	psubusb %mm1, %mm2                   
	por %mm5, %mm2                       
	movq 1760(%rsp), %mm0                         	# c.pQPb
	psubusb %mm2, %mm0                   
	pcmpeqb %mm4, %mm0                   
	pxor %mm7, %mm1                      
	pand %mm0, %mm1                      
	pxor %mm1, %mm7                      
	movq %mm6, %mm5                      
	punpckhbw %mm4, %mm6                 
	punpcklbw %mm4, %mm5                 
	movq %mm5, %mm0                      
	movq %mm6, %mm1                      
	psllw $2, %mm0                        
	psllw $2, %mm1                        
	paddw w04, %mm0             
	paddw w04, %mm1             
	movq (%rdx), %mm2                       	# src
	movq (%rdx), %mm3                       	# src
	add %rcx, %rdx                             	# D.15906, src
	punpcklbw %mm4, %mm2                 
	punpckhbw %mm4, %mm3                 
	paddw %mm2, %mm0                     
	paddw %mm3, %mm1                     
	movq (%rdx), %mm2                       	# src
	movq (%rdx), %mm3                       	# src
	add %rcx, %rdx                             	# D.15906, src
	punpcklbw %mm4, %mm2                 
	punpckhbw %mm4, %mm3                 
	paddw %mm2, %mm0                     
	paddw %mm3, %mm1                     
	movq (%rdx), %mm2                       	# src
	movq (%rdx), %mm3                       	# src
	add %rcx, %rdx                             	# D.15906, src
	punpcklbw %mm4, %mm2                 
	punpckhbw %mm4, %mm3                 
	paddw %mm2, %mm0                     
	paddw %mm3, %mm1                     
	movq %mm0, (%rax)                       	# tmp1224
	movq %mm1, 8(%rax)                      	# tmp1224
	movq (%rdx), %mm2                       	# src
	movq (%rdx), %mm3                       	# src
	add %rcx, %rdx                             	# D.15906, src
	punpcklbw %mm4, %mm2                 
	punpckhbw %mm4, %mm3                 
	paddw %mm2, %mm0                     
	paddw %mm3, %mm1                     
	psubw %mm5, %mm0                     
	psubw %mm6, %mm1                     
	movq %mm0, 16(%rax)                     	# tmp1224
	movq %mm1, 24(%rax)                     	# tmp1224
	movq (%rdx), %mm2                       	# src
	movq (%rdx), %mm3                       	# src
	add %rcx, %rdx                             	# D.15906, src
	punpcklbw %mm4, %mm2                 
	punpckhbw %mm4, %mm3                 
	paddw %mm2, %mm0                     
	paddw %mm3, %mm1                     
	psubw %mm5, %mm0                     
	psubw %mm6, %mm1                     
	movq %mm0, 32(%rax)                     	# tmp1224
	movq %mm1, 40(%rax)                     	# tmp1224
	movq (%rdx), %mm2                       	# src
	movq (%rdx), %mm3                       	# src
	add %rcx, %rdx                             	# D.15906, src
	punpcklbw %mm4, %mm2                 
	punpckhbw %mm4, %mm3                 
	paddw %mm2, %mm0                     
	paddw %mm3, %mm1                     
	psubw %mm5, %mm0                     
	psubw %mm6, %mm1                     
	movq %mm0, 48(%rax)                     	# tmp1224
	movq %mm1, 56(%rax)                     	# tmp1224
	movq (%rdx), %mm2                       	# src
	movq (%rdx), %mm3                       	# src
	add %rcx, %rdx                             	# D.15906, src
	punpcklbw %mm4, %mm2                 
	punpckhbw %mm4, %mm3                 
	paddw %mm2, %mm0                     
	paddw %mm3, %mm1                     
	psubw %mm5, %mm0                     
	psubw %mm6, %mm1                     
	movq %mm0, 64(%rax)                     	# tmp1224
	movq %mm1, 72(%rax)                     	# tmp1224
	movq %mm7, %mm6                      
	punpckhbw %mm4, %mm7                 
	punpcklbw %mm4, %mm6                 
	movq (%rdx), %mm2                       	# src
	movq (%rdx), %mm3                       	# src
	add %rcx, %rdx                             	# D.15906, src
	punpcklbw %mm4, %mm2                 
	punpckhbw %mm4, %mm3                 
	paddw %mm2, %mm0                     
	paddw %mm3, %mm1                     
	mov %rdi, %rdx                             	# src, src
	add %rcx, %rdx                             	# D.15906, src
	movq (%rdx), %mm2                       	# src
	movq (%rdx), %mm3                       	# src
	add %rcx, %rdx                             	# D.15906, src
	punpcklbw %mm4, %mm2                 
	punpckhbw %mm4, %mm3                 
	psubw %mm2, %mm0                     
	psubw %mm3, %mm1                     
	movq %mm0, 80(%rax)                     	# tmp1224
	movq %mm1, 88(%rax)                     	# tmp1224
	movq (%rdx), %mm2                       	# src
	movq (%rdx), %mm3                       	# src
	add %rcx, %rdx                             	# D.15906, src
	punpcklbw %mm4, %mm2                 
	punpckhbw %mm4, %mm3                 
	psubw %mm2, %mm0                     
	psubw %mm3, %mm1                     
	paddw %mm6, %mm0                     
	paddw %mm7, %mm1                     
	movq %mm0, 96(%rax)                     	# tmp1224
	movq %mm1, 104(%rax)                    	# tmp1224
	movq (%rdx), %mm2                       	# src
	movq (%rdx), %mm3                       	# src
	add %rcx, %rdx                             	# D.15906, src
	punpcklbw %mm4, %mm2                 
	punpckhbw %mm4, %mm3                 
	psubw %mm2, %mm0                     
	psubw %mm3, %mm1                     
	paddw %mm6, %mm0                     
	paddw %mm7, %mm1                     
	movq %mm0, 112(%rax)                    	# tmp1224
	movq %mm1, 120(%rax)                    	# tmp1224
	movq (%rdx), %mm2                       	# src
	movq (%rdx), %mm3                       	# src
	add %rcx, %rdx                             	# D.15906, src
	punpcklbw %mm4, %mm2                 
	punpckhbw %mm4, %mm3                 
	psubw %mm2, %mm0                     
	psubw %mm3, %mm1                     
	paddw %mm6, %mm0                     
	paddw %mm7, %mm1                     
	movq %mm0, 128(%rax)                    	# tmp1224
	movq %mm1, 136(%rax)                    	# tmp1224
	movq (%rdx), %mm2                       	# src
	movq (%rdx), %mm3                       	# src
	add %rcx, %rdx                             	# D.15906, src
	punpcklbw %mm4, %mm2                 
	punpckhbw %mm4, %mm3                 
	psubw %mm2, %mm0                     
	psubw %mm3, %mm1                     
	paddw %mm6, %mm0                     
	paddw %mm7, %mm1                     
	movq %mm0, 144(%rax)                    	# tmp1224
	movq %mm1, 152(%rax)                    	# tmp1224
	mov %rdi, %rdx                             	# src, src
	
# 0 "" 2
#NO_APP
	addq	72(%rsp), %rdx	# %sfp, src
	movq	176(%rsp), %rdi	# %sfp, D.15897
	leaq	(%rdx,%rdi), %r8	#, D.15895
	movq	168(%rsp), %rdi	# %sfp, offset
#APP
# 2804 "postprocess_template.c" 1
	movq 320(%rsp), %mm6                         	# both_masks
	pcmpeqb %mm5, %mm5                   
	pxor %mm6, %mm5                      
	pxor %mm7, %mm7                      
	1:                                     
	movq (%rax), %mm0                       	# temp_sums
	movq 8(%rax), %mm1                      	# temp_sums
	paddw 32(%rax), %mm0                    	# temp_sums
	paddw 40(%rax), %mm1                    	# temp_sums
	movq (%rdi, %r8), %mm2                   	# offset, D.15895
	movq %mm2, %mm3                      
	movq %mm2, %mm4                      
	punpcklbw %mm7, %mm2                 
	punpckhbw %mm7, %mm3                 
	paddw %mm2, %mm0                     
	paddw %mm3, %mm1                     
	paddw %mm2, %mm0                     
	paddw %mm3, %mm1                     
	psrlw $4, %mm0                        
	psrlw $4, %mm1                        
	packuswb %mm1, %mm0                  
	pand %mm6, %mm0                      
	pand %mm5, %mm4                      
	por %mm4, %mm0                       
	movq %mm0, (%rdi, %r8)                   	# offset, D.15895
	add $16, %rax                            	# temp_sums
	add %rcx, %rdi                             	# D.15906, offset
	 js 1b                                 
	
# 0 "" 2
#NO_APP
	jmp	.L968	#
.L967:
	movq	72(%rsp), %rdx	# %sfp, D.15913
	addq	80(%rsp), %rdx	# %sfp, D.15913
	addq	%rbx, %rdx	# dstBlock, src
.L968:
	cmpq	$-1, %rsi	#, D.15906
	je	.L963	#,
	leaq	336(%rsp), %rsi	#, tmp1231
#APP
# 2844 "postprocess_template.c" 1
	pxor %mm7, %mm7                      
	movq (%rdx), %mm0                       	# temp_src
	movq %mm0, %mm1                      
	punpcklbw %mm7, %mm0                 
	punpckhbw %mm7, %mm1                 
	movq (%rdx, %rcx), %mm2                   	# temp_src, D.15906
	lea (%rdx, %rcx, 2), %rax             	# temp_src, D.15906
	movq %mm2, %mm3                      
	punpcklbw %mm7, %mm2                 
	punpckhbw %mm7, %mm3                 
	movq (%rax), %mm4                
	movq %mm4, %mm5                      
	punpcklbw %mm7, %mm4                 
	punpckhbw %mm7, %mm5                 
	paddw %mm0, %mm0                     
	paddw %mm1, %mm1                     
	psubw %mm4, %mm2                     
	psubw %mm5, %mm3                     
	psubw %mm2, %mm0                     
	psubw %mm3, %mm1                     
	psllw $2, %mm2                        
	psllw $2, %mm3                        
	psubw %mm2, %mm0                     
	psubw %mm3, %mm1                     
	movq (%rax, %rcx), %mm2            	# D.15906
	movq %mm2, %mm3                      
	punpcklbw %mm7, %mm2                 
	punpckhbw %mm7, %mm3                 
	psubw %mm2, %mm0                     
	psubw %mm3, %mm1                     
	psubw %mm2, %mm0                     
	psubw %mm3, %mm1                     
	movq %mm0, (%rsi)                       	# tmp1231
	movq %mm1, 8(%rsi)                      	# tmp1231
	movq (%rax, %rcx, 2), %mm0         	# D.15906
	movq %mm0, %mm1                      
	punpcklbw %mm7, %mm0                 
	punpckhbw %mm7, %mm1                 
	psubw %mm0, %mm2                     
	psubw %mm1, %mm3                     
	movq %mm2, 16(%rsi)                     	# tmp1231
	movq %mm3, 24(%rsi)                     	# tmp1231
	paddw %mm4, %mm4                     
	paddw %mm5, %mm5                     
	psubw %mm2, %mm4                     
	psubw %mm3, %mm5                     
	lea (%rax, %rcx), %rdx                	# D.15906, temp_src
	psllw $2, %mm2                        
	psllw $2, %mm3                        
	psubw %mm2, %mm4                     
	psubw %mm3, %mm5                     
	movq (%rdx, %rcx, 2), %mm2                	# temp_src, D.15906
	movq %mm2, %mm3                      
	punpcklbw %mm7, %mm2                 
	punpckhbw %mm7, %mm3                 
	psubw %mm2, %mm4                     
	psubw %mm3, %mm5                     
	psubw %mm2, %mm4                     
	psubw %mm3, %mm5                     
	movq (%rax, %rcx, 4), %mm6         	# D.15906
	punpcklbw %mm7, %mm6                 
	psubw %mm6, %mm2                     
	movq (%rax, %rcx, 4), %mm6         	# D.15906
	punpckhbw %mm7, %mm6                 
	psubw %mm6, %mm3                     
	paddw %mm0, %mm0                     
	paddw %mm1, %mm1                     
	psubw %mm2, %mm0                     
	psubw %mm3, %mm1                     
	psllw $2, %mm2                        
	psllw $2, %mm3                        
	psubw %mm2, %mm0                     
	psubw %mm3, %mm1                     
	movq (%rdx, %rcx, 4), %mm2                	# temp_src, D.15906
	movq %mm2, %mm3                      
	punpcklbw %mm7, %mm2                 
	punpckhbw %mm7, %mm3                 
	paddw %mm2, %mm2                     
	paddw %mm3, %mm3                     
	psubw %mm2, %mm0                     
	psubw %mm3, %mm1                     
	movq (%rsi), %mm2                       	# tmp1231
	movq 8(%rsi), %mm3                      	# tmp1231
	movq %mm7, %mm6                      
	pcmpgtw %mm0, %mm6                   
	pxor %mm6, %mm0                      
	psubw %mm6, %mm0                     
	movq %mm7, %mm6                      
	pcmpgtw %mm1, %mm6                   
	pxor %mm6, %mm1                      
	psubw %mm6, %mm1                     
	movq %mm7, %mm6                      
	pcmpgtw %mm2, %mm6                   
	pxor %mm6, %mm2                      
	psubw %mm6, %mm2                     
	movq %mm7, %mm6                      
	pcmpgtw %mm3, %mm6                   
	pxor %mm6, %mm3                      
	psubw %mm6, %mm3                     
	movq %mm0, %mm6                      
	psubusw %mm2, %mm6                   
	psubw %mm6, %mm0                     
	movq %mm1, %mm6                      
	psubusw %mm3, %mm6                   
	psubw %mm6, %mm1                     
	movd 1760(%rsp), %mm2                         	# c.pQPb
	punpcklbw %mm7, %mm2                 
	movq %mm7, %mm6                      
	pcmpgtw %mm4, %mm6                   
	pxor %mm6, %mm4                      
	psubw %mm6, %mm4                     
	pcmpgtw %mm5, %mm7                   
	pxor %mm7, %mm5                      
	psubw %mm7, %mm5                     
	psllw $3, %mm2                        
	movq %mm2, %mm3                      
	pcmpgtw %mm4, %mm2                   
	pcmpgtw %mm5, %mm3                   
	pand %mm2, %mm4                      
	pand %mm3, %mm5                      
	psubusw %mm0, %mm4                   
	psubusw %mm1, %mm5                   
	movq w05, %mm2              
	pmullw %mm2, %mm4                    
	pmullw %mm2, %mm5                    
	movq w20, %mm2              
	paddw %mm2, %mm4                     
	paddw %mm2, %mm5                     
	psrlw $6, %mm4                        
	psrlw $6, %mm5                        
	movq 16(%rsi), %mm0                     	# tmp1231
	movq 24(%rsi), %mm1                     	# tmp1231
	pxor %mm2, %mm2                      
	pxor %mm3, %mm3                      
	pcmpgtw %mm0, %mm2                   
	pcmpgtw %mm1, %mm3                   
	pxor %mm2, %mm0                      
	pxor %mm3, %mm1                      
	psubw %mm2, %mm0                     
	psubw %mm3, %mm1                     
	psrlw $1, %mm0                        
	psrlw $1, %mm1                        
	pxor %mm6, %mm2                      
	pxor %mm7, %mm3                      
	pand %mm2, %mm4                      
	pand %mm3, %mm5                      
	movq %mm4, %mm2                      
	psubusw %mm0, %mm2                   
	psubw %mm2, %mm4                     
	movq %mm5, %mm2                      
	psubusw %mm1, %mm2                   
	psubw %mm2, %mm5                     
	pxor %mm6, %mm4                      
	pxor %mm7, %mm5                      
	psubw %mm6, %mm4                     
	psubw %mm7, %mm5                     
	packsswb %mm5, %mm4                  
	movq 312(%rsp), %mm1                         	# eq_mask
	pandn %mm4, %mm1                     
	movq (%rdx), %mm0                       	# temp_src
	paddb   %mm1, %mm0                   
	movq %mm0, (%rdx)                       	# temp_src
	movq (%rdx, %rcx), %mm0                   	# temp_src, D.15906
	psubb %mm1, %mm0                     
	movq %mm0, (%rdx, %rcx)                   	# temp_src, D.15906
	
# 0 "" 2
#NO_APP
.L963:
	movslq	4(%rsp), %r14	# %sfp, D.15906
	movq	8(%rsp), %rdi	# %sfp, tempBlock2
#APP
# 1995 "postprocess_template.c" 1
	lea (%rbx, %r14), %rax                	# dstBlock, D.15906
	movq (%rbx), %mm0                       	# dstBlock
	movq (%rax), %mm1                
	movq %mm0, %mm2                      
	punpcklbw %mm1, %mm0                 
	punpckhbw %mm1, %mm2                 
	movq (%rax, %r14), %mm1            	# D.15906
	movq (%rax, %r14, 2), %mm3         	# D.15906
	movq %mm1, %mm4                      
	punpcklbw %mm3, %mm1                 
	punpckhbw %mm3, %mm4                 
	movq %mm0, %mm3                      
	punpcklwd %mm1, %mm0                 
	punpckhwd %mm1, %mm3                 
	movq %mm2, %mm1                      
	punpcklwd %mm4, %mm2                 
	punpckhwd %mm4, %mm1                 
	movd %mm0, 128(%r12)                    	# tempBlock2
	psrlq $32, %mm0                       
	movd %mm0, 144(%r12)                    	# tempBlock2
	movd %mm3, 160(%r12)                    	# tempBlock2
	psrlq $32, %mm3                       
	movd %mm3, 176(%r12)                    	# tempBlock2
	movd %mm3, 48(%rdi)                     	# tempBlock2
	movd %mm2, 192(%r12)                    	# tempBlock2
	movd %mm2, 64(%rdi)                     	# tempBlock2
	psrlq $32, %mm2                       
	movd %mm2, 80(%rdi)                     	# tempBlock2
	movd %mm1, 96(%rdi)                     	# tempBlock2
	psrlq $32, %mm1                       
	movd %mm1, 112(%rdi)                    	# tempBlock2
	lea (%rax, %r14, 4), %rax      	# D.15906
	movq (%rbx, %r14, 4), %mm0                	# dstBlock, D.15906
	movq (%rax), %mm1                
	movq %mm0, %mm2                      
	punpcklbw %mm1, %mm0                 
	punpckhbw %mm1, %mm2                 
	movq (%rax, %r14), %mm1            	# D.15906
	movq (%rax, %r14, 2), %mm3         	# D.15906
	movq %mm1, %mm4                      
	punpcklbw %mm3, %mm1                 
	punpckhbw %mm3, %mm4                 
	movq %mm0, %mm3                      
	punpcklwd %mm1, %mm0                 
	punpckhwd %mm1, %mm3                 
	movq %mm2, %mm1                      
	punpcklwd %mm4, %mm2                 
	punpckhwd %mm4, %mm1                 
	movd %mm0, 132(%r12)                    	# tempBlock2
	psrlq $32, %mm0                       
	movd %mm0, 148(%r12)                    	# tempBlock2
	movd %mm3, 164(%r12)                    	# tempBlock2
	psrlq $32, %mm3                       
	movd %mm3, 180(%r12)                    	# tempBlock2
	movd %mm3, 52(%rdi)                     	# tempBlock2
	movd %mm2, 196(%r12)                    	# tempBlock2
	movd %mm2, 68(%rdi)                     	# tempBlock2
	psrlq $32, %mm2                       
	movd %mm2, 84(%rdi)                     	# tempBlock2
	movd %mm1, 100(%rdi)                    	# tempBlock2
	psrlq $32, %mm1                       
	movd %mm1, 116(%rdi)                    	# tempBlock2
	
# 0 "" 2
#NO_APP
	movl	%ebp, %eax	# x, tmp1432
	subl	$8, %eax	#, tmp1432
	js	.L970	#,
	testl	$8192, %r15d	#, D.15893
	je	.L971	#,
	leaq	48(%r12), %rdx	#, src
	movl	$16, %esi	#, tmp1235
#APP
# 412 "postprocess_template.c" 1
	pxor %mm7, %mm7                      
	lea (%rdx, %rsi), %rax                	# src, tmp1235
	lea (%rax, %rsi, 4), %rcx      	# tmp1235
	movq (%rax, %rsi, 2), %mm0         	# tmp1235
	movq (%rdx, %rsi, 4), %mm1                	# src, tmp1235
	movq %mm1, %mm2                      
	psubusb %mm0, %mm1                   
	psubusb %mm2, %mm0                   
	por %mm1, %mm0                       
	movq (%rcx), %mm3                
	movq (%rcx, %rsi), %mm4            	# tmp1235
	movq %mm3, %mm5                      
	psubusb %mm4, %mm3                   
	psubusb %mm5, %mm4                   
	por %mm4, %mm3                       
	pavgusb %mm3, %mm0 
	movq %mm2, %mm1                      
	psubusb %mm5, %mm2                   
	movq %mm2, %mm4                      
	pcmpeqb %mm7, %mm2                   
	psubusb %mm1, %mm5                   
	por %mm5, %mm4                       
	psubusb %mm0, %mm4                   
	movq %mm4, %mm3                      
	movq 1760(%rsp), %mm0                         	# c.pQPb
	paddusb %mm0, %mm0                   
	psubusb %mm0, %mm4                   
	pcmpeqb %mm7, %mm4                   
	psubusb b01, %mm3           
	pand %mm4, %mm3                      
	pavgusb %mm7, %mm3 
	movq %mm3, %mm1                      
	pavgusb %mm7, %mm3 
	pavgusb %mm1, %mm3 
	movq (%rdx, %rsi, 4), %mm0                	# src, tmp1235
	pxor %mm2, %mm0                      
	psubusb %mm3, %mm0                   
	pxor %mm2, %mm0                      
	movq %mm0, (%rdx, %rsi, 4)                	# src, tmp1235
	movq (%rcx), %mm0                
	pxor %mm2, %mm0                      
	paddusb %mm3, %mm0                   
	pxor %mm2, %mm0                      
	movq %mm0, (%rcx)                
	pavgusb %mm7, %mm1 
	movq (%rax, %rsi, 2), %mm0         	# tmp1235
	pxor %mm2, %mm0                      
	psubusb %mm1, %mm0                   
	pxor %mm2, %mm0                      
	movq %mm0, (%rax, %rsi, 2)         	# tmp1235
	movq (%rcx, %rsi), %mm0            	# tmp1235
	pxor %mm2, %mm0                      
	paddusb %mm1, %mm0                   
	pxor %mm2, %mm0                      
	movq %mm0, (%rcx, %rsi)            	# tmp1235
	pavgusb %mm7, %mm1 
	movq (%rax, %rsi), %mm0            	# tmp1235
	pxor %mm2, %mm0                      
	psubusb %mm1, %mm0                   
	pxor %mm2, %mm0                      
	movq %mm0, (%rax, %rsi)            	# tmp1235
	movq (%rcx, %rsi, 2), %mm0         	# tmp1235
	pxor %mm2, %mm0                      
	paddusb %mm1, %mm0                   
	pxor %mm2, %mm0                      
	movq %mm0, (%rcx, %rsi, 2)         	# tmp1235
	
# 0 "" 2
#NO_APP
	jmp	.L972	#
.L971:
	testb	$2, %r15b	#, D.15893
	je	.L973	#,
	leaq	64(%r12), %rsi	#, src
	movslq	2828(%rsp), %rax	# c.nonBQP, D.15893
#APP
# 114 "postprocess_template.c" 1
	movq 1776(%rsp,%rax,8), %mm7                         	# c.mmxDcOffset
	movq 2288(%rsp,%rax,8), %mm6                         	# c.mmxDcThreshold
	
# 0 "" 2
#NO_APP
	movl	$16, %edx	#, tmp1243
#APP
# 120 "postprocess_template.c" 1
	lea (%rsi, %rdx), %rax                	# src, tmp1243
	movq (%rsi), %mm0                       	# src
	movq (%rax), %mm1                
	movq %mm0, %mm3                      
	movq %mm0, %mm4                      
	psubusb %mm1, %mm4 
	paddb %mm1, %mm4 
	movq %mm3, %mm5 
	psubusb %mm1, %mm5 
	psubb %mm5, %mm3 
	psubb %mm1, %mm0                     
	paddb %mm7, %mm0                     
	pcmpgtb %mm6, %mm0                   
	movq (%rax,%rdx), %mm2             	# tmp1243
	psubusb %mm2, %mm4 
	paddb %mm2, %mm4 
	movq %mm3, %mm5 
	psubusb %mm2, %mm5 
	psubb %mm5, %mm3 
	psubb %mm2, %mm1                     
	paddb %mm7, %mm1                     
	pcmpgtb %mm6, %mm1                   
	paddb %mm1, %mm0                     
	movq (%rax, %rdx, 2), %mm1         	# tmp1243
	psubusb %mm1, %mm4 
	paddb %mm1, %mm4 
	movq %mm3, %mm5 
	psubusb %mm1, %mm5 
	psubb %mm5, %mm3 
	psubb %mm1, %mm2                     
	paddb %mm7, %mm2                     
	pcmpgtb %mm6, %mm2                   
	paddb %mm2, %mm0                     
	lea (%rax, %rdx, 4), %rax      	# tmp1243
	movq (%rsi, %rdx, 4), %mm2                	# src, tmp1243
	psubusb %mm2, %mm4 
	paddb %mm2, %mm4 
	movq %mm3, %mm5 
	psubusb %mm2, %mm5 
	psubb %mm5, %mm3 
	psubb %mm2, %mm1                     
	paddb %mm7, %mm1                     
	pcmpgtb %mm6, %mm1                   
	paddb %mm1, %mm0                     
	movq (%rax), %mm1                
	psubusb %mm1, %mm4 
	paddb %mm1, %mm4 
	movq %mm3, %mm5 
	psubusb %mm1, %mm5 
	psubb %mm5, %mm3 
	psubb %mm1, %mm2                     
	paddb %mm7, %mm2                     
	pcmpgtb %mm6, %mm2                   
	paddb %mm2, %mm0                     
	movq (%rax, %rdx), %mm2            	# tmp1243
	psubusb %mm2, %mm4 
	paddb %mm2, %mm4 
	movq %mm3, %mm5 
	psubusb %mm2, %mm5 
	psubb %mm5, %mm3 
	psubb %mm2, %mm1                     
	paddb %mm7, %mm1                     
	pcmpgtb %mm6, %mm1                   
	paddb %mm1, %mm0                     
	movq (%rax, %rdx, 2), %mm1         	# tmp1243
	psubusb %mm1, %mm4 
	paddb %mm1, %mm4 
	movq %mm3, %mm5 
	psubusb %mm1, %mm5 
	psubb %mm5, %mm3 
	psubb %mm1, %mm2                     
	paddb %mm7, %mm2                     
	pcmpgtb %mm6, %mm2                   
	paddb %mm2, %mm0                     
	psubusb %mm3, %mm4                   
	                                       
	movq %mm0, %mm1                      
	psrlw $8, %mm0                        
	paddb %mm1, %mm0                     
	movq %mm0, %mm1                      
	psrlq $16, %mm0                       
	paddb %mm1, %mm0                     
	movq %mm0, %mm1                      
	psrlq $32, %mm0                       
	paddb %mm1, %mm0                     
	movq 1760(%rsp), %mm7                         	# c.pQPb
	paddusb %mm7, %mm7                   
	psubusb %mm7, %mm4                   
	packssdw %mm4, %mm4                  
	movd %mm0, %edx                         	# numEq
	movd %mm4, %ecx                         	# dcOk
	
# 0 "" 2
#NO_APP
	negl	%edx	# D.15893
	movzbl	%dl, %eax	# D.15893, numEq
	cmpl	2896(%rsp), %eax	# c.ppMode.flatnessThreshold, numEq
	jle	.L974	#,
	testl	%ecx, %ecx	# dcOk
	jne	.L972	#,
	leaq	1648(%rsp), %rdx	#, tmp1656
	movl	$16, %esi	#,
	movq	%r12, %rdi	# tempBlock2,
	call	doVertLowPass_3DNow	#
	jmp	.L972	#
.L974:
	movl	$16, %edx	#, tmp1246
#APP
# 552 "postprocess_template.c" 1
	lea (%rsi, %rdx), %rax                	# src, tmp1246
	pcmpeqb %mm6, %mm6                   
	movq (%rax, %rdx, 2), %mm1         	# tmp1246
	movq (%rsi, %rdx, 4), %mm0                	# src, tmp1246
	pxor %mm6, %mm1                      
	pavgusb %mm1, %mm0 
	movq (%rax, %rdx, 4), %mm2         	# tmp1246
	movq (%rax, %rdx), %mm3            	# tmp1246
	pxor %mm6, %mm2                      
	movq %mm2, %mm5                      
	movq b80, %mm4              
	lea (%rax, %rdx, 4), %rcx      	# tmp1246
	pavgusb %mm3, %mm2 
	pavgusb %mm0, %mm4 
	pavgusb %mm2, %mm4 
	pavgusb %mm0, %mm4 
	movq (%rax), %mm2                
	pxor %mm6, %mm2                      
	pavgusb %mm3, %mm2 
	pavgusb (%rsi), %mm1 	# src
	movq b80, %mm3              
	pavgusb %mm2, %mm3 
	pavgusb %mm1, %mm3 
	pavgusb %mm2, %mm3 
	pavgusb (%rcx, %rdx), %mm5 	# tmp1246
	movq (%rcx, %rdx, 2), %mm1         	# tmp1246
	pxor %mm6, %mm1                      
	pavgusb (%rsi, %rdx, 4), %mm1 	# src, tmp1246
	movq b80, %mm2              
	pavgusb %mm5, %mm2 
	pavgusb %mm1, %mm2 
	pavgusb %mm5, %mm2 
	movq b00, %mm1              
	movq b00, %mm5              
	psubb %mm2, %mm1                     
	psubb %mm3, %mm5                     
	psubusb %mm1, %mm2 
	paddb %mm1, %mm2 
	psubusb %mm5, %mm3 
	paddb %mm5, %mm3 
	movq %mm3, %mm1 
	psubusb %mm2, %mm1 
	psubb %mm1, %mm3 
	movq b00, %mm7              
	movq 1760(%rsp), %mm2                         	# c.pQPb
	pavgusb %mm6, %mm2 
	psubb %mm6, %mm2                     
	movq %mm4, %mm1                      
	pcmpgtb %mm7, %mm1                   
	pxor %mm1, %mm4                      
	psubb %mm1, %mm4                     
	pcmpgtb %mm4, %mm2                   
	psubusb %mm3, %mm4                   
	movq %mm4, %mm3                      
	psubusb b01, %mm4           
	pavgusb %mm7, %mm4 
	pavgusb %mm7, %mm4 
	paddb %mm3, %mm4                     
	pand %mm2, %mm4                      
	movq b80, %mm5              
	psubb %mm0, %mm5                     
	paddsb %mm6, %mm5                    
	pcmpgtb %mm5, %mm7                   
	pxor %mm7, %mm5                      
	movq %mm4, %mm3 
	psubusb %mm5, %mm3 
	psubb %mm3, %mm4 
	pxor %mm1, %mm7                      
	pand %mm7, %mm4                      
	movq (%rax, %rdx, 2), %mm0         	# tmp1246
	movq (%rsi, %rdx, 4), %mm2                	# src, tmp1246
	pxor %mm1, %mm0                      
	pxor %mm1, %mm2                      
	paddb %mm4, %mm0                     
	psubb %mm4, %mm2                     
	pxor %mm1, %mm0                      
	pxor %mm1, %mm2                      
	movq %mm0, (%rax, %rdx, 2)         	# tmp1246
	movq %mm2, (%rsi, %rdx, 4)                	# src, tmp1246
	
# 0 "" 2
#NO_APP
	jmp	.L972	#
.L973:
	testl	$16384, %r15d	#, D.15893
	je	.L972	#,
	leaq	48(%r12), %rdx	#, src
	movslq	2828(%rsp), %rax	# c.nonBQP, D.15893
#APP
# 2553 "postprocess_template.c" 1
	movq 1776(%rsp,%rax,8), %mm7                         	# c.mmxDcOffset
	movq 2288(%rsp,%rax,8), %mm6                         	# c.mmxDcThreshold
	
# 0 "" 2
#NO_APP
	movl	$16, %ecx	#, tmp1252
#APP
# 2559 "postprocess_template.c" 1
	lea (%rdx, %rcx), %rax                	# src, tmp1252
	movq (%rdx), %mm0                       	# src
	movq (%rax), %mm1                
	movq %mm1, %mm3                      
	movq %mm1, %mm4                      
	psubb %mm1, %mm0                     
	paddb %mm7, %mm0                     
	pcmpgtb %mm6, %mm0                   
	movq (%rax,%rcx), %mm2             	# tmp1252
	psubusb %mm2, %mm4 
	paddb %mm2, %mm4 
	movq %mm3, %mm5 
	psubusb %mm2, %mm5 
	psubb %mm5, %mm3 
	psubb %mm2, %mm1                     
	paddb %mm7, %mm1                     
	pcmpgtb %mm6, %mm1                   
	paddb %mm1, %mm0                     
	movq (%rax, %rcx, 2), %mm1         	# tmp1252
	psubusb %mm1, %mm4 
	paddb %mm1, %mm4 
	movq %mm3, %mm5 
	psubusb %mm1, %mm5 
	psubb %mm5, %mm3 
	psubb %mm1, %mm2                     
	paddb %mm7, %mm2                     
	pcmpgtb %mm6, %mm2                   
	paddb %mm2, %mm0                     
	lea (%rax, %rcx, 4), %rax      	# tmp1252
	movq (%rdx, %rcx, 4), %mm2                	# src, tmp1252
	psubusb %mm2, %mm4 
	paddb %mm2, %mm4 
	movq %mm3, %mm5 
	psubusb %mm2, %mm5 
	psubb %mm5, %mm3 
	psubb %mm2, %mm1                     
	paddb %mm7, %mm1                     
	pcmpgtb %mm6, %mm1                   
	paddb %mm1, %mm0                     
	movq (%rax), %mm1                
	psubusb %mm1, %mm4 
	paddb %mm1, %mm4 
	movq %mm3, %mm5 
	psubusb %mm1, %mm5 
	psubb %mm5, %mm3 
	psubb %mm1, %mm2                     
	paddb %mm7, %mm2                     
	pcmpgtb %mm6, %mm2                   
	paddb %mm2, %mm0                     
	movq (%rax, %rcx), %mm2            	# tmp1252
	psubusb %mm2, %mm4 
	paddb %mm2, %mm4 
	movq %mm3, %mm5 
	psubusb %mm2, %mm5 
	psubb %mm5, %mm3 
	psubb %mm2, %mm1                     
	paddb %mm7, %mm1                     
	pcmpgtb %mm6, %mm1                   
	paddb %mm1, %mm0                     
	movq (%rax, %rcx, 2), %mm1         	# tmp1252
	psubusb %mm1, %mm4 
	paddb %mm1, %mm4 
	movq %mm3, %mm5 
	psubusb %mm1, %mm5 
	psubb %mm5, %mm3 
	psubb %mm1, %mm2                     
	paddb %mm7, %mm2                     
	pcmpgtb %mm6, %mm2                   
	paddb %mm2, %mm0                     
	movq (%rdx, %rcx, 8), %mm2                	# src, tmp1252
	psubusb %mm2, %mm4 
	paddb %mm2, %mm4 
	movq %mm3, %mm5 
	psubusb %mm2, %mm5 
	psubb %mm5, %mm3 
	psubb %mm2, %mm1                     
	paddb %mm7, %mm1                     
	pcmpgtb %mm6, %mm1                   
	paddb %mm1, %mm0                     
	movq (%rax, %rcx, 4), %mm1         	# tmp1252
	psubb %mm1, %mm2                     
	paddb %mm7, %mm2                     
	pcmpgtb %mm6, %mm2                   
	paddb %mm2, %mm0                     
	psubusb %mm3, %mm4                   
	pxor %mm6, %mm6                      
	movq 1760(%rsp), %mm7                         	# c.pQPb
	paddusb %mm7, %mm7                   
	psubusb %mm4, %mm7                   
	pcmpeqb %mm6, %mm7                   
	pcmpeqb %mm6, %mm7                   
	movq %mm7, 304(%rsp)                         	# dc_mask
	movq 2896(%rsp), %mm7                         	# c.ppMode.flatnessThreshold
	punpcklbw %mm7, %mm7                 
	punpcklbw %mm7, %mm7                 
	punpcklbw %mm7, %mm7                 
	psubb %mm0, %mm6                     
	pcmpgtb %mm7, %mm6                   
	movq %mm6, 312(%rsp)                         	# eq_mask
	
# 0 "" 2
#NO_APP
	movq	312(%rsp), %rsi	# eq_mask, D.15906
	movq	%rsi, %rcx	# D.15906, D.15906
	andq	304(%rsp), %rcx	# dc_mask, D.15906
	movq	%rcx, 320(%rsp)	# D.15906, both_masks
	leaq	64(%r12), %rax	#, src
	testq	%rcx, %rcx	# D.15906
	je	.L976	#,
	movl	$16, %r8d	#, tmp1254
	leaq	368(%rsp), %rcx	#, tmp1255
	movq	%rdx, %rax	# src, src
#APP
# 2664 "postprocess_template.c" 1
	movq 1760(%rsp), %mm0                         	# c.pQPb
	pxor %mm4, %mm4                      
	movq (%rdx), %mm6                       	# src
	movq (%rdx, %r8), %mm5                   	# src, tmp1254
	movq %mm5, %mm1                      
	movq %mm6, %mm2                      
	psubusb %mm6, %mm5                   
	psubusb %mm1, %mm2                   
	por %mm5, %mm2                       
	psubusb %mm2, %mm0                   
	pcmpeqb %mm4, %mm0                   
	pxor %mm6, %mm1                      
	pand %mm0, %mm1                      
	pxor %mm1, %mm6                      
	movq (%rdx, %r8, 8), %mm5                	# src, tmp1254
	add %r8, %rdx                             	# tmp1254, src
	movq (%rdx, %r8, 8), %mm7                	# src, tmp1254
	movq %mm5, %mm1                      
	movq %mm7, %mm2                      
	psubusb %mm7, %mm5                   
	psubusb %mm1, %mm2                   
	por %mm5, %mm2                       
	movq 1760(%rsp), %mm0                         	# c.pQPb
	psubusb %mm2, %mm0                   
	pcmpeqb %mm4, %mm0                   
	pxor %mm7, %mm1                      
	pand %mm0, %mm1                      
	pxor %mm1, %mm7                      
	movq %mm6, %mm5                      
	punpckhbw %mm4, %mm6                 
	punpcklbw %mm4, %mm5                 
	movq %mm5, %mm0                      
	movq %mm6, %mm1                      
	psllw $2, %mm0                        
	psllw $2, %mm1                        
	paddw w04, %mm0             
	paddw w04, %mm1             
	movq (%rdx), %mm2                       	# src
	movq (%rdx), %mm3                       	# src
	add %r8, %rdx                             	# tmp1254, src
	punpcklbw %mm4, %mm2                 
	punpckhbw %mm4, %mm3                 
	paddw %mm2, %mm0                     
	paddw %mm3, %mm1                     
	movq (%rdx), %mm2                       	# src
	movq (%rdx), %mm3                       	# src
	add %r8, %rdx                             	# tmp1254, src
	punpcklbw %mm4, %mm2                 
	punpckhbw %mm4, %mm3                 
	paddw %mm2, %mm0                     
	paddw %mm3, %mm1                     
	movq (%rdx), %mm2                       	# src
	movq (%rdx), %mm3                       	# src
	add %r8, %rdx                             	# tmp1254, src
	punpcklbw %mm4, %mm2                 
	punpckhbw %mm4, %mm3                 
	paddw %mm2, %mm0                     
	paddw %mm3, %mm1                     
	movq %mm0, (%rcx)                       	# tmp1255
	movq %mm1, 8(%rcx)                      	# tmp1255
	movq (%rdx), %mm2                       	# src
	movq (%rdx), %mm3                       	# src
	add %r8, %rdx                             	# tmp1254, src
	punpcklbw %mm4, %mm2                 
	punpckhbw %mm4, %mm3                 
	paddw %mm2, %mm0                     
	paddw %mm3, %mm1                     
	psubw %mm5, %mm0                     
	psubw %mm6, %mm1                     
	movq %mm0, 16(%rcx)                     	# tmp1255
	movq %mm1, 24(%rcx)                     	# tmp1255
	movq (%rdx), %mm2                       	# src
	movq (%rdx), %mm3                       	# src
	add %r8, %rdx                             	# tmp1254, src
	punpcklbw %mm4, %mm2                 
	punpckhbw %mm4, %mm3                 
	paddw %mm2, %mm0                     
	paddw %mm3, %mm1                     
	psubw %mm5, %mm0                     
	psubw %mm6, %mm1                     
	movq %mm0, 32(%rcx)                     	# tmp1255
	movq %mm1, 40(%rcx)                     	# tmp1255
	movq (%rdx), %mm2                       	# src
	movq (%rdx), %mm3                       	# src
	add %r8, %rdx                             	# tmp1254, src
	punpcklbw %mm4, %mm2                 
	punpckhbw %mm4, %mm3                 
	paddw %mm2, %mm0                     
	paddw %mm3, %mm1                     
	psubw %mm5, %mm0                     
	psubw %mm6, %mm1                     
	movq %mm0, 48(%rcx)                     	# tmp1255
	movq %mm1, 56(%rcx)                     	# tmp1255
	movq (%rdx), %mm2                       	# src
	movq (%rdx), %mm3                       	# src
	add %r8, %rdx                             	# tmp1254, src
	punpcklbw %mm4, %mm2                 
	punpckhbw %mm4, %mm3                 
	paddw %mm2, %mm0                     
	paddw %mm3, %mm1                     
	psubw %mm5, %mm0                     
	psubw %mm6, %mm1                     
	movq %mm0, 64(%rcx)                     	# tmp1255
	movq %mm1, 72(%rcx)                     	# tmp1255
	movq %mm7, %mm6                      
	punpckhbw %mm4, %mm7                 
	punpcklbw %mm4, %mm6                 
	movq (%rdx), %mm2                       	# src
	movq (%rdx), %mm3                       	# src
	add %r8, %rdx                             	# tmp1254, src
	punpcklbw %mm4, %mm2                 
	punpckhbw %mm4, %mm3                 
	paddw %mm2, %mm0                     
	paddw %mm3, %mm1                     
	mov %rax, %rdx                             	# src, src
	add %r8, %rdx                             	# tmp1254, src
	movq (%rdx), %mm2                       	# src
	movq (%rdx), %mm3                       	# src
	add %r8, %rdx                             	# tmp1254, src
	punpcklbw %mm4, %mm2                 
	punpckhbw %mm4, %mm3                 
	psubw %mm2, %mm0                     
	psubw %mm3, %mm1                     
	movq %mm0, 80(%rcx)                     	# tmp1255
	movq %mm1, 88(%rcx)                     	# tmp1255
	movq (%rdx), %mm2                       	# src
	movq (%rdx), %mm3                       	# src
	add %r8, %rdx                             	# tmp1254, src
	punpcklbw %mm4, %mm2                 
	punpckhbw %mm4, %mm3                 
	psubw %mm2, %mm0                     
	psubw %mm3, %mm1                     
	paddw %mm6, %mm0                     
	paddw %mm7, %mm1                     
	movq %mm0, 96(%rcx)                     	# tmp1255
	movq %mm1, 104(%rcx)                    	# tmp1255
	movq (%rdx), %mm2                       	# src
	movq (%rdx), %mm3                       	# src
	add %r8, %rdx                             	# tmp1254, src
	punpcklbw %mm4, %mm2                 
	punpckhbw %mm4, %mm3                 
	psubw %mm2, %mm0                     
	psubw %mm3, %mm1                     
	paddw %mm6, %mm0                     
	paddw %mm7, %mm1                     
	movq %mm0, 112(%rcx)                    	# tmp1255
	movq %mm1, 120(%rcx)                    	# tmp1255
	movq (%rdx), %mm2                       	# src
	movq (%rdx), %mm3                       	# src
	add %r8, %rdx                             	# tmp1254, src
	punpcklbw %mm4, %mm2                 
	punpckhbw %mm4, %mm3                 
	psubw %mm2, %mm0                     
	psubw %mm3, %mm1                     
	paddw %mm6, %mm0                     
	paddw %mm7, %mm1                     
	movq %mm0, 128(%rcx)                    	# tmp1255
	movq %mm1, 136(%rcx)                    	# tmp1255
	movq (%rdx), %mm2                       	# src
	movq (%rdx), %mm3                       	# src
	add %r8, %rdx                             	# tmp1254, src
	punpcklbw %mm4, %mm2                 
	punpckhbw %mm4, %mm3                 
	psubw %mm2, %mm0                     
	psubw %mm3, %mm1                     
	paddw %mm6, %mm0                     
	paddw %mm7, %mm1                     
	movq %mm0, 144(%rcx)                    	# tmp1255
	movq %mm1, 152(%rcx)                    	# tmp1255
	mov %rax, %rdx                             	# src, src
	
# 0 "" 2
#NO_APP
	leaq	16(%rdx), %rax	#, src
	addq	$144, %rdx	#, D.15895
	movq	$-128, %rdi	#, offset
#APP
# 2804 "postprocess_template.c" 1
	movq 320(%rsp), %mm6                         	# both_masks
	pcmpeqb %mm5, %mm5                   
	pxor %mm6, %mm5                      
	pxor %mm7, %mm7                      
	1:                                     
	movq (%rcx), %mm0                       	# temp_sums
	movq 8(%rcx), %mm1                      	# temp_sums
	paddw 32(%rcx), %mm0                    	# temp_sums
	paddw 40(%rcx), %mm1                    	# temp_sums
	movq (%rdi, %rdx), %mm2                   	# offset, D.15895
	movq %mm2, %mm3                      
	movq %mm2, %mm4                      
	punpcklbw %mm7, %mm2                 
	punpckhbw %mm7, %mm3                 
	paddw %mm2, %mm0                     
	paddw %mm3, %mm1                     
	paddw %mm2, %mm0                     
	paddw %mm3, %mm1                     
	psrlw $4, %mm0                        
	psrlw $4, %mm1                        
	packuswb %mm1, %mm0                  
	pand %mm6, %mm0                      
	pand %mm5, %mm4                      
	por %mm4, %mm0                       
	movq %mm0, (%rdi, %rdx)                   	# offset, D.15895
	add $16, %rcx                            	# temp_sums
	add %r8, %rdi                             	# tmp1254, offset
	 js 1b                                 
	
# 0 "" 2
#NO_APP
.L976:
	cmpq	$-1, %rsi	#, D.15906
	je	.L972	#,
	leaq	336(%rsp), %rsi	#, tmp1264
	movq	%rax, %rdx	# src, temp_src
	movl	$16, %ecx	#, tmp1263
#APP
# 2844 "postprocess_template.c" 1
	pxor %mm7, %mm7                      
	movq (%rdx), %mm0                       	# temp_src
	movq %mm0, %mm1                      
	punpcklbw %mm7, %mm0                 
	punpckhbw %mm7, %mm1                 
	movq (%rdx, %rcx), %mm2                   	# temp_src, tmp1263
	lea (%rdx, %rcx, 2), %rax             	# temp_src, tmp1263
	movq %mm2, %mm3                      
	punpcklbw %mm7, %mm2                 
	punpckhbw %mm7, %mm3                 
	movq (%rax), %mm4                
	movq %mm4, %mm5                      
	punpcklbw %mm7, %mm4                 
	punpckhbw %mm7, %mm5                 
	paddw %mm0, %mm0                     
	paddw %mm1, %mm1                     
	psubw %mm4, %mm2                     
	psubw %mm5, %mm3                     
	psubw %mm2, %mm0                     
	psubw %mm3, %mm1                     
	psllw $2, %mm2                        
	psllw $2, %mm3                        
	psubw %mm2, %mm0                     
	psubw %mm3, %mm1                     
	movq (%rax, %rcx), %mm2            	# tmp1263
	movq %mm2, %mm3                      
	punpcklbw %mm7, %mm2                 
	punpckhbw %mm7, %mm3                 
	psubw %mm2, %mm0                     
	psubw %mm3, %mm1                     
	psubw %mm2, %mm0                     
	psubw %mm3, %mm1                     
	movq %mm0, (%rsi)                       	# tmp1264
	movq %mm1, 8(%rsi)                      	# tmp1264
	movq (%rax, %rcx, 2), %mm0         	# tmp1263
	movq %mm0, %mm1                      
	punpcklbw %mm7, %mm0                 
	punpckhbw %mm7, %mm1                 
	psubw %mm0, %mm2                     
	psubw %mm1, %mm3                     
	movq %mm2, 16(%rsi)                     	# tmp1264
	movq %mm3, 24(%rsi)                     	# tmp1264
	paddw %mm4, %mm4                     
	paddw %mm5, %mm5                     
	psubw %mm2, %mm4                     
	psubw %mm3, %mm5                     
	lea (%rax, %rcx), %rdx                	# tmp1263, temp_src
	psllw $2, %mm2                        
	psllw $2, %mm3                        
	psubw %mm2, %mm4                     
	psubw %mm3, %mm5                     
	movq (%rdx, %rcx, 2), %mm2                	# temp_src, tmp1263
	movq %mm2, %mm3                      
	punpcklbw %mm7, %mm2                 
	punpckhbw %mm7, %mm3                 
	psubw %mm2, %mm4                     
	psubw %mm3, %mm5                     
	psubw %mm2, %mm4                     
	psubw %mm3, %mm5                     
	movq (%rax, %rcx, 4), %mm6         	# tmp1263
	punpcklbw %mm7, %mm6                 
	psubw %mm6, %mm2                     
	movq (%rax, %rcx, 4), %mm6         	# tmp1263
	punpckhbw %mm7, %mm6                 
	psubw %mm6, %mm3                     
	paddw %mm0, %mm0                     
	paddw %mm1, %mm1                     
	psubw %mm2, %mm0                     
	psubw %mm3, %mm1                     
	psllw $2, %mm2                        
	psllw $2, %mm3                        
	psubw %mm2, %mm0                     
	psubw %mm3, %mm1                     
	movq (%rdx, %rcx, 4), %mm2                	# temp_src, tmp1263
	movq %mm2, %mm3                      
	punpcklbw %mm7, %mm2                 
	punpckhbw %mm7, %mm3                 
	paddw %mm2, %mm2                     
	paddw %mm3, %mm3                     
	psubw %mm2, %mm0                     
	psubw %mm3, %mm1                     
	movq (%rsi), %mm2                       	# tmp1264
	movq 8(%rsi), %mm3                      	# tmp1264
	movq %mm7, %mm6                      
	pcmpgtw %mm0, %mm6                   
	pxor %mm6, %mm0                      
	psubw %mm6, %mm0                     
	movq %mm7, %mm6                      
	pcmpgtw %mm1, %mm6                   
	pxor %mm6, %mm1                      
	psubw %mm6, %mm1                     
	movq %mm7, %mm6                      
	pcmpgtw %mm2, %mm6                   
	pxor %mm6, %mm2                      
	psubw %mm6, %mm2                     
	movq %mm7, %mm6                      
	pcmpgtw %mm3, %mm6                   
	pxor %mm6, %mm3                      
	psubw %mm6, %mm3                     
	movq %mm0, %mm6                      
	psubusw %mm2, %mm6                   
	psubw %mm6, %mm0                     
	movq %mm1, %mm6                      
	psubusw %mm3, %mm6                   
	psubw %mm6, %mm1                     
	movd 1760(%rsp), %mm2                         	# c.pQPb
	punpcklbw %mm7, %mm2                 
	movq %mm7, %mm6                      
	pcmpgtw %mm4, %mm6                   
	pxor %mm6, %mm4                      
	psubw %mm6, %mm4                     
	pcmpgtw %mm5, %mm7                   
	pxor %mm7, %mm5                      
	psubw %mm7, %mm5                     
	psllw $3, %mm2                        
	movq %mm2, %mm3                      
	pcmpgtw %mm4, %mm2                   
	pcmpgtw %mm5, %mm3                   
	pand %mm2, %mm4                      
	pand %mm3, %mm5                      
	psubusw %mm0, %mm4                   
	psubusw %mm1, %mm5                   
	movq w05, %mm2              
	pmullw %mm2, %mm4                    
	pmullw %mm2, %mm5                    
	movq w20, %mm2              
	paddw %mm2, %mm4                     
	paddw %mm2, %mm5                     
	psrlw $6, %mm4                        
	psrlw $6, %mm5                        
	movq 16(%rsi), %mm0                     	# tmp1264
	movq 24(%rsi), %mm1                     	# tmp1264
	pxor %mm2, %mm2                      
	pxor %mm3, %mm3                      
	pcmpgtw %mm0, %mm2                   
	pcmpgtw %mm1, %mm3                   
	pxor %mm2, %mm0                      
	pxor %mm3, %mm1                      
	psubw %mm2, %mm0                     
	psubw %mm3, %mm1                     
	psrlw $1, %mm0                        
	psrlw $1, %mm1                        
	pxor %mm6, %mm2                      
	pxor %mm7, %mm3                      
	pand %mm2, %mm4                      
	pand %mm3, %mm5                      
	movq %mm4, %mm2                      
	psubusw %mm0, %mm2                   
	psubw %mm2, %mm4                     
	movq %mm5, %mm2                      
	psubusw %mm1, %mm2                   
	psubw %mm2, %mm5                     
	pxor %mm6, %mm4                      
	pxor %mm7, %mm5                      
	psubw %mm6, %mm4                     
	psubw %mm7, %mm5                     
	packsswb %mm5, %mm4                  
	movq 312(%rsp), %mm1                         	# eq_mask
	pandn %mm4, %mm1                     
	movq (%rdx), %mm0                       	# temp_src
	paddb   %mm1, %mm0                   
	movq %mm0, (%rdx)                       	# temp_src
	movq (%rdx, %rcx), %mm0                   	# temp_src, tmp1263
	psubb %mm1, %mm0                     
	movq %mm0, (%rdx, %rcx)                   	# temp_src, tmp1263
	
# 0 "" 2
#NO_APP
.L972:
	leaq	-4(%rbx), %rcx	#, D.15895
	leaq	64(%r12), %rsi	#, D.15894
#APP
# 2080 "postprocess_template.c" 1
	lea (%rcx, %r14), %rax                	# D.15895, D.15906
	lea (%rax,%r14,4), %rdx        	# D.15906
	movq (%rsi), %mm0                       	# D.15894
	movq 16(%rsi), %mm1                     	# D.15894
	movq %mm0, %mm2                      
	punpcklbw %mm1, %mm0                 
	punpckhbw %mm1, %mm2                 
	movq 32(%rsi), %mm1                     	# D.15894
	movq 48(%rsi), %mm3                     	# D.15894
	movq %mm1, %mm4                      
	punpcklbw %mm3, %mm1                 
	punpckhbw %mm3, %mm4                 
	movq %mm0, %mm3                      
	punpcklwd %mm1, %mm0                 
	punpckhwd %mm1, %mm3                 
	movq %mm2, %mm1                      
	punpcklwd %mm4, %mm2                 
	punpckhwd %mm4, %mm1                 
	movd %mm0, (%rcx)                       	# D.15895
	psrlq $32, %mm0                       
	movd %mm0, (%rax)                
	movd %mm3, (%rax, %r14)            	# D.15906
	psrlq $32, %mm3                       
	movd %mm3, (%rax, %r14, 2)         	# D.15906
	movd %mm2, (%rcx, %r14, 4)                	# D.15895, D.15906
	psrlq $32, %mm2                       
	movd %mm2, (%rdx)                
	movd %mm1, (%rdx, %r14)            	# D.15906
	psrlq $32, %mm1                       
	movd %mm1, (%rdx, %r14, 2)         	# D.15906
	movq 64(%rsi), %mm0                     	# D.15894
	movq 80(%rsi), %mm1                     	# D.15894
	movq %mm0, %mm2                      
	punpcklbw %mm1, %mm0                 
	punpckhbw %mm1, %mm2                 
	movq 96(%rsi), %mm1                     	# D.15894
	movq 112(%rsi), %mm3                    	# D.15894
	movq %mm1, %mm4                      
	punpcklbw %mm3, %mm1                 
	punpckhbw %mm3, %mm4                 
	movq %mm0, %mm3                      
	punpcklwd %mm1, %mm0                 
	punpckhwd %mm1, %mm3                 
	movq %mm2, %mm1                      
	punpcklwd %mm4, %mm2                 
	punpckhwd %mm4, %mm1                 
	movd %mm0, 4(%rcx)                      	# D.15895
	psrlq $32, %mm0                       
	movd %mm0, 4(%rax)               
	movd %mm3, 4(%rax, %r14)           	# D.15906
	psrlq $32, %mm3                       
	movd %mm3, 4(%rax, %r14, 2)        	# D.15906
	movd %mm2, 4(%rcx, %r14, 4)               	# D.15895, D.15906
	psrlq $32, %mm2                       
	movd %mm2, 4(%rdx)               
	movd %mm1, 4(%rdx, %r14)           	# D.15906
	psrlq $32, %mm1                       
	movd %mm1, 4(%rdx, %r14, 2)        	# D.15906
	
# 0 "" 2
#NO_APP
	cmpl	$0, 16(%rsp)	#, %sfp
	jle	.L978	#,
	cmpb	$0, 107(%rsp)	#, %sfp
	je	.L978	#,
	movq	%rbx, %rax	# dstBlock, D.15897
	subq	72(%rsp), %rax	# %sfp, D.15897
	leaq	-8(%rax), %rdi	#, D.15895
	leaq	1648(%rsp), %rdx	#, tmp1663
	movl	4(%rsp), %esi	# %sfp,
	call	dering_3DNow	#
.L978:
	testl	$1048576, %r15d	#, D.15893
	je	.L970	#,
	movslq	2992(%rsp), %rdx	# isColor, isColor
	movl	%ebp, %eax	# x, D.15893
	sarl	$3, %eax	#, D.15893
	cltq
	addq	144(%rsp), %rax	# %sfp, D.15897
	movq	1712(%rsp,%rdx,8), %rcx	# c.tempBlurredPast, tmp1282
	leaq	(%rcx,%rax,4), %rax	#, D.15912
	movslq	%ebp, %rsi	# x, D.15897
	addq	128(%rsp), %rsi	# %sfp, D.15897
	addq	1688(%rsp,%rdx,8), %rsi	# c.tempBlurred, D.15895
	movq	%rax, 328(%rsp)	# D.15912, tempBlurredPast
	movl	2880(%rsp), %edx	# MEM[(const int *)&c + 1232B], MEM[(const int *)&c + 1232B]
	movl	%edx, 508(%rax)	# MEM[(const int *)&c + 1232B], MEM[(uint32_t *)_399 + 508B]
	movl	2884(%rsp), %edx	# MEM[(const int *)&c + 1236B], MEM[(const int *)&c + 1236B]
	movl	%edx, 512(%rax)	# MEM[(const int *)&c + 1236B], MEM[(uint32_t *)_399 + 512B]
	movl	2888(%rsp), %edx	# MEM[(const int *)&c + 1240B], MEM[(const int *)&c + 1240B]
	movl	%edx, 516(%rax)	# MEM[(const int *)&c + 1240B], MEM[(uint32_t *)_399 + 516B]
	leaq	-8(%rbx), %rdi	#, D.15895
#APP
# 2169 "postprocess_template.c" 1
	lea (%r14, %r14, 2), %rax             	# D.15906
	lea (%r14, %r14, 4), %rdx             	# D.15906
	lea (%rdx, %r14, 2), %rcx      	# D.15906
	pcmpeqb %mm7, %mm7                   
	movq b80, %mm6              
	pxor %mm0, %mm0                      
	movq (%rdi), %mm5                     	# D.15895
	movq (%rsi), %mm2                     	# D.15895
	pxor %mm7, %mm2                      
	pavgusb %mm2, %mm5 
	paddb %mm6, %mm5                     
	movq %mm5, %mm2                      
	psllw $8, %mm5                        
	pmaddwd %mm5, %mm5                   
	pmaddwd %mm2, %mm2                   
	paddd %mm2, %mm5                     
	psrld $14, %mm5                       
	paddd %mm5, %mm0                     
	movq (%rdi, %r14), %mm5                     	# D.15895, D.15906
	movq (%rsi, %r14), %mm2                     	# D.15895, D.15906
	pxor %mm7, %mm2                      
	pavgusb %mm2, %mm5 
	paddb %mm6, %mm5                     
	movq %mm5, %mm2                      
	psllw $8, %mm5                        
	pmaddwd %mm5, %mm5                   
	pmaddwd %mm2, %mm2                   
	paddd %mm2, %mm5                     
	psrld $14, %mm5                       
	paddd %mm5, %mm0                     
	movq (%rdi, %r14, 2), %mm5                     	# D.15895, D.15906
	movq (%rsi, %r14, 2), %mm2                     	# D.15895, D.15906
	pxor %mm7, %mm2                      
	pavgusb %mm2, %mm5 
	paddb %mm6, %mm5                     
	movq %mm5, %mm2                      
	psllw $8, %mm5                        
	pmaddwd %mm5, %mm5                   
	pmaddwd %mm2, %mm2                   
	paddd %mm2, %mm5                     
	psrld $14, %mm5                       
	paddd %mm5, %mm0                     
	movq (%rdi, %rax), %mm5                     	# D.15895
	movq (%rsi, %rax), %mm2                     	# D.15895
	pxor %mm7, %mm2                      
	pavgusb %mm2, %mm5 
	paddb %mm6, %mm5                     
	movq %mm5, %mm2                      
	psllw $8, %mm5                        
	pmaddwd %mm5, %mm5                   
	pmaddwd %mm2, %mm2                   
	paddd %mm2, %mm5                     
	psrld $14, %mm5                       
	paddd %mm5, %mm0                     
	movq (%rdi, %r14, 4), %mm5                     	# D.15895, D.15906
	movq (%rsi, %r14, 4), %mm2                     	# D.15895, D.15906
	pxor %mm7, %mm2                      
	pavgusb %mm2, %mm5 
	paddb %mm6, %mm5                     
	movq %mm5, %mm2                      
	psllw $8, %mm5                        
	pmaddwd %mm5, %mm5                   
	pmaddwd %mm2, %mm2                   
	paddd %mm2, %mm5                     
	psrld $14, %mm5                       
	paddd %mm5, %mm0                     
	movq (%rdi, %rdx), %mm5                     	# D.15895
	movq (%rsi, %rdx), %mm2                     	# D.15895
	pxor %mm7, %mm2                      
	pavgusb %mm2, %mm5 
	paddb %mm6, %mm5                     
	movq %mm5, %mm2                      
	psllw $8, %mm5                        
	pmaddwd %mm5, %mm5                   
	pmaddwd %mm2, %mm2                   
	paddd %mm2, %mm5                     
	psrld $14, %mm5                       
	paddd %mm5, %mm0                     
	movq (%rdi, %rax,2), %mm5                     	# D.15895
	movq (%rsi, %rax,2), %mm2                     	# D.15895
	pxor %mm7, %mm2                      
	pavgusb %mm2, %mm5 
	paddb %mm6, %mm5                     
	movq %mm5, %mm2                      
	psllw $8, %mm5                        
	pmaddwd %mm5, %mm5                   
	pmaddwd %mm2, %mm2                   
	paddd %mm2, %mm5                     
	psrld $14, %mm5                       
	paddd %mm5, %mm0                     
	movq (%rdi, %rcx), %mm5                     	# D.15895
	movq (%rsi, %rcx), %mm2                     	# D.15895
	pxor %mm7, %mm2                      
	pavgusb %mm2, %mm5 
	paddb %mm6, %mm5                     
	movq %mm5, %mm2                      
	psllw $8, %mm5                        
	pmaddwd %mm5, %mm5                   
	pmaddwd %mm2, %mm2                   
	paddd %mm2, %mm5                     
	psrld $14, %mm5                       
	paddd %mm5, %mm0                     
	movq %mm0, %mm4                      
	psrlq $32, %mm0                       
	paddd %mm0, %mm4                     
	movd %mm4, %ecx                      
	shll $2, %ecx                         
	mov 328(%rsp), %rdx                      	# tempBlurredPast
	addl -4(%rdx), %ecx              
	addl 4(%rdx), %ecx               
	addl -1024(%rdx), %ecx           
	addl $4, %ecx                         
	addl 1024(%rdx), %ecx            
	shrl $3, %ecx                         
	movl %ecx, (%rdx)                
	cmpl 512(%rdx), %ecx             
	 jb 2f                                 
	cmpl 516(%rdx), %ecx             
	 jb 1f                                 
	lea (%rax, %r14, 2), %rdx      	# D.15906
	lea (%rdx, %r14, 2), %rcx      	# D.15906
	movq (%rdi), %mm0                       	# D.15895
	movq (%rdi, %r14), %mm1                   	# D.15895, D.15906
	movq (%rdi, %r14, 2), %mm2                	# D.15895, D.15906
	movq (%rdi, %rax), %mm3            	# D.15895
	movq (%rdi, %r14, 4), %mm4                	# D.15895, D.15906
	movq (%rdi, %rdx), %mm5            	# D.15895
	movq (%rdi, %rax, 2), %mm6         	# D.15895
	movq (%rdi, %rcx), %mm7            	# D.15895
	movq %mm0, (%rsi)                       	# D.15895
	movq %mm1, (%rsi, %r14)                   	# D.15895, D.15906
	movq %mm2, (%rsi, %r14, 2)                	# D.15895, D.15906
	movq %mm3, (%rsi, %rax)            	# D.15895
	movq %mm4, (%rsi, %r14, 4)                	# D.15895, D.15906
	movq %mm5, (%rsi, %rdx)            	# D.15895
	movq %mm6, (%rsi, %rax, 2)         	# D.15895
	movq %mm7, (%rsi, %rcx)            	# D.15895
	jmp 4f                                 
	1:                                     
	lea (%rax, %r14, 2), %rdx      	# D.15906
	lea (%rdx, %r14, 2), %rcx      	# D.15906
	movq (%rdi), %mm0                       	# D.15895
	pavgusb (%rsi), %mm0 	# D.15895
	movq (%rdi, %r14), %mm1                   	# D.15895, D.15906
	pavgusb (%rsi, %r14), %mm1 	# D.15895, D.15906
	movq (%rdi, %r14, 2), %mm2                	# D.15895, D.15906
	pavgusb (%rsi, %r14, 2), %mm2 	# D.15895, D.15906
	movq (%rdi, %rax), %mm3            	# D.15895
	pavgusb (%rsi, %rax), %mm3 	# D.15895
	movq (%rdi, %r14, 4), %mm4                	# D.15895, D.15906
	pavgusb (%rsi, %r14, 4), %mm4 	# D.15895, D.15906
	movq (%rdi, %rdx), %mm5            	# D.15895
	pavgusb (%rsi, %rdx), %mm5 	# D.15895
	movq (%rdi, %rax, 2), %mm6         	# D.15895
	pavgusb (%rsi, %rax, 2), %mm6 	# D.15895
	movq (%rdi, %rcx), %mm7            	# D.15895
	pavgusb (%rsi, %rcx), %mm7 	# D.15895
	movq %mm0, (%rsi)                       	# D.15895
	movq %mm1, (%rsi, %r14)                   	# D.15895, D.15906
	movq %mm2, (%rsi, %r14, 2)                	# D.15895, D.15906
	movq %mm3, (%rsi, %rax)            	# D.15895
	movq %mm4, (%rsi, %r14, 4)                	# D.15895, D.15906
	movq %mm5, (%rsi, %rdx)            	# D.15895
	movq %mm6, (%rsi, %rax, 2)         	# D.15895
	movq %mm7, (%rsi, %rcx)            	# D.15895
	movq %mm0, (%rdi)                       	# D.15895
	movq %mm1, (%rdi, %r14)                   	# D.15895, D.15906
	movq %mm2, (%rdi, %r14, 2)                	# D.15895, D.15906
	movq %mm3, (%rdi, %rax)            	# D.15895
	movq %mm4, (%rdi, %r14, 4)                	# D.15895, D.15906
	movq %mm5, (%rdi, %rdx)            	# D.15895
	movq %mm6, (%rdi, %rax, 2)         	# D.15895
	movq %mm7, (%rdi, %rcx)            	# D.15895
	jmp 4f                                 
	2:                                     
	cmpl 508(%rdx), %ecx             
	 jb 3f                                 
	lea (%rax, %r14, 2), %rdx      	# D.15906
	lea (%rdx, %r14, 2), %rcx      	# D.15906
	movq (%rdi), %mm0                       	# D.15895
	movq (%rdi, %r14), %mm1                   	# D.15895, D.15906
	movq (%rdi, %r14, 2), %mm2                	# D.15895, D.15906
	movq (%rdi, %rax), %mm3            	# D.15895
	movq (%rsi), %mm4                       	# D.15895
	movq (%rsi, %r14), %mm5                   	# D.15895, D.15906
	movq (%rsi, %r14, 2), %mm6                	# D.15895, D.15906
	movq (%rsi, %rax), %mm7            	# D.15895
	pavgusb %mm4, %mm0 
	pavgusb %mm5, %mm1 
	pavgusb %mm6, %mm2 
	pavgusb %mm7, %mm3 
	pavgusb %mm4, %mm0 
	pavgusb %mm5, %mm1 
	pavgusb %mm6, %mm2 
	pavgusb %mm7, %mm3 
	movq %mm0, (%rsi)                       	# D.15895
	movq %mm1, (%rsi, %r14)                   	# D.15895, D.15906
	movq %mm2, (%rsi, %r14, 2)                	# D.15895, D.15906
	movq %mm3, (%rsi, %rax)            	# D.15895
	movq %mm0, (%rdi)                       	# D.15895
	movq %mm1, (%rdi, %r14)                   	# D.15895, D.15906
	movq %mm2, (%rdi, %r14, 2)                	# D.15895, D.15906
	movq %mm3, (%rdi, %rax)            	# D.15895
	movq (%rdi, %r14, 4), %mm0                	# D.15895, D.15906
	movq (%rdi, %rdx), %mm1            	# D.15895
	movq (%rdi, %rax, 2), %mm2         	# D.15895
	movq (%rdi, %rcx), %mm3            	# D.15895
	movq (%rsi, %r14, 4), %mm4                	# D.15895, D.15906
	movq (%rsi, %rdx), %mm5            	# D.15895
	movq (%rsi, %rax, 2), %mm6         	# D.15895
	movq (%rsi, %rcx), %mm7            	# D.15895
	pavgusb %mm4, %mm0 
	pavgusb %mm5, %mm1 
	pavgusb %mm6, %mm2 
	pavgusb %mm7, %mm3 
	pavgusb %mm4, %mm0 
	pavgusb %mm5, %mm1 
	pavgusb %mm6, %mm2 
	pavgusb %mm7, %mm3 
	movq %mm0, (%rsi, %r14, 4)                	# D.15895, D.15906
	movq %mm1, (%rsi, %rdx)            	# D.15895
	movq %mm2, (%rsi, %rax, 2)         	# D.15895
	movq %mm3, (%rsi, %rcx)            	# D.15895
	movq %mm0, (%rdi, %r14, 4)                	# D.15895, D.15906
	movq %mm1, (%rdi, %rdx)            	# D.15895
	movq %mm2, (%rdi, %rax, 2)         	# D.15895
	movq %mm3, (%rdi, %rcx)            	# D.15895
	jmp 4f                                 
	3:                                     
	lea (%rax, %r14, 2), %rdx      	# D.15906
	lea (%rdx, %r14, 2), %rcx      	# D.15906
	movq (%rdi), %mm0                       	# D.15895
	movq (%rdi, %r14), %mm1                   	# D.15895, D.15906
	movq (%rdi, %r14, 2), %mm2                	# D.15895, D.15906
	movq (%rdi, %rax), %mm3            	# D.15895
	movq (%rsi), %mm4                       	# D.15895
	movq (%rsi, %r14), %mm5                   	# D.15895, D.15906
	movq (%rsi, %r14, 2), %mm6                	# D.15895, D.15906
	movq (%rsi, %rax), %mm7            	# D.15895
	pavgusb %mm4, %mm0 
	pavgusb %mm5, %mm1 
	pavgusb %mm6, %mm2 
	pavgusb %mm7, %mm3 
	pavgusb %mm4, %mm0 
	pavgusb %mm5, %mm1 
	pavgusb %mm6, %mm2 
	pavgusb %mm7, %mm3 
	pavgusb %mm4, %mm0 
	pavgusb %mm5, %mm1 
	pavgusb %mm6, %mm2 
	pavgusb %mm7, %mm3 
	movq %mm0, (%rsi)                       	# D.15895
	movq %mm1, (%rsi, %r14)                   	# D.15895, D.15906
	movq %mm2, (%rsi, %r14, 2)                	# D.15895, D.15906
	movq %mm3, (%rsi, %rax)            	# D.15895
	movq %mm0, (%rdi)                       	# D.15895
	movq %mm1, (%rdi, %r14)                   	# D.15895, D.15906
	movq %mm2, (%rdi, %r14, 2)                	# D.15895, D.15906
	movq %mm3, (%rdi, %rax)            	# D.15895
	movq (%rdi, %r14, 4), %mm0                	# D.15895, D.15906
	movq (%rdi, %rdx), %mm1            	# D.15895
	movq (%rdi, %rax, 2), %mm2         	# D.15895
	movq (%rdi, %rcx), %mm3            	# D.15895
	movq (%rsi, %r14, 4), %mm4                	# D.15895, D.15906
	movq (%rsi, %rdx), %mm5            	# D.15895
	movq (%rsi, %rax, 2), %mm6         	# D.15895
	movq (%rsi, %rcx), %mm7            	# D.15895
	pavgusb %mm4, %mm0 
	pavgusb %mm5, %mm1 
	pavgusb %mm6, %mm2 
	pavgusb %mm7, %mm3 
	pavgusb %mm4, %mm0 
	pavgusb %mm5, %mm1 
	pavgusb %mm6, %mm2 
	pavgusb %mm7, %mm3 
	pavgusb %mm4, %mm0 
	pavgusb %mm5, %mm1 
	pavgusb %mm6, %mm2 
	pavgusb %mm7, %mm3 
	movq %mm0, (%rsi, %r14, 4)                	# D.15895, D.15906
	movq %mm1, (%rsi, %rdx)            	# D.15895
	movq %mm2, (%rsi, %rax, 2)         	# D.15895
	movq %mm3, (%rsi, %rcx)            	# D.15895
	movq %mm0, (%rdi, %r14, 4)                	# D.15895, D.15906
	movq %mm1, (%rdi, %rdx)            	# D.15895
	movq %mm2, (%rdi, %rax, 2)         	# D.15895
	movq %mm3, (%rdi, %rcx)            	# D.15895
	4:                                     
	
# 0 "" 2
#NO_APP
.L970:
	addq	$8, %rbx	#, dstBlock
	addl	$8, %ebp	#, x
	addq	$8, %r13	#, ivtmp.2080
	movq	8(%rsp), %rax	# %sfp, tempBlock2
	movq	%r12, 8(%rsp)	# tempBlock2, %sfp
	cmpl	%ebp, 28(%rsp)	# x, %sfp
	jg	.L995	#,
.L948:
	cmpl	$0, 16(%rsp)	#, %sfp
	jle	.L980	#,
	testb	$4, %r15b	#, D.15893
	je	.L980	#,
	movq	%rbx, %rax	# dstBlock, D.15897
	subq	72(%rsp), %rax	# %sfp, D.15897
	leaq	-8(%rax), %rdi	#, D.15895
	leaq	1648(%rsp), %rdx	#, tmp1671
	movl	4(%rsp), %esi	# %sfp,
	call	dering_3DNow	#
.L980:
	testl	$1048576, %r15d	#, D.15893
	je	.L981	#,
	movslq	2992(%rsp), %rsi	# isColor, isColor
	movl	16(%rsp), %eax	# %sfp, D.15893
	sarl	$3, %eax	#, D.15893
	sall	$8, %eax	#, D.15893
	cltq
	movl	%ebp, %edx	# x, D.15893
	sarl	$3, %edx	#, D.15893
	movslq	%edx, %rdx	# D.15893, D.15897
	leaq	256(%rax,%rdx), %rdx	#, D.15897
	movq	1712(%rsp,%rsi,8), %rax	# c.tempBlurredPast, tmp1314
	leaq	(%rax,%rdx,4), %rcx	#, D.15912
	movslq	%ebp, %rbp	# x, D.15897
	addq	128(%rsp), %rbp	# %sfp, D.15897
	movq	%rbp, %rdx	# D.15897, D.15895
	addq	1688(%rsp,%rsi,8), %rdx	# c.tempBlurred, D.15895
	leaq	-8(%rbx), %rdi	#, D.15895
	leaq	2880(%rsp), %r8	#,
	movl	4(%rsp), %esi	# %sfp,
	call	tempNoiseReducer_3DNow	#
.L981:
	movl	192(%rsp), %ebx	# %sfp, D.15893
	cmpl	%ebx, 24(%rsp)	# D.15893, %sfp
	jg	.L982	#,
	movl	28(%rsp), %ebx	# %sfp, width
	movl	288(%rsp), %esi	# %sfp, D.15893
	cmpl	%esi, %ebx	# D.15893, width
	je	.L983	#,
	movl	196(%rsp), %esi	# %sfp, ivtmp.2104
	movl	%esi, %r14d	# ivtmp.2104, D.15893
	movl	4(%rsp), %r13d	# %sfp, D.15892
	movl	$0, %eax	#, ivtmp.2056
	movl	$0, %ebp	#, i
	movslq	%ebx, %r12	# width, D.15898
	testl	%esi, %esi	# ivtmp.2104
	jg	.L997	#,
	jmp	.L982	#
.L983:
	movl	4(%rsp), %ecx	# %sfp,
	movl	196(%rsp), %edx	# %sfp,
	movq	256(%rsp), %rsi	# %sfp,
	movq	232(%rsp), %rdi	# %sfp,
	call	linecpy	#
	jmp	.L982	#
.L997:
	addl	$1, %ebp	#, i
	leal	0(%r13,%rax), %ebx	#, D.15892
	cltq
	movq	232(%rsp), %rsi	# %sfp, dstBlock
	leaq	(%rsi,%rax), %rdi	#, D.15895
	movslq	%ebx, %rsi	# D.15892, D.15897
	addq	224(%rsp), %rsi	# %sfp, D.15895
	movq	%r12, %rdx	# D.15898,
	call	memcpy	#
	movl	%ebx, %eax	# D.15892, ivtmp.2056
	cmpl	%r14d, %ebp	# D.15893, i
	jne	.L997	#,
.L982:
	addl	$8, 16(%rsp)	#, %sfp
	movl	16(%rsp), %eax	# %sfp, y
	movl	212(%rsp), %esi	# %sfp, D.15892
	addl	%esi, 156(%rsp)	# D.15892, %sfp
	movl	216(%rsp), %edi	# %sfp, D.15892
	addl	%edi, 152(%rsp)	# D.15892, %sfp
	subl	$8, 196(%rsp)	#, %sfp
	subl	$8, 200(%rsp)	#, %sfp
	movl	220(%rsp), %esi	# %sfp, D.15892
	addl	%esi, 204(%rsp)	# D.15892, %sfp
	cmpl	%eax, 24(%rsp)	# y, %sfp
	jg	.L986	#,
.L944:
#APP
# 3690 "postprocess_template.c" 1
	femms
# 0 "" 2
#NO_APP
	leaq	1648(%rsp), %rsi	#, tmp1333
	movl	$157, %ecx	#, tmp1334
	movq	3000(%rsp), %rdi	# c2, c2
	rep movsq
	jmp	.L1008	#
.L913:
	movl	2892(%rsp), %r9d	# c.ppMode.baseDcDiff, D.15892
	leaq	1648(%rsp), %rax	#, tmp1696
	leaq	1776(%rsp), %rdx	#, ivtmp.2157
	leaq	584(%rax), %r8	#, D.15913
	movl	$0, %ecx	#, ivtmp.2156
	movl	$126, %edi	#, tmp1357
	movabsq	$72340172838076673, %rsi	#, tmp1358
	jmp	.L914	#
.L1008:
	addq	$2920, %rsp	#,
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
.LFE157:
	.size	postProcess_3DNow, .-postProcess_3DNow
	.globl	postproc_version
	.type	postproc_version, @function
postproc_version:
.LFB73:
	.cfi_startproc
	movl	$3474276, %eax	#,
	ret
	.cfi_endproc
.LFE73:
	.size	postproc_version, .-postproc_version
	.section	.rodata.str1.1
.LC6:
	.string	""
	.text
	.globl	postproc_configuration
	.type	postproc_configuration, @function
postproc_configuration:
.LFB74:
	.cfi_startproc
	movl	$.LC6, %eax	#,
	ret
	.cfi_endproc
.LFE74:
	.size	postproc_configuration, .-postproc_configuration
	.section	.rodata.str1.8
	.align 8
.LC7:
	.string	"libpostproc license: LGPL version 2.1 or later"
	.text
	.globl	postproc_license
	.type	postproc_license, @function
postproc_license:
.LFB75:
	.cfi_startproc
	movl	$.LC7+21, %eax	#,
	ret
	.cfi_endproc
.LFE75:
	.size	postproc_license, .-postproc_license
	.section	.rodata.str1.1
.LC8:
	.string	"default"
.LC9:
	.string	"hb"
.LC10:
	.string	"pp: Missing argument\n"
.LC11:
	.string	"help"
.LC12:
	.string	"%s"
.LC14:
	.string	"pp: %s\n"
.LC15:
	.string	"pp: %s::%s\n"
.LC16:
	.string	"pp: option: %s\n"
.LC17:
	.string	"autoq"
.LC18:
	.string	"nochrom"
.LC19:
	.string	"chrom"
.LC20:
	.string	"noluma"
.LC21:
	.string	"fullyrange"
.LC22:
	.string	"pp: lumMode=%X, chromMode=%X\n"
	.section	.rodata.str1.8
	.align 8
.LC23:
	.string	"%d errors in postprocess string \"%s\"\n"
	.text
	.globl	pp_get_mode_by_name_and_quality
	.type	pp_get_mode_by_name_and_quality, @function
pp_get_mode_by_name_and_quality:
.LFB177:
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
	subq	$680, %rsp	#,
	.cfi_def_cfa_offset 736
	movq	%rdi, %rax	# name, name
	movq	%rdi, 56(%rsp)	# name, %sfp
	movl	%esi, 52(%rsp)	# quality, %sfp
	testq	%rdi, %rdi	# name
	jne	.L1016	#,
	movl	$.LC10, %edx	#,
	movl	$16, %esi	#,
	movl	$0, %edi	#,
	call	av_log	#
	movl	$0, %eax	#, D.15997
	jmp	.L1087	#
.L1016:
	movl	$.LC11, %edi	#, tmp220
	movl	$5, %ecx	#, tmp221
	movq	56(%rsp), %rsi	# %sfp, name
	repz cmpsb
	seta	%dl	#, tmp222
	setb	%al	#, tmp223
	movl	$pp_help, %ebx	#, p
	cmpb	%al, %dl	# tmp223, tmp222
	je	.L1094	#,
	jmp	.L1095	#
.L1021:
	subq	%rbx, %rax	# p, D.15998
	leaq	2(%rax), %rdx	#, D.15998
	cmpq	$500, %rdx	#, D.15998
	movl	$500, %eax	#, tmp350
	cmovnb	%rax, %rdx	# D.15998,, tmp350, D.15998
	movq	%rbx, %rsi	# p,
	leaq	160(%rsp), %rdi	#, tmp410
	call	av_strlcpy	#
	leaq	160(%rsp), %rcx	#, tmp411
	movl	$.LC12, %edx	#,
	movl	$32, %esi	#,
	movl	$0, %edi	#,
	movl	$0, %eax	#,
	call	av_log	#
	movl	$10, %esi	#,
	movq	%rbx, %rdi	# p,
	call	strchr	#
	leaq	1(%rax), %rbx	#, p
.L1094:
	movl	$10, %esi	#,
	movq	%rbx, %rdi	# p,
	call	strchr	#
	testq	%rax, %rax	# D.16000
	jne	.L1021	#,
	jmp	.L1087	#
.L1095:
	movl	$48, %edi	#,
	call	av_malloc	#
	movq	%rax, %r14	#, ppMode
	testq	%rax, %rax	# ppMode
	je	.L1066	#,
	movl	$0, (%rax)	#, ppMode_86->lumMode
	movl	$0, 4(%rax)	#, ppMode_86->chromMode
	movl	$700, 24(%rax)	#, ppMode_86->maxTmpNoise
	movl	$1500, 28(%rax)	#, ppMode_86->maxTmpNoise
	movl	$3000, 32(%rax)	#, ppMode_86->maxTmpNoise
	movl	$234, 16(%rax)	#, ppMode_86->maxAllowedY
	movl	$16, 12(%rax)	#, ppMode_86->minAllowedY
	movl	$32, 36(%rax)	#, ppMode_86->baseDcDiff
	movl	$39, 40(%rax)	#, ppMode_86->flatnessThreshold
	movl	$0x3c23d70a, 20(%rax)	#, ppMode_86->maxClippedThreshold
	movl	$0, 8(%rax)	#, ppMode_86->error
	leaq	160(%rsp), %rbx	#, tmp229
	movl	$62, %ecx	#, tmp232
	movl	$0, %eax	#, tmp231
	movq	%rbx, %rdi	# tmp229, tmp230
	rep stosq
	movl	$0, (%rdi)	#, MEM[(void *)&temp]
	movl	$499, %edx	#,
	movq	56(%rsp), %r15	# %sfp, name
	movq	%r15, %rsi	# name,
	movq	%rbx, %rdi	# tmp229,
	call	av_strlcpy	#
	movq	%r15, %rcx	# name,
	movl	$.LC14, %edx	#,
	movl	$48, %esi	#,
	movl	$0, %edi	#,
	movl	$0, %eax	#,
	call	av_log	#
	movq	%rbx, 40(%rsp)	# tmp229, %sfp
.L1062:
	leaq	64(%rsp), %rdx	#,
	movl	$filterDelimiters.6664, %esi	#,
	movq	40(%rsp), %r15	# %sfp, p
	movq	%r15, %rdi	# p,
	call	av_strtok	#
	movq	%rax, %rbx	#, filterToken
	testq	%rax, %rax	# filterToken
	je	.L1022	#,
	movq	%rax, %rdi	# filterToken, filterToken
	movl	$0, %eax	#, tmp240
	movq	$-1, %rcx	#, tmp239
	repnz scasb
	notq	%rcx	# tmp237
	movq	%r15, %rax	# p, p
	addq	%rcx, %rax	# tmp237, p
	movq	%rax, 40(%rsp)	# p, %sfp
	leaq	64(%rsp), %rdx	#,
	movl	$optionDelimiters.6665, %esi	#,
	movq	%rbx, %rdi	# filterToken,
	call	av_strtok	#
	movq	%rax, 8(%rsp)	# filterName, %sfp
	testq	%rax, %rax	# filterName
	jne	.L1023	#,
	addl	$1, 8(%r14)	#, ppMode_86->error
	jmp	.L1022	#
.L1023:
	movq	8(%rsp), %r15	# %sfp, filterName
	movq	%r15, %r8	# filterName,
	movq	%rbx, %rcx	# filterToken,
	movl	$.LC15, %edx	#,
	movl	$48, %esi	#,
	movl	$0, %edi	#,
	movl	$0, %eax	#,
	call	av_log	#
	movl	$1, 28(%rsp)	#, %sfp
	cmpb	$45, (%r15)	#, *filterName_107
	jne	.L1024	#,
	addq	$1, 8(%rsp)	#, %sfp
	movl	$0, 28(%rsp)	#, %sfp
.L1024:
	movl	$0, %ebx	#, numOfUnknownOptions
	movl	$-1, %eax	#, luma
	movl	%eax, 36(%rsp)	# luma, %sfp
	movl	%eax, 24(%rsp)	# luma, %sfp
	movl	$1000000, %r12d	#, quality
	movl	52(%rsp), %r13d	# %sfp, quality
.L1034:
	leaq	64(%rsp), %rdx	#,
	movl	$optionDelimiters.6665, %esi	#,
	movl	$0, %edi	#,
	call	av_strtok	#
	movq	%rax, %rbp	#, __s2
	testq	%rax, %rax	# __s2
	je	.L1025	#,
	movq	%rax, %rcx	# __s2,
	movl	$.LC16, %edx	#,
	movl	$48, %esi	#,
	movl	$0, %edi	#,
	movl	$0, %eax	#,
	call	av_log	#
	movl	$.LC17, %esi	#, tmp245
	movl	$6, %ecx	#, tmp247
	movq	%rbp, %rdi	# __s2, __s2
	repz cmpsb
	seta	%dl	#, tmp248
	setb	%al	#, tmp249
	cmpb	%al, %dl	# tmp249, tmp248
	je	.L1068	#,
	movzbl	0(%rbp), %eax	# *__s2_112, D.15999
	cmpl	$97, %eax	#, D.15999
	jne	.L1027	#,
	cmpb	$0, 1(%rbp)	#, MEM[(const unsigned char *)__s2_112 + 1B]
	je	.L1069	#,
	jmp	.L1096	#
.L1027:
	movl	$.LC18, %esi	#, tmp251
	movl	$8, %ecx	#, tmp253
	movq	%rbp, %rdi	# __s2, __s2
	repz cmpsb
	seta	%cl	#, tmp254
	setb	%dl	#, tmp255
	cmpb	%dl, %cl	# tmp255, tmp254
	je	.L1070	#,
	cmpl	$121, %eax	#, D.15999
	jne	.L1029	#,
	cmpb	$0, 1(%rbp)	#, MEM[(const unsigned char *)__s2_112 + 1B]
	je	.L1071	#,
	jmp	.L1097	#
.L1029:
	movl	$.LC19, %esi	#, tmp257
	movl	$6, %ecx	#, tmp259
	movq	%rbp, %rdi	# __s2, __s2
	repz cmpsb
	seta	%cl	#, tmp260
	setb	%dl	#, tmp261
	cmpb	%dl, %cl	# tmp261, tmp260
	je	.L1072	#,
	cmpl	$99, %eax	#, D.15999
	jne	.L1031	#,
	cmpb	$0, 1(%rbp)	#, MEM[(const unsigned char *)__s2_112 + 1B]
	je	.L1073	#,
	jmp	.L1098	#
.L1031:
	movl	$.LC20, %esi	#, tmp263
	movl	$7, %ecx	#, tmp265
	movq	%rbp, %rdi	# __s2, __s2
	repz cmpsb
	seta	%cl	#, tmp266
	setb	%dl	#, tmp267
	cmpb	%dl, %cl	# tmp267, tmp266
	je	.L1074	#,
	cmpl	$110, %eax	#, D.15999
	jne	.L1033	#,
	cmpb	$0, 1(%rbp)	#, MEM[(const unsigned char *)__s2_112 + 1B]
	je	.L1075	#,
.L1033:
	movslq	%ebx, %rax	# numOfUnknownOptions, numOfUnknownOptions
	movq	%rbp, 80(%rsp,%rax,8)	# __s2, options
	addl	$1, %ebx	#, numOfUnknownOptions
	jmp	.L1026	#
.L1068:
	movl	%r13d, %r12d	# quality, quality
	jmp	.L1026	#
.L1069:
	movl	%r13d, %r12d	# quality, quality
	jmp	.L1026	#
.L1070:
	movl	$0, 24(%rsp)	#, %sfp
	jmp	.L1026	#
.L1071:
	movl	$0, 24(%rsp)	#, %sfp
	jmp	.L1026	#
.L1072:
	movl	$1, 24(%rsp)	#, %sfp
	jmp	.L1026	#
.L1073:
	movl	$1, 24(%rsp)	#, %sfp
	jmp	.L1026	#
.L1074:
	movl	$0, 36(%rsp)	#, %sfp
	jmp	.L1026	#
.L1075:
	movl	$0, 36(%rsp)	#, %sfp
	jmp	.L1026	#
.L1082:
	movl	$0, 24(%rsp)	#, %sfp
	jmp	.L1026	#
.L1083:
	movl	$1, 24(%rsp)	#, %sfp
	jmp	.L1026	#
.L1084:
	movl	$0, 36(%rsp)	#, %sfp
.L1026:
	cmpl	$8, %ebx	#, numOfUnknownOptions
	jle	.L1034	#,
	movl	%r12d, 20(%rsp)	# quality, %sfp
	movslq	%ebx, %rax	# numOfUnknownOptions, numOfUnknownOptions
	movq	$0, 80(%rsp,%rax,8)	#, options
.L1063:
	movl	$replaceTable, %r12d	#, ivtmp.2204
	movl	$.LC8, %edi	#, D.16006
	movl	$0, 32(%rsp)	#, %sfp
	movl	%ebx, 48(%rsp)	# numOfUnknownOptions, %sfp
	movq	40(%rsp), %rbx	# %sfp, p
	movq	8(%rsp), %r15	# %sfp, filterName
.L1038:
	movq	%r15, %rsi	# filterName,
	call	strcmp	#
	testl	%eax, %eax	# D.15999
	jne	.L1035	#,
	movq	8(%r12), %rbp	# MEM[base: _310, offset: 8B], D.16006
	movq	%rbp, %rdi	# D.16006, D.16006
	movq	$-1, %rcx	#, tmp271
	repnz scasb
	notq	%rcx	# tmp272
	leaq	-1(%rcx), %rsi	#, D.15998
	leaq	-1(%rbx), %r13	#, p
	movb	$44, -1(%rbx)	#, MEM[(char *)p_314 + -1B]
	movq	%r13, %rdi	# p, p
	movq	$-1, %rcx	#, tmp277
	repnz scasb
	notq	%rcx	# tmp278
	leaq	-1(%rcx), %rdx	#, D.15998
	movq	%r13, %rax	# p, D.16001
	leaq	160(%rsp), %rbx	#, tmp427
	subq	%rbx, %rax	# tmp427, D.16001
	addl	%edx, %eax	# D.15998, D.16007
	addl	%esi, %eax	# D.15998, D.15999
	cmpl	$498, %eax	#, D.15999
	jle	.L1036	#,
	movl	48(%rsp), %ebx	# %sfp, numOfUnknownOptions
	addl	$1, 8(%r14)	#, ppMode_86->error
	movq	%r13, 40(%rsp)	# p, %sfp
	jmp	.L1037	#
.L1036:
	movslq	%esi, %rbx	# D.15998, D.16003
	addl	$1, %edx	#, D.15999
	movslq	%edx, %rdx	# D.15999, D.15998
	leaq	0(%r13,%rbx), %rdi	#, D.15996
	movq	%r13, %rsi	# p,
	call	memmove	#
	movq	%rbx, %rdx	# D.16003,
	movq	%rbp, %rsi	# D.16006,
	movq	%r13, %rdi	# p,
	call	memcpy	#
	movq	%r13, %rbx	# p, p
	movl	$1, 32(%rsp)	#, %sfp
.L1035:
	addq	$16, %r12	#, ivtmp.2204
	movq	(%r12), %rdi	# MEM[base: _311, offset: 0B], D.16006
	testq	%rdi, %rdi	# D.16006
	jne	.L1038	#,
	movq	%rbx, 40(%rsp)	# p, %sfp
	movl	48(%rsp), %ebx	# %sfp, numOfUnknownOptions
	jmp	.L1037	#
.L1061:
	movq	%r15, %rbp	# ivtmp.2189, D.15996
	movq	8(%rsp), %rsi	# %sfp,
	movq	(%r15), %rdi	# MEM[base: _349, offset: 0B],
	call	strcmp	#
	testl	%eax, %eax	# D.15999
	je	.L1039	#,
	movq	8(%rsp), %rsi	# %sfp,
	movq	%r12, %rdi	# D.16006,
	call	strcmp	#
	testl	%eax, %eax	# D.15999
	jne	.L1040	#,
.L1039:
	movl	(%r14), %ecx	# ppMode_86->lumMode, D.15999
	movl	20(%rbp), %eax	# MEM[base: _349, offset: 20B], D.15999
	movl	%eax, %edx	# D.15999, D.15999
	notl	%edx	# D.15999
	movl	%edx, %esi	# D.15999, tmp295
	andl	%ecx, %esi	# D.15999, tmp295
	movl	%esi, (%r14)	# tmp295, ppMode_86->lumMode
	movl	4(%r14), %esi	# ppMode_86->chromMode, D.15999
	andl	%esi, %edx	# D.15999, tmp296
	movl	%edx, 4(%r14)	# tmp296, ppMode_86->chromMode
	cmpl	$0, 28(%rsp)	#, %sfp
	je	.L1041	#,
	movl	20(%rsp), %edx	# %sfp, quality
	cmpl	12(%rbp), %edx	# MEM[base: _349, offset: 12B], quality
	jl	.L1042	#,
	cmpl	$0, 36(%rsp)	#, %sfp
	je	.L1042	#,
	orl	%eax, %ecx	# D.15999, tmp301
	movl	%ecx, (%r14)	# tmp301, ppMode_86->lumMode
.L1042:
	movl	24(%rsp), %edi	# %sfp, chrom
	cmpl	$1, %edi	#, chrom
	je	.L1043	#,
	cmpl	$-1, %edi	#, chrom
	jne	.L1044	#,
	cmpl	$0, 8(%rbp)	#, MEM[base: _349, offset: 8B]
	je	.L1044	#,
.L1043:
	movl	20(%rsp), %ecx	# %sfp, quality
	cmpl	16(%rbp), %ecx	# MEM[base: _349, offset: 16B], quality
	jl	.L1044	#,
	orl	%eax, %esi	# D.15999, tmp302
	movl	%esi, 4(%r14)	# tmp302, ppMode_86->chromMode
.L1044:
	cmpl	$8, %eax	#, D.15999
	jne	.L1045	#,
	movl	$16, 12(%r14)	#, ppMode_86->minAllowedY
	movl	$234, 16(%r14)	#, ppMode_86->maxAllowedY
	movq	80(%rsp), %rax	# options, D.16006
	testq	%rax, %rax	# D.16006
	je	.L1076	#,
	leaq	88(%rsp), %rdx	#, ivtmp.2168
.L1048:
	movl	$.LC21, %edi	#, tmp306
	movq	%rax, %rsi	# D.16006, D.16006
	movl	$11, %ecx	#, tmp307
	repz cmpsb
	seta	%sil	#, tmp308
	setb	%cl	#, tmp309
	cmpb	%cl, %sil	# tmp309, tmp308
	je	.L1046	#,
	cmpb	$102, (%rax)	#, MEM[(const unsigned char *)_340]
	jne	.L1047	#,
	cmpb	$0, 1(%rax)	#, MEM[(const unsigned char *)_340 + 1B]
	jne	.L1047	#,
.L1046:
	movl	$0, 12(%r14)	#, ppMode_86->minAllowedY
	movl	$255, 16(%r14)	#, ppMode_86->maxAllowedY
	subl	$1, %ebx	#, numOfUnknownOptions
.L1047:
	addq	$8, %rdx	#, ivtmp.2168
	movq	-8(%rdx), %rax	# MEM[base: _54, offset: -8B], D.16006
	testq	%rax, %rax	# D.16006
	jne	.L1048	#,
	movl	$1, %r13d	#, filterNameOk
	jmp	.L1040	#
.L1045:
	cmpl	$1048576, %eax	#, D.15999
	jne	.L1049	#,
	movq	80(%rsp), %rbp	# options, D.16006
	testq	%rbp, %rbp	# D.16006
	je	.L1077	#,
	leaq	88(%rsp), %r13	#, ivtmp.2174
	movl	$0, %r12d	#, numOfNoises
.L1052:
	movl	$0, %edx	#,
	leaq	72(%rsp), %rsi	#, tmp440
	movq	%rbp, %rdi	# D.16006,
	call	strtol	#
	movslq	%r12d, %rdx	# numOfNoises, numOfNoises
	movl	%eax, 24(%r14,%rdx,4)	# D.16001, ppMode_86->maxTmpNoise
	cmpq	72(%rsp), %rbp	# tail, D.16006
	je	.L1050	#,
	addl	$1, %r12d	#, numOfNoises
	subl	$1, %ebx	#, numOfUnknownOptions
	cmpl	$2, %r12d	#, numOfNoises
	jg	.L1051	#,
.L1050:
	addq	$8, %r13	#, ivtmp.2174
	movq	-8(%r13), %rbp	# MEM[base: _25, offset: -8B], D.16006
	testq	%rbp, %rbp	# D.16006
	jne	.L1052	#,
	movl	$1, %r13d	#, filterNameOk
	jmp	.L1040	#
.L1051:
	movl	$1, %r13d	#, filterNameOk
	jmp	.L1040	#
.L1049:
	cmpl	$16384, %eax	#, D.15999
	sete	%cl	#, D.16002
	cmpl	$1024, %eax	#, D.15999
	sete	%dl	#, D.16002
	orb	%dl, %cl	# D.16002, tmp406
	jne	.L1085	#,
	leal	-1(%rax), %edx	#, D.16007
	cmpl	$1, %edx	#, D.16007
	ja	.L1053	#,
.L1085:
	movq	80(%rsp), %rbp	# options, D.16006
	movl	$1, %r13d	#, filterNameOk
	testq	%rbp, %rbp	# D.16006
	je	.L1040	#,
	leaq	88(%rsp), %r13	#, ivtmp.2183
	movl	$0, %r12d	#, o
.L1058:
	movl	$0, %edx	#,
	leaq	72(%rsp), %rsi	#, tmp443
	movq	%rbp, %rdi	# D.16006,
	call	strtol	#
	cmpq	72(%rsp), %rbp	# tail, D.16006
	je	.L1055	#,
	subl	$1, %ebx	#, numOfUnknownOptions
	testl	%r12d, %r12d	# o
	jne	.L1056	#,
	movl	%eax, 36(%r14)	# D.16001, ppMode_86->baseDcDiff
	jmp	.L1057	#
.L1056:
	movl	%eax, 40(%r14)	# D.16001, ppMode_86->flatnessThreshold
.L1057:
	addl	$1, %r12d	#, o
	movq	0(%r13), %rbp	# MEM[base: _12, offset: 0B], D.16006
	addq	$8, %r13	#, ivtmp.2183
	testq	%rbp, %rbp	# D.16006
	je	.L1086	#,
	cmpl	$1, %r12d	#, o
	jle	.L1058	#,
.L1086:
	movl	$1, %r13d	#, filterNameOk
	jmp	.L1040	#
.L1055:
	movl	$1, %r13d	#, filterNameOk
	jmp	.L1040	#
.L1053:
	movl	$1, %r13d	#, filterNameOk
	cmpl	$2097152, %eax	#, D.15999
	jne	.L1040	#,
	movl	$15, 44(%r14)	#, ppMode_86->forcedQuant
	movq	80(%rsp), %rbp	# options, D.16006
	testq	%rbp, %rbp	# D.16006
	je	.L1040	#,
	movl	$0, %edx	#,
	leaq	72(%rsp), %rsi	#, tmp444
	movq	%rbp, %rdi	# D.16006,
	call	strtol	#
	cmpq	%rbp, 72(%rsp)	# D.16006, tail
	je	.L1060	#,
	subl	$1, %ebx	#, numOfUnknownOptions
	movl	%eax, 44(%r14)	# D.16001, ppMode_86->forcedQuant
	jmp	.L1040	#
.L1060:
	movl	$1, %r13d	#, filterNameOk
	jmp	.L1040	#
.L1076:
	movl	$1, %r13d	#, filterNameOk
	jmp	.L1040	#
.L1077:
	movl	$1, %r13d	#, filterNameOk
.L1040:
	addq	$32, %r15	#, ivtmp.2189
	movq	-8(%r15), %r12	# MEM[base: _323, offset: -8B], D.16006
	testq	%r12, %r12	# D.16006
	jne	.L1061	#,
	testl	%r13d, %r13d	# filterNameOk
	jne	.L1041	#,
	addl	$1, 8(%r14)	#, ppMode_86->error
.L1041:
	addl	%ebx, 8(%r14)	# numOfUnknownOptions, ppMode_86->error
	jmp	.L1062	#
.L1022:
	movl	4(%r14), %r8d	# ppMode_86->chromMode,
	movl	(%r14), %ecx	# ppMode_86->lumMode,
	movl	$.LC22, %edx	#,
	movl	$48, %esi	#,
	movl	$0, %edi	#,
	movl	$0, %eax	#,
	call	av_log	#
	movl	8(%r14), %ecx	# ppMode_86->error, D.15999
	movq	%r14, %rax	# ppMode, D.15997
	testl	%ecx, %ecx	# D.15999
	je	.L1087	#,
	movq	56(%rsp), %r8	# %sfp,
	movl	$.LC23, %edx	#,
	movl	$16, %esi	#,
	movl	$0, %edi	#,
	movl	$0, %eax	#,
	call	av_log	#
	movq	%r14, %rdi	# ppMode,
	call	av_free	#
	movl	$0, %eax	#, D.15997
	jmp	.L1087	#
.L1066:
	movl	$0, %eax	#, D.15997
	jmp	.L1087	#
.L1025:
	movl	%r12d, 20(%rsp)	# quality, %sfp
	movslq	%ebx, %rax	# numOfUnknownOptions, numOfUnknownOptions
	movq	$0, 80(%rsp,%rax,8)	#, options
	jmp	.L1063	#
.L1037:
	movl	$filters+8, %r15d	#, ivtmp.2189
	movl	$.LC9, %r12d	#, D.16006
	movl	32(%rsp), %r13d	# %sfp, filterNameOk
	jmp	.L1061	#
.L1096:
	movl	$.LC18, %esi	#, tmp333
	movl	$8, %ecx	#, tmp335
	movq	%rbp, %rdi	# __s2, __s2
	repz cmpsb
	seta	%cl	#, tmp336
	setb	%dl	#, tmp337
	cmpb	%dl, %cl	# tmp337, tmp336
	je	.L1082	#,
	jmp	.L1029	#
.L1097:
	movl	$.LC19, %esi	#, tmp339
	movl	$6, %ecx	#, tmp341
	movq	%rbp, %rdi	# __s2, __s2
	repz cmpsb
	seta	%cl	#, tmp342
	setb	%dl	#, tmp343
	cmpb	%dl, %cl	# tmp343, tmp342
	je	.L1083	#,
	jmp	.L1031	#
.L1098:
	movl	$.LC20, %esi	#, tmp345
	movl	$7, %ecx	#, tmp347
	movq	%rbp, %rdi	# __s2, __s2
	repz cmpsb
	seta	%dl	#, tmp348
	setb	%al	#, tmp349
	cmpb	%al, %dl	# tmp349, tmp348
	je	.L1084	#,
	jmp	.L1033	#
.L1087:
	addq	$680, %rsp	#,
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
.LFE177:
	.size	pp_get_mode_by_name_and_quality, .-pp_get_mode_by_name_and_quality
	.globl	pp_free_mode
	.type	pp_free_mode, @function
pp_free_mode:
.LFB178:
	.cfi_startproc
	subq	$8, %rsp	#,
	.cfi_def_cfa_offset 16
	call	av_free	#
	addq	$8, %rsp	#,
	.cfi_def_cfa_offset 8
	ret
	.cfi_endproc
.LFE178:
	.size	pp_free_mode, .-pp_free_mode
	.globl	pp_get_context
	.type	pp_get_context, @function
pp_get_context:
.LFB182:
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
	subq	$8, %rsp	#,
	.cfi_def_cfa_offset 64
	movl	%edi, %r12d	# width, width
	movl	%esi, %r13d	# height, height
	movl	%edx, %ebp	# cpuCaps, cpuCaps
	movl	$1256, %edi	#,
	call	av_mallocz	#
	movq	%rax, %rbx	#, c
	leal	15(%r12), %eax	#, D.16032
	movl	%eax, %r14d	# D.16032, stride
	andl	$-16, %r14d	#, stride
	movl	$16, %ecx	#, tmp112
	cltd
	idivl	%ecx	# tmp112
	leal	2(%rax), %r15d	#, qpStride
	movl	$0, %eax	#, D.16031
	testq	%rbx, %rbx	# c
	je	.L1106	#,
	movq	$av_codec_context_class, (%rbx)	#, c_10->av_class
	testb	$8, %bpl	#, cpuCaps
	je	.L1107	#,
	movl	%ebp, %eax	# cpuCaps, tmp114
	andl	$3, %eax	#, tmp114
	movl	%eax, 1200(%rbx)	# tmp114, c_10->hChromaSubSample
	movl	%ebp, %eax	# cpuCaps, D.16032
	sarl	$4, %eax	#, D.16032
	andl	$3, %eax	#, tmp116
	movl	%eax, 1204(%rbx)	# tmp116, c_10->vChromaSubSample
	jmp	.L1108	#
.L1107:
	movl	$1, 1200(%rbx)	#, c_10->hChromaSubSample
	movl	$1, 1204(%rbx)	#, c_10->vChromaSubSample
.L1108:
	testl	$524288, %ebp	#, cpuCaps
	je	.L1109	#,
	call	av_get_cpu_flags	#
	movl	%eax, 1188(%rbx)	# D.16032, c_10->cpuCaps
	jmp	.L1110	#
.L1109:
	movl	%ebp, %eax	# cpuCaps, tmp122
	shrl	$31, %eax	#, tmp122
	movl	%eax, 1188(%rbx)	# tmp122, c_10->cpuCaps
	testl	$536870912, %ebp	#, cpuCaps
	je	.L1113	#,
	orl	$2, 1188(%rbx)	#, c_10->cpuCaps
.L1113:
	testl	$1073741824, %ebp	#, cpuCaps
	je	.L1114	#,
	orl	$4, 1188(%rbx)	#, c_10->cpuCaps
.L1114:
	testl	$268435456, %ebp	#, cpuCaps
	je	.L1110	#,
	orl	$1, 1188(%rbx)	#, c_10->cpuCaps
.L1110:
	movl	%r15d, %r8d	# qpStride,
	movl	%r14d, %ecx	# stride,
	movl	%r13d, %edx	# height,
	movl	%r12d, %esi	# width,
	movq	%rbx, %rdi	# c,
	call	reallocBuffers	#
	movl	$-1, 1184(%rbx)	#, c_10->frameNum
	movq	%rbx, %rax	# c, D.16031
.L1106:
	addq	$8, %rsp	#,
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
.LFE182:
	.size	pp_get_context, .-pp_get_context
	.globl	pp_free_context
	.type	pp_free_context, @function
pp_free_context:
.LFB183:
	.cfi_startproc
	pushq	%rbx	#
	.cfi_def_cfa_offset 16
	.cfi_offset 3, -16
	movq	%rdi, %rbx	# vc, vc
	movq	40(%rdi), %rdi	# MEM[(struct PPContext *)vc_5(D)].tempBlurred, MEM[(struct PPContext *)vc_5(D)].tempBlurred
	call	av_free	#
	movq	48(%rbx), %rdi	# MEM[(struct PPContext *)vc_5(D) + 48B], MEM[(struct PPContext *)vc_5(D) + 48B]
	call	av_free	#
	movq	56(%rbx), %rdi	# MEM[(struct PPContext *)vc_5(D) + 56B], MEM[(struct PPContext *)vc_5(D) + 56B]
	call	av_free	#
	movq	64(%rbx), %rdi	# MEM[(struct PPContext *)vc_5(D)].tempBlurredPast, MEM[(struct PPContext *)vc_5(D)].tempBlurredPast
	call	av_free	#
	movq	72(%rbx), %rdi	# MEM[(struct PPContext *)vc_5(D) + 72B], MEM[(struct PPContext *)vc_5(D) + 72B]
	call	av_free	#
	movq	80(%rbx), %rdi	# MEM[(struct PPContext *)vc_5(D) + 80B], MEM[(struct PPContext *)vc_5(D) + 80B]
	call	av_free	#
	movq	8(%rbx), %rdi	# MEM[(struct PPContext *)vc_5(D)].tempBlocks, MEM[(struct PPContext *)vc_5(D)].tempBlocks
	call	av_free	#
	movq	16(%rbx), %rdi	# MEM[(struct PPContext *)vc_5(D)].yHistogram, MEM[(struct PPContext *)vc_5(D)].yHistogram
	call	av_free	#
	movq	88(%rbx), %rdi	# MEM[(struct PPContext *)vc_5(D)].tempDst, MEM[(struct PPContext *)vc_5(D)].tempDst
	call	av_free	#
	movq	96(%rbx), %rdi	# MEM[(struct PPContext *)vc_5(D)].tempSrc, MEM[(struct PPContext *)vc_5(D)].tempSrc
	call	av_free	#
	movq	104(%rbx), %rdi	# MEM[(struct PPContext *)vc_5(D)].deintTemp, MEM[(struct PPContext *)vc_5(D)].deintTemp
	call	av_free	#
	movq	1152(%rbx), %rdi	# MEM[(struct PPContext *)vc_5(D)].stdQPTable, MEM[(struct PPContext *)vc_5(D)].stdQPTable
	call	av_free	#
	movq	1160(%rbx), %rdi	# MEM[(struct PPContext *)vc_5(D)].nonBQPTable, MEM[(struct PPContext *)vc_5(D)].nonBQPTable
	call	av_free	#
	movq	1168(%rbx), %rdi	# MEM[(struct PPContext *)vc_5(D)].forcedQPTable, MEM[(struct PPContext *)vc_5(D)].forcedQPTable
	call	av_free	#
	movl	$314, %ecx	#, tmp114
	movl	$0, %eax	#, tmp113
	movq	%rbx, %rdi	# vc, vc
	rep stosl
	movq	%rbx, %rdi	# vc,
	call	av_free	#
	popq	%rbx	#
	.cfi_def_cfa_offset 8
	ret
	.cfi_endproc
.LFE183:
	.size	pp_free_context, .-pp_free_context
	.section	.rodata.str1.1
.LC24:
	.string	"using npp filters 0x%X/0x%X\n"
	.text
	.globl	pp_postprocess
	.type	pp_postprocess, @function
pp_postprocess:
.LFB184:
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
	subq	$56, %rsp	#,
	.cfi_def_cfa_offset 112
	movq	%rdi, 16(%rsp)	# src, %sfp
	movq	%rsi, %r13	# srcStride, srcStride
	movq	%rdx, 24(%rsp)	# dst, %sfp
	movq	%rcx, %r14	# dstStride, dstStride
	movl	%r8d, 36(%rsp)	# width, %sfp
	movl	%r9d, 32(%rsp)	# height, %sfp
	movq	112(%rsp), %rbp	# QP_store, QP_store
	movq	128(%rsp), %r12	# vm, vm
	movq	136(%rsp), %rbx	# vc, vc
	leal	15(%r8), %r15d	#, D.16123
	sarl	$4, %r15d	#, mbWidth
	movl	%r9d, %eax	# height, height
	addl	$15, %eax	#, D.16123
	sarl	$4, %eax	#, mbHeight
	movl	%eax, 40(%rsp)	# mbHeight, %sfp
	movl	(%rcx), %edx	# *dstStride_40(D), tmp288
	sarl	$31, %edx	#, tmp288
	movl	%edx, %eax	# tmp288, tmp290
	xorl	(%rcx), %eax	# *dstStride_40(D), tmp290
	subl	%edx, %eax	# tmp288, D.16123
	movl	%eax, %edx	# D.16123, D.16123
	movl	(%rsi), %ecx	# *srcStride_43(D), tmp292
	sarl	$31, %ecx	#, tmp292
	movl	%ecx, %eax	# tmp292, tmp294
	xorl	(%rsi), %eax	# *srcStride_43(D), tmp294
	subl	%ecx, %eax	# tmp292, D.16123
	cmpl	%eax, %edx	# D.16123, D.16123
	cmovge	%edx, %eax	# D.16123,, minStride
	movl	120(%rsp), %edx	# QPStride, tmp296
	sarl	$31, %edx	#, tmp296
	movl	%edx, %ecx	# tmp296, QPStride
	xorl	120(%rsp), %ecx	# QPStride, QPStride
	subl	%edx, %ecx	# tmp296, QPStride
	movl	%ecx, %edi	# QPStride, QPStride
	movl	%ecx, 12(%rsp)	# QPStride, %sfp
	movl	1196(%rbx), %ecx	# MEM[(struct PPContext *)vc_37(D)].stride, D.16123
	cmpl	%ecx, %eax	# D.16123, minStride
	jg	.L1120	#,
	cmpl	1192(%rbx), %edi	# MEM[(struct PPContext *)vc_37(D)].qpStride, QPStride
	jle	.L1121	#,
.L1120:
	movl	1192(%rbx), %r8d	# MEM[(struct PPContext *)vc_37(D)].qpStride, MEM[(struct PPContext *)vc_37(D)].qpStride
	movl	12(%rsp), %edi	# %sfp, QPStride
	cmpl	%r8d, %edi	# MEM[(struct PPContext *)vc_37(D)].qpStride, QPStride
	cmovge	%edi, %r8d	# QPStride,, D.16123
	cmpl	%ecx, %eax	# D.16123, minStride
	cmovge	%eax, %ecx	# minStride,, D.16123
	movl	32(%rsp), %edx	# %sfp,
	movl	36(%rsp), %esi	# %sfp,
	movq	%rbx, %rdi	# vc,
	call	reallocBuffers	#
.L1121:
	testq	%rbp, %rbp	# QP_store
	je	.L1122	#,
	testl	$2097152, (%r12)	#, MEM[(struct PPMode *)vm_35(D)].lumMode
	jne	.L1123	#,
	jmp	.L1124	#
.L1122:
	movq	1168(%rbx), %rbp	# MEM[(struct PPContext *)vc_37(D)].forcedQPTable, QP_store
	testl	$2097152, (%r12)	#, MEM[(struct PPMode *)vm_35(D)].lumMode
	jne	.L1125	#,
	movl	$0, %eax	#, ivtmp.2246
	testl	%r15d, %r15d	# mbWidth
	jg	.L1175	#,
	movl	$0, 12(%rsp)	#, %sfp
	movl	$0, 120(%rsp)	#, QPStride
	jmp	.L1124	#
.L1125:
	testl	%r15d, %r15d	# mbWidth
	jle	.L1153	#,
	movl	$0, %eax	#, ivtmp.2243
.L1127:
	movq	1168(%rbx), %rdx	# MEM[(struct PPContext *)vc_37(D)].forcedQPTable, MEM[(struct PPContext *)vc_37(D)].forcedQPTable
	movl	44(%r12), %ecx	# MEM[(struct PPMode *)vm_35(D)].forcedQuant, MEM[(struct PPMode *)vm_35(D)].forcedQuant
	movb	%cl, (%rdx,%rax)	# MEM[(struct PPMode *)vm_35(D)].forcedQuant, *_63
	addq	$1, %rax	#, ivtmp.2243
	cmpl	%eax, %r15d	# ivtmp.2243, mbWidth
	jg	.L1127	#,
	movl	$0, 12(%rsp)	#, %sfp
	movl	$0, 120(%rsp)	#, QPStride
	jmp	.L1124	#
.L1175:
	movq	1168(%rbx), %rdx	# MEM[(struct PPContext *)vc_37(D)].forcedQPTable, MEM[(struct PPContext *)vc_37(D)].forcedQPTable
	movb	$1, (%rdx,%rax)	#, *_70
	addq	$1, %rax	#, ivtmp.2246
	cmpl	%eax, %r15d	# ivtmp.2246, mbWidth
	jg	.L1175	#,
	movl	$0, 12(%rsp)	#, %sfp
	movl	$0, 120(%rsp)	#, QPStride
	jmp	.L1124	#
.L1153:
	movl	$0, 12(%rsp)	#, %sfp
	movl	$0, 120(%rsp)	#, QPStride
.L1124:
	testb	$16, 144(%rsp)	#, pict_type
	je	.L1128	#,
	movl	40(%rsp), %edi	# %sfp, D.16123
	imull	12(%rsp), %edi	# %sfp, D.16123
	cmpl	%r15d, %edi	# mbWidth, D.16123
	cmovl	%r15d, %edi	# D.16123,, mbWidth, count
	movl	%edi, %ecx	# count, i
	sarl	$2, %ecx	#, i
	testl	%ecx, %ecx	# i
	jle	.L1154	#,
	leal	-1(%rcx), %eax	#, D.16121
	leaq	4(,%rax,4), %r8	#, D.16121
	movl	$0, %edx	#, ivtmp.2240
.L1130:
	movq	1152(%rbx), %rsi	# MEM[(struct PPContext *)vc_37(D)].stdQPTable, MEM[(struct PPContext *)vc_37(D)].stdQPTable
	movl	0(%rbp,%rdx), %eax	# MEM[base: QP_store_1, index: ivtmp.2240_122, offset: 0B], D.16128
	shrl	%eax	# D.16128
	andl	$2139062143, %eax	#, tmp315
	movl	%eax, (%rsi,%rdx)	# tmp315, *_81
	addq	$4, %rdx	#, ivtmp.2240
	cmpq	%r8, %rdx	# D.16121, ivtmp.2240
	jne	.L1130	#,
	jmp	.L1129	#
.L1154:
	movl	$0, %ecx	#, i
.L1129:
	leal	0(,%rcx,4), %eax	#, i
	cmpl	%eax, %edi	# i, count
	jle	.L1131	#,
.L1174:
	movslq	%eax, %rcx	# i, D.16125
	movq	1152(%rbx), %rsi	# MEM[(struct PPContext *)vc_37(D)].stdQPTable, MEM[(struct PPContext *)vc_37(D)].stdQPTable
	movzbl	0(%rbp,%rcx), %edx	# *_92, D.16126
	sarb	%dl	# D.16126
	movb	%dl, (%rsi,%rcx)	# D.16126, *_91
	addl	$1, %eax	#, i
	cmpl	%edi, %eax	# count, i
	jne	.L1174	#,
.L1131:
	movq	1152(%rbx), %rbp	# MEM[(struct PPContext *)vc_37(D)].stdQPTable, QP_store
	movl	12(%rsp), %eax	# %sfp, QPStride
	movl	%eax, 120(%rsp)	# QPStride, QPStride
.L1128:
	movl	144(%rsp), %eax	# pict_type, D.16123
	andl	$7, %eax	#, D.16123
	cmpl	$3, %eax	#, D.16123
	je	.L1133	#,
	cmpl	$0, 120(%rsp)	#, QPStride
	jns	.L1134	#,
	movl	40(%rsp), %r15d	# %sfp, mbHeight
	testl	%r15d, %r15d	# mbHeight
	jle	.L1133	#,
	movl	12(%rsp), %eax	# %sfp, QPStride
	movl	%eax, %r10d	# QPStride, D.16122
	movl	120(%rsp), %r11d	# QPStride, D.16122
	subl	%eax, %r11d	# QPStride, D.16122
	movl	$0, %esi	#, ivtmp.2231
	movl	$0, %r8d	#, ivtmp.2230
	movl	$0, %r9d	#, i
	movq	%r13, 40(%rsp)	# srcStride, %sfp
	movl	%eax, %r13d	# QPStride, QPStride
	jmp	.L1135	#
.L1134:
	movl	40(%rsp), %eax	# %sfp, D.16123
	imull	120(%rsp), %eax	# QPStride, D.16123
	cmpl	%r15d, %eax	# mbWidth, D.16123
	cmovge	%eax, %r15d	# D.16123,, count
	movl	%r15d, %edx	# count, i
	sarl	$2, %edx	#, i
	testl	%edx, %edx	# i
	jle	.L1155	#,
	leal	-1(%rdx), %eax	#, D.16121
	leaq	4(,%rax,4), %rdi	#, D.16121
	movl	$0, %eax	#, ivtmp.2220
.L1137:
	movq	1160(%rbx), %rsi	# MEM[(struct PPContext *)vc_37(D)].nonBQPTable, MEM[(struct PPContext *)vc_37(D)].nonBQPTable
	movl	0(%rbp,%rax), %ecx	# MEM[base: QP_store_2, index: ivtmp.2220_98, offset: 0B], D.16128
	andl	$1061109567, %ecx	#, D.16128
	movl	%ecx, (%rsi,%rax)	# D.16128, *_106
	addq	$4, %rax	#, ivtmp.2220
	cmpq	%rdi, %rax	# D.16121, ivtmp.2220
	jne	.L1137	#,
	jmp	.L1136	#
.L1155:
	movl	$0, %edx	#, i
.L1136:
	leal	0(,%rdx,4), %eax	#, i
	cmpl	%eax, %r15d	# i, count
	jle	.L1133	#,
.L1173:
	movslq	%eax, %rcx	# i, D.16125
	movq	1160(%rbx), %rsi	# MEM[(struct PPContext *)vc_37(D)].nonBQPTable, MEM[(struct PPContext *)vc_37(D)].nonBQPTable
	movzbl	0(%rbp,%rcx), %edx	# *_116, D.16126
	andl	$63, %edx	#, D.16126
	movb	%dl, (%rsi,%rcx)	# D.16126, *_115
	addl	$1, %eax	#, i
	cmpl	%r15d, %eax	# count, i
	jne	.L1173	#,
	jmp	.L1133	#
.L1139:
	movslq	%eax, %rcx	# ivtmp.2225, D.16125
	leal	(%rsi,%rax), %edx	#, D.16122
	movslq	%edx, %rdx	# D.16122, D.16125
	movzbl	0(%rbp,%rdx), %edx	# *_129, *_129
	andl	$63, %edx	#, D.16126
	movq	1160(%rbx), %r8	# MEM[(struct PPContext *)vc_37(D)].nonBQPTable, tmp563
	movb	%dl, (%r8,%rcx)	# D.16126, *_125
	addl	$1, %eax	#, ivtmp.2225
	cmpl	%edi, %eax	# D.16122, ivtmp.2225
	jne	.L1139	#,
	movl	12(%rsp), %r8d	# %sfp, ivtmp.2230
.L1140:
	addl	$1, %r9d	#, i
	addl	%r10d, %r8d	# D.16122, ivtmp.2230
	addl	%r11d, %esi	# D.16122, ivtmp.2231
	cmpl	%r15d, %r9d	# mbHeight, i
	je	.L1185	#,
.L1135:
	testl	%r13d, %r13d	# QPStride
	jle	.L1140	#,
	leal	(%r10,%r8), %edi	#, D.16122
	movl	%r8d, %eax	# ivtmp.2230, ivtmp.2225
	movl	%r8d, 12(%rsp)	# ivtmp.2230, %sfp
	jmp	.L1139	#
.L1185:
	movq	40(%rsp), %r13	# %sfp, srcStride
.L1133:
	movl	4(%r12), %r8d	# MEM[(struct PPMode *)vm_35(D)].chromMode,
	movl	(%r12), %ecx	# MEM[(struct PPMode *)vm_35(D)].lumMode,
	movl	$.LC24, %edx	#,
	movl	$48, %esi	#,
	movq	%rbx, %rdi	# vc,
	movl	$0, %eax	#,
	call	av_log	#
	movl	(%r14), %ecx	# *dstStride_40(D), D.16123
	movq	24(%rsp), %rax	# %sfp, dst
	movq	(%rax), %rdx	# *dst_139(D), D.16130
	movl	0(%r13), %esi	# *srcStride_43(D), D.16123
	movq	16(%rsp), %rax	# %sfp, src
	movq	(%rax), %rdi	# *src_142(D), D.16131
	movq	(%r12), %rax	# MEM[(struct PPMode *)vm_35(D)], MEM[(struct PPMode *)vm_35(D)]
	movq	%rax, 1208(%rbx)	# MEM[(struct PPMode *)vm_35(D)], MEM[(struct PPContext *)vc_37(D)].ppMode
	movq	8(%r12), %rax	# MEM[(struct PPMode *)vm_35(D)], MEM[(struct PPMode *)vm_35(D)]
	movq	%rax, 1216(%rbx)	# MEM[(struct PPMode *)vm_35(D)], MEM[(struct PPContext *)vc_37(D)].ppMode
	movq	16(%r12), %rax	# MEM[(struct PPMode *)vm_35(D)], MEM[(struct PPMode *)vm_35(D)]
	movq	%rax, 1224(%rbx)	# MEM[(struct PPMode *)vm_35(D)], MEM[(struct PPContext *)vc_37(D)].ppMode
	movq	24(%r12), %rax	# MEM[(struct PPMode *)vm_35(D)], MEM[(struct PPMode *)vm_35(D)]
	movq	%rax, 1232(%rbx)	# MEM[(struct PPMode *)vm_35(D)], MEM[(struct PPContext *)vc_37(D)].ppMode
	movq	32(%r12), %rax	# MEM[(struct PPMode *)vm_35(D)], MEM[(struct PPMode *)vm_35(D)]
	movq	%rax, 1240(%rbx)	# MEM[(struct PPMode *)vm_35(D)], MEM[(struct PPContext *)vc_37(D)].ppMode
	movq	40(%r12), %rax	# MEM[(struct PPMode *)vm_35(D)], MEM[(struct PPMode *)vm_35(D)]
	movq	%rax, 1248(%rbx)	# MEM[(struct PPMode *)vm_35(D)], MEM[(struct PPContext *)vc_37(D)].ppMode
	movl	$postProcess_C, %eax	#, pp
	testl	$16777216, (%r12)	#, MEM[(struct PPMode *)vm_35(D)].lumMode
	jne	.L1141	#,
	movl	1188(%rbx), %r8d	# MEM[(struct PPContext *)vc_37(D)].cpuCaps, D.16123
	movl	$postProcess_SSE2, %eax	#, pp
	testb	$16, %r8b	#, D.16123
	jne	.L1141	#,
	movl	$postProcess_MMX2, %eax	#, pp
	testb	$2, %r8b	#, D.16123
	jne	.L1141	#,
	movl	$postProcess_3DNow, %eax	#, pp
	testb	$4, %r8b	#, D.16123
	jne	.L1141	#,
	andl	$1, %r8d	#, D.16123
	movl	$postProcess_MMX, %eax	#, tmp449
	movl	$postProcess_C, %r8d	#, tmp448
	cmove	%r8, %rax	# tmp448,, pp
.L1141:
	pushq	%rbx	# vc
	.cfi_def_cfa_offset 120
	pushq	$0	#
	.cfi_def_cfa_offset 128
	movl	136(%rsp), %r11d	# QPStride, tmp566
	pushq	%r11	# tmp566
	.cfi_def_cfa_offset 136
	pushq	%rbp	# QP_store
	.cfi_def_cfa_offset 144
	movl	64(%rsp), %r9d	# %sfp,
	movl	68(%rsp), %r15d	# %sfp, width
	movl	%r15d, %r8d	# width,
	call	*%rax	# pp
	movq	48(%rsp), %rax	# %sfp, src
	movq	8(%rax), %rdi	# MEM[(const uint8_t * *)src_142(D) + 8B], D.16131
	addq	$32, %rsp	#,
	.cfi_def_cfa_offset 112
	testq	%rdi, %rdi	# D.16131
	je	.L1119	#,
	cmpq	$0, 16(%rax)	#, MEM[(const uint8_t * *)src_142(D) + 16B]
	je	.L1119	#,
	movq	24(%rsp), %rcx	# %sfp, dst
	movq	8(%rcx), %rax	# MEM[(uint8_t * *)dst_139(D) + 8B], D.16130
	testq	%rax, %rax	# D.16130
	je	.L1119	#,
	cmpq	$0, 16(%rcx)	#, MEM[(uint8_t * *)dst_139(D) + 16B]
	je	.L1119	#,
	movl	1200(%rbx), %ecx	# MEM[(struct PPContext *)vc_37(D)].hChromaSubSample, MEM[(struct PPContext *)vc_37(D)].hChromaSubSample
	movl	%r15d, %esi	# width, width
	sarl	%cl, %esi	# MEM[(struct PPContext *)vc_37(D)].hChromaSubSample, width
	movl	%esi, 12(%rsp)	# width, %sfp
	movl	1204(%rbx), %ecx	# MEM[(struct PPContext *)vc_37(D)].vChromaSubSample, MEM[(struct PPContext *)vc_37(D)].vChromaSubSample
	movl	32(%rsp), %r15d	# %sfp, height
	sarl	%cl, %r15d	# MEM[(struct PPContext *)vc_37(D)].vChromaSubSample, height
	cmpl	$0, 4(%r12)	#, MEM[(struct PPMode *)vm_35(D)].chromMode
	je	.L1143	#,
	movl	4(%r14), %ecx	# MEM[(const int *)dstStride_40(D) + 4B], D.16123
	movl	4(%r13), %esi	# MEM[(const int *)srcStride_43(D) + 4B], D.16123
	movq	(%r12), %rdx	# MEM[(struct PPMode *)vm_35(D)], MEM[(struct PPMode *)vm_35(D)]
	movq	%rdx, 1208(%rbx)	# MEM[(struct PPMode *)vm_35(D)], MEM[(struct PPContext *)vc_37(D)].ppMode
	movq	8(%r12), %rdx	# MEM[(struct PPMode *)vm_35(D)], MEM[(struct PPMode *)vm_35(D)]
	movq	%rdx, 1216(%rbx)	# MEM[(struct PPMode *)vm_35(D)], MEM[(struct PPContext *)vc_37(D)].ppMode
	movq	16(%r12), %rdx	# MEM[(struct PPMode *)vm_35(D)], MEM[(struct PPMode *)vm_35(D)]
	movq	%rdx, 1224(%rbx)	# MEM[(struct PPMode *)vm_35(D)], MEM[(struct PPContext *)vc_37(D)].ppMode
	movq	24(%r12), %rdx	# MEM[(struct PPMode *)vm_35(D)], MEM[(struct PPMode *)vm_35(D)]
	movq	%rdx, 1232(%rbx)	# MEM[(struct PPMode *)vm_35(D)], MEM[(struct PPContext *)vc_37(D)].ppMode
	movq	32(%r12), %rdx	# MEM[(struct PPMode *)vm_35(D)], MEM[(struct PPMode *)vm_35(D)]
	movq	%rdx, 1240(%rbx)	# MEM[(struct PPMode *)vm_35(D)], MEM[(struct PPContext *)vc_37(D)].ppMode
	movq	40(%r12), %rdx	# MEM[(struct PPMode *)vm_35(D)], MEM[(struct PPMode *)vm_35(D)]
	movq	%rdx, 1248(%rbx)	# MEM[(struct PPMode *)vm_35(D)], MEM[(struct PPContext *)vc_37(D)].ppMode
	movl	$postProcess_C, %r10d	#, pp
	testl	$16777216, (%r12)	#, MEM[(struct PPMode *)vm_35(D)].lumMode
	jne	.L1144	#,
	movl	1188(%rbx), %edx	# MEM[(struct PPContext *)vc_37(D)].cpuCaps, D.16123
	movl	$postProcess_SSE2, %r10d	#, pp
	testb	$16, %dl	#, D.16123
	jne	.L1144	#,
	movl	$postProcess_MMX2, %r10d	#, pp
	testb	$2, %dl	#, D.16123
	jne	.L1144	#,
	movl	$postProcess_3DNow, %r10d	#, pp
	testb	$4, %dl	#, D.16123
	jne	.L1144	#,
	andl	$1, %edx	#, D.16123
	movl	$postProcess_MMX, %edx	#, tmp451
	movl	$postProcess_C, %r10d	#, tmp450
	cmovne	%rdx, %r10	# tmp450,, tmp451, pp
.L1144:
	pushq	%rbx	# vc
	.cfi_def_cfa_offset 120
	pushq	$1	#
	.cfi_def_cfa_offset 128
	movl	136(%rsp), %edx	# QPStride, tmp573
	pushq	%rdx	# tmp573
	.cfi_def_cfa_offset 136
	pushq	%rbp	# QP_store
	.cfi_def_cfa_offset 144
	movl	%r15d, %r9d	# height,
	movl	44(%rsp), %r8d	# %sfp,
	movq	%rax, %rdx	# D.16130,
	call	*%r10	# pp
	movl	8(%r14), %ecx	# MEM[(const int *)dstStride_40(D) + 8B], D.16123
	movq	56(%rsp), %rax	# %sfp, dst
	movq	16(%rax), %rdx	# MEM[(uint8_t * *)dst_139(D) + 16B], D.16130
	movl	8(%r13), %esi	# MEM[(const int *)srcStride_43(D) + 8B], D.16123
	movq	48(%rsp), %rax	# %sfp, src
	movq	16(%rax), %rdi	# MEM[(const uint8_t * *)src_142(D) + 16B], D.16131
	movq	(%r12), %rax	# MEM[(struct PPMode *)vm_35(D)], MEM[(struct PPMode *)vm_35(D)]
	movq	%rax, 1208(%rbx)	# MEM[(struct PPMode *)vm_35(D)], MEM[(struct PPContext *)vc_37(D)].ppMode
	movq	8(%r12), %rax	# MEM[(struct PPMode *)vm_35(D)], MEM[(struct PPMode *)vm_35(D)]
	movq	%rax, 1216(%rbx)	# MEM[(struct PPMode *)vm_35(D)], MEM[(struct PPContext *)vc_37(D)].ppMode
	movq	16(%r12), %rax	# MEM[(struct PPMode *)vm_35(D)], MEM[(struct PPMode *)vm_35(D)]
	movq	%rax, 1224(%rbx)	# MEM[(struct PPMode *)vm_35(D)], MEM[(struct PPContext *)vc_37(D)].ppMode
	movq	24(%r12), %rax	# MEM[(struct PPMode *)vm_35(D)], MEM[(struct PPMode *)vm_35(D)]
	movq	%rax, 1232(%rbx)	# MEM[(struct PPMode *)vm_35(D)], MEM[(struct PPContext *)vc_37(D)].ppMode
	movq	32(%r12), %rax	# MEM[(struct PPMode *)vm_35(D)], MEM[(struct PPMode *)vm_35(D)]
	movq	%rax, 1240(%rbx)	# MEM[(struct PPMode *)vm_35(D)], MEM[(struct PPContext *)vc_37(D)].ppMode
	movq	40(%r12), %rax	# MEM[(struct PPMode *)vm_35(D)], MEM[(struct PPMode *)vm_35(D)]
	movq	%rax, 1248(%rbx)	# MEM[(struct PPMode *)vm_35(D)], MEM[(struct PPContext *)vc_37(D)].ppMode
	movl	(%r12), %r8d	# MEM[(struct PPMode *)vm_35(D)].lumMode, MEM[(struct PPMode *)vm_35(D)].lumMode
	addq	$32, %rsp	#,
	.cfi_def_cfa_offset 112
	movl	$postProcess_C, %eax	#, pp
	testl	$16777216, %r8d	#, MEM[(struct PPMode *)vm_35(D)].lumMode
	jne	.L1145	#,
	movl	1188(%rbx), %r8d	# MEM[(struct PPContext *)vc_37(D)].cpuCaps, D.16123
	movl	$postProcess_SSE2, %eax	#, pp
	testb	$16, %r8b	#, D.16123
	jne	.L1145	#,
	movl	$postProcess_MMX2, %eax	#, pp
	testb	$2, %r8b	#, D.16123
	jne	.L1145	#,
	movl	$postProcess_3DNow, %eax	#, pp
	testb	$4, %r8b	#, D.16123
	jne	.L1145	#,
	andl	$1, %r8d	#, D.16123
	movl	$postProcess_MMX, %eax	#, tmp453
	movl	$postProcess_C, %r8d	#, tmp452
	cmove	%r8, %rax	# tmp452,, pp
.L1145:
	pushq	%rbx	# vc
	.cfi_def_cfa_offset 120
	pushq	$2	#
	.cfi_def_cfa_offset 128
	movl	136(%rsp), %ebx	# QPStride, tmp576
	pushq	%rbx	# tmp576
	.cfi_def_cfa_offset 136
	pushq	%rbp	# QP_store
	.cfi_def_cfa_offset 144
	movl	%r15d, %r9d	# height,
	movl	44(%rsp), %r8d	# %sfp,
	call	*%rax	# pp
	addq	$32, %rsp	#,
	.cfi_def_cfa_offset 112
	jmp	.L1119	#
.L1143:
	movl	4(%r13), %edx	# MEM[(const int *)srcStride_43(D) + 4B], D.16123
	cmpl	4(%r14), %edx	# MEM[(const int *)dstStride_40(D) + 4B], D.16123
	je	.L1146	#,
.L1148:
	movl	$0, %ebx	#, y
	movslq	12(%rsp), %rbp	# %sfp, D.16121
	testl	%r15d, %r15d	# height
	jg	.L1186	#,
	jmp	.L1119	#
.L1146:
	movl	8(%r14), %ebx	# MEM[(const int *)dstStride_40(D) + 8B], tmp578
	cmpl	%ebx, 8(%r13)	# tmp578, MEM[(const int *)srcStride_43(D) + 8B]
	jne	.L1148	#,
	testl	%edx, %edx	# D.16123
	jle	.L1149	#,
	imull	%r15d, %edx	# height, D.16123
	movslq	%edx, %rdx	# D.16123, D.16121
	movq	%rdi, %rsi	# D.16131,
	movq	%rax, %rdi	# D.16130,
	call	memcpy	#
	jmp	.L1150	#
.L1149:
	leal	-1(%r15), %ecx	#, D.16123
	imull	%edx, %ecx	# D.16123, D.16123
	movslq	%ecx, %rcx	# D.16123, D.16125
	addq	%rcx, %rax	# D.16125, D.16130
	movl	%r15d, %esi	# height, D.16123
	negl	%esi	# D.16123
	imull	%esi, %edx	# D.16123, D.16123
	movslq	%edx, %rdx	# D.16123, D.16121
	leaq	(%rdi,%rcx), %rsi	#, D.16131
	movq	%rax, %rdi	# D.16130,
	call	memcpy	#
.L1150:
	movl	8(%r13), %edx	# MEM[(const int *)srcStride_43(D) + 8B], D.16123
	movq	16(%rsp), %rax	# %sfp, src
	movq	16(%rax), %rsi	# MEM[(const uint8_t * *)src_142(D) + 16B], D.16131
	movq	24(%rsp), %rax	# %sfp, dst
	movq	16(%rax), %rdi	# MEM[(uint8_t * *)dst_139(D) + 16B], D.16130
	testl	%edx, %edx	# D.16123
	jle	.L1151	#,
	imull	%r15d, %edx	# height, D.16123
	movslq	%edx, %rdx	# D.16123, D.16121
	call	memcpy	#
	jmp	.L1119	#
.L1151:
	leal	-1(%r15), %eax	#, D.16123
	imull	%edx, %eax	# D.16123, D.16123
	cltq
	addq	%rax, %rdi	# D.16125, D.16130
	negl	%r15d	# D.16123
	imull	%r15d, %edx	# D.16123, D.16123
	movslq	%edx, %rdx	# D.16123, D.16121
	addq	%rax, %rsi	# D.16125, D.16131
	call	memcpy	#
	jmp	.L1119	#
.L1186:
	movl	%r15d, 12(%rsp)	# height, %sfp
	movq	16(%rsp), %r12	# %sfp, src
	movq	24(%rsp), %r15	# %sfp, dst
.L1172:
	movl	%ebx, %edi	# y, D.16123
	imull	4(%r14), %edi	# MEM[(const int *)dstStride_40(D) + 4B], D.16123
	movslq	%edi, %rdi	# D.16123, D.16125
	addq	8(%r15), %rdi	# MEM[(uint8_t * *)dst_139(D) + 8B], D.16130
	movl	%ebx, %esi	# y, D.16123
	imull	4(%r13), %esi	# MEM[(const int *)srcStride_43(D) + 4B], D.16123
	movslq	%esi, %rsi	# D.16123, D.16125
	addq	8(%r12), %rsi	# MEM[(const uint8_t * *)src_142(D) + 8B], D.16131
	movq	%rbp, %rdx	# D.16121,
	call	memcpy	#
	movl	%ebx, %edi	# y, D.16123
	imull	8(%r14), %edi	# MEM[(const int *)dstStride_40(D) + 8B], D.16123
	movslq	%edi, %rdi	# D.16123, D.16125
	addq	16(%r15), %rdi	# MEM[(uint8_t * *)dst_139(D) + 16B], D.16130
	movl	%ebx, %esi	# y, D.16123
	imull	8(%r13), %esi	# MEM[(const int *)srcStride_43(D) + 8B], D.16123
	movslq	%esi, %rsi	# D.16123, D.16125
	addq	16(%r12), %rsi	# MEM[(const uint8_t * *)src_142(D) + 16B], D.16131
	movq	%rbp, %rdx	# D.16121,
	call	memcpy	#
	addl	$1, %ebx	#, y
	cmpl	12(%rsp), %ebx	# %sfp, y
	jne	.L1172	#,
	jmp	.L1119	#
.L1123:
	movq	1168(%rbx), %rbp	# MEM[(struct PPContext *)vc_37(D)].forcedQPTable, QP_store
	jmp	.L1125	#
.L1119:
	addq	$56, %rsp	#,
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
.LFE184:
	.size	pp_postprocess, .-pp_postprocess
	.local	lut.5312
	.comm	lut.5312,2048,64
	.section	.rodata
	.type	optionDelimiters.6665, @object
	.size	optionDelimiters.6665, 3
optionDelimiters.6665:
	.string	":|"
	.type	filterDelimiters.6664, @object
	.size	filterDelimiters.6664, 3
filterDelimiters.6664:
	.string	",/"
	.section	.rodata.str1.1
.LC25:
	.string	"Postproc"
	.section	.rodata
	.align 64
	.type	av_codec_context_class, @object
	.size	av_codec_context_class, 80
av_codec_context_class:
# class_name:
	.quad	.LC25
# item_name:
	.quad	context_to_name
# option:
	.quad	0
	.zero	56
	.globl	pp_help
	.align 64
	.type	pp_help, @object
	.size	pp_help, 2493
pp_help:
	.ascii	"Available postprocessing filters:\nFilters                  "
	.ascii	"      Options\nshort  long name       short   long option   "
	.ascii	"  Description\n*      *               a       autoq         "
	.ascii	"  CPU power dependent enabler\n                       c     "
	.ascii	"  chrom           chrominance filtering enabled\n           "
	.ascii	"            y       nochrom         chrominance filtering di"
	.ascii	"sabled\n                       n       noluma          luma "
	.ascii	"filtering disabled\nhb     hdeblock        (2 threshold)    "
	.ascii	"       horizontal deblocking filter\n       1. difference fa"
	.ascii	"ctor: default=32, higher -> more deblocking\n       2. flatn"
	.ascii	"ess threshold: default=39, lower -> more deblocking\n       "
	.ascii	"                the h & v deblocking filters share these\n  "
	.ascii	"                     so you can't set different thresholds f"
	.ascii	"or h / v\nvb     vdeblock        (2 threshold)           ver"
	.ascii	"tical deblocking filter\nha     hadeblock       (2 threshold"
	.ascii	")           horizontal deblocking filter\nva     vadeblock  "
	.ascii	"     (2 threshold)           vertical deblocking filter\nh1 "
	.ascii	"    x1hdeblock                              experimental h d"
	.ascii	"eblock filter 1\nv1     x1vdeblock                          "
	.ascii	"    experimental v deblock filter 1\ndr     dering          "
	.ascii	"                        deringing filter\nal     autolevels "
	.ascii	"                             automatic brightness / contrast"
	.ascii	"\n                       f        fullyrange     stretch lum"
	.ascii	"inance to (0..255)\nlb     linblenddeint                    "
	.ascii	"       linear blend deinterlacer\nli     linipoldeint       "
	.ascii	"                     linear interpolating deinterlace\nci   "
	.ascii	"  cubicipoldeint                          cubic interpolatin"
	.ascii	"g deinterlacer\nmd     mediandeint                          "
	.ascii	"   median deinterlacer\nfd     ffmpegdeint                  "
	.ascii	"           ffmpeg deinterlacer\nl5     lowpass5             "
	.ascii	"                   FIR lowpass deinterlacer\nde     default "
	.ascii	"                                hb:a,vb:a,dr:a\nfa     fast "
	.ascii	"                                   h1:a,v1:a,dr:a\nac       "
	.ascii	"                                      ha:a:128:7"
	.ascii	",va:a,dr:a\ntn     tmpnoise        (3 threshold)           t"
	.ascii	"emporal noise reducer\n                     1. <= 2. <= 3.  "
	.ascii	"          larger -> stronger filtering\nfq     forceQuant   "
	.ascii	"   <quantizer>             force quantizer\nUsage:\n<filterN"
	.ascii	"a"
	.string	"me>[:<option>[:<option>...]][[,|/][-]<filterName>[:<option>...]]...\nlong form example:\nvdeblock:autoq/hdeblock:autoq/linblenddeint    default,-vdeblock\nshort form example:\nvb:a/hb:a/lb                                   de,-vb\nmore examples:\ntn:64:128:256\n\n"
	.section	.rodata.str1.1
.LC26:
	.string	"hb:a,vb:a,dr:a"
.LC27:
	.string	"de"
.LC28:
	.string	"fast"
.LC29:
	.string	"h1:a,v1:a,dr:a"
.LC30:
	.string	"fa"
.LC31:
	.string	"ac"
.LC32:
	.string	"ha:a:128:7,va:a,dr:a"
	.section	.rodata
	.align 64
	.type	replaceTable, @object
	.size	replaceTable, 88
replaceTable:
	.quad	.LC8
	.quad	.LC26
	.quad	.LC27
	.quad	.LC26
	.quad	.LC28
	.quad	.LC29
	.quad	.LC30
	.quad	.LC29
	.quad	.LC31
	.quad	.LC32
	.quad	0
	.section	.rodata.str1.1
.LC33:
	.string	"hdeblock"
.LC34:
	.string	"vb"
.LC35:
	.string	"vdeblock"
.LC36:
	.string	"h1"
.LC37:
	.string	"x1hdeblock"
.LC38:
	.string	"v1"
.LC39:
	.string	"x1vdeblock"
.LC40:
	.string	"ha"
.LC41:
	.string	"ahdeblock"
.LC42:
	.string	"va"
.LC43:
	.string	"avdeblock"
.LC44:
	.string	"dr"
.LC45:
	.string	"dering"
.LC46:
	.string	"al"
.LC47:
	.string	"autolevels"
.LC48:
	.string	"lb"
.LC49:
	.string	"linblenddeint"
.LC50:
	.string	"li"
.LC51:
	.string	"linipoldeint"
.LC52:
	.string	"ci"
.LC53:
	.string	"cubicipoldeint"
.LC54:
	.string	"md"
.LC55:
	.string	"mediandeint"
.LC56:
	.string	"fd"
.LC57:
	.string	"ffmpegdeint"
.LC58:
	.string	"l5"
.LC59:
	.string	"lowpass5"
.LC60:
	.string	"tn"
.LC61:
	.string	"tmpnoise"
.LC62:
	.string	"fq"
.LC63:
	.string	"forcequant"
.LC64:
	.string	"be"
.LC65:
	.string	"bitexact"
.LC66:
	.string	"vi"
.LC67:
	.string	"visualize"
	.section	.rodata
	.align 64
	.type	filters, @object
	.size	filters, 608
filters:
# shortName:
	.quad	.LC9
# longName:
	.quad	.LC33
# chromDefault:
	.long	1
# minLumQuality:
	.long	1
# minChromQuality:
	.long	3
# mask:
	.long	2
# shortName:
	.quad	.LC34
# longName:
	.quad	.LC35
# chromDefault:
	.long	1
# minLumQuality:
	.long	2
# minChromQuality:
	.long	4
# mask:
	.long	1
# shortName:
	.quad	.LC36
# longName:
	.quad	.LC37
# chromDefault:
	.long	1
# minLumQuality:
	.long	1
# minChromQuality:
	.long	3
# mask:
	.long	8192
# shortName:
	.quad	.LC38
# longName:
	.quad	.LC39
# chromDefault:
	.long	1
# minLumQuality:
	.long	2
# minChromQuality:
	.long	4
# mask:
	.long	512
# shortName:
	.quad	.LC40
# longName:
	.quad	.LC41
# chromDefault:
	.long	1
# minLumQuality:
	.long	1
# minChromQuality:
	.long	3
# mask:
	.long	16384
# shortName:
	.quad	.LC42
# longName:
	.quad	.LC43
# chromDefault:
	.long	1
# minLumQuality:
	.long	2
# minChromQuality:
	.long	4
# mask:
	.long	1024
# shortName:
	.quad	.LC44
# longName:
	.quad	.LC45
# chromDefault:
	.long	1
# minLumQuality:
	.long	5
# minChromQuality:
	.long	6
# mask:
	.long	4
# shortName:
	.quad	.LC46
# longName:
	.quad	.LC47
# chromDefault:
	.long	0
# minLumQuality:
	.long	1
# minChromQuality:
	.long	2
# mask:
	.long	8
# shortName:
	.quad	.LC48
# longName:
	.quad	.LC49
# chromDefault:
	.long	1
# minLumQuality:
	.long	1
# minChromQuality:
	.long	4
# mask:
	.long	131072
# shortName:
	.quad	.LC50
# longName:
	.quad	.LC51
# chromDefault:
	.long	1
# minLumQuality:
	.long	1
# minChromQuality:
	.long	4
# mask:
	.long	65536
# shortName:
	.quad	.LC52
# longName:
	.quad	.LC53
# chromDefault:
	.long	1
# minLumQuality:
	.long	1
# minChromQuality:
	.long	4
# mask:
	.long	262144
# shortName:
	.quad	.LC54
# longName:
	.quad	.LC55
# chromDefault:
	.long	1
# minLumQuality:
	.long	1
# minChromQuality:
	.long	4
# mask:
	.long	524288
# shortName:
	.quad	.LC56
# longName:
	.quad	.LC57
# chromDefault:
	.long	1
# minLumQuality:
	.long	1
# minChromQuality:
	.long	4
# mask:
	.long	4194304
# shortName:
	.quad	.LC58
# longName:
	.quad	.LC59
# chromDefault:
	.long	1
# minLumQuality:
	.long	1
# minChromQuality:
	.long	4
# mask:
	.long	8388608
# shortName:
	.quad	.LC60
# longName:
	.quad	.LC61
# chromDefault:
	.long	1
# minLumQuality:
	.long	7
# minChromQuality:
	.long	8
# mask:
	.long	1048576
# shortName:
	.quad	.LC62
# longName:
	.quad	.LC63
# chromDefault:
	.long	1
# minLumQuality:
	.long	0
# minChromQuality:
	.long	0
# mask:
	.long	2097152
# shortName:
	.quad	.LC64
# longName:
	.quad	.LC65
# chromDefault:
	.long	1
# minLumQuality:
	.long	0
# minChromQuality:
	.long	0
# mask:
	.long	16777216
# shortName:
	.quad	.LC66
# longName:
	.quad	.LC67
# chromDefault:
	.long	1
# minLumQuality:
	.long	0
# minChromQuality:
	.long	0
# mask:
	.long	33554432
# shortName:
	.quad	0
# longName:
	.quad	0
# chromDefault:
	.long	0
# minLumQuality:
	.long	0
# minChromQuality:
	.long	0
# mask:
	.long	0
	.align 8
	.type	deringThreshold, @object
	.size	deringThreshold, 4
deringThreshold:
	.long	20
	.align 8
	.type	b80, @object
	.size	b80, 8
b80:
	.quad	-9187201950435737472
	.align 8
	.type	b08, @object
	.size	b08, 8
b08:
	.quad	578721382704613384
	.align 8
	.type	b02, @object
	.size	b02, 8
b02:
	.quad	144680345676153346
	.align 8
	.type	b01, @object
	.size	b01, 8
b01:
	.quad	72340172838076673
	.align 8
	.type	b00, @object
	.size	b00, 8
b00:
	.zero	8
	.align 8
	.type	w20, @object
	.size	w20, 8
w20:
	.quad	9007336695791648
	.align 8
	.type	w04, @object
	.size	w04, 8
w04:
	.quad	1125917086973956
	.align 8
	.type	w05, @object
	.size	w05, 8
w05:
	.quad	1407396358717445
	.globl	postproc_ffversion
	.align 16
	.type	postproc_ffversion, @object
	.size	postproc_ffversion, 21
postproc_ffversion:
	.string	"FFmpeg version 2.5.2"
	.section	.rodata.cst4,"aM",@progbits,4
	.align 4
.LC2:
	.long	1593835520
	.section	.rodata.cst8,"aM",@progbits,8
	.align 8
.LC3:
	.long	0
	.long	1083179008
	.align 8
.LC4:
	.long	0
	.long	1071644672
	.align 8
.LC5:
	.long	0
	.long	1081081856
	.ident	"GCC: (GNU) 4.9.2 20141224 (prerelease)"
	.section	.note.GNU-stack,"",@progbits
