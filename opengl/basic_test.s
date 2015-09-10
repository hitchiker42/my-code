	.file	"basic_test.c"
# GNU C11 (GCC) version 5.2.0 (x86_64-unknown-linux-gnu)
#	compiled by GNU C version 5.2.0, GMP version 6.0.0, MPFR version 3.1.3-p4, MPC version 1.0.3
# GGC heuristics: --param ggc-min-expand=100 --param ggc-min-heapsize=131072
# options passed:  basic_test.c -mtune=generic -march=x86-64 -O2
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
	.globl	quit_on_esc
	.type	quit_on_esc, @function
quit_on_esc:
.LFB55:
	.cfi_startproc
	cmpl	$256, %esi	#, key
	jne	.L1	#,
	testl	%ecx, %ecx	# action
	jne	.L1	#,
	xorl	%esi, %esi	#
	jmp	glfwSetWindowShouldClose	#
	.p2align 4,,10
	.p2align 3
.L1:
	rep ret
	.cfi_endproc
.LFE55:
	.size	quit_on_esc, .-quit_on_esc
	.section	.text.unlikely
.LCOLDE0:
	.text
.LHOTE0:
	.section	.text.unlikely
.LCOLDB1:
	.text
.LHOTB1:
	.p2align 4,,15
	.globl	draw_triangles
	.type	draw_triangles, @function
draw_triangles:
.LFB56:
	.cfi_startproc
	movslq	%esi, %rax	# num_triangles,
	pushq	%rbp	#
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	xorl	%edi, %edi	# ivtmp.29
	movq	%rax, %rcx	#,
	addq	$30, %rax	#, tmp115
	shrq	$4, %rax	#, tmp117
	movq	%rsp, %rbp	#,
	.cfi_def_cfa_register 6
	salq	$4, %rax	#, tmp119
	subq	%rax, %rsp	# tmp119,
	leaq	15(%rsp), %rsi	#, tmp121
	subq	%rax, %rsp	# tmp119,
	xorl	%eax, %eax	# ivtmp.27
	leaq	15(%rsp), %rdx	#, tmp132
	andq	$-16, %rsi	#, tmp123
	andq	$-16, %rdx	#, tmp134
	testl	%ecx, %ecx	# num_triangles
	jle	.L13	#,
	.p2align 4,,10
	.p2align 3
.L14:
	movl	%edi, (%rsi,%rax,4)	# ivtmp.29, MEM[base: first_7, index: ivtmp.27_1, step: 4, offset: 0B]
	movl	$3, (%rdx,%rax,4)	#, MEM[base: count_9, index: ivtmp.27_1, step: 4, offset: 0B]
	addq	$1, %rax	#, ivtmp.27
	addl	$3, %edi	#, ivtmp.29
	cmpl	%eax, %ecx	# ivtmp.27, num_triangles
	jg	.L14	#,
.L13:
	movl	$4, %edi	#,
	call	*__glewMultiDrawArrays(%rip)	# __glewMultiDrawArrays
	leave
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE56:
	.size	draw_triangles, .-draw_triangles
	.section	.text.unlikely
.LCOLDE1:
	.text
.LHOTE1:
	.section	.rodata.str1.1,"aMS",@progbits,1
.LC2:
	.string	"ucolor"
.LC3:
	.string	"shift"
	.section	.text.unlikely
.LCOLDB6:
	.text
.LHOTB6:
	.p2align 4,,15
	.globl	main_loop
	.type	main_loop, @function
main_loop:
.LFB57:
	.cfi_startproc
	pushq	%r15	#
	.cfi_def_cfa_offset 16
	.cfi_offset 15, -16
	pushq	%r14	#
	.cfi_def_cfa_offset 24
	.cfi_offset 14, -24
	movl	%esi, %r15d	# program, program
	pushq	%r13	#
	.cfi_def_cfa_offset 32
	.cfi_offset 13, -32
	pushq	%r12	#
	.cfi_def_cfa_offset 40
	.cfi_offset 12, -40
	movl	%edx, %r12d	# program2, program2
	pushq	%rbp	#
	.cfi_def_cfa_offset 48
	.cfi_offset 6, -48
	pushq	%rbx	#
	.cfi_def_cfa_offset 56
	.cfi_offset 3, -56
	movq	%rdi, %rbp	# window, window
	movl	$1, %edi	#,
	xorl	%ebx, %ebx	# i
	subq	$56, %rsp	#,
	.cfi_def_cfa_offset 112
	leaq	40(%rsp), %rsi	#, tmp177
	call	*__glewGenBuffers(%rip)	# __glewGenBuffers
	leaq	16(%rsp), %rsi	#, tmp178
	movl	$2, %edi	#,
	call	*__glewGenVertexArrays(%rip)	# __glewGenVertexArrays
	movl	16(%rsp), %edi	# VAO,
	call	*__glewBindVertexArray(%rip)	# __glewBindVertexArray
	movl	$35045, %ecx	#,
	movl	$7, %edx	#,
	movl	$indices, %esi	#,
	movl	$34963, %edi	#,
	call	make_data_buffer	#
	movl	$35044, %ecx	#,
	movl	$168, %edx	#,
	movl	$data, %esi	#,
	movl	$34962, %edi	#,
	movl	%eax, 40(%rsp)	# D.21604, buffers
	call	make_data_buffer	#
	subq	$8, %rsp	#,
	.cfi_def_cfa_offset 120
	movl	%eax, 40(%rsp)	# D.21604, buffers
	movl	%eax, %edi	# D.21604,
	pushq	$0	#
	.cfi_def_cfa_offset 128
	movl	$28, %r9d	#,
	xorl	%r8d, %r8d	#
	movl	$5126, %ecx	#,
	movl	$3, %edx	#,
	xorl	%esi, %esi	#
	call	bind_vertex_attrib	#
	movl	48(%rsp), %edi	# buffers,
	movl	$28, %r9d	#,
	xorl	%r8d, %r8d	#
	movl	$5126, %ecx	#,
	movl	$4, %edx	#,
	movl	$1, %esi	#,
	movq	$12, (%rsp)	#,
	call	bind_vertex_attrib	#
	movl	36(%rsp), %edi	# VAO,
	call	*__glewBindVertexArray(%rip)	# __glewBindVertexArray
	movl	$35044, %ecx	#,
	movl	$48, %edx	#,
	movl	$square, %esi	#,
	movl	$34962, %edi	#,
	call	make_data_buffer	#
	movl	$3, %edx	#,
	xorl	%r9d, %r9d	#
	xorl	%r8d, %r8d	#
	movl	$5126, %ecx	#,
	movl	%eax, %edi	# D.21604,
	xorl	%esi, %esi	#
	movl	%eax, 52(%rsp)	# D.21604, buffers
	movq	$0, (%rsp)	#,
	call	bind_vertex_attrib	#
	movl	$.LC2, %esi	#,
	movl	%r12d, %edi	# program2,
	call	*__glewGetUniformLocation(%rip)	# __glewGetUniformLocation
	movl	$.LC3, %esi	#,
	movl	%eax, %r14d	#, color_loc
	movl	%r12d, %edi	# program2,
	call	*__glewGetUniformLocation(%rip)	# __glewGetUniformLocation
	movl	%eax, %r13d	#, shift_loc
	popq	%rax	#
	.cfi_def_cfa_offset 120
	popq	%rdx	#
	.cfi_def_cfa_offset 112
	jmp	.L20	#
	.p2align 4,,10
	.p2align 3
.L21:
	pxor	%xmm0, %xmm0	# D.21607
	addl	$1, %ebx	#, i
	movzbl	%bl, %eax	# i, D.21606
	leaq	8(%rsp), %rsi	#, tmp179
	leaq	12(%rsp), %rdi	#, tmp180
	cvtsi2ss	%eax, %xmm0	# D.21606, D.21607
	divss	.LC4(%rip), %xmm0	#, D.21607
	movss	%xmm0, data+12(%rip)	# D.21607, data[0].D.21400.D.21399.r
	movss	%xmm0, data+44(%rip)	# D.21607, data[1].D.21400.D.21399.g
	movss	%xmm0, data+76(%rip)	# D.21607, data[2].D.21400.D.21399.b
	movss	%xmm0, data+100(%rip)	# D.21607, data[3].D.21400.D.21399.g
	movss	%xmm0, data+132(%rip)	# D.21607, data[4].D.21400.D.21399.b
	movss	%xmm0, data+152(%rip)	# D.21607, data[5].D.21400.D.21399.r
	cvtss2sd	%xmm0, %xmm0	# D.21607, D.21608
	mulsd	.LC5(%rip), %xmm0	#, D.21608
	cvtsd2ss	%xmm0, %xmm0	# D.21608, theta
	call	sincosf	#
	movss	8(%rsp), %xmm0	#, D.21609
	movl	$16640, %edi	#,
	movss	%xmm0, shift_matrix(%rip)	# D.21609, shift_matrix
	movss	12(%rsp), %xmm0	#, D.21609
	movss	%xmm0, shift_matrix+4(%rip)	# D.21609, shift_matrix
	call	glClear	#
	movl	16(%rsp), %edi	# VAO,
	call	*__glewBindVertexArray(%rip)	# __glewBindVertexArray
	movl	%r15d, %edi	# program,
	call	*__glewUseProgram(%rip)	# __glewUseProgram
	movl	32(%rsp), %esi	# buffers,
	movl	$34962, %edi	#,
	call	*__glewBindBuffer(%rip)	# __glewBindBuffer
	movl	$data, %ecx	#,
	movl	$168, %edx	#,
	xorl	%esi, %esi	#
	movl	$34962, %edi	#,
	call	*__glewBufferSubData(%rip)	# __glewBufferSubData
	xorl	%ecx, %ecx	#
	movl	$5121, %edx	#,
	movl	$6, %esi	#,
	movl	$4, %edi	#,
	call	glDrawElements	#
	movl	20(%rsp), %edi	# VAO,
	call	*__glewBindVertexArray(%rip)	# __glewBindVertexArray
	movl	%r12d, %edi	# program2,
	call	*__glewUseProgram(%rip)	# __glewUseProgram
	movl	36(%rsp), %esi	# buffers,
	movl	$34962, %edi	#,
	call	*__glewBindBuffer(%rip)	# __glewBindBuffer
	movl	$sq_color, %edx	#,
	movl	$1, %esi	#,
	movl	%r14d, %edi	# color_loc,
	call	*__glewUniform4fv(%rip)	# __glewUniform4fv
	movl	$shift_matrix, %edx	#,
	movl	$1, %esi	#,
	movl	%r13d, %edi	# shift_loc,
	call	*__glewUniform2fv(%rip)	# __glewUniform2fv
	movl	$4, %edx	#,
	xorl	%esi, %esi	#
	movl	$5, %edi	#,
	call	glDrawArrays	#
	movq	%rbp, %rdi	# window,
	call	glfwSwapBuffers	#
	call	glfwPollEvents	#
.L20:
	movq	%rbp, %rdi	# window,
	call	glfwWindowShouldClose	#
	testl	%eax, %eax	# D.21606
	je	.L21	#,
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
.LFE57:
	.size	main_loop, .-main_loop
	.section	.text.unlikely
.LCOLDE6:
	.text
.LHOTE6:
	.section	.rodata.str1.1
.LC7:
	.string	"basic_test"
	.section	.text.unlikely
.LCOLDB8:
	.section	.text.startup,"ax",@progbits
.LHOTB8:
	.p2align 4,,15
	.globl	main
	.type	main, @function
main:
.LFB58:
	.cfi_startproc
	pushq	%r12	#
	.cfi_def_cfa_offset 16
	.cfi_offset 12, -16
	pushq	%rbp	#
	.cfi_def_cfa_offset 24
	.cfi_offset 6, -24
	movl	$.LC7, %edx	#,
	pushq	%rbx	#
	.cfi_def_cfa_offset 32
	.cfi_offset 3, -32
	movl	$800, %esi	#,
	movl	$800, %edi	#,
	call	init_gl_context	#
	movq	fragment_shader_source(%rip), %rsi	# fragment_shader_source,
	movq	vertex_shader_source(%rip), %rdi	# vertex_shader_source,
	xorl	%edx, %edx	#
	movq	%rax, %rbx	#, win
	call	create_shader_program	#
	movq	fragment_shader_source(%rip), %rsi	# fragment_shader_source,
	movq	square_vertex_shader_source(%rip), %rdi	# square_vertex_shader_source,
	xorl	%edx, %edx	#
	movl	%eax, %ebp	#, program
	call	create_shader_program	#
	movq	%rbx, %rdi	# win,
	movl	%eax, %r12d	#, square_program
	movl	$quit_on_esc, %esi	#,
	call	glfwSetKeyCallback	#
	movl	%r12d, %edx	# square_program,
	movl	%ebp, %esi	# program,
	movq	%rbx, %rdi	# win,
	call	main_loop	#
	popq	%rbx	#
	.cfi_def_cfa_offset 24
	xorl	%eax, %eax	#
	popq	%rbp	#
	.cfi_def_cfa_offset 16
	popq	%r12	#
	.cfi_def_cfa_offset 8
	ret
	.cfi_endproc
.LFE58:
	.size	main, .-main
	.section	.text.unlikely
.LCOLDE8:
	.section	.text.startup
.LHOTE8:
	.data
	.type	indices, @object
	.size	indices, 7
indices:
	.byte	0
	.byte	1
	.byte	2
	.byte	3
	.byte	4
	.byte	5
	.byte	6
	.local	shift_matrix
	.comm	shift_matrix,8,8
	.align 16
	.type	sq_color, @object
	.size	sq_color, 16
sq_color:
	.long	0
	.long	1056964608
	.long	1056964608
	.long	1056964608
	.align 32
	.type	square, @object
	.size	square, 48
square:
# x:
	.long	3204448256
# y:
	.long	3204448256
	.zero	4
# x:
	.long	1056964608
# y:
	.long	3204448256
	.zero	4
# x:
	.long	3204448256
# y:
	.long	1056964608
	.zero	4
# x:
	.long	1056964608
# y:
	.long	1056964608
	.zero	4
	.align 32
	.type	data, @object
	.size	data, 168
data:
# <anonymous>:
# <anonymous>:
# x:
	.long	3212836864
# y:
	.long	3212836864
	.zero	4
# <anonymous>:
# <anonymous>:
# r:
	.long	1065353216
# g:
	.long	1065353216
# b:
	.long	0
# a:
	.long	1056964608
# <anonymous>:
# <anonymous>:
# x:
	.long	1065353216
# y:
	.long	3212836864
	.zero	4
# <anonymous>:
# <anonymous>:
# r:
	.long	0
# g:
	.long	1065353216
# b:
	.long	1065353216
# a:
	.long	1056964608
# <anonymous>:
# <anonymous>:
# x:
	.long	0
# y:
	.long	1065353216
	.zero	4
# <anonymous>:
# <anonymous>:
# r:
	.long	1065353216
# g:
	.long	0
# b:
	.long	1065353216
# a:
	.long	1056964608
# <anonymous>:
# <anonymous>:
# x:
	.long	3212836864
# y:
	.long	1065353216
	.zero	4
# <anonymous>:
# <anonymous>:
# r:
	.long	0
# g:
	.long	1065353216
# b:
	.long	1065353216
# a:
	.long	1056964608
# <anonymous>:
# <anonymous>:
# x:
	.long	1065353216
# y:
	.long	1065353216
	.zero	4
# <anonymous>:
# <anonymous>:
# r:
	.long	1065353216
# g:
	.long	0
# b:
	.long	1065353216
# a:
	.long	1056964608
# <anonymous>:
# <anonymous>:
# x:
	.long	0
# y:
	.long	3212836864
	.zero	4
# <anonymous>:
# <anonymous>:
# r:
	.long	1065353216
# g:
	.long	1065353216
# b:
	.long	0
# a:
	.long	1056964608
	.globl	fragment_shader_source
	.section	.rodata.str1.8,"aMS",@progbits,1
	.align 8
.LC9:
	.string	"#version 330 core\nin vec4 v_color;\nout vec4 color;\nvoid main(){\n  color = v_color;\n}\n"
	.data
	.align 8
	.type	fragment_shader_source, @object
	.size	fragment_shader_source, 8
fragment_shader_source:
	.quad	.LC9
	.globl	square_vertex_shader_source
	.section	.rodata.str1.8
	.align 8
.LC10:
	.string	"#version 330 core\nlayout(location = 0) in vec3 position;\nuniform vec4 ucolor;\nuniform vec2 shift;\nout vec4 v_color;\nvoid main(){\n  gl_Position.xyz = position;\n  gl_Position.w = 1.0f;\n  gl_Position.xy += shift;\n  v_color = ucolor;\n}\n"
	.data
	.align 8
	.type	square_vertex_shader_source, @object
	.size	square_vertex_shader_source, 8
square_vertex_shader_source:
	.quad	.LC10
	.globl	vertex_shader_source
	.section	.rodata.str1.8
	.align 8
.LC11:
	.string	"#version 330 core\nlayout(location = 0) in vec3 position;\nlayout(location = 1) in vec4 color;\nout vec4 v_color;\nvoid main(){\n  gl_Position.xyz = position;\n  gl_Position.w = 1.0f;\n  v_color = color;\n}\n"
	.data
	.align 8
	.type	vertex_shader_source, @object
	.size	vertex_shader_source, 8
vertex_shader_source:
	.quad	.LC11
	.section	.rodata.cst4,"aM",@progbits,4
	.align 4
.LC4:
	.long	1132396544
	.section	.rodata.cst8,"aM",@progbits,8
	.align 8
.LC5:
	.long	1413754136
	.long	1075388923
	.ident	"GCC: (GNU) 5.2.0"
	.section	.note.GNU-stack,"",@progbits
