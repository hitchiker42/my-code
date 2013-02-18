	.file	"rand.c"
	.text
	.globl	main
	.align	16, 0x90
	.type	main,@function
main:                                   # @main
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbp
.Ltmp2:
	.cfi_def_cfa_offset 16
.Ltmp3:
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
.Ltmp4:
	.cfi_def_cfa_register %rbp
	subq	$80, %rsp
	movl	$0, -4(%rbp)
	movl	$0, -8(%rbp)
	movl	$0, -12(%rbp)
	leaq	-12(%rbp), %rsi
	xorl	%edi, %edi
	xorb	%al, %al
	movb	%al, -13(%rbp)          # 1-byte Spill
	movq	%rsi, -24(%rbp)         # 8-byte Spill
	callq	time
	movl	%eax, %edi
	callq	srand
	movl	$.L.str, %edi
	movb	-13(%rbp), %al          # 1-byte Reload
	callq	printf
	movl	$.L.str1, %edi
	movq	-24(%rbp), %rsi         # 8-byte Reload
	movb	-13(%rbp), %cl          # 1-byte Reload
	movl	%eax, -28(%rbp)         # 4-byte Spill
	movb	%cl, %al
	callq	__isoc99_scanf
	movl	%eax, -32(%rbp)         # 4-byte Spill
	callq	rand
	movslq	%eax, %rsi
	imulq	$-2004318071, %rsi, %rsi # imm = 0xFFFFFFFF88888889
	shrq	$32, %rsi
	movl	%esi, %edx
	addl	%eax, %edx
	movl	%edx, %r8d
	shrl	$31, %r8d
	sarl	$3, %edx
	addl	%r8d, %edx
	imull	$15, %edx, %edx
	movl	%eax, %r8d
	subl	%edx, %r8d
	movl	%r8d, -8(%rbp)
	movl	-8(%rbp), %edx
	addl	$1, %edx
	movl	%edx, -8(%rbp)
	movl	%eax, -36(%rbp)         # 4-byte Spill
.LBB0_1:                                # %while.cond
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB0_3 Depth 2
	cmpl	$0, -8(%rbp)
	je	.LBB0_21
# BB#2:                                 # %while.body
                                        #   in Loop: Header=BB0_1 Depth=1
	jmp	.LBB0_3
.LBB0_3:                                # %while.cond4
                                        #   Parent Loop BB0_1 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movb	$0, %al
	cmpl	$16, -12(%rbp)
	movb	%al, -37(%rbp)          # 1-byte Spill
	jge	.LBB0_5
# BB#4:                                 # %land.rhs
                                        #   in Loop: Header=BB0_3 Depth=2
	cmpl	$0, -12(%rbp)
	setg	%al
	movb	%al, -37(%rbp)          # 1-byte Spill
.LBB0_5:                                # %land.end
                                        #   in Loop: Header=BB0_3 Depth=2
	movb	-37(%rbp), %al          # 1-byte Reload
	testb	$1, %al
	jne	.LBB0_6
	jmp	.LBB0_20
.LBB0_6:                                # %while.body7
                                        #   in Loop: Header=BB0_3 Depth=2
	movl	-8(%rbp), %eax
	cmpl	-12(%rbp), %eax
	jne	.LBB0_8
# BB#7:                                 # %if.then
	leaq	.L.str2, %rdi
	movb	$0, %al
	callq	printf
	movl	$0, -4(%rbp)
	movl	%eax, -44(%rbp)         # 4-byte Spill
	jmp	.LBB0_22
.LBB0_8:                                # %if.else
                                        #   in Loop: Header=BB0_3 Depth=2
	movl	-8(%rbp), %eax
	movl	-12(%rbp), %ecx
	subl	$1, %ecx
	cmpl	%ecx, %eax
	je	.LBB0_10
# BB#9:                                 # %lor.lhs.false
                                        #   in Loop: Header=BB0_3 Depth=2
	movl	-8(%rbp), %eax
	movl	-12(%rbp), %ecx
	addl	$1, %ecx
	cmpl	%ecx, %eax
	jne	.LBB0_11
.LBB0_10:                               # %if.then13
                                        #   in Loop: Header=BB0_3 Depth=2
	leaq	.L.str3, %rdi
	movb	$0, %al
	callq	printf
	movl	%eax, -48(%rbp)         # 4-byte Spill
	jmp	.LBB0_18
.LBB0_11:                               # %if.else15
                                        #   in Loop: Header=BB0_3 Depth=2
	movl	-8(%rbp), %eax
	movl	-12(%rbp), %ecx
	addl	$2, %ecx
	cmpl	%ecx, %eax
	je	.LBB0_15
# BB#12:                                # %lor.lhs.false18
                                        #   in Loop: Header=BB0_3 Depth=2
	movl	-8(%rbp), %eax
	movl	-12(%rbp), %ecx
	addl	$3, %ecx
	cmpl	%ecx, %eax
	je	.LBB0_15
# BB#13:                                # %lor.lhs.false21
                                        #   in Loop: Header=BB0_3 Depth=2
	movl	-8(%rbp), %eax
	movl	-12(%rbp), %ecx
	subl	$2, %ecx
	cmpl	%ecx, %eax
	je	.LBB0_15
# BB#14:                                # %lor.lhs.false24
                                        #   in Loop: Header=BB0_3 Depth=2
	movl	-8(%rbp), %eax
	movl	-12(%rbp), %ecx
	subl	$3, %ecx
	cmpl	%ecx, %eax
	jne	.LBB0_16
.LBB0_15:                               # %if.then27
                                        #   in Loop: Header=BB0_3 Depth=2
	leaq	.L.str4, %rdi
	movb	$0, %al
	callq	printf
	movl	%eax, -52(%rbp)         # 4-byte Spill
	jmp	.LBB0_17
.LBB0_16:                               # %if.else29
                                        #   in Loop: Header=BB0_3 Depth=2
	leaq	.L.str5, %rdi
	movb	$0, %al
	callq	printf
	movl	%eax, -56(%rbp)         # 4-byte Spill
.LBB0_17:                               # %if.end
                                        #   in Loop: Header=BB0_3 Depth=2
	jmp	.LBB0_18
.LBB0_18:                               # %if.end31
                                        #   in Loop: Header=BB0_3 Depth=2
	jmp	.LBB0_19
.LBB0_19:                               # %if.end32
                                        #   in Loop: Header=BB0_3 Depth=2
	leaq	.L.str, %rdi
	movb	$0, %al
	callq	printf
	leaq	.L.str1, %rdi
	leaq	-12(%rbp), %rsi
	movl	%eax, -60(%rbp)         # 4-byte Spill
	movb	$0, %al
	callq	__isoc99_scanf
	movl	%eax, -64(%rbp)         # 4-byte Spill
	jmp	.LBB0_3
.LBB0_20:                               # %while.end
                                        #   in Loop: Header=BB0_1 Depth=1
	leaq	.L.str6, %rdi
	movb	$0, %al
	callq	printf
	leaq	.L.str, %rdi
	movl	%eax, -68(%rbp)         # 4-byte Spill
	movb	$0, %al
	callq	printf
	leaq	.L.str1, %rdi
	leaq	-12(%rbp), %rsi
	movl	%eax, -72(%rbp)         # 4-byte Spill
	movb	$0, %al
	callq	__isoc99_scanf
	movl	%eax, -76(%rbp)         # 4-byte Spill
	jmp	.LBB0_1
.LBB0_21:                               # %while.end38
	movl	$0, -4(%rbp)
.LBB0_22:                               # %return
	movl	-4(%rbp), %eax
	addq	$80, %rsp
	popq	%rbp
	ret
.Ltmp5:
	.size	main, .Ltmp5-main
	.cfi_endproc

	.type	.L.str,@object          # @.str
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str:
	.asciz	 "\nGuess an integer from 1-15\n"
	.size	.L.str, 29

	.type	.L.str1,@object         # @.str1
.L.str1:
	.asciz	 "%d"
	.size	.L.str1, 3

	.type	.L.str2,@object         # @.str2
.L.str2:
	.asciz	 "\nyou won\n"
	.size	.L.str2, 10

	.type	.L.str3,@object         # @.str3
.L.str3:
	.asciz	 "\nVery Close\n"
	.size	.L.str3, 13

	.type	.L.str4,@object         # @.str4
.L.str4:
	.asciz	 "\nGetting Close\n"
	.size	.L.str4, 16

	.type	.L.str5,@object         # @.str5
.L.str5:
	.asciz	 "\nNot Close\n"
	.size	.L.str5, 12

	.type	.L.str6,@object         # @.str6
.L.str6:
	.asciz	 "\nPick is not in Range\n"
	.size	.L.str6, 23


	.section	".note.GNU-stack","",@progbits
