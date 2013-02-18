	.file	"rand.c"
	.section	.rodata
	.align 8
.LC0:
	.string	"Guess a random number between one and 25"
.LC1:
	.string	"%d"
	.align 8
.LC2:
	.string	"Congradulations you guessed right"
	.align 8
.LC3:
	.string	"Sorry wrong answer, enter a non zero number to  guess again?"
.LC4:
	.string	"Too many tries"
	.text
	.globl	main
	.type	main, @function
main:
.LFB0:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	subq	$16, %rsp
	call	rand
	movl	%eax, %ecx
	movl	$1374389535, %edx
	movl	%ecx, %eax
	imull	%edx
	sarl	$3, %edx
	movl	%ecx, %eax
	sarl	$31, %eax
	subl	%eax, %edx
	movl	%edx, %eax
	sall	$2, %eax
	addl	%edx, %eax
	leal	0(,%rax,4), %edx
	addl	%edx, %eax
	movl	%ecx, %edx
	subl	%eax, %edx
	leal	1(%rdx), %eax
	movl	%eax, -8(%rbp)
	movl	$0, -12(%rbp)
	movl	$0, -4(%rbp)
	movl	$.LC0, %edi
	call	puts
	leaq	-12(%rbp), %rax
	movq	%rax, %rsi
	movl	$.LC1, %edi
	movl	$0, %eax
	call	__isoc99_scanf
	jmp	.L7
.L9:
	nop
.L7:
	movl	-12(%rbp), %eax
	cmpl	-8(%rbp), %eax
	jne	.L2
	movl	$.LC2, %edi
	call	puts
	movl	$0, %eax
	jmp	.L8
.L2:
	movl	$.LC3, %edi
	call	puts
	addl	$1, -4(%rbp)
	cmpl	$10, -4(%rbp)
	jle	.L4
	movl	$.LC4, %edi
	movl	$0, %eax
	call	printf
	movl	$0, %eax
	jmp	.L8
.L4:
	leaq	-12(%rbp), %rax
	movq	%rax, %rsi
	movl	$.LC1, %edi
	movl	$0, %eax
	call	__isoc99_scanf
	movl	-12(%rbp), %eax
	testl	%eax, %eax
	jle	.L5
	movl	-12(%rbp), %eax
	cmpl	$25, %eax
	jle	.L9
.L5:
	movl	$0, %eax
.L8:
	leave
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE0:
	.size	main, .-main
	.ident	"GCC: (GNU) 4.7.2 20120921 (Red Hat 4.7.2-2)"
	.section	.note.GNU-stack,"",@progbits
