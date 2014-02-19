	.file	"rand.c"
	.section	.rodata.str1.8,"aMS",@progbits,1
	.align 8
.LC0:
	.string	"Guess a random number between one and 25"
	.section	.rodata.str1.1,"aMS",@progbits,1
.LC1:
	.string	"%d"
	.section	.rodata.str1.8
	.align 8
.LC2:
	.string	"Congradulations you guessed right"
	.align 8
.LC3:
	.string	"Sorry wrong answer, enter a non zero number to  guess again?"
	.section	.rodata.str1.1
.LC4:
	.string	"Too many tries"
	.text
	.globl	main
	.type	main, @function
main:
.LFB21:
	.cfi_startproc
	pushq	%r12
	.cfi_def_cfa_offset 16
	.cfi_offset 12, -16
	pushq	%rbp
	.cfi_def_cfa_offset 24
	.cfi_offset 6, -24
	pushq	%rbx
	.cfi_def_cfa_offset 32
	.cfi_offset 3, -32
	subq	$16, %rsp
	.cfi_def_cfa_offset 48
	call	rand@PLT
	movl	%eax, %ebp
	movl	$1374389535, %edx
	imull	%edx
	sarl	$3, %edx
	movl	%ebp, %eax
	sarl	$31, %eax
	subl	%eax, %edx
	leal	(%rdx,%rdx,4), %eax
	leal	(%rax,%rax,4), %eax
	subl	%eax, %ebp
	addl	$1, %ebp
	movl	$0, 12(%rsp)
	leaq	.LC0(%rip), %rdi
	call	puts@PLT
	leaq	12(%rsp), %rsi
	leaq	.LC1(%rip), %rdi
	movl	$0, %eax
	call	__isoc99_scanf@PLT
	movl	$11, %ebx
	leaq	12(%rsp), %r12
.L5:
	cmpl	%ebp, 12(%rsp)
	jne	.L2
	leaq	.LC2(%rip), %rdi
	call	puts@PLT
	jmp	.L3
.L2:
	leaq	.LC3(%rip), %rdi
	call	puts@PLT
	subl	$1, %ebx
	jne	.L4
	leaq	.LC4(%rip), %rdi
	movl	$0, %eax
	call	printf@PLT
	jmp	.L3
.L4:
	movq	%r12, %rsi
	leaq	.LC1(%rip), %rdi
	movl	$0, %eax
	call	__isoc99_scanf@PLT
	movl	12(%rsp), %eax
	leal	-1(%rax), %edx
	cmpl	$24, %edx
	jbe	.L5
.L3:
	movl	$0, %eax
	addq	$16, %rsp
	.cfi_def_cfa_offset 32
	popq	%rbx
	.cfi_def_cfa_offset 24
	popq	%rbp
	.cfi_def_cfa_offset 16
	popq	%r12
	.cfi_def_cfa_offset 8
	ret
	.cfi_endproc
.LFE21:
	.size	main, .-main
	.ident	"GCC: (GNU) 4.8.2 20140206 (prerelease)"
	.section	.note.GNU-stack,"",@progbits
