	.file	"prog5.c"
	.text
	.type	tgkill, @function
tgkill:
.LFB80:
	.cfi_startproc
	pushq	%rbx
	.cfi_def_cfa_offset 16
	.cfi_offset 3, -16
	movl	$234, %eax
#APP
# 396 "my_threads.c" 1
	syscall
# 0 "" 2
#NO_APP
	movl	%eax, %ebx
	cmpl	$-4096, %ebx
	jbe	.L2
	call	__errno_location
	negl	%ebx
	movl	%ebx, (%rax)
	movl	$-1, %eax
.L2:
	popq	%rbx
	.cfi_def_cfa_offset 8
	ret
	.cfi_endproc
.LFE80:
	.size	tgkill, .-tgkill
	.section	.rodata.str1.1,"aMS",@progbits,1
.LC0:
	.string	"%s: %s; Error number %d\n"
.LC1:
	.string	"%s; Error number %d\n"
	.text
	.type	my_perror, @function
my_perror:
.LFB62:
	.cfi_startproc
	pushq	%rbx
	.cfi_def_cfa_offset 16
	.cfi_offset 3, -16
	movq	%rdi, %rbx
	testq	%rdi, %rdi
	je	.L6
	call	__errno_location
	movl	(%rax), %r8d
	movslq	%r8d, %rax
	movq	sys_errlist(,%rax,8), %rcx
	movq	%rbx, %rdx
	movl	$.LC0, %esi
	movq	stderr(%rip), %rdi
	movl	$0, %eax
	call	fprintf
	jmp	.L5
.L6:
	call	__errno_location
	movl	(%rax), %ecx
	movslq	%ecx, %rax
	movq	sys_errlist(,%rax,8), %rdx
	movl	$.LC1, %esi
	movq	stderr(%rip), %rdi
	movl	$0, %eax
	call	fprintf
.L5:
	popq	%rbx
	.cfi_def_cfa_offset 8
	ret
	.cfi_endproc
.LFE62:
	.size	my_perror, .-my_perror
	.globl	microsleep
	.type	microsleep, @function
microsleep:
.LFB63:
	.cfi_startproc
	subq	$8, %rsp
	.cfi_def_cfa_offset 16
	imulq	$1000, %rdi, %rdi
	movq	%rdi, sleep_time.5170+8(%rip)
	movl	$0, %esi
	movl	$sleep_time.5170, %edi
	call	nanosleep
	addq	$8, %rsp
	.cfi_def_cfa_offset 8
	ret
	.cfi_endproc
.LFE63:
	.size	microsleep, .-microsleep
	.section	.rodata.str1.1
.LC3:
	.string	"%s\n"
.LC2:
	.string	"0123456789abcdef"
	.text
	.globl	print_uint128
	.type	print_uint128, @function
print_uint128:
.LFB64:
	.cfi_startproc
	subq	$56, %rsp
	.cfi_def_cfa_offset 64
	movq	%rdi, %rax
	movq	%rsi, %rdx
	movq	$0, (%rsp)
	movq	$0, 8(%rsp)
	movq	$0, 16(%rsp)
	movq	$0, 24(%rsp)
	movq	$0, 32(%rsp)
	leaq	39(%rsp), %rcx
	movq	%rsp, %rdi
	jmp	.L12
.L14:
	cmpq	%rdi, %rcx
	je	.L17
	subq	$1, %rcx
	movq	%rax, %rsi
	andl	$15, %esi
	movzbl	.LC2(%rsi), %esi
	movb	%sil, (%rcx)
	shrdq	$4, %rdx, %rax
	shrq	$4, %rdx
.L12:
	movq	%rdx, %rsi
	orq	%rax, %rsi
	jne	.L14
	jmp	.L18
.L16:
	subq	$1, %rcx
	movb	$48, (%rcx)
	jmp	.L19
.L18:
	movq	%rsp, %rdx
.L19:
	movq	%rcx, %rax
	subq	%rdx, %rax
	cmpq	$8, %rax
	jg	.L16
	movb	$120, -1(%rcx)
	leaq	-2(%rcx), %rsi
	movb	$48, -2(%rcx)
	movl	$.LC3, %edi
	movl	$0, %eax
	call	printf
	jmp	.L13
.L17:
	movl	$-1, %eax
.L13:
	addq	$56, %rsp
	.cfi_def_cfa_offset 8
	ret
	.cfi_endproc
.LFE64:
	.size	print_uint128, .-print_uint128
	.globl	futex
	.type	futex, @function
futex:
.LFB65:
	.cfi_startproc
	pushq	%rbx
	.cfi_def_cfa_offset 16
	.cfi_offset 3, -16
	movl	$202, %eax
#APP
# 66 "my_threads.c" 1
	syscall
# 0 "" 2
#NO_APP
	movl	%eax, %ebx
	cmpl	$-4096, %ebx
	jbe	.L22
	call	__errno_location
	negl	%ebx
	movl	%ebx, (%rax)
	movl	$-1, %eax
.L22:
	popq	%rbx
	.cfi_def_cfa_offset 8
	ret
	.cfi_endproc
.LFE65:
	.size	futex, .-futex
	.section	.rodata.str1.8,"aMS",@progbits,1
	.align 8
.LC4:
	.string	"value of &futex_addr = %d\nvalue of val = %d\n"
	.text
	.globl	futex_wait
	.type	futex_wait, @function
futex_wait:
.LFB66:
	.cfi_startproc
	pushq	%rbx
	.cfi_def_cfa_offset 16
	.cfi_offset 3, -16
	movq	%rdi, %r9
	movl	%esi, %r10d
	movl	$202, %eax
#APP
# 77 "my_threads.c" 1
	xorq %rcx,%rcx
	xorq %r8,%r8
	movl %r10d,%edx
	movq $0,%rsi
	movl %eax,%eax
	syscall

# 0 "" 2
#NO_APP
	movl	%eax, %ebx
	cmpl	$-4096, %ebx
	jbe	.L27
	movl	%r10d, %ecx
	movl	(%r9), %edx
	movl	$.LC4, %esi
	movq	stderr(%rip), %rdi
	movl	$0, %eax
	call	fprintf
	call	__errno_location
	negl	%ebx
	movl	%ebx, (%rax)
	cmpl	$11, %ebx
	sete	%al
	movzbl	%al, %eax
	leal	-1(%rax,%rax), %eax
.L27:
	popq	%rbx
	.cfi_def_cfa_offset 8
	ret
	.cfi_endproc
.LFE66:
	.size	futex_wait, .-futex_wait
	.globl	futex_wake
	.type	futex_wake, @function
futex_wake:
.LFB67:
	.cfi_startproc
	pushq	%rbx
	.cfi_def_cfa_offset 16
	.cfi_offset 3, -16
	movl	%esi, %eax
#APP
# 106 "my_threads.c" 1
	movl %eax,%edx
	movq $1,%rsi
	movq $202,%rax
	syscall

# 0 "" 2
#NO_APP
	movl	%eax, %ebx
	cmpl	$-4096, %ebx
	jbe	.L32
	call	__errno_location
	negl	%ebx
	movl	%ebx, (%rax)
	movl	$-1, %eax
.L32:
	popq	%rbx
	.cfi_def_cfa_offset 8
	ret
	.cfi_endproc
.LFE67:
	.size	futex_wake, .-futex_wake
	.globl	futex_wait_locking
	.type	futex_wait_locking, @function
futex_wait_locking:
.LFB68:
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
	movq	%rdi, %rbp
	movl	%esi, %r12d
	movq	%rcx, %r9
#APP
# 125 "my_threads.c" 1
	movq $1,%r10
	movq $0,%r11
1:
	movq $1,%rax
	cmpl %eax,(%r9)
	je 2f
	pause
	jmp 1b
2:
	lock cmpxchgl %r11d,(%r9)
	jnz 1b
xorq %rcx,%rcx
	xorq %r8,%r8
	movl %r12d,%edx
	movq $0,%rsi
	movl $202,%eax
	lock xchgl %r10d,(%r9)
	syscall

# 0 "" 2
#NO_APP
	movl	%eax, %ebx
	cmpl	$-4096, %ebx
	jbe	.L36
	movl	%r12d, %ecx
	movl	0(%rbp), %edx
	movl	$.LC4, %esi
	movq	stderr(%rip), %rdi
	movl	$0, %eax
	call	fprintf
	call	__errno_location
	negl	%ebx
	movl	%ebx, (%rax)
	cmpl	$11, %ebx
	sete	%al
	movzbl	%al, %eax
	leal	-1(%rax,%rax), %eax
.L36:
	popq	%rbx
	.cfi_def_cfa_offset 24
	popq	%rbp
	.cfi_def_cfa_offset 16
	popq	%r12
	.cfi_def_cfa_offset 8
	ret
	.cfi_endproc
.LFE68:
	.size	futex_wait_locking, .-futex_wait_locking
	.globl	futex_wake_locking
	.type	futex_wake_locking, @function
futex_wake_locking:
.LFB69:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	pushq	%rbx
	.cfi_def_cfa_offset 24
	.cfi_offset 3, -24
	subq	$8, %rsp
	.cfi_def_cfa_offset 32
	movl	%esi, %eax
	movq	%rdx, %rbx
	movl	$1, %ebp
#APP
# 169 "my_threads.c" 1
	movq $0,%r9
1:
	movq $1,%rax
	cmpl %eax,(%rcx)
	je 2f
	pause
	jmp 1b
2:
	lock cmpxchgl %r9d,(%rcx)
	jnz 1b
	movl %eax,%edx
	movq $1,%rsi
	movq $202,%rax
	syscall
	lock xchgl %rbp,(%rbx)

# 0 "" 2
#NO_APP
	movl	%eax, %ebx
	cmpl	$-4096, %ebx
	jbe	.L41
	call	__errno_location
	negl	%ebx
	movl	%ebx, (%rax)
	movl	$-1, %eax
.L41:
	addq	$8, %rsp
	.cfi_def_cfa_offset 24
	popq	%rbx
	.cfi_def_cfa_offset 16
	popq	%rbp
	.cfi_def_cfa_offset 8
	ret
	.cfi_endproc
.LFE69:
	.size	futex_wake_locking, .-futex_wake_locking
	.globl	futex_cmp_requeue
	.type	futex_cmp_requeue, @function
futex_cmp_requeue:
.LFB70:
	.cfi_startproc
	pushq	%rbx
	.cfi_def_cfa_offset 16
	.cfi_offset 3, -16
	movl	%esi, %r9d
	movq	%rdx, %rax
	movl	%ecx, %r8d
#APP
# 206 "my_threads.c" 1
	movl %r8d,%r9d
	movq %rax,%r8
	movl %r9d,%edx
	movq $4,%rsi
	movq $202,%rax
	syscall

# 0 "" 2
#NO_APP
	movl	%eax, %ebx
	cmpl	$-4096, %ebx
	jbe	.L45
	call	__errno_location
	negl	%ebx
	movl	%ebx, (%rax)
	movl	$-1, %eax
.L45:
	popq	%rbx
	.cfi_def_cfa_offset 8
	ret
	.cfi_endproc
.LFE70:
	.size	futex_cmp_requeue, .-futex_cmp_requeue
	.globl	futex_requeue
	.type	futex_requeue, @function
futex_requeue:
.LFB71:
	.cfi_startproc
	pushq	%rbx
	.cfi_def_cfa_offset 16
	.cfi_offset 3, -16
	movl	%esi, %r8d
	movq	%rdx, %rax
#APP
# 232 "my_threads.c" 1
	movq %rax,%r8
	movl %r8d,%edx
	movq $4,%rsi
	movq $202,%rax
	syscall

# 0 "" 2
#NO_APP
	movl	%eax, %ebx
	cmpl	$-4096, %ebx
	jbe	.L49
	call	__errno_location
	negl	%ebx
	movl	%ebx, (%rax)
	movl	$-1, %eax
.L49:
	popq	%rbx
	.cfi_def_cfa_offset 8
	ret
	.cfi_endproc
.LFE71:
	.size	futex_requeue, .-futex_requeue
	.globl	futex_up
	.type	futex_up, @function
futex_up:
.LFB72:
	.cfi_startproc
	pushq	%rbx
	.cfi_def_cfa_offset 16
	.cfi_offset 3, -16
	movl	$202, %eax
#APP
# 257 "my_threads.c" 1
	lock addq $1,(%rdi)
	cmpl $1,(%rdi)
	je 1f
	movq $1,(%rdi)
	movq $1,%rdx
	movq $1,%rsi
	movq %rax, %rax
	syscall
	1:
	
# 0 "" 2
#NO_APP
	movq	%rax, %rbx
	cmpq	$-4096, %rbx
	jbe	.L53
	call	__errno_location
	negl	%ebx
	movl	%ebx, (%rax)
	movq	$-1, %rax
.L53:
	popq	%rbx
	.cfi_def_cfa_offset 8
	ret
	.cfi_endproc
.LFE72:
	.size	futex_up, .-futex_up
	.globl	futex_down
	.type	futex_down, @function
futex_down:
.LFB73:
	.cfi_startproc
	pushq	%rbx
	.cfi_def_cfa_offset 16
	.cfi_offset 3, -16
	movl	$202, %eax
#APP
# 278 "my_threads.c" 1
	lock subq $1,(%rdi)
	cmpq $0,(%rdi)
	je 1f
	movq $-1,(%rdi)
	movq $-1,%rdx
	movq $0,%rsi
	xorq %rcx,%rcx
	xorq %r8,%r8
	syscall
	1:
	
# 0 "" 2
#NO_APP
	movq	%rax, %rbx
	cmpq	$-4096, %rbx
	jbe	.L57
	call	__errno_location
	negl	%ebx
	movl	%ebx, (%rax)
	movq	$-1, %rax
.L57:
	popq	%rbx
	.cfi_def_cfa_offset 8
	ret
	.cfi_endproc
.LFE73:
	.size	futex_down, .-futex_down
	.globl	futex_spin_up
	.type	futex_spin_up, @function
futex_spin_up:
.LFB74:
	.cfi_startproc
	movl	$1, %eax
#APP
# 301 "my_threads.c" 1
	lock xchgq %rax,(%rdi)

# 0 "" 2
#NO_APP
	ret
	.cfi_endproc
.LFE74:
	.size	futex_spin_up, .-futex_spin_up
	.globl	futex_spin_down
	.type	futex_spin_down, @function
futex_spin_down:
.LFB75:
	.cfi_startproc
	movl	$0, %edx
#APP
# 307 "my_threads.c" 1
	1:
	movq $1, %rax
	cmpl %eax,(%rdi)
	je 2f
	pause
	jmp 1b
2:
	lock cmpxchgl %edx,(%rdi)
	jnz 1b

# 0 "" 2
#NO_APP
	ret
	.cfi_endproc
.LFE75:
	.size	futex_spin_down, .-futex_spin_down
	.type	str_malloc, @function
str_malloc:
.LFB82:
	.cfi_startproc
	pushq	%rbx
	.cfi_def_cfa_offset 16
	.cfi_offset 3, -16
	movq	%rdi, %rbx
	movl	$string_mem_lock, %edi
	call	futex_spin_down
	movq	%rbx, %rax
	addq	string_mem_pointer(%rip), %rax
	cmpq	string_mem_end(%rip), %rax
	jbe	.L62
	movl	$0, %r9d
	movl	$-1, %r8d
	movl	$34, %ecx
	movl	$3, %edx
	movl	$8388608, %esi
	movl	$0, %edi
	call	mmap
	movq	%rax, string_mem_pointer(%rip)
	addq	$8388608, %rax
	movq	%rax, string_mem_end(%rip)
.L62:
	movq	string_mem_pointer(%rip), %rax
	addq	%rax, %rbx
	movq	%rbx, string_mem_pointer(%rip)
	movl	$string_mem_lock, %ecx
	movl	$1, %edx
#APP
# 301 "my_threads.c" 1
	lock xchgq %rdx,(%rcx)

# 0 "" 2
#NO_APP
	popq	%rbx
	.cfi_def_cfa_offset 8
	ret
	.cfi_endproc
.LFE82:
	.size	str_malloc, .-str_malloc
	.type	atomic_hash_table_update, @function
atomic_hash_table_update:
.LFB84:
	.cfi_startproc
	pushq	%r14
	.cfi_def_cfa_offset 16
	.cfi_offset 14, -16
	pushq	%r13
	.cfi_def_cfa_offset 24
	.cfi_offset 13, -24
	pushq	%r12
	.cfi_def_cfa_offset 32
	.cfi_offset 12, -32
	pushq	%rbp
	.cfi_def_cfa_offset 40
	.cfi_offset 6, -40
	pushq	%rbx
	.cfi_def_cfa_offset 48
	.cfi_offset 3, -48
	subq	$16, %rsp
	.cfi_def_cfa_offset 64
	movq	%rdi, %rbp
	movl	8(%rdi), %r8d
	movl	%r8d, %ecx
	movq	(%rdi), %rdi
	movabsq	$-3750763034362895579, %rbx
	movl	$0, %eax
	movabsq	$1099511628211, %rsi
	jmp	.L65
.L66:
	movslq	%eax, %rdx
	movzbl	(%rdi,%rdx), %edx
	xorq	%rdx, %rbx
	imulq	%rsi, %rbx
	addl	$1, %eax
.L65:
	cmpl	%eax, %ecx
	jg	.L66
	andl	$1048575, %ebx
	movq	16(%rbp), %r14
	movl	$0, %r13d
	cmpq	$0, global_hash_table(,%rbx,8)
	jne	.L81
	movl	%r8d, %edi
	call	str_malloc
	movq	%rax, %r13
	movl	8(%rbp), %ecx
	movq	0(%rbp), %rsi
	movq	%rax, %rdi
#APP
# 365 "prog5.h" 1
	rep movsb
# 0 "" 2
#NO_APP
	movq	%rax, 0(%rbp)
	movq	global_hash_table(,%rbx,8), %rax
	movq	%rax, 8(%rsp)
	leaq	0(,%rbx,8), %rdx
	lock cmpxchgq	%rbp, global_hash_table(%rdx)
	movq	%rax, 8(%rsp)
	jne	.L81
	movl	$1, %eax
	lock xaddl	%eax, indices_index(%rip)
	movl	%eax, %eax
	movl	%ebx, hash_table_indices(,%rax,4)
	movl	$1, %eax
	jmp	.L69
.L81:
	movq	global_hash_table(,%rbx,8), %r12
	movl	8(%r12), %edx
	movl	$0, %ecx
	cmpl	8(%rbp), %edx
	jne	.L71
	movq	0(%rbp), %rsi
	movq	(%r12), %rdi
	movl	%edx, %edx
	call	memcmp
	testl	%eax, %eax
	sete	%cl
	movzbl	%cl, %ecx
.L71:
	testl	%ecx, %ecx
	je	.L72
	addq	$12, %r12
	lock addl	$1, (%r12)
	testq	%r14, %r14
	je	.L73
	movq	16(%rbp), %rdx
	movq	global_hash_table(,%rbx,8), %rax
	addq	$16, %rax
	lock orq	%rdx, (%rax)
	jmp	.L74
.L73:
	movq	24(%rbp), %rdx
	movq	global_hash_table(,%rbx,8), %rax
	addq	$24, %rax
	lock orq	%rdx, (%rax)
	jmp	.L74
.L72:
	addq	$1, %rbx
	cmpq	$0, global_hash_table(,%rbx,8)
	jne	.L81
	testq	%r13, %r13
	jne	.L76
	movl	8(%rbp), %edi
	call	str_malloc
	movq	%rax, %r13
	movl	8(%rbp), %ecx
	movq	0(%rbp), %rsi
	movq	%rax, %rdi
#APP
# 365 "prog5.h" 1
	rep movsb
# 0 "" 2
#NO_APP
	movq	%rax, 0(%rbp)
.L76:
	movq	global_hash_table(,%rbx,8), %rax
	movq	%rax, 8(%rsp)
	leaq	0(,%rbx,8), %rdx
	lock cmpxchgq	%rbp, global_hash_table(%rdx)
	movq	%rax, 8(%rsp)
	jne	.L81
	movl	$1, %eax
	lock xaddl	%eax, indices_index(%rip)
	movl	%eax, %eax
	movl	%ebx, hash_table_indices(,%rax,4)
	movl	$1, %eax
	jmp	.L69
.L74:
	movl	$0, %eax
.L69:
	addq	$16, %rsp
	.cfi_def_cfa_offset 48
	popq	%rbx
	.cfi_def_cfa_offset 40
	popq	%rbp
	.cfi_def_cfa_offset 32
	popq	%r12
	.cfi_def_cfa_offset 24
	popq	%r13
	.cfi_def_cfa_offset 16
	popq	%r14
	.cfi_def_cfa_offset 8
	ret
	.cfi_endproc
.LFE84:
	.size	atomic_hash_table_update, .-atomic_hash_table_update
	.globl	futex_spin_down_slow
	.type	futex_spin_down_slow, @function
futex_spin_down_slow:
.LFB76:
	.cfi_startproc
	movl	$0, %edx
#APP
# 321 "my_threads.c" 1
	1:
	movq $1,%rax
	cmpl %eax,(%rdi)
	je 2f
	pause
	pause
	pause
	pause
	pause
	jmp 1b
2:
	lock cmpxchgl %edx,(%rdi)
	jnz 1b

# 0 "" 2
#NO_APP
	ret
	.cfi_endproc
.LFE76:
	.size	futex_spin_down_slow, .-futex_spin_down_slow
	.globl	clone_syscall
	.type	clone_syscall, @function
clone_syscall:
.LFB77:
	.cfi_startproc
	pushq	%rbx
	.cfi_def_cfa_offset 16
	.cfi_offset 3, -16
	movl	$56, %eax
#APP
# 352 "my_threads.c" 1
	syscall
# 0 "" 2
#NO_APP
	movq	%rax, %rbx
	cmpq	$-4096, %rbx
	jbe	.L85
	call	__errno_location
	negl	%ebx
	movl	%ebx, (%rax)
	movq	$-1, %rax
.L85:
	popq	%rbx
	.cfi_def_cfa_offset 8
	ret
	.cfi_endproc
.LFE77:
	.size	clone_syscall, .-clone_syscall
	.globl	my_clone
	.type	my_clone, @function
my_clone:
.LFB78:
	.cfi_startproc
	pushq	%rbx
	.cfi_def_cfa_offset 16
	.cfi_offset 3, -16
	movq	%rcx, %r9
	movq	%r8, %r10
	movl	$56, %eax
#APP
# 362 "my_threads.c" 1
	xorq %rcx,%rcx
	movq  $0,%r8
	syscall

# 0 "" 2
#NO_APP
	movq	%rax, %rbx
	cmpq	$-4096, %rax
	jbe	.L89
	call	__errno_location
	negl	%ebx
	movl	%ebx, (%rax)
	movq	$-1, %rax
	jmp	.L90
.L89:
	testq	%rax, %rax
	jg	.L91
	movq	%r10, %rdi
	call	*%r9
.L91:
.L90:
	popq	%rbx
	.cfi_def_cfa_offset 8
	.p2align 4,,3
	ret
	.cfi_endproc
.LFE78:
	.size	my_clone, .-my_clone
	.globl	thread_exit
	.type	thread_exit, @function
thread_exit:
.LFB79:
	.cfi_startproc
#APP
# 386 "my_threads.c" 1
	mov $60,%rax
	syscall

# 0 "" 2
#NO_APP
	.cfi_endproc
.LFE79:
	.size	thread_exit, .-thread_exit
	.globl	terminate_gracefully
	.type	terminate_gracefully, @function
terminate_gracefully:
.LFB83:
	.cfi_startproc
	subq	$8, %rsp
	.cfi_def_cfa_offset 16
	cmpl	$11, %edi
	je	.L95
	movl	$0, %edi
	call	thread_exit
.L95:
	movl	$1, %edi
	call	exit
	.cfi_endproc
.LFE83:
	.size	terminate_gracefully, .-terminate_gracefully
	.globl	parse_buf
	.type	parse_buf, @function
parse_buf:
.LFB85:
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
	subq	$8, %rsp
	.cfi_def_cfa_offset 64
	movq	%rdx, %r12
	movq	%rdi, %rbx
	leal	-1(%rsi), %eax
	cltq
	salq	$4, %rax
	movq	file_bit_masks(%rax), %r13
	movq	file_bit_masks+8(%rax), %r14
.L98:
#APP
# 126 "prog5.c" 1
	prefetchnta 768(%rbx)
# 0 "" 2
#NO_APP
.L99:
	movzbl	(%rbx), %eax
	cmpb	$0, eng_accept(%rax)
	jne	.L100
	movzbl	1(%rbx), %eax
	cmpb	$0, eng_accept(%rax)
	jne	.L101
	movzbl	2(%rbx), %eax
	cmpb	$0, eng_accept(%rax)
	jne	.L102
	movzbl	3(%rbx), %eax
	cmpb	$0, eng_accept(%rax)
	jne	.L103
	leaq	4(%rbx), %rbx
	jmp	.L99
.L103:
	leaq	1(%rbx), %rbx
.L102:
	addq	$1, %rbx
.L101:
	addq	$1, %rbx
.L100:
	movl	$1, %ebp
.L107:
	movq	%rbx, %r15
	movzbl	(%rbx,%rbp), %eax
	cmpb	$0, eng_accept(%rax)
	je	.L104
	leaq	1(%rbp), %rax
	movzbl	(%rbx,%rax), %edx
	cmpb	$0, eng_accept(%rdx)
	je	.L105
	movzbl	2(%rbx,%rbp), %edx
	cmpb	$0, eng_accept(%rdx)
	je	.L108
	movzbl	3(%rbx,%rbp), %edx
	cmpb	$0, eng_accept(%rdx)
	je	.L111
	addq	$4, %rbp
	jmp	.L107
.L111:
	movq	%rax, %rbp
.L106:
.L108:
	addq	$1, %rbp
.L105:
	addq	$1, %rbp
.L104:
	leaq	-6(%rbp), %rax
	cmpq	$44, %rax
	ja	.L109
	movq	%r15, (%r12)
	movl	%ebp, 8(%r12)
	movl	$1, 12(%r12)
	movq	%r13, 16(%r12)
	movq	%r14, 24(%r12)
	movq	%r12, %rdi
	call	atomic_hash_table_update
	leaq	32(%r12), %rdx
	testl	%eax, %eax
	cmovne	%rdx, %r12
.L109:
	leaq	(%r15,%rbp), %rbx
	cmpb	$-1, (%rbx)
	jne	.L98
	movq	%r12, %rax
	addq	$8, %rsp
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
	.cfi_endproc
.LFE85:
	.size	parse_buf, .-parse_buf
	.section	.rodata.str1.1
.LC5:
	.string	"Futex failure\n"
	.text
	.globl	thread_main
	.type	thread_main, @function
thread_main:
.LFB86:
	.cfi_startproc
	pushq	%r14
	.cfi_def_cfa_offset 16
	.cfi_offset 14, -16
	pushq	%r13
	.cfi_def_cfa_offset 24
	.cfi_offset 13, -24
	pushq	%r12
	.cfi_def_cfa_offset 32
	.cfi_offset 12, -32
	pushq	%rbp
	.cfi_def_cfa_offset 40
	.cfi_offset 6, -40
	pushq	%rbx
	.cfi_def_cfa_offset 48
	.cfi_offset 3, -48
	movq	%rdi, %rbx
	movl	$0, %r9d
	movl	$-1, %r8d
	movl	$34, %ecx
	movl	$3, %edx
	movl	$2097152, %esi
	movl	$0, %edi
	call	mmap
	movq	%rax, %rbp
	movq	%rbx, %rdx
	salq	$13, %rdx
	movq	%rbx, %rax
	salq	$17, %rax
	leaq	thread_bufs(%rdx,%rax), %r13
	leaq	0(,%rbx,8), %r12
	leaq	thread_futex_locks(%r12), %r14
	addq	$thread_futexes, %r12
.L116:
	movl	thread_fileinfo_vals(,%rbx,8), %esi
	movl	thread_fileinfo_vals+4(,%rbx,8), %edi
	addq	%r13, %rdi
	movq	%rbp, %rdx
	call	parse_buf
	movq	%rax, %rbp
	movl	$thread_queue_lock, %edi
	call	futex_spin_down
	movzbl	thread_queue_index(%rip), %esi
	leal	1(%rsi), %eax
	movb	%al, thread_queue_index(%rip)
	movzbl	%sil, %esi
	movb	%bl, thread_queue(%rsi)
	movl	$main_thread_wait, %eax
#APP
# 194 "prog5.c" 1
	lock incq (%rax)
# 0 "" 2
#NO_APP
	movl	$main_thread_wait_lock, %edx
	movl	$10, %esi
	movq	%rax, %rdi
	call	futex_wake_locking
	movl	$thread_queue_lock, %ecx
	movl	$1, %edx
#APP
# 301 "my_threads.c" 1
	lock xchgq %rdx,(%rcx)

# 0 "" 2
#NO_APP
	cmpl	$-1, %eax
	jne	.L114
	movl	$.LC5, %edi
	call	perror
	movl	$1, %edi
	call	exit
.L114:
	movq	%r14, %rcx
	movl	$0, %edx
	movl	$0, %esi
	movq	%r12, %rdi
	call	futex_wait_locking
	cmpl	$-1, %eax
	jne	.L115
	movl	$.LC5, %edi
	call	perror
	movl	$1, %edi
	call	exit
.L115:
	movq	$0, (%r12)
	mfence
	jmp	.L116
	.cfi_endproc
.LFE86:
	.size	thread_main, .-thread_main
	.section	.rodata.str1.1
.LC6:
	.string	"error reading from file"
.LC7:
	.string	"error closing file"
	.text
	.globl	setup_block
	.type	setup_block, @function
setup_block:
.LFB90:
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
	movq	%rdi, %rbp
	movq	%rsi, %rbx
	cmpq	$139263, 8(%rdi)
	ja	.L119
	movl	16(%rdi), %edi
	movl	$139264, %edx
	call	read
	movq	%rax, %r12
	cmpq	$-1, %rax
	jne	.L120
	movl	$.LC6, %edi
	call	perror
	movl	$1, %edi
	call	exit
.L120:
	movl	16(%rbp), %edi
	call	close
	cmpl	$-1, %eax
	jne	.L121
	movl	$.LC7, %edi
	call	perror
	movl	$1, %edi
	call	exit
.L121:
	movq	$0, 8(%rbp)
	leal	-1(%r12), %edx
	movl	%edx, %eax
	movzbl	(%rbx,%rax), %eax
	cmpb	$0, eng_accept(%rax)
	je	.L145
	movl	%r12d, %r12d
	movb	$-1, (%rbx,%r12)
	jmp	.L123
.L136:
	movl	%eax, %edx
.L145:
	leal	-1(%rdx), %eax
	movl	%eax, %ecx
	movzbl	(%rbx,%rcx), %ecx
	cmpb	$0, eng_accept(%rcx)
	je	.L136
	movl	%edx, %edx
	movb	$-1, (%rbx,%rdx)
	jmp	.L123
.L119:
	movl	16(%rdi), %edi
	movl	$131072, %edx
	call	read
	cmpq	$-1, %rax
	jne	.L124
	movl	$.LC6, %edi
	call	perror
	movl	$1, %edi
	call	exit
.L124:
	subq	$131072, 8(%rbp)
.L123:
	movl	24(%rbp), %eax
	testl	%eax, %eax
	je	.L125
	movzbl	(%rbx), %edx
	movl	$0, %r12d
	cmpb	$0, eng_accept(%rdx)
	je	.L126
	cmpl	$49, %eax
	jg	.L126
	movl	$0, %edx
.L127:
	movl	24(%rbp), %eax
	addl	$1, %eax
	movl	%eax, 24(%rbp)
	leal	1(%rdx), %r12d
	movl	%edx, %edx
	movzbl	(%rbx,%rdx), %ecx
	movslq	%eax, %rdx
	movb	%cl, 32(%rbp,%rdx)
	movl	%r12d, %edx
	movzbl	(%rbx,%rdx), %edx
	cmpb	$0, eng_accept(%rdx)
	je	.L126
	movl	%r12d, %edx
	cmpl	$49, %eax
	jle	.L127
.L126:
	cmpl	$50, 24(%rbp)
	jne	.L128
.L142:
	addl	$1, %r12d
	movl	%r12d, %eax
	movzbl	(%rbx,%rax), %eax
	cmpb	$0, eng_accept(%rax)
	jne	.L142
	movl	$0, 24(%rbp)
.L128:
	movl	24(%rbp), %eax
	cmpl	$6, %eax
	jle	.L130
	movq	border_word_mem_ptr(%rip), %rdi
	leaq	32(%rbp), %rcx
	movl	20(%rbp), %esi
	leal	-1(%rsi), %edx
	movq	%rcx, (%rdi)
	movl	%eax, 8(%rdi)
	movl	$1, 12(%rdi)
	movslq	%edx, %rax
	salq	$4, %rax
	movq	file_bit_masks(%rax), %r9
	movq	file_bit_masks+8(%rax), %r10
	movq	%r9, 16(%rdi)
	movq	%r10, 24(%rdi)
	call	atomic_hash_table_update
	testl	%eax, %eax
	je	.L130
	addq	$32, border_word_mem_ptr(%rip)
.L130:
	movl	$0, 24(%rbp)
	movl	%r12d, 28(%rbp)
	jmp	.L131
.L125:
	movl	$0, 28(%rbp)
.L131:
	cmpq	$0, 8(%rbp)
	je	.L132
	movzbl	131071(%rbx), %eax
	movl	$131071, %edx
	cmpb	$0, eng_accept(%rax)
	je	.L146
	jmp	.L134
.L140:
	movl	%eax, %edx
.L134:
	leal	-1(%rdx), %eax
	movl	%eax, %ecx
	movzbl	(%rbx,%rcx), %ecx
	cmpb	$0, eng_accept(%rcx)
	jne	.L140
	movl	%edx, %esi
	movl	$131072, %ecx
	subq	%rsi, %rcx
	addq	%rbx, %rsi
	leaq	32(%rbp), %rdi
#APP
# 365 "prog5.h" 1
	rep movsb
# 0 "" 2
#NO_APP
	movl	$131072, %eax
	subl	%edx, %eax
	movl	%eax, 24(%rbp)
	jmp	.L146
.L141:
	movl	%ecx, %edx
.L146:
	leal	-1(%rdx), %ecx
	movl	%ecx, %eax
	movzbl	(%rbx,%rax), %eax
	cmpb	$0, eng_accept(%rax)
	je	.L141
	movl	%edx, %edx
	movb	$-1, (%rbx,%rdx)
.L132:
	movq	%rbp, %rax
	popq	%rbx
	.cfi_def_cfa_offset 24
	popq	%rbp
	.cfi_def_cfa_offset 16
	popq	%r12
	.cfi_def_cfa_offset 8
	ret
	.cfi_endproc
.LFE90:
	.size	setup_block, .-setup_block
	.section	.rodata.str1.1
.LC8:
	.string	"No words common to all files\n"
.LC9:
	.string	"The word "
.LC10:
	.string	" occurred %d times\n"
	.text
	.globl	sort_words
	.type	sort_words, @function
sort_words:
.LFB96:
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
	movl	$-1, %r10d
	movl	$0, %eax
	movl	$0, %ebx
	jmp	.L149
.L159:
	leal	1(%rax), %r8d
	movl	%eax, %eax
	movl	hash_table_indices(,%rax,4), %eax
	movq	global_hash_table(,%rax,8), %rax
	movq	all_file_bits+8(%rip), %rcx
	xorq	24(%rax), %rcx
	movq	all_file_bits(%rip), %rdx
	xorq	16(%rax), %rdx
	orq	%rdx, %rcx
	jne	.L150
	movl	12(%rax), %r9d
	cmpl	%r10d, %r9d
	cmovae	%r10d, %r9d
	movl	%ebx, %edx
	movq	%rax, most_common.5582(,%rdx,8)
	movl	%ebx, %eax
	jmp	.L196
.L157:
	movl	$0, %esi
	testl	%eax, %eax
	je	.L153
	leal	-1(%rax), %esi
	shrl	%esi
	movl	%esi, %esi
	salq	$3, %rsi
.L153:
	movq	most_common.5582(%rsi), %rsi
	movq	%rsi, most_common.5582(,%rcx,8)
	movl	$0, %ecx
	testl	%eax, %eax
	je	.L154
	leal	-1(%rax), %ecx
	shrl	%ecx
	movl	%ecx, %ecx
	salq	$3, %rcx
.L154:
	movq	%rdx, most_common.5582(%rcx)
	testl	%eax, %eax
	je	.L196
	subl	$1, %eax
	shrl	%eax
.L196:
	movl	%eax, %ecx
	movq	most_common.5582(,%rcx,8), %rdx
	movl	12(%rdx), %edi
	movl	$0, %esi
	testl	%eax, %eax
	je	.L156
	leal	-1(%rax), %esi
	shrl	%esi
	movl	%esi, %esi
	salq	$3, %rsi
.L156:
	movq	most_common.5582(%rsi), %rsi
	cmpl	12(%rsi), %edi
	ja	.L157
	addl	$1, %ebx
	movl	%r9d, %r10d
.L150:
	movl	%r8d, %eax
.L149:
	cmpl	$30, %ebx
	ja	.L158
	cmpl	indices_index(%rip), %eax
	jb	.L159
.L158:
	cmpl	$19, %ebx
	ja	.L200
	testl	%ebx, %ebx
	jne	.L190
	movq	stderr(%rip), %rcx
	movl	$29, %edx
	movl	$1, %esi
	movl	$.LC8, %edi
	call	fwrite
	movl	$0, %edi
	call	exit
.L162:
	movslq	%ebp, %rax
	movq	most_common.5582(,%rax,8), %r12
	movl	$.LC9, %edi
	movl	$0, %eax
	call	printf
	movq	(%r12), %rdi
	movl	8(%r12), %esi
	movq	stdout(%rip), %rcx
	movl	$1, %edx
	call	fwrite
	movl	12(%r12), %esi
	movl	$.LC10, %edi
	movl	$0, %eax
	call	printf
	addl	$1, %ebp
	jmp	.L161
.L190:
	movl	$0, %ebp
.L161:
	cmpl	%ebx, %ebp
	jb	.L162
	leal	-1(%rbx), %r8d
	jmp	.L197
.L168:
	movl	%r8d, %eax
	movq	most_common.5582(,%rax,8), %rdx
	movq	most_common.5582(%rip), %rcx
	movq	%rcx, most_common.5582(,%rax,8)
	movq	%rdx, most_common.5582(%rip)
	subl	$1, %r8d
	movl	$0, %edx
	jmp	.L164
.L167:
	movl	%eax, %esi
	movq	most_common.5582(,%rsi,8), %r9
	movl	%edx, %edi
	movq	most_common.5582(,%rdi,8), %rsi
	movl	12(%rsi), %r11d
	cmpl	%r11d, 12(%r9)
	cmovbe	%edx, %eax
	addl	$2, %ecx
	cmpl	%ecx, %r8d
	jb	.L166
	movl	%ecx, %r9d
	movq	most_common.5582(,%r9,8), %r10
	movl	%eax, %r9d
	movq	most_common.5582(,%r9,8), %r9
	movl	12(%r9), %r11d
	cmpl	%r11d, 12(%r10)
	cmova	%ecx, %eax
.L166:
	cmpl	%eax, %edx
	je	.L197
	movl	%eax, %edx
	movq	most_common.5582(,%rdx,8), %rcx
	movq	%rcx, most_common.5582(,%rdi,8)
	movq	%rsi, most_common.5582(,%rdx,8)
	movl	%eax, %edx
.L164:
	leal	(%rdx,%rdx), %ecx
	leal	1(%rcx), %eax
	cmpl	%eax, %r8d
	jae	.L167
.L197:
	testl	%r8d, %r8d
	jne	.L168
	movl	$most_common.5582, %eax
	movl	%ebx, %edx
	jmp	.L169
.L180:
	leal	1(%rax), %r9d
	movl	%eax, %eax
	movl	hash_table_indices(,%rax,4), %eax
	movq	global_hash_table(,%rax,8), %rcx
	movq	all_file_bits+8(%rip), %rdx
	xorq	24(%rcx), %rdx
	movq	all_file_bits(%rip), %rax
	xorq	16(%rcx), %rax
	orq	%rax, %rdx
	jne	.L170
	cmpl	%r10d, 12(%rcx)
	jbe	.L170
	movq	most_common.5582+120(%rip), %rax
	movl	12(%rax), %eax
	movq	%rax, %r10
	salq	$32, %r10
	orq	%rax, %r10
	movl	$15, %eax
	movl	$16, %edx
	jmp	.L171
.L173:
	movslq	%edx, %r8
	movq	most_common.5582(,%r8,8), %r8
	movl	12(%r8), %edi
	movq	%r10, %rsi
	sarq	$32, %rsi
	cmpl	%esi, %edi
	jae	.L172
	salq	$32, %rdi
	movl	%esi, %r10d
	orq	%rdi, %r10
	movl	%edx, %eax
.L172:
	addl	$1, %edx
.L171:
	cmpl	$30, %edx
	jle	.L173
	movslq	%eax, %rdx
	movq	%rcx, most_common.5582(,%rdx,8)
	jmp	.L198
.L179:
	movl	$0, %r8d
	testl	%eax, %eax
	je	.L175
	leal	-1(%rax), %esi
	movl	%esi, %r8d
	shrl	$31, %r8d
	addl	%r8d, %esi
	sarl	%esi
	movslq	%esi, %r8
	salq	$3, %r8
.L175:
	movq	most_common.5582(%r8), %rsi
	movq	%rsi, most_common.5582(,%rcx,8)
	movl	$0, %ecx
	testl	%eax, %eax
	je	.L176
	leal	-1(%rax), %ecx
	movl	%ecx, %esi
	shrl	$31, %esi
	addl	%esi, %ecx
	sarl	%ecx
	movslq	%ecx, %rcx
	salq	$3, %rcx
.L176:
	movq	%rdx, most_common.5582(%rcx)
	testl	%eax, %eax
	je	.L198
	subl	$1, %eax
	movl	%eax, %edx
	shrl	$31, %edx
	addl	%edx, %eax
	sarl	%eax
.L198:
	movslq	%eax, %rcx
	movq	most_common.5582(,%rcx,8), %rdx
	movl	12(%rdx), %edi
	movl	$0, %r8d
	testl	%eax, %eax
	je	.L178
	leal	-1(%rax), %esi
	movl	%esi, %r8d
	shrl	$31, %r8d
	addl	%r8d, %esi
	sarl	%esi
	movslq	%esi, %r8
	salq	$3, %r8
.L178:
	movq	most_common.5582(%r8), %rsi
	cmpl	12(%rsi), %edi
	ja	.L179
.L170:
	movl	%r9d, %eax
.L200:
	cmpl	indices_index(%rip), %eax
	jb	.L180
	movl	$30, %r8d
	jmp	.L199
.L186:
	movl	%r8d, %eax
	movq	most_common.5582(,%rax,8), %rdx
	movq	most_common.5582(%rip), %rcx
	movq	%rcx, most_common.5582(,%rax,8)
	movq	%rdx, most_common.5582(%rip)
	subl	$1, %r8d
	movl	$0, %edx
	jmp	.L182
.L185:
	movl	%eax, %esi
	movq	most_common.5582(,%rsi,8), %r9
	movl	%edx, %edi
	movq	most_common.5582(,%rdi,8), %rsi
	movl	12(%rsi), %ebx
	cmpl	%ebx, 12(%r9)
	cmovbe	%edx, %eax
	addl	$2, %ecx
	cmpl	%ecx, %r8d
	jb	.L184
	movl	%ecx, %r9d
	movq	most_common.5582(,%r9,8), %r10
	movl	%eax, %r9d
	movq	most_common.5582(,%r9,8), %r9
	movl	12(%r9), %ebx
	cmpl	%ebx, 12(%r10)
	cmova	%ecx, %eax
.L184:
	cmpl	%eax, %edx
	je	.L199
	movl	%eax, %edx
	movq	most_common.5582(,%rdx,8), %rcx
	movq	%rcx, most_common.5582(,%rdi,8)
	movq	%rsi, most_common.5582(,%rdx,8)
	movl	%eax, %edx
.L182:
	leal	(%rdx,%rdx), %ecx
	leal	1(%rcx), %eax
	cmpl	%eax, %r8d
	jae	.L185
.L199:
	testl	%r8d, %r8d
	jne	.L186
	movl	$most_common.5582, %eax
	movl	$31, %edx
.L169:
	popq	%rbx
	.cfi_def_cfa_offset 24
	popq	%rbp
	.cfi_def_cfa_offset 16
	popq	%r12
	.cfi_def_cfa_offset 8
	ret
	.cfi_endproc
.LFE96:
	.size	sort_words, .-sort_words
	.section	.rodata.str1.8
	.align 8
.LC11:
	.string	"Error virtual memory exhausted"
	.text
	.globl	sort_words_2
	.type	sort_words_2, @function
sort_words_2:
.LFB97:
	.cfi_startproc
	pushq	%rbx
	.cfi_def_cfa_offset 16
	.cfi_offset 3, -16
	movl	indices_index(%rip), %ebx
	salq	$3, %rbx
	movq	%rbx, %rdi
	call	malloc
	testq	%rax, %rax
	sete	%cl
	testq	%rbx, %rbx
	setne	%dl
	testb	%dl, %cl
	je	.L218
	movl	$.LC11, %edi
	call	perror
	movl	$1, %edi
	call	exit
.L211:
	leal	1(%rdx), %r10d
	movl	%edx, %edx
	movl	hash_table_indices(,%rdx,4), %edx
	movq	global_hash_table(,%rdx,8), %rdx
	movq	all_file_bits+8(%rip), %rsi
	xorq	24(%rdx), %rsi
	movq	all_file_bits(%rip), %rcx
	xorq	16(%rdx), %rcx
	orq	%rcx, %rsi
	jne	.L204
	movl	%r9d, %ecx
	movq	%rdx, (%rax,%rcx,8)
	movl	%r9d, %edx
	jmp	.L223
.L210:
	movl	$0, %edi
	testl	%edx, %edx
	je	.L206
	leal	-1(%rdx), %edi
	shrl	%edi
	movl	%edi, %edi
	salq	$3, %rdi
.L206:
	movq	(%rax,%rdi), %rdi
	movq	%rdi, (%rcx)
	movl	$0, %ecx
	testl	%edx, %edx
	je	.L207
	leal	-1(%rdx), %ecx
	shrl	%ecx
	movl	%ecx, %ecx
	salq	$3, %rcx
.L207:
	movq	%rsi, (%rax,%rcx)
	testl	%edx, %edx
	je	.L223
	subl	$1, %edx
	shrl	%edx
.L223:
	movl	%edx, %ecx
	leaq	(%rax,%rcx,8), %rcx
	movq	(%rcx), %rsi
	movl	12(%rsi), %r8d
	movl	$0, %edi
	testl	%edx, %edx
	je	.L209
	leal	-1(%rdx), %edi
	shrl	%edi
	movl	%edi, %edi
	salq	$3, %rdi
.L209:
	movq	(%rax,%rdi), %rdi
	cmpl	12(%rdi), %r8d
	ja	.L210
	addl	$1, %r9d
.L204:
	movl	%r10d, %edx
	jmp	.L203
.L218:
	movl	$0, %edx
	movl	$0, %r9d
.L203:
	cmpl	indices_index(%rip), %edx
	jb	.L211
	leal	-1(%r9), %r10d
	jmp	.L224
.L217:
	movl	%r10d, %edx
	leaq	(%rax,%rdx,8), %rdx
	movq	(%rdx), %rcx
	movq	(%rax), %rsi
	movq	%rsi, (%rdx)
	movq	%rcx, (%rax)
	subl	$1, %r10d
	movl	$0, %ecx
	jmp	.L213
.L216:
	movl	%edx, %edi
	movq	(%rax,%rdi,8), %r11
	movl	%ecx, %edi
	leaq	(%rax,%rdi,8), %r8
	movq	(%r8), %rdi
	movl	12(%rdi), %ebx
	cmpl	%ebx, 12(%r11)
	cmovbe	%ecx, %edx
	addl	$2, %esi
	cmpl	%esi, %r10d
	jb	.L215
	movl	%esi, %r11d
	movq	(%rax,%r11,8), %rbx
	movl	%edx, %r11d
	movq	(%rax,%r11,8), %r11
	movl	12(%r11), %r11d
	cmpl	%r11d, 12(%rbx)
	cmova	%esi, %edx
.L215:
	cmpl	%edx, %ecx
	je	.L224
	movl	%edx, %ecx
	leaq	(%rax,%rcx,8), %rcx
	movq	(%rcx), %rsi
	movq	%rsi, (%r8)
	movq	%rdi, (%rcx)
	movl	%edx, %ecx
.L213:
	leal	(%rcx,%rcx), %esi
	leal	1(%rsi), %edx
	cmpl	%edx, %r10d
	jae	.L216
.L224:
	testl	%r10d, %r10d
	jne	.L217
	movl	%r9d, %edx
	popq	%rbx
	.cfi_def_cfa_offset 8
	ret
	.cfi_endproc
.LFE97:
	.size	sort_words_2, .-sort_words_2
	.section	.rodata.str1.1
.LC12:
	.string	"error opening file"
.LC13:
	.string	"error calling stat on file"
	.section	.rodata.str1.8
	.align 8
.LC14:
	.string	"Found empty file, %s, no words common to all files\n"
	.text
	.globl	setup_fileinfo
	.type	setup_fileinfo, @function
setup_fileinfo:
.LFB101:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	pushq	%rbx
	.cfi_def_cfa_offset 24
	.cfi_offset 3, -24
	subq	$8, %rsp
	.cfi_def_cfa_offset 32
	movq	%rdi, %rbp
	movl	$0, %esi
	movl	$0, %eax
	call	open
	movl	%eax, %ebx
	cmpl	$-1, %eax
	jne	.L227
	movl	$.LC12, %edi
	call	perror
	movl	$1, %edi
	call	exit
.L227:
	movl	$stat_buf.5635, %edx
	movl	%eax, %esi
	movl	$1, %edi
	call	__fxstat
	cmpl	$-1, %eax
	jne	.L228
	movl	$.LC13, %edi
	call	perror
	movl	$1, %edi
	call	exit
.L228:
	movq	stat_buf.5635+48(%rip), %rsi
	testq	%rsi, %rsi
	jne	.L229
	movq	%rbp, %rdx
	movl	$.LC14, %esi
	movq	stderr(%rip), %rdi
	movl	$0, %eax
	call	fprintf
	movl	$1, %edi
	call	exit
.L229:
	movl	fileinfo_mem_index(%rip), %ecx
	movl	%ecx, %eax
	leaq	(%rax,%rax,4), %rdx
	leaq	(%rax,%rdx,2), %r9
	salq	$3, %r9
	leaq	fileinfo_mem(%r9), %rdx
	addl	$1, %ecx
	movl	%ecx, fileinfo_mem_index(%rip)
	movl	next_file_id(%rip), %r8d
	leal	1(%r8), %eax
	movl	%eax, next_file_id(%rip)
	movl	$11, %ecx
	movl	$0, %eax
	movq	%rdx, %rdi
	rep stosq
	movq	%rsi, fileinfo_mem(%r9)
	movq	%rsi, fileinfo_mem+8(%r9)
	movl	%ebx, fileinfo_mem+16(%r9)
	movl	%r8d, fileinfo_mem+20(%r9)
	movq	%rdx, %rax
	addq	$8, %rsp
	.cfi_def_cfa_offset 24
	popq	%rbx
	.cfi_def_cfa_offset 16
	popq	%rbp
	.cfi_def_cfa_offset 8
	ret
	.cfi_endproc
.LFE101:
	.size	setup_fileinfo, .-setup_fileinfo
	.section	.rodata.str1.8
	.align 8
.LC15:
	.string	"Invalid value for fileinfo_queue_index\n"
	.text
	.globl	setup_thread_args
	.type	setup_thread_args, @function
setup_thread_args:
.LFB88:
	.cfi_startproc
	pushq	%rbx
	.cfi_def_cfa_offset 16
	.cfi_offset 3, -16
	movq	fileinfo_queue_index(%rip), %rax
	testq	%rax, %rax
	jns	.L232
	movq	stderr(%rip), %rcx
	movl	$39, %edx
	movl	$1, %esi
	movl	$.LC15, %edi
	call	fwrite
	movl	$1, %edi
	call	exit
.L232:
	movslq	%edi, %rbx
	movq	%rbx, %rcx
	salq	$13, %rcx
	movq	%rbx, %rdx
	salq	$17, %rdx
	leaq	thread_bufs(%rcx,%rdx), %rsi
	movq	fileinfo_queue(,%rax,8), %rdi
	call	setup_block
	movq	%rax, %rdx
	movq	%rax, info.5477(%rip)
	movl	20(%rax), %eax
	movl	%eax, thread_fileinfo_vals(,%rbx,8)
	movl	28(%rdx), %eax
	movl	%eax, thread_fileinfo_vals+4(,%rbx,8)
	movl	$1, %eax
	cmpq	$0, 8(%rdx)
	jne	.L233
	movq	fileinfo_queue_index(%rip), %rax
	testq	%rax, %rax
	jle	.L234
	subq	$1, %rax
	movq	%rax, fileinfo_queue_index(%rip)
	movl	$1, %eax
	jmp	.L233
.L234:
	movq	current_file(%rip), %rdx
	movl	$0, %eax
	cmpq	num_files(%rip), %rdx
	jae	.L233
	leaq	1(%rdx), %rax
	movq	%rax, current_file(%rip)
	movq	filenames(%rip), %rax
	movq	(%rax,%rdx,8), %rdi
	call	setup_fileinfo
	movq	%rax, info.5477(%rip)
	movq	fileinfo_queue_index(%rip), %rdx
	movq	%rax, fileinfo_queue(,%rdx,8)
	movl	$1, %eax
.L233:
	popq	%rbx
	.cfi_def_cfa_offset 8
	ret
	.cfi_endproc
.LFE88:
	.size	setup_thread_args, .-setup_thread_args
	.globl	refill_fileinfo_queue
	.type	refill_fileinfo_queue, @function
refill_fileinfo_queue:
.LFB89:
	.cfi_startproc
	movq	num_files(%rip), %rax
	cmpq	%rax, current_file(%rip)
	jb	.L247
	movq	fileinfo_queue_index(%rip), %rax
	movq	fileinfo_queue(,%rax,8), %rax
	cmpq	$0, 8(%rax)
	setne	%al
	movzbl	%al, %eax
	ret
.L242:
	.cfi_def_cfa_offset 16
	leaq	1(%rax), %rdx
	movq	%rdx, current_file(%rip)
	movq	filenames(%rip), %rdx
	movq	(%rdx,%rax,8), %rdi
	call	setup_fileinfo
	movq	%rax, info.5480(%rip)
	movq	fileinfo_queue_index(%rip), %rdx
	leaq	1(%rdx), %rcx
	movq	%rcx, fileinfo_queue_index(%rip)
	movq	%rax, fileinfo_queue(,%rdx,8)
	jmp	.L245
.L247:
	.cfi_def_cfa_offset 8
	subq	$8, %rsp
	.cfi_def_cfa_offset 16
.L245:
	movq	fileinfo_queue_index(%rip), %rdx
	cmpq	$3, %rdx
	jg	.L241
	movq	current_file(%rip), %rax
	cmpq	num_files(%rip), %rax
	jb	.L242
.L241:
	movl	$0, %eax
	testq	%rdx, %rdx
	je	.L240
	leaq	-1(%rdx), %rax
	movq	%rax, fileinfo_queue_index(%rip)
	movl	%edx, %eax
.L240:
	addq	$8, %rsp
	.cfi_def_cfa_offset 8
	ret
	.cfi_endproc
.LFE89:
	.size	refill_fileinfo_queue, .-refill_fileinfo_queue
	.section	.rodata.str1.1
.LC16:
	.string	"Futex failure"
	.section	.rodata.str1.8
	.align 8
.LC17:
	.string	"Invalid value in futex main_thread_wait\n"
	.align 8
.LC18:
	.string	"More threads waiting then exist, exiting\n"
	.text
	.globl	main_wait_loop
	.type	main_wait_loop, @function
main_wait_loop:
.LFB87:
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
	movl	$3, %ebx
	testl	%edi, %edi
	je	.L249
	movl	main_thread_wait(%rip), %eax
	cmpl	$1, %eax
	jg	.L254
.L256:
	movl	$main_thread_wait_lock, %ecx
	movl	$0, %edx
	movl	$0, %esi
	movl	$main_thread_wait, %edi
	call	futex_wait_locking
	cmpl	$-1, %eax
	jne	.L251
	movl	$.LC16, %edi
	call	my_perror
	movl	$1, %edi
	call	exit
.L251:
	cmpl	$-1, main_thread_wait(%rip)
	jge	.L254
	movq	stderr(%rip), %rcx
	movl	$40, %edx
	movl	$1, %esi
	movl	$.LC17, %edi
	call	fwrite
	movl	$1, %edi
	call	exit
.L255:
	lock subl	$1, main_thread_wait(%rip)
	cmpl	$4, main_thread_wait(%rip)
	jle	.L252
	movq	stderr(%rip), %rcx
	movl	$41, %edx
	movl	$1, %esi
	movl	$.LC18, %edi
	call	fwrite
	movl	$1, %edi
	call	exit
.L252:
	movl	$thread_queue_lock, %edi
	call	futex_spin_down
	movzbl	thread_queue_index(%rip), %eax
	subl	$1, %eax
	movb	%al, thread_queue_index(%rip)
	movzbl	%al, %eax
	movzbl	thread_queue(%rax), %ebx
	movl	$thread_queue_lock, %eax
#APP
# 301 "my_threads.c" 1
	lock xchgq %r12,(%rax)

# 0 "" 2
#NO_APP
	movzbl	%bl, %ebp
	movl	%ebp, %edi
	call	setup_thread_args
	testl	%eax, %eax
	jne	.L253
	movslq	%ebp, %rbp
	movl	thread_pids(,%rbp,4), %esi
	movl	$15, %edx
	movl	tgid(%rip), %edi
	call	tgkill
	movl	$2, %ebx
	jmp	.L249
.L253:
	movzbl	%bl, %ebx
	leaq	0(,%rbx,8), %rdi
	leaq	thread_futex_locks(%rdi), %rdx
	addq	$thread_futexes, %rdi
	movl	$10, %esi
	call	futex_wake_locking
	cmpl	$-1, %eax
	jne	.L263
	movl	$.LC16, %edi
	call	my_perror
	movl	$1, %edi
	call	exit
.L250:
.L254:
	movl	$1, %r12d
.L263:
	movl	main_thread_wait(%rip), %eax
	testl	%eax, %eax
	jg	.L255
	movl	$0, %eax
	call	refill_fileinfo_queue
	testl	%eax, %eax
	jne	.L256
	movl	$3, %ebx
.L249:
	cmpl	$0, main_thread_wait(%rip)
	je	.L257
	jmp	.L261
.L259:
	lock subl	$1, main_thread_wait(%rip)
	movl	$thread_queue_lock, %edi
	call	futex_spin_down
	movzbl	thread_queue_index(%rip), %eax
	subl	$1, %eax
	movb	%al, thread_queue_index(%rip)
	movzbl	%al, %eax
	movzbl	thread_queue(%rax), %eax
	movl	$thread_queue_lock, %edx
#APP
# 301 "my_threads.c" 1
	lock xchgq %rbp,(%rdx)

# 0 "" 2
#NO_APP
	movzbl	%al, %eax
	movl	thread_pids(,%rax,4), %esi
	movl	$15, %edx
	movl	tgid(%rip), %edi
	call	tgkill
#APP
# 299 "prog5.c" 1
	lock decq (%ebx)
# 0 "" 2
#NO_APP
	jmp	.L265
.L261:
	movl	$1, %ebp
.L265:
	movl	main_thread_wait(%rip), %eax
	testl	%eax, %eax
	jg	.L259
.L257:
	movl	$main_thread_wait_lock, %ecx
	movl	$0, %edx
	movl	$0, %esi
	movl	$main_thread_wait, %edi
	call	futex_wait_locking
	cmpl	$-1, %eax
	jne	.L261
	movl	$.LC16, %edi
	call	my_perror
	movl	$1, %edi
	call	exit
	.cfi_endproc
.LFE87:
	.size	main_wait_loop, .-main_wait_loop
	.globl	init_thread_filestart
	.type	init_thread_filestart, @function
init_thread_filestart:
.LFB102:
	.cfi_startproc
	pushq	%r14
	.cfi_def_cfa_offset 16
	.cfi_offset 14, -16
	pushq	%r13
	.cfi_def_cfa_offset 24
	.cfi_offset 13, -24
	pushq	%r12
	.cfi_def_cfa_offset 32
	.cfi_offset 12, -32
	pushq	%rbp
	.cfi_def_cfa_offset 40
	.cfi_offset 6, -40
	pushq	%rbx
	.cfi_def_cfa_offset 48
	.cfi_offset 3, -48
	movq	%rdi, %rbp
	movl	%esi, %r12d
	cmpq	$139263, 8(%rdi)
	ja	.L268
	movslq	%esi, %rax
	movq	%rax, %r14
	salq	$13, %r14
	salq	$17, %rax
	addq	%rax, %r14
	leaq	thread_bufs(%r14), %rbx
	movl	16(%rdi), %edi
	movl	$139264, %edx
	movq	%rbx, %rsi
	call	read
	movq	%rax, %r13
	cmpq	$-1, %rax
	jne	.L269
	movl	$.LC6, %edi
	call	perror
	movl	$1, %edi
	call	exit
.L269:
	movl	16(%rbp), %edi
	call	close
	cmpl	$-1, %eax
	jne	.L270
	movl	$.LC7, %edi
	call	perror
	movl	$1, %edi
	call	exit
.L270:
	leal	-1(%r13), %edx
	movl	%edx, %eax
	movzbl	thread_bufs(%r14,%rax), %eax
	cmpb	$0, eng_accept(%rax)
	je	.L275
	movl	%r13d, %r13d
	movb	$-1, (%rbx,%r13)
	jmp	.L272
.L274:
	movl	%eax, %edx
.L275:
	leal	-1(%rdx), %eax
	movl	%eax, %ecx
	movzbl	(%rbx,%rcx), %ecx
	cmpb	$0, eng_accept(%rcx)
	je	.L274
	movl	%edx, %edx
	movb	$-1, (%rbx,%rdx)
.L272:
	movl	20(%rbp), %eax
	movslq	%r12d, %r8
	movl	%eax, thread_fileinfo_vals(,%r8,8)
	movl	$0, thread_fileinfo_vals+4(,%r8,8)
	leaq	thread_pids(,%r8,4), %rdx
	leal	1(%r12), %esi
	sall	$21, %esi
	movslq	%esi, %rsi
	addq	$thread_stacks-1, %rsi
	movl	$thread_main, %ecx
	movl	$1388288, %edi
	call	my_clone
	movl	$0, %eax
	jmp	.L273
.L268:
	movslq	%esi, %rbx
	movq	%rbx, %rdx
	salq	$13, %rdx
	movq	%rbx, %rax
	salq	$17, %rax
	leaq	thread_bufs(%rdx,%rax), %rsi
	call	setup_block
	movq	%rax, %rbp
	movl	20(%rax), %eax
	movl	%eax, thread_fileinfo_vals(,%rbx,8)
	movl	$0, thread_fileinfo_vals+4(,%rbx,8)
	leaq	thread_pids(,%rbx,4), %rdx
	leal	1(%r12), %esi
	sall	$21, %esi
	movslq	%esi, %rsi
	addq	$thread_stacks-1, %rsi
	movq	%rbx, %r8
	movl	$thread_main, %ecx
	movl	$1388288, %edi
	call	my_clone
	movq	%rbp, %rax
.L273:
	popq	%rbx
	.cfi_def_cfa_offset 40
	popq	%rbp
	.cfi_def_cfa_offset 32
	popq	%r12
	.cfi_def_cfa_offset 24
	popq	%r13
	.cfi_def_cfa_offset 16
	popq	%r14
	.cfi_def_cfa_offset 8
	ret
	.cfi_endproc
.LFE102:
	.size	init_thread_filestart, .-init_thread_filestart
	.globl	init_thread
	.type	init_thread, @function
init_thread:
.LFB103:
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
	movl	%esi, %r12d
	movslq	%esi, %rbx
	movq	%rbx, %rdx
	salq	$13, %rdx
	movq	%rbx, %rax
	salq	$17, %rax
	leaq	thread_bufs(%rdx,%rax), %rsi
	call	setup_block
	movq	%rax, %rbp
	movl	20(%rax), %eax
	movl	%eax, thread_fileinfo_vals(,%rbx,8)
	movl	$0, thread_fileinfo_vals+4(,%rbx,8)
	leal	1(%r12), %esi
	sall	$21, %esi
	movslq	%esi, %rsi
	leaq	thread_pids(,%rbx,4), %rdx
	addq	$thread_stacks-1, %rsi
	movq	%rbx, %r8
	movl	$thread_main, %ecx
	movl	$1388288, %edi
	call	my_clone
	movq	8(%rbp), %rax
	testq	%rax, %rax
	movl	$0, %eax
	cmovne	%rbp, %rax
	popq	%rbx
	.cfi_def_cfa_offset 24
	popq	%rbp
	.cfi_def_cfa_offset 16
	popq	%r12
	.cfi_def_cfa_offset 8
	ret
	.cfi_endproc
.LFE103:
	.size	init_thread, .-init_thread
	.section	.rodata.str1.1
.LC19:
	.string	"st"
.LC20:
	.string	"nd"
.LC21:
	.string	"rd"
.LC22:
	.string	"th"
	.section	.rodata.str1.8
	.align 8
.LC23:
	.string	"The %d%s most common word was "
	.section	.rodata.str1.1
.LC24:
	.string	", with %d occurances\n"
	.text
	.globl	print_results
	.type	print_results, @function
print_results:
.LFB104:
	.cfi_startproc
	pushq	%r14
	.cfi_def_cfa_offset 16
	.cfi_offset 14, -16
	pushq	%r13
	.cfi_def_cfa_offset 24
	.cfi_offset 13, -24
	pushq	%r12
	.cfi_def_cfa_offset 32
	.cfi_offset 12, -32
	pushq	%rbp
	.cfi_def_cfa_offset 40
	.cfi_offset 6, -40
	pushq	%rbx
	.cfi_def_cfa_offset 48
	.cfi_offset 3, -48
	movq	%rdi, %r13
	movl	%esi, %r12d
	leal	-1(%rsi), %ebx
	leal	-30(%rsi), %ebp
	jmp	.L282
.L284:
	movl	%r12d, %esi
	subl	%ebx, %esi
	movl	$.LC19, %edx
	cmpl	$1, %esi
	je	.L283
	movl	$.LC20, %edx
	cmpl	$2, %esi
	je	.L283
	cmpl	$3, %esi
	movl	$.LC21, %eax
	movl	$.LC22, %edx
	cmove	%rax, %rdx
.L283:
	movl	$.LC23, %edi
	movl	$0, %eax
	call	printf
	movslq	%ebx, %rax
	leaq	0(%r13,%rax,8), %r14
	movq	(%r14), %rax
	movq	(%rax), %rdi
	movl	8(%rax), %esi
	movq	stdout(%rip), %rcx
	movl	$1, %edx
	call	fwrite
	movq	(%r14), %rax
	movl	12(%rax), %esi
	movl	$.LC24, %edi
	movl	$0, %eax
	call	printf
	subl	$1, %ebx
.L282:
	cmpl	%ebp, %ebx
	jae	.L284
	popq	%rbx
	.cfi_def_cfa_offset 40
	popq	%rbp
	.cfi_def_cfa_offset 32
	popq	%r12
	.cfi_def_cfa_offset 24
	popq	%r13
	.cfi_def_cfa_offset 16
	popq	%r14
	.cfi_def_cfa_offset 8
	ret
	.cfi_endproc
.LFE104:
	.size	print_results, .-print_results
	.section	.rodata.str1.1
.LC25:
	.string	"Error no filenames given\n"
	.text
	.globl	main
	.type	main, @function
main:
.LFB105:
	.cfi_startproc
	pushq	%rbx
	.cfi_def_cfa_offset 16
	.cfi_offset 3, -16
	subl	$1, %edi
	movslq	%edi, %rdi
	movq	%rdi, num_files(%rip)
	addq	$8, %rsi
	movq	%rsi, filenames(%rip)
	testq	%rdi, %rdi
	jne	.L290
	movq	stderr(%rip), %rcx
	movl	$25, %edx
	movl	$1, %esi
	movl	$.LC25, %edi
	call	fwrite
	movl	$1, %edi
	call	exit
.L290:
#APP
# 583 "prog5.c" 1
	movq $39,%rax
syscall
# 0 "" 2
#NO_APP
	movl	%eax, tgid(%rip)
	movl	$0, %r9d
	movl	$-1, %r8d
	movl	$34, %ecx
	movl	$3, %edx
	movl	$8388608, %esi
	movl	$0, %edi
	call	mmap
	movq	%rax, string_mem_pointer(%rip)
	addq	$8388608, %rax
	movq	%rax, string_mem_end(%rip)
	movl	$0, %edx
	movl	$signal_action, %esi
	movl	$15, %edi
	call	sigaction
	movq	num_files(%rip), %rcx
	cmpq	$1, %rcx
	jne	.L291
	movq	$1, current_file(%rip)
	movq	$1, all_file_bits(%rip)
	movq	filenames(%rip), %rax
	movq	(%rax), %rdi
	call	setup_fileinfo
	movl	$1, %ebx
	jmp	.L292
.L294:
	leal	-1(%rbx), %esi
	movq	%rax, %rdi
	call	init_thread
	testq	%rax, %rax
	je	.L293
	addl	$1, %ebx
.L292:
	cmpl	$3, %ebx
	jle	.L294
.L293:
	testq	%rax, %rax
	je	.L305
	movq	fileinfo_queue_index(%rip), %rcx
	leaq	1(%rcx), %rdx
	movq	%rdx, fileinfo_queue_index(%rip)
	movq	%rax, fileinfo_queue(,%rdx,8)
	movl	$1, %edi
	jmp	.L295
.L291:
	movq	%rcx, %rax
	salq	$4, %rax
	movq	file_bit_strings-8(%rax), %rdx
	movq	file_bit_strings-16(%rax), %rax
	movq	%rax, all_file_bits(%rip)
	movq	%rdx, all_file_bits+8(%rip)
	movl	$1, %ebx
	cmpq	$2, %rcx
	jbe	.L297
	jmp	.L306
.L299:
	movq	current_file(%rip), %rax
	leaq	1(%rax), %rdx
	movq	%rdx, current_file(%rip)
	movq	filenames(%rip), %rdx
	movq	(%rdx,%rax,8), %rdi
	call	setup_fileinfo
	leal	-1(%rbx), %esi
	movq	%rax, %rdi
	call	init_thread_filestart
	testq	%rax, %rax
	je	.L298
	movq	fileinfo_queue_index(%rip), %rcx
	leaq	1(%rcx), %rdx
	movq	%rdx, fileinfo_queue_index(%rip)
	movq	%rax, fileinfo_queue(,%rdx,8)
.L298:
	addl	$1, %ebx
	jmp	.L296
.L306:
	movl	$1, %ebx
.L296:
	cmpl	$3, %ebx
	jle	.L299
	movq	fileinfo_queue_index(%rip), %rax
	movl	$1, %edi
	cmpq	$-1, %rax
	jne	.L295
	movq	current_file(%rip), %rdx
	movb	$0, %dil
	cmpq	num_files(%rip), %rdx
	jae	.L295
	leaq	1(%rax), %rbx
	movq	%rbx, fileinfo_queue_index(%rip)
	leaq	1(%rdx), %rax
	movq	%rax, current_file(%rip)
	movq	filenames(%rip), %rax
	movq	(%rax,%rdx,8), %rdi
	call	setup_fileinfo
	movq	%rax, fileinfo_queue(,%rbx,8)
	movl	$1, %edi
	jmp	.L295
.L301:
	leaq	1(%rax), %rdx
	movq	%rdx, current_file(%rip)
	movq	filenames(%rip), %rdx
	movq	(%rdx,%rax,8), %rdi
	call	setup_fileinfo
	leal	-1(%rbx), %esi
	movq	%rax, %rdi
	call	init_thread_filestart
	testq	%rax, %rax
	je	.L300
	movq	fileinfo_queue_index(%rip), %rcx
	leaq	1(%rcx), %rdx
	movq	%rdx, fileinfo_queue_index(%rip)
	movq	%rax, fileinfo_queue(,%rdx,8)
.L300:
	addl	$1, %ebx
.L297:
	movq	current_file(%rip), %rax
	cmpq	num_files(%rip), %rax
	jb	.L301
	cmpq	$0, fileinfo_queue_index(%rip)
	js	.L302
.L310:
	movq	fileinfo_queue_index(%rip), %rax
	movq	fileinfo_queue(,%rax,8), %rdi
	leal	-1(%rbx), %esi
	call	init_thread
	testq	%rax, %rax
	jne	.L303
	subq	$1, fileinfo_queue_index(%rip)
.L303:
	addl	$1, %ebx
	cmpl	$3, %ebx
	jg	.L302
	cmpq	$0, fileinfo_queue_index(%rip)
	jns	.L310
.L302:
	movq	fileinfo_queue_index(%rip), %rdi
	notq	%rdi
	shrq	$63, %rdi
	jmp	.L295
.L305:
	movl	$0, %edi
.L295:
	call	main_wait_loop
	.cfi_endproc
.LFE105:
	.size	main, .-main
	.local	stat_buf.5635
	.comm	stat_buf.5635,144,32
	.local	most_common.5582
	.comm	most_common.5582,256,32
	.local	info.5480
	.comm	info.5480,8,8
	.local	info.5477
	.comm	info.5477,8,8
	.local	sleep_time.5170
	.comm	sleep_time.5170,16,16
	.globl	signal_action
	.section	.rodata
	.align 32
	.type	signal_action, @object
	.size	signal_action, 152
signal_action:
	.quad	terminate_gracefully
	.zero	144
	.align 32
	.type	file_bit_masks, @object
	.size	file_bit_masks, 2048
file_bit_masks:
	.quad	1
	.zero	8
	.quad	2
	.zero	8
	.quad	4
	.zero	8
	.quad	8
	.zero	8
	.quad	16
	.zero	8
	.quad	32
	.zero	8
	.quad	64
	.zero	8
	.quad	128
	.zero	8
	.quad	256
	.zero	8
	.quad	512
	.zero	8
	.quad	1024
	.zero	8
	.quad	2048
	.zero	8
	.quad	4096
	.zero	8
	.quad	8192
	.zero	8
	.quad	16384
	.zero	8
	.quad	32768
	.zero	8
	.quad	65536
	.zero	8
	.quad	131072
	.zero	8
	.quad	262144
	.zero	8
	.quad	524288
	.zero	8
	.quad	1048576
	.zero	8
	.quad	2097152
	.zero	8
	.quad	4194304
	.zero	8
	.quad	8388608
	.zero	8
	.quad	16777216
	.zero	8
	.quad	33554432
	.zero	8
	.quad	67108864
	.zero	8
	.quad	134217728
	.zero	8
	.quad	268435456
	.zero	8
	.quad	536870912
	.zero	8
	.quad	1073741824
	.zero	8
	.quad	2147483648
	.zero	8
	.quad	4294967296
	.zero	8
	.quad	8589934592
	.zero	8
	.quad	17179869184
	.zero	8
	.quad	34359738368
	.zero	8
	.quad	68719476736
	.zero	8
	.quad	137438953472
	.zero	8
	.quad	274877906944
	.zero	8
	.quad	549755813888
	.zero	8
	.quad	1099511627776
	.zero	8
	.quad	2199023255552
	.zero	8
	.quad	4398046511104
	.zero	8
	.quad	8796093022208
	.zero	8
	.quad	17592186044416
	.zero	8
	.quad	35184372088832
	.zero	8
	.quad	70368744177664
	.zero	8
	.quad	140737488355328
	.zero	8
	.quad	281474976710656
	.zero	8
	.quad	562949953421312
	.zero	8
	.quad	1125899906842624
	.zero	8
	.quad	2251799813685248
	.zero	8
	.quad	4503599627370496
	.zero	8
	.quad	9007199254740992
	.zero	8
	.quad	18014398509481984
	.zero	8
	.quad	36028797018963968
	.zero	8
	.quad	72057594037927936
	.zero	8
	.quad	144115188075855872
	.zero	8
	.quad	288230376151711744
	.zero	8
	.quad	576460752303423488
	.zero	8
	.quad	1152921504606846976
	.zero	8
	.quad	2305843009213693952
	.zero	8
	.quad	4611686018427387904
	.zero	8
	.quad	-9223372036854775808
	.zero	8
	.zero	8
	.quad	1
	.zero	8
	.quad	2
	.zero	8
	.quad	4
	.zero	8
	.quad	8
	.zero	8
	.quad	16
	.zero	8
	.quad	32
	.zero	8
	.quad	64
	.zero	8
	.quad	128
	.zero	8
	.quad	256
	.zero	8
	.quad	512
	.zero	8
	.quad	1024
	.zero	8
	.quad	2048
	.zero	8
	.quad	4096
	.zero	8
	.quad	8192
	.zero	8
	.quad	16384
	.zero	8
	.quad	32768
	.zero	8
	.quad	65536
	.zero	8
	.quad	131072
	.zero	8
	.quad	262144
	.zero	8
	.quad	524288
	.zero	8
	.quad	1048576
	.zero	8
	.quad	2097152
	.zero	8
	.quad	4194304
	.zero	8
	.quad	8388608
	.zero	8
	.quad	16777216
	.zero	8
	.quad	33554432
	.zero	8
	.quad	67108864
	.zero	8
	.quad	134217728
	.zero	8
	.quad	268435456
	.zero	8
	.quad	536870912
	.zero	8
	.quad	1073741824
	.zero	8
	.quad	2147483648
	.zero	8
	.quad	4294967296
	.zero	8
	.quad	8589934592
	.zero	8
	.quad	17179869184
	.zero	8
	.quad	34359738368
	.zero	8
	.quad	68719476736
	.zero	8
	.quad	137438953472
	.zero	8
	.quad	274877906944
	.zero	8
	.quad	549755813888
	.zero	8
	.quad	1099511627776
	.zero	8
	.quad	2199023255552
	.zero	8
	.quad	4398046511104
	.zero	8
	.quad	8796093022208
	.zero	8
	.quad	17592186044416
	.zero	8
	.quad	35184372088832
	.zero	8
	.quad	70368744177664
	.zero	8
	.quad	140737488355328
	.zero	8
	.quad	281474976710656
	.zero	8
	.quad	562949953421312
	.zero	8
	.quad	1125899906842624
	.zero	8
	.quad	2251799813685248
	.zero	8
	.quad	4503599627370496
	.zero	8
	.quad	9007199254740992
	.zero	8
	.quad	18014398509481984
	.zero	8
	.quad	36028797018963968
	.zero	8
	.quad	72057594037927936
	.zero	8
	.quad	144115188075855872
	.zero	8
	.quad	288230376151711744
	.zero	8
	.quad	576460752303423488
	.zero	8
	.quad	1152921504606846976
	.zero	8
	.quad	2305843009213693952
	.zero	8
	.quad	4611686018427387904
	.zero	8
	.quad	-9223372036854775808
	.align 32
	.type	file_bit_strings, @object
	.size	file_bit_strings, 2048
file_bit_strings:
	.quad	1
	.zero	8
	.quad	3
	.zero	8
	.quad	7
	.zero	8
	.quad	15
	.zero	8
	.quad	31
	.zero	8
	.quad	63
	.zero	8
	.quad	127
	.zero	8
	.quad	255
	.zero	8
	.quad	511
	.zero	8
	.quad	1023
	.zero	8
	.quad	2047
	.zero	8
	.quad	4095
	.zero	8
	.quad	8191
	.zero	8
	.quad	16383
	.zero	8
	.quad	32767
	.zero	8
	.quad	65535
	.zero	8
	.quad	131071
	.zero	8
	.quad	262143
	.zero	8
	.quad	524287
	.zero	8
	.quad	1048575
	.zero	8
	.quad	2097151
	.zero	8
	.quad	4194303
	.zero	8
	.quad	8388607
	.zero	8
	.quad	16777215
	.zero	8
	.quad	33554431
	.zero	8
	.quad	67108863
	.zero	8
	.quad	134217727
	.zero	8
	.quad	268435455
	.zero	8
	.quad	536870911
	.zero	8
	.quad	1073741823
	.zero	8
	.quad	2147483647
	.zero	8
	.quad	4294967295
	.zero	8
	.quad	8589934591
	.zero	8
	.quad	17179869183
	.zero	8
	.quad	34359738367
	.zero	8
	.quad	68719476735
	.zero	8
	.quad	137438953471
	.zero	8
	.quad	274877906943
	.zero	8
	.quad	549755813887
	.zero	8
	.quad	1099511627775
	.zero	8
	.quad	2199023255551
	.zero	8
	.quad	4398046511103
	.zero	8
	.quad	8796093022207
	.zero	8
	.quad	17592186044415
	.zero	8
	.quad	35184372088831
	.zero	8
	.quad	70368744177663
	.zero	8
	.quad	140737488355327
	.zero	8
	.quad	281474976710655
	.zero	8
	.quad	562949953421311
	.zero	8
	.quad	1125899906842623
	.zero	8
	.quad	2251799813685247
	.zero	8
	.quad	4503599627370495
	.zero	8
	.quad	9007199254740991
	.zero	8
	.quad	18014398509481983
	.zero	8
	.quad	36028797018963967
	.zero	8
	.quad	72057594037927935
	.zero	8
	.quad	-1
	.quad	1
	.quad	-1
	.quad	3
	.quad	-1
	.quad	7
	.quad	-1
	.quad	15
	.quad	-1
	.quad	31
	.quad	-1
	.quad	63
	.quad	-1
	.quad	127
	.quad	-1
	.quad	255
	.quad	-1
	.quad	511
	.quad	-1
	.quad	1023
	.quad	-1
	.quad	2047
	.quad	-1
	.quad	4095
	.quad	-1
	.quad	8191
	.quad	-1
	.quad	16383
	.quad	-1
	.quad	32767
	.quad	-1
	.quad	65535
	.quad	-1
	.quad	131071
	.quad	-1
	.quad	262143
	.quad	-1
	.quad	524287
	.quad	-1
	.quad	1048575
	.quad	-1
	.quad	2097151
	.quad	-1
	.quad	4194303
	.quad	-1
	.quad	8388607
	.quad	-1
	.quad	16777215
	.quad	-1
	.quad	33554431
	.quad	-1
	.quad	67108863
	.quad	-1
	.quad	134217727
	.quad	-1
	.quad	268435455
	.quad	-1
	.quad	536870911
	.quad	-1
	.quad	1073741823
	.quad	-1
	.quad	2147483647
	.quad	-1
	.quad	4294967295
	.quad	-1
	.quad	8589934591
	.quad	-1
	.quad	17179869183
	.quad	-1
	.quad	34359738367
	.quad	-1
	.quad	68719476735
	.quad	-1
	.quad	137438953471
	.quad	-1
	.quad	274877906943
	.quad	-1
	.quad	549755813887
	.quad	-1
	.quad	1099511627775
	.quad	-1
	.quad	2199023255551
	.quad	-1
	.quad	4398046511103
	.quad	-1
	.quad	8796093022207
	.quad	-1
	.quad	17592186044415
	.quad	-1
	.quad	35184372088831
	.quad	-1
	.quad	70368744177663
	.quad	-1
	.quad	140737488355327
	.quad	-1
	.quad	281474976710655
	.quad	-1
	.quad	562949953421311
	.quad	-1
	.quad	1125899906842623
	.quad	-1
	.quad	2251799813685247
	.quad	-1
	.quad	4503599627370495
	.quad	-1
	.quad	9007199254740991
	.quad	-1
	.quad	18014398509481983
	.quad	-1
	.quad	36028797018963967
	.quad	-1
	.quad	72057594037927935
	.zero	256
	.align 32
	.type	eng_accept, @object
	.size	eng_accept, 256
eng_accept:
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	1
	.byte	1
	.byte	1
	.byte	1
	.byte	1
	.byte	1
	.byte	1
	.byte	1
	.byte	1
	.byte	1
	.byte	1
	.byte	1
	.byte	1
	.byte	1
	.byte	1
	.byte	1
	.byte	1
	.byte	1
	.byte	1
	.byte	1
	.byte	1
	.byte	1
	.byte	1
	.byte	1
	.byte	1
	.byte	1
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	1
	.byte	1
	.byte	1
	.byte	1
	.byte	1
	.byte	1
	.byte	1
	.byte	1
	.byte	1
	.byte	1
	.byte	1
	.byte	1
	.byte	1
	.byte	1
	.byte	1
	.byte	1
	.byte	1
	.byte	1
	.byte	1
	.byte	1
	.byte	1
	.byte	1
	.byte	1
	.byte	1
	.byte	1
	.byte	1
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.data
	.align 8
	.type	border_word_mem_ptr, @object
	.size	border_word_mem_ptr, 8
border_word_mem_ptr:
	.quad	border_word_mem
	.local	border_word_mem
	.comm	border_word_mem,2048,32
	.local	fileinfo_mem_index
	.comm	fileinfo_mem_index,4,4
	.local	fileinfo_mem
	.comm	fileinfo_mem,8800,32
	.local	all_file_bits
	.comm	all_file_bits,16,16
	.align 4
	.type	next_file_id, @object
	.size	next_file_id, 4
next_file_id:
	.long	1
	.local	indices_index
	.comm	indices_index,4,4
	.local	hash_table_indices
	.comm	hash_table_indices,2097152,32
	.local	global_hash_table
	.comm	global_hash_table,8388608,32
	.comm	filenames,8,8
	.globl	string_mem_lock
	.align 16
	.type	string_mem_lock, @object
	.size	string_mem_lock, 4
string_mem_lock:
	.long	1
	.comm	string_mem_end,8,8
	.comm	string_mem_pointer,8,8
	.globl	current_file
	.bss
	.align 8
	.type	current_file, @object
	.size	current_file, 8
current_file:
	.zero	8
	.comm	num_files,8,8
	.comm	sigwait_set,128,32
	.globl	main_thread_wait_lock
	.data
	.align 16
	.type	main_thread_wait_lock, @object
	.size	main_thread_wait_lock, 4
main_thread_wait_lock:
	.long	1
	.globl	main_thread_wait
	.bss
	.align 16
	.type	main_thread_wait, @object
	.size	main_thread_wait, 4
main_thread_wait:
	.zero	4
	.data
	.align 16
	.type	thread_queue_lock, @object
	.size	thread_queue_lock, 4
thread_queue_lock:
	.long	1
	.align 8
	.type	fileinfo_queue_index, @object
	.size	fileinfo_queue_index, 8
fileinfo_queue_index:
	.quad	-1
	.comm	thread_fileinfo_vals,32,32
	.comm	thread_bufs,557056,32
	.globl	thread_futex_locks
	.align 16
	.type	thread_futex_locks, @object
	.size	thread_futex_locks, 32
thread_futex_locks:
	.quad	1
	.quad	1
	.quad	1
	.quad	1
	.comm	thread_futexes,32,16
	.local	fileinfo_queue
	.comm	fileinfo_queue,32,32
	.local	thread_queue_index
	.comm	thread_queue_index,1,1
	.local	thread_queue
	.comm	thread_queue,4,1
	.local	tgid
	.comm	tgid,4,4
	.local	thread_pids
	.comm	thread_pids,16,16
	.local	thread_stacks
	.comm	thread_stacks,8388608,32
	.ident	"GCC: (GNU) 4.8.2 20140206 (prerelease)"
	.section	.note.GNU-stack,"",@progbits
