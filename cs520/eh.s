/* C exception handler implemented in x86_64 assembly*/
/* exports 3 functions with C declarations as follows:
        int catchException(void); //enables an exception handler
        void cancelCatchException(void);//cancels current exception handler
        void throwException(int exception_number);
        //jumps to current exception handler
*/
/* Almost completely self contained with one exception,
   We call the c exit function rather than making the exit syscall
   because the assignment says to use the c exit function.
   if jumps to exit were replaced my jumps to my_exit it would
   be completely self contained


   I do all output using the write syscall, and I wrote a simple
   function to format a base-10 integer so I don't need to call printf

   I use a really simple memory allocator, I allocate 6 pages of memory
   int the bss section and maintain a pointer to that memory. To allocate
   memory I simply increment that pointer by the desired size and return
   the old value. To free memory I just decrement the pointer.
   I've called by memory functions malloc and free (they're local symbols)
   so it should be really easy to switch in the libc malloc and free
*/
/* A decent ammount of code was taken from setjmp.S for x86_64
   in glibc. meaning that this code is technically licensed under the
   gnu gpl v3*/
/* Quick note to self
   for some value x
   x:
        .quad <number>
   movq x,%rax moves the value of x (i.e the location of x) into rax
   movq x(%rip),%rax moves the value of <number> into rax
*/
/*Macros*/
/*begin global function*/
.macro ENTRY name
        .globl \name;
        .type \name,@function;
        .p2align 4
\name\():
        .cfi_startproc
.endm
/*begin local function*/
.macro LOCAL_ENTRY name
        .local \name;
        .type \name,@function;
        .p2align 4
\name\():
        .cfi_startproc
.endm
/*end any function*/
.macro END name
.cfi_endproc
.size \name, .-\name
.endm

/*declare global symbol, with optional size and type parameters */
.macro defglobal name size=8 type=@object
        .globl \name
        .align 8
        .type \name, \type
        .size \name, \size
.endm
/*declare local symbol, with optional size and type parameters */
.macro deflocal name size=8 type=@object
        .local \name
        .align 8
        .type \name, \type
        .size \name, \size
.endm

/*just copying what gcc does for strings*/
/*define a local variable containing the adress of the string <val>*/
/*The label used for the string itself is local to the macro*/
.altmacro
.macro defstring name val
.local \name
LOCAL str
/*read only data section, "aMS" means allocatable,mergable,
  and contains strings, @progbits indicates it contines data 
  and 1 indicates that each data element is one byte*/
        .section	.rodata.str,"aMS",@progbits,1
str:
        .string \val
        .data
        .align 8
        .type	\name, @object
        .size	\name, 8
\name\():
        .quad str
.endm

/*Allocate an array of quadwords*/
.macro defarray_sized name size init:vararg
        .data
        .align 8
        .type \name, @object
\name\():
        .irp elem \init
        \size \elem
        .endr
        .size \name, .-\name
.endm

.macro defarray name init:vararg
defarray_sized \name .quad \init
.endm

/*write string in %rdi to stderr*/
.macro write
        movq %rdi,%rsi /*move string to second arg*/
        /*inlined strlen*/
        movq $-1,%rcx
        xorq %rax,%rax
        cld
        repnz scasb
        movq $-2,%rdx
        subq %rcx,%rdx
        movq $2,%rdi /*stderr fileno*/
        movq $1,%rax /*syscall number for write*/
        syscall /*calls write(ind fd, const void* but,size_t count)*/
.endm

.macro L name
        \name\():
.endm

.macro gen_label
        .LG\@:
        .set .LLabel,.LG\@
.endm

/*Symbolic constansts for offsets of saved because,
  registers that's what you'd do in a higher level language*/
.LJB_RBX = 0
.LJB_RBP = 1
.LJB_R12 = 2
.LJB_R13 = 3
.LJB_R14 = 4
.LJB_R15 = 5
.LJB_RSP = 6
.LJB_RIP = 7
.LJB_NXT = 8
/* code*/
.text
/*Cancel exception handler*/
ENTRY cancelCatchException
        movq current_exception,%rax
        movq (.LJB_NXT*8)(%rax),%rcx
        testq %rax,%rcx
        je 1f
        callq free /*not the libc free*/
1:      
        movq %rcx,current_exception
        retq
END cancelCatchException
/*establish exception handler*/
ENTRY catchException
        movq $72,%rdi
        callq malloc /*NOTE: this isn't the libc malloc, see below*/
        movq %rax,%rdi

        /* Save registers.*/
        movq %rbx, (.LJB_RBX*8)(%rdi)
        movq %rbp, (.LJB_RBP*8)(%rdi)
        movq %r12, (.LJB_R12*8)(%rdi)
        movq %r13, (.LJB_R13*8)(%rdi)
        movq %r14, (.LJB_R14*8)(%rdi)
        movq %r15, (.LJB_R15*8)(%rdi)
        /*puts (%rsp)+8 into rdx*/
        lea 8(%rsp), %rdx	/* Save SP as it will be after we return.  */
        movq %rdx, (.LJB_RSP*8)(%rdi)
        movq (%rsp), %rax	/* Save adress we are returning to now.  */
        movq %rax, (.LJB_RIP*8)(%rdi)
        /*make new exception head of exception list*/
        movq current_exception,%rcx
        movq %rcx,(.LJB_NXT*8)(%rdi)
        movq %rdi,current_exception
        xorl %eax, %eax
        retq
END catchException
/* Throw exception
   void throwException(int exception_number)
   in essence calls longjmp(global_jmp_buf,exception_number)*/
ENTRY throwException
        /*restore last exception*/
        testq %rdi,%rdi
        jz zero_handler
        movq current_exception,%rsi /*load current offset address*/
        movq (.LJB_NXT*8)(%rsi),%rcx /*copy adress of previous exception handler*/
        movq %rcx,current_exception /*restore previous handler*/
        callq free

        /* Restore registers.  */
        movq (.LJB_RSP*8)(%rsi),%r8
        movq (.LJB_RBP*8)(%rsi),%r9
        movq (.LJB_RIP*8)(%rsi),%rdx
        /* We add unwind information here.*/
        .cfi_def_cfa %rsi,0
        .cfi_register %rsp,%r8
        .cfi_register %rbp,%r9
        .cfi_register %rip,%rdx
        .cfi_offset %rbx,.LJB_RBX*8
        .cfi_offset %r12,.LJB_R12*8
        .cfi_offset %r13,.LJB_R13*8
        .cfi_offset %r14,.LJB_R14*8
        .cfi_offset %r15,.LJB_R15*8
        movq (.LJB_RBX*8)(%rsi),%rbx
        movq (.LJB_R12*8)(%rsi),%r12
        movq (.LJB_R13*8)(%rsi),%r13
        movq (.LJB_R14*8)(%rsi),%r14
        movq (.LJB_R15*8)(%rsi),%r15
        /* Set return value for catchException.  */
        mov %edi, %eax
        mov %r8,%rsp
        movq %r9,%rbp
        jmpq *%rdx /*jump to the absolute adress in rdx*/
END throwException

/*Handle error caused by throwing an exception with no handler defined*/
LOCAL_ENTRY base_handler
        movq %rsp,%rbp /*setup temp stack*/
        movq %rdi,%rbx
        movq no_exception_start(%rip), %rdi
        write

        movq %rbx,%rdi
        callq itoa_10
        movq %rax,%rsi
        movq $2,%rdi /*stderr fileno*/
        movq $1,%rax /*syscall number for write*/
        syscall

        movq no_exception_end(%rip), %rdi
        write

        movq %rbx,%rdi
        jmp exit
END base_handler
/*Handle error caused by calling throwException with a 0 argument*/
LOCAL_ENTRY zero_handler
        movq zero_exception(%rip),%rdi
        write
        movq $-1,%rdi
        /* Check %rax for error. (don't even ask about the number)*/
        cmpq $-4095, %rax
        /*if we get a syscall error return that as the program exit value*/
        cmovae %rax,%rdi
        jmp exit
END zero_handler

/*Allocate memory*/
LOCAL_ENTRY malloc
        movq mempointer,%rax
        addq %rdi,mempointer
        retq
END malloc
        
LOCAL_ENTRY free
        subq $72,mempointer
        retq
END free

/*really naive strlen, a better version (i.e the glibc version)
  would use simd instructions*/
LOCAL_ENTRY strlen
        movq $-1,%rcx /*set rcx (the count register) to -1*/
        xorq %rax,%rax /*set %al to 0 (we're searching for a zero byte*/
        cld /*clear direction flags, just makes it so we search forward*/
        repnz scasb /*compare %rdi to %al until we find a zero byte*/
        subq $-2,%rcx /*we started with -1 in rcx, and decremented by 1 for each
                        iteration, so this puts the length in rcx*/
        movq %rcx,%rax
        retq
END strlen

/*convert integer into base 10 string,
  returns a pointer to the string in rax and the string length in rdx*/
LOCAL_ENTRY itoa_10
        xorq %rcx,%rcx
        movl %edi, %eax
        movl $10, %esi
gen_label
        cltd /*sign extend %eax into %edx:%eax*/
        idivl %esi
        movzbl digits(%edx), %edx /* load character into edx */
        movb %dl,itoa_mem_end(%rcx)
        decq %rcx
        testl %eax,%eax
        jnz .LLabel
        leaq itoa_mem_end(%rcx) ,%rax
        incq %rax
        xorq %rdx,%rdx
        subq %rcx,%rdx
        retq
END itoa_10

/*invoke the exit syscall*/
LOCAL_ENTRY my_exit
        movq $60,%rax
        syscall
END my_exit
/*data*/
.data
/*strings go in the .rodata.str section*/
defstring no_exception "Error exception with number %d thrown, but no exception handler present\n"
defstring no_exception_start "Error exception with number "
defstring no_exception_end " thrown, but no exception handler present\n"
defstring zero_exception "Error 0 passed as argument to throwException\n"
/*Instead of using a fixed stack of exception handlers we use a linked list.
  We take advantage of a couple aspects of linked lists to make error handling
  and canceling exceptions a bit eaiser.
  First of all there is always an exception handler in place, as a protection
  mechanism. The destination of the base handler is a function to print
  an error message and terminate (this should only happen if throwException
  is called with no other handlers avaliable). The location of the next link
  is a pointer to the current link, thus it's a circular list.
  The way canceling exceptions works is to set the current exception to the
  next one in the list, so if no exception is present this just sets it to
  itself.
*/
deflocal current_exception
current_exception:
        .quad base_exception

.local base_exception
defarray base_exception 0, temp_stack+4096*3, 0, 0, 0, 0, temp_stack+4096*3, base_handler, base_exception

deflocal mempointer
mempointer:
        .quad temp_stack
/* Started as a massive hack to deal with losing the stack pointer 
   if we jump to an error function (that's still a massive hack...)
   but now I use this as memory
*/
deflocal temp_stack
        .bss /*put large (uninitialized) objects in the bss section*/
        .align 32
        .type	temp_stack, @object
        .size	temp_stack, 4096*8
temp_stack:
        .zero 4096*8
/*hold the string for itoa, it makes things eaiser because
  we can start at the end and fill in the string in decending order
  which means the result will be in the correct order

  the way converting an int to a string works is basically you have
  an array containing the characters corrsponting to the digits in
  a parcitular base then you run the loop:
    char c = number % base;
    *result_string--=c
    while (number/=base)
  assuming result string starts as a pointer to the end of the result string
*/
deflocal itoa_mem
deflocal itoa_mem_end
itoa_mem_end:
        .quad itoa_mem+128
        .bss
        .align 32
        .type itoa_mem, @object
        .size iota_mem, 128
itoa_mem:
        .zero 128
//get back to data section
//slashes are comments
	.data
deflocal digits
digits:
        .ascii "0123456789"
deflocal program_break
deflocal program_stack

/*
This might work for dynamic allocation but it would mess with the 
system malloc because it changes the program break
LOCAL_ENTRY malloc2
        pushq %rbx
        
        movq %rdi,%rbx
        movq program_break(%rip),%rdi
        testq %rdi,%rdi
        jeq 1f
        movq $12,%rax /*brk syscall number/
        syscall
        movq %rax,mempointer /*initial break/
        pushq %rbp
        movq %rax,%rbp
        /*now allocate some memory/
        movq %rax,%rdi
        addq $4096,%rdi
        movq $12,%rax
        syscall
        /*update break (also set stack values in default handler)/
        movq %rax,program_break(%rip)
        movq %rsp,.LJB_RSP(base_exception)
        movq %rsp,.LJB_RBP(base_exception)
        
        /*increase mempointer by ammount of memory to allocate/
        addq %rbp,%rbx
        popq %rbp
        jmp 2f
1:      
        /*test if allocating memory goes beyond the program break/
        movq mempointer,%rax
        addq %rax,%rbx
        cmpq %rbx,program_break(%rip)
        jae 3f
2:      
        movq %rbx,%rdi
        popq %rbx
        retq
3:      
        movq $12,%rax
        movq program_break(%rip),%rdi
        addq $4096,%rdi
        syscall
        movq %rax,program_break(%rip)
END malloc
*/
