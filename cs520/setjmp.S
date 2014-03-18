/* C exception handler implemented in x86_64 assembly*/
/* exports 3 functions with C declarations as follows:
        int catchException(void); //enables an exception handler
        void cancelCatchException(void);//cancels current exception handler
        void throwException(int exception_number);
        //jumps to current exception handler
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
.macro ENTRY name
        .globl \name;
        .type \name,@function;
        .p2align 4
\name\():
        .cfi_startproc
.endm

.macro LOCAL_ENTRY name
        .local \name;
        .type \name,@function;
        .p2align 4
\name\():
        .cfi_startproc
.endm

.macro END name
.cfi_endproc
.size \name, .-\name
.endm

.macro L name
\name\():
.endm

.macro defglobal name size=8 type=@object
        .globl \name
        .align 8
        .type \name, \type
        .size \name, \size
.endm

/*will still fail even if 0 is passed as size*/
.macro xmalloc sz
        movq \sz,%rdi
        callq malloc
        testq %rax,%rax
        movq $-1,%rdi
        je exit
.endm
        
/*just copying what gcc  does for strings*/
.altmacro
.macro defstring name val
.globl \name
LOCAL str
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
/*Symbolic constansts for offsets, because that's what you'd
  do in a higher level language*/        
.LJB_RBX=0
.LJB_RBP=1
.LJB_R12=2
.LJB_R13=3
.LJB_R14=4
.LJB_R15=5
.LJB_RSP=6
.LJB_RIP=7
.LJB_NXT=8
/* code*/
.text
/*Cancel exception handler*/
ENTRY cancelCatchException
        movq current_exception,%rax
        movq (.LJB_NXT*8)(%rax),%rcx
        movq %rcx,current_exception
        retq
END cancelCatchException
/*establish exception handler*/
ENTRY catchException
        movq $64,%rdi
        callq malloc
        movq %rax,%rdi
        
        /* Save registers.*/
        movq %rbx, (.LJB_RBX*8)(%rdi)
        movq %rbp, (.LJB_RBP*8)(%rdi)
        movq %r12, (.LJB_R12*8)(%rdi)
        movq %r13, (.LJB_R13*8)(%rdi)
        movq %r14, (.LJB_R14*8)(%rdi)
        movq %r15, (.LJB_R15*8)(%rdi)
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
/* Jump to the position specified by ENV, causing the
   setjmp call there to return VAL, or 1 if VAL is 0.
   void __longjmp (__jmp_buf env, int val).  */
/* void throwException(int exception_number)
   in essence calls longjmp(global_jmp_buf,exception_number)*/
ENTRY throwException
        /*restore last exception*/
        testq %rdi,%rdi
        jz zero_handler
        movq current_exception,%rsi /*load current offset address*/
        movq (.LJB_NXT*8)(%rsi),%rcx /*copy adress of previous exception handler*/
        movq %rcx,current_exception /*restore previous handler*/
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
        /* Set return value for setjmp.  */
        mov %edi, %eax
        mov %r8,%rsp
        movq %r9,%rbp
        jmpq *%rdx /*jump to the absolute adress in rdx*/
END throwException

LOCAL_ENTRY base_handler
        movq %rsp,%rbp
        pushq %rdi
        movq %rdi,%rdx
        movq no_exception(%rip), %rsi
        movq stderr(%rip), %rdi
        xorl %eax,%eax /*%al is the number of xmm args passed 
                         to a vaarg function, so we need to clear it */
        callq fprintf
        popq %rdi
        callq exit
END base_handler

LOCAL_ENTRY zero_handler
        movq zero_exception(%rip),%rsi
        movq stderr(%rip),%rdi
        xorl %eax,%eax
        callq fprintf
        movq $-1,%rdi
        callq exit
END zero_handler
        
/*data*/
.data
/*strings go in the .rodata.str section*/
defstring no_exception "Error exception with number %d thrown, but no exception handler present\n"
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
  itself. We leak memory though...I should fix that...I kinda forgot C normally
  isn't gc'd. Eh whatever it'll be fine for now.
*/
defglobal current_exception
current_exception:      
        .quad base_exception

defglobal base_exception 72
base_exception: 
	.quad	0 /*rbx*/
        .quad   temp_stack+4096*3 /*rbp*/
	.quad	0 /*r12-15*/
	.quad	0
	.quad	0
	.quad	0
        .quad   temp_stack+4096*3 /*rsp*/
	.quad	base_handler /*rip*/
	.quad	base_exception /*pointer to next exception*/
/*Only a tiny bit of a massive hack here, since we can't know the stack
  top at assemble time and I set rsp before I jump to my exception handler
  I setup a temporary stack ala sigaltstack to deal with stuff
*/
defglobal temp_stack\
        .bss
        .align 32
	.type	temp_stack, @object
	.size	temp_stack, 4096*6
temp_stack:     
        .zero 4096*6
/* I suppose we should probably use a stack...
	.globl	exception_stack
	.bss
	.align 32
	.type	exception_stack, @object
	.size	exception_stack, 3200
exception_stack:
	.zero	3200*/
