/*Macros*/
/*begin global function*/

.macro ENTRY name:req
        .globl \name;
        .type \name,@function;
        .p2align 4
\name\():
        .cfi_startproc
.endm
/*begin local function*/
.macro LOCAL_ENTRY name:req
        .local \name;
        .type \name,@function;
        .p2align 4
\name\():
        .cfi_startproc
.endm
/*end any function*/
.macro END name:req
.cfi_endproc
.size \name, .-\name
.endm

.macro defvar name:req visibility:req size=8 align=8 type=@object
        \visibility \name
        .align 8
        .type \name, \type
        .size \name, \size
.endm
/*declare global symbol, with optional size and type parameters */
.macro defglobal name:req size=8 align=8 type=@object
defvar \name .globl \size \align \type
.endm
/*declare local symbol, with optional size and type parameters */
.macro deflocal name:req size=8 align=8 type=@object
defvar \name .local \size \align \type
.endm

/*just copying what gcc does for strings*/
/*define a variable containing the adress of the string <val>*/
/*The label used for the string itself is local to the macro*/
/*The string is placed in read only memory and the symbol is local by default*/
.altmacro
.macro defstring name:req val:req visibility=.local
\visiblity \name
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

/*write string in %rdi to fd, defaults to stderr*/
/*optional length argument, if not provided assume the string is*/
/*zero terminated and find the length*/
.macro write fd=2 len=-1
        movq %rdi,%rsi /*move string to second arg*/
        .iflt len
        /*inlined strlen*/
        movq $-1,%rcx
        xorq %rax,%rax
        cld
        repnz scasb
        movq $-2,%rdx
        .endif
        subq %rcx,%rdx
        movq $\fd,%rdi 
        movq $1,%rax /*syscall number for write*/
        syscall /*calls write(ind fd, const void* but,size_t count)*/
.endm

/*simple wrapper for the exit syscall*/
.macro my_exit
        movq $60,%rax
        syscall
.endm
/*just so syscall looks a bit more like a normal instruction*/
.macro my_syscall syscall_no:req
        movq \syscall_no,%rax
        syscall
.endm

/*create a label with the name name*/
.macro L name:req
        \name\():
.endm


/*generate a unique label of the name .L_G<N> where N is the number of macros 
  executed so far during assembly, the most recently defined label is stored
  in the symbol .LLabel, if a label is needed past the next call to gen label
  the value of .LLabel should be stored in another symbol*/
.macro gen_label
        .L_G\@:
        .set .LLabel,.L_G\@
.endm

/*copy (src) to (dest), opitonal temp is an intemediate register*/
/*if temp is not provided src is used to hold the temp result, and so is clobbered*/
.macro mem_mov instr:req src:req dest:req temp
        .ifb temp
        \instr (\src),\src
        \instr \src,(\dest)
        .else
        \instr (\src),\temp
        \instr \temp,(\dest)
        .endif
.endm

.macro mem_movl src:req dest:req temp
        mem_mov movl src dest temp
.endm
.macro mem_movq src:req dest:req temp
        mem_mov movq src dest temp
.endm

.macro defdata name:req size:req type=@object
        .bss
        .align 32
        .type \name,\type
        .size \name,\size
\name\():
        .zero \size
.endm 

.macro defarray_typed name elemsize init:vararg
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
defarray_typed \name .quad \init
.endm

.macro zeroq reg:req
        xorq \reg,\reg
.endm
.macro zerol reg:req
        xorl \reg,\reg
.endm
