/*Constants*/
.LNR_WRITE = $1
.LNR_BRK = $12
.LNR_SIGACTION = $13
.LNR_CLONE = $56
.LNR_EXIT = $60
.LNR_WAIT = $61 /*really wait4, wait pid doesn't exist*/
.LNR_GETTID = $186
.LNR_TKILL = $200
.LNR_FUTEX = $202
.LFUTEX_WAIT = $0
.LFUTEX_WAKE = $1
.LFUTEX_FD = $2
.LFUTEX_REQUEUE = $3
.LFUTEX_CMP_REQUEUE = $4

.LCSIGNAL =       0x000000ff /* Signal mask to be sent at exit.  */
.LCLONE_VM =      0x00000100 /* Set if VM shared between processes.  */
.LCLONE_FS =      0x00000200 /* Set if fs info shared between processes.  */
.LCLONE_FILES =   0x00000400 /* Set if open files shared between processes.  */
.LCLONE_SIGHAND = 0x00000800 /* Set if signal handlers shared.  */
.LCLONE_PTRACE =  0x00002000 /* Set if tracing continues on the child.  */
.LCLONE_VFORK =   0x00004000 /* Set if the parent wants the child to
				     wake it up on mm_release.  */
.LCLONE_PARENT =  0x00008000 /* Set if we want to have the same
				     parent as the cloner.  */
.LCLONE_THREAD =  0x00010000 /* Set to add to same thread group.  */
.LCLONE_NEWNS =   0x00020000 /* Set to create new namespace.  */
.LCLONE_SYSVSEM = 0x00040000 /* Set to shared SVID SEM_UNDO semantics.  */
.LCLONE_SETTLS =  0x00080000 /* Set TLS info.  */
.LCLONE_PARENT_SETTID = 0x00100000 /* Store TID in userlevel buffer
					   before MM copy.  */
.LCLONE_CHILD_CLEARTID = 0x00200000 /* Register exit futex and memory
					    location to clear.  */
.LCLONE_DETACHED = 0x00400000 /* Create clone detached.  */
.LCLONE_UNTRACED = 0x00800000 /* Set if the tracing process can't
				      force CLONE_PTRACE on this clone.  */
.LCLONE_CHILD_SETTID = 0x01000000 /* Store TID in userlevel buffer in
					  the child.  */
.Lpthread_clone_flags = (.LCLONE_VM | .LCLONE_FS | .LCLONE_FILES | .LCLONE_SIGHAND
		     | .LCLONE_SETTLS | .LCLONE_PARENT_SETTID
		     | .LCLONE_CHILD_CLEARTID | .LCLONE_SYSVSEM
		     | 0)
.LCLONE_FLAGNS = (.LCLONE_VM | .LCLONE_FS | .LCLONE_FILES | .LCLONE_SIGHAND
                  | .LCLONE_PARENT_SETTID)
#endif
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
        movq .LNR_WRITE,%rax /*syscall number for write*/
        syscall /*calls write(ind fd, const void* but,size_t count)*/
.endm

/*simple wrapper for the exit syscall*/
/*if error checking is needed compare rax with -4095, */
/*if above or equal there's an error*/
.macro my_exit
        movq .LNR_EXIT,%rax
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

/*perform a conditional jump over one instruction using a temporary label*/
.macro jump_over jmp_op:req instr:req
        \jmp_op .L_G\@
        \instr
        .L_G\@:.endm
/*perform a conditional jump if not equal over one instruction using a temporary label*/
.macro jne_over instr:req
        jump_over jne \instr
.endm
/*perform a conditional jump if equal over one instruction using a temporary label*/
.macro jeq_over instr:req
        jump_over jeq \instr
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

/*int futex(int *uaddr, int op, int val, const struct timespec *timeout,
        int *uaddr2, int val3);*/

/*Effectively this unlocks a futex*/
ENTRY futex_up
        lock addq $1,(%rdi)
        testq $1,(%rdi)
        /*assume most common case is the uncontested case*/
        jne_over retq
        /*if we get here it's contested*/
        movq $1,(%rdi)
        movq .LFUTEX_WAKE,%rsi
        movq $1,%rdx
        my_syscall .LNR_FUTEX
        retq
END futex_up

/*Effectively this locks a futex*/
ENTRY futex_down
        lock subq $1,(%rdi)
        cmpq $0,%rdi
        /*if non contendend rdi is 0*/
        jne_over retq
        /*again if we get here it's contended*/
        movq $-1,(%rdi)
        movq $-1,%rdx
        movq .LFUTEX_WAIT,%rsi
        /*fourth argument is a timer, for now just wait forever*/
        xorq %rcx,%rcx
        my_syscall .LNR_FUTEX
        renq
END futex_down

ENTRY run_with_futex /*uint64_t* futex,(void*)(*f)(void*),void* arg*/
        pushq %rbx /*used to store futex addr*/
        /*will get clobbered by call to futex_down*/
        /*I would move them to r8,r9, but I don't know if the syscall*/
        /*clobbers them*/
        pushq %rdx 
        pushq %rsi
        movq %rdi,%rbx /*save futex location in a callee saved register*/
        callq futex_down /*lock futex*/
        popq %rcx /*function to call*/
        popq %rdi /*argument*/
        callq *%rcx /*call function*/
        movq %rbx,%rdi /*restore futex addr*/
        callq futex_up /*unlock futex*/
        popq %rbx /*restore rbx*/
        retq
END run_with_futex

/*try to lock futex, don't wait if we fail to lock it */
ENTRY futex_trylock
        xorq %rsi,%rsi
        movq $1,%rax
        lock cmpxchgq %rsi,(%rdi)
        retq /*if succssful rax will be 1*/
END futex_trylock


/*Just a quick syscall wrapper*/
ENTRY clone
        my_syscall .LNR_CLONE
        retq
END clone


ENTRY write /*fd,string,len*/
        movq .LNR_WRITE,%rax /*syscall number for write*/
        syscall /*calls write(int fd, const void* but,size_t count)*/
END write

ENTRY brk
        movq .LNR_BRK,%rax
        syscall
END brk

ENTRY syscall_1
        movq %rdi,%rax
        movq %rsi,%rdi
        syscall
        retq
END syscall_1

ENTRY syscall_2
        movq %rdi,%rax
        movq %rsi,%rdi
        movq %rdi,%rsi
        syscall
        retq
END syscall_2

ENTRY syscall_3
        movq %rdi,%rax
        movq %rsi,%rdi
        movq %rdx,%rsi
        movq %rcx,%rdx
        syscall
        retq
END syscall_3

ENTRY syscall_4
        movq %rdi,%rax
        movq %rsi,%rdi
        movq %rdx,%rsi
        movq %rcx,%rdx
/*umm this is what glibc does...*/
        movq %r8,%r10
        syscall
        retq
END syscall_4

ENTRY syscall_5
        movq %rdi,%rax
        movq %rsi,%rdi
        movq %rdx,%rsi
        movq %rcx,%rdx
/*umm this is what glibc does...*/
        movq %r8,%r10
        movq %r9,%r8
        syscall
        retq
END syscall_5

ENTRY syscall_6
        movq %rdi,%rax
        movq %rsi,%rdi
        movq %rdx,%rsi
        movq %rcx,%rdx
/*umm this is what glibc does...*/
        movq %r8,%r10
        movq %r9,%r8
        movq 8(%rsp),%r9
        syscall
        retq
END syscall_6

