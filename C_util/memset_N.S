/*
    Assembly functions equivlent to memset but for 16, 32, and 64 bit values.

    Most of this is just a copy of the glibc asm code for memcopy, since
    it uses simd instructions to do most of the copying

    The code for this is relatively simple so I'm not using any assembler
    macros or any thing like that.
*/
#define ENTRY(name)             \
    .globl name;                \
    .type name,@function;       \
    .p2align 4                  \
name:                           \
    .cfi_startproc

#define END(name)       \
    .cfi_endproc        \
    .size name, .-name-
.intel_syntax;

/*
    Everything we need to do can be done with AVX, but if AVX2 is present
    integer movement instructions are used in favor of floating point
    movement instructions, since they may perform better.
*/
/*
    Idea, use a register to hold the size of whatever we're copying
    and use that to decide how to cleanup the extra stuff at the end
*/
/*
    Assume the memory we're given is aligned to the size of the value
    we're duplicating, but not more than that.
*/
ENTRY(memset_16):
    mov xmm0, rsi;
    mov rax, rdi;

    punpcklwd xmm0, xmm0;
    punpckldq xmm0, xmm0;

//For values smaller than this use a simplier algorithm, I'm not sure
//what the best number for this value is, so this is just a guess
    cmp rdx, 64; //128 bytes
    jbe .L128_or_less

    mov r8, 16 //save size of value we're duplicating

    mov rcx, 7
    and rcx, rdi //figure out how many bytes needed to align
    shr rcx, 1 //convert bytes to words

    sub rdx, rcx //update length
    mov r9, rdx //save length

    shr rdx, 2 //change length to 64 byte chunks (divide by 4)
.Lalign16:
    test rcx, rcx
    jz .Lmemset_64_main;
    mov [rdi], si
    sub rcx, 1
    add rdi, 2
    jmp .Lalign16

/*
    This should probably go away, in favor of a 64 bit equivlent
*/
.L128_or_less:
    shl rdx, 2 //convert length into bytes, it makes calculations eaiser
    cmp rdx, 64
    jb .Lless_than_64
    movdqu [rdi], xmm0
    movdqu [rdi + 16], xmm0
    movdqu [rdi + 32], xmm0
    movdqu [rdi + 48], xmm0
    add rdi, 64
    sub rdx, 64
/*
    The moves of the form mov [rdi - X + rdx], xmm0, might cause some
    words to be written twice, but they allow us to avoid some tests/branches
    that we would otherwise need to take
*/
.Lless_than_64:
    cmp rdx, 16
    jb .Lless_than_16
    movdqu [rdi], xmm0
    movdqu [rdi - 16 + rdx], xmm0
    cmp rdx, 32
    jbe .Lret
    movdqu [rdi + 16], xmm0
    movdqu [rdi - 32 + rdx], xmm0
.Lless_than_16:
    movq rcx, xmm0
    test rdx, 24
.L16_or_less:
.L8_16:
.L4_7:
.Lret:
    rep
    ret
END(memset_16)

ENTRY(memset_32):
    mov xmm0, rsi;
    mov rax, rdi;
    punpckldq xmm0, xmm0;
    cmp rdx, 32; //128 bytes
    jbe .Lless_than_32

    mov r8, 32 //save size of value we're duplicating
    mov r9, rdx

    test rdi, 
    
    mov rcx, 3
    and rcx, rdi //figure out how many words needed to align

    sub rdx, rcx //update length
    mov r9, rdx //save length
    shr rdx, 1 //change length to 64 byte chunks (divide by 2)

    test rdi, 4
    jz .Lmemset_64_main;
    
    mov [rdi], esi
    sub r9, 1
    add rdi, 4

    jmp .Lmemset_64_main
END(memset_32)

ENTRY(memset_64)
    mov xmm0, rsi; /* The quadword we're duplicating */
    mov rax, rdi; /* We return a pointer to the filled memory */

.Lmemset64_main:

    unpcklqdq xmm0, xmm0
#if (defined __AVX__) && (defined __AVX_2__) 
    vperm2I128 ymm0, ymm0, ymm0, 0;
#elif (defined __AVX__)
    vperm2F128 ymm0, ymm0, ymm0, 0;
#endif

    cmp rdx, 1024; //for less than 1k use unaligned writes
    jbe .Lmemset_64_small

    //Align to 256 bit boundry
    mov rcx, rdi
    and rcx, 3
    jz .Lalign64_after
    sub rdx, rcx
.Lalign64:
    movq [rdi], xmm0
    add rsi, 8

    sub rcx,1
    jnz .Lalign64

.Lalign64_after:
    mov rcx, rdx
    and rdx, 15//number of excess 64 bit chunks
    shr rcx, 4 //change lenght to 128 byte units
    //At the start of the loop we have at least 128 bytes to copy
.Lloop:
//I have no idea what the best distance for prefetching is, this is
//the value the memcpy implementation in glibc uses
//This seems to assume 64 bytes get fetched per call
    prefetcht0 [rdi + 768]
    prefetcht0 [rdi + 832]

    sub rcx, 1
#ifdef __AVX__
    vmovdqa [rdi], ymm0
    vmovdqa [rdi + 32], ymm0
    vmovdqa [rdi + 64], ymm0
    vmovdqa [rdi + 96], ymm0
#else
    movdqa [rdi], xmm0
    movdqa [rdi + 16], xmm0
    movdqa [rdi + 32], xmm0
    movdqa [rdi + 48], xmm0
    movdqa [rdi + 64], xmm0
    movdqa [rdi + 80], xmm0
    movdqa [rdi + 96], xmm0
    movdqa [rdi + 112], xmm0
#endif

    lea rdi, [rdi + 128]

    jnz .Lloop; //this tests the results from the sub rcx,1 instruction
//Clean up leftover quadwords/memset small blocks of memory
.Lmemset_64_small:
    //Copy extra 64 bit chunks, then test r8
END(memset_64)
