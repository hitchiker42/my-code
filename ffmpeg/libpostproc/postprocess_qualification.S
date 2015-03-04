;deinterlace an Nx8 block, where N is the current simd register size/8
;I think this should work, but I might not totally understand the simd
;abstraction macros    
%macro deInterlaceInterpolateLinear 0
cglobal deInterlaceInterpolateLinear, 2,4,2,src,scale,tmp1,tmp2
    mov tmp, scale
    shl tmp, 2
    add src, tmp ;src += 4*stride
    lea tmp1, [src + scale]
    lea tmp2, [tmp1 + scale * 4]
    mova mm0, [src]
    mova mm1, [tmp1 + scale]
    pavgb mm0,mm1
    mova [tmp1], mm0
    mova mm0, [src + scale * 4]
    pavgb mm1,mm0
    mova [tmp1 + scale * 2],mm1
    mova mm1, [tmp2 + scale * 6]
    pavgb mm0,mm1
    mova [tmp2], mm0
    mova mm1, [src + scale * 8]
    pavgb mm0,mm1
    mova [tmp2 + scale * 2], mm0
    RET
%endmacro

%if cpuflag(mmx)
INIT_MMX mmx
deInterlaceInterpolateLinear
%endif

%if cpuflag(sse2)
INIT_SSE sse2
deInterlaceInterpolateLinear
%endif

%if cpuflag(avx2)
INIT_AVX avx2
deInterlaceInterpolateLinear
%endif
    
