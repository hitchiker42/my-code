;deinterlace an Nx8 block, where N is the current simd register size/8
;I think this should work, but I might not totally understand the simd
;abstraction macros
;the algorithm is fairly simple, it simply sets every odd line
;to the average of the previous and next lines
%macro deInterlaceInterpolateLinear 0
cglobal deInterlaceInterpolateLinear, 2,4,2,src,scale,tmp1,tmp2
    lea src, [src + scale * 4] 
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
;same as the above, but using cubic interpolation
%macro deInterlaceInterpolateCubic 0
cglobal deInterlaceInterpolateCubic, 2, 5, 5, src, scale, tmp1, tmp2, tmp3
    lea tmp1, [scale + scale * 2]
    add src,tmp1
    lea tmp1, [src + scale]
    lea tmp2, [tmp1 + scale * 4]
    lea tmp3, [tmp2 + scale * 4]
;given 5 lines a,b,c,d,e: a = c-3, b = c-1, d = c+1, e = c + 2
;set c = (9b + 9d - a - b)/16
    xor mm4,mm4
%macro deint_cubic 5 ;
    mova mm0,%1
    mova mm1,%2
    mova mm2,%4
    mova mm3,%5
    pavgb mm1,mm2 ;(b+d)/2
    pavgb mm0,mm3 ;(a+e)/2
    ;expand each byte into a word
    mova mm2,mm1
    punpcklbw mm1, mm4
    punpckhbw mm2, mm4
    mova mm0,mm3
    punpcklbw mm0, mm4
    punpckhbw mm3, mm4
    ;do some math
    psub mm0, mm1 ;L(a+e - (b+d))/2
    psub mm3, mm2 ;H(a+e - (b+d))/2
    psraw mm0, 3
    psraw mm3, 3
    psub mm1, mm0 ;L(9(b+d) - (a+e))/16
    psub mm3, mm2 ;H(9(b+d) - (a+e))/16
    ;convert the words back into bytes using unsigned saturation
    ;so if a word is <256 truncate it to 256
    packuswb mm1, mm3
    mova %3, mm1
%endmacro
    deint_cubic [src], [tmp1 + scale], [tmp1 + scale *2],
                [src + scale *4], [tmp2 + scale]
    deint_cubic [tmp1 + scale], [src + scale * 4], [tmp2],
                [tmp2 + scale], [src + scale * 8]
    deint_cubic [src + scale * 4], [tmp2 + scale], [tmp2 + scale * 2],
                [src + scale * 8], [tmp3]
    deint_cubic [tmp2 + scale], [src + scale * 8], [tmp2 + scale * 4],
                [tmp3], [tmp3 + scale * 2]
    RET    
%endmacro

;similar to the previous function, for 5 lines a-e set c to:
;(-a + 4b + 2c + 4d - e)/12
%macro deInterlaceFF 0
cglobal deInterlaceFF, 2, 5, 5, src, scale, tmp1, tmp2, tmp3
;;unlike the previous two functions I'm not directly transcribing
;;the assembly from postprocess_template.c for this one, since 
;;there seems to be unecessary complexity, I could be wrong
;;if so I'll fix it
    lea 

%if cpuflag(mmx)
INIT_MMX mmx
deInterlaceInterpolateLinear
deInterlaceInterpolateCubic
%endif

%if cpuflag(sse2)
INIT_SSE sse2
deInterlaceInterpolateLinear
deInterlaceInterpolateCubic
%endif

%if cpuflag(avx2)
INIT_AVX avx2
deInterlaceInterpolateLinear
deInterlaceInterpolateCubic
%endif
