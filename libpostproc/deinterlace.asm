;******************************************************************************
;* DeInterlacing filters written using SIMD extensions
;* Copyright (c) 2015 Tucker DiNapoli
;*
;* Algorithms from existing postprocessing code by Michael Niedermayer
;*
;* This file is part of FFmpeg.
;*
;* FFmpeg is free software; you can redistribute it and/or
;* modify it under the terms of the GNU Lesser General Public
;* License as published by the Free Software Foundation; either
;* version 2.1 of the License, or (at your option) any later version.
;*
;* FFmpeg is distributed in the hope that it will be useful,
;* but WITHOUT ANY WARRANTY; without even the implied warranty of
;* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;* Lesser General Public License for more details.
;*
;* You should have received a copy of the GNU Lesser General Public
;* License along with FFmpeg; if not, write to the Free Software
;* Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
;******************************************************************************

;; I imagine that the makefiles include conifig.asm by default, but I haven't
;; really looked at the build system close yet.
;; I personally had to set these to relative paths in order to assemble this
;; since it's not build by the makefile or anything
%include "PPutil.asm"
;; I tried to use named parameters in function definations (rather than just rN)
;; but I couldn't get the code to assemble. I'm not sure if there's anything I
;; need to do other than include them in the argument list to the cglobal call.


;; deinterlace an Nx8 block, where N is the current simd register size/8
;; The C code will need to be re-written in order to use the sse/avx versions
;; as currently the code only works on 8x8 blocks
;; The algorithm for linear interpolation is just averaging the line before
;; and the line after every odd line
%macro gen_deinterlace_interpolate_linear 0
cglobal deinterlace_interpolate_linear, 2, 4, 2;, src, scale, tmp1, tmp2
    lea r1, [r1 + r2 * 4] 
    lea r3, [r1 + r2]
    lea r4, [r3 + r2 * 4]
    mova m0, [r1]
    mova m1, [r3 + r2]
    pavgb m0,m1
    mova [r3], m0
    mova m0, [r1 + r2 * 4]
    pavgb m1,m0
    mova [r3 + r2 * 2],m1
    mova m1, [r4 + r2]
    pavgb m0,m1
    mova [r4], m0
    mova m1, [r1 + r2 * 8]
    pavgb m0,m1
    mova [r4 + r2 * 2], m0
    RET
%endmacro
;; same as the above, but using cubic interpolation
%macro gen_deinterlace_interpolate_cubic 0
cglobal deinterlace_interpolate_cubic, 2, 5, 5;, src, scale, tmp1, tmp2, tmp3
    lea r3, [r2 + r2 * 2]
    add r1,r3
    lea r3, [r1 + r2]
    lea r4, [r3 + r2 * 4]
    lea r5, [r4 + r2 * 4]
    pxor m4,m4       
    deint_cubic [r1], [r3 + r2], [r3 + r2 *2],\
                [r1 + r2 *4], [r4 + r2]
    deint_cubic [r3 + r2], [r1 + r2 * 4], [r4],\
                [r4 + r2], [r1 + r2 * 8]
    deint_cubic [r1 + r2 * 4], [r4 + r2], [r4 + r2 * 2],\
                [r1 + r2 * 8], [r5]
    deint_cubic [r4 + r2], [r1 + r2 * 8], [r4 + r2 * 4],\
                [r5], [r5 + r2 * 2]
    RET    
%endmacro
;; The body of the above function is in this macro
;; given 5 lines a,b,c,d,e: a = c-3, b = c-1, d = c+1, e = c + 2
;; set c = (9b + 9d - a - b)/16
%macro deint_cubic 5
    mova m0,%1
    mova m1,%2
    mova m2,%4
    mova m3,%5
    pavgb m1,m2 ;(b+d)/2
    pavgb m0,m3 ;(a+e)/2
    ;; convert each byte into a word
    mova m2,m1
    punpcklbw m1, m4
    punpckhbw m2, m4
    mova m0,m3
    punpcklbw m0, m4
    punpckhbw m3, m4
    ;; do some math
    psubw m0, m1 ;L(a+e - (b+d))/2
    psubw m3, m2 ;H(a+e - (b+d))/2
    psraw m0, 3
    psraw m3, 3
    psubw m1, m0 ;L(9(b+d) - (a+e))/16
    psubw m3, m2 ;H(9(b+d) - (a+e))/16
    ;; convert the words back into bytes using unsigned saturation
    packuswb m1, m3
    mova %3, m1
%endmacro
;; for each set of 3 lines a b c, such that b is an odd line, set b to:
;; (a+2b+c)/4. As with previous functions operates on an Nx8 block
;; where N is dependent on the simd register size
%macro gen_deinterlace_blend_linear
;; The C version takes a third argument, a pointer to a uint8_t, I'm guessing
;; this is the 3rd line, so I'm using that instead, It should be easy
;; to fix if I'm wrong, the replacement code is in comments, with the 3 argument
;; being in r5
cglobal deinterlace_blend_linear, 2, 4, 2 ;src, scale, tmp1, tmp2
    lea r1, [r1 + r2 * 4]
    lea r3, [r1 + r2]
    lea r4, [r3 + r2 * 4]
    mova m0, [r1 - r2] ;L0
    ;; or mova m0, [r5]
    mova m1, [r3] ;L2
    mova m2, [r1] ;L1
    pavgb m0, m1
    pavgb m0, m2
    mova [r1], m0 ;2L1 + L0 + L1 / 4
    mova m0, [r3 + r2 * 2] ;L3
    pavgb m2, m0
    pavgb m2, m1
    mova [r3], m2 ;L4
    mova m2, [r3 + r2 * 2]
    pavgb m1, m2 
    pavgb m1, m0
    mova [r3+r2], m1 ;L5
    mova m1, [r1 + r2 * 4]
    pavgb m0, m1 
    pavgb m0, m2
    mova [r3 + r2 * 2], m0 ;L6
    mova m0, [r4]
    pavgb m2, m0
    pavgb m2, m1
    mova [r1 + r2 * 4], m2 ;L7
    mova m2, [r4 + r2]
    pavgb m1, m2
    pavgb m1, m0
    mova [r4], m1
    mova m1, [r4 + r2 * 2]
    pavgb m0, m1
    pavgb m0, m2
    mova [r4 + r2], m0
    mova m0, [r1 + r2 * 8]
    pavgb m2, m0
    pavgb m2, m1
    mova [r4 + r2 * 2], m2
    ;; with extra arg mova [r5], m1

    


;; I'm not exactly sure how to insure the following only get built if 
;; the specified instruction set is available.
;; If the INIT_XXX macros do that then great, otherwise I'll correct it 
SECTION_TEXT

INIT_MMX mmx2
gen_deinterlace_interpolate_linear
gen_deinterlace_interpolate_cubic

INIT_XMM sse2
gen_deinterlace_interpolate_linear
gen_deinterlace_interpolate_cubic

INIT_YMM avx2
gen_deinterlace_interpolate_linear
gen_deinterlace_interpolate_cubic

