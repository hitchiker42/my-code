;******************************************************************************
;*
;* Copyright (c) 2015 Tucker DiNapoli
;*
;* Defination of the PPContext struct in assembly, and some macros
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
%include "config.asm"
%include "libavutil/x86/x86util.asm"
%if ARCH_X86_64
%define pointer resq 1
%else
%define pointer resd 1
%endif
struc PPMode
    .lum_mode resd 1
    .chrom_mode resd 1
    .error resd 1
    .min_allowed_y resd 1
    .max_allowed_y resd 1
    .max_clipped_threshold resd 1
    .max_tmp_noise resd 3
    .base_dc_diff resd 1
    .flatness_threshold resd 1
    .forcedQuant resd 1
endstruc
struc PPContext
    .av_class pointer 1
    .temp_blocks pointer 1
    .y_historgam pointer 1
    alignb 8
    .packed_yoffset resq 1
    .packed_yscale resq 1; 8 byte aligned by default
    .temp_blurred pointer 3
    .temp_blurred_past pointer 3
    .temp_dst pointer 1
    .temp_src pointer 1
    .deint_temp pointer 1
    alignb 8
    .pQPb resq 1
    .pQPb2 resq 1
;; These next fields & next alignment may need to be changed for 128/256 bit registers
    alignb 8
    .mmx_dc_offset resq 64
    .mmx_dc_threshold resq 64
    .std_QP_table pointer 1
    .non_BQP_table pointer 1 ;not sure about placement of B in name
    .forced_QP_table pointer 1
    .QP resd 1
    .nonBQP resd 1
    .frame_num resd 1
    .cpu_caps resd 1
    .qp_stride resd 1
    .stride resd 1
    .h_chroma_subsample resd 1
    .v_chroma_subsample resd 1
    .ppMode resd PPMode_size
endstruc
;;splits arguments into lines
%macro split 0-*
%rep %0
%1
%rotate 1
%endrep
%endmacro

%assign __gensym_counter 0
%macro gensym 0
%assign __gensym_counter __gensym_counter+1
?G %+ __gensym_counter
%endmacro

;; copy low quadword to upper quadword(s)
%macro dup_low_quadword 1
%if cpuflag(sse2)
    pshufpd %1, %1, 0x00
%elif cpuflag(avx2)
    vpermq %1, %1, 01010101b
%endif
%endmacro
%macro do_simd_sizes 2
%1 %2b
%1 %2w
%1 %2d
%1 %2q
%endmacro
;; given an argument xx defines a macro pcmovxx
%macro gen_pcmovxx 1 
%macro pcmov%1 4-6 ,%1 ;;dst, src, cmp1, cmp2, [tmp = cmp2]
%if %0 == 5
%ifnidn %5,%3
    mova %5,%3
%endif
%endif
    pcmp%6 %5,%4
    mova %1, %2
    pand %1, %5
%endmacro
%endmacro
do_simd_sizes gen_pcmovxx,eq
do_simd_sizes gen_pcmovxx,ne
do_simd_sizes gen_pcmovxx,lt
do_simd_sizes gen_pcmovxx,le
do_simd_sizes gen_pcmovxx,gt
do_simd_sizes gen_pcmovxx,ge
;; condition needs to be passed as 6th parameter
do_simd_sizes gen_pcmovxx,xx

%macro define_qword_vector_constant 5
%if cpuflag(avx2)
SECTION_RODATA 32
%else
SECTION_RODATA 16
%endif
%1: 
    dq %2
%if cpuflag(sse2)
    dq %3
%if cpuflag(avx2)
    dq %4
    dq %5
%endif
%endif
SECTION_TEXT
%endmacro
%macro declare_vector_space
