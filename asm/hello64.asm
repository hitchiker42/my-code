section .data
    string:  db 48h,65h,6ch,6ch,6fh,2ch,57h,6fh,72h,6ch,64h,21h,0ah
    len:    equ $ - string

section .text
    global _start
_start:
    mov dx,1
    mov rax,string
    mov rsi,[rax]
    outsw
    

