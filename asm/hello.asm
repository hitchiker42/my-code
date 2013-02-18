section .data
    string:  db 48h,65h,6ch,6ch,6fh,2ch,57h,6fh,72h,6ch,64h,21h,0ah
    len:    equ $ - string

section .text
    global _start
_start:
    mov eax,4
    mov ebx,1
    mov ecx,string
    mov edx,len
    int 80h
    mov eax,1
    mov ebx,0
    int 80h
