section .text
    global _start
_start:
    mov eax,5d
    mov ecx,eax
    jmp .fact
.fact:
    cmp ecx,0d
    je .retNum
    imul ecx
    dec ecx
    jmp .fact
.retNum:
    mov ecx,eax
    mov eax,4
    mov ebx,1
    mov edx,1
    int 80h
    mov eax,1
    mov ebx,0
    int 80h
