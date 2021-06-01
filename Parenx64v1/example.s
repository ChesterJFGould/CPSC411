global start

section .text

start:
mov r8 , 1000000000000000000
add r8 , -1486618624
mov rdx , r8
add rdx , 2
mov rax , 65
print:
mov r8, msg
mov [r8], rax
mov rax, 1
mov rdi, 1
mov rsi, msg
mov rdx, len
syscall
mov r9, 0x0a
mov [r8], r9
syscall
exit:
mov rax, 60
mov rdi, 0
syscall

section .data

dummy: db 0
len: equ 1
msg: times len dw 0

