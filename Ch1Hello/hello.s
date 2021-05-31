global start

section .text

start:
	mov r8, 5

fac:
	mov r9, 1

facAcc:
	cmp r8, 0
	je facDone
	imul r9, r8
	dec r8
	jmp facAcc

facDone:
	mov r8, msg
	mov [r8], r9

print:
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
