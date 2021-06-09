global start

section .text

start:
	mov rax, 314159

setup:
	mov r8, 10
	mov r10, msg
	add r10, len
	xor r11, r11
print_rax:
	xor rdx, rdx
	dec r10
	div r8
	add rdx, '0'
	mov [r10], dl
	inc r11
	cmp rax, 0
	jne print_rax
.print:
	mov rax, 1
	mov rdi, 1
	mov rsi, r10
	mov rdx, r11
	syscall
.print_newline:
	mov rax, 1
	mov rdi, 1
	mov rsi, nl
	mov rdx, 1
	syscall
exit:
	mov rax, 60
	mov rdi, 0
	syscall

section .data

len: equ 20
msg: times len dw 0
nl: db 0x0a
