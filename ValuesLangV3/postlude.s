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

len: equ 1
msg: times len dw 0
