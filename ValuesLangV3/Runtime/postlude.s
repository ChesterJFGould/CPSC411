setup_print:
	mov r8, 10 ; we need to div by 10
	mov r10, msg ; r10 becomes pointer into msg that we traverse backwards
	add r10, len
	xor r11, r11 ; r11 becomes the counter for the string length
print_rax:
	xor rdx, rdx
	dec r10 
	inc r11
	div r8
	add rdx, '0'
	mov [r10], dl
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
