done:
	mov r8, 0b111 ; Int tag
	and r8, rax
	cmp r8, 0
	je print_int
	mov r8, 0xFF
	and r8, rax
	cmp r8, 0b1110 ; True tag
	je print_true
	cmp r8, 0b110 ; False tag
	je print_false
	cmp r8, 0b10110 ; Empty tag
	je print_empty
	cmp r8, 0b11110 ; Void tag
	je print_void
	cmp r8, 0b101110 ; Char tag
	je print_char
	cmp r8, 0b111110 ; Error tag
	je print_error
print_int:
	sar rax, 3
	mov r8, 10 ; we need to div by 10
	mov r10, msg ; r10 becomes pointer into msg that we traverse backwards
	add r10, len
	xor r11, r11 ; r11 becomes the counter for the string length
	cmp rax, 0
	jl rax_negative
rax_positive:
print_rax_positive:
	xor rdx, rdx
	dec r10 
	inc r11
	div r8
	add rdx, '0'
	mov [r10], dl
	cmp rax, 0
	jne print_rax_positive
	jmp print
rax_negative:
	neg rax
print_rax_negative:
	xor rdx, rdx
	dec r10 
	inc r11
	div r8
	add rdx, '0'
	mov [r10], dl
	cmp rax, 0
	jne print_rax_negative
	dec r10
	inc r11
	mov BYTE [r10], '-'
	jmp print
print_true:
	mov r10, true
	mov r11, trueLen
	jmp print
print_false:
	mov r10, false
	mov r11, trueLen
	jmp print
print_empty:
	mov r10, empty
	mov r11, emptyLen
	jmp print
print_void:
	jmp exit
print_char:
	mov r10, msg
	add r10, len
	dec r10
	mov BYTE [r10], `\'`
	dec r10
	shr rax, 8
	mov [r10], al
	dec r10
	mov BYTE [r10], `\'`
	mov r11, 4
	jmp print
print_error:
	shr rax, 8
	jmp error
print_vec:
	shr rax 3
	shl rax 3
print:
	mov rax, 1
	mov rdi, 1
	mov rsi, r10
	mov rdx, r11
	syscall
print_newline:
	mov rax, 1
	mov rdi, 1
	mov rsi, nl
	mov rdx, 1
	syscall
exit:
	mov rax, 60
	mov rdi, 0
	syscall
error:
	mov rdi, rax
	mov rax, 60
	syscall

section .data

len: equ 20
msg: times len dw 0
trueLen: equ 5
true: db 'true', 0
falseLen: equ 6
false: db 'false', 0
emptyLen: equ 3
empty: db '[]', 0
nl: db 0x0a
