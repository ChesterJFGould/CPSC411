global start
extern printf

section .text

start:
	; init stack
	mov rax, 9 ; mmap
	mov rdi, 0 ; page aligned
	mov rsi, 8388608 ; 8 MiB
	mov rdx, 3 ; rw
	mov r10, 34 ; map type = memory
	mov r8, -1 ; not a fd
	mov r9, 0 ; offset
	syscall
	mov rbp, rsp
	mov r15, done ; setup return address

mov RSP , R15
mov RDI , 10
mov R15 , RSP
jmp fac
true0:
mov RAX , 1
jmp RCX
return1:
add RBP , 16
mov RCX , [ RBP - 0 ]
mov RBX , [ RBP - 8 ]
mov RSP , RAX
mov RAX , RBX
imul RAX , RSP
jmp RCX
return2:
add RBP , 16
mov RCX , [ RBP - 0 ]
mov RBX , [ RBP - 8 ]
mov RSP , RAX
mov RDI , RSP
mov [ RBP - 0 ] , RCX
mov [ RBP - 8 ] , RBX
sub RBP , 16
mov R15 , return1
jmp fac
false3:
mov RDI , RBX
mov [ RBP - 0 ] , RCX
mov [ RBP - 8 ] , RBX
sub RBP , 16
mov R15 , return2
jmp pred
fac:
mov RCX , R15
mov RBX , RDI
cmp RBX , 1
jl true0
jmp false3
pred:
mov RCX , R15
mov RSP , RDI
mov RBX , 1
mov RAX , RSP
sub RAX , RBX
jmp RCX
done:
setup_print:
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

section .data

len: equ 20
msg: times len dw 0
nl: db 0x0a

