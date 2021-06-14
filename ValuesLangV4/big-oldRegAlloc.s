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
jmp main0
main0:
mov RSP , 10
mov RAX , 10
mov RBX , 10
mov RCX , 10
mov RDX , 10
mov RSI , 10
mov RDI , 10
mov R8 , 10
mov R9 , 10
mov R12 , 10
mov R13 , 10
mov R14 , 10
mov R15 , 10
mov QWORD [ RBP - 0 ] , 10
mov QWORD [ RBP - 8 ] , 10
mov QWORD [ RBP - 16 ] , 10
mov QWORD [ RBP - 24 ] , 10
mov QWORD [ RBP - 32 ] , 10
mov QWORD [ RBP - 40 ] , 10
mov QWORD [ RBP - 48 ] , 10
mov QWORD [ RBP - 56 ] , 10
mov QWORD [ RBP - 64 ] , 10
mov QWORD [ RBP - 72 ] , 10
mov QWORD [ RBP - 80 ] , 10
mov QWORD [ RBP - 88 ] , 10
mov QWORD [ RBP - 96 ] , 62
mov RSP , RSP
add RSP , 2
mov RSP , RAX
add RSP , 2
mov RSP , RBX
add RSP , 2
mov RSP , RCX
add RSP , 2
mov RSP , RDX
add RSP , 2
mov RSP , RSI
add RSP , 2
mov RSP , RDI
add RSP , 2
mov RSP , R8
add RSP , 2
mov RSP , R9
add RSP , 2
mov RSP , R12
add RSP , 2
mov RSP , R13
add RSP , 2
mov RSP , R14
add RSP , 2
mov RSP , R15
add RSP , 2
mov RSP , QWORD [ RBP - 0 ]
add RSP , 2
mov RSP , QWORD [ RBP - 8 ]
add RSP , 2
mov RSP , QWORD [ RBP - 16 ]
add RSP , 2
mov RSP , QWORD [ RBP - 24 ]
add RSP , 2
mov RSP , QWORD [ RBP - 32 ]
add RSP , 2
mov RSP , QWORD [ RBP - 40 ]
add RSP , 2
mov RSP , QWORD [ RBP - 48 ]
add RSP , 2
mov RSP , QWORD [ RBP - 56 ]
add RSP , 2
mov RSP , QWORD [ RBP - 64 ]
add RSP , 2
mov RSP , QWORD [ RBP - 72 ]
add RSP , 2
mov RSP , QWORD [ RBP - 80 ]
add RSP , 2
mov RSP , QWORD [ RBP - 88 ]
add RSP , 2
mov RSP , QWORD [ RBP - 96 ]
add RSP , 2
mov RAX , RSP
jmp done
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

