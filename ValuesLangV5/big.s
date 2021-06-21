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

mov QWORD [ RBP - 0 ] , 7
mov QWORD [ RBP - 8 ] , 8
mov QWORD [ RBP - 16 ] , 9
mov QWORD [ RBP - 24 ] , 10
mov QWORD [ RBP - 32 ] , 11
mov QWORD [ RBP - 40 ] , 12
mov QWORD [ RBP - 48 ] , 13
mov QWORD [ RBP - 56 ] , 14
mov QWORD [ RBP - 64 ] , 15
mov QWORD [ RBP - 72 ] , 16
mov QWORD [ RBP - 80 ] , 17
mov QWORD [ RBP - 88 ] , 18
mov QWORD [ RBP - 96 ] , 19
mov QWORD [ RBP - 104 ] , 20
mov QWORD [ RBP - 112 ] , 21
mov QWORD [ RBP - 120 ] , 22
mov QWORD [ RBP - 128 ] , 23
mov QWORD [ RBP - 136 ] , 24
mov QWORD [ RBP - 144 ] , 25
mov QWORD [ RBP - 152 ] , 26
mov RDI , 1
mov RSI , 2
mov RDX , 3
mov RCX , 4
mov R8 , 5
mov R9 , 6
jmp big
big:
mov RDI , RDI
mov RDI , RSI
mov RDI , RDX
mov RDI , RCX
mov RDI , R8
mov RDI , R9
mov RDI , QWORD [ RBP - 0 ]
mov RDI , QWORD [ RBP - 8 ]
mov RDI , QWORD [ RBP - 16 ]
mov RDI , QWORD [ RBP - 24 ]
mov RDI , QWORD [ RBP - 32 ]
mov RDI , QWORD [ RBP - 40 ]
mov RDI , QWORD [ RBP - 48 ]
mov RDI , QWORD [ RBP - 56 ]
mov RDI , QWORD [ RBP - 64 ]
mov RDI , QWORD [ RBP - 72 ]
mov RDI , QWORD [ RBP - 80 ]
mov RDI , QWORD [ RBP - 88 ]
mov RDI , QWORD [ RBP - 96 ]
mov RDI , QWORD [ RBP - 104 ]
mov RDI , QWORD [ RBP - 112 ]
mov RDI , QWORD [ RBP - 120 ]
mov RDI , QWORD [ RBP - 128 ]
mov RDI , QWORD [ RBP - 136 ]
mov RDI , QWORD [ RBP - 144 ]
mov RDI , QWORD [ RBP - 152 ]
mov RAX , RDI
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

