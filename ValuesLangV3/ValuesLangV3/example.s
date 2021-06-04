global start

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
mov QWORD [ RBP - 32 ] , 10
mov R10 , QWORD [ RBP - 32 ]
mov QWORD [ RBP - 0 ] , R10
mov R10 , QWORD [ RBP - 0 ]
add R10 , QWORD [ RBP - 32 ]
mov QWORD [ RBP - 0 ] , R10
mov R10 , QWORD [ RBP - 0 ]
mov QWORD [ RBP - 40 ] , R10
mov R10 , QWORD [ RBP - 32 ]
mov QWORD [ RBP - 8 ] , R10
mov R10 , QWORD [ RBP - 8 ]
add R10 , QWORD [ RBP - 40 ]
mov QWORD [ RBP - 8 ] , R10
mov R10 , QWORD [ RBP - 8 ]
mov QWORD [ RBP - 48 ] , R10
mov R10 , QWORD [ RBP - 48 ]
mov QWORD [ RBP - 16 ] , R10
mov R10 , QWORD [ RBP - 16 ]
add R10 , 12
mov QWORD [ RBP - 16 ] , R10
mov R10 , QWORD [ RBP - 16 ]
mov QWORD [ RBP - 56 ] , R10
mov R10 , QWORD [ RBP - 56 ]
mov QWORD [ RBP - 24 ] , R10
mov R10 , QWORD [ RBP - 24 ]
add R10 , QWORD [ RBP - 32 ]
mov QWORD [ RBP - 24 ] , R10
mov RAX , QWORD [ RBP - 24 ]
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
