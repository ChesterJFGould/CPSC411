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
	; init heap
	mov rax, 9 ; mmap
	mov rdi, 0 ; page aligned
	mov rsi, 8388608 ; 8 MiB
	mov rdx, 3 ; rw
	mov r10, 34 ; map type = memory
	mov r8, -1 ; not a fd
	mov r9, 0 ; offset
	syscall
	sub rsp, 8288608; Why 8288608 and not 8388608? Because 8388608 segfaults and
	                ; 8288608 doesn't, that's why.
	mov r15, done ; setup return address

mov RBX , R15
mov RAX , RSP
add RSP , 8
mov R12 , closureDef$6
mov QWORD [ RAX + 0 ] , R12
mov R10 , succ$0
mov R12 , RAX
mov QWORD [ R10 + 0 ] , R12
mov RAX , RSP
add RSP , 8
mov R12 , closureDef$8
mov QWORD [ RAX + 0 ] , R12
mov R10 , succ$2
mov R12 , RAX
mov QWORD [ R10 + 0 ] , R12
mov RAX , 10
mov R10 , succ$2
mov RCX , QWORD [ R10 + 0 ]
mov RAX , QWORD [ RCX + 0 ]
mov RDI , RCX
mov RSI , 1
mov QWORD [ RBP + 0 ] , RBX
sub RBP , 8
mov R15 , continuation$25
jmp RAX
closureDef$6:
mov RAX , RDI
mov RAX , RSI
mov RBX , R15
mov RAX , RAX
add RAX , 1
jmp RBX
consequence$20:
mov RAX , 2
jmp RCX
alternative$21:
mov RAX , RBX
jmp RCX
continuation$22:
add RBP , 16
mov RCX , QWORD [ RBP + 0 ]
mov RBX , QWORD [ RBP + 8 ]
mov RAX , RAX
cmp RAX , 2
je consequence$20
jmp alternative$21
closureDef$8:
mov RAX , RDI
mov RBX , RSI
mov RCX , R15
mov R10 , succ$0
mov RDX , QWORD [ R10 + 0 ]
mov RAX , QWORD [ RDX + 0 ]
mov RDI , RDX
mov RSI , RBX
mov QWORD [ RBP + 0 ] , RCX
mov QWORD [ RBP + 8 ] , RBX
sub RBP , 16
mov R15 , continuation$22
jmp RAX
consequence$23:
mov RAX , 1
jmp RBX
alternative$24:
mov RAX , 0
jmp RBX
continuation$25:
add RBP , 8
mov RBX , QWORD [ RBP + 0 ]
mov RAX , RAX
cmp RAX , 2
je consequence$23
jmp alternative$24
section .data
succ$0: dq 0
succ$2: dq 0
section .text
done:
print_int:
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

