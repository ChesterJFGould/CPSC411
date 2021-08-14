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
mov R12 , closureDef$3
mov [ RAX + 0 ] , R12
mov R10 , fac$0
mov R12 , RAX
mov [ R10 + 0 ] , R12
mov R10 , fac$0
mov RCX , [ R10 + 0 ]
mov RAX , [ RCX + 0 ]
mov RDI , RCX
mov RSI , 10
mov R15 , RBX
jmp RAX
continuation$13:
add RBP , 16
mov RDX , [ RBP + 0 ]
mov RCX , [ RBP + 8 ]
mov RBX , RAX
mov RAX , RCX
imul RAX , RBX
jmp RDX
consequence$14:
mov RAX , 1
jmp RDX
alternative$15:
mov R10 , fac$0
mov RSI , [ R10 + 0 ]
mov RBX , [ RSI + 0 ]
mov RAX , RCX
sub RAX , 1
mov RDI , RSI
mov RSI , RAX
mov [ RBP + 0 ] , RDX
mov [ RBP + 8 ] , RCX
sub RBP , 16
mov R15 , continuation$13
jmp RBX
closureDef$3:
mov RAX , RDI
mov RCX , RSI
mov RDX , R15
cmp RCX , 1
jl consequence$14
jmp alternative$15
section .data
fac$0: dq 0
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

