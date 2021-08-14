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
mov R12 , closureDef$9
mov [ RAX + 0 ] , R12
mov R10 , zero$0
mov R12 , RAX
mov [ R10 + 0 ] , R12
mov RAX , RSP
add RSP , 8
mov R12 , closureDef$11
mov [ RAX + 0 ] , R12
mov R10 , pred$2
mov R12 , RAX
mov [ R10 + 0 ] , R12
mov RAX , RSP ; Set rax to heap pointer
add RSP , 8 ; Allocate 8 bytes
mov R12 , closureDef$13 ; Put closureDef$13 as code pointer
mov [ RAX + 0 ] , R12 ; ^
mov R10 , even$4 ; Load even$4 label
mov R12 , RAX ; Load closure
mov [ R10 + 0 ] , R12 ; Put the closure into the label
mov RAX , RSP
add RSP , 8
mov R12 , closureDef$15
mov [ RAX + 0 ] , R12
mov R10 , odd$5
mov R12 , RAX
mov [ R10 + 0 ] , R12
mov R10 , even$4
mov RCX , [ R10 + 0 ]
mov RAX , [ RCX + 0 ]
mov RDI , RCX
mov RSI , 0
mov [ RBP + 0 ] , RBX
sub RBP , 8
mov R15 , continuation$56
jmp RAX
consequence$44:
mov RAX , 1
jmp RBX
alternative$45:
mov RAX , 0
jmp RBX
closureDef$9:
mov RAX , RDI
mov RAX , RSI
mov RBX , R15
cmp RAX , 0
je consequence$44
jmp alternative$45
closureDef$11:
mov RAX , RDI
mov RAX , RSI
mov RBX , R15
mov RAX , RAX
sub RAX , 1
jmp RBX
continuation$46:
add RBP , 24
mov R8 , [ RBP + 0 ]
mov RDX , [ RBP + 8 ]
mov RBX , [ RBP + 16 ]
mov RAX , RAX
mov RDI , R8
mov RSI , RAX
mov R15 , RDX
jmp RBX
consequence$47:
mov RAX , 1 ; Put true into the return register
jmp RDX ; Jump to the return place
alternative$48:
mov R10 , odd$5
mov R8 , [ R10 + 0 ]
mov RBX , [ R8 + 0 ]
mov R10 , pred$2
mov RSI , [ R10 + 0 ]
mov RAX , [ RSI + 0 ]
mov RDI , RSI
mov RSI , RCX
mov [ RBP + 0 ] , R8
mov [ RBP + 8 ] , RDX
mov [ RBP + 16 ] , RBX
sub RBP , 24
mov R15 , continuation$46
jmp RAX
continuation$49:
add RBP , 16 ; De-allocate zero$0 stack
mov RDX , [ RBP + 0 ] ; Load return place
mov RCX , [ RBP + 8 ] ; Load n
mov RAX , RAX ; Load the result
cmp RAX , 1 ; See if result is true
je consequence$47 ; If result is true then jump to consequence$47
jmp alternative$48 ; Else jump to alternative$48
closureDef$13:
mov RAX , RDI ; Closure
mov RCX , RSI ; n
mov RDX , R15 ; Return place
mov R10 , zero$0 ; Get zero$0 closure and code pointer
mov RBX , [ R10 + 0 ] ; zero$0 Closure
mov RAX , [ RBX + 0 ] ; zero$0 Code Pointer
mov RDI , RBX ; Put zero$0 closure into the closure register
mov RSI , RCX ; Put n into the argument register
mov [ RBP + 0 ] , RDX ; Save return place
mov [ RBP + 8 ] , RCX ; Save n
sub RBP , 16 ; Allocate space on stack
mov R15 , continuation$49 ; Put the continuation in the link register
jmp RAX ; Call zero$0
continuation$50:
add RBP , 24
mov R8 , [ RBP + 0 ]
mov RDX , [ RBP + 8 ]
mov RBX , [ RBP + 16 ]
mov RAX , RAX
mov RDI , R8
mov RSI , RAX
mov R15 , RDX
jmp RBX
consequence$51:
mov RAX , 0
jmp RDX
alternative$52:
mov R10 , even$4
mov R8 , [ R10 + 0 ]
mov RBX , [ R8 + 0 ]
mov R10 , pred$2
mov RSI , [ R10 + 0 ]
mov RAX , [ RSI + 0 ]
mov RDI , RSI
mov RSI , RCX
mov [ RBP + 0 ] , R8
mov [ RBP + 8 ] , RDX
mov [ RBP + 16 ] , RBX
sub RBP , 24
mov R15 , continuation$50
jmp RAX
continuation$53:
add RBP , 16
mov RDX , [ RBP + 0 ]
mov RCX , [ RBP + 8 ]
mov RAX , RAX
cmp RAX , 1
je consequence$51
jmp alternative$52
closureDef$15:
mov RAX , RDI
mov RCX , RSI
mov RDX , R15
mov R10 , zero$0
mov RBX , [ R10 + 0 ]
mov RAX , [ RBX + 0 ]
mov RDI , RBX
mov RSI , RCX
mov [ RBP + 0 ] , RDX
mov [ RBP + 8 ] , RCX
sub RBP , 16
mov R15 , continuation$53
jmp RAX
consequence$54:
mov RAX , 1
jmp RBX
alternative$55:
mov RAX , 0
jmp RBX
continuation$56:
add RBP , 8
mov RBX , [ RBP + 0 ]
mov RAX , RAX
cmp RAX , 1
je consequence$54
jmp alternative$55
section .data
zero$0: dq 0
pred$2: dq 0
even$4: dq 0
odd$5: dq 0
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

