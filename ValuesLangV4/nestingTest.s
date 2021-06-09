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
jmp main4
tc0:
mov RSP , RSP
add RSP , RAX
mov RAX , RSP
jmp done
ta1:
add RAX , RBX
mov RAX , RAX
jmp done
pc2:
jmp tc0
pa3:
jmp tc0
main4:
mov RSP , 3
mov RAX , 20
mov RBX , 31
mov RCX , RSP
add RCX , RAX
cmp RCX , 23
jge pc2
jmp pa3
done:
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

