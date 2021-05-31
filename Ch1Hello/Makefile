hello: hello.o
	ld -e start -o hello hello.o

hello.o: hello.s
	nasm -f elf64 -o hello.o hello.s
