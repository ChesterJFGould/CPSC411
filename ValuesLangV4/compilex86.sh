#! /bin/sh

nasm -f elf64 -o "$1.o" "$1.s"

ld -e start -o "$1" "$1.o"

rm "$1.o"
