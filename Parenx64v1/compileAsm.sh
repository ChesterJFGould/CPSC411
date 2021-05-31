#! /bin/sh

cabal run -v0 < "$1.es" > "$1.s"

nasm -f elf64 -o "$1.o" "$1.s"

ld -e start -o "$1" "$1.o"

rm "$1.o"
rm "$1.s"
