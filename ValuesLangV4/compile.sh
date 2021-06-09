#! /bin/sh

asmTemp="$(mktemp).s"
objTemp="$(mktemp).o"

cabal run -v0 < "$1.es" > "$asmTemp"

nasm -f elf64 -o "$objTemp" "$asmTemp"

ld -e start -o "$1" "$objTemp"

rm "$asmTemp"
rm "$objTemp"
