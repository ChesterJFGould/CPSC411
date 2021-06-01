# Description
An implementation of the "Asm-lang v2" language from the CPSC 411 book with
different syntax.

# Syntax
Here's how the sexpr syntax translates to the syntax used in this
implementation.

```
(set! <aloc> <triv>) -> <aloc> := <triv>

(set! <aloc_1> (+ <aloc_1> <triv>)) -> <aloc_1> += <triv>

(set! <aloc_1> (* <aloc_1> <triv>)) -> <aloc_1> *= <triv>

(halt <triv>) -> halt <triv>
```
