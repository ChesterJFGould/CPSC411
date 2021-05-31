# Description
My implementation of the "Paren-x64 v1" language.

# Modifications
+ Code is in Haskell instead of Racket.
+ The binary prints the ascii representation of the number in RAX instead of
  returning it as an exit code.
+ Uses a different syntax than that of the book.
+ Doesn't do integer bound checks, just parses all int literals as the native
  Haskell `Int` type.

# Syntax
A high level representation of how the syntax shown in the book relates to the
syntax used in this implementation. Checkout `example.es` for a more concrete
example.

```
(set! <reg> <int64>) -> <reg> := <int>

(set! <reg> <reg>) -> <reg> := <reg>

(set! <reg_1> (+ <reg_1> <int32>)) -> <reg_1> += <int>

(set! <reg_1> (* <reg_1> <int32>)) -> <reg_1> *= <int>

(set! <reg_1> (+ <reg_1> <reg>)) -> <reg_1> += <reg>

(set! <reg_1> (* <reg_1> <reg>)) -> <reg_1> *= <reg>
```
