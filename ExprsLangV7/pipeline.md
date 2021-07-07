# Exprs
+ Our very simple front-end language.
+ Has arbitrarily nested expressions.
+ No binops or unops, everything is a function.

# Unique
+ Similar to the other "Unique" languages, we simply gensym all the variables.
+ However we now gensym both labels and alocs to allow arbitrary chars to be
  used in variables.
+ We also add the definitions of the binops and unops as functions and
  therefore we must add a `BinOp` and `UnOp` `Expr` type. These are also
  accompanied by type checks that throw an appropriate error code.
+ The book says to wait to implement the dynamic type checks, however that
  doesn't really make sense as this is the last language with actual types
  and unops.
+ These checks can just directly use the unops instead of calling the functions.

# Bits
+ The last expression language.
+ We get rid of separate data types for the singular 64 bit word.
+ All literals must be lowered to the appropriate ptr representation, and binops
  and unops must be appropriately rewritten.
+ We also introduce the needed bitwise binops (and, or, xor, shl).
+ We also introduce a distinction between pred and expr types
+ Bool literals in a pred position do not get lowered as a ptr but instead are
  kept intact.
+ Boolean operators in non-pred position get lowered into an if statement with
  true and false ptrs in the body.
+ Non boolean exprs in pred position become a check for inequality with the
  false ptr.

# Values
+ Nested `Expr` are turned into a series of lets.
+ We now distinguish between `Tail` and `Expr`.

# Monadic
+ Lets are turned into a sequence of `Stmt`.
+ Calling conventions are implemented and non tail calls become a jump statement
  with the original expression being replaced with the return register.
+ We add physical locations.

# Canonical
+ Non-trivial exprs become `[Stmt]`.

# Asm
+ Exprs can only be in a set or the result of a tail and so are transformed into
  the appropriate series of statements.
+ The link register location accompanying `Body` now gets set at the beginning
  of a body to appropriate value and blocks which return now jump to it.

# Undead
+ We add the undead-out set to each `Stmt` and the set of all `Aloc`, and
  `(Mloc, Mloc)` conflicts to each body.
+ We then use this info to save all undead alocs across each call.

# Nested
+ `Mloc` are replaced with `Rloc` via register allocation.

# Blocks
+ Bodies are split into a series of blocks that are connected via
  conditional or unconditional jumps at the end of tails.
+ Return label must be put into the link register just before a `JumpRet`.
  since all registers in use are saved this is fine.

# Para
+ Blocks are flattened into a sequence of abstracted asm instructions.

# Paren
+ Stmts are rewritten into a form with all the x64 idiosyncrasies.

# General
+ Lowering should be done in the `State Int64` monad so we can gensym in
  `Expr.Lower`, `Canonical.Lower`, and `Nested.Lower`.
+ When compiling `Label` a "$" should be put between the name and number. This
  avoids `Label "x" 11` and `Label "x1" 1` being both compiled to "x11".

# Error Codes
`[ 4 bits : Op | 4 bits Arg ]`

## Op Codes
Eq and Neq can't error as they check for general Ptr equality.
```
0 -> Add
1 -> Sub
2 -> Mul
3 -> Lt
4 -> Gt
5 -> Lte
6 -> Gte
```

## Arg Codes
0 -> 1st argument wrong type
1 -> 2nd argument wrong type
etc.

## Example
`0x11` corresponds to add being called with an incorrectly typed 2nd argument.

# Ptrs
It seems to me that there would be some better tag encoding such that a tag also
acts as its own mask. E.g. false? becomes `(ptr & falseTag) == falseTag`
instead of `(ptr & falseMask) == falseTag`.
