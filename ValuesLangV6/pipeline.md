# Architecture
+ We will use a link register instead of pushing the return address on the stack.
  This seems to be better for function-oriented languages as it speeds up leaf
  functions and possibly tail calls (thinking about that one).
+ Part of the function preamble now is to put the link register into an aloc.
+ We now do not have a halt instruction as the program body is treated as
  returning to the program end. This means we must setup the link register at
  the beginning of the program.
+ `R15` will be our link register.

# Non-tail call translation
I really don't like how the book proposes to load the return address into the
link register. Setting up a special case where we have a `return-point` node
with explicit labels that are put implicitly after the last statement and then
implicitly return `RAX` seems weird. Instead I propose the following.
```
(begin
  (set! ...)
  ...
  (set! ...)
  (return-point
   (jump ...))
  RAX)
   
```
This way makes everything more clear as well as gives us options on how to load
the return address. We could either do what the book does and put a label after
the jump, or we could do some RIP relative addressing to avoid that (I like this
idea better, although that may be just because I want to rebel against the
book). In any case all we have to do is have jump become a statement and add a
statement construct of the form `data Stmt = ReturnPoint Stmt`. Actually a
better way may be to just separate jumps that return from tail jump (tail calls
and if statements) with a construct of the form `data Stmt = JumpRet Label [Loc]`
or some variation thereof. Either way this removes any special case from the
lowering procedure.

An explicitly returning jump would then take the form
```
(begin
  (set! ...)
  ...
  (set! ...)
  (jump-ret ...)
  RAX)
```
This is actually probably much better than a `return-point` construct as we know
the `return-point` would only ever wrap a jump (I think) and so we would be
implicitly creating a `JumpRet` anyways. Much better to make it explicit. This
also means we must gensym a label, however this is perfect as we already do
that! So we keep `JumpRet` until we lower into `Block` and then we just pull out
the code after the jump into a new block with the return label. Funnily enough
this is very similar to CPS-transform.

# Values v6
+ We "simply" add a call construct as a value.
+ Our tail and value constructs are now identical, however we will keep them
  distinct as is facilitates TCO.
+ We also add subtraction (simple).
+ The book also creates a new type "entry" which is essentially a tail which is
  blessed to be the top level of a function or program body.
  e.g. `data Block = Block Label Entry`, `data Program = Program [Block] Entry`.

# Asm
+ We need undead-out info to know which variables to save across calls. Therefore
  that must be done in this stage before register allocation. We will do it by
  splitting off the `conflicts` function into `saveCalls` and `conflicts` functions
  which will then be dispatched through an `analyze` function which will return
  the new AST, and the conflict graph nodes and edges. This will be split off into
  its own module. In the future we should look for a good way to just tag each
  node with its undead-out set and then split up these functions into separate
  stages. `Implementing Functional Programming Languages` may have some info on
  how to do this cleanly.
