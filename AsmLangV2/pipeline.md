# Note on register abstraction
Since we are specifically compiling to x64, pretending we don't know which
registers will be used as "auxiliary" or as a frame pointer seems a bit like
premature generalization. It's probably better to just hard code these in the
spec and then focus on the best manner in which to generate code for multiple
ISAs later instead of pretending right now that we are also going to write a
code gen for arm or whatever. If we were actually concerned about generating
code for a different ISA at this stage we wouldn't be still specifying specific
x86 register names in the spec, and probably be using some abstract MTM.

# Statement Nesting
We're just going to ignore that the book wants statements to be able to be
composed of arbitrarily nested blocks. Since for now these have no semantic
relevance and are simply flattened later on in the pipeline it seems they
might just be a product of the book using a Lisp.

# Outline
`Asm-lang -> Para-asm -> Paren-x64 -> x64` should probably work.

# Paren-x64 v2
+ We essentially give Paren-x64 v1 a stack via `QWORD [rbp - <offset>]`.
+ Offset must be a multiple of 8 so we just represent it as any int and then
  multiply it by 8 on code gen.
+ We get an new type `address` which we represent as an offset from the frame
  pointer.
+ Set gets to use either an address for its destination xor source.
+ Binops get to an address for its source but still only a register for its
  destination.
+ The book specifies that we should abstract the frame pointer to be any
  arbitrary register but also that we should never touch this besides
  when used via an address essentially. It is probably a good idea to just
  statically enforce this by just specifying that RBP will always be used as
  the frame pointer and then remove it as a general register.
+ Although since this stage will be generated we could also just make sure
  previous code gens create correct code wrt the frame pointer. This would let
  us mess with the frame pointer if it makes sense.

# Paren-x64-fvars v2
+ Only change is we abstract an address to be an "frame variable" which
  essentially designates a word offset from the frame pointer.
+ However this is exactly the same thing we decided to do above by representing
  an address as any int and then multiplying by 8 during code gen.
+ Therefore this stage can probably just be skipped.

# Para-asm v2
+ This stage basically abstracts the destination of all ops so it can be a
  register or stack offset (stack variable).
+ It also guarentees all programs end with a special "halt <triv>" instruction
  so we are guarenteed to return a val.
+ This stage also removes the r10 and r11 registers so we can use them to store
  intermediate values.
+ The book specifies we should abstract out which registers become "auxiliary".
  This probably seems like a better idea when you're in an dynamically typed
  (sorry "gradually typed") language and using strings (symbols) to represent
  registers. Similarily to abstracting the frame pointer in the base lang we
  should probably just specify which registers we are using for auxiliary and
  stick with those.

# Nested-Asm v2
+ The book is terrible at standardizing these language names.
+ Programs can now be composed of arbitrarily nested instruction sequences.
e.g.
```
(begin (begin (set! r9 10))
       (begin (begin (set! r12 r9) (set! r13 r12))
              (halt r13)))
```
+ Code gen literally just flattens out the program.
+ This stage can maybe just be avoided by embedding the code gen in the Writer
  monad.

# Asm-lang v2
+ This stage removes all specific locations (registers and stack offsets) and
  replaces them with "abstract locations" (alocs). In the book these take the
  form "<name>.<number>" however we can just represent them as a
  (Var String Int) or something similar. The number is there to fasciliate gensym?
+ Since at this stage we aren't going to be gensymming the variable names but
  simply parsing them, and since we're gonna rewrite this stage for the next
  compiler anyways, we'll probably just use a String instead of String Int pair.

## Asm-lang v2/locals
+ This "stage" just adds the set of all used alocs to the program as meta-data.
+ We absolutely do not need to represent this as some separate stage. We can
  just have some func (in the Writer monad?) that runs over the tree and spits
  out all the symbols and then chucks them in a set.

## Asm-lang v2/assignments
+ Same as before this just adds meta-data, in particular the stack offset
  corresponding to each individual aloc.
+ Just as before we can just generate this after genning the locals info by
  just `zip (toList locals) [0..]`.
