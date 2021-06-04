# Alocs
Alocs should now be a String Int pair as we will be gensymming them. Stages
that gensym should probably pass the last used number to the next stage so
if it needs to gensym it can be guarenteed to not overlap.

# Pipeline
`Values -> Cmf -> Asm -> Para -> Paren -> x64`?

# Imp-cmf-lang v3
+ Allows alocs to be assigned to the result of a binop allowing arbitrary
  operands, i.e. allowing `x := y + 3` instead of just `x += y`.
+ Halt value can now also be the result of a binop.
+ Variant of ANF
+ Compiles to Asm-lang v2

# Imp-mf-lang v3
+ This seems like another stage that is there to just provide nesting. However
  the book also calls it "Monadic Form" and says that it provides a way to
  compose pure values, however the language layed out doesn't seem to do that
  as none of the forms can create a value out of two other values? I think I'm
  missing something but we can probably just skip this stage like we skipped the
  other stages that simply exist to provide nesting.
+ The book also says that this language design probably won't make sense now but
  will make sense later, the good thing is that we are rewriting most of the
  compiler at each stage so we can probably just not include it now and then
  kick ourselves when we realize why its important later.
+ I realized where the composition comes in, values can be sequences of effects
  followed by a value. I guess if all the alocs in the effects are gensymmed
  then it makes sense to call these values "pure".

# Values-unique-lang v3
+ This stage uses alocs instead of actual variables so we don't get lexical
  scoping. This means that it is essentially equivalent to Imp-mf-lang v3 but
  with "parallel" assignment, i.e. assignment that is order independent. This
  will aparently make way for future optimizations, however let us not forget
  the cardinal sin of premature optimization. It might be possible to simply
  skip this stage and the next and just go to Imp-cmf-lang v3.
+ Maybe best to go through this stage so we're not juggling gensymming and
  lowering lets at the same time.

# Values-lang v3
+ Finally some lexical scoping, we now step out of macro assembler territory
  and into the realm of high level languages.
+ We still don't get arbitrarily nested binops.

# Para
+ We can probably just give this stage access to all the registers instead of
  removing them and adding them back in during the next stage since it is part
  of the current spec that R10 and R11 are used for intermediate values. This
  also brings the aux regs inline with how the frame pointer (RBP) is treated.
