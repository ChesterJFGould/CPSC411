# Pipeline
`text -> Values -> Unique -> Monadic -> Canonical -> Asm -> Nested -> Block -> Para -> Paren -> x86`.

# Values v5
+ A program now consists of many function definitions followed by a program
  body (supercombinators anyone?).
+ A program or function body (tail) can now contain a call to a function as its
  last value (tail value). This restriction means that all calls are just jumps
  (TCO) and so we don't need to worry about return values, saving locals, etc.

# Unique v5
+ Very similar to v4 but now we must split variables into labels (function names)
  and alocs (variable names).
+ The book does a really awful job of guarenteeing correctness through types, a
  call should (for now) only be able to call a label. The book says that
  theoretically you could call an int?

# Procedural Monadic v5
+ Same as always, we simple lower let expressions into a sequence of set
  statements.
+ However we keep tail calls

# Monadic v5
+ We now lower calls to a series of set statements into registers followed by a
  jump, as well as setting up the function arguments at the beginning of a
  function definition.
+ We can probably just merge this stage with Procedural Monadic v5 as lowering
  lets and imposing calling conventions should be simple to do at the same time
  through the power of structural induction.
+ Obviously (by the fact that we now can set registers) we now also have
  physical locations alongside alocs. The register alocation algorithm for this
  is beautiful. We also add a tail jump which is decorated with its
  undead-out set (in this case physical locations used to store arguments).

# Canonical v5
+ We now remove if and begin expressions so expressions now only consist of a
  triv or a binop.

# Asm v5
+ We now remove values from the language, every value must be replaced by a
  triv. e.g. `x = y + z` -> `x := z; x += y`.

# Pred v5
+ Register allocation time! Here's how it goes since we now have to impose
  restrictions on colouring due to the mixing of physical and abstract
  locations. When gathering up the conflicts we can also add that an abstract
  and physical location conflict (i.e. a node and a colour conflict) according
  to the exact same rules as before. In effect we pretend that physical
  locations are exactly the same as abstract locations (except we obviously
  don't colour them).
+ This means that our graph colouring needs to be able to accept an edge set
  that also contains colours.

# Nested v5
+ This stage is not needed as we do register allocation going from Asm -> Pred
+ In the book register allocation is done going from Asm -> Pred but the actual
  AST isn't updated for whatever reason. Even the book seems to be a bit
  confused about this.

# Block v5
+ We now remove all if statements and replace them with jumps to labels. In the
  book we still have if statemnts inside predicates, however it's trivial and
  infact more consistent to just remove them with all of the others.
+ Statements are now purely set statements and tails consist of a list of
  statements followed by a conditional or non-conditional jump, or a halt
  statement.

# Other Stages
+ All subsequent stages are the same as calls have been reduced to the same
  kind of jump as ifs.
