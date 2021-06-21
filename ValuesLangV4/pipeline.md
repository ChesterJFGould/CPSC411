# Pipeline
`Values -> Unique -> Monadic -> Canonical -> Asm -> Nested -> --Pred-- -> Block -> Para -> Paren -> x86`.

# Paren-x64 v4
+ Extremely similar to v2, we just add labels, conditional and unconditional
  jumps, and comparisons.

# Para-x64 v4
+ Since we now have jumps there is no clear program endpoint (or indeed a single
  endpoint)so we can't just store the output of the program separately from the
  instruction sequence. This also means we must compile the halt instruction to
  a move and jump (move the output into RAX and then jump to the runtime label
  which prints RAX and exits).
+ We can compile computed conditional jumps (jump by a value stored in a reg or
  memory) by introducing a conditional jump that jumps over an unconditional
  jump if the condition is false.

# Block-asm v4
+ We essentially split the program into labelled "blocks" which are guarenteed
  to run from start to finish. i.e. jumps can only jump to labels and only
  appear at the end of blocks.
+ Jumps at the end of a block basically take the form of an if statement,
  however only a simple relop can appear as the predicate and the cons or alt
  paths are unconditional jumps. The end of a block can also be an unconditional
  jump instead of an if statement.
+ Still use physical locations (registers and addresses).

# Block-pred v4
+ Similar to Block-asm v4 however we add a pred type which can be a relop, bool
  constant, or negatation of a pred. These can only appear as the predicate in
  an if at the end of a block.
+ This language exists both to ease the compilation of higher level languages
  (relative to this one) as well as enable us to do some mandatory optimizations
  such as turning if statement which contain a constant bool into an
  unconditional jump.

# Nested-asm v4
+ We remove basic blocks and jumps, and allow if statements to contain
  arbitrarily nested branches. This means we also remove labels.
+ We get rid of the Not pred on this level as it is really easy to do (swap
  labels) and a pain to not do (pass through a Pred -> Pred function through all
  entire pred lowering function).

# Asm-pred v4
+ Now we finally replace physical locations with abstract locations. This is the
  stage in which register allocation will be performed.
+ Undead-out analysis now should split and generate a tree with each branch
  representing which branch was taken. As we are still using if statements and
  not expressions and if statement can't merge back into the same path.

# Imp-cmf v4
+ This stage just abstracts out values so they can be a simple operation between
  any alocs. Programs now also return a value instead of halting on a triv.

# Imp-mf v4
+ We now get if expressions as well as statements. The book also calls for a
  form that performs arbitrary effects and then returns a value, although I'm
  not sure if this is needed.

# Values-unique v4
+ Identical to Values v4 but with alocs instead of variables.

# Values v4
+ So along with an if statement we are essentially adding a type system without
  really acknowleding it.
+ We could probably just actually put a type system on the frontend which would
  greatly simplify the type declarations.
+ However unifying the Expr and Pred data types and then just using a type
  system to verify correctness does mean some lowering functions would have
  to not be total. e.g. lowering a let expression would involve just ignoring
  the boolean case as we are not storing booleans in variables yet.
+ It's probably easiest to stick with separate Expr and Pred types for now
  since unifying Expr and Pred makes it too tempting to just implement boolean
  valued variables and that opens a whole can of worms.
