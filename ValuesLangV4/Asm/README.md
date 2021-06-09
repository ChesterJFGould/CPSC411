# About the TARDIS Monad...
The lower functions for this stage use the TARDIS Monad, this lets us calculate
the undead in/out sets and do a linear scan register allocation in a single
pass.
Because of this some of the lower functions (like the lowerIf function) are
annoyingly complicated.
However the alternative would be to either make the AST types more complicated
by parameterizing them over some tag
(e.g. `data Stmt tag = Set tag Aloc Triv ...`) or add a new stage/language
where we just take the Asm AST types and annotate them all with the undead out
set at that instruction.
Using the TARDIS Monad seemed like a better solution than that so that's why
it's there.
