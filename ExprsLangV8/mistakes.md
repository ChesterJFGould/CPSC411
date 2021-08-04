+ The way memory is handled is completely rediculous. Memory cannot, for
  starters, be treated as a whole bunch of registers (spilling must be performed).
  There is also now a situation in which memory can be addressed by the addition
  of two labels which nasm DOES NOT SUPPORT, i.e. our compiler can generate code
  which WILL NOT COMPILE. Luckily I'm pretty sure our compiler will not generate
  such code, however the types SHOULD GUARENTEE THIS!!!
+ We can ducktape the above by loading labels into temporary registers
+ We can maybe separate the label from the triv type by having a separate "load
  code pointer" form? <- This won't be so easy with first class functions.
+ Shift right only takes literals and the cl register so we should probably
  just get rid of it. Except we can't do this without div.
+ The biggest mistake was using tagged data types instead of a proper type system...
