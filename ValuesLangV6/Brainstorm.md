x := 5
y := 6
RDI := x
RSI := y |
JumpRet add [RDI, RSI] | [RAX, y, x]
z := RAX | [z, y, x]
z += x | [z, y]
z += y | []

->

x := 5 | [x]
y := 6 | [x, y]
RDI := x | [RDI, x, y]
RSI := y | [RDI, RSI, x, y]
RBP - 0 := y | [RBP - 0, RDI, RSI, x]
RBP - 8 := x | [RBP - 8, RBP - 0, RDI, RSI]
RBP -= 16 [RBP - 8, RBP - 0, RDI, RSI]
JumpRet add [RDI, RSI] | [RAX, RBP - 8, RBP - 0]
RBP += 16 | [RAX, RBP - 8, RBP - 0]
y := RBP - 0 | [y, RAX, RBP - 8]
x := RBP - 8 | [y, x, RAX]
z := RAX | [z, y, x]
z += x | [z, y]
z += y | []
