module Block.Types where

data Program = Program [Block] Tail
             deriving Show

data Block = Block Label Tail
           deriving Show

data Tail = Halt Triv
          | Seq [Stmt] Tail
          | If Pred Label Label
          | Jump Label
          deriving Show

data Stmt = Set Loc Triv
          | BinOp Op Loc Triv
          deriving Show

data Pred = RelOp RelOp Loc Triv
          deriving Show

data Triv = Int Integer
          | Loc Loc
          deriving Show

data Loc = Reg Reg
         | Addr Int
         deriving Show

data Reg = RSP
         | RBP
         | RAX
         | RBX
         | RCX
         | RDX
         | RSI
         | RDI
         | R8
         | R9
         | R10
         | R11
         | R12
         | R13
         | R14
         | R15
         deriving Show

data Label = Label String
           deriving Show

data Op = Add
        | Mul
        deriving Show

data RelOp = Lt
           | Gt
           | Eq
           | Lte
           | Gte
           | Neq
           deriving Show
