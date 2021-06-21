module Nested.Types where

data Program = Program [Block] Tail

data Block = Block Label Tail

data Tail = Halt Triv
          | TSeq [Stmt] Tail
          | TIf Pred Tail Tail
          | Jump Label

data Stmt = Set Loc Triv
          | BinOp Op Loc Triv
          | If Pred [Stmt] [Stmt]

data Pred = Bool Bool
          | RelOp RelOp Loc Triv
          | Not Pred
          | PSeq [Stmt] Pred
          | PIf Pred Pred Pred

data Triv = Int Integer
          | Loc Loc

data Loc = Reg Reg
         | Addr Int
         deriving (Eq, Ord)

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
         deriving (Eq, Ord, Show)

data Label = Label String

data Op = Add
        | Mul

data RelOp = Lt
           | Gt
           | Eq
           | Lte
           | Gte
           | Neq
           deriving Show
