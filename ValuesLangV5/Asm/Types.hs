module Asm.Types where

data Program = Program [Block] Tail
             deriving Show

data Block = Block Label Tail
           deriving Show

data Tail = Halt Triv
          | TSeq [Stmt] Tail
          | TIf Pred Tail Tail
          | Jump Label [Loc]
          deriving Show

data Stmt = Set Loc Triv
          | BinOp Op Loc Triv
          | If Pred [Stmt] [Stmt]
          deriving Show

data Pred = Bool Bool
          | RelOp RelOp Loc Triv
          | Not Pred
          | PSeq [Stmt] Pred
          | PIf Pred Pred Pred
          deriving Show

data Triv = Int Integer
          | Loc Loc
          deriving Show

data Loc = LAloc Aloc
         | LRloc Rloc
         deriving (Eq, Ord, Show)

data Aloc = Aloc String Int
          deriving (Eq, Ord, Show)

data Rloc = Reg Reg
          | Addr Int
          deriving (Eq, Ord, Show)

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
