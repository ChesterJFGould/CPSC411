module Canonical.Types where

data Program = Program [Block] Tail
             deriving Show

data Block = Block Label Tail
           deriving Show

data Tail = Expr Expr
          | TSeq [Stmt] Tail
          | TIf Pred Tail Tail
          | Jump Label [Loc]
          deriving Show

data Expr = Triv Triv
          | BinOp Op Triv Triv
          deriving Show

data Stmt = Set Loc Expr
          | If Pred [Stmt] [Stmt]
          deriving Show

data Pred = Bool Bool
          | RelOp RelOp Triv Triv
          | Not Pred
          | PSeq [Stmt] Pred
          | PIf Pred Pred Pred
          deriving Show

data Triv = Int Integer
          | TLoc Loc
          deriving Show

data Loc = Aloc String Int
         | Reg Reg
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
