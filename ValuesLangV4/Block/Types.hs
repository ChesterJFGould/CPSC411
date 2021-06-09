module Block.Types where

import Data.Int

data Program = Program [Block] Label
             deriving Show

data Block = Block Label Top
           deriving Show

data Top = Halt Triv
         | Jump Label
         | Seq [Stmt] Top
         | If Pred Label Label
         deriving Show

data Pred = RelOp RelOp Loc Triv
          deriving Show

data RelOp = Lt
           | Gt
           | Eq
           | Lte
           | Gte
           | Neq
           deriving Show

data Stmt = Set Loc Triv
          | BinOp Op Loc Triv
          deriving Show

data Op = Add
        | Mul
        deriving Show

data Triv = Int Int64
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

data Label = Label String Int
           deriving Show
