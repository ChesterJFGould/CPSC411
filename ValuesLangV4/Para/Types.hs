module Para.Types where

import Data.Int

data Program = Program [Stmt]
             deriving Show

data Stmt = Halt Triv
          | Set Loc Triv
          | BinOp Op Loc Triv
          | Jump Label
          | Labelled Label Stmt
          | Compare Loc Triv
          | JumpIf RelOp Label
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

data Label = Label String Int
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
