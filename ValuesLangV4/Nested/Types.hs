module Nested.Types where

import Data.Int

data Program = Program Top
             deriving Show

data Top = Halt Triv
         | Seq [Stmt] Top
         | TIf Pred Top Top
         deriving Show

data Stmt = Set Loc Triv
          | BinOp Op Loc Triv
          | If Pred [Stmt] [Stmt]
          deriving Show

data Op = Add
        | Mul
        deriving Show

data Pred = Bool Bool
          | RelOp RelOp Loc Triv
          | Not Pred
          | PSeq [Stmt] Pred
          | PIf Pred Pred Pred
          deriving Show

data RelOp = Lt
           | Gt
           | Eq
           | Lte
           | Gte
           | Neq
           deriving Show

data Triv = Int Int64
          | Loc Loc
          deriving Show

data Loc = Reg Reg
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
