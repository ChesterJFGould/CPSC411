module Asm.Types where

import Data.Int

data Program = Program Top
             deriving Show

data Top = Halt Triv
         | Seq [Stmt] Top
         | TIf Pred Top Top
         deriving Show

data Stmt = Set Aloc Triv
          | BinOp Op Aloc Triv
          | If Pred [Stmt] [Stmt]
          deriving Show

data Op = Add
        | Mul
        deriving Show

data Pred = Bool Bool
          | RelOp RelOp Aloc Triv
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
          | TAloc Aloc
          deriving Show

data Aloc = Aloc String Int
          deriving (Eq, Ord, Show)
