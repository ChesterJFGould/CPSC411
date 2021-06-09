module Canonical.Types where

import Data.Int

data Program = Program Top
             deriving Show

data Top = Expr Expr
         | TIf Pred Top Top
         | Seq [Stmt] Top
         deriving Show

data Expr = Triv Triv
          | BinOp Op Triv Triv
          deriving Show

data Op = Add
        | Mul
        deriving Show

data Stmt = Set Aloc Expr
          | If Pred [Stmt] [Stmt]
          deriving Show

data Pred = Bool Bool
          | RelOp RelOp Triv Triv
          | Not Pred
          | PIf Pred Pred Pred
          | PSeq [Stmt] Pred
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
          deriving Show
