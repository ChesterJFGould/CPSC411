module Monadic.Types where

import Data.Int

data Program = Program Expr
             deriving Show

data Expr = Triv Triv
          | BinOp Op Triv Triv
          | If Pred Expr Expr
          | Seq [Stmt] Expr
          deriving Show

data Op = Add
        | Mul
        deriving Show

data Stmt = Set Aloc Expr
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
