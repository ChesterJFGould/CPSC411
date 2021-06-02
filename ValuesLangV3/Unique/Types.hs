module Unique.Types where

import Data.Int

data Program = Program { value :: Expr
                       , gensymIndex :: Int }
             deriving Show

data Expr = Triv Triv
          | BinOp Op Triv Triv
          | Let [(Aloc, Expr)] Expr
          deriving Show

data Op = Add
        | Mul
        deriving Show

data Triv = Int Int64
          | Aloc Aloc
          deriving Show

data Aloc = Var String Int
          deriving Show
