module Cmf.Types where

import Data.Int

data Program = Program { prog :: ([Stmt], Expr)
                       , gensymIndex :: Int }
             deriving Show

data Stmt = Set Aloc Expr
          deriving Show

data Expr = Triv Triv
          | BinOp Op Triv Triv
          deriving Show

data Op = Add
        | Mul
        deriving Show

data Triv = Int Int64
          | Aloc Aloc
          deriving Show

data Aloc = Var String Int
          deriving Show
