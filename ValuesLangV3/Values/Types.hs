module Values.Types where

import Data.Int

data Program = Program Expr
             deriving Show

data Expr = Triv Triv
          | BinOp Op Triv Triv
          | Let [(Var, Expr)] Expr
          deriving Show

data Op = Add
        | Mul
        deriving Show

data Triv = Int Int64
          | TVar Var
          deriving Show

data Var = Var String
         deriving (Eq, Ord, Show)
