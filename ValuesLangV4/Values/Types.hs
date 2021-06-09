module Values.Types where

import Data.Int

data Program = Program Expr
             deriving Show

data Expr = Triv Triv
          | BinOp Op Triv Triv
          | Let [(Var, Expr)] Expr
          | If Pred Expr Expr
          deriving Show

data Op = Add
        | Mul
        deriving Show

data Pred = Bool Bool
          | RelOp RelOp Triv Triv
          | Not Pred
          | PLet [(Var, Expr)] Pred
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
          | TVar Var
          deriving Show

data Var = Var String
         deriving (Eq, Ord, Show)
