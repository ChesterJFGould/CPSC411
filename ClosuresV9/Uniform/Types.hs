module Uniform.Types where

data Program = Program [Def] Body

data Def = Def Label Body

data Body = Body Expr

data Expr = Triv Triv
          | Apply Expr Expr
          | Let Aloc Expr Expr
          | Lambda Aloc Expr
          | If Expr Expr Expr
          | BinOp BinOp Expr Expr

data Triv = Int Integer
          | Bool Bool
          | TAloc Aloc

data BinOp = Add
           | Sub
           | Mul
           | Lt
           | Gt
           | Eq
           | Lte
           | Gte
           | Neq
