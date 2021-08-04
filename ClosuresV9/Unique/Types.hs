module Unique.Types where

data Program = Program [Def] Body

data Def = Def Label [Aloc] Body

data Body = Body Expr

data Expr = Triv Triv
          | BinOp BinOp Expr Expr
          | Apply Expr Expr
          | Let Aloc Expr Expr
          | Lambda Aloc Expr
          | If Expr Expr Expr

data Triv = Int Integer
          | Bool Bool
          | TAloc Aloc
          | TLabel Label

data BinOp = Add
           | Sub
           | Mul
           | Lt
           | Gt
           | Eq
           | Lte
           | Gte
           | Neq
