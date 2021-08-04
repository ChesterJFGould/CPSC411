module Closures.Types where

data Program = Program [Def] Body

data Def = Const Label Body
         | Func Label Aloc Aloc Body

data Body = Body Expr

data Expr = Triv Triv
          | BinOp BinOp Expr Expr
          | ApplyClosure Expr Expr
          | Let Aloc Expr Expr
          | Closure Label [Expr]
          | ClosureRef Aloc Int
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
