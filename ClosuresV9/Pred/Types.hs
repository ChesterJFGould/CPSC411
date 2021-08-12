module Pred.Types where

import Compiler.Types

data Program = Program [Def] Body
             deriving Show

data Def = Const Label Body
         | Func Label Aloc Aloc Body
         deriving Show

data Body = Body Expr
          deriving Show

data Expr = Value Value
          | NumOp NumOp Expr Expr
          | ApplyClosure Expr Expr
          | Let Aloc Expr Expr
          | Closure Label [Expr]
          | ClosureRef Aloc Int
          | If Pred Expr Expr
          deriving Show

data Pred = Bool Bool
          | RelOp RelOp Expr Expr
          | Not Pred
          | PLet Aloc Expr Pred
          | PIf Pred Pred Pred
          deriving Show
