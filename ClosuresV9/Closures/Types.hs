module Closures.Types where

import Compiler.Types

data Program = Program [Def] Body
             deriving Show

data Def = Const Label Body
         | Func Label Aloc Aloc Body
         deriving Show

data Body = Body Expr
          deriving Show

data Expr = Value Value
          | BinOp BinOp Expr Expr
          | ApplyClosure Expr Expr
          | Let Aloc Expr Expr
          | Closure Label [Expr]
          | ClosureRef Aloc Int
          | If Expr Expr Expr
          deriving Show
