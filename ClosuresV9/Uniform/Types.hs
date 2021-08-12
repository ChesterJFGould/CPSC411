module Uniform.Types where

import Compiler.Types

data Program = Program [Def] Body
             deriving Show

data Def = Def Label Body
         deriving Show

data Body = Body Expr
          deriving Show

data Expr = Value Value
          | Apply Expr Expr
          | Let Aloc Expr Expr
          | Lambda Aloc Expr
          | If Expr Expr Expr
          | BinOp BinOp Expr Expr
          deriving Show
