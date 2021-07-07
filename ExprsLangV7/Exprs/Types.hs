module Exprs.Types where

import Compiler.Types
import Compiler.Value

import Data.Word

data Program = Program [Func] Expr
             deriving Show

data Func = Func Var [Var] Expr
          deriving Show

data Expr = Triv Triv
          | Apply Var [Expr]
          | Let [(Var, Expr)] Expr
          | If Expr Expr Expr
          deriving Show

data Triv = TVar Var
          | Value Value
          deriving Show

data Var = Var String
         deriving (Eq, Ord, Show)
