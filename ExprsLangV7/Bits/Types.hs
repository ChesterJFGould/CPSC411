module Bits.Types where

import Compiler.Types

import Data.Word

data Program = Program [Func] Body
             deriving Show

data Func = Func Label [Aloc] Body
          deriving Show

data Body = Body Expr
          deriving Show

data Expr = Triv ATriv
          | BinOp BinOp Expr Expr
          | Apply Label [Expr]
          | Let [(Aloc, Expr)] Expr
          | If Pred Expr Expr
          deriving Show

data Pred = Bool Bool
          | RelOp RelOp Expr Expr
          | Not Pred
          | PLet [(Aloc, Expr)] Pred
          | PIf Pred Pred Pred
          deriving Show
