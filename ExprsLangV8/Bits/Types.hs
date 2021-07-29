module Bits.Types where

import Compiler.Types

data Program = Program [Func] Body
             deriving Show

data Func = Func Label [Aloc] Body
          deriving Show

data Body = Body Expr
          deriving Show

data Expr = Triv ATriv
          | BinOp BinOp Expr Expr
          | MRef Expr Expr
          | Alloc Expr
          | Apply Label [Expr]
          | Let [(Aloc, Expr)] Expr
          | If Pred Expr Expr
          | Seq [Stmt] Expr
          deriving Show

data Stmt = MSet Expr Expr Expr
          deriving Show

data Pred = Bool Bool
          | RelOp RelOp Expr Expr
          | Not Pred
          | PLet [(Aloc, Expr)] Pred
          | PIf Pred Pred Pred
          | PSeq [Stmt] Pred
          deriving Show
