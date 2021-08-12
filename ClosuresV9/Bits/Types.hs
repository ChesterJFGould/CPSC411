module Bits.Types where

import Compiler.Types

data Program = Program [Def] Body
             deriving Show

data Def = Const Label Body
         | Func Label Aloc Aloc Body
         deriving Show

data Body = Body Expr
          deriving Show

data Expr = Triv ATriv
          | NumOp NumOp Expr Expr
          | Apply Expr Expr Expr
          | Let Aloc Expr Expr
          | Alloc Expr
          | MRef Expr Expr
          | If Pred Expr Expr
          | Seq [Stmt] Expr
          deriving Show

data Stmt = MSet Expr Expr Expr
          deriving Show

data Pred = Bool Bool
          | RelOp RelOp Expr Expr
          | Not Pred
          | PLet Aloc Expr Pred
          | PIf Pred Pred Pred
          deriving Show
