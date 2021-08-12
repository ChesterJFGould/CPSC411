module Values.Types where

import Compiler.Types

data Program = Program [Label] [Func] Body
             deriving Show

data Func = Func Label Aloc Aloc Body
          deriving Show

data Body = Body Expr
          deriving Show

data Expr = Triv ATriv
          | NumOp NumOp ATriv ATriv
          | Apply ATriv ATriv ATriv
          | Let Aloc Expr Expr
          | Alloc ATriv
          | MRef ATriv ATriv
          | If Pred Expr Expr
          | Seq [Stmt] Expr
          deriving Show

data Stmt = MSet ATriv ATriv Expr
          | SLet Aloc Expr Stmt
          deriving Show

data Pred = Bool Bool
          | RelOp RelOp ATriv ATriv
          | Not Pred
          | PLet Aloc Expr Pred
          | PIf Pred Pred Pred
          deriving Show
