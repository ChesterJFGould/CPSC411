module Consts.Types where

data Program = Program [Label] [Func] Body

data Func = Func Label Aloc Aloc Body

data Body = Body Expr

data Expr = Triv ATriv
          | BinOp BinOp Expr Expr
          | Apply Expr Expr Expr
          | Let Aloc Expr Expr
          | Alloc Expr
          | MRef Expr Expr
          | If Pred Expr Expr
          | Seq [Stmt] Expr

data Stmt = MSet Expr Expr Expr

data Pred = Bool Bool
          | RelOp RelOp Expr Expr
          | Not Pred
          | PLet Aloc Expr Pred
          | PIf Pred Pred Pred
