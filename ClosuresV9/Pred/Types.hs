module Pred.Types where

data Program = Program [Def] Body

data Def = Const Label Body
         | Func Label Aloc Aloc Body

data Body = Body Expr

data Expr = Triv Triv
          | BinOp BinOp Expr Expr
          | ApplyClosure Expr Expr
          | Let Aloc Expr Expr
          | Closure Label [Expr]
          | ClosureRef Expr Int
          | If Pred Expr Expr

data Pred = Bool Bool
          | RelOp RelOp Expr Expr
          | Not Pred
          | PLet Aloc Expr Pred
          | PIf Pred Pred Pred

data Triv = TInt Integer
          | TBool Bool
          | TAloc Aloc
          | TLabel Label
