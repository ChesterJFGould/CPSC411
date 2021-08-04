module Values.Types where

data Program = Program [Label] [Func] Body

data Func = Func Label Aloc Aloc Body

data Body = Body Expr

data Expr = Triv ATriv
          | BinOp BinOp ATriv ATriv
          | Apply ATriv ATriv ATriv
          | Let Aloc Expr Expr
          | Alloc ATriv
          | MRef ATriv ATriv
          | If Pred Expr Expr
          | Seq [Stmt] Expr

data Stmt = MSet ATriv ATriv Expr
          | SLet Aloc Expr Stmt

data Pred = Bool Bool
          | RelOp RelOp ATriv ATriv
          | Not Pred
          | PLet Aloc Expr Pred
          | PIf Pred Pred Pred
