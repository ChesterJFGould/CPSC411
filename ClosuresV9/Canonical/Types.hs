module Canonical.Types where

data Program = Program [Label] [Block] Body

data Block = Block Label Body

data Body = Body Tail

data Tail = Jump MTriv [Mloc]
          | TSeq [Stmt] Tail
          | TIf Pred Tail Tail

data Stmt = Set Mloc Expr
          | MSet MTriv MTriv Expr
          | JumpRet MTriv [Mloc]
          | If Pred [Stmt] [Stmt]

data Expr = Triv MTriv
          | BinOp BinOp MTriv MTriv
          | Alloc MTriv
          | MRef MTriv MTriv

data Pred = Bool Bool
          | RelOp RelOp MTriv MTriv
          | Not Pred
          | PSeq [Stmt] Pred
          | PIf Pred Pred Pred

