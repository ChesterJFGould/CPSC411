module Nested.Types where

data Program = Program [Label] [Block] Body

data Block = Block Label Body

data Body = Body Tail

data Tail = Jump RTriv
          | TSeq [Stmt] Tail
          | TIf Pred Tail Tail

data Stmt = Set Rloc RTriv
          | NumOp NumOp Rloc RTriv
          | MRef Rloc RTriv RTriv
          | MSet RTriv RTriv RTriv
          | JumpRet RTriv
          | If Pred [Stmt] [Stmt]

data Pred = Bool Bool
          | RelOp RelOp Rloc RTriv
          | Not Pred
          | PSeq [Stmt] Pred
          | PIf Pred Pred Pred
