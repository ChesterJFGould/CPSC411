module Asm.Types where

data Program = Program [Label] [Block] Body

data Block = Block Label Body

data Body = Body Tail

data Tail = Jump MTriv [Mloc]
          | TSeq [Stmt] Tail
          | TIf Pred Tail Tail

data Stmt = Set Mloc MTriv
          | BinOp BinOp Mloc MTriv
          | MRef Mloc MTriv MTriv
          | MSet MTriv MTriv MTriv
          | JumpRet MTriv [Mloc]
          | If Pred [Stmt] [Stmt]

data Pred = Bool Bool
          | RelOp RelOp Mloc MTriv
          | Not Pred
          | PSeq [Stmt] Pred
          | PIf Pred Pred Pred
