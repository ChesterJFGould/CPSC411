module Undead.Types where

import Data.Set

data Program = Program [Label] [Block] Body

data Block = Block Label Body

data Body = Body Tail

data Tail = Jump MTriv [Mloc]
          | TSeq [TStmt] Tail
          | TIf Pred Tail Tail

type TStmt = (Set Mloc, Stmt)

data Stmt = Set Mloc MTriv
          | BinOp BinOp Mloc MTriv
          | MRef Mloc MTriv MTriv
          | MSet MTriv MTriv MTriv
          | JumpRet MTriv [Mloc]
          | If Pred [TStmt] [TStmt]

data Pred = Bool Bool
          | RelOp RelOp Mloc MTriv
          | Not Pred
          | PSeq [TStmt] Pred
          | PIf Pred Pred Pred
