module Blocks.Types where

data Program = Program [Label] [Block] Body

data Block = Block Label Body

data Body = Body Tail

data Tail = Jump RTriv
          | Seq [Stmt] Tail
          | If Pred Label Label

data Stmt = Set Rloc RTriv
          | NumOp NumOp Rloc RTriv
          | MRef Rloc RTriv RTriv
          | MSet RTriv RTriv RTriv

data Pred = RelOp RelOp Rloc RTriv
