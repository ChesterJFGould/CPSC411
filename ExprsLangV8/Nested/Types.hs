module Nested.Types where

import Compiler.Types

data Program = Program [Block] Body

data Block = Block Label Body

data Body = Body Tail

data Tail = Seq [Stmt] Tail
          | TIf Pred Tail Tail
          | Jump RPlace

data Stmt = Set Rloc RTriv
          | BinOp BinOp Rloc RTriv
          | MSet Rloc RTriv RTriv
          | MRef Rloc Rloc RTriv
          | JumpRet Label
          | If Pred [Stmt] [Stmt]

data Pred = Bool Bool
          | RelOp RelOp Rloc RTriv
          | Not Pred
          | PSeq [Stmt] Pred
          | PIf Pred Pred Pred
