module Nested.Types where

import Compiler.Types

import Data.Word

data Program = Program [Block] Body

data Block = Block Label Body

data Body = Body Tail

data Tail = Seq [Stmt] Tail
          | TIf Pred Tail Tail
          | Jump RPlace

data Stmt = Set Rloc RTriv
          | BinOp Rloc RTriv
          | If Pred [Stmt] [Stmt]
          | JumpRet Label

data Pred = Bool Bool
          | RelOp RelOp RTriv RTriv
          | Not Pred
          | PSeq [Stmt] Pred
          | PIf Pred Pred Pred
