module Asm.Types where

import Compiler.Types

import Data.Word

data Program = Program [Block] Body

data Block = Block Label Body

data Body = Body Tail

data Tail = Seq [Stmt] Tail
          | TIf Pred Tail Tail
          | Jump MPlace [Mloc]

data Stmt = Set Mloc MTriv
          | BinOp BinOp Mloc MTriv
          | If Pred [Stmt] [Stmt]
          | JumpRet Label [Mloc]

data Pred = Bool Bool
          | RelOp RelOp Mloc MTriv
          | Not Pred
          | PSeq [Stmt] Pred
          | PIf Pred Pred Pred
