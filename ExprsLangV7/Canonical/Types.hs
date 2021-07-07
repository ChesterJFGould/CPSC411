module Canonical.Types where

import Compiler.Types

import Data.Word

data Program = Program [Block] Body

data Block = Block Label Body

data Body = Body Aloc Tail

data Tail = Expr Expr
          | Seq [Stmt] Tail
          | TIf Pred Tail Tail
          | Jump Label [Mloc]

data Stmt = Set Mloc Expr
          | If Pred [Stmt] [Stmt]
          | JumpRet Label [Mloc]

data Expr = Triv MTriv
          | BinOp BinOp MTriv MTriv

data Pred = Bool Bool
          | RelOp RelOp MTriv MTriv
          | Not Pred
          | PSeq [Stmt] Pred
          | PIf Pred Pred Pred
