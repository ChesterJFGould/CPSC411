module Undead.Types where

import Compiler.Types

import Data.Set
import Data.Word

type Tagged tag val = (tag, val)

type Undead val = Tagged (Set Mloc) val

data Program = Program [Block] TBody

data Block = Block Label TBody

type TBody = Tagged (Set Aloc, Set (Mloc, Mloc)) Body

data Body = Body Tail

data Tail = Seq [TStmt] Tail
          | TIf Pred Tail Tail
          | Jump MPlace [Mloc]

type TStmt = Undead Stmt

data Stmt = Set Mloc MTriv
          | BinOp BinOp Mloc MTriv
          | If Pred [TStmt] [TStmt]
          | JumpRet Label [Mloc]

data Pred = Bool
          | RelOp RelOp Mloc MTriv
          | Not Pred
          | PSeq [TStmt] Pred
          | PIf Pred Pred Pred
