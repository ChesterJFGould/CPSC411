module Undead.Types where

import Compiler.Types

import Data.Set
import Data.Word

data Program = Program [Block] Body
             deriving Show

data Block = Block Label Body
           deriving Show

data Body = Body Tail
          deriving Show

data Tail = Seq [TStmt] Tail
          | TIf Pred Tail Tail
          | Jump MPlace [Mloc]
          deriving Show

type TStmt = (Set Mloc, Stmt)

data Stmt = Set Mloc MTriv
          | BinOp BinOp Mloc MTriv
          | If Pred [TStmt] [TStmt]
          | JumpRet Label [Mloc]
          deriving Show

data Pred = Bool Bool
          | RelOp RelOp Mloc MTriv
          | Not Pred
          | PSeq [TStmt] Pred
          | PIf Pred Pred Pred
          deriving Show
