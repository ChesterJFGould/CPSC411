module Undead.Types where

import Compiler.Types

import Data.Set

data Program = Program [Label] [Block] Body
             deriving Show

data Block = Block Label Body
           deriving Show

data Body = Body Tail
          deriving Show

data Tail = Jump MTriv [Mloc]
          | TSeq [TStmt] Tail
          | TIf Pred Tail Tail
          deriving Show

type TStmt = (Set Mloc, Stmt)

data Stmt = Set Mloc MTriv
          | NumOp NumOp Mloc MTriv
          | MRef Mloc MTriv MTriv
          | MSet MTriv MTriv MTriv
          | JumpRet MTriv [Mloc]
          | If Pred [TStmt] [TStmt]
          deriving Show

data Pred = Bool Bool
          | RelOp RelOp Mloc MTriv
          | Not Pred
          | PSeq [TStmt] Pred
          | PIf Pred Pred Pred
          deriving Show
