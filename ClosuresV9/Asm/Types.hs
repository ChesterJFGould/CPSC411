module Asm.Types where

import Compiler.Types

data Program = Program [Label] [Block] Body
             deriving Show

data Block = Block Label Body
           deriving Show

data Body = Body Tail
          deriving Show

data Tail = Jump MTriv [Mloc]
          | TSeq [Stmt] Tail
          | TIf Pred Tail Tail
          deriving Show

data Stmt = Set Mloc MTriv
          | NumOp NumOp Mloc MTriv
          | MRef Mloc MTriv MTriv
          | MSet MTriv MTriv MTriv
          | JumpRet MTriv [Mloc]
          | If Pred [Stmt] [Stmt]
          deriving Show

data Pred = Bool Bool
          | RelOp RelOp Mloc MTriv
          | Not Pred
          | PSeq [Stmt] Pred
          | PIf Pred Pred Pred
          deriving Show
