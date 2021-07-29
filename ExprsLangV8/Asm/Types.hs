module Asm.Types where

import Compiler.Types

data Program = Program [Block] Body
             deriving Show

data Block = Block Label Body
           deriving Show

data Body = Body Tail
          deriving Show

data Tail = Jump MPlace [Mloc]
          | Seq [Stmt] Tail
          | TIf Pred Tail Tail
          deriving Show

data Stmt = Set Mloc MTriv
          | BinOp BinOp Mloc MTriv
          | MSet MTriv MTriv MTriv
          | MRef Mloc MTriv MTriv
          | JumpRet Label [Mloc]
          | If Pred [Stmt] [Stmt]
          deriving Show

data Pred = Bool Bool
          | RelOp RelOp Mloc MTriv
          | Not Pred
          | PSeq [Stmt] Pred
          | PIf Pred Pred Pred
          deriving Show
