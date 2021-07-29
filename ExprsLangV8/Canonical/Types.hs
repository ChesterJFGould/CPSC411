module Canonical.Types where

import Compiler.Types

data Program = Program [Block] Body
             deriving Show

data Block = Block Label Body
           deriving Show

data Body = Body Tail
          deriving Show

data Tail = Jump MPlace [Mloc]
          | TSeq [Stmt] Tail
          | TIf Pred Tail Tail
          deriving Show

data Stmt = Set Mloc Expr
          | MSet MTriv MTriv Expr
          | JumpRet Label [Mloc]
          | If Pred [Stmt] [Stmt]
          deriving Show

data Expr = Triv MTriv
          | BinOp BinOp MTriv MTriv
          | MRef MTriv MTriv
          | Alloc MTriv
          deriving Show

data Pred = Bool Bool
          | RelOp RelOp MTriv MTriv
          | Not Pred
          | PSeq [Stmt] Pred
          | PIf Pred Pred Pred
          deriving Show
