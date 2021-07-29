module Monadic.Types where

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

data Expr = Triv MTriv
          | BinOp BinOp MTriv MTriv
          | MRef MTriv MTriv
          | Alloc MTriv
          | Seq [Stmt] Expr
          | If Pred Expr Expr
          deriving Show

data Stmt = Set Mloc Expr
          | MSet MTriv MTriv Expr
          | JumpRet Label [Mloc]
          deriving Show

data Pred = Bool Bool
          | RelOp RelOp MTriv MTriv
          | Not Pred
          | PIf Pred Pred Pred
          | PSeq [Stmt] Pred
          deriving Show
