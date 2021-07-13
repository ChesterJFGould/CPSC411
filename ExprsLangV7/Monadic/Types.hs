module Monadic.Types where

import Compiler.Types

import Data.Word

data Program = Program [Block] Body
             deriving Show

data Block = Block Label Body
           deriving Show

data Body = Body Tail
          deriving Show
        
data Tail = Expr Expr
          | TSeq [Stmt] Tail
          | TIf Pred Tail Tail
          | Jump Label [Mloc]
          deriving Show

data Expr = Triv MTriv
          | BinOp BinOp MTriv MTriv
          | Seq [Stmt] Expr
          | If Pred Expr Expr
          deriving Show

data Pred = Bool Bool
          | RelOp RelOp MTriv MTriv
          | Not Pred
          | PSeq [Stmt] Pred
          | PIf Pred Pred Pred
          deriving Show

data Stmt = Set Mloc Expr
          | JumpRet Label [Mloc]
          deriving Show
