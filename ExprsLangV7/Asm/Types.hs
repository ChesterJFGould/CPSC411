module Asm.Types where

import Compiler.Types

import Data.Word

data Program = Program [Block] Body
             deriving Show

data Block = Block Label Body
           deriving Show

data Body = Body Tail
          deriving Show

data Tail = Seq [Stmt] Tail
          | TIf Pred Tail Tail
          | Jump MPlace [Mloc]
          deriving Show

data Stmt = Set Mloc MTriv
          | BinOp BinOp Mloc MTriv
          | If Pred [Stmt] [Stmt]
          | JumpRet Label [Mloc]
          deriving Show

data Pred = Bool Bool
          | RelOp RelOp Mloc MTriv
          | Not Pred
          | PSeq [Stmt] Pred
          | PIf Pred Pred Pred
          deriving Show
