module Nested.Types where

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
          | Jump RPlace
          deriving Show

data Stmt = Set Rloc RTriv
          | BinOp BinOp Rloc RTriv
          | If Pred [Stmt] [Stmt]
          | JumpRet Label
          deriving Show

data Pred = Bool Bool
          | RelOp RelOp Rloc RTriv
          | Not Pred
          | PSeq [Stmt] Pred
          | PIf Pred Pred Pred
          deriving Show
