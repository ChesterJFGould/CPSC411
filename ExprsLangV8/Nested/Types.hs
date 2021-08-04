module Nested.Types where

import Compiler.Types

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
          | MSet RTriv RTriv RTriv
          | MRef Rloc RTriv RTriv
          | JumpRet Label
          | If Pred [Stmt] [Stmt]
          deriving Show

data Pred = Bool Bool
          | RelOp RelOp Rloc RTriv
          | Not Pred
          | PSeq [Stmt] Pred
          | PIf Pred Pred Pred
          deriving Show
