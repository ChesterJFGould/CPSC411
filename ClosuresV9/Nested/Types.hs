module Nested.Types where

import Compiler.Types

data Program = Program [Label] [Block] Body
             deriving Show

data Block = Block Label Body
           deriving Show

data Body = Body Tail
          deriving Show

data Tail = Jump RTriv
          | TSeq [Stmt] Tail
          | TIf Pred Tail Tail
          deriving Show

data Stmt = Set Rloc RTriv
          | NumOp NumOp Rloc RTriv
          | MRef Rloc RTriv RTriv
          | MSet RTriv RTriv RTriv
          | JumpRet RTriv
          | If Pred [Stmt] [Stmt]
          deriving Show

data Pred = Bool Bool
          | RelOp RelOp Rloc RTriv
          | Not Pred
          | PSeq [Stmt] Pred
          | PIf Pred Pred Pred
          deriving Show
