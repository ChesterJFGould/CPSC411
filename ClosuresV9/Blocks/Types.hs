module Blocks.Types where

import Compiler.Types

data Program = Program [Label] [Block] Body
             deriving Show

data Block = Block Label Body
           deriving Show

data Body = Body Tail
          deriving Show

data Tail = Jump RTriv
          | Seq [Stmt] Tail
          | If Pred Label Label
          deriving Show

data Stmt = Set Rloc RTriv
          | NumOp NumOp Rloc RTriv
          | MRef Rloc RTriv RTriv
          | MSet RTriv RTriv RTriv
          deriving Show

data Pred = RelOp RelOp Rloc RTriv
          deriving Show
