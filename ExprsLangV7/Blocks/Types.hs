module Blocks.Types where

import Compiler.Types

import Data.Word

data Program = Program [Block] Body

data Block = Block Label Body

data Body = Body Tail

data Tail = Seq [Stmt] Tail
          | If Pred Label Label
          | Jump RPlace

data Stmt = Set Rloc RTriv
          | BinOp BinOp Rloc RTriv

data Pred = RelOp Rloc RTriv
