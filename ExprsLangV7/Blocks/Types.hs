module Blocks.Types where

import Compiler.Types

import Data.Word

data Program = Program [Block] Body
             deriving Show

data Block = Block Label Body
           deriving Show

data Body = Body Tail
          deriving Show

data Tail = Seq [Stmt] Tail
          | If Pred Label Label
          | Jump RPlace
          deriving Show

data Stmt = Set Rloc RTriv
          | BinOp BinOp Rloc RTriv
          deriving Show

data Pred = RelOp RelOp Rloc RTriv
          deriving Show
