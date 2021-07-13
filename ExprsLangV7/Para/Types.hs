module Para.Types where

import Compiler.Types

import Data.Word

data Program = Program [Stmt]
             deriving Show

data Stmt = Set Rloc RTriv
          | BinOp BinOp Rloc RTriv
          | Compare Rloc RTriv
          | JumpIf RelOp Label
          | Jump RPlace
          | Labelled Label Stmt
          deriving Show
