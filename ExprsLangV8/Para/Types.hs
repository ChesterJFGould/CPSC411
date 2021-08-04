module Para.Types where

import Compiler.Types

data Program = Program [Stmt]
             deriving Show

data Stmt = Set Rloc RTriv
          | BinOp BinOp Rloc RTriv
          | MSet RTriv RTriv RTriv
          | MRef Rloc RTriv RTriv
          | Compare Rloc RTriv
          | JumpIf RelOp Label
          | Jump RPlace
          | Labelled Label Stmt
          deriving Show
