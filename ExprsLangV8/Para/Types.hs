module Para.Types where

import Compiler.Types

data Program = Program [Stmt]

data Stmt = Set Rloc RTriv
          | BinOp BinOp Rloc RTriv
          | MSet Rloc RTriv RTriv
          | MRef Rloc Rloc RTriv
          | Compare Rloc RTriv
          | JumpIf RelOp Label
          | Jump RPlace
          | Labelled Label Stmt
