module Para.Types where

import Compiler.Types

data Program = Program [Label] [Stmt]
             deriving Show

data Stmt = Set Rloc RTriv
          | NumOp NumOp Rloc RTriv
          | MRef Rloc RTriv RTriv
          | MSet RTriv RTriv RTriv
          | Compare Rloc RTriv
          | JumpIf RelOp Label
          | Jump RTriv
          | Labelled Label Stmt
          deriving Show
