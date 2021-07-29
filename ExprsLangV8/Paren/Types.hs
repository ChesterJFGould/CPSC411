module Paren.Types where

import Compiler.Types
import Data.Word

data Program = Program [Stmt]

data Stmt = SetReg Reg RVal
          | SetAddr Addr AVal
          | BinOp BinOp Reg OVal
          | Compare Reg OVal
          | JumpIf RelOp Label
          | Jump RPlace
          | Labelled Label Stmt

data RVal = RLit Word64
          | RReg Reg
          | RAddr Addr
          | RLabel Label

data AVal = AReg Reg
          | ALabel Label

data OVal = OReg Reg
          | OAddr Addr
          | OLabel Label
