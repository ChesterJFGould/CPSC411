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

data RVal = RPtr Word64
          | RReg Reg
          | RAddr Addr
          | RLabel Label

data AVal = APtr Word32
          | AReg Reg
          | ALabel Label

data OVal = OPtr Word32
          | OReg Reg
          | OAddr Addr
          | OLabel Label
