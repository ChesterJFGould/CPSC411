module Paren.Types where

data Program = Program [Label] [Stmt]

data Stmt = SetReg Reg RVal
          | SetAddr Addr AVal
          | BinOp BinOp Reg OVal
          | Compare Reg OVal
          | JumpIf RelOp RTriv
          | Jump RTriv
          | Labelled Label Stmt

data RVal = RInt Int64
          | RReg Reg
          | RAddr Addr
          | RLabel Labe

data AVal = AInt Int32
          | AReg Reg
          | ALabel Label

data OVal = OInt Int32
          | OReg Reg
          | OAddr Addr
          | OLabel Label
