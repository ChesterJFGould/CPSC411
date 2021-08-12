module Paren.Types where

data Program = Program [Label] [Stmt]

data Stmt = SetReg Reg RVal
          | SetAddr Addr AVal
          | NumOp NumOp Reg OVal
          | Compare Reg OVal
          | JumpIf RelOp RTriv
          | Jump RTriv
          | Labelled Label Stmt

data RVal = RInt Int64
          | RReg Reg
          | RAddr Addr
          | RLabel Label

data AVal = AInt Int32
          | AReg Reg
          | ALabel Label

data OVal = OInt Int32
          | OReg Reg
          | OAddr Addr
          | OLabel Label
