module Paren.Types where

import Compiler.Types

import Data.Int

data Program = Program [Stmt]

data Stmt = SetAddr Addr AVal
          | SetReg Reg RVal
          | BinOp Op Reg OVal
          | Compare Reg CVal
          | JumpIf RelOp Label
          | Jump Place
          | Labelled Label Stmt

data AVal = AInt Int32
          | AReg Reg

data RVal = RInt Int64
          | RReg Reg
          | RAddr Addr

data OVal = OInt Int32
          | OReg Reg
          | OAddr Addr

data CVal = CInt Int64
          | CReg Reg

data Place = PLabel Label
           | PReg Reg
           | PAddr Addr
