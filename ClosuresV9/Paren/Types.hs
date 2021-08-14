module Paren.Types where

import Compiler.Types

import Data.Word

data Program = Program [Label] [Stmt]
             deriving Show

data Stmt = SetReg Reg RVal
          | SetAddr Addr AVal
          | NumOp NumOp Reg OVal
          | Compare Reg OVal
          | JumpIf RelOp Label
          | Jump RTriv
          | Labelled Label Stmt
          deriving Show

data RVal = RWord Word64
          | RReg Reg
          | RAddr Addr
          | RVLabel Label
          deriving Show

data AVal = AWord Word32
          | AReg Reg
          | AVLabel Label
          deriving Show

data OVal = OWord Word32
          | OReg Reg
          | OAddr Addr
          | OVLabel Label
          deriving Show
