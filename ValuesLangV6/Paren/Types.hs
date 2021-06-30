module Paren.Types where

import Compiler.Types

import Data.Int

data Program = Program [Stmt]
             deriving Show

data Stmt = SetAddr Addr AVal
          | SetReg Reg RVal
          | BinOp Op Reg OVal
          | Compare Reg CVal
          | JumpIf RelOp Label
          | Jump Place
          | Labelled Label Stmt
          deriving Show

data AVal = AInt Int32
          | AReg Reg
          | ALabel Label
          deriving Show

data RVal = RInt Int64
          | RReg Reg
          | RAddr Addr
          | RLabel Label
          deriving Show

data OVal = OInt Int32
          | OReg Reg
          | OAddr Addr
          | OLabel Label
          deriving Show

data CVal = CInt Int32
          | CReg Reg
          | CAddr Addr
          | CLabel Label
          deriving Show

data Place = PLabel Label
           | PReg Reg
           | PAddr Addr
           deriving Show
