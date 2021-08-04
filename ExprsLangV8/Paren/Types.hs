module Paren.Types where

import Compiler.Types
import Data.Word

data Program = Program [Stmt]
             deriving Show

data Stmt = SetReg Reg RVal
          | SetAddr Addr AVal
          | BinOp BinOp Reg OVal
          | Compare Reg OVal
          | JumpIf RelOp Label
          | Jump RPlace
          | Labelled Label Stmt
          deriving Show

data RVal = RLit Lit
          | RReg Reg
          | RAddr Addr
          | RLabel Label
          deriving Show

data AVal = AReg Reg
          | ALabel Label
          deriving Show

data OVal = OReg Reg
          | OAddr Addr
          | OLabel Label
          deriving Show
