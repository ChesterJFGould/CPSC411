module Paren.Types where

import Data.Int

data Program = Program [Stmt]
             deriving Show

data Stmt = SetAddr Addr AVal
          | SetReg Reg RVal
          | BinOp Op Reg OVal
          | Compare Reg CVal
          | JumpIf RelOp Label
          | Jump Label
          | Labelled Label Stmt
          | Halt RVal
          deriving Show

data AVal = AInt Int32
          | AReg Reg
          deriving Show

data RVal = RInt Int64
          | RReg Reg
          | RAddr Addr
          deriving Show

data OVal = OInt Int32
          | OReg Reg
          | OAddr Addr
          deriving Show

data CVal = CInt Int64
          | CReg Reg
          deriving Show

data Label = Label String
           deriving Show

data Addr = Addr Int
          deriving Show

data Reg = RSP
         | RBP
         | RAX
         | RBX
         | RCX
         | RDX
         | RSI
         | RDI
         | R8
         | R9
         | R10
         | R11
         | R12
         | R13
         | R14
         | R15
         deriving Show

data Op = Add
        | Mul
        deriving Show

data RelOp = Lt
           | Gt
           | Eq
           | Lte
           | Gte
           | Neq
           deriving Show
