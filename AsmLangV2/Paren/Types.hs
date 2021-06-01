module Paren.Types where

import Data.Int

data Program = Program [Stmt]
             deriving Show

data Stmt = SetAddr Addr AVal
          | SetReg Reg RVal
          | BinOp Op Reg OVal
          deriving Show

data Op = Add
        | Mul
        deriving Show

data AVal = AReg Reg
          | AInt32 Int32
          deriving Show

data RVal = RReg Reg
          | RAddr Addr
          | RInt64 Int64
          deriving Show

data OVal = OReg Reg
          | OAddr Addr
          | OInt32 Int32
          deriving Show

data Reg = RSP
         | RBP
         | RAX
         | RBX
         | RCX
         | RDX
         | RSI
         | R8
         | R9
         | R10
         | R11
         | R12
         | R13
         | R14
         | R15
         deriving Show

data Addr = Addr Int -- Word offset down from RBP i.e. QWORD [ RBP - Int ]
          deriving Show
