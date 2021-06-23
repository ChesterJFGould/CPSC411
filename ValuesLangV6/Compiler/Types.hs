module Compiler.Types where

data MLoc = MAloc Aloc
          | MRloc Loc
          deriving Show

data Loc = Reg Reg
         | LAddr Addr
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

data Addr = Addr Int
          deriving Show

data Aloc = Aloc String Int
          deriving Show

data Label = Label String
           deriving Show

data Op = Add
        | Sub
        | Mul
        deriving Show

data RelOp = Lt
           | Gt
           | Eq
           | Lte
           | Gte
           | Neq
           deriving Show
