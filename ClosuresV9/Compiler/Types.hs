module Compiler.Types where

import Data.Int
import Data.Word

data Aloc = Aloc String Int64
          deriving (Eq, Ord, Show)

data Mloc = MAloc Aloc
          | MRloc Rloc
          deriving (Eq, Ord, Show)

data Rloc = Reg Reg
          | Addr Addr
          deriving (Eq, Ord, Show)

data Label = Label String Int64
           deriving (Eq, Ord, Show)

data Value = VInt Integer
           | VBool Bool
           | VAloc Aloc
           | VLabel Label
           deriving Show

data NumOp = Add
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

data BinOp = BNumOp NumOp
           | BRelOp RelOp
           deriving Show

data Lit = Lit Word64
         deriving Show

data ATriv = ALit Lit
           | AAloc Aloc
           | ALabel Label
           deriving Show

data MTriv = MLit Lit
           | MMloc Mloc
           | MLabel Label
           deriving Show

data RTriv = RLit Lit
           | RRloc Rloc
           | RLabel Label
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
         deriving (Eq, Ord, Show)

data Addr = Stack Int
          deriving (Eq, Ord, Show)
