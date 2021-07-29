module Compiler.Types where

import Control.Monad.State
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

data Addr = Stack Int32
          | Offset Reg Int32
          | Pointer Reg Reg
          deriving (Eq, Ord, Show)

data Lit = Lit Word64
         deriving Show

data ATriv = AAloc Aloc
           | ALit Lit
           deriving Show

data MTriv = MMloc Mloc
           | MLit Lit
           deriving Show

data RTriv = RRloc Rloc
           | RLit Lit
           deriving Show

data Label = Label String Int64
           deriving Show

data MPlace = PMloc Mloc
            | MLabel Label
            deriving Show

data RPlace = PRloc Rloc
            | RLabel Label
            deriving Show

data BinOp = Add
           | Sub
           | Mul
           | And
           | Ior
           | Xor
           | Shr
           deriving Show

data RelOp = Lt
           | Gt
           | Eq
           | Lte
           | Gte
           | Neq
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
