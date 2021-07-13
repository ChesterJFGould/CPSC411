module Compiler.Types where

import Control.Monad.State
import Data.Int
import Data.Word

type Gensym = State Int64

data Mloc = MAloc Aloc
          | MRloc Rloc
          deriving (Eq, Ord, Show)

data Aloc = Aloc String Int64
          deriving (Eq, Ord, Show)

data Rloc = Reg Reg
          | LAddr Addr
          deriving (Eq, Ord, Show)

data Addr = Addr Int
          deriving (Eq, Ord, Show)

data Label = Label String Int64
           deriving Show

data Ptr = Ptr Word64
         deriving Show

data ATriv = TAloc Aloc
           | APtr Ptr
           deriving Show

data MTriv = TMloc Mloc
           | MPtr Ptr
           deriving Show

data RTriv = RPtr Ptr
           | TRLabel Label
           | TRloc Rloc
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
