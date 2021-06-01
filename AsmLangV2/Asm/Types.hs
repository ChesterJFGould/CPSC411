module Asm.Types where

import Data.Int
import Data.Set
import Data.Map

data Program = Program { prog :: ([Stmt], Triv)
                       , alocs :: Set Aloc
                       , alocAddrs :: Map Aloc Int }
             deriving Show

data Stmt = Stmt Op Aloc Triv

instance Show Stmt where
         show (Stmt op aloc triv) = unwords [show aloc, show op, show triv]

data Op = Set
        | Add
        | Mul

instance Show Op where
         show Set = ":="
         show Add = "+="
         show Mul = "*="

data Triv = Int64 Int64
          | Aloc Aloc

instance Show Triv where
         show (Int64 i) = show i
         show (Aloc aloc) = show aloc

data Aloc = Var String
          deriving (Eq, Ord)

instance Show Aloc where
         show (Var v) = v
