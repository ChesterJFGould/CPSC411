module Asm.Types where

import Data.Int
import Data.Map
import Data.Set

data Program = Program { prog :: ([Stmt], Triv)
                       , alocs :: Set Aloc
                       , alocAddrs :: Map Aloc Int }
             deriving Show

data Stmt = Stmt Op Aloc Triv
          deriving Show

data Op = Set
        | Add
        | Mul
        deriving Show

data Triv = Int Int64
          | Aloc Aloc
          deriving Show

data Aloc = Var String Int
          deriving (Eq, Ord, Show)
