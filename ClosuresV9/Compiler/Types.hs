module Compiler.Types where

data Aloc = Aloc String Int

data Label = Label String Int

data BinOp = Add
           | Sub
           | Mul

data RelOp = Lt
           | Gt
           | Eq
           | Lte
           | Gte
           | Neq
