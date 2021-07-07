module Unique.Types where

import Compiler.Types hiding (BinOp)
import Compiler.Value

import Data.Word

data Program = Program [Func] Body
             deriving Show

data Func = Func Label [Aloc] Body
          deriving Show

data Body = Body Expr
          deriving Show

data Expr = Triv Triv
          | UnOp UnOp Expr
          | BinOp BinOp Expr Expr
          | Apply Label [Expr]
          | Let [(Aloc, Expr)] Expr
          | If Expr Expr Expr
          deriving Show

data Triv = UAloc Aloc
          | Value Value
          deriving Show

data BinOp = Add
           | Sub
           | Mul
           | Lt
           | Gt
           | Eq
           | Lte
           | Gte
           | Neq
           deriving (Enum, Show)

data UnOp = IsInt
          | IsBool
          | IsEmpty
          | IsVoid
          | IsChar
          | IsError
          | Not
          deriving Show
