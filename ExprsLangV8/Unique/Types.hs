module Unique.Types where

import Compiler.Types hiding (BinOp)
import Compiler.Value

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
          | Seq [Stmt] Expr
          deriving Show

data Triv = TAloc Aloc
          | Value Value
          deriving Show

data Stmt = SetVec Expr Expr Expr
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
           | Cons
           | VectorRef
           deriving Show

data UnOp = IsInt
          | IsBool
          | IsEmpty
          | IsVoid
          | IsChar
          | IsError
          | IsPair
          | IsVector
          | Not
          | Car
          | Cdr
          | MakeVector
          | VectorLength
          deriving Show
