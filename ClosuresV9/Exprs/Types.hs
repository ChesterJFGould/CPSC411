module Exprs.Types where

data Type = TInt
          | TBool
          | TFunc Type Type
          deriving Show

data Program = Program [Def] Body
             deriving Show

data Def = Def Type Var [Var] Body
         deriving Show

data Body = Body Expr
          deriving Show

data Expr = Triv Triv
          | BinOp BinOp Expr Expr
          | Apply Expr Expr
          | Let Var Expr Expr
          | Lambda Var Type Expr
          | If Expr Expr Expr
          deriving Show

data Triv = Int Integer
          | Bool Bool
          | TVar Var
          deriving Show

data Var = Var String
         deriving (Eq, Show)

data BinOp = Add
           | Sub
           | Mul
           | Lt
           | Gt
           | Eq
           | Lte
           | Gte
           | Neq
           deriving Show
