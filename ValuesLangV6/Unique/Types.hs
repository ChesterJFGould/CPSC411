module Unique.Types where

import Compiler.Types

data Program = Program [Func] Body
             deriving Show

data Func = Func Label [Aloc] Body
          deriving Show

data Body = Body Aloc Tail
          deriving Show

data Tail = Expr Expr
          | TLet [(Aloc, Expr)] Tail
          | TIf Pred Tail Tail
          | TCall Label [Triv]
          deriving Show

data Expr = Triv Triv
          | BinOp Op Triv Triv
          | Let [(Aloc, Expr)] Expr
          | If Pred Expr Expr
          | Call Label [Triv]
          deriving Show

data Pred = Bool Bool
          | RelOp RelOp Triv Triv
          | Not Pred
          | PLet [(Aloc, Expr)] Pred
          | PIf Pred Pred Pred
          deriving Show

data Triv = Int Integer
          | TAloc Aloc
          deriving Show
