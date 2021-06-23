module Monadic.Types where

import Compiler.Types

data Program = Program [Block] Body
             deriving Show

data Block = Block Label Body
           deriving Show

data Body = Body Aloc Tail
          deriving Show

data Tail = Expr Expr
          | TSeq [Stmt] Tail
          | TIf Pred Tail Tail
          | Jump Label [MLoc]
          deriving Show

data Expr = Triv Triv
          | BinOp Op Triv Triv
          | Seq [Stmt] Expr
          | If Pred Expr Expr
          deriving Show

data Stmt = Set MLoc Expr
          | JumpRet Label [MLoc]
          deriving Show

data Pred = Bool Bool
          | RelOp RelOp Triv Triv
          | Not Pred
          | PSeq [Stmt] Pred
          | PIf Pred Pred Pred
          deriving Show

data Triv = Int Integer
          | Loc MLoc
          deriving Show
