module Asm.Types where

import Compiler.Types

data Program = Program [Block] Tail
             deriving Show

data Block = Block Label Tail
           deriving Show

data Tail = TSeq [Stmt] Tail
          | TIf Pred Tail Tail
          | Jump Place [MLoc]
          deriving Show

data Stmt = Set MLoc Triv
          | BinOp Op MLoc Triv
          | If Pred [Stmt] [Stmt]
          | JumpRet Label [MLoc]
          deriving Show

data Pred = Bool Bool
          | RelOp RelOp MLoc Triv
          | Not Pred
          | PSeq [Stmt] Pred
          | PIf Pred Pred Pred
          deriving Show

data Triv = Int Integer
          | Loc MLoc
          deriving Show

data Place = PLabel Label
           | PLoc MLoc
           deriving Show
