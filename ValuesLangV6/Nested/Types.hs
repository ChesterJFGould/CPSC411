module Nested.Types where

import Compiler.Types

data Program = Program [Block] Tail
             deriving Show

data Block = Block Label Tail
           deriving Show

data Tail = TSeq [Stmt] Tail
          | TIf Pred Tail Tail
          | Jump Place
          deriving Show

data Stmt = Set Loc Triv
          | BinOp Op Loc Triv
          | If Pred [Stmt] [Stmt]
          | JumpRet Label
          deriving Show

data Pred = Bool Bool
          | RelOp RelOp Loc Triv
          | Not Pred
          | PSeq [Stmt] Pred
          | PIf Pred Pred Pred
          deriving Show

data Triv = Int Integer
          | Loc Loc
          deriving Show

data Place = PLabel Label
           | PLoc Loc
           deriving Show
