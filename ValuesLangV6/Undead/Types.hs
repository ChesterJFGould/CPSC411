module Undead.Types where

import Compiler.Types

import Data.Set

type Tagged tag val = (tag, val)

type Undead val = Tagged (Set MLoc) val

data Program = Program [Block] TBody
             deriving Show

data Block = Block Label TBody
           deriving Show

type TBody = Tagged (Set Aloc, Set (MLoc, MLoc)) Body

data Body = Body TTail
          deriving Show

type TTail = Undead Tail

data Tail = TSeq [TStmt] (TTail)
          | TIf (TPred) (TTail) (TTail)
          | Jump Place [MLoc]
          deriving Show

type TStmt = Undead Stmt

data Stmt = Set MLoc Triv
          | BinOp Op MLoc Triv
          | If TPred [TStmt] [TStmt]
          | JumpRet Label [MLoc]
          deriving Show

type TPred = Undead Pred

data Pred = Bool Bool
          | RelOp RelOp MLoc Triv
          | Not TPred
          | PSeq [TStmt] TPred
          | PIf TPred TPred TPred
          deriving Show

data Triv = Int Integer
          | Loc MLoc
          deriving Show

data Place = PLabel Label
           | PLoc MLoc
           deriving Show
