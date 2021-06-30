module Para.Types where

import Compiler.Types

data Program = Program [Stmt]
             deriving Show

data Stmt = Set Loc Triv
          | BinOp Op Loc Triv
          | Compare Loc Triv
          | JumpIf RelOp Label
          | Jump Place
          | Labelled Label Stmt
          deriving Show

data Triv = Int Integer
          | Loc Loc
          | TLabel Label
          deriving Show

data Place = PLabel Label
           | PLoc Loc
           deriving Show
