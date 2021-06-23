module Para.Types where

import Compiler.Types

data Program = Program [Stmt]

data Stmt = Set Loc Triv
          | BinOp Op Loc Triv
          | Compare Loc Triv
          | JumpIf RelOp Label
          | Jump Place
          | Labelled Label Stmt

data Triv = Int Integer
          | Loc Loc

data Place = PLabel Label
           | PLoc Loc
