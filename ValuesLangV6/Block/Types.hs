module Block.Types where

import Compiler.Types

data Program = Program [Block] Tail
             deriving Show

data Block = Block Label Tail
           deriving Show

data Tail = Seq [Stmt] Tail
          | If Pred Label Label
          | Jump Place
          deriving Show

data Stmt = Set Loc Triv
          | BinOp Op Loc Triv
          deriving Show

data Pred = RelOp RelOp Loc Triv
          deriving Show

data Triv = Int Integer
          | Loc Loc
          | TLabel Label
          deriving Show

data Place = PLabel Label
           | PLoc Loc
           deriving Show
