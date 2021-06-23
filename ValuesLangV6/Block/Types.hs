module Block.Types where

import Compiler.Types

data Program = Program [Block] Tail

data Block = Block Label Tail

data Tail = Seq [Stmt] Tail
          | If Pred Label Label
          | Jump Place

data Stmt = Set Loc Triv
          | BinOp Op Loc Triv

data Pred = RelOp RelOp Loc Triv

data Triv = Int Integer
          | Loc Loc

data Place = PLabel Label
           | PLoc Loc
