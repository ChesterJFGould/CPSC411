module Nested.Types where

import Compiler.Types

data Program = Program [Block] Tail

data Block = Block Label Tail

data Tail = TSeq [Stmt] Tail
          | TIf Pred Tail Tail
          | Jump Place

data Stmt = Set Loc Triv
          | BinOp Op Loc Triv
          | If Pred [Stmt] [Stmt]
          | JumpRet Label

data Pred = Bool Bool
          | RelOp RelOp Triv Triv
          | Not Pred
          | PSeq [Stmt] Pred
          | PIf Pred Pred Pred

data Triv = Int Integer
          | Loc Loc

data Place = PLabel Label
           | PLoc Loc
