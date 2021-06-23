module Asm.Types where

import Compiler.Types

data Program = Program [Block] Tail

data Block = Block Label Tail

data Tail = TSeq [Stmt] Tail
          | TIf Pred Tail Tail
          | Jump Place [MLoc]

data Stmt = Set MLoc Triv
          | BinOp Op MLoc Triv
          | If Pred [Stmt] [Stmt]
          | JumpRet Label [MLoc]

data Pred = Bool Bool
          | RelOp RelOp Triv Triv
          | Not Pred
          | PSeq [Stmt] Pred
          | PIf Pred Pred Pred

data Triv = Int Integer
          | Loc MLoc

data Place = PLabel Label
           | PLoc MLoc
