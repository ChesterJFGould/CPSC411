module Monadic.Types where

import Compiler.Types

import Data.Word

data Program = Program [Block] Body

data Block = Bloc Label Body

data Body = Body Aloc Tail
        
data Tail = Expr Expr
          | TSeq [Stmt] Tail
          | TIf Pred Tail Tail
          | Jump Label [Mloc]

data Expr = Triv MTriv
          | BinOp BinOp MTriv MTriv
          | Seq [Stmt] Expr
          | If Pred Expr Expr

data Pred = Bool Bool
          | RelOp RelOp MTriv MTriv
          | Not Pred
          | PSeq [Stmt] Pred
          | PIf Pred Pred Pred

data Stmt = Set Mloc Expr
          | JumpRet Label [Mloc]
