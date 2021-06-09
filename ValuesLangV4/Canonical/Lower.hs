module Canonical.Lower
( lower
)
where

import Canonical.Types

import qualified Asm.Types as A

import Control.Monad.State
import Data.Int

lower :: Program -> A.Program
lower (Program top) = A.Program (lowerTop top)

lowerTop :: Top -> A.Top
lowerTop (Expr (Triv triv)) = A.Halt (lowerTriv triv)
lowerTop (Expr (BinOp op (Int a) (Int b))) = (A.Halt . A.Int) $ reifyOp op a b
lowerTop (Expr (BinOp op (TAloc aloc) triv)) = A.Seq [ A.BinOp op' aloc' triv' ]
                                                     (A.Halt $ A.TAloc aloc')
                                             where op' = lowerOp op
                                                   aloc' = lowerAloc aloc
                                                   triv' = lowerTriv triv
lowerTop (TIf p c a) = A.TIf p' c' a'
                     where p' = lowerPred p
                           c' = lowerTop c
                           a' = lowerTop a
lowerTop (Seq stmts body) = A.Seq stmts' body'
                          where stmts' = lowerStmts stmts
                                body' = lowerTop body

lowerStmts :: [Stmt] -> [A.Stmt]
lowerStmts stmts = (concat . map lowerStmt) stmts

lowerStmt :: Stmt -> [A.Stmt]
lowerStmt (Set aloc (Triv triv)) = [ A.Set aloc' triv' ]
                                 where aloc' = lowerAloc aloc
                                       triv' = lowerTriv triv
lowerStmt (Set aloc (BinOp op left right)) = [ A.Set aloc' left'
                                             , A.BinOp op' aloc' right' ]
                                           where aloc' = lowerAloc aloc
                                                 op' = lowerOp op
                                                 left' = lowerTriv left
                                                 right' = lowerTriv right
lowerStmt (If p c a) = [ A.If p' c' a' ]
                     where p' = lowerPred p
                           c' = lowerStmts c
                           a' = lowerStmts a

lowerPred :: Pred -> A.Pred
lowerPred (Bool b) = A.Bool b
lowerPred (RelOp op (Int a) (Int b)) = A.Bool $ reifyRelOp op a b
lowerPred (RelOp op (TAloc aloc) triv) = A.RelOp op' aloc' triv'
                                       where op' = lowerRelOp op
                                             aloc' = lowerAloc aloc
                                             triv' = lowerTriv triv
lowerPred (Not pred) = A.Not $ lowerPred pred
lowerPred (PIf p c a) = A.PIf p' c' a'
                      where p' = lowerPred p
                            c' = lowerPred c
                            a' = lowerPred a
lowerPred (PSeq stmts body) = A.PSeq stmts' body'
                            where stmts' = lowerStmts stmts
                                  body' = lowerPred body

lowerTriv :: Triv -> A.Triv
lowerTriv (Int i) = A.Int i
lowerTriv (TAloc aloc) = A.TAloc $ lowerAloc aloc

lowerAloc :: Aloc -> A.Aloc
lowerAloc (Aloc v i) = A.Aloc v i

lowerOp :: Op -> A.Op
lowerOp Add = A.Add
lowerOp Mul = A.Mul

lowerRelOp :: RelOp -> A.RelOp
lowerRelOp Lt = A.Lt
lowerRelOp Gt = A.Gt
lowerRelOp Eq = A.Eq
lowerRelOp Lte = A.Lte
lowerRelOp Gte = A.Gte
lowerRelOp Neq = A.Neq

reifyRelOp :: RelOp -> Int64 -> Int64 -> Bool
reifyRelOp Lt = (<)
reifyRelOp Gt = (>)
reifyRelOp Eq = (==)
reifyRelOp Lte = (<=)
reifyRelOp Gte = (>=)
reifyRelOp Neq = (/=)

reifyOp :: Op -> Int64 -> Int64 -> Int64
reifyOp Add = (+)
reifyOp Mul = (*)
