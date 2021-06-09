module Monadic.Lower
( lower
)
where

import Monadic.Types

import qualified Canonical.Types as C

lower :: Program -> C.Program
lower (Program expr) = C.Program (lowerTop expr)

lowerTop :: Expr -> C.Top
lowerTop (Triv triv) = (C.Expr . C.Triv) $ lowerTriv triv
lowerTop (BinOp op left right) = C.Expr $ C.BinOp (lowerOp op)
                                                  (lowerTriv left)
                                                  (lowerTriv right)
lowerTop (If p c a) = C.TIf (lowerPred p)
                            (lowerTop c)
                            (lowerTop a)
lowerTop (Seq stmts body) = C.Seq (lowerStmts stmts)
                                  (lowerTop body)

lowerStmts :: [Stmt] -> [C.Stmt]
lowerStmts = concat . map lowerStmt

lowerStmt :: Stmt -> [C.Stmt]
lowerStmt (Set aloc expr) = lowerStmt' (C.Set (lowerAloc aloc)) expr

lowerStmt' :: (C.Expr -> C.Stmt) -> Expr -> [C.Stmt]
lowerStmt' set (Triv triv) = [ set $ C.Triv $ lowerTriv triv ]
lowerStmt' set (BinOp op left right) = [ set $ C.BinOp (lowerOp op)
                                                       (lowerTriv left)
                                                       (lowerTriv right) ]
lowerStmt' set (If p c a) = [ C.If (lowerPred p)
                                   (lowerStmt' set c)
                                   (lowerStmt' set a) ]
lowerStmt' set (Seq stmts body) = lowerStmts stmts ++ lowerStmt' set body

lowerPred :: Pred -> C.Pred
lowerPred (Bool b) = C.Bool b
lowerPred (RelOp op left right) = C.RelOp (lowerRelOp op)
                                          (lowerTriv left)
                                          (lowerTriv right)
lowerPred (Not pred) = C.Not $ lowerPred pred
lowerPred (PIf p c a) = C.PIf (lowerPred p)
                              (lowerPred c)
                              (lowerPred a)
lowerPred (PSeq stmts body) = C.PSeq (lowerStmts stmts)
                                     (lowerPred body)

lowerTriv :: Triv -> C.Triv
lowerTriv (Int i) = C.Int i
lowerTriv (TAloc aloc) = C.TAloc $ lowerAloc aloc

lowerAloc :: Aloc -> C.Aloc
lowerAloc (Aloc v i) = C.Aloc v i

lowerOp :: Op -> C.Op
lowerOp Add = C.Add
lowerOp Mul = C.Mul

lowerRelOp :: RelOp -> C.RelOp
lowerRelOp Lt = C.Lt
lowerRelOp Gt = C.Gt
lowerRelOp Eq = C.Eq
lowerRelOp Lte = C.Lte
lowerRelOp Gte = C.Gte
lowerRelOp Neq = C.Neq
