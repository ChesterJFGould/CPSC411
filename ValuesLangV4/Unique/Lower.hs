module Unique.Lower
( lower
)
where

import Unique.Types

import qualified Monadic.Types as M

lower :: Program -> M.Program
lower (Program expr) = M.Program (lowerExpr expr)

lowerExpr :: Expr -> M.Expr
lowerExpr (Triv triv) = M.Triv $ lowerTriv triv
lowerExpr (BinOp op left right) = M.BinOp (lowerOp op)
                                          (lowerTriv left)
                                          (lowerTriv right)
lowerExpr (If p c a) = M.If (lowerPred p)
                            (lowerExpr c)
                            (lowerExpr a)

lowerExpr (Let assignments body) = M.Seq (map (uncurry M.Set)
                                              (zip (map (lowerAloc . fst) assignments)
                                                   (map (lowerExpr . snd) assignments)))
                                         (lowerExpr body)

lowerPred :: Pred -> M.Pred
lowerPred (Bool b) = M.Bool b
lowerPred (RelOp op left right) = M.RelOp (lowerRelOp op)
                                          (lowerTriv left)
                                          (lowerTriv right)

lowerPred (Not pred) = M.Not $ lowerPred pred
lowerPred (PIf p c a) = M.PIf (lowerPred p)
                              (lowerPred c)
                              (lowerPred a)
lowerPred (PLet assignments body) = M.PSeq (map (uncurry M.Set)
                                              (zip (map (lowerAloc . fst) assignments)
                                                   (map (lowerExpr . snd) assignments)))
                                           (lowerPred body)

lowerTriv :: Triv -> M.Triv
lowerTriv (Int i) = M.Int i
lowerTriv (TAloc a) = M.TAloc $ lowerAloc a

lowerAloc :: Aloc -> M.Aloc
lowerAloc (Aloc v i) = M.Aloc v i

lowerOp :: Op -> M.Op
lowerOp Add = M.Add
lowerOp Mul = M.Mul

lowerRelOp :: RelOp -> M.RelOp
lowerRelOp Lt = M.Lt
lowerRelOp Gt = M.Gt
lowerRelOp Eq = M.Eq
lowerRelOp Lte = M.Lte
lowerRelOp Gte = M.Gte
lowerRelOp Neq = M.Neq
