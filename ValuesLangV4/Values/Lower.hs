module Values.Lower
( lower
)
where

import Values.Types

import qualified Unique.Types as U

import Control.Monad.State
import Data.Map

lower :: Program -> U.Program
lower (Program expr) = U.Program $ evalState (lowerExpr expr) (empty, 0)

lowerExpr :: Expr -> State (Map Var U.Aloc, Int) U.Expr
lowerExpr (Triv triv) = lowerTriv triv >>= return . U.Triv
lowerExpr (BinOp op left right) = do
                                  left' <- lowerTriv left
                                  right' <- lowerTriv right
                                  return $ U.BinOp (lowerOp op) left' right'
lowerExpr (Let assignments body) = do
                                   (env, _) <- get
                                   vals <- mapM (lowerExpr . snd) assignments
                                   alocs <- mapM (gensym . fst) assignments
                                   body' <- lowerExpr body
                                   (_, i) <- get
                                   put (env, i)
                                   return $ U.Let (zip alocs vals) body'
lowerExpr (If p c a) = do
                       p' <- lowerPred p
                       c' <- lowerExpr c
                       a' <- lowerExpr a
                       return $ U.If p' c' a'

lowerTriv :: Triv -> State (Map Var U.Aloc, Int) U.Triv
lowerTriv (Int i) = return $ U.Int i
lowerTriv (TVar v) = get >>= (return . U.TAloc  . (! v) . fst)

lowerPred :: Pred -> State (Map Var U.Aloc, Int) U.Pred
lowerPred (Bool b) = return $ U.Bool b
lowerPred (RelOp op left right) = do
                                  left' <- lowerTriv left
                                  right' <- lowerTriv right
                                  return $ U.RelOp (lowerRelOp op) left' right'
lowerPred (Not p) = lowerPred p >>= return . U.Not
lowerPred (PLet assignments body) = do
                                    (env, _) <- get
                                    vals <- mapM (lowerExpr . snd) assignments
                                    alocs <- mapM (gensym . fst) assignments
                                    body' <- lowerPred body
                                    (_, i) <- get
                                    put (env, i)
                                    return $ U.PLet (zip alocs vals) body'
lowerPred (PIf p c a) = do
                        p' <- lowerPred p
                        c' <- lowerPred c
                        a' <- lowerPred a
                        return $ U.PIf p' c' a'

lowerOp :: Op -> U.Op
lowerOp Add = U.Add
lowerOp Mul = U.Mul

lowerRelOp :: RelOp -> U.RelOp
lowerRelOp Lt = U.Lt
lowerRelOp Gt = U.Gt
lowerRelOp Eq = U.Eq
lowerRelOp Lte = U.Lte
lowerRelOp Gte = U.Gte
lowerRelOp Neq= U.Neq

gensym :: Var -> State (Map Var U.Aloc, Int) U.Aloc
gensym var@(Var template) = do
                           (env, i) <- get
                           put (insert var (U.Aloc template i) env, i + 1)
                           return $ U.Aloc template i
