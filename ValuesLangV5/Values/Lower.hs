module Values.Lower
( lower
)
where

import Values.Types

import qualified Unique.Types as U

import Control.Monad.State
import Data.Map as M

lower :: Program -> U.Program
lower prog = evalState (lowerProg prog) (M.empty, 0)

type Env a = State (Map Var U.Aloc, Int) a

lowerProg :: Program -> Env U.Program
lowerProg (Program defs tail) = do
                                defs' <- mapM lowerFunc defs
                                tail' <- lowerTail tail
                                return $ U.Program defs' tail'

lowerFunc :: Func -> Env U.Func
lowerFunc (Func (Var f) args body) = do
                                     env <- gets fst
                                     args' <- mapM gensym args
                                     body' <- lowerTail body
                                     i <- gets snd
                                     put (env, i)
                                     return $ U.Func (U.Label f) args' body'

lowerTail :: Tail -> Env U.Tail
lowerTail (Expr expr) = lowerExpr expr >>= return . U.Expr
lowerTail (TLet assignments tail) = lowerLet lowerTail U.TLet assignments tail
lowerTail (TIf p c a) = lowerIf lowerTail U.TIf p c a
lowerTail (Call (Var f) args) = mapM lowerTriv args >>= return . U.Call (U.Label f)

lowerExpr :: Expr -> Env U.Expr
lowerExpr (Triv triv) = lowerTriv triv >>= return . U.Triv
lowerExpr (BinOp op left right) = do
                                  left' <- lowerTriv left
                                  right' <- lowerTriv right
                                  return $ U.BinOp (lowerOp op) left' right'
lowerExpr (Let assignments expr) = lowerLet lowerExpr U.Let assignments expr
lowerExpr (If p c a) = lowerIf lowerExpr U.If p c a

lowerPred :: Pred -> Env U.Pred
lowerPred (Bool b) = return $ U.Bool b
lowerPred (RelOp op left right) = do
                                  left' <- lowerTriv left
                                  right' <- lowerTriv right
                                  return $ U.RelOp (lowerRelOp op) left' right'
lowerPred (PLet assignments pred) = lowerLet lowerPred U.PLet assignments pred
lowerPred (PIf p c a) = lowerIf lowerPred U.PIf p c a

lowerLet :: (a -> Env b) -> ([(U.Aloc, U.Expr)] -> b -> b) -> [(Var, Expr)] -> a -> Env b
lowerLet lowerer cons assignments body = do
                                         env <- gets fst
                                         vals <- mapM (lowerExpr . snd) assignments
                                         vars <- mapM (gensym . fst) assignments
                                         let assignments' = zip vars vals
                                         body' <- lowerer body
                                         i <- gets snd
                                         put (env, i)
                                         return $ cons assignments' body'

lowerIf :: (a -> Env b) -> (U.Pred -> b -> b -> b) -> Pred -> a -> a -> Env b
lowerIf lowerer cons p c a = do
                               p' <- lowerPred p
                               c' <- lowerer c
                               a' <- lowerer a
                               return $ cons p' c' a'

lowerTriv :: Triv -> Env U.Triv
lowerTriv (Int i) = return $ U.Int i
lowerTriv (TVar v) = getVar v >>= return . U.TAloc

lowerOp :: Op -> U.Op
lowerOp Add = U.Add
lowerOp Mul = U.Mul

lowerRelOp :: RelOp -> U.RelOp
lowerRelOp Lt = U.Lt
lowerRelOp Gt = U.Gt
lowerRelOp Eq = U.Eq
lowerRelOp Lte = U.Lte
lowerRelOp Gte = U.Gte
lowerRelOp Neq = U.Neq

getVar :: Var -> Env U.Aloc
getVar v = gets ((! v) . fst)

gensym :: Var -> Env U.Aloc
gensym var@(Var v) = do
                     (env, i) <- get
                     let aloc = U.Aloc v i
                     put (M.insert var aloc env, i + 1)
                     return aloc
