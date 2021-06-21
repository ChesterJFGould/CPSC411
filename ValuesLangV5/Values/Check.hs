module Values.Check
( check
)
where

import Values.Types

import Control.Monad.State
import Control.Monad.Writer
import Data.List as L
import Data.Map as M

data Type = IntT
          | FuncT Int

check :: Program -> Either String Program
check prog@(Program defs body) = case errors of
                                      [] -> Right prog
                                      errors -> Left $ unlines errors
                               where errors = (flip evalState) env
                                              $ execWriterT (mapM checkFunc defs >> checkTail body)
                                     env = M.fromList $ L.map funcType defs

funcType :: Func -> (Var, Type)
funcType (Func var args _) = (var, FuncT $ length args)

type Checker a = WriterT [String] (State (Map Var Type)) a

checkFunc :: Func -> Checker ()
checkFunc (Func _ args tail) = do
                               env <- lift get
                               mapM putInt args
                               checkTail tail
                               lift $ put env

checkTail :: Tail -> Checker ()
checkTail (Expr expr) = checkExpr expr
checkTail (TLet assignments tail) = checkLet checkTail assignments tail
checkTail (TIf p c a) = checkIf checkTail p c a
checkTail (Call f args) = checkCall f args

checkExpr :: Expr -> Checker ()
checkExpr (Triv triv) = checkTriv triv
checkExpr (BinOp _ left right) = checkTriv left >> checkTriv right
checkExpr (Let assignments expr) = checkLet checkExpr assignments expr
checkExpr (If p c a) = checkIf checkExpr p c a

checkPred :: Pred -> Checker ()
checkPred (Bool _) = return ()
checkPred (RelOp _ left right) = checkTriv left >> checkTriv right
checkPred (Not pred) = checkPred pred
checkPred (PLet assignments pred) = checkLet checkPred assignments pred
checkPred (PIf p c a) = checkIf checkPred p c a

checkLet :: (a -> Checker ()) -> [(Var, Expr)] -> a -> Checker ()
checkLet checker assignments body = do
                                    env <- lift get
                                    mapM (checkExpr . snd) assignments
                                    mapM (putInt . fst) assignments
                                    checker body
                                    lift $ put env

checkIf :: (a -> Checker ()) -> Pred -> a -> a -> Checker ()
checkIf checker p c a = do
                        checkPred p
                        checker c
                        checker a

checkTriv :: Triv -> Checker ()
checkTriv (Int _) = return ()
checkTriv (TVar v) = do
                     t <- getType v
                     case t of
                          Nothing -> undefinedVariable v
                          Just (FuncT _) -> tell [unwords ["Functions can only be called :", show v]]
                          Just IntT -> return ()

checkCall :: Var -> [Triv] -> Checker ()
checkCall f args = do
                   t <- getType f
                   case t of
                        Nothing -> undefinedVariable f
                        Just IntT -> tell [unwords ["Cannot call non-function : ", show f]]
                        Just (FuncT n)
                             | length args < n -> tell [unwords ["Too few arguments in call to function :", show f]]
                             | length args > n -> tell [unwords ["Too many arguments in call to function :", show f]]
                             | otherwise -> return ()
                   mapM checkTriv args
                   return ()

putInt :: Var -> Checker ()
putInt v = lift $ modify' (M.insert v IntT)

getType :: Var -> Checker (Maybe Type)
getType v = lift $ gets (M.lookup v)

undefinedVariable :: Var -> Checker ()
undefinedVariable v = tell [unwords ["Undefined variable :", show v]]
