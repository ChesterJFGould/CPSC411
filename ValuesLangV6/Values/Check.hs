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
          deriving (Eq, Ord)

type Checker a = WriterT [String] (State (Map Var Type)) a

check :: Program -> Either String Program
check prog@(Program defs body) = case errors of
                                      [] -> Right prog
                                      _ -> Left (intercalate "\n" errors)
                               where errors = evalState (execWriterT checkProg) env
                                     checkProg = sequence [ checkDuplicateDef defs
                                                          , mapM checkFunc defs >> return ()
                                                          , checkBody body ]
                                     env = M.fromList (L.map funcType defs)

checkDuplicateDef :: [Func] -> Checker ()
checkDuplicateDef = checkDuplicateDef' []

checkDuplicateDef' :: [Var] -> [Func] -> Checker ()
checkDuplicateDef' defs [] = return ()
checkDuplicateDef' defs (Func name _ _ : rest) = if name `elem` defs
                                                 then tell [ unwords ["Duplicate definitions for function :", show name] ]
                                                      >> checkDuplicateDef' defs rest
                                                 else checkDuplicateDef' (name : defs) rest

checkFunc :: Func -> Checker ()
checkFunc (Func _ args body) = do
                               env <- lift get
                               mapM putInt args
                               checkBody body
                               lift (put env)

checkBody :: Body -> Checker()
checkBody (Body tail) = checkTail tail

checkTail :: Tail -> Checker ()
checkTail (Expr expr) = checkExpr expr
checkTail (TLet assignments tail) = checkLet checkTail assignments tail
checkTail (TIf p c a) = checkIf checkTail p c a
checkTail (TCall f args) = checkCall f args

checkExpr :: Expr -> Checker ()
checkExpr (Triv triv) = checkTriv triv
checkExpr (BinOp _ l r) = mapM checkTriv [l, r] >> return ()
checkExpr (Let assignments expr) = checkLet checkExpr assignments expr
checkExpr (If p c a) = checkIf checkExpr p c a
checkExpr (Call f args) = checkCall f args

checkPred :: Pred -> Checker ()
checkPred (Bool _) = return ()
checkPred (RelOp _ l r) = mapM checkTriv [l, r] >> return ()
checkPred (Not pred) = checkPred pred
checkPred (PLet assignments pred) = checkLet checkPred assignments pred
checkPred (PIf p c a) = checkIf checkPred p c a

checkLet :: (a -> Checker ()) -> [(Var, Expr)] -> a -> Checker ()
checkLet checker assignments body = do
                                    env <- lift get
                                    mapM (checkExpr . snd) assignments
                                    mapM (putInt . fst) assignments
                                    checker body
                                    lift (put env)

checkIf :: (a -> Checker ()) -> Pred -> a -> a -> Checker ()
checkIf checker p c a = checkPred p >> mapM checker [c, a] >> return ()

checkCall :: Var -> [Triv] -> Checker ()
checkCall f args = do
                   fT <- getType f
                   case fT of
                        Nothing -> undefinedVariable f
                        Just IntT -> tell [ unwords ["Cannot call Int :", show f] ]
                        Just (FuncT n)
                             | n < length args -> tell [ unwords ["Too many arguments in call to function :", show f] ]
                             | n > length args -> tell [ unwords ["Too few arguments in call to function :", show f] ]
                             | otherwise -> return ()
                   mapM checkTriv args
                   return ()

checkTriv :: Triv -> Checker ()
checkTriv (Int _) = return ()
checkTriv (TVar var) = do
                       varT <- getType var
                       case varT of
                            Nothing -> undefinedVariable var
                            Just IntT -> return ()
                            Just (FuncT _) -> tell [ unwords ["Functions can only be called :", show var] ]

undefinedVariable :: Var -> Checker ()
undefinedVariable var = tell [ unwords ["Undefined variable :", show var] ]

getType :: Var -> Checker (Maybe Type)
getType = lift . gets . M.lookup

putInt :: Var -> Checker ()
putInt = lift . modify . ((flip M.insert) IntT)

funcType :: Func -> (Var, Type)
funcType (Func f args _) = (f, FuncT (length args))
