module Values.Lower
( lower
)
where

import Compiler.Types
import Values.Types

import qualified Unique.Types as U

import Control.Monad.State
import Data.Map as M

type Gensym a = State (Map Var Aloc, Int) a

lower :: Program -> U.Program
lower (Program defs body) = evalState (U.Program <$> mapM lowerFunc defs
                                                 <*> lowerBody body)
                                      (M.empty, 0)

lowerFunc :: Func -> Gensym U.Func
lowerFunc (Func f args body) = do
                               env <- gets fst
                               args' <- mapM gensym args
                               body' <- lowerBody body
                               return (U.Func (lowerLabel f) args' body')

lowerBody :: Body -> Gensym U.Body
lowerBody (Body body) = U.Body <$> gensymS "lr"
                               <*> lowerTail body

lowerTail :: Tail -> Gensym U.Tail
lowerTail (Expr expr) = U.Expr <$> lowerExpr expr
lowerTail (TLet assignments tail) = lowerLet U.TLet lowerTail assignments tail
lowerTail (TIf p c a) = lowerIf U.TIf lowerTail p c a
lowerTail (TCall f args) = lowerCall U.TCall f args

lowerExpr :: Expr -> Gensym U.Expr
lowerExpr (Triv triv) = U.Triv <$> lowerTriv triv
lowerExpr (BinOp op l r) = U.BinOp op <$> lowerTriv l
                                      <*> lowerTriv r
lowerExpr (Let assignments expr) = lowerLet U.Let lowerExpr assignments expr
lowerExpr (If p c a) = lowerIf U.If lowerExpr p c a
lowerExpr (Call f args) = lowerCall U.Call f args

lowerPred :: Pred -> Gensym U.Pred
lowerPred (Bool b) = return (U.Bool b)
lowerPred (RelOp op l r) = U.RelOp op <$> lowerTriv l
                                      <*> lowerTriv r
lowerPred (Not pred) = U.Not <$> lowerPred pred
lowerPred (PLet assignments pred) = lowerLet U.PLet lowerPred assignments pred
lowerPred (PIf p c a) = lowerIf U.PIf lowerPred p c a

lowerLet :: ([(Aloc, U.Expr)] -> a -> a) -> (b -> Gensym a) -> [(Var, Expr)] -> b -> Gensym a
lowerLet cons lowerer assignments body = do
                                         env <- gets fst
                                         vals <- mapM (lowerExpr . snd) assignments
                                         vars <- mapM (gensym . fst) assignments -- gensym also modifies state
                                         body' <- lowerer body
                                         i <- gets snd
                                         put (env, i)
                                         return (cons (zip vars vals) body')

lowerIf :: (U.Pred -> a -> a -> a) -> (b -> Gensym a) -> Pred -> b -> b -> Gensym a
lowerIf cons lowerer p c a = cons <$> lowerPred p
                                  <*> lowerer c
                                  <*> lowerer a

lowerCall :: (Label -> [U.Triv] -> a) -> Var -> [Triv] -> Gensym a
lowerCall cons f args = cons (lowerLabel f) <$> mapM lowerTriv args

lowerTriv :: Triv -> Gensym U.Triv
lowerTriv (Int i) = return (U.Int i)
lowerTriv (TVar var) = U.TAloc <$> lowerVar var

lowerVar :: Var -> Gensym Aloc
lowerVar var = gets ((! var) . fst)

lowerLabel :: Var -> Label
lowerLabel (Var v) = Label v

gensymS :: String -> Gensym Aloc
gensymS = gensym . Var

gensym :: Var -> Gensym Aloc
gensym var@(Var n) = do
                     (env, i) <- get
                     let aloc = Aloc n i
                     put (M.insert var aloc env, i + 1)
                     return aloc
