module Values.Lower
( lower
)
where

import Values.Types
import qualified Unique.Types as U

import Control.Monad.State
import Data.Map hiding (map)

lower :: Program -> U.Program
lower (Program body) = U.Program body' i
                     where (body', (i, _)) = runState (lower' body) (0, empty)

lower' :: Expr -> State (Int, Map Var U.Aloc) U.Expr
lower' (Triv triv) = lowerTriv triv >>= return . U.Triv
lower' (BinOp op left right) = do
                               left <- lowerTriv left
                               right <- lowerTriv right
                               return $ U.BinOp (lowerOp op) left right
lower' (Let assignments body) = do
                                (_, env) <- get
                                vals' <- mapM (lower' . snd) assignments
                                vars' <- mapM (gensym . fst) assignments
                                body' <- lower' body
                                (i, _) <- get
                                put (i, env)
                                return $ U.Let (zip vars' vals') body'

lowerOp :: Op -> U.Op
lowerOp Add = U.Add
lowerOp Mul = U.Mul

lowerTriv :: Triv -> State (Int, Map Var U.Aloc) U.Triv
lowerTriv (Int i) = return $ U.Int i
lowerTriv (TVar var) = lowerVar var >>= return . U.Aloc

lowerVar :: Var -> State (Int, Map Var U.Aloc) U.Aloc
lowerVar var = get >>= return . (! var) . snd

gensym :: Var -> State (Int, Map Var U.Aloc) U.Aloc
gensym var@(Var v) = do
                     (i, map) <- get
                     put (i + 1, insert var (U.Var v i) map)
                     return $ U.Var v i
