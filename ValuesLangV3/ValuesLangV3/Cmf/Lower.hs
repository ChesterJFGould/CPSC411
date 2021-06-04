module Cmf.Lower
( lower
)
where

import Cmf.Types

import qualified Asm.Types as A

import Control.Monad.State
import Control.Monad.Writer
import Data.Map as M
import Data.Set as S

lower :: Program -> A.Program
lower (Program (stmts, out) i) = A.Program stmts' out'
                               where (out', stmts') = (flip evalState) i
                                                      $ runWriterT (mapM lowerStmt stmts
                                                                    >> lowerExpr out)

lowerStmt :: Stmt -> WriterT [A.Stmt] (State Int) ()
lowerStmt (Set aloc expr) = do
                            expr' <- lowerExpr expr
                            tell [A.Stmt A.Set (lowerAloc aloc) expr']

lowerExpr :: Expr -> WriterT [A.Stmt] (State Int) A.Triv
lowerExpr (Triv triv) = return $ lowerTriv triv
lowerExpr (BinOp op left right) = do
                                  tmp <- lift $ gensym "tmp"
                                  tell [ A.Stmt A.Set tmp $ lowerTriv left
                                       , A.Stmt (lowerOp op) tmp $ lowerTriv right ]
                                  return $ A.Aloc tmp

lowerOp :: Op -> A.Op
lowerOp Add = A.Add
lowerOp Mul = A.Mul

lowerTriv :: Triv -> A.Triv
lowerTriv (Int i) = A.Int i
lowerTriv (Aloc aloc) = A.Aloc $ lowerAloc aloc

lowerAloc :: Aloc -> A.Aloc
lowerAloc (Var v i) = A.Var v i

gensym :: String -> State Int A.Aloc
gensym name = do
              i <- get
              put (i + 1)
              return $ A.Var name i

genAlocs :: [A.Stmt] -> Set A.Aloc
genAlocs [] = S.empty
genAlocs (A.Stmt A.Set aloc _ : rest) = S.insert aloc $ genAlocs rest
genAlocs (_ : rest) = genAlocs rest

genAlocAddrs :: Set A.Aloc -> Map A.Aloc Int
genAlocAddrs alocs = M.fromList $ zip (S.toList alocs) [0..]
