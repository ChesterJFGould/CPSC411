module Unique.Lower
( lower
)
where

import Unique.Types

import qualified Cmf.Types as C

import Control.Monad.Writer

lower :: Program -> C.Program
lower (Program body i) = C.Program (stmts, out) i
                       where (out, stmts) = runWriter (lower' body)

lower' :: Expr -> Writer [C.Stmt] C.Expr
lower' (Triv triv) = return . C.Triv $ lowerTriv triv
lower' (BinOp op left right) = return $ C.BinOp (lowerOp op)
                                                (lowerTriv left)
                                                (lowerTriv right)
lower' (Let assignments body) = mapM lowerAssignment assignments
                                >> lower' body

lowerAssignment :: (Aloc, Expr) -> Writer [C.Stmt] ()
lowerAssignment (aloc, val) = do
                              val' <- lower' val
                              tell [C.Set (lowerAloc aloc) val']

lowerOp :: Op -> C.Op
lowerOp Add = C.Add
lowerOp Mul = C.Mul

lowerTriv :: Triv -> C.Triv
lowerTriv (Int i) = C.Int i
lowerTriv (Aloc aloc) = C.Aloc $ lowerAloc aloc

lowerAloc :: Aloc -> C.Aloc
lowerAloc (Var v i) = C.Var v i
