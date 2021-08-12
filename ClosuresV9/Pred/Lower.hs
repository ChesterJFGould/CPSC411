module Pred.Lower
( lower
)
where

import qualified Bits.Types as B
import Compiler.Gensym
import Compiler.Types
import Compiler.LowerValue
import Pred.Types

import Data.Word

lower :: Program -> Gensym B.Program
lower (Program defs body) = B.Program <$> lowerDefs defs
                                      <*> lowerBody body

lowerDefs :: [Def] -> Gensym [B.Def]
lowerDefs = mapM lowerDef

lowerDef :: Def -> Gensym B.Def
lowerDef (Const label body) = B.Const label <$> lowerBody body
lowerDef (Func label env arg body) = B.Func label env arg <$> lowerBody body

lowerBody :: Body -> Gensym B.Body
lowerBody (Body expr) = B.Body <$> lowerExpr expr

lowerExpr :: Expr -> Gensym B.Expr
lowerExpr (Value value) = return (B.Triv (lowerValue value))
lowerExpr (NumOp op a b) = B.NumOp op <$> lowerExpr a
                                      <*> lowerExpr b
lowerExpr (ApplyClosure f arg) = do
                                 functionAloc <- genAloc "function"
                                 f' <- lowerExpr f
                                 let functionExpr= B.Triv (AAloc functionAloc)
                                 arg' <- lowerExpr arg
                                 return (B.Let functionAloc f'
                                               (B.Apply (B.MRef functionExpr (intExpr 0))
                                                        functionExpr
                                                        arg'))
lowerExpr (Let var val body) = B.Let var <$> lowerExpr val
                                         <*> lowerExpr body
lowerExpr (Closure label exprs) = do
                                  closureAloc <- genAloc "closure"
                                  let closureExpr = B.Triv (AAloc closureAloc)
                                      labelExpr = B.Triv (ALabel label)
                                      labelSet = B.MSet closureExpr (intExpr 0) labelExpr
                                  exprs' <- mapM lowerExpr exprs
                                  let exprsSet = zipWith (\i expr -> B.MSet closureExpr (intExpr (8 * (i + 1))) expr) [0..] exprs'
                                      closureSize = 8 + 8 * length exprs
                                  return (B.Let closureAloc (B.Alloc (intExpr (fromIntegral closureSize)))
                                                (B.Seq [ labelSet ]
                                                       (B.Seq exprsSet closureExpr)))
lowerExpr (ClosureRef aloc index) = return (B.MRef (B.Triv (AAloc aloc)) (intExpr (fromIntegral (8 * (index + 1)))))
lowerExpr (If p c a) = B.If <$> lowerPred p
                            <*> lowerExpr c
                            <*> lowerExpr a

lowerPred :: Pred -> Gensym B.Pred
lowerPred (Bool b) = return (B.Bool b)
lowerPred (RelOp op a b) = B.RelOp op <$> lowerExpr a
                                      <*> lowerExpr b
lowerPred (Not pred) = B.Not <$> lowerPred pred
lowerPred (PLet var val body) = B.PLet var <$> lowerExpr val
                                           <*> lowerPred body

intExpr :: Word64 -> B.Expr
intExpr i = B.Triv (ALit (Lit i))
