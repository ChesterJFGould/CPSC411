module Closures.Lower
( lower
)
where

import Compiler.Types
import Closures.Types
import qualified Pred.Types as P

lower :: Program -> P.Program
lower (Program defs body) = P.Program (lowerDefs defs)
                                      (lowerBody body)

lowerDefs :: [Def] -> [P.Def]
lowerDefs = map lowerDef

lowerDef :: Def -> P.Def
lowerDef (Const label body) = P.Const label (lowerBody body)
lowerDef (Func label env arg body) = P.Func label env arg (lowerBody body)

lowerBody :: Body -> P.Body
lowerBody (Body expr) = P.Body (lowerExpr expr)

lowerExpr :: Expr -> P.Expr
lowerExpr (Value value) = P.Value value
lowerExpr (BinOp (BNumOp op) a b) = P.NumOp op (lowerExpr a)
                                               (lowerExpr b)
lowerExpr (BinOp (BRelOp op) a b) = P.If (P.RelOp op (lowerExpr a)
                                                     (lowerExpr b))
                                         (P.Value (VBool True))
                                         (P.Value (VBool False))
lowerExpr (ApplyClosure f arg) = P.ApplyClosure (lowerExpr f)
                                                (lowerExpr arg)
lowerExpr (Let var val body) = P.Let var (lowerExpr val)
                                         (lowerExpr body)
lowerExpr (Closure label exprs) = P.Closure label (map lowerExpr exprs)
lowerExpr (ClosureRef env index) = P.ClosureRef env index
lowerExpr (If p c a) = P.If (lowerPred p)
                            (lowerExpr c)
                            (lowerExpr a)

lowerPred :: Expr -> P.Pred
lowerPred (Value (VBool b)) = P.Bool b
lowerPred (BinOp (BRelOp op) a b) = P.RelOp op (lowerExpr a)
                                               (lowerExpr b)
lowerPred (Let var val body) = P.PLet var (lowerExpr val)
                                         (lowerPred body)
lowerPred (If p c a) = P.PIf (lowerPred p)
                               (lowerPred c)
                               (lowerPred a)
lowerPred expr = P.RelOp Eq (P.Value (VBool True))
                            (lowerExpr expr)
