module Consts.Lower
( lower
)
where

import Consts.Types
import Compiler.Gensym
import Compiler.Types
import qualified Values.Types as V

lower :: Program -> Gensym V.Program
lower (Program labels funcs body) = V.Program labels <$> lowerFuncs funcs
                                                     <*> lowerBody body

lowerFuncs :: [Func] -> Gensym [V.Func]
lowerFuncs = mapM lowerFunc

lowerFunc :: Func -> Gensym V.Func
lowerFunc (Func label env arg body) = V.Func label env arg <$> lowerBody body

lowerBody :: Body -> Gensym V.Body
lowerBody (Body expr) = V.Body <$> lowerExpr expr

lowerExpr :: Expr -> Gensym V.Expr
lowerExpr (Triv triv) = return (V.Triv triv)
lowerExpr (NumOp op a b) = trivialize V.Let (\[a', b'] -> V.NumOp op a' b') [a, b]
lowerExpr (Apply f env arg) = trivialize V.Let (\[f', env', arg'] -> V.Apply f' env' arg') [f, env, arg]
lowerExpr (Let var val body) = V.Let var <$> lowerExpr val
                                         <*> lowerExpr body
lowerExpr (Alloc expr) = trivialize V.Let (\[expr'] -> V.Alloc expr') [expr]
lowerExpr (MRef ptr offset) = trivialize V.Let (\[ptr', offset'] -> V.MRef ptr' offset') [ptr, offset]
lowerExpr (If p c a) = V.If <$> lowerPred p
                            <*> lowerExpr c
                            <*> lowerExpr a
lowerExpr (Seq stmts expr) = V.Seq <$> lowerStmts stmts
                                   <*> lowerExpr expr

lowerStmts :: [Stmt] -> Gensym [V.Stmt]
lowerStmts = mapM lowerStmt

lowerStmt :: Stmt -> Gensym V.Stmt
lowerStmt (MSet ptr offset val) = do
                                  val' <- lowerExpr val
                                  trivialize V.SLet (\[ptr', offset'] -> V.MSet ptr' offset' val') [ptr, offset]

lowerPred :: Pred -> Gensym V.Pred
lowerPred (Bool b) = return (V.Bool b)
lowerPred (RelOp op a b) = trivialize V.PLet (\[a', b'] -> V.RelOp op a' b') [a, b]
lowerPred (Not pred) = V.Not <$> lowerPred pred
lowerPred (PLet var val pred) = V.PLet var <$> lowerExpr val
                                           <*> lowerPred pred
lowerPred (PIf p c a) = V.PIf <$> lowerPred p
                              <*> lowerPred c
                              <*> lowerPred a

trivialize :: (Aloc -> V.Expr -> a -> a) -> ([ATriv] -> a) -> [Expr] -> Gensym a
trivialize = trivialize' []

trivialize' :: [ATriv] -> (Aloc -> V.Expr -> a -> a) -> ([ATriv] -> a) -> [Expr] -> Gensym a
trivialize' acc letCons aCons [] = return (aCons (reverse acc))
trivialize' acc letCons aCons (Triv triv : rest) = trivialize' (triv : acc) letCons aCons rest
trivialize' acc letCons aCons (expr : rest) = do
                                              tmpAloc <- genAloc "tmp"
                                              let tmpTriv = AAloc tmpAloc
                                              letCons tmpAloc <$> lowerExpr expr
                                                              <*> trivialize' (tmpTriv : acc) letCons aCons rest
