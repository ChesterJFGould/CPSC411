module Bits.Lower
( lower
)
where

import Bits.Types
import Compiler.Gensym
import Compiler.Types
import qualified Values.Types as V

lower :: Program -> Gensym V.Program
lower (Program defs body) = V.Program <$> lowerDefs defs
                                      <*> lowerBody body

lowerDefs :: [Func] -> Gensym [V.Func]
lowerDefs = mapM lowerFunc

lowerFunc :: Func -> Gensym V.Func
lowerFunc (Func name args body) = V.Func name args <$> lowerBody body

lowerBody :: Body -> Gensym V.Body
lowerBody (Body expr) = V.Body <$> lowerExpr expr

lowerExpr :: Expr -> Gensym V.Expr
lowerExpr (Triv triv) = return (V.Triv triv)
lowerExpr (BinOp op a b) = trivialize V.Let (\[a', b'] -> V.BinOp op a' b') [a, b]
lowerExpr (MRef ptr offset) = trivialize V.Let (\[ptr', offset'] -> V.MRef ptr' offset') [ptr, offset]
lowerExpr (Alloc expr) = trivialize V.Let (\[expr'] -> V.Alloc expr') [expr]
lowerExpr (Apply f args) = trivialize V.Let (V.Call f) args
lowerExpr (Let assignments expr) = V.Let <$> lowerAssignments assignments
                                         <*> lowerExpr expr
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
lowerPred (PLet assignments pred) = V.PLet <$> lowerAssignments assignments
                                           <*> lowerPred pred
lowerPred (PIf p c a) = V.PIf <$> lowerPred p
                              <*> lowerPred c
                              <*> lowerPred a
lowerPred (PSeq stmts pred) = V.PSeq <$> lowerStmts stmts
                                     <*> lowerPred pred

lowerAssignments :: [(Aloc, Expr)] -> Gensym [(Aloc, V.Expr)]
lowerAssignments = mapM lowerAssignment

lowerAssignment :: (Aloc, Expr) -> Gensym (Aloc, V.Expr)
lowerAssignment (aloc, expr) = (,) aloc <$> lowerExpr expr

trivialize :: ([(Aloc, V.Expr)] -> a -> a) -> ([ATriv] -> a) -> [Expr] -> Gensym a
trivialize = trivialize' []

trivialize' :: [ATriv] -> ([(Aloc, V.Expr)] -> a -> a) -> ([ATriv] -> a) -> [Expr] -> Gensym a
trivialize' acc let' cont [] = return (cont (reverse acc))
trivialize' acc let' cont (Triv triv : rest) = trivialize' (triv : acc) let' cont rest
trivialize' acc let' cont (expr : rest) = do
                                          tmp <- genAloc "tmp"
                                          expr' <- lowerExpr expr
                                          rest' <- trivialize' (AAloc tmp : acc) let' cont rest
                                          return (let' [(tmp, expr')] rest')
