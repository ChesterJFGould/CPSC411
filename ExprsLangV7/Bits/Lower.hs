module Bits.Lower
( lower
)
where

import Bits.Types
import Compiler.Types

import qualified Values.Types as V

import Control.Monad.State

lower :: Program -> Gensym V.Program
lower (Program defs body) = V.Program <$> mapM lowerFunc defs
                                      <*> lowerBody body

lowerFunc :: Func -> Gensym V.Func
lowerFunc (Func name args body) = V.Func name args <$> lowerBody body

lowerBody :: Body -> Gensym V.Body
lowerBody (Body expr) = V.Body <$> lowerExpr expr

lowerExpr :: Expr -> Gensym V.Expr
lowerExpr (Triv triv) = return (V.Triv triv)
lowerExpr (BinOp op a b) = decompose V.Let (\[a', b'] -> V.BinOp op a' b') [a, b]
lowerExpr (Apply f args) = decompose V.Let (\args' -> V.Call f args') args
lowerExpr (Let assignments body) = lowerLet V.Let lowerExpr assignments body
lowerExpr (If p c a) = V.If <$> lowerPred p
                            <*> lowerExpr c
                            <*> lowerExpr a

lowerPred :: Pred -> Gensym V.Pred
lowerPred (Bool b) = return (V.Bool b)
lowerPred (RelOp op a b) = decompose V.PLet (\[a', b'] -> V.RelOp op a' b') [a, b]
lowerPred (Not pred) = V.Not <$> lowerPred pred
lowerPred (PLet assignments pred) = lowerLet V.PLet lowerPred assignments pred
lowerPred (PIf p c a) = V.PIf <$> lowerPred p
                              <*> lowerPred c
                              <*> lowerPred a

decompose :: ([(Aloc, V.Expr)] -> a -> a) -> ([ATriv] -> a) -> [Expr] -> Gensym a
decompose = decompose' []

decompose' :: [ATriv] -> ([(Aloc, V.Expr)] -> a -> a) -> ([ATriv] -> a) -> [Expr] -> Gensym a
decompose' acc let' cons [] = return (cons (reverse acc))
decompose' acc let' cons (Triv triv : rest) = decompose' (triv : acc) let' cons rest
decompose' acc let' cons (expr : rest) = do
                                         temp <- genAloc "temp"
                                         expr' <- lowerExpr expr
                                         rest' <- decompose' (TAloc temp : acc) let' cons rest
                                         return (let' [(temp, expr')] rest')

lowerLet :: ([(Aloc, V.Expr)] -> a -> a) -> (b -> Gensym a) -> [(Aloc, Expr)] -> b -> Gensym a
lowerLet cons lowerer assignments body = do
                                         vals' <- mapM (lowerExpr . snd) assignments
                                         body' <- lowerer body
                                         let vars = map fst assignments
                                             assignments' = zip vars vals'
                                         return (cons assignments' body')

genAloc :: String -> Gensym Aloc
genAloc template = do
                   i <- get
                   let aloc = Aloc template i
                   put (i + 1)
                   return aloc
