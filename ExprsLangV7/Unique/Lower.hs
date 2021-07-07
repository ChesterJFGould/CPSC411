module Unique.Lower
( lower
)
where

import Compiler.Ptrs
import Compiler.Types as C
import Compiler.Value
import Unique.Types as U

import qualified Bits.Types as B

import Data.Word

lower :: Program -> B.Program
lower (Program defs body) = B.Program (map lowerFunc defs) (lowerBody body)

lowerFunc :: Func -> B.Func
lowerFunc (Func name args body) = B.Func name args (lowerBody body)

lowerBody :: Body -> B.Body
lowerBody (Body expr) = B.Body (lowerExpr expr)

lowerExpr :: Expr -> B.Expr
lowerExpr (Triv triv) = B.Triv (lowerTriv triv)
lowerExpr (UnOp op operand) = lowerUnOp op operand
lowerExpr (BinOp op a b) = lowerBinOp op a b
lowerExpr (Apply f args) = B.Apply f (map lowerExpr args)
lowerExpr (Let assignments body) = let vars = map fst assignments
                                       vals = map (lowerExpr . snd) assignments
                                       assignments' = zip vars vals
                                       body' = lowerExpr body
                                   in B.Let assignments' body'
lowerExpr (If p c a) = B.If (lowerPred p) (lowerExpr c) (lowerExpr a)

-- This could absolutely be optimized.
lowerPred :: Expr -> B.Pred
lowerPred expr = B.RelOp C.Neq (lowerExpr expr) (lowerExpr (Triv (Value (Bool False))))

lowerTriv :: Triv -> ATriv
lowerTriv (UAloc aloc) = TAloc aloc
lowerTriv (Value value) = APtr (toPtr value)

lowerUnOp :: UnOp -> Expr -> B.Expr
lowerUnOp IsInt expr = hasTag intTag expr
lowerUnOp IsBool expr = hasTag boolTag expr
lowerUnOp IsEmpty expr = hasTag emptyTag expr
lowerUnOp IsVoid expr = hasTag voidTag expr
lowerUnOp IsChar expr = hasTag charTag expr
lowerUnOp IsError expr = hasTag errorTag expr
lowerUnOp Not expr = B.If (lowerPred expr)
                          (lowerExpr (Triv (Value (Bool False))))
                          (lowerExpr (Triv (Value (Bool True))))

lowerBinOp :: U.BinOp -> Expr -> Expr -> B.Expr
lowerBinOp U.Add a b = B.BinOp C.Add (lowerExpr a) (lowerExpr b)
lowerBinOp U.Sub a b = B.BinOp C.Sub (lowerExpr a) (lowerExpr b)
lowerBinOp U.Mul a b = B.BinOp C.Mul (B.BinOp Shr (lowerExpr a) (lowerExpr (Triv (Value (Int 3)))))
                                     (lowerExpr b)
lowerBinOp U.Lt a b = predExpr (B.RelOp C.Lt (lowerExpr a) (lowerExpr b))
lowerBinOp U.Gt a b = predExpr (B.RelOp C.Gt (lowerExpr a) (lowerExpr b))
lowerBinOp U.Eq a b = predExpr (B.RelOp C.Eq (lowerExpr a) (lowerExpr b))
lowerBinOp U.Lte a b = predExpr (B.RelOp C.Lte (lowerExpr a) (lowerExpr b))
lowerBinOp U.Gte a b = predExpr (B.RelOp C.Gte (lowerExpr a) (lowerExpr b))
lowerBinOp U.Neq a b = predExpr (B.RelOp C.Neq (lowerExpr a) (lowerExpr b))

predExpr :: B.Pred -> B.Expr
predExpr p = B.If p
                  (lowerExpr (Triv (Value (Bool True))))
                  (lowerExpr (Triv (Value (Bool False))))

hasTag :: Tag -> Expr -> B.Expr
hasTag (Tag tag mask) expr = B.If (B.RelOp C.Eq (B.BinOp C.And (lowerExpr expr) (wordExpr mask)) (wordExpr tag))
                                  (lowerExpr (Triv (Value (Bool True))))
                                  (lowerExpr (Triv (Value (Bool False))))

wordExpr :: Word64 -> B.Expr
wordExpr = B.Triv . APtr . Ptr
