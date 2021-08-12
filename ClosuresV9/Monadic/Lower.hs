module Monadic.Lower
( lower
)
where

import qualified Canonical.Types as C
import Monadic.Types

lower :: Program -> C.Program
lower (Program labels blocks body) = C.Program labels (lowerBlocks blocks)
                                                      (lowerBody body)

lowerBlocks :: [Block] -> [C.Block]
lowerBlocks = map lowerBlock

lowerBlock :: Block -> C.Block
lowerBlock (Block label body) = C.Block label (lowerBody body)

lowerBody :: Body -> C.Body
lowerBody (Body tail) = C.Body (lowerTail tail)

lowerTail :: Tail -> C.Tail
lowerTail (Jump triv used) = C.Jump triv used
lowerTail (TSeq stmts tail) = C.TSeq (lowerStmts stmts)
                                     (lowerTail tail)
lowerTail (TIf p c a) = C.TIf (lowerPred p)
                              (lowerTail c)
                              (lowerTail a)

lowerStmts :: [Stmt] -> [C.Stmt]
lowerStmts = concat . map lowerStmt

lowerStmt :: Stmt -> [C.Stmt]
lowerStmt (Set loc expr) = lowerExpr (C.Set loc) expr
lowerStmt (MSet ptr offset expr) = lowerExpr (C.MSet ptr offset) expr
lowerStmt (JumpRet triv used) = [ C.JumpRet triv used ]

lowerExpr :: (C.Expr -> C.Stmt) -> Expr -> [C.Stmt]
lowerExpr setCons (Triv triv) = [ setCons (C.Triv triv) ]
lowerExpr setCons (NumOp op a b) = [ setCons (C.NumOp op a b) ]
lowerExpr setCons (Alloc triv) = [ setCons (C.Alloc triv) ]
lowerExpr setCons (MRef ptr offset) = [ setCons (C.MRef ptr offset) ]
lowerExpr setCons (Seq stmts expr) = lowerStmts stmts ++ lowerExpr setCons expr
lowerExpr setCons (If p c a) = [ C.If (lowerPred p)
                                      (lowerExpr setCons c)
                                      (lowerExpr setCons a)
                               ]

lowerPred :: Pred -> C.Pred
lowerPred (Bool b) = C.Bool b
lowerPred (RelOp op a b) = C.RelOp op a b
lowerPred (Not pred) = C.Not (lowerPred pred)
lowerPred (PSeq stmts pred) = C.PSeq (lowerStmts stmts)
                                     (lowerPred pred)
lowerPred (PIf p c a) = C.PIf (lowerPred p)
                              (lowerPred c)
                              (lowerPred a)
