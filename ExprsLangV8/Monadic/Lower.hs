module Monadic.Lower
( lower
)
where

import qualified Canonical.Types as C
import Monadic.Types

lower :: Program -> C.Program
lower (Program blocks body) = C.Program (lowerBlocks blocks)
                                        (lowerBody body)

lowerBlocks :: [Block] -> [C.Block]
lowerBlocks = map lowerBlock

lowerBlock :: Block -> C.Block
lowerBlock (Block label body) = C.Block label (lowerBody body)

lowerBody :: Body -> C.Body
lowerBody (Body tail) = C.Body (lowerTail tail)

lowerTail :: Tail -> C.Tail
lowerTail (Jump place undeadOut) = C.Jump place undeadOut
lowerTail (TSeq stmts tail) = C.TSeq (lowerStmts stmts)
                                     (lowerTail tail)
lowerTail (TIf p c a) = C.TIf (lowerPred p)
                              (lowerTail c)
                              (lowerTail a)

lowerSet :: (C.Expr -> C.Stmt) -> Expr -> [C.Stmt]
lowerSet cons (Triv triv) = [ cons (C.Triv triv) ]
lowerSet cons (BinOp op a b) = [ cons (C.BinOp op a b) ]
lowerSet cons (MRef ptr offset) = [ cons (C.MRef ptr offset) ]
lowerSet cons (Alloc size) = [ cons (C.Alloc size) ]
lowerSet cons (Seq stmts expr) = lowerStmts stmts ++ lowerSet cons expr
lowerSet cons (If p c a) = [ C.If (lowerPred p)
                                  (lowerSet cons c)
                                  (lowerSet cons a) ]

lowerStmts :: [Stmt] -> [C.Stmt]
lowerStmts = concat . map lowerStmt

lowerStmt :: Stmt -> [C.Stmt]
lowerStmt (Set loc expr) = lowerSet (C.Set loc) expr
lowerStmt (MSet ptr offset expr) = lowerSet (C.MSet ptr offset) expr
lowerStmt (JumpRet label undeadOut) = [ C.JumpRet label undeadOut ]

lowerPred :: Pred -> C.Pred
lowerPred (Bool b) = C.Bool b
lowerPred (RelOp op a b) = C.RelOp op a b
lowerPred (Not pred) = C.Not (lowerPred pred)
lowerPred (PIf p c a) = C.PIf (lowerPred p)
                              (lowerPred c)
                              (lowerPred a)
lowerPred (PSeq stmts pred) = C.PSeq (lowerStmts stmts)
                                     (lowerPred pred)
