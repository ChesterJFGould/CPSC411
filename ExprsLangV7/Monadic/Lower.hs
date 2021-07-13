module Monadic.Lower
( lower
)
where

import Compiler.Types
import Monadic.Types

import qualified Canonical.Types as C

lower :: Program -> C.Program
lower (Program blocks body) = C.Program (map lowerBlock blocks) (lowerBody body)

lowerBlock :: Block -> C.Block
lowerBlock (Block label body) = C.Block label (lowerBody body)

lowerBody :: Body -> C.Body
lowerBody (Body tail) = C.Body (lowerTail tail)

lowerTail :: Tail -> C.Tail
lowerTail (Expr expr) = lowerExpr expr
lowerTail (TSeq stmts tail) = C.Seq (lowerStmts stmts) (lowerTail tail)
lowerTail (TIf p c a) = C.TIf (lowerPred p) (lowerTail c) (lowerTail a)
lowerTail (Jump label undeadOut) = C.Jump label undeadOut

lowerExpr :: Expr -> C.Tail
lowerExpr (Triv triv) = C.Expr (C.Triv triv)
lowerExpr (BinOp op a b) = C.Expr (C.BinOp op a b)
lowerExpr (Seq stmts expr) = C.Seq (lowerStmts stmts) (lowerExpr expr)
lowerExpr (If p c a) = C.TIf (lowerPred p) (lowerExpr c) (lowerExpr a)

lowerStmts :: [Stmt] -> [C.Stmt]
lowerStmts = concat . map lowerStmt

lowerStmt :: Stmt -> [C.Stmt]
lowerStmt (Set loc expr) = lowerSet loc expr
lowerStmt (JumpRet label undeadOut) = [ C.JumpRet label undeadOut ]

lowerSet :: Mloc -> Expr -> [C.Stmt]
lowerSet loc (Triv triv) = [ C.Set loc (C.Triv triv) ]
lowerSet loc (BinOp op a b) = [ C.Set loc (C.BinOp op a b) ]
lowerSet loc (Seq stmts expr) = lowerStmts stmts ++ lowerSet loc expr
lowerSet loc (If p c a) = [ C.If (lowerPred p) (lowerSet loc c) (lowerSet loc a) ]

lowerPred :: Pred -> C.Pred
lowerPred (Bool b) = C.Bool b
lowerPred (RelOp op a b) = C.RelOp op a b
lowerPred (Not pred) = C.Not (lowerPred pred)
lowerPred (PSeq stmts pred) = C.PSeq (lowerStmts stmts) (lowerPred pred)
lowerPred (PIf p c a) = C.PIf (lowerPred p) (lowerPred c) (lowerPred a)
