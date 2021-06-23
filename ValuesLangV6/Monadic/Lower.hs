module Monadic.Lower
( lower
)
where

import Compiler.Types
import Monadic.Types

import qualified Canonical.Types as C

lower :: Program -> C.Program
lower (Program defs body) = C.Program (map lowerBlock defs) (lowerBody body)

lowerBlock :: Block -> C.Block
lowerBlock (Block label body) = C.Block label (lowerBody body)

lowerBody :: Body -> C.Body
lowerBody (Body lr tail) = C.Body lr (lowerTail tail)

lowerTail :: Tail -> C.Tail
lowerTail (Expr expr) = lowerExpr expr
lowerTail (TSeq stmts tail) = C.TSeq (lowerStmts stmts) (lowerTail tail)
lowerTail (TIf p c a) = C.TIf (lowerPred p) (lowerTail c) (lowerTail a)
lowerTail (Jump label undeadOut) = C.Jump label undeadOut

lowerExpr :: Expr -> C.Tail
lowerExpr (Triv triv) = (C.Expr . C.Triv) (lowerTriv triv)
lowerExpr (BinOp op l r) = C.Expr (C.BinOp op (lowerTriv l) (lowerTriv r))
lowerExpr (Seq stmts expr) = C.TSeq (lowerStmts stmts) (lowerExpr expr)
lowerExpr (If p c a) = C.TIf (lowerPred p) (lowerExpr c) (lowerExpr a)

lowerStmts :: [Stmt] -> [C.Stmt]
lowerStmts = concat . map lowerStmt

lowerStmt :: Stmt -> [C.Stmt]
lowerStmt (Set loc expr) = lowerSet loc expr
lowerStmt (JumpRet label undeadOut) = [ C.JumpRet label undeadOut ]

lowerSet :: MLoc -> Expr -> [C.Stmt]
lowerSet loc (Triv triv) = [ C.Set loc ((C.Triv . lowerTriv) triv) ]
lowerSet loc (BinOp op l r) = [ C.Set loc (C.BinOp op (lowerTriv l) (lowerTriv r)) ]
lowerSet loc (Seq stmts expr) = lowerStmts stmts ++ lowerSet loc expr
lowerSet loc (If p c a) = [ C.If (lowerPred p) (lowerSet loc c) (lowerSet loc a) ]

lowerPred :: Pred -> C.Pred
lowerPred (Bool b) = C.Bool b
lowerPred (RelOp op l r) = C.RelOp op (lowerTriv l) (lowerTriv r)
lowerPred (Not pred) = C.Not (lowerPred pred)
lowerPred (PSeq stmts pred) = C.PSeq (lowerStmts stmts) (lowerPred pred)
lowerPred (PIf p c a) = C.PIf (lowerPred p) (lowerPred c) (lowerPred a)

lowerTriv :: Triv -> C.Triv
lowerTriv (Int i) = C.Int i
lowerTriv (Loc loc) = C.Loc loc
