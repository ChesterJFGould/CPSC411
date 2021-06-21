module Monadic.Lower
( lower
)
where

import Monadic.Types

import qualified Canonical.Types as C

lower :: Program -> C.Program
lower (Program defs body) = C.Program (map lowerBlock defs) (lowerTail body)

lowerBlock :: Block -> C.Block
lowerBlock (Block label body) = C.Block (lowerLabel label) (lowerTail body)

lowerTail :: Tail -> C.Tail
lowerTail (Expr expr) = lowerExpr expr
lowerTail (TSeq stmts tail) = C.TSeq (lowerStmts stmts) (lowerTail tail)
lowerTail (TIf p c a) = C.TIf (lowerPred p) (lowerTail c) (lowerTail a)
lowerTail (Jump label undeadOut) = C.Jump (lowerLabel label) (map lowerLoc undeadOut)

lowerExpr :: Expr -> C.Tail
lowerExpr (Triv triv) = (C.Expr . C.Triv . lowerTriv) triv
lowerExpr (BinOp op left right) = C.Expr (C.BinOp (lowerOp op) (lowerTriv left) (lowerTriv right))
lowerExpr (Seq stmts expr) = C.TSeq (lowerStmts stmts) (lowerExpr expr)
lowerExpr (If p c a) = C.TIf (lowerPred p) (lowerExpr c) (lowerExpr a)

lowerPred :: Pred -> C.Pred
lowerPred (Bool b) = C.Bool b
lowerPred (RelOp op left right) = C.RelOp (lowerRelOp op) (lowerTriv left) (lowerTriv right)
lowerPred (Not pred) = (C.Not . lowerPred) pred
lowerPred (PSeq stmts pred) = C.PSeq (lowerStmts stmts) (lowerPred pred)
lowerPred (PIf p c a) = C.PIf (lowerPred p) (lowerPred c) (lowerPred a)

lowerStmts :: [Stmt] -> [C.Stmt]
lowerStmts = concat . map lowerStmt

lowerStmt :: Stmt -> [C.Stmt]
lowerStmt (Set loc expr) = lowerSet (lowerLoc loc) expr

lowerSet :: C.Loc -> Expr -> [C.Stmt]
lowerSet loc (Triv triv) = [ C.Set loc ((C.Triv . lowerTriv) triv) ]
lowerSet loc (BinOp op left right) = [ C.Set loc (C.BinOp (lowerOp op) (lowerTriv left) (lowerTriv right)) ]
lowerSet loc (Seq stmts expr) = lowerStmts stmts ++ lowerSet loc expr
lowerSet loc (If p c a) = [ C.If (lowerPred p) (lowerSet loc c) (lowerSet loc a) ]

lowerTriv :: Triv -> C.Triv
lowerTriv (Int i) = C.Int i
lowerTriv (TLoc loc) = (C.TLoc . lowerLoc) loc

lowerLoc :: Loc -> C.Loc
lowerLoc (Aloc n i) = C.Aloc n i
lowerLoc (Reg reg) = (C.Reg . lowerReg) reg
lowerLoc (Addr i) = C.Addr i

lowerLabel :: Label -> C.Label
lowerLabel (Label n) = C.Label n

lowerReg :: Reg -> C.Reg
lowerReg RSP = C.RSP
lowerReg RBP = C.RBP
lowerReg RAX = C.RAX
lowerReg RBX = C.RBX
lowerReg RCX = C.RCX
lowerReg RDX = C.RDX
lowerReg RSI = C.RSI
lowerReg RDI = C.RDI
lowerReg R8 = C.R8
lowerReg R9 = C.R9
lowerReg R10 = C.R10
lowerReg R11 = C.R11
lowerReg R12 = C.R12
lowerReg R13 = C.R13
lowerReg R14 = C.R14
lowerReg R15 = C.R15

lowerOp :: Op -> C.Op
lowerOp Add = C.Add
lowerOp Mul = C.Mul

lowerRelOp :: RelOp -> C.RelOp
lowerRelOp Lt = C.Lt
lowerRelOp Gt = C.Gt
lowerRelOp Eq = C.Eq
lowerRelOp Lte = C.Lte
lowerRelOp Gte = C.Gte
lowerRelOp Neq = C.Neq
