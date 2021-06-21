module Canonical.Lower
( lower
)
where

import Canonical.Types

import qualified Asm.Types as A

lower :: Program -> A.Program
lower (Program defs body) = A.Program (map lowerBlock defs) (lowerTail body)

lowerBlock :: Block -> A.Block
lowerBlock (Block label body) = A.Block (lowerLabel label) (lowerTail body)

lowerTail :: Tail -> A.Tail
lowerTail (Expr (Triv triv)) = (A.Halt . lowerTriv) triv
lowerTail (Expr (BinOp op (Int a) (Int b))) = (A.Halt . A.Int) (reifyOp op a b)
lowerTail (Expr (BinOp op (Int i) (TLoc loc))) = lowerTail (Expr (BinOp op (TLoc loc) (Int i)))
lowerTail (Expr (BinOp op (TLoc loc) triv)) = A.TSeq [ A.BinOp (lowerOp op) (lowerLoc loc) (lowerTriv triv) ]
                                                     ((A.Halt . A.Loc) (lowerLoc loc))
lowerTail (TSeq stmts tail) = A.TSeq (lowerStmts stmts) (lowerTail tail)
lowerTail (TIf p c a) = A.TIf (lowerPred p) (lowerTail c) (lowerTail a)
lowerTail (Jump label undeadOut) = A.Jump (lowerLabel label) (map lowerLoc undeadOut)

lowerStmts :: [Stmt] -> [A.Stmt]
lowerStmts = concat . map lowerStmt

lowerStmt :: Stmt -> [A.Stmt]
lowerStmt (Set loc (Triv triv)) = [ A.Set (lowerLoc loc) (lowerTriv triv) ]
lowerStmt (Set loc (BinOp op (Int a) (Int b))) = [ A.Set (lowerLoc loc) (A.Int (reifyOp op a b)) ]
lowerStmt (Set loc (BinOp op (Int i) (TLoc loc'))) = lowerStmt (Set loc (BinOp op (TLoc loc') (Int i)))
lowerStmt (Set loc (BinOp op (TLoc loc') triv)) = [ A.Set (lowerLoc loc) ((A.Loc . lowerLoc) loc')
                                                  , A.BinOp (lowerOp op) (lowerLoc loc) (lowerTriv triv) ]
lowerStmt (If p c a) = [ A.If (lowerPred p) (lowerStmts c) (lowerStmts a) ]

lowerPred :: Pred -> A.Pred
lowerPred (Bool b) = A.Bool b
lowerPred (RelOp op (Int a) (Int b)) = A.Bool (reifyRelOp op a b)
lowerPred (RelOp op (Int i) (TLoc loc)) = lowerPred (Not (RelOp op (TLoc loc) (Int i)))
lowerPred (RelOp op (TLoc loc) triv) = A.RelOp (lowerRelOp op) (lowerLoc loc) (lowerTriv triv)
lowerPred (Not pred) = (A.Not . lowerPred) pred
lowerPred (PSeq stmts pred) = A.PSeq (lowerStmts stmts) (lowerPred pred)
lowerPred (PIf p c a) = A.PIf (lowerPred p) (lowerPred c) (lowerPred a)

lowerTriv :: Triv -> A.Triv
lowerTriv (Int i) = A.Int i
lowerTriv (TLoc loc) = (A.Loc . lowerLoc) loc

lowerLoc :: Loc -> A.Loc
lowerLoc (Aloc n i) = A.LAloc (A.Aloc n i)
lowerLoc (Reg reg) = (A.LRloc . A.Reg . lowerReg) reg
lowerLoc (Addr i) = (A.LRloc . A.Addr) i

lowerLabel :: Label -> A.Label
lowerLabel (Label n) = A.Label n

reifyOp :: Op -> Integer -> Integer -> Integer
reifyOp Add = (+)
reifyOp Mul = (*)

reifyRelOp :: RelOp -> Integer -> Integer -> Bool
reifyRelOp Lt = (<)
reifyRelOp Gt = (>)
reifyRelOp Eq = (==)
reifyRelOp Lte = (<=)
reifyRelOp Gte = (>=)
reifyRelOp Neq = (/=)

lowerReg :: Reg -> A.Reg
lowerReg RSP = A.RSP
lowerReg RBP = A.RBP
lowerReg RAX = A.RAX
lowerReg RBX = A.RBX
lowerReg RCX = A.RCX
lowerReg RDX = A.RDX
lowerReg RSI = A.RSI
lowerReg RDI = A.RDI
lowerReg R8 = A.R8
lowerReg R9 = A.R9
lowerReg R10 = A.R10
lowerReg R11 = A.R11
lowerReg R12 = A.R12
lowerReg R13 = A.R13
lowerReg R14 = A.R14
lowerReg R15 = A.R15

lowerOp :: Op -> A.Op
lowerOp Add = A.Add
lowerOp Mul = A.Mul

lowerRelOp :: RelOp -> A.RelOp
lowerRelOp Lt = A.Lt
lowerRelOp Gt = A.Gt
lowerRelOp Eq = A.Eq
lowerRelOp Lte = A.Lte
lowerRelOp Gte = A.Gte
lowerRelOp Neq = A.Neq
