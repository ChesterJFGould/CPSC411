module Block.Lower
( lower
)
where

import Block.Types

import qualified Para.Types as P

lower :: Program -> P.Program
lower (Program defs body) = P.Program (lowerTail body ++ (concat . map lowerBlock) defs)

lowerBlock :: Block -> [P.Stmt]
lowerBlock (Block label body) = ( P.Labelled (lowerLabel label) fst
                                : rest )
                              where (fst : rest) = lowerTail body

lowerTail :: Tail -> [P.Stmt]
lowerTail (Halt triv) = [ P.Halt (lowerTriv triv) ]
lowerTail (Seq stmts tail) = map lowerStmt stmts ++ lowerTail tail
lowerTail (If (RelOp op loc triv) c a) = [ P.Compare (lowerLoc loc) (lowerTriv triv)
                                         , P.JumpIf (lowerRelOp op) (lowerLabel c)
                                         , P.Jump (lowerLabel a) ]
lowerTail (Jump label) = [ P.Jump (lowerLabel label) ]

lowerStmt :: Stmt -> P.Stmt
lowerStmt (Set loc triv) = P.Set (lowerLoc loc) (lowerTriv triv)
lowerStmt (BinOp op loc triv) = P.BinOp (lowerOp op) (lowerLoc loc) (lowerTriv triv)

lowerTriv :: Triv -> P.Triv
lowerTriv (Int i) = P.Int i
lowerTriv (Loc loc) = P.Loc (lowerLoc loc)

lowerLoc :: Loc -> P.Loc
lowerLoc (Reg reg) = P.Reg (lowerReg reg)
lowerLoc (Addr i) = P.Addr i

lowerLabel :: Label -> P.Label
lowerLabel (Label label) = P.Label label

lowerReg :: Reg -> P.Reg
lowerReg RSP = P.RSP
lowerReg RBP = P.RBP
lowerReg RAX = P.RAX
lowerReg RBX = P.RBX
lowerReg RCX = P.RCX
lowerReg RDX = P.RDX
lowerReg RSI = P.RSI
lowerReg RDI = P.RDI
lowerReg R8 = P.R8
lowerReg R9 = P.R9
lowerReg R10 = P.R10
lowerReg R11 = P.R11
lowerReg R12 = P.R12
lowerReg R13 = P.R13
lowerReg R14 = P.R14
lowerReg R15 = P.R15

lowerOp :: Op -> P.Op
lowerOp Add = P.Add
lowerOp Mul = P.Mul

lowerRelOp :: RelOp -> P.RelOp
lowerRelOp Lt = P.Lt
lowerRelOp Gt = P.Gt
lowerRelOp Eq = P.Eq
lowerRelOp Lte = P.Lte
lowerRelOp Gte = P.Gte
lowerRelOp Neq = P.Neq
