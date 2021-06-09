module Block.Lower
( lower
)
where

import Block.Types

import qualified Para.Types as P

lower :: Program -> P.Program
lower (Program blocks main) = P.Program stmts
                            where stmts = main' : (concat . map lowerBlock) blocks
                                  main' = P.Jump $ lowerLabel main

lowerBlock :: Block -> [P.Stmt]
lowerBlock (Block label top) = (P.Labelled label' fst) : rest
                             where fst : rest = lowerTop top
                                   label' = lowerLabel label

lowerTop :: Top -> [P.Stmt]
lowerTop (Halt triv) = [ P.Halt triv' ]
                     where triv' = lowerTriv triv
lowerTop (Jump label) = [ P.Jump label' ]
                      where label' = lowerLabel label
lowerTop (Seq stmts top) = stmts' ++ top'
                         where stmts' = lowerStmts stmts
                               top' = lowerTop top
lowerTop (If (RelOp op loc triv) c a) = [ P.Compare loc' triv'
                                        , P.JumpIf op' c'
                                        , P.Jump a' ]
                                      where loc' = lowerLoc loc
                                            triv' = lowerTriv triv
                                            op' = lowerRelOp op
                                            c' = lowerLabel c
                                            a' = lowerLabel a

lowerStmts :: [Stmt] -> [P.Stmt]
lowerStmts = map lowerStmt

lowerStmt :: Stmt -> P.Stmt
lowerStmt (Set loc triv) = P.Set loc' triv'
                         where loc' = lowerLoc loc
                               triv' = lowerTriv triv
lowerStmt (BinOp op loc triv) = P.BinOp op' loc' triv'
                              where op' = lowerOp op
                                    loc' = lowerLoc loc
                                    triv' = lowerTriv triv

lowerTriv :: Triv -> P.Triv
lowerTriv (Int i) = P.Int i
lowerTriv (Loc loc) = P.Loc loc'
                    where loc' = lowerLoc loc

lowerLoc :: Loc -> P.Loc
lowerLoc (Reg reg) = P.Reg reg'
                   where reg' = lowerReg reg
lowerLoc (Addr i) = P.Addr i

lowerLabel :: Label -> P.Label
lowerLabel (Label n i) = P.Label n i

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
