module Paren.Lower
( lower
)
where

import Paren.Types

import qualified Para as P

lower :: P.Program -> Program
lower (P.Program stmts out) = Program $ lowerStmts stmts
                                        ++ lowerStmt (P.Stmt P.Set (P.Reg P.RAX) out)

lowerStmts :: [P.Stmt] -> [Stmt]
lowerStmts = concat . map lowerStmt

lowerStmt :: P.Stmt -> [Stmt]
lowerStmt (P.Stmt P.Set (P.Addr addr) (P.Int64 i)) = [SetAddr (Addr addr) (AInt32 . fromIntegral $ i)]
lowerStmt (P.Stmt P.Set (P.Addr addr) (P.Loc (P.Reg reg))) = [SetAddr (Addr addr) (AReg . lowerReg $ reg)]
lowerStmt (P.Stmt P.Set (P.Addr addr) (P.Loc (P.Addr addr'))) = [ SetReg R10 (RAddr . Addr $ addr')
                                                                , SetAddr (Addr addr) (AReg R10) ]

lowerStmt (P.Stmt P.Set (P.Reg reg) (P.Int64 i)) = [SetReg (lowerReg reg) (RInt64 i)]
lowerStmt (P.Stmt P.Set (P.Reg reg) (P.Loc (P.Reg reg'))) = [SetReg (lowerReg reg) (RReg . lowerReg $ reg')]
lowerStmt (P.Stmt P.Set (P.Reg reg) (P.Loc (P.Addr addr))) = [SetReg (lowerReg reg) (RAddr . Addr $ addr)]

lowerStmt (P.Stmt op (P.Addr addr) (P.Int64 i)) = [ SetReg R10 (RAddr . Addr $ addr)
                                                  , BinOp (lowerOp op) R10 (OInt32 . fromIntegral $ i)
                                                  , SetAddr (Addr addr) (AReg R10) ]

lowerStmt (P.Stmt op (P.Addr addr) (P.Loc (P.Reg reg))) = [ SetReg R10 (RAddr . Addr $ addr)
                                                          , BinOp (lowerOp op) R10 (OReg . lowerReg $ reg)
                                                          , SetAddr (Addr addr) (AReg R10) ]

lowerStmt (P.Stmt op (P.Addr addr) (P.Loc (P.Addr addr'))) = [ SetReg R10 (RAddr . Addr $ addr)
                                                             , SetReg R11 (RAddr . Addr $ addr')
                                                             , BinOp (lowerOp op) R10 (OReg R11)
                                                             , SetAddr (Addr addr) (AReg R10) ]

lowerStmt (P.Stmt op (P.Reg reg) (P.Int64 i)) = [BinOp (lowerOp op) (lowerReg reg) (OInt32 . fromIntegral $ i)]

lowerStmt (P.Stmt op (P.Reg reg) (P.Loc (P.Reg reg'))) = [BinOp (lowerOp op) (lowerReg reg) (OReg . lowerReg $ reg')]

lowerStmt (P.Stmt op (P.Reg reg) (P.Loc (P.Addr addr))) = [ SetReg R10 (RAddr . Addr $ addr)
                                                          , BinOp (lowerOp op) (lowerReg reg) (OReg R10) ]

lowerOp :: P.Op -> Op
lowerOp P.Add = Add
lowerOp P.Mul = Mul

lowerReg :: P.Reg -> Reg
lowerReg P.RSP = RSP
lowerReg P.RBP = RBP
lowerReg P.RAX = RAX
lowerReg P.RBX = RBX
lowerReg P.RCX = RCX
lowerReg P.RDX = RDX
lowerReg P.RSI = RSI
lowerReg P.R8 = R8
lowerReg P.R9 = R9
lowerReg P.R12 = R12
lowerReg P.R13 = R13
lowerReg P.R14 = R14
lowerReg P.R15 = R15
