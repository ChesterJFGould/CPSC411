module Para.Lower
( lower
)
where

import Para.Types

import qualified Paren.Types as P

lower :: Program -> P.Program
lower (Program stmts out) = P.Program $ concat (map lowerStmt stmts
                                                ++ [lowerStmt (Stmt Set (Reg RAX) out)])

lowerStmt :: Stmt -> [P.Stmt]
lowerStmt (Stmt Set (Addr addr) (Int i)) = [P.SetAddr (P.Addr addr) (P.AInt . fromIntegral $ i)]
lowerStmt (Stmt Set (Addr addr) (Loc (Reg reg))) = [P.SetAddr (P.Addr addr) (P.AReg . lowerReg $ reg)]
lowerStmt (Stmt Set (Addr addr) (Loc (Addr addr'))) = [ P.SetReg P.R10 (P.RAddr . P.Addr $ addr')
                                                      , P.SetAddr (P.Addr addr) (P.AReg P.R10) ]
lowerStmt (Stmt Set (Reg reg) (Int i)) = [P.SetReg (lowerReg reg) (P.RInt i)]
lowerStmt (Stmt Set (Reg reg) (Loc (Reg reg'))) = [P.SetReg (lowerReg reg) (P.RReg . lowerReg $ reg')]
lowerStmt (Stmt Set (Reg reg) (Loc (Addr addr))) = [P.SetReg (lowerReg reg) (P.RAddr . P.Addr $ addr)]
lowerStmt (Stmt op (Addr addr) (Int i)) = [ P.SetReg P.R10 (P.RAddr . P.Addr $ addr)
                                          , P.BinOp (lowerOp op) P.R10 (P.OInt . fromIntegral $ i)
                                          , P.SetAddr (P.Addr addr) (P.AReg P.R10) ]
lowerStmt (Stmt op (Addr addr) (Loc (Reg reg))) = [P.SetAddr (P.Addr addr) (P.AReg .lowerReg $ reg)]
lowerStmt (Stmt op (Addr addr) (Loc (Addr addr'))) = [ P.SetReg P.R10 (P.RAddr . P.Addr $ addr)
                                                     , P.BinOp (lowerOp op) P.R10 (P.OAddr . P.Addr $ addr')
                                                     , P.SetAddr (P.Addr addr) (P.AReg P.R10) ]
lowerStmt (Stmt op (Reg reg) (Int i)) = [P.BinOp (lowerOp op) (lowerReg reg) (P.OInt . fromIntegral $ i)]
lowerStmt (Stmt op (Reg reg) (Loc (Reg reg'))) = [P.BinOp (lowerOp op) (lowerReg reg) (P.OReg . lowerReg $ reg')]
lowerStmt (Stmt op (Reg reg) (Loc (Addr addr))) = [P.BinOp (lowerOp op) (lowerReg reg) (P.OAddr . P.Addr $ addr)]

lowerOp :: Op -> P.Op
lowerOp Add = P.Add
lowerOp Mul = P.Mul

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
