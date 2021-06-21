module Para.Lower
( lower
)
where

import Para.Types

import qualified Paren.Types as P

lower :: Program -> P.Program
lower (Program stmts) = P.Program ((concat . map lowerStmt) stmts)

lowerStmt :: Stmt -> [P.Stmt]
lowerStmt (Set (Reg reg) (Int i)) = [ P.SetReg (lowerReg reg) (P.RInt (fromIntegral i)) ]
lowerStmt (Set (Reg a) (Loc (Reg b))) = [ P.SetReg (lowerReg a) (P.RReg (lowerReg b)) ]
lowerStmt (Set (Reg reg) (Loc (Addr addr))) = [ P.SetReg (lowerReg reg)
                                                         (P.RAddr (P.Addr addr)) ]
lowerStmt (Set (Addr a) (Int i)) = [ P.SetAddr (P.Addr a) (P.AInt (fromIntegral i)) ]
lowerStmt (Set (Addr a) (Loc (Reg reg))) = [ P.SetAddr (P.Addr a) (P.AReg (lowerReg reg)) ]
lowerStmt (Set (Addr a) (Loc (Addr b))) = [ P.SetReg P.R10 (P.RAddr (P.Addr b))
                                          , P.SetAddr (P.Addr a) (P.AReg P.R10) ]
lowerStmt (BinOp op (Reg reg) (Int i)) = [ P.BinOp (lowerOp op)
                                                   (lowerReg reg)
                                                   (P.OInt (fromIntegral i)) ]
lowerStmt (BinOp op (Reg a) (Loc (Reg b))) = [ P.BinOp (lowerOp op)
                                                       (lowerReg a)
                                                       (P.OReg (lowerReg b)) ]
lowerStmt (BinOp op (Reg reg) (Loc (Addr addr))) = [ P.BinOp (lowerOp op)
                                                             (lowerReg reg)
                                                             (P.OAddr (P.Addr addr)) ]
lowerStmt (BinOp op (Addr addr) (Int i)) = [ P.SetReg P.R10 (P.RAddr (P.Addr addr))
                                           , P.BinOp (lowerOp op)
                                                     P.R10
                                                     (P.OInt (fromIntegral i))
                                           , P.SetAddr (P.Addr addr) (P.AReg P.R10) ]
lowerStmt (BinOp op (Addr addr) (Loc (Reg reg))) = [ P.SetReg P.R10 (P.RAddr (P.Addr addr))
                                                   , P.BinOp (lowerOp op)
                                                             P.R10
                                                             (P.OReg (lowerReg reg))
                                                   , P.SetAddr (P.Addr addr)
                                                               (P.AReg P.R10) ]
lowerStmt (BinOp op (Addr a) (Loc (Addr b))) = [ P.SetReg P.R10 (P.RAddr (P.Addr a))
                                               , P.BinOp (lowerOp op)
                                                         P.R10
                                                         (P.OAddr (P.Addr b))
                                               , P.SetAddr (P.Addr a) (P.AReg P.R10) ]
lowerStmt (Compare (Reg reg) (Int i)) = [ P.Compare (lowerReg reg)
                                                    (P.CInt (fromIntegral i)) ]
lowerStmt (Compare (Reg a) (Loc (Reg b))) = [ P.Compare (lowerReg a)
                                                        (P.CReg (lowerReg b)) ]
lowerStmt (Compare (Reg reg) (Loc (Addr addr))) = [ P.SetReg P.R10 (P.RAddr (P.Addr addr))
                                                  , P.Compare (lowerReg reg)
                                                              (P.CReg P.R10) ]
lowerStmt (Compare (Addr addr) (Int i)) = [ P.SetReg P.R10 (P.RAddr (P.Addr addr))
                                          , P.Compare P.R10 (P.CInt (fromIntegral i)) ]
lowerStmt (Compare (Addr addr) (Loc (Reg reg))) = [ P.SetReg P.R10 (P.RAddr (P.Addr addr))
                                                  , P.Compare (lowerReg reg)
                                                             (P.CReg P.R10) ]
lowerStmt (Compare (Addr a) (Loc (Addr b))) = [ P.SetReg P.R10 (P.RAddr (P.Addr a))
                                              , P.SetReg P.R11 (P.RAddr (P.Addr b))
                                              , P.Compare P.R10 (P.CReg P.R11) ]
lowerStmt (JumpIf op label) = [ P.JumpIf (lowerRelOp op) (lowerLabel label) ]
lowerStmt (Jump label) = [ P.Jump (lowerLabel label) ]
lowerStmt (Labelled label stmt) = P.Labelled (lowerLabel label) hd : tl
                                where (hd : tl) = lowerStmt stmt
lowerStmt (Halt (Int i)) = [ P.Halt (P.RInt (fromIntegral i)) ]
lowerStmt (Halt (Loc (Reg reg))) = [ P.Halt (P.RReg (lowerReg reg)) ]
lowerStmt (Halt (Loc (Addr addr))) = [ P.Halt (P.RAddr (P.Addr addr)) ]

lowerLabel :: Label -> P.Label
lowerLabel (Label label) = P.Label label

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
