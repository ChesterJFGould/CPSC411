module Para.Lower
( lower
)
where

import Para.Types

import qualified Paren.Types as P

lower :: Program -> P.Program
lower (Program stmts) = P.Program stmts'
                      where stmts' = (concat . map lowerStmt) stmts

lowerStmt :: Stmt -> [P.Stmt]
lowerStmt (Halt triv) = lowerStmt (Set (Reg RAX) triv) ++ [ P.Halt ]
lowerStmt (Jump label) = [ P.Jump label' ]
                       where label' = lowerLabel label
lowerStmt (Labelled label stmt) = P.Labelled label' fst : rest
                                where fst : rest = lowerStmt stmt
                                      label' = lowerLabel label
lowerStmt (Compare (Reg reg) (Int i)) = [ P.Compare reg' i' ]
                                      where reg' = lowerReg reg
                                            i' = P.CInt i
lowerStmt (Compare (Reg a) (Loc (Reg b))) = [ P.Compare a' b' ]
                                          where a' = lowerReg a
                                                b' = P.CReg $ lowerReg b
lowerStmt (Compare (Reg reg) (Loc (Addr addr))) = [ P.SetReg P.R10 addr'
                                                  , P.Compare reg' (P.CReg P.R10) ]
                                                where addr' = P.RAddr $ P.Addr addr
                                                      reg' = lowerReg reg
lowerStmt (Compare (Addr addr) (Int i)) = [ P.SetReg P.R10 addr'
                                          , P.Compare P.R10 (P.CInt i) ]
                                        where addr' = P.RAddr $ P.Addr addr
lowerStmt (Compare (Addr addr) (Loc (Reg reg))) = [ P.SetReg P.R10 addr'
                                                  , P.Compare P.R10 reg' ]
                                                where addr' = P.RAddr $ P.Addr addr
                                                      reg' = P.CReg $ lowerReg reg
lowerStmt (Compare (Addr a) (Loc (Addr b))) = [ P.SetReg P.R10 a'
                                              , P.SetReg P.R11 b'
                                              , P.Compare P.R10 (P.CReg P.R11) ]
                                            where a' = P.RAddr $ P.Addr a
                                                  b' = P.RAddr $ P.Addr b
lowerStmt (JumpIf op label) = [ P.JumpIf op' label' ]
                            where op' = lowerRelOp op
                                  label' = lowerLabel label
lowerStmt (Set (Reg reg) (Int i)) = [ P.SetReg reg' i' ]
                                  where reg' = lowerReg reg
                                        i' = P.RInt i
lowerStmt (Set (Reg a) (Loc (Reg b))) = [ P.SetReg a' b' ]
                                      where a' = lowerReg a
                                            b' = P.RReg $ lowerReg b
lowerStmt (Set (Reg reg) (Loc (Addr addr))) = [ P.SetReg reg' addr' ]
                                            where reg' = lowerReg reg
                                                  addr' = P.RAddr $ P.Addr addr
lowerStmt (Set (Addr addr) (Int i)) = [ P.SetAddr addr' i' ]
                                    where addr' = P.Addr addr
                                          i' = P.AInt $ fromIntegral i
lowerStmt (Set (Addr addr) (Loc (Reg reg))) = [ P.SetAddr addr' reg' ]
                                            where addr' = P.Addr addr
                                                  reg' = P.AReg $ lowerReg reg
lowerStmt (Set (Addr a) (Loc (Addr b))) = [ P.SetReg P.R10 b'
                                          , P.SetAddr a' (P.AReg P.R10) ]
                                        where b' = P.RAddr $ P.Addr b
                                              a' = P.Addr a
lowerStmt (BinOp op (Reg reg) (Int i)) = [ P.BinOp op' reg' i' ]
                                       where op' = lowerOp op
                                             reg' = lowerReg reg
                                             i' = P.OInt $ fromIntegral i
lowerStmt (BinOp op (Reg a) (Loc (Reg b))) = [ P.BinOp op' a' b' ]
                                           where op' = lowerOp op
                                                 a' = lowerReg a
                                                 b' = P.OReg $ lowerReg b
lowerStmt (BinOp op (Reg reg) (Loc (Addr addr))) = [ P.BinOp op' reg' addr' ]
                                                 where op' = lowerOp op
                                                       reg' = lowerReg reg
                                                       addr' = P.OAddr $ P.Addr addr
lowerStmt (BinOp op (Addr addr) (Int i)) = [ P.SetReg P.R10 (P.RAddr addr')
                                           , P.BinOp op' P.R10 i'
                                           , P.SetAddr addr' (P.AReg P.R10) ]
                                         where addr' = P.Addr addr
                                               op' = lowerOp op
                                               i' = P.OInt $ fromIntegral i
lowerStmt (BinOp op (Addr addr) (Loc (Reg reg))) = [ P.SetReg P.R10 (P.RAddr addr')
                                                   , P.BinOp op' P.R10 reg'
                                                   , P.SetAddr addr' (P.AReg P.R10) ]
                                                 where addr' = P.Addr addr
                                                       op' = lowerOp op
                                                       reg' = P.OReg $ lowerReg reg
lowerStmt (BinOp op (Addr a) (Loc (Addr b))) = [ P.SetReg P.R10 (P.RAddr a')
                                               , P.BinOp op' P.R10 b'
                                               , P.SetAddr a' (P.AReg P.R10) ]
                                             where a' = P.Addr a
                                                   op' = lowerOp op
                                                   b' = P.OAddr $ P.Addr b

lowerLabel :: Label -> P.Label
lowerLabel (Label n i) = P.Label n i

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
