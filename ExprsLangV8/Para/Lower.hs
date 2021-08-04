module Para.Lower
( lower
)
where

import Compiler.Locs
import Compiler.Types
import Para.Types
import qualified Paren.Types as P

lower :: Program -> P.Program
lower (Program stmts) = P.Program (lowerStmts stmts)

lowerStmts :: [Stmt] -> [P.Stmt]
lowerStmts = concat . map lowerStmt

lowerStmt :: Stmt -> [P.Stmt]
lowerStmt (Set (Reg reg) triv) = [ P.SetReg reg (asRVal triv) ]
lowerStmt (Set (Addr addr) triv) = concat [ trivStmts
                                          , [P.SetAddr addr triv'] ]
                                 where (triv', trivStmts) = asAVal tempRegister1 triv
lowerStmt (BinOp op (Reg reg) triv) = concat [ trivStmts
                                                     , [P.BinOp op reg triv'] ]
                                             where (triv', trivStmts) = asOVal tempRegister1 triv
lowerStmt (BinOp op (Addr addr) triv) = concat [ trivStmts
                                                       , [ P.SetReg tempRegister2 (P.RAddr addr)
                                                         , P.BinOp op tempRegister2 triv'
                                                         , P.SetAddr addr (P.AReg tempRegister2) ] ]
                                              where (triv', trivStmts) = asOVal tempRegister1 triv
lowerStmt (MSet ptr offset val) = concat [ ptrStmts
                                         , offsetStmts
                                         , valStmts
                                         , [ P.SetAddr (Pointer ptr' offset') val' ] ]
                                  where (ptr', ptrStmts) = asAddrTriv tempRegister1 ptr
                                        (offset', offsetStmts) = asAddrTriv tempRegister2 offset
                                        (val', valStmts) = asAVal tempRegister3 val
lowerStmt (MRef (Reg reg) ptr offset) = concat [ ptrStmts
                                               , offsetStmts
                                               , [P.SetReg reg (P.RAddr (Pointer ptr' offset'))] ]
                                      where (ptr', ptrStmts) = asAddrTriv tempRegister1 ptr
                                            (offset', offsetStmts) = asAddrTriv tempRegister2 offset
lowerStmt (MRef (Addr addr) ptr offset) = concat [ ptrStmts
                                                 , offsetStmts
                                                 , [ P.SetReg tempRegister3 (P.RAddr (Pointer ptr' offset'))
                                                   , P.SetAddr addr (P.AReg tempRegister3) ] ]
                                        where (ptr', ptrStmts) = asAddrTriv tempRegister1 ptr
                                              (offset', offsetStmts) = asAddrTriv tempRegister2 offset
lowerStmt (Compare loc triv) = concat [ locStmts
                                      , trivStmts
                                      , [P.Compare loc' triv'] ]
                             where (loc', locStmts) = asReg tempRegister1 loc
                                   (triv', trivStmts) = asOVal tempRegister2 triv
lowerStmt (JumpIf op label) = [ P.JumpIf op label ]
lowerStmt (Jump place) = [ P.Jump place ]
lowerStmt (Labelled label stmt) = P.Labelled label hd : tl
                                where (hd : tl) = lowerStmt stmt

asAddrTriv :: Reg -> RTriv -> (AddrTriv, [P.Stmt])
asAddrTriv tmp (RRloc (Reg ptr)) = (AddrReg ptr, [])
asAddrTriv tmp (RRloc (Addr ptr)) = ( AddrReg tmp
                                    , [ P.SetReg tmp (P.RAddr ptr) ] )
asAddrTriv tmp (RLit lit) = ( AddrReg tmp
                            , [ P.SetReg tmp (P.RLit lit) ] )
-- DUCKTAPE
-- This is so we can take the address of the addition
-- of two labels which NASM doesn't support.
asAddrTriv tmp (RLabel label) = ( AddrReg tmp
                                , [ P.SetReg tmp (P.RLabel label) ] )

asReg :: Reg -> Rloc -> (Reg, [P.Stmt])
asReg tmp (Reg reg) = (reg, [])
asReg tmp (Addr addr) = (tmp, [ P.SetReg tmp (P.RAddr addr) ])

asRVal :: RTriv -> P.RVal
asRVal (RRloc (Reg reg)) = P.RReg reg
asRVal (RRloc (Addr addr)) = P.RAddr addr
asRVal (RLit lit) = P.RLit lit
asRVal (RLabel label) = P.RLabel label

asAVal :: Reg -> RTriv -> (P.AVal, [P.Stmt])
asAVal tmp (RRloc (Reg reg)) = (P.AReg reg, [])
asAVal tmp triv = (P.AReg tmp, [ P.SetReg tmp (asRVal triv) ])

asOVal :: Reg -> RTriv -> (P.OVal, [P.Stmt])
asOVal tmp (RRloc (Reg reg)) = (P.OReg reg, [])
asOVal tmp (RRloc (Addr addr)) = (P.OAddr addr, [])
asOVal tmp (RLit lit) = (P.OReg tmp, [ P.SetReg tmp (P.RLit lit) ])
asOVal tmp (RLabel label) = (P.OLabel label, [])
