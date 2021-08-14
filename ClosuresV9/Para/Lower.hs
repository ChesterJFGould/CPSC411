module Para.Lower
( lower
)
where

import qualified Compiler.Locations as Locations
import Compiler.Types
import Para.Types
import qualified Paren.Types as P

import Data.Word

lower :: Program -> P.Program
lower (Program labels stmts) = P.Program labels (lowerStmts stmts)

lowerStmts :: [Stmt] -> [P.Stmt]
lowerStmts = concat . map lowerStmt

lowerStmt :: Stmt -> [P.Stmt]
lowerStmt (Set (Reg reg) triv) = [ P.SetReg reg (trivToRVal triv) ]
lowerStmt (Set (Addr addr) triv) = let (trivAVal, loadTrivStmts) = trivToAVal Locations.tempRegister1 triv
                                   in loadTrivStmts
                                      ++
                                      [ P.SetAddr addr trivAVal ]
lowerStmt (NumOp op (Reg reg) triv) = let (trivOVal, loadTrivStmts) = trivToOVal Locations.tempRegister1 triv
                                      in loadTrivStmts
                                         ++
                                         [ P.NumOp op reg trivOVal ]
lowerStmt (NumOp op (Addr addr) triv) = let (trivOVal, loadTrivStmts) = trivToOVal Locations.tempRegister1 triv
                                        in loadTrivStmts
                                           ++
                                           [ P.SetReg Locations.tempRegister2 (P.RAddr addr)
                                           , P.NumOp op Locations.tempRegister2 trivOVal
                                           , P.SetAddr addr (P.AReg Locations.tempRegister2)
                                           ]
lowerStmt (MRef (Reg reg) ptr offset) = let (ptrAddrTriv, loadPtrStmts) = trivToAddrTriv Locations.tempRegister1 ptr
                                            (offsetAddrTriv, loadOffsetStmts) = trivToAddrTriv Locations.tempRegister2 offset
                                            pointer = Pointer ptrAddrTriv offsetAddrTriv
                                        in loadPtrStmts
                                           ++
                                           loadOffsetStmts
                                           ++
                                           [ P.SetReg reg (P.RAddr pointer) ]
lowerStmt (MRef (Addr addr) ptr offset) = let (ptrAddrTriv, loadPtrStmts) = trivToAddrTriv Locations.tempRegister1 ptr
                                              (offsetAddrTriv, loadOffsetStmts) = trivToAddrTriv Locations.tempRegister2 offset
                                              pointer = Pointer ptrAddrTriv offsetAddrTriv
                                          in loadPtrStmts
                                             ++
                                             loadOffsetStmts
                                             ++
                                             [ P.SetReg Locations.tempRegister3 (P.RAddr pointer)
                                             , P.SetAddr addr (P.AReg Locations.tempRegister3)
                                             ]
lowerStmt (MSet ptr offset val) = let (ptrAddrTriv, loadPtrStmts) = trivToAddrTriv Locations.tempRegister1 ptr
                                      (offsetAddrTriv, loadOffsetStmts) = trivToAddrTriv Locations.tempRegister2 offset
                                      pointer = Pointer ptrAddrTriv offsetAddrTriv
                                      valRVal = trivToRVal val
                                  in loadPtrStmts
                                     ++
                                     loadOffsetStmts
                                     ++
                                     [ P.SetReg Locations.tempRegister3 valRVal
                                     , P.SetAddr pointer (P.AReg Locations.tempRegister3)
                                     ]
lowerStmt (Compare (Reg reg) triv) = let (trivOVal, loadTrivStmts) = trivToOVal Locations.tempRegister1 triv
                                     in loadTrivStmts
                                        ++
                                        [ P.Compare reg trivOVal ]
lowerStmt (Compare (Addr addr) triv) = let (trivOVal, loadTrivStmts) = trivToOVal Locations.tempRegister1 triv
                                       in loadTrivStmts
                                          ++
                                          [ P.SetReg Locations.tempRegister2 (P.RAddr addr)
                                          , P.Compare Locations.tempRegister2 trivOVal
                                          ]
lowerStmt (JumpIf op label) = [ P.JumpIf op label ]
lowerStmt (Jump triv) = [ P.Jump triv ]
lowerStmt (Labelled label stmt) = let (hd : tl) = lowerStmt stmt
                                  in P.Labelled label hd : tl

trivToRVal :: RTriv -> P.RVal
trivToRVal (RLit (Lit lit)) = P.RWord lit
trivToRVal (RRloc (Reg reg)) = P.RReg reg
trivToRVal (RRloc (Addr addr)) = P.RAddr addr
trivToRVal (RLabel label) = P.RVLabel label

trivToAVal :: Reg -> RTriv -> (P.AVal, [P.Stmt])
trivToAVal tmp (RLit (Lit lit))
           | fitsIntoWord32 lit = (P.AWord (fromIntegral lit), [])
           | otherwise = ( P.AReg tmp
                         , [ P.SetReg tmp (P.RWord lit) ]
                         )
trivToAVal tmp (RRloc (Reg reg)) = (P.AReg reg, [])
trivToAVal tmp (RRloc (Addr addr)) = ( P.AReg tmp
                                   , [ P.SetReg tmp (P.RAddr addr) ]
                                   )
trivAVal tmp (RLabel label) = (P.AVLabel label, [])

trivToOVal :: Reg -> RTriv -> (P.OVal, [P.Stmt])
trivToOVal tmp (RLit (Lit lit))
           | fitsIntoWord32 lit = (P.OWord (fromIntegral lit), [])
           | otherwise = ( P.OReg tmp
                         , [ P.SetReg tmp (P.RWord lit) ]
                         )
trivToOVal tmp (RRloc (Reg reg)) = ( P.OReg reg, [] )
trivToOVal tmp (RRloc (Addr addr)) = ( P.OAddr addr, [] )
trivToOVal tmp (RLabel label) = ( P.OVLabel label, [] )

trivToAddrTriv :: Reg -> RTriv -> (AddrTriv, [P.Stmt])
trivToAddrTriv tmp (RLit (Lit lit))
               | fitsIntoWord24 lit = ( AddrLit (Lit lit), [] )
               | otherwise = ( AddrReg tmp
                             , [ P.SetReg tmp (P.RWord lit) ]
                             )
trivToAddrTriv tmp (RRloc (Reg reg)) = ( AddrReg reg, [] )
trivToAddrTriv tmp (RRloc (Addr addr)) = ( AddrReg tmp
                                         , [ P.SetReg tmp (P.RAddr addr) ]
                                         )
trivToAddrTriv tmp (RLabel label) = ( AddrReg tmp
                                    , [ P.SetReg tmp (P.RVLabel label) ]
                                    )

fitsIntoWord32 :: Word64 -> Bool
fitsIntoWord32 word = (word >= (fromIntegral (minBound :: Word32)))
                      &&
                      (word <= (fromIntegral (maxBound :: Word32)))

fitsIntoWord24 :: Word64 -> Bool
fitsIntoWord24 word = (word >= 0)
                      &&
                      (word <= ((2 ^ 24) - 1))
