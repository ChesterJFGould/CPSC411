module Paren.Compile
( compile
)
where

import qualified Compiler.Locations as Locations
import Compiler.Types
import Paren.Types

import Data.List

compile :: String -> String -> Program -> String
compile prelude postlude (Program labels stmts) = intercalate "\n" [ prelude
                                                                   , compileStmts stmts
                                                                   , compileGlobals labels
                                                                   , postlude
                                                                   ]

compileStmts :: [Stmt] -> String
compileStmts = intercalate "\n" . map compileStmt

compileStmt :: Stmt -> String
compileStmt (SetReg reg rVal) = unwords [ "mov", compileReg reg, ",", compileRVal rVal ]
compileStmt (SetAddr addr aVal) = unwords [ "mov", compileAddr addr, ",", compileAVal aVal ]
compileStmt (NumOp op reg oVal) = unwords [ compileNumOp op, compileReg reg, ",", compileOVal oVal ]
compileStmt (Compare reg oVal) = unwords [ "cmp", compileReg reg, ",", compileOVal oVal ]
compileStmt (JumpIf op label) = unwords [ compileRelOp op, compileLabel label ]
compileStmt (Jump rTriv) = unwords [ "jmp", compileRTriv rTriv ]
compileStmt (Labelled label stmt) = intercalate "\n" [ compileLabel label ++ ":"
                                                     , compileStmt stmt
                                                     ]

compileGlobals :: [Label] -> String
compileGlobals labels = intercalate "\n" [ "section .data"
                                         , intercalate "\n" (map compileGlobal labels)
                                         , "section .text"
                                         ]

compileGlobal :: Label -> String
compileGlobal label = unwords [ compileLabel label ++ ":", "dq 0" ]

compileReg :: Reg -> String
compileReg = show

compileAddr :: Addr -> String
compileAddr (Stack offset) = unwords [ "QWORD", "[", compileReg Locations.frameRegister, "-", show (offset * 8), "]" ]
compileAddr (Pointer ptr offset) = unwords [ "QWORD", "[", compileAddrTriv ptr, "+", compileAddrTriv offset, "]" ]

compileAddrTriv :: AddrTriv -> String
compileAddrTriv (AddrLit lit) = compileLit lit
compileAddrTriv (AddrReg reg) = compileReg reg

compileLit :: Lit -> String
compileLit (Lit lit) = show lit

compileLabel :: Label -> String
compileLabel (Label template n) = template ++ "$" ++ show n

compileRVal :: RVal -> String
compileRVal (RWord word) = show word
compileRVal (RReg reg) = compileReg reg
compileRVal (RAddr addr) = compileAddr addr
compileRVal (RVLabel label) = compileLabel label

compileAVal :: AVal -> String
compileAVal (AWord word) = show word
compileAVal (AReg reg) = compileReg reg
compileAVal (AVLabel label) = compileLabel label

compileOVal :: OVal -> String
compileOVal (OWord word) = show word
compileOVal (OReg reg) = compileReg reg
compileOVal (OAddr addr) = compileAddr addr
compileOVal (OVLabel label) = compileLabel label

compileNumOp :: NumOp -> String
compileNumOp Add = "add"
compileNumOp Sub = "sub"
compileNumOp Mul = "imul"

compileRelOp :: RelOp -> String
compileRelOp Lt = "jl"
compileRelOp Gt = "jg"
compileRelOp Eq = "je"
compileRelOp Lte = "jle"
compileRelOp Gte = "gte"
compileRelOp Neq = "jne"

compileRTriv :: RTriv -> String
compileRTriv (RLit lit) = compileLit lit
compileRTriv (RRloc rLoc) = compileRloc rLoc
compileRTriv (RLabel label) = compileLabel label

compileRloc :: Rloc -> String
compileRloc (Reg reg) = compileReg reg
compileRloc (Addr addr) = compileAddr addr
