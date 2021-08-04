module Paren.Compile
( compile
)
where

import Compiler.Locs
import Compiler.Types as C
import Paren.Types as P

import Data.List

compile :: String -> String -> Program -> String
compile prelude postlude (Program stmts) = intercalate "\n" [ prelude
                                                            , compileStmts stmts
                                                            , postlude ]

compileStmts :: [Stmt] -> String
compileStmts = intercalate "\n" . map compileStmt

compileStmt :: Stmt -> String
compileStmt (SetReg reg val) = unwords [ "mov", compileReg reg, ",", compileRVal val ]
compileStmt (SetAddr addr val) = unwords [ "mov", compileAddr addr, ",", compileAVal val ]
compileStmt (BinOp Shr reg val) = unwords [ "shrx", compileReg reg, ",", compileReg reg, ",", compileOVal val ]
compileStmt (BinOp op reg val) = unwords [ compileBinOp op, compileReg reg, ",", compileOVal val ]
compileStmt (Compare reg val) = unwords [ "cmp", compileReg reg, ",", compileOVal val ]
compileStmt (JumpIf op label) = unwords [ compileRelOp op, compileLabel label ]
compileStmt (Jump place) = unwords [ "jmp", compilePlace place ]
compileStmt (Labelled label stmt) = intercalate "\n" [ compileLabel label ++ ":"
                                                     , compileStmt stmt ]

compileRVal :: RVal -> String
compileRVal (P.RLit lit) = compileLit lit
compileRVal (RReg reg) = compileReg reg
compileRVal (RAddr addr) = compileAddr addr
compileRVal (P.RLabel label) = compileLabel label

compileAVal :: AVal -> String
compileAVal (AReg reg) = compileReg reg
compileAVal (ALabel label) = compileLabel label

compileOVal :: OVal -> String
compileOVal (OReg reg) = compileReg reg
compileOVal (OAddr addr) = compileAddr addr
compileOVal (OLabel label) = compileLabel label

compilePlace :: RPlace -> String
compilePlace (PRloc rloc) = compileRloc rloc
compilePlace (PLabel label) = compileLabel label

compileRloc :: Rloc -> String
compileRloc (Reg reg) = compileReg reg
compileRloc (Addr addr) = compileAddr addr

compileLabel :: Label -> String
compileLabel (Label template n) = template ++ "$" ++ show n

compileLit :: Lit -> String
compileLit (Lit i) = show i

compileReg :: Reg -> String
compileReg = show

compileBinOp :: BinOp -> String
compileBinOp Add = "add"
compileBinOp Sub = "sub"
compileBinOp Mul = "imul"
compileBinOp And = "and"
compileBinOp Ior = "or"
compileBinOp Xor = "xor"
compileBinOp Shr = "shrx"

compileRelOp :: RelOp -> String
compileRelOp Lt = "jl"
compileRelOp Gt = "jg"
compileRelOp Eq = "je"
compileRelOp Lte = "jle"
compileRelOp Gte = "jge"
compileRelOp Neq = "jne"

compileAddr :: Addr -> String
compileAddr (Stack n) = unwords [ "[", compileReg frameRegister, "-", show (n * 8), "]" ]
compileAddr (Pointer ptr offset) = unwords [ "[", compileAddrTriv ptr, "+", compileAddrTriv offset, "]" ]

compileAddrTriv :: AddrTriv -> String
compileAddrTriv (AddrReg reg) = compileReg reg
compileAddrTriv (AddrLit lit) = compileLit lit
