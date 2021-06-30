module Paren.Compile
( compile
)
where

import Compiler.Types
import Compiler.Values
import Paren.Types

import Data.List

compile :: String -> String -> Program -> String
compile prelude postlude (Program stmts) = intercalate "\n" [ prelude, (intercalate "\n" . map compileStmt) stmts, postlude ]

compileStmt :: Stmt -> String
compileStmt (SetAddr addr val) = unwords [ "mov", compileAddr addr, ",", compileAVal val ]
compileStmt (SetReg reg val) = unwords [ "mov", compileReg reg, ",", compileRVal val ]
compileStmt (BinOp op reg val) = unwords [ compileOp op, compileReg reg, ",", compileOVal val ]
compileStmt (Compare reg val) = unwords [ "cmp", compileReg reg, ",", compileCVal val ]
compileStmt (JumpIf op label) = unwords [ compileRelOp op, compileLabel label ]
compileStmt (Jump place) = unwords [ "jmp", compilePlace place ]
compileStmt (Labelled label stmt) = intercalate "\n" [ compileLabel label ++ ":", compileStmt stmt ]

compileAVal :: AVal -> String
compileAVal (AInt i) = show i
compileAVal (AReg reg) = compileReg reg
compileAVal (ALabel label) = compileLabel label

compileRVal :: RVal -> String
compileRVal (RInt i) = show i
compileRVal (RReg reg) = compileReg reg
compileRVal (RAddr addr) = compileAddr addr
compileRVal (RLabel label) = compileLabel label

compileOVal :: OVal -> String
compileOVal (OInt i) = show i
compileOVal (OReg reg) = compileReg reg
compileOVal (OAddr addr) = compileAddr addr
compileOVal (OLabel label) = compileLabel label

compileCVal :: CVal -> String
compileCVal (CInt i) = show i
compileCVal (CReg reg) = compileReg reg
compileCVal (CAddr addr) = compileAddr addr
compileCVal (CLabel label) = compileLabel label

compilePlace :: Place -> String
compilePlace (PLabel label) = compileLabel label
compilePlace (PReg reg) = compileReg reg
compilePlace (PAddr addr) = compileAddr addr

compileReg :: Reg -> String
compileReg = show

compileAddr :: Addr -> String
compileAddr (Addr i) = unwords [ "[", compileReg frameRegister, "-", show (i * 8), "]" ]

compileLabel :: Label -> String
compileLabel (Label label) = label

compileOp :: Op -> String
compileOp Add = "add"
compileOp Sub = "sub"
compileOp Mul = "imul"

compileRelOp :: RelOp -> String
compileRelOp Lt = "jl"
compileRelOp Gt = "jg"
compileRelOp Eq = "je"
compileRelOp Lte = "jle"
compileRelOp Gte = "jge"
compileRelOp Neq = "jne"
