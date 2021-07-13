module Paren.Compile
( compile
)
where

import qualified Compiler.Locs as Locs
import Compiler.Types as C
import Paren.Types as P

import Data.List

compile :: String -> String -> Program -> String
compile prelude postlude (Program stmts) = unlines' [prelude, compileStmts stmts, postlude]

compileStmts :: [Stmt] -> String
compileStmts = unlines' . map compileStmt

compileStmt :: Stmt -> String
compileStmt (SetReg reg val) = unwords [ "mov", compileReg reg, ",", compileRVal val ]
compileStmt (SetAddr addr val) = unwords [ "mov", compileAddr addr, ",", compileAVal val ]
compileStmt (BinOp op reg val) = unwords [ compileBinOp op, compileReg reg, ",", compileOVal val ]
compileStmt (Compare reg val) = unwords [ "cmp", compileReg reg, ",", compileOVal val ]
compileStmt (JumpIf op label) = unwords [ compileRelOp op, compileLabel label ]
compileStmt (Jump place) = unwords [ "jmp", compileRPlace place ]
compileStmt (Labelled label stmt) = unlines' [ compileLabel label ++ ":", compileStmt stmt ]

compileRVal :: RVal -> String
compileRVal (P.RPtr ptr) = show ptr
compileRVal (RReg reg) = compileReg reg
compileRVal (RAddr addr) = compileAddr addr
compileRVal (P.RLabel label) = compileLabel label

compileAVal :: AVal -> String
compileAVal (P.APtr ptr) = show ptr
compileAVal (AReg reg) = compileReg reg
compileAVal (ALabel label) = compileLabel label

compileOVal :: OVal -> String
compileOVal (OPtr ptr) = show ptr
compileOVal (OReg reg) = compileReg reg
compileOVal (OAddr addr) = compileAddr addr
compileOVal (OLabel label) = compileLabel label

compileBinOp :: BinOp -> String
compileBinOp Add = "add"
compileBinOp Sub = "sub"
compileBinOp Mul = "imul"
compileBinOp And = "and"
compileBinOp Ior = "or"
compileBinOp Xor = "xor"
compileBinOp Shr = "shr"

compileRelOp :: RelOp -> String
compileRelOp Lt = "jl"
compileRelOp Gt = "jg"
compileRelOp Eq = "je"
compileRelOp Lte = "jle"
compileRelOp Gte = "jge"
compileRelOp Neq = "jne"

compileReg :: Reg -> String
compileReg = show

compileAddr :: Addr -> String
compileAddr (Addr i) = unwords [ "QWORD", "[", compileReg Locs.frame, "-", show (i * 8), "]" ]

compileLabel :: Label -> String
compileLabel (Label template i) = template ++ "$" ++ show i

compileRPlace :: RPlace -> String
compileRPlace (PRloc loc) = compileRloc loc
compileRPlace (C.RLabel label) = compileLabel label

compileRloc :: Rloc -> String
compileRloc (Reg reg) = compileReg reg
compileRloc (LAddr addr) = compileAddr addr

unlines' :: [String] -> String
unlines' = intercalate "\n"
