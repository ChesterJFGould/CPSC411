module Paren.Compile
( compile
)
where

import Paren.Types

compile :: String -> String -> Program -> String
compile prelude postlude (Program stmts) = prelude ++ (unlines $ map compileStmt stmts) ++ postlude

compileStmt :: Stmt -> String
compileStmt (SetAddr addr val) = unwords ["mov", compileAddr addr, ",", compileAVal val]
compileStmt (SetReg reg val) = unwords ["mov", compileReg reg, ",", compileRVal val]
compileStmt (BinOp op reg val) = unwords [compileOp op, compileReg reg, ",", compileOVal val]

compileAddr :: Addr -> String
compileAddr (Addr offset) = unwords ["QWORD", "[", compileReg RBP, "-", show (offset * 8), "]"]

compileAVal :: AVal -> String
compileAVal (AInt i) = show i
compileAVal (AReg reg) = compileReg reg

compileRVal :: RVal -> String
compileRVal (RInt i) = show i
compileRVal (RReg reg) = compileReg reg
compileRVal (RAddr addr) = compileAddr addr

compileOVal :: OVal -> String
compileOVal (OInt i) = show i
compileOVal (OReg reg) = compileReg reg
compileOVal (OAddr addr) = compileAddr addr

compileReg :: Reg -> String
compileReg = show

compileOp :: Op -> String
compileOp Add = "add"
compileOp Mul = "imul"
