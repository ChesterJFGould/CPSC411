module Paren.Compile
( compile
)
where

import Paren.Types

compile :: String -> String -> Program -> String
compile prelude postlude (Program stmts) = unlines ([prelude] ++ map compile' stmts ++ [postlude])

compile' :: Stmt -> String
compile' (SetAddr addr val) = unwords ["mov", compileAddr addr, ",", compileAVal val]
compile' (SetReg reg val) = unwords ["mov", compileReg reg, ",", compileRVal val]
compile' (BinOp op reg val) = unwords [compileOp op, compileReg reg, ",", compileOVal val]

compileAddr :: Addr -> String
compileAddr (Addr addr) = unwords ["QWORD", "[", "rbp", "-", show (addr * 8), "]"]

compileAVal :: AVal -> String
compileAVal (AReg reg) = compileReg reg
compileAVal (AInt32 i) = show i

compileReg :: Reg -> String
compileReg = show

compileRVal :: RVal -> String
compileRVal (RReg reg) = compileReg reg
compileRVal (RAddr addr) = compileAddr addr
compileRVal (RInt64 i) = show i

compileOp :: Op -> String
compileOp Add = "add"
compileOp Mul = "imul"

compileOVal :: OVal -> String
compileOVal (OReg reg) = compileReg reg
compileOVal (OAddr addr) = compileAddr addr
compileOVal (OInt32 i) = show i
