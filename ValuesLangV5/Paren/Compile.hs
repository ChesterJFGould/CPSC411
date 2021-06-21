module Paren.Compile
( compile
)
where

import Paren.Types

import Data.List

compile :: String -> String -> Program -> String
compile prelude postlude (Program stmts) = intercalate "\n" ( [ prelude ]
                                                            ++ map compileStmt stmts
                                                            ++ [ postlude ] )

compileStmt :: Stmt -> String
compileStmt (SetAddr addr val) = unwords [ "mov", compileAddr addr, ",", compileAVal val ]
compileStmt (SetReg reg val) = unwords [ "mov", compileReg reg, ",", compileRVal val ]
compileStmt (BinOp op reg val) = unwords [ compileOp op
                                         , compileReg reg
                                         , ","
                                         , compileOVal val ]
compileStmt (Compare reg val) = unwords [ "cmp", compileReg reg, ",", compileCVal val ]
compileStmt (JumpIf op label) = unwords [ compileRelOp op, compileLabel label ]
compileStmt (Jump label) = unwords [ "jmp", compileLabel label ]
compileStmt (Labelled label stmt) = intercalate "\n" [ compileLabel label ++ ":"
                                                     , compileStmt stmt ]
compileStmt (Halt val) = intercalate "\n" [ compileStmt (SetReg RAX val)
                                          , compileStmt (Jump (Label "done")) ]

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

compileCVal :: CVal -> String
compileCVal (CInt i) = show i
compileCVal (CReg reg) = compileReg reg

compileLabel :: Label -> String
compileLabel (Label label) = label

compileAddr :: Addr -> String
compileAddr (Addr addr) = unwords [ "QWORD"
                                  , "["
                                  , compileReg RBP
                                  , "-"
                                  , show (addr * 8)
                                  , "]" ]

compileReg :: Reg -> String
compileReg = show

compileOp :: Op -> String
compileOp Add = "add"
compileOp Mul = "imul"

compileRelOp :: RelOp -> String
compileRelOp Lt = "jl"
compileRelOp Gt = "jg"
compileRelOp Eq = "je"
compileRelOp Lte = "jle"
compileRelOp Gte = "jge"
compileRelOp Neq = "jne"
