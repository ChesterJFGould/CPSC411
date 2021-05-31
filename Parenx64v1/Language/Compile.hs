module Language.Compile
( compile
)
where

import Language.Types

import Data.Char
import Data.List

prelude = unlines [ "global start"
                  , ""
                  , "section .text"
                  , ""
                  , "start:" ]

postlude = unlines [ ""
                   , "print:"
                   , "mov r8, msg"
                   , "mov [r8], rax"
                   , "mov rax, 1"
                   , "mov rdi, 1"
                   , "mov rsi, msg"
                   , "mov rdx, len"
                   , "syscall"
                   , "mov r9, 0x0a"
                   , "mov [r8], r9"
                   , "syscall"
                   , "exit:"
                   , "mov rax, 60"
                   , "mov rdi, 0"
                   , "syscall"
                   , ""
                   , "section .data"
                   , ""
                   , "dummy: db 0"
                   , "len: equ 1"
                   , "msg: times len dw 0" ]

compile :: Program -> String
compile prog = prelude ++ compile' prog ++ postlude

compile' :: Program -> String
compile' (Program prog) = (intercalate "\n" . map compileStmt) prog

compileStmt :: Stmt -> String
compileStmt (Set reg triv) = unwords ["mov", compileReg reg, ",", compileTriv triv]
compileStmt (Add reg triv) = unwords ["add", compileReg reg, ",", compileTriv triv]
compileStmt (Mult reg triv) = unwords ["imul", compileReg reg, ",", compileTriv triv]

compileTriv :: Triv -> String
compileTriv (Int i) = show i
compileTriv (Reg reg) = compileReg reg

compileReg :: Reg -> String
compileReg = (map toLower . show)
