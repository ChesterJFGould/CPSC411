module Para.Types where

import Data.Int

data Program = Program [Stmt] Triv
             deriving Show

data Stmt = Stmt Op Loc Triv
          deriving Show

data Op = Set
        | Add
        | Mul
        deriving Show

data Triv = Int64 Int64
          | Loc Loc
          deriving Show

data Loc = Reg Reg
         | Addr Int -- QWORD [ RBP - Int ] | fv<Int>
         deriving Show

data Reg = RSP
         | RBP
         | RAX
         | RBX
         | RCX
         | RDX
         | RSI
         | R8
         | R9
         | R12
         | R13
         | R14
         | R15
         deriving Show
