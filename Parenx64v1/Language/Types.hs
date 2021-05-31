module Language.Types where

import Data.List
import Data.Int

data Stmt = Set Reg Triv
          | Add Reg Triv
          | Mult Reg Triv

instance Show Stmt where
         show (Set reg triv) = unwords [show reg, ":=", show triv]
         show (Add reg triv) = unwords [show reg, "+=", show triv]
         show (Mult reg triv) = unwords [show reg, "*=", show triv]

data Triv = Int32 Int32
          | Int64 Int64
          | Reg Reg

instance Show Triv where
         show (Int32 i) = show i
         show (Int64 i) = show i
         show (Reg reg) = show reg

data Reg = RSP
         | RBP
         | RAX
         | RBX
         | RCX
         | RDX
         | RSI
         | R8
         | R9
         | R10
         | R11
         | R12
         | R13
         | R14
         | R15
         deriving (Eq, Ord, Show)

newtype Program = Program [Stmt]

instance Show Program where
         show (Program stmts) = (intercalate "\n" . map show) stmts

instance Semigroup Program where
         (Program a) <> (Program b) = Program (a ++ b)
