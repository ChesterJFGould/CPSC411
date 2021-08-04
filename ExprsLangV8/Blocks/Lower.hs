module Blocks.Lower
( lower
)
where

import Blocks.Types
import Compiler.Types
import qualified Para.Types as P

lower :: Program -> P.Program
lower (Program blocks body) = P.Program (lowerBody body ++ lowerBlocks blocks)

lowerBlocks :: [Block] -> [P.Stmt]
lowerBlocks = concat . map lowerBlock

lowerBlock :: Block -> [P.Stmt]
lowerBlock (Block label body) = P.Labelled label hd : tl
                              where (hd : tl) = lowerBody body

lowerBody :: Body -> [P.Stmt]
lowerBody (Body tail) = lowerTail tail

lowerTail :: Tail -> [P.Stmt]
lowerTail (Seq stmts tail) = lowerStmts stmts ++ lowerTail tail
lowerTail (If (RelOp op loc triv) cLabel aLabel) = [ P.Compare loc triv
                                                   , P.JumpIf op cLabel
                                                   , P.Jump (PLabel aLabel) ]
lowerTail (Jump place) = [ P.Jump place ]

lowerStmts :: [Stmt] -> [P.Stmt]
lowerStmts = map lowerStmt

lowerStmt :: Stmt -> P.Stmt
lowerStmt (Set loc triv) = P.Set loc triv
lowerStmt (BinOp op loc triv) = P.BinOp op loc triv
lowerStmt (MSet ptr offset val) = P.MSet ptr offset val
lowerStmt (MRef loc ptr offset) = P.MRef loc ptr offset
