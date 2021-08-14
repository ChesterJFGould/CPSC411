module Blocks.Lower
( lower
)
where

import Blocks.Types
import Compiler.Types
import qualified Para.Types as P

lower :: Program -> P.Program
lower (Program labels blocks body) = P.Program labels (lowerBody body ++ lowerBlocks blocks)

lowerBlocks :: [Block] -> [P.Stmt]
lowerBlocks = concat . map lowerBlock

lowerBlock :: Block -> [P.Stmt]
lowerBlock (Block label body) = let (hd : tl) = lowerBody body
                                in P.Labelled label hd : tl

lowerBody :: Body -> [P.Stmt]
lowerBody (Body tail) = lowerTail tail

lowerTail :: Tail -> [P.Stmt]
lowerTail (Jump triv) = [ P.Jump triv ]
lowerTail (Seq stmts tail) = lowerStmts stmts ++ lowerTail tail
lowerTail (If (RelOp op loc triv) c a) = [ P.Compare loc triv
                                         , P.JumpIf op c
                                         , P.Jump (RLabel a)
                                         ]

lowerStmts :: [Stmt] -> [P.Stmt]
lowerStmts = map lowerStmt

lowerStmt :: Stmt -> P.Stmt
lowerStmt (Set loc triv) = P.Set loc triv
lowerStmt (NumOp op loc triv) = P.NumOp op loc triv
lowerStmt (MRef loc ptr offset) = P.MRef loc ptr offset
lowerStmt (MSet ptr offset val) = P.MSet ptr offset val
