module Block.Lower
( lower
)
where

import Block.Types

import qualified Para.Types as P

lower :: Program -> P.Program
lower (Program defs body) = P.Program (lowerTail body ++ (concat . map lowerBlock) defs)

lowerBlock :: Block -> [P.Stmt]
lowerBlock (Block label body) = P.Labelled label hd : tl
                              where (hd : tl) = lowerTail body

lowerTail :: Tail -> [P.Stmt]
lowerTail (Seq stmts tail) = lowerStmts stmts ++ lowerTail tail
lowerTail (If (RelOp op loc triv) c a) = [ P.Compare loc (lowerTriv triv)
                                         , P.JumpIf op c
                                         , P.Jump (P.PLabel a) ]
lowerTail (Jump place) = [ P.Jump (lowerPlace place) ]

lowerStmts :: [Stmt] -> [P.Stmt]
lowerStmts = map lowerStmt

lowerStmt :: Stmt -> P.Stmt
lowerStmt (Set loc triv) = P.Set loc (lowerTriv triv)
lowerStmt (BinOp op loc triv) = P.BinOp op loc (lowerTriv triv)

lowerTriv :: Triv -> P.Triv
lowerTriv (Int i) = P.Int i
lowerTriv (Loc loc) = P.Loc loc
lowerTriv (TLabel label) = P.TLabel label

lowerPlace :: Place -> P.Place
lowerPlace (PLabel label) = P.PLabel label
lowerPlace (PLoc loc) = P.PLoc loc
