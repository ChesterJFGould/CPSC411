module Canonical.Lower
( lower
)
where

import Canonical.Types
import Compiler.Types
import Compiler.Values

import qualified Asm.Types as A

lower :: Program -> A.Program
lower (Program defs body) = A.Program (map lowerBlock defs)
                                      (lowerBody body)

lowerBlock :: Block -> A.Block
lowerBlock (Block label body) = A.Block label (lowerBody body)

lowerBody :: Body -> A.Tail
lowerBody (Body lr tail) = A.TSeq [ A.Set (MAloc lr)
                                          ((A.Loc . MRloc . Reg) linkRegister) ]
                                  (lowerTail (MAloc lr) tail)

lowerTail :: MLoc -> Tail -> A.Tail
lowerTail lr (Expr expr) = A.TSeq (lowerSet ((MRloc . Reg) RAX) expr)
                                  (A.Jump (A.PLoc lr)
                                          [ (MRloc . Reg) RAX
                                          , lr ])
lowerTail lr (TSeq stmts tail) = A.TSeq (lowerStmts stmts) (lowerTail lr tail)
lowerTail lr (TIf p c a) = A.TIf (lowerPred p) (lowerTail lr c) (lowerTail lr a)
lowerTail lr (Jump label undeadOut) = A.TSeq [ A.Set ((MRloc . Reg) linkRegister)
                                                     (A.Loc lr) ]
                                             (A.Jump (A.PLabel label)
                                                     ( (MRloc . Reg) linkRegister
                                                     : undeadOut ))

lowerStmts :: [Stmt] -> [A.Stmt]
lowerStmts = concat . map lowerStmt

lowerStmt :: Stmt -> [A.Stmt]
lowerStmt (Set loc expr) = lowerSet loc expr
lowerStmt (If p c a) = [ A.If (lowerPred p) (lowerStmts c) (lowerStmts a) ]
lowerStmt (JumpRet label undeadOut) = [ A.JumpRet label undeadOut ]

lowerPred :: Pred -> A.Pred
lowerPred (Bool b) = A.Bool b
lowerPred (RelOp op (Int a) (Int b)) = A.Bool (reifyRelOp op a b)
lowerPred (RelOp op (Int a) (Loc loc)) = lowerPred (Not (RelOp op (Int a) (Loc loc)))
lowerPred (RelOp op (Loc loc) triv) = A.RelOp op loc (lowerTriv triv)
lowerPred (Not pred) = A.Not (lowerPred pred)
lowerPred (PSeq stmts pred) = A.PSeq (lowerStmts stmts) (lowerPred pred)
lowerPred (PIf p c a) = A.PIf (lowerPred p) (lowerPred c) (lowerPred a)

lowerSet :: MLoc -> Expr -> [A.Stmt]
lowerSet loc (Triv triv) = [ A.Set loc (lowerTriv triv) ]
lowerSet loc (BinOp op l r) = [ A.Set loc (lowerTriv l)
                              , A.BinOp op loc (lowerTriv r) ]

lowerTriv :: Triv -> A.Triv
lowerTriv (Int i) = A.Int i
lowerTriv (Loc loc) = A.Loc loc

reifyRelOp :: RelOp -> Integer -> Integer -> Bool
reifyRelOp Lt = (<)
reifyRelOp Gt = (>)
reifyRelOp Eq = (==)
reifyRelOp Lte = (<=)
reifyRelOp Gte = (>=)
reifyRelOp Neq = (/=)
