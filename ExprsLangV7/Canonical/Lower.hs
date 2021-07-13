module Canonical.Lower
( lower
)
where

import Canonical.Types
import qualified Compiler.Locs as Locs
import Compiler.Types

import qualified Asm.Types as A

import Control.Monad.State
import Data.Word

lower :: Program -> Gensym A.Program
lower (Program blocks body) = A.Program <$> mapM lowerBlock blocks
                                        <*> lowerBody body

lowerBlock :: Block -> Gensym A.Block
lowerBlock (Block name body) = A.Block name <$> lowerBody body

lowerBody :: Body -> Gensym A.Body
lowerBody (Body tail) = do
                        lr <- genAloc "lr"
                        let lr' = MAloc lr
                            tail' = A.Seq [ A.Set lr' ((TMloc . MRloc . Reg) Locs.link) ]
                                          (lowerTail lr' tail)
                        return (A.Body tail')

lowerTail :: Mloc -> Tail -> A.Tail
lowerTail lr (Expr (Triv triv)) = A.Seq [ A.Set returnReg triv ]
                                        (A.Jump (PMloc lr) [lr, returnReg])
                                where returnReg = (MRloc . Reg) Locs.return
lowerTail lr (Expr (BinOp op a b)) = A.Seq [ A.Set returnReg a
                                           , A.BinOp op returnReg b ]
                                           (A.Jump (PMloc lr) [lr, returnReg])
                                   where returnReg = (MRloc . Reg) Locs.return
lowerTail lr (Seq stmts tail) = A.Seq (lowerStmts stmts) (lowerTail lr tail)
lowerTail lr (TIf p c a) = A.TIf (lowerPred p) (lowerTail lr c) (lowerTail lr a)
lowerTail lr (Jump label undeadOut) = A.Seq [ A.Set ((MRloc . Reg) Locs.link) (TMloc lr) ]
                                            ( A.Jump (MLabel label) undeadOut )

lowerStmts :: [Stmt] -> [A.Stmt]
lowerStmts = concat . map lowerStmt

lowerStmt :: Stmt -> [A.Stmt]
lowerStmt (Set loc expr) = lowerSet loc expr
lowerStmt (If p c a) = [ A.If (lowerPred p) (lowerStmts c) (lowerStmts a) ]
lowerStmt (JumpRet label undeadOut) = [ A.JumpRet label undeadOut ]

lowerPred :: Pred -> A.Pred
lowerPred (Bool b) = A.Bool b
lowerPred (RelOp op (MPtr (Ptr a)) (MPtr (Ptr b))) = A.Bool (reifyRelOp op a b)
lowerPred (RelOp op a@(MPtr _) b) = lowerPred (RelOp (swapRelOp op) b a)
lowerPred (RelOp op (TMloc loc) triv) = A.RelOp op loc triv
lowerPred (Not pred) = A.Not (lowerPred pred)
lowerPred (PSeq stmts pred) = A.PSeq (lowerStmts stmts) (lowerPred pred)
lowerPred (PIf p c a) = A.PIf (lowerPred p) (lowerPred c) (lowerPred a)

lowerSet :: Mloc -> Expr -> [A.Stmt]
lowerSet loc (Triv triv) = [ A.Set loc triv ]
lowerSet loc (BinOp op a b) = [ A.Set loc a
                              , A.BinOp op loc b ]

swapRelOp :: RelOp -> RelOp
swapRelOp Lt = Gt
swapRelOp Gt = Lt
swapRelOp Eq = Eq
swapRelOp Lte = Gte
swapRelOp Gte = Lte
swapRelOp Neq = Neq

reifyRelOp :: RelOp -> Word64 -> Word64 -> Bool
reifyRelOp Lt = (<)
reifyRelOp Gt = (>)
reifyRelOp Eq = (==)
reifyRelOp Lte = (<=)
reifyRelOp Gte = (>=)
reifyRelOp Neq = (/=)

genAloc :: String -> Gensym Aloc
genAloc template = do
                   i <- get
                   let aloc = Aloc template i
                   put (i + 1)
                   return aloc
