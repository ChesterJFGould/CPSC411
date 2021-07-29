module Canonical.Lower
( lower
)
where

import qualified Asm.Types as A
import Canonical.Types
import Compiler.Gensym
import Compiler.Locs
import Compiler.Types

import Control.Monad
import Data.Word

lower :: Program -> Gensym A.Program
lower (Program blocks body) = A.Program <$> lowerBlocks blocks
                                        <*> lowerBody body

lowerBlocks :: [Block] -> Gensym [A.Block]
lowerBlocks = mapM lowerBlock

lowerBlock (Block label body) = A.Block label <$> lowerBody body

lowerBody :: Body -> Gensym A.Body
lowerBody (Body tail) = A.Body <$> lowerTail tail

lowerTail :: Tail -> Gensym A.Tail
lowerTail (Jump place undeadOut) = return (A.Jump place undeadOut)
lowerTail (TSeq stmts tail) = A.Seq <$> lowerStmts stmts
                                    <*> lowerTail tail
lowerTail (TIf p c a) = A.TIf <$> lowerPred p
                              <*> lowerTail c
                              <*> lowerTail a

lowerStmts :: [Stmt] -> Gensym [A.Stmt]
lowerStmts = liftM concat . mapM lowerStmt

lowerStmt :: Stmt -> Gensym [A.Stmt]
lowerStmt (Set loc (Triv triv)) = return [ A.Set loc triv ]
lowerStmt (Set loc (BinOp op a b)) = return [ A.Set loc a
                                            , A.BinOp op loc b ]
lowerStmt (Set loc (MRef ptr offset)) = return [ A.MRef loc ptr offset ]
lowerStmt (Set loc (Alloc size)) = return (lowerAlloc (A.Set loc) size)
lowerStmt (MSet ptr offset (Triv triv)) = return [ A.MSet ptr offset triv ]
lowerStmt (MSet ptr offset (BinOp op a b)) = do
                                             tmp <- genAloc "tmp"
                                             let tmp' = MAloc tmp
                                             return [ A.Set tmp' a
                                                    , A.BinOp op tmp' b
                                                    , A.MSet ptr offset (MMloc tmp') ]
lowerStmt (MSet ptr offset (MRef ptr' offset')) = do
                                                  tmp <- genAloc "tmp"
                                                  let tmp' = MAloc tmp
                                                  return [ A.MRef tmp' ptr' offset'
                                                         , A.MSet ptr offset (MMloc tmp') ]
lowerStmt (MSet ptr offset (Alloc size)) = return (lowerAlloc (A.MSet ptr offset) size)
lowerStmt (JumpRet label undeadOut) = return [ A.JumpRet label undeadOut ]
                       -- Hmmmmmmmmmmmmm
lowerStmt (If p c a) = (((.) . (.) . (.)) pure A.If) <$> lowerPred p
                                                     <*> lowerStmts c
                                                     <*> lowerStmts a

lowerPred :: Pred -> Gensym A.Pred
lowerPred (Bool b) = return (A.Bool b)
lowerPred (RelOp op (MLit (Lit a)) (MLit (Lit b))) = return (A.Bool (reifyRelOp op a b))
lowerPred (RelOp op a@(MLit _) b) = lowerPred (RelOp (commuteRelOp op) b a)
lowerPred (RelOp op (MMloc loc) triv) = return (A.RelOp op loc triv)
lowerPred (Not pred) = A.Not <$> lowerPred pred
lowerPred (PSeq stmts pred) = A.PSeq <$> lowerStmts stmts
                                     <*> lowerPred pred
lowerPred (PIf p c a) = A.PIf <$> lowerPred p
                              <*> lowerPred c
                              <*> lowerPred a

lowerAlloc :: (MTriv -> A.Stmt) -> MTriv -> [A.Stmt]
lowerAlloc cons size = [ cons (MMloc heapPointer')
                       , A.BinOp Add heapPointer' size ]
                     where heapPointer' = MRloc heapPointer

reifyRelOp :: RelOp -> Word64 -> Word64 -> Bool
reifyRelOp Lt = (<)
reifyRelOp Gt = (>)
reifyRelOp Eq = (==)
reifyRelOp Lte = (<=)
reifyRelOp Gte = (>=)
reifyRelOp Neq = (/=)

commuteRelOp :: RelOp -> RelOp
commuteRelOp Lt = Gt
commuteRelOp Gt = Lt
commuteRelOp Eq = Eq
commuteRelOp Lte = Gte
commuteRelOp Gte = Lte
commuteRelOp Neq = Neq
