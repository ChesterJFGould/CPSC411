module Canonical.Lower
( lower
)
where

import qualified Asm.Types as A
import Canonical.Types
import Compiler.Gensym
import qualified Compiler.Locations as Locations
import Compiler.Types

import Data.Word

lower :: Program -> Gensym A.Program
lower (Program labels blocks body) = A.Program labels <$> lowerBlocks blocks
                                                      <*> lowerBody body

lowerBlocks :: [Block] -> Gensym [A.Block]
lowerBlocks = mapM lowerBlock

lowerBlock :: Block -> Gensym A.Block
lowerBlock (Block label body) = A.Block label <$> lowerBody body

lowerBody :: Body -> Gensym A.Body
lowerBody (Body tail) = A.Body <$> lowerTail tail

lowerTail :: Tail -> Gensym A.Tail
lowerTail (Jump triv used) = return (A.Jump triv used)
lowerTail (TSeq stmts tail) = A.TSeq <$> lowerStmts stmts
                                     <*> lowerTail tail
lowerTail (TIf p c a) = A.TIf <$> lowerPred p
                              <*> lowerTail c
                              <*> lowerTail a

lowerStmts :: [Stmt] -> Gensym [A.Stmt]
lowerStmts = (concat <$>) . mapM lowerStmt

lowerStmt :: Stmt -> Gensym [A.Stmt]
lowerStmt (Set mloc (Triv triv)) = return [ A.Set mloc triv ]
lowerStmt (Set mloc (NumOp op a b)) = return [ A.Set mloc a
                                             , A.NumOp op mloc b
                                             ]
lowerStmt (Set mloc (Alloc triv)) = let heapMloc = (MRloc . Reg) Locations.heapRegister
                                        heapTriv = MMloc heapMloc
                                    in return [ A.Set mloc heapTriv
                                              , A.NumOp Add heapMloc triv
                                              ]
lowerStmt (Set mloc (MRef ptr offset)) = return [ A.MRef mloc ptr offset ]
lowerStmt (MSet ptr offset (Triv triv)) = return [ A.MSet ptr offset triv ]
lowerStmt (MSet ptr offset expr) = do
                                   tmpAloc <- genAloc "asmTmp"
                                   let tmpMloc = MAloc tmpAloc
                                   loadTmpStmts <- lowerStmt (Set tmpMloc expr)
                                   let tmpTriv = MMloc tmpMloc
                                       msetStmt = A.MSet ptr offset tmpTriv
                                   return (loadTmpStmts ++ [ msetStmt ])
lowerStmt (JumpRet triv used) = return [ A.JumpRet triv used ]
lowerStmt (If p c a) = do
                       p' <- lowerPred p
                       c' <- lowerStmts c
                       a' <- lowerStmts a
                       return [ A.If p' c' a' ]

lowerPred :: Pred -> Gensym A.Pred
lowerPred (Bool b) = return (A.Bool b)
lowerPred (RelOp _ (MLabel _) _) = error "Tried to lower a comparison with a label"
lowerPred (RelOp _ _ (MLabel _)) = error "Tried to lower a comparison with a label"
lowerPred (RelOp op (MLit (Lit a)) (MLit (Lit b))) = return (A.Bool (reifyRelOp op a b))
lowerPred (RelOp op triv (MMloc mloc)) = return (A.RelOp (commuteRelOp op) mloc triv)
lowerPred (RelOp op (MMloc mloc) triv) = return (A.RelOp op mloc triv)
lowerPred (Not pred) = A.Not <$> lowerPred pred
lowerPred (PSeq stmts pred) = A.PSeq <$> lowerStmts stmts
                                     <*> lowerPred pred
lowerPred (PIf p c a) = A.PIf <$> lowerPred p
                              <*> lowerPred c
                              <*> lowerPred a

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
