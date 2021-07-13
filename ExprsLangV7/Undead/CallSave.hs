{-# LANGUAGE RecursiveDo #-}
module Undead.CallSave
( callSave
)
where

import qualified Compiler.Locs as Locs
import Compiler.Types
import Undead.Types

import Control.Monad.RevState
import Data.Maybe
import qualified Data.Set as S

type Undead a = State (S.Set Mloc) a

callSave :: Program -> Program
callSave (Program blocks body) = Program (map saveBlock blocks) (saveBody body)

saveBlock :: Block -> Block
saveBlock (Block label body) = Block label (saveBody body)

saveBody :: Body -> Body
saveBody (Body tail) = Body (saveTail tail)

saveTail :: Tail -> Tail
saveTail (Seq stmts tail) = Seq (saveStmts stmts) (saveTail tail)
saveTail (TIf p c a) = TIf (savePred p) (saveTail c) (saveTail a)
saveTail tail@(Jump _ _) = tail

saveStmts :: [TStmt] -> [TStmt]
saveStmts = concat . map saveStmt

saveStmt :: TStmt -> [TStmt]
saveStmt stmt@(_, Set _ _) = [ stmt ]
saveStmt stmt@(_, BinOp _ _ _) = [ stmt ]
saveStmt (undeadOut, If p c a) = [ (undeadOut, If (savePred p) (saveStmts c) (saveStmts a)) ]
saveStmt (undeadOut, JumpRet label used) = wrapJumpRet label used undeadOut

savePred :: Pred -> Pred
savePred pred@(Bool _) = pred
savePred pred@(RelOp _ _ _) = pred
savePred (Not pred) = Not (savePred pred)
savePred (PSeq stmts pred) = PSeq (saveStmts stmts) (savePred pred)
savePred (PIf p c a) = PIf (savePred p) (savePred c) (savePred a)

wrapJumpRet :: Label -> [Mloc] -> S.Set Mloc -> [TStmt]
wrapJumpRet label used undeadOut = patchStmts stmts undeadOut
                                 where stmts = concat [ saveStmts
                                                      , [ BinOp Sub framePointer stackSize
                                                        , JumpRet label used
                                                        , BinOp Add framePointer stackSize ]
                                                      , loadStmts ]
                                       saveStmts = zipWith Set saveLocs (map TMloc undeadAlocs)
                                       loadStmts = zipWith Set undeadAlocs (map TMloc saveLocs)
                                       framePointer = (MRloc . Reg) Locs.frame
                                       -- We should maybe add a word type to MTriv for this kind of thing as this isn't
                                       -- actually a ptr.
                                       stackSize = (MPtr . Ptr . fromIntegral) (length undeadAlocs * 8)
                                       undeadAlocs = mapMaybe maybeAloc (S.toList undeadOut)
                                       saveLocs = map MRloc Locs.save

maybeAloc :: Mloc -> Maybe Mloc
maybeAloc loc@(MAloc _) = Just loc
maybeAloc (MRloc _) = Nothing

patchStmts :: [Stmt] -> S.Set Mloc -> [TStmt]
patchStmts stmts = evalState (patchStmts' stmts)

patchStmts' :: [Stmt] -> Undead [TStmt]
patchStmts' = mapM patchStmt

patchStmt :: Stmt -> Undead TStmt
patchStmt stmt@(Set loc triv) = do
                                setTrivUndead triv
                                setLocDead loc
                                wrapUndeadOut stmt
patchStmt stmt@(BinOp _ loc triv) = do
                                    setTrivUndead triv
                                    setLocUndead loc
                                    wrapUndeadOut stmt
patchStmt (If p c a) = patchIf If patchStmts' p c' a' >>= wrapUndeadOut
                     where c' = removeTags c
                           a' = removeTags a
patchStmt stmt@(JumpRet _ used) = do
                                  mapM setLocUndead used
                                  setLocDead ((MRloc . Reg) Locs.return)
                                  wrapUndeadOut stmt

patchPred :: Pred -> Undead Pred
patchPred pred@(Bool _) = return pred
patchPred pred@(RelOp _ loc triv) = do
                                    setTrivUndead triv
                                    setLocUndead loc
                                    return pred
patchPred (Not pred) = Not <$> patchPred pred
patchPred (PSeq stmts pred) = PSeq <$> patchStmts' stmts'
                                   <*> patchPred pred
                            where stmts' = removeTags stmts
patchPred (PIf p c a) = patchIf PIf patchPred p c a

patchIf :: (Pred -> a -> a -> b) -> (c -> Undead a) -> Pred -> c -> c -> Undead b
patchIf cons patcher p c a = mdo
                             p' <- patchPred p
                             put (S.union cUndeadIn aUndeadIn)
                             cUndeadIn <- get
                             c' <- patcher c
                             put undeadOut
                             aUndeadIn <- get
                             a' <- patcher a
                             undeadOut <- get
                             return (cons p' c' a')

setLocDead :: Mloc -> Undead ()
setLocDead = modify . S.delete

setLocUndead :: Mloc -> Undead ()
setLocUndead = modify . S.insert

setTrivUndead :: MTriv -> Undead ()
setTrivUndead (TMloc loc) = setLocUndead loc
setTrivUndead (MPtr _) = return ()

wrapUndeadOut :: Stmt -> Undead TStmt
wrapUndeadOut stmt = do
                     undeadOut <- get
                     return (undeadOut, stmt)

removeTags :: [TStmt] -> [Stmt]
removeTags = map snd
