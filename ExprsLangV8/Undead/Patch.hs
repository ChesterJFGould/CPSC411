{-# LANGUAGE RecursiveDo #-}
module Undead.Patch
( patch
)
where

import Compiler.Locs
import Compiler.Types
import Undead.Types

import Control.Monad.RevState
import qualified Data.Set as S

type Undead a = State (S.Set Mloc) a

patch :: S.Set Mloc -> [Stmt] -> [TStmt]
patch undeadOut stmts = evalState (patchStmts stmts) undeadOut

patchStmts :: [Stmt] -> Undead [TStmt]
patchStmts = mapM patchStmt

patchStmt :: Stmt -> Undead TStmt
patchStmt (Set loc triv) = do
                           setTrivUndead triv
                           setLocDead loc
                           wrapUndead (Set loc triv)
patchStmt (BinOp op loc triv) = do
                                setTrivUndead triv
                                setLocUndead loc
                                wrapUndead (BinOp op loc triv)
patchStmt (MSet ptr offset val) = do
                                  setTrivUndead val
                                  setTrivUndead offset
                                  setTrivUndead ptr
                                  wrapUndead (MSet ptr offset val)
patchStmt (MRef loc ptr offset) = do
                                  setTrivUndead offset
                                  setTrivUndead ptr
                                  setLocDead loc
                                  wrapUndead (MRef loc ptr offset)
patchStmt (JumpRet label used) = do
                                 setLocsUndead used
                                 setLocDead ((MRloc . Reg) returnRegister)
                                 wrapUndead (JumpRet label used)
patchStmt (If p c a) = mdo
                       p' <- patchPred p
                       put (S.union cUndeadIn aUndeadIn)
                       cUndeadIn <- get
                       c' <- patchStmts (removeTags c)
                       put undeadOut
                       aUndeadIn <- get
                       a' <- patchStmts (removeTags a)
                       undeadOut <- get
                       wrapUndead (If p' c' a')

patchPred :: Pred -> Undead Pred
patchPred (Bool b) = return (Bool b)
patchPred (RelOp op a b) = return (RelOp op a b)
patchPred (Not pred) = Not <$> patchPred pred
patchPred (PSeq stmts pred) = PSeq <$> patchStmts (removeTags stmts)
                                   <*> patchPred pred
patchPred (PIf p c a) = PIf <$> patchPred p
                            <*> patchPred c
                            <*> patchPred a

removeTags :: [TStmt] -> [Stmt]
removeTags = map removeTag

removeTag :: TStmt -> Stmt
removeTag = snd

wrapUndead :: Stmt -> Undead TStmt
wrapUndead stmt = do
                  undead <- get
                  return (undead, stmt)

setLocDead :: Mloc -> Undead ()
setLocDead = modify . S.delete

setLocUndead :: Mloc -> Undead ()
setLocUndead = modify . S.insert

setLocsUndead :: [Mloc] -> Undead [()]
setLocsUndead = mapM setLocUndead

setTrivUndead :: MTriv -> Undead ()
setTrivUndead (MMloc loc) = setLocUndead loc
setTrivUndead (MLit _) = return ()
