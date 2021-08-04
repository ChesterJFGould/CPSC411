{-# LANGUAGE RecursiveDo #-}
module Asm.Lower
( lower
)
where

import Asm.Types
import Compiler.Gensym
import Compiler.Locs
import Compiler.Types
import qualified Undead.Types as U

import Control.Monad.RevState
import qualified Data.Set as S

type Undead a = State (S.Set Mloc) a

lower :: Program -> U.Program
lower (Program blocks body) = U.Program (lowerBlocks blocks)
                                        (lowerBody body)

lowerBlocks :: [Block] -> [U.Block]
lowerBlocks = map lowerBlock

lowerBlock :: Block -> U.Block
lowerBlock (Block label body) = U.Block label (lowerBody body)

lowerBody :: Body -> U.Body
lowerBody (Body tail) = U.Body (evalState (lowerTail tail) S.empty)

lowerTail :: Tail -> Undead U.Tail
lowerTail (Jump place used) = do
                              mapM setLocUndead used
                              return (U.Jump place used)
lowerTail (Seq stmts tail) = U.Seq <$> lowerStmts stmts
                                   <*> lowerTail tail
lowerTail (TIf p c a) = U.TIf <$> lowerPred p
                              <*> lowerTail c
                              <*> lowerTail a

lowerStmts :: [Stmt] -> Undead [U.TStmt]
lowerStmts = mapM lowerStmt

lowerStmt :: Stmt -> Undead U.TStmt
lowerStmt (Set loc triv) = do
                           setTrivUndead triv
                           setLocDead loc
                           wrapUndead (U.Set loc triv)
lowerStmt (BinOp op loc triv) = do
                                setTrivUndead triv
                                setLocUndead loc
                                wrapUndead (U.BinOp op loc triv)
lowerStmt (MSet ptr offset val) = do
                                  setTrivUndead val
                                  setTrivUndead offset
                                  setTrivUndead ptr
                                  wrapUndead (U.MSet ptr offset val)
lowerStmt (MRef loc ptr offset) = do
                                  setTrivUndead offset
                                  setTrivUndead ptr
                                  setLocDead loc
                                  wrapUndead (U.MRef loc ptr offset)
lowerStmt (JumpRet label used) = do
                                 mapM setLocUndead used
                                 setLocDead ((MRloc . Reg) returnRegister)
                                 wrapUndead (U.JumpRet label used)
lowerStmt (If p c a) = mdo
                       p' <- lowerPred p
                       put (S.union cUndeadIn aUndeadIn)
                       cUndeadIn <- get
                       c' <- lowerStmts c
                       put undeadOut
                       aUndeadIn <- get
                       a' <- lowerStmts a
                       undeadOut <- get
                       wrapUndead (U.If p' c' a')

lowerPred :: Pred -> Undead U.Pred
lowerPred (Bool b) = return (U.Bool b)
lowerPred (RelOp op a b) = do
                           setLocUndead a
                           setTrivUndead b
                           return (U.RelOp op a b)
lowerPred (Not pred) = U.Not <$> lowerPred pred
lowerPred (PSeq stmts pred) = U.PSeq <$> lowerStmts stmts
                                     <*> lowerPred pred
lowerPred (PIf p c a) = U.PIf <$> lowerPred p
                              <*> lowerPred c
                              <*> lowerPred a

wrapUndead :: U.Stmt -> Undead U.TStmt
wrapUndead stmt = do
                  undead <- get
                  return (undead, stmt)

setLocDead :: Mloc -> Undead ()
setLocDead = modify . S.delete

setLocUndead :: Mloc -> Undead ()
setLocUndead = modify . S.insert

setTrivUndead :: MTriv -> Undead ()
setTrivUndead (MMloc loc) = setLocUndead loc
setTrivUndead (MLit _) = return ()
