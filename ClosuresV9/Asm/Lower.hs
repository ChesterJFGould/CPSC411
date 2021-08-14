{-# LANGUAGE RecursiveDo #-}
module Asm.Lower
( lower
)
where

import Asm.Types
import qualified Compiler.Locations as Locations
import Compiler.Types
import qualified Undead.Types as U

import Control.Monad.RevState
import qualified Data.Set as S

type Undead = State (S.Set Mloc)

lower :: Program -> U.Program
lower (Program labels blocks body) = U.Program labels (lowerBlocks blocks)
                                                      (lowerBody body)

lowerBlocks :: [Block] -> [U.Block]
lowerBlocks = map lowerBlock

lowerBlock :: Block -> U.Block
lowerBlock (Block label body) = U.Block label (lowerBody body)

lowerBody :: Body -> U.Body
lowerBody (Body tail) = U.Body (evalState (lowerTail tail) S.empty)

lowerTail :: Tail -> Undead U.Tail
lowerTail (Jump triv used) = do
                             mapM setLocUndead used
                             setTrivUndead triv
                             return (U.Jump triv used)
lowerTail (TSeq stmts tail) = U.TSeq <$> lowerStmts stmts
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
                           tagUndead (U.Set loc triv)
lowerStmt (NumOp op a b) = do
                           setTrivUndead b
                           setLocUndead a
                           tagUndead (U.NumOp op a b)
lowerStmt (MRef ptr offset val) = do
                                  setTrivUndead val
                                  setTrivUndead offset
                                  setLocDead ptr
                                  tagUndead (U.MRef ptr offset val)
lowerStmt (MSet ptr offset val) = do
                                  setTrivUndead val
                                  setTrivUndead offset
                                  setTrivUndead ptr
                                  tagUndead (U.MSet ptr offset val)
lowerStmt (JumpRet triv used) = do
                                mapM setLocUndead used
                                setTrivUndead triv
                                setLocDead ((MRloc . Reg) Locations.returnRegister)
                                tagUndead (U.JumpRet triv used)
lowerStmt (If p c a) = mdo
                       p' <- lowerPred p
                       put (S.union cUndeadIn aUndeadIn)
                       cUndeadIn <- get
                       c' <- lowerStmts c
                       put undeadOut
                       aUndeadIn <- get
                       a' <- lowerStmts a
                       undeadOut <- get
                       tagUndead (U.If p' c' a')

lowerPred :: Pred -> Undead U.Pred
lowerPred (Bool b) = return (U.Bool b)
lowerPred (RelOp op a b) = do
                           setTrivUndead b
                           setLocUndead a
                           return (U.RelOp op a b)
lowerPred (Not pred) = U.Not <$> lowerPred pred
lowerPred (PSeq stmts pred) = U.PSeq <$> lowerStmts stmts
                                     <*> lowerPred pred
lowerPred (PIf p c a) = U.PIf <$> lowerPred p
                              <*> lowerPred c
                              <*> lowerPred a

tagUndead :: U.Stmt -> Undead U.TStmt
tagUndead stmt = do
                 undead <- get
                 return (undead, stmt)

setLocUndead :: Mloc -> Undead ()
setLocUndead = modify . S.insert

setTrivUndead :: MTriv -> Undead ()
setTrivUndead (MLit _) = return ()
setTrivUndead (MMloc mloc) = setLocUndead mloc
setTrivUndead (MLabel _) = return ()

setLocDead :: Mloc -> Undead ()
setLocDead = modify . S.delete
