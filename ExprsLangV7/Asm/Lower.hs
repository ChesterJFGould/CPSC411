{-# LANGUAGE RecursiveDo #-}
module Asm.Lower
( lower
)
where

import Asm.Types
import qualified Compiler.Locs as Locs
import Compiler.Types

import qualified Undead.Types as U

import Control.Monad.RevState
import qualified Data.Set as S

type Undead a = State (S.Set Mloc) a

lower :: Program -> U.Program
lower (Program blocks body) = U.Program (map lowerBlock blocks) (lowerBody body)

lowerBlock :: Block -> U.Block
lowerBlock (Block label body) = U.Block label (lowerBody body)

lowerBody :: Body -> U.Body
lowerBody (Body tail) = U.Body (evalState (lowerTail tail) (S.empty))

lowerTail :: Tail -> Undead U.Tail
lowerTail (Seq stmts tail) = U.Seq <$> (lowerStmts stmts)
                                   <*> (lowerTail tail)
lowerTail (TIf p c a) = lowerIf U.TIf lowerTail p c a
lowerTail (Jump place undeadOut) = modify (S.union (S.fromList undeadOut))
                                   >> return (U.Jump place undeadOut)

lowerStmts :: [Stmt] -> Undead [U.TStmt]
lowerStmts = mapM lowerStmt

lowerStmt :: Stmt -> Undead U.TStmt
lowerStmt (Set loc triv) = do
                           setTrivUndead triv
                           setLocDead loc
                           wrapUndeadOut (U.Set loc triv)
lowerStmt (BinOp op loc triv) = do
                                setTrivUndead triv
                                setLocUndead loc
                                wrapUndeadOut (U.BinOp op loc triv)
lowerStmt (If p c a) = lowerIf U.If lowerStmts p c a >>= wrapUndeadOut
lowerStmt (JumpRet label used) = do
                                 mapM setLocUndead used
                                 setLocDead ((MRloc . Reg) Locs.return)
                                 wrapUndeadOut (U.JumpRet label used)

lowerPred :: Pred -> Undead U.Pred
lowerPred (Bool b) = return (U.Bool b)
lowerPred (RelOp op loc triv) = do
                                setTrivUndead triv
                                setLocUndead loc
                                return (U.RelOp op loc triv)
lowerPred (Not pred) = U.Not <$> lowerPred pred
lowerPred (PSeq stmts pred) = U.PSeq <$> lowerStmts stmts
                                     <*> lowerPred pred
lowerPred (PIf p c a) = lowerIf U.PIf lowerPred p c a

lowerIf :: (U.Pred -> a -> a -> b) -> (c -> Undead a) -> Pred -> c -> c -> Undead b
lowerIf cons lowerer p c a = mdo
                             p' <- lowerPred p
                             put (S.union cUndeadIn aUndeadIn)
                             cUndeadIn <- get
                             c' <- lowerer c
                             put undeadOut
                             aUndeadIn <- get
                             a' <- lowerer a
                             undeadOut <- get
                             return (cons p' c' a')

maybeAloc :: Mloc -> Maybe Mloc
maybeAloc loc@(MAloc _) = Just loc
maybeAloc (MRloc _) = Nothing

setLocDead :: Mloc -> Undead ()
setLocDead = modify . S.delete

setLocUndead :: Mloc -> Undead ()
setLocUndead = modify . S.insert

setTrivUndead :: MTriv -> Undead ()
setTrivUndead (TMloc loc) = setLocUndead loc
setTrivUndead (MPtr _) = return ()

wrapUndeadOut :: U.Stmt -> Undead U.TStmt
wrapUndeadOut stmt = do
                     undeadOut <- get
                     return (undeadOut, stmt)
