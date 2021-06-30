{-# LANGUAGE RecursiveDo #-}

module Asm.Lower
( lower
)
where

import Asm.Types
import Compiler.Types
import Compiler.Values

import qualified Undead.Types as U

import Control.Monad.Tardis
import Data.List as L
import Data.Set as S

type Analysis = Tardis (Set MLoc) (Set Aloc, Set (MLoc, MLoc))

lower :: Program -> U.Program
lower (Program defs tail) = U.Program (L.map lowerBlock defs) (lowerBody tail)

lowerBlock :: Block -> U.Block
lowerBlock (Block label body) = U.Block label (lowerBody body)

lowerBody :: Tail -> U.TBody
lowerBody body = (tag, U.Body body')
               where (body', (_, tag)) = runTardis (lowerTail body) (S.empty, (S.empty, S.empty))

lowerTail :: Tail -> Analysis U.TTail
lowerTail (TSeq stmts tail) = do
                              stmts' <- lowerStmts stmts
                              tail' <- lowerTail tail
                              undeadOut <- getFuture
                              return (undeadOut, U.TSeq stmts' tail')
{-
lowerTail (TSeq stmts tail) = tagUndeadOut ( U.TSeq <$> lowerStmts stmts
                                                    <*> lowerTail tail )
-}
lowerTail (TIf p c a) = tagUndeadOut ( lowerIf U.TIf lowerTail p c a )
lowerTail (Jump place undeadOut) = tagUndeadOut ( do
                                                  mapM setLocUndead undeadOut
                                                  let place' = lowerPlace place
                                                  return (U.Jump place' undeadOut) )

lowerStmts :: [Stmt] -> Analysis [U.TStmt]
lowerStmts = mapM lowerStmt

lowerStmt :: Stmt -> Analysis U.TStmt
lowerStmt (Set loc triv) = tagUndeadOut ( do
                                          setTrivUndead triv
                                          undeadWithoutTriv triv >>= conflict loc
                                          setLocDead loc
                                          let triv' = lowerTriv triv
                                          return (U.Set loc triv') )
lowerStmt (BinOp op loc triv) = tagUndeadOut ( do
                                               setTrivUndead triv
                                               setLocUndead loc
                                               undeadWithoutLoc loc >>= conflict loc
                                               let triv' = lowerTriv triv
                                               return (U.BinOp op loc triv') )
lowerStmt (If p c a) = tagUndeadOut ( lowerIf U.If lowerStmts p c a )
lowerStmt (JumpRet label undeadOut) = tagUndeadOut ( do
                                                     mapM setLocUndead undeadOut
                                                     setLocDead ((MRloc . Reg) returnRegister)
                                                     return (U.JumpRet label undeadOut) )

lowerPred :: Pred -> Analysis U.TPred
lowerPred (Bool b) = tagUndeadOut ( return (U.Bool b) )
lowerPred (RelOp op loc triv) = tagUndeadOut ( do
                                               setLocUndead loc
                                               setTrivUndead triv
                                               let triv' = lowerTriv triv
                                               return (U.RelOp op loc triv') )
lowerPred (PSeq stmts pred) = tagUndeadOut ( U.PSeq <$> lowerStmts stmts
                                                    <*> lowerPred pred )
lowerPred (PIf p c a) = tagUndeadOut ( lowerIf U.PIf lowerPred p c a )

lowerIf :: (U.TPred -> a -> a -> b) -> (c -> Analysis a) -> Pred -> c -> c -> Analysis b
lowerIf cons lowerer p c a = mdo
                             p' <- lowerPred p
                             sendPast (S.union cUndeadIn aUndeadIn)
                             cUndeadIn <- getFuture
                             c' <- lowerer c
                             sendPast undeadOut
                             aUndeadIn <- getFuture
                             a' <- lowerer a
                             undeadOut <- getFuture
                             return (cons p' c' a')

lowerTriv :: Triv -> U.Triv
lowerTriv (Int i) = U.Int i
lowerTriv (Loc loc) = U.Loc loc

lowerPlace :: Place -> U.Place
lowerPlace (PLabel label) = U.PLabel label
lowerPlace (PLoc loc) = U.PLoc loc

tagUndeadOut :: Analysis a -> Analysis (U.Undead a)
tagUndeadOut analysis = do
                        analysis' <- analysis
                        undeadOut <- getFuture
                        return (undeadOut, analysis')

conflict :: MLoc -> Set MLoc -> Analysis ()
conflict loc conflicts = do
                         (nodes, edges) <- getPast
                         let newEdges = S.map ((,) loc) conflicts
                             edges' = S.union newEdges edges
                             nodes' = case loc of
                                            MRloc _ -> nodes
                                            MAloc loc -> S.insert loc nodes
                         sendFuture (nodes', edges')

setLocDead :: MLoc -> Analysis ()
setLocDead = modifyBackwards . S.delete

setLocUndead :: MLoc -> Analysis ()
setLocUndead = modifyBackwards . S.insert

setTrivUndead :: Triv -> Analysis ()
setTrivUndead (Int _) = return ()
setTrivUndead (Loc loc) = setLocUndead loc

undeadWithoutLoc :: MLoc -> Analysis (Set MLoc)
undeadWithoutLoc = getsFuture . S.delete

undeadWithoutTriv :: Triv -> Analysis (Set MLoc)
undeadWithoutTriv (Int _) = getFuture
undeadWithoutTriv (Loc loc) = undeadWithoutLoc loc
