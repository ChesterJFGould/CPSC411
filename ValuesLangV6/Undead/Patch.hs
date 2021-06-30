{-# LANGUAGE RecursiveDo #-}

module Undead.Patch
( patchStmts
)
where

import Compiler.Types
import Compiler.Values
import Undead.Types

import Control.Monad.Tardis
import Data.List as L
import Data.Set as S
import Debug.Trace

type Patch a = Tardis (Set MLoc) (Set Aloc, Set (MLoc, MLoc)) a

patchStmts :: Set MLoc -> [Stmt] -> ([TStmt], (Set Aloc, Set (MLoc, MLoc)))
patchStmts undeadOut stmts = (stmts'', (nodes, edges))
                           where (stmts'', (_, (nodes, edges))) = runTardis (patchStmts' stmts')
                                                                            (undeadOut, (S.empty, S.empty))
                                 stmts' = L.map ((,) S.empty) stmts

patchStmts' :: [TStmt] -> Patch [TStmt]
patchStmts' = mapM patchStmt

patchStmt :: TStmt -> Patch TStmt
patchStmt (_, stmt@(Set loc triv)) = tagUndeadOut ( do
                                                    setTrivUndead triv
                                                    undeadWithoutTriv triv >>= conflict loc
                                                    setLocDead loc
                                                    return stmt )
patchStmt (_, stmt@(BinOp op loc triv)) = tagUndeadOut ( do
                                                         setLocUndead loc
                                                         setTrivUndead triv
                                                         undeadWithoutLoc loc >>= conflict loc
                                                         return stmt )
patchStmt (_, If p c a) = tagUndeadOut ( patchIf If patchStmts' p c a )
patchStmt (_, stmt@(JumpRet label undeadOut)) = tagUndeadOut ( do
                                                               mapM setLocUndead undeadOut
                                                               setLocDead ((MRloc . Reg) returnRegister)
                                                               return stmt )

patchPred :: TPred -> Patch TPred
patchPred (_, pred@(Bool _)) = tagUndeadOut ( return pred )
patchPred (_, pred@(RelOp _ loc triv)) = tagUndeadOut ( do
                                                   setLocDead loc
                                                   setTrivUndead triv
                                                   return pred )
patchPred (_, Not pred) = tagUndeadOut ( Not <$> patchPred pred )
patchPred (_, PSeq stmts pred) = tagUndeadOut ( PSeq <$> patchStmts' stmts
                                                     <*> patchPred pred )
patchPred (_, PIf p c a) = tagUndeadOut ( patchIf PIf patchPred p c a )

patchIf :: (TPred -> a -> a -> b) -> (c -> Patch a) -> TPred -> c -> c -> Patch b
patchIf cons patch p c a = mdo
                           p' <- patchPred p
                           sendPast (S.union cUndeadIn aUndeadIn)
                           cUndeadIn <- getFuture
                           c' <- patch c
                           sendPast undeadOut
                           aUndeadIn <- getFuture
                           a' <- patch a
                           undeadOut <- getFuture
                           return (cons p' c' a')

tagUndeadOut :: Patch a -> Patch (Undead a)
tagUndeadOut patch = do
                     patch' <- patch
                     undeadOut <- getFuture
                     return (undeadOut, patch')

conflict :: MLoc -> Set MLoc -> Patch ()
conflict loc conflicts = do
                         (nodes, edges) <- getPast
                         let newEdges = S.map ((,) loc) conflicts
                             edges' = S.union newEdges edges
                             nodes' = case loc of
                                           MRloc _ -> nodes
                                           MAloc loc -> S.insert loc nodes
                         sendFuture (nodes', edges')

setLocDead :: MLoc -> Patch ()
setLocDead = modifyBackwards . S.delete

setLocUndead :: MLoc -> Patch ()
setLocUndead = modifyBackwards . S.insert

setTrivUndead :: Triv -> Patch ()
setTrivUndead (Int _) = return ()
setTrivUndead (Loc loc) = setLocUndead loc

undeadWithoutLoc :: MLoc -> Patch (Set MLoc)
undeadWithoutLoc = getsFuture . S.delete

undeadWithoutTriv :: Triv -> Patch (Set MLoc)
undeadWithoutTriv (Int _) = getFuture
undeadWithoutTriv (Loc loc) = undeadWithoutLoc loc
