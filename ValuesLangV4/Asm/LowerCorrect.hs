{-# LANGUAGE RecursiveDo #-}
module Asm.LowerCorrect
( lower
)
where

import Asm.Types

import qualified Nested.Types as N

import Control.Monad.State
import Control.Monad.Tardis
import Data.List as L
import Data.Set as S
import Data.Map as M
import Data.Maybe
import Debug.Trace

lower :: Program -> N.Program
lower prog@(Program top) = N.Program $ lowerTop locs top
                         where locs = assignRegs conflicts
                               conflicts = progConflicts prog

lowerTop :: Map Aloc N.Loc -> Top -> N.Top
lowerTop locs (Halt triv) = N.Halt triv'
                          where triv' = lowerTriv locs triv
lowerTop locs (TIf p c a) = N.TIf p' c' a'
                          where p' = lowerPred locs p
                                c' = lowerTop locs c
                                a' = lowerTop locs a
lowerTop locs (Seq stmts top) = N.Seq stmts' top'
                              where stmts' = L.map (lowerStmt locs) stmts
                                    top' = lowerTop locs top

lowerStmt :: Map Aloc N.Loc -> Stmt -> N.Stmt
lowerStmt locs (Set aloc triv) = N.Set loc triv'
                               where loc = lowerAloc locs aloc
                                     triv' = lowerTriv locs triv
lowerStmt locs (BinOp op aloc triv) = N.BinOp op' loc triv'
                                    where op' = lowerOp op
                                          loc = lowerAloc locs aloc
                                          triv' = lowerTriv locs triv
lowerStmt locs (If p c a) = N.If p' c' a'
                          where p' = lowerPred locs p
                                c' = L.map (lowerStmt locs) c
                                a' = L.map (lowerStmt locs) a

lowerPred :: Map Aloc N.Loc -> Pred -> N.Pred
lowerPred locs (Bool b) = N.Bool b
lowerPred locs (RelOp op aloc triv) = N.RelOp op' loc triv'
                                    where op' = lowerRelOp op
                                          loc = lowerAloc locs aloc
                                          triv' = lowerTriv locs triv
lowerPred locs (Not pred) = N.Not $ lowerPred locs pred
lowerPred locs (PIf p c a) = N.PIf p' c' a'
                           where p' = lowerPred locs p
                                 c' = lowerPred locs c
                                 a' = lowerPred locs a
lowerPred locs (PSeq stmts pred) = N.PSeq stmts' pred'
                                 where stmts' = L.map (lowerStmt locs) stmts
                                       pred' = lowerPred locs pred

lowerTriv :: Map Aloc N.Loc -> Triv -> N.Triv
lowerTriv locs (Int i) = N.Int i
lowerTriv locs (TAloc aloc) = N.Loc loc
                            where loc = lowerAloc locs aloc

lowerAloc :: Map Aloc N.Loc -> Aloc -> N.Loc
lowerAloc locs aloc = locs ! aloc

lowerOp :: Op -> N.Op
lowerOp Add = N.Add
lowerOp Mul = N.Mul

lowerRelOp :: RelOp -> N.RelOp
lowerRelOp Lt = N.Lt
lowerRelOp Gt = N.Gt
lowerRelOp Eq = N.Eq
lowerRelOp Lte = N.Lte
lowerRelOp Gte = N.Gte
lowerRelOp Neq = N.Neq

assignRegs :: Map Aloc (Set Aloc) -> Map Aloc N.Loc
assignRegs = (flip execState $ M.empty)
             . mapM assignReg
             . lengthSort
             . M.toList
           where lengthSort = sortBy (\a b -> compare (length $ snd a)
                                                      (length $ snd b))

assignReg :: (Aloc, Set Aloc) -> State (Map Aloc N.Loc) ()
assignReg (aloc, conflicts) = get >>= return -- Map conflicts to already alocated location
                                      . S.map fromJust
                                      . S.filter isJust
                                      . (flip S.map $ conflicts)
                                      . flip M.lookup
                                  >>= return . firstNotIn locations -- Get first non conflicting location
                                  >>= modify . M.insert aloc -- Alocate aloc to that location

locations :: [N.Loc]
locations = L.map N.Reg [ N.RSP
                        , N.RAX
                        , N.RBX
                        , N.RCX
                        , N.RDX
                        , N.RSI
                        , N.RDI
                        , N.R8
                        , N.R9
                        , N.R12
                        , N.R13
                        , N.R14
                        , N.R15 ]
            ++ L.map N.Addr [0..]

firstNotIn :: [N.Loc] -> Set N.Loc -> N.Loc
firstNotIn (loc : rest) conflicts
           | loc `S.member` conflicts = firstNotIn rest conflicts
           | otherwise = loc

progConflicts :: Program -> Map Aloc (Set Aloc)
progConflicts (Program top) = conflicts
                            where (_, conflicts) = execTardis (topConflicts top)
                                                              (S.empty, M.empty)

topConflicts :: Top -> Tardis (Set Aloc) (Map Aloc (Set Aloc)) ()
topConflicts (Halt (TAloc aloc)) = modifyBackwards (S.insert aloc)
topConflicts (Halt _) = return ()
topConflicts (TIf p c a) = ifConflicts p c a topConflicts
topConflicts (Seq stmts top) = mapM stmtConflicts stmts >> topConflicts top

stmtConflicts :: Stmt -> Tardis (Set Aloc) (Map Aloc (Set Aloc)) ()
stmtConflicts (Set aloc triv) = do
                                putTrivUndead triv
                                getUndeadWithoutTriv triv >>= addConflicts aloc
                                removeAlocUndead aloc
stmtConflicts (BinOp _ aloc triv) = do
                                    putTrivUndead triv
                                    putAlocUndead aloc
                                    getUndeadWithoutAloc aloc >>= addConflicts aloc
stmtConflicts (If p c a) = ifConflicts p c a ((>> return ()) . mapM stmtConflicts)

predConflicts :: Pred -> Tardis (Set Aloc) (Map Aloc (Set Aloc)) ()
predConflicts (Bool _) = return ()
predConflicts (RelOp _ aloc triv) = putAlocUndead aloc >> putTrivUndead triv
predConflicts (Not pred) = predConflicts pred
predConflicts (PIf p c a) = ifConflicts p c a predConflicts
predConflicts (PSeq stmts pred) = mapM stmtConflicts stmts >> predConflicts pred

ifConflicts :: Pred -> a -> a -> (a -> Tardis (Set Aloc) (Map Aloc (Set Aloc)) ())
               -> Tardis (Set Aloc) (Map Aloc (Set Aloc)) ()
ifConflicts p c a aConflicts = mdo
                               predConflicts p
                               sendPast (S.union cUndeadIn aUndeadIn)
                               cUndeadIn <- getFuture
                               aConflicts c
                               sendPast undeadOut
                               aUndeadIn <- getFuture
                               aConflicts a
                               undeadOut <- getFuture
                               return ()

addConflicts :: Aloc -> Set Aloc -> Tardis (Set Aloc) (Map Aloc (Set Aloc)) ()
addConflicts aloc conflicts = getPast >>= sendFuture . (flip (S.foldr (M.alter (insertOrAdd aloc))) $ conflicts) . M.alter unionOrAdd aloc
                            where unionOrAdd Nothing = Just conflicts
                                  unionOrAdd (Just oldConflicts) = Just $ S.union conflicts oldConflicts
                                  insertOrAdd aloc conflicts = Just $ S.insert aloc (maybe S.empty id conflicts)

putTrivUndead :: Triv -> Tardis (Set Aloc) (Map Aloc (Set Aloc)) ()
putTrivUndead (TAloc aloc) = modifyBackwards (S.insert aloc)
putTrivUndead _ = return ()

putAlocUndead :: Aloc -> Tardis (Set Aloc) (Map Aloc (Set Aloc)) ()
putAlocUndead = modifyBackwards . S.insert

removeAlocUndead :: Aloc -> Tardis (Set Aloc) (Map Aloc (Set Aloc)) ()
removeAlocUndead = modifyBackwards . S.delete

getUndeadWithoutTriv :: Triv -> Tardis (Set Aloc) (Map Aloc (Set Aloc)) (Set Aloc)
getUndeadWithoutTriv (TAloc aloc) = getsFuture $ S.delete aloc
getUndeadWithoutTriv _ = getFuture

getUndeadWithoutAloc :: Aloc -> Tardis (Set Aloc) (Map Aloc (Set Aloc)) (Set Aloc)
getUndeadWithoutAloc = getsFuture . S.delete
