{-# LANGUAGE RecursiveDo #-}
module Asm.Lower
( lower
)
where

import Asm.Types

import qualified Nested.Types as N

import Control.Monad.Tardis
import Data.List as L
import Data.Map as M
import Data.Set as S
import Graph

lower :: Program -> N.Program
lower prog = prog'
           where prog' = lower' locs prog
                 locs = M.map (locations !!) colours
                 colours = colour nodes edges
                 (nodes, edges) = conflicts prog

lower' :: Map Aloc N.Loc -> Program -> N.Program
lower' locs (Program top) = N.Program $ lowerTop locs top

lowerTop :: Map Aloc N.Loc -> Top -> N.Top
lowerTop locs (Halt triv) = N.Halt triv'
                          where triv' = lowerTriv locs triv
lowerTop locs (Seq stmts top) = N.Seq stmts' top'
                              where stmts' = lowerStmts locs stmts
                                    top' = lowerTop locs top
lowerTop locs (TIf p c a) = N.TIf p' c' a'
                          where p' = lowerPred locs p
                                c' = lowerTop locs c
                                a' = lowerTop locs a

lowerStmts :: Map Aloc N.Loc -> [Stmt] -> [N.Stmt]
lowerStmts locs = L.map (lowerStmt locs)

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
                                c' = lowerStmts locs c
                                a' = lowerStmts locs a

lowerPred :: Map Aloc N.Loc -> Pred -> N.Pred
lowerPred locs (Bool b) = N.Bool b
lowerPred locs (RelOp op aloc triv) = N.RelOp op' loc triv'
                                    where op' = lowerRelOp op
                                          loc = lowerAloc locs aloc
                                          triv' = lowerTriv locs triv
lowerPred locs (Not pred) = N.Not pred'
                          where pred' = lowerPred locs pred
lowerPred locs (PSeq stmts pred) = N.PSeq stmts' pred'
                                 where stmts' = lowerStmts locs stmts
                                       pred' = lowerPred locs pred
lowerPred locs (PIf p c a) = N.PIf p' c' a'
                           where p' = lowerPred locs p
                                 c' = lowerPred locs c
                                 a' = lowerPred locs a

lowerTriv :: Map Aloc N.Loc -> Triv -> N.Triv
lowerTriv locs (Int i) = N.Int i
lowerTriv locs (TAloc aloc) = N.Loc loc
                            where loc = lowerAloc locs aloc

lowerAloc :: Map Aloc N.Loc -> Aloc -> N.Loc
lowerAloc map aloc = map ! aloc

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

conflicts :: Program -> (Set Aloc, Set (Aloc, Aloc))
conflicts (Program top) = conflictSet
                        where (_, conflictSet) = execTardis (topConflicts top)
                                                            (S.empty, (S.empty, S.empty))

type ConflictState a = Tardis (Set Aloc) (Set Aloc, Set (Aloc, Aloc)) a

topConflicts :: Top -> ConflictState ()
topConflicts (Halt triv) = setUndeadTriv triv
topConflicts (Seq stmts top) = stmtsConflicts stmts >> topConflicts top
topConflicts (TIf p c a) = ifConflicts p c a topConflicts

stmtsConflicts :: [Stmt] -> ConflictState ()
stmtsConflicts = (>> return ()) . mapM stmtConflicts

stmtConflicts :: Stmt -> ConflictState ()
stmtConflicts (Set aloc triv) = do
                                setUndeadTriv triv
                                undeadWithoutTriv triv >>= conflict aloc
                                setDeadAloc aloc
stmtConflicts (BinOp _ aloc triv) = do
                                    setUndeadTriv triv
                                    setUndeadAloc aloc
                                    undeadWithoutAloc aloc >>= conflict aloc
stmtConflicts (If p c a) = ifConflicts p c a stmtsConflicts

predConflicts :: Pred -> ConflictState ()
predConflicts (Bool _) = return ()
predConflicts (RelOp _ aloc triv) = setUndeadAloc aloc >> setUndeadTriv triv
predConflicts (Not pred) = predConflicts pred
predConflicts (PIf p c a) = ifConflicts p c a predConflicts
predConflicts (PSeq stmts pred) = stmtsConflicts stmts >> predConflicts pred

ifConflicts :: Pred -> a -> a -> (a -> ConflictState ()) -> ConflictState ()
ifConflicts p c a calcConflicts = mdo
                                  predConflicts p
                                  sendPast (S.union cUndeadIn aUndeadIn)
                                  cUndeadIn <- getFuture
                                  calcConflicts c
                                  sendPast undeadOut
                                  aUndeadIn <- getFuture
                                  calcConflicts a
                                  undeadOut <- getFuture
                                  return ()

conflict :: Aloc -> Set Aloc -> ConflictState ()
conflict aloc set = do
                    (nodes, edges) <- getPast
                    sendFuture ( S.insert aloc nodes
                               , S.foldr (S.insert . (,) aloc)
                                         edges
                                         set )

setDeadAloc :: Aloc -> ConflictState ()
setDeadAloc = modifyBackwards . S.delete

setUndeadAloc :: Aloc -> ConflictState ()
setUndeadAloc = modifyBackwards . S.insert

setUndeadTriv :: Triv -> ConflictState ()
setUndeadTriv (Int _) = return ()
setUndeadTriv (TAloc aloc) = setUndeadAloc aloc

undeadWithoutAloc :: Aloc -> ConflictState (Set Aloc)
undeadWithoutAloc aloc = getFuture >>= return . S.delete aloc

undeadWithoutTriv :: Triv -> ConflictState (Set Aloc)
undeadWithoutTriv (Int _) = getFuture
undeadWithoutTriv (TAloc aloc) = undeadWithoutAloc aloc
