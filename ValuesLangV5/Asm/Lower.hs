{-# LANGUAGE RecursiveDo #-}

module Asm.Lower
( lower
)
where

import Asm.Types

import qualified Nested.Types as N

import qualified Graph.Colour as G

import Control.Monad.Tardis
import Data.List as L
import Data.Map as M
import Data.Set as S

lower :: Program -> N.Program
lower (Program defs body) = N.Program (L.map lowerBlock defs) (lowerBody body)

lowerBlock :: Block -> N.Block
lowerBlock (Block label body) = N.Block (lowerLabel label) (lowerBody body)

lowerBody :: Tail -> N.Tail
lowerBody body = lowerTail locs body
               where locs = G.colour nodes edges locations
                     edges = S.map conflictToEdge conflicts
                     (_, (conflicts, nodes)) = execTardis (conflictsTail body)
                                                          (S.empty, (S.empty, S.empty))

conflictToEdge :: (Loc, Loc) -> (G.CNode Aloc N.Loc, G.CNode Aloc N.Loc)
conflictToEdge (a, b) = (convert a, convert b)
                      where convert (LAloc aloc) = G.Node aloc
                            convert (LRloc loc) = (G.Colour . lowerRloc) loc

locations :: [N.Loc]
locations = L.map N.Reg [ N.RDI
                        , N.RSI
                        , N.RDX
                        , N.RCX
                        , N.R8
                        , N.R9
                        , N.RSP
                        , N.RAX
                        , N.RBX
                        , N.R12
                        , N.R13
                        , N.R14
                        , N.R15 ]
            ++ L.map N.Addr [0..]

lowerTail :: Map Aloc N.Loc -> Tail -> N.Tail
lowerTail env (Halt triv) = N.Halt (lowerTriv env triv)
lowerTail env (TSeq stmts tail) = N.TSeq (lowerStmts env stmts) (lowerTail env tail)
lowerTail env (TIf p c a) = lowerIf env lowerTail N.TIf p c a
lowerTail env (Jump label _) = N.Jump (lowerLabel label)

lowerStmts :: Map Aloc N.Loc -> [Stmt] -> [N.Stmt]
lowerStmts = L.map . lowerStmt

lowerStmt :: Map Aloc N.Loc -> Stmt -> N.Stmt
lowerStmt env (Set loc triv) = N.Set (lowerLoc env loc) (lowerTriv env triv)
lowerStmt env (BinOp op loc triv) = N.BinOp (lowerOp op)
                                            (lowerLoc env loc)
                                            (lowerTriv env triv)
lowerStmt env (If p c a) = lowerIf env lowerStmts N.If p c a

lowerPred :: Map Aloc N.Loc -> Pred -> N.Pred
lowerPred env (Bool b) = N.Bool b
lowerPred env (RelOp op loc triv) = N.RelOp (lowerRelOp op)
                                            (lowerLoc env loc)
                                            (lowerTriv env triv)
lowerPred env (Not pred) = N.Not (lowerPred env pred)
lowerPred env (PSeq stmts pred) = N.PSeq (lowerStmts env stmts) (lowerPred env pred)
lowerPred env (PIf p c a) = lowerIf env lowerPred N.PIf p c a

lowerIf :: Map Aloc N.Loc -> (Map Aloc N.Loc -> a -> b) -> (N.Pred -> b -> b -> c)
           -> Pred -> a -> a -> c
lowerIf env lowerer cons p c a = cons (lowerPred env p)
                                      (lowerer env c)
                                      (lowerer env a)

lowerTriv :: Map Aloc N.Loc -> Triv -> N.Triv
lowerTriv env (Int i) = N.Int i
lowerTriv env (Loc loc) = N.Loc (lowerLoc env loc)

lowerLoc :: Map Aloc N.Loc -> Loc -> N.Loc
lowerLoc env (LAloc aloc) = lowerAloc env aloc
lowerLoc env (LRloc loc) = lowerRloc loc

lowerAloc :: Map Aloc N.Loc -> Aloc -> N.Loc
lowerAloc = (!)

lowerRloc :: Rloc -> N.Loc
lowerRloc (Reg reg) = N.Reg (lowerReg reg)
lowerRloc (Addr i) = N.Addr i

lowerLabel :: Label -> N.Label
lowerLabel (Label l) = N.Label l

lowerReg :: Reg -> N.Reg
lowerReg RSP = N.RSP
lowerReg RBP = N.RBP
lowerReg RAX = N.RAX
lowerReg RBX = N.RBX
lowerReg RCX = N.RCX
lowerReg RDX = N.RDX
lowerReg RSI = N.RSI
lowerReg RDI = N.RDI
lowerReg R8 = N.R8
lowerReg R9 = N.R9
lowerReg R10 = N.R10
lowerReg R11 = N.R11
lowerReg R12 = N.R12
lowerReg R13 = N.R13
lowerReg R14 = N.R14
lowerReg R15 = N.R15

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

type Conflict a = Tardis (Set Loc) (Set (Loc, Loc), Set Aloc) a

conflictsTail :: Tail -> Conflict ()
conflictsTail (Halt triv) = setUndeadTriv triv
conflictsTail (TSeq stmts tail) = conflictsStmts stmts >> conflictsTail tail
conflictsTail (TIf p c a) = conflictsIf conflictsTail p c a
conflictsTail (Jump label undeadOut) = mapM setUndeadLoc undeadOut >> return ()

conflictsStmts :: [Stmt] -> Conflict ()
conflictsStmts = (>> return ()) . mapM conflictsStmt

conflictsStmt :: Stmt -> Conflict ()
conflictsStmt (Set loc triv) = do
                               setUndeadTriv triv
                               undeadOutWithoutTriv triv >>= conflictsWith loc
                               setDeadLoc loc
                               takeNoteOf loc
conflictsStmt (BinOp _ loc triv) = do
                                   setUndeadTriv triv
                                   setUndeadLoc loc
                                   undeadOutWithoutLoc loc >>= conflictsWith loc
conflictsStmt (If p c a) = conflictsIf conflictsStmts p c a

conflictsPred :: Pred -> Conflict ()
conflictsPred (Bool _) = return ()
conflictsPred (RelOp _ loc triv) = setUndeadLoc loc >> setUndeadTriv triv
conflictsPred (Not pred) = conflictsPred pred
conflictsPred (PSeq stmts pred) = conflictsStmts stmts >> conflictsPred pred
conflictsPred (PIf p c a) = conflictsIf conflictsPred p c a

conflictsIf :: (a -> Conflict ()) -> Pred -> a -> a -> Conflict ()
conflictsIf conflicts p c a = mdo
                              conflictsPred p
                              sendPast (S.union cUndeadIn aUndeadIn)
                              cUndeadIn <- getFuture
                              conflicts c
                              sendPast undeadOut
                              aUndeadIn <- getFuture
                              conflicts a
                              undeadOut <- getFuture
                              return ()

conflictsWith :: Loc -> Set Loc -> Conflict ()
conflictsWith loc conflicts = do
                              (edges, nodes) <- getPast
                              let edges' = (S.union edges . S.map ((,) loc)) conflicts
                              sendFuture (edges', nodes)

setUndeadLoc :: Loc -> Conflict ()
setUndeadLoc = modifyBackwards . S.insert

setUndeadTriv :: Triv -> Conflict ()
setUndeadTriv (Int _) = return ()
setUndeadTriv (Loc loc) = setUndeadLoc loc

setDeadLoc :: Loc -> Conflict ()
setDeadLoc = modifyBackwards . S.delete

undeadOutWithoutLoc :: Loc -> Conflict (Set Loc)
undeadOutWithoutLoc = getsFuture . S.delete

undeadOutWithoutTriv :: Triv -> Conflict (Set Loc)
undeadOutWithoutTriv (Int _) = getFuture
undeadOutWithoutTriv (Loc loc) = undeadOutWithoutLoc loc

takeNoteOf :: Loc -> Conflict ()
takeNoteOf (LAloc aloc) = do
                          (edges, nodes) <- getPast
                          let nodes' = S.insert aloc nodes
                          sendFuture (edges, nodes')
takeNoteOf (LRloc _) = return ()
