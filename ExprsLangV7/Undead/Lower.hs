module Undead.Lower
( lower
)
where

import qualified Compiler.Locs as Locs
import Compiler.Types
import qualified Graph.Colour as Graph
import Undead.AnalyzeConflicts
import Undead.CallSave
import Undead.Types

import qualified Nested.Types as N

import Data.List as L
import Data.Map as M
import Data.Set as S

lower :: Program -> N.Program
lower = lowerProgram . callSave

lowerProgram :: Program -> N.Program
lowerProgram (Program blocks body) = N.Program (L.map lowerBlock blocks) (lowerBody body)

lowerBlock :: Block -> N.Block
lowerBlock (Block label body) = N.Block label (lowerBody body)

lowerBody :: Body -> N.Body
lowerBody body@(Body tail) = N.Body (lowerTail context tail)
                           where (nodes, edges) = analyzeConflicts body
                                 edges' = S.map locsToNode edges
                                 context = Graph.colour nodes edges' Locs.general

locsToNode :: (Mloc, Mloc) -> (Graph.Node Aloc Rloc, Graph.Node Aloc Rloc)
locsToNode (a, b) = (locToNode a, locToNode b)

locToNode :: Mloc -> Graph.Node Aloc Rloc
locToNode (MAloc aloc) = Graph.Node aloc
locToNode (MRloc rloc) = Graph.Colour rloc

lowerTail :: Map Aloc Rloc -> Tail -> N.Tail
lowerTail context (Seq stmts tail) = N.Seq (lowerStmts context stmts) (lowerTail context tail)
lowerTail context (TIf p c a) = N.TIf (lowerPred context p) (lowerTail context c) (lowerTail context a)
lowerTail context (Jump place _) = N.Jump (lowerPlace context place)

lowerStmts :: Map Aloc Rloc -> [TStmt] -> [N.Stmt]
lowerStmts context = L.map (lowerStmt context . snd)

lowerStmt :: Map Aloc Rloc -> Stmt -> N.Stmt
lowerStmt context (Set loc triv) = N.Set (lowerLoc context loc) (lowerTriv context triv)
lowerStmt context (BinOp op loc triv) = N.BinOp op (lowerLoc context loc) (lowerTriv context triv)
lowerStmt context (If p c a) = N.If (lowerPred context p) (lowerStmts context c) (lowerStmts context a)
lowerStmt context (JumpRet label _) = N.JumpRet label

lowerPred :: Map Aloc Rloc -> Pred -> N.Pred
lowerPred context (Bool b) = N.Bool b
lowerPred context (RelOp op loc triv) = N.RelOp op (lowerLoc context loc) (lowerTriv context triv)
lowerPred context (Not pred) = N.Not (lowerPred context pred)
lowerPred context (PSeq stmts pred) = N.PSeq (lowerStmts context stmts) (lowerPred context pred)
lowerPred context (PIf p c a) = N.PIf (lowerPred context p) (lowerPred context c) (lowerPred context a)

lowerLoc :: Map Aloc Rloc -> Mloc -> Rloc
lowerLoc context (MAloc aloc) = context ! aloc
lowerLoc context (MRloc rloc) = rloc

lowerTriv :: Map Aloc Rloc -> MTriv -> RTriv
lowerTriv context (TMloc mloc) = TRloc (lowerLoc context mloc)
lowerTriv context (MPtr ptr) = RPtr ptr

lowerPlace :: Map Aloc Rloc -> MPlace -> RPlace
lowerPlace context (PMloc mloc) = PRloc (lowerLoc context mloc)
lowerPlace context (MLabel label) = RLabel label
