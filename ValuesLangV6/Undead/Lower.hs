module Undead.Lower
( lower
)
where

import Compiler.Types
import Compiler.Values
import Undead.JumpRetSave
import Undead.Types

import qualified Nested.Types as N

import Data.List as L
import Data.Map as M
import Data.Set as S

import Graph.Colour

lower :: Program -> N.Program
lower prog = N.Program (L.map lowerBlock defs) (lowerBody body)
           where (Program defs body) = jumpRetSave prog

lowerBlock :: Block -> N.Block
lowerBlock (Block label body) = N.Block label (lowerBody body)

lowerBody :: TBody -> N.Tail
lowerBody ((nodes, edges), Body body) = lowerTail assignments body
                                 where assignments = colour generalLocations nodes edges'
                                       edges' = S.map locsToNodes edges

locsToNodes :: (MLoc, MLoc) -> (Node Aloc Loc, Node Aloc Loc)
locsToNodes (a, b) = (locToNode a, locToNode b)

locToNode :: MLoc -> Node Aloc Loc
locToNode (MAloc aloc) = Node aloc
locToNode (MRloc rloc) = Colour rloc

lowerTail :: Map Aloc Loc -> TTail -> N.Tail
lowerTail env (_, TSeq stmts tail) = N.TSeq (lowerStmts env stmts) (lowerTail env tail)
lowerTail env (_, TIf p c a) = N.TIf (lowerPred env p) (lowerTail env c) (lowerTail env a)
lowerTail env (_, Jump place _) = N.Jump (lowerPlace env place)

lowerStmts :: Map Aloc Loc -> [TStmt] -> [N.Stmt]
lowerStmts = L.map . lowerStmt

lowerStmt :: Map Aloc Loc -> TStmt -> N.Stmt
lowerStmt env (_, Set loc triv) = N.Set (lowerLoc env loc) (lowerTriv env triv)
lowerStmt env (_, BinOp op loc triv) = N.BinOp op (lowerLoc env loc) (lowerTriv env triv)
lowerStmt env (_, If p c a) = N.If (lowerPred env p) (lowerStmts env c) (lowerStmts env a)
lowerStmt env (_, JumpRet label _) = N.JumpRet label

lowerPred :: Map Aloc Loc -> TPred -> N.Pred
lowerPred env (_, Bool b) = N.Bool b
lowerPred env (_, RelOp op loc triv) = N.RelOp op (lowerLoc env loc) (lowerTriv env triv)
lowerPred env (_, Not pred) = N.Not (lowerPred env pred)
lowerPred env (_, PSeq stmts pred) = N.PSeq (lowerStmts env stmts) (lowerPred env pred)
lowerPred env (_, PIf p c a) = N.PIf (lowerPred env p) (lowerPred env c) (lowerPred env a)

lowerTriv :: Map Aloc Loc -> Triv -> N.Triv
lowerTriv env (Int i) = N.Int i
lowerTriv env (Loc loc) = N.Loc (lowerLoc env loc)

lowerLoc :: Map Aloc Loc -> MLoc -> Loc
lowerLoc env (MAloc aloc) = env ! aloc
lowerLoc env (MRloc loc) = loc

lowerPlace :: Map Aloc Loc -> Place -> N.Place
lowerPlace env (PLabel label) = N.PLabel label
lowerPlace env (PLoc loc) = N.PLoc (lowerLoc env loc)
