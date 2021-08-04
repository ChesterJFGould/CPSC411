module Undead.Lower
( lower
)
where

import Compiler.Locs
import Compiler.Types
import qualified Graph.Colour as G
import qualified Nested.Types as N
import Undead.CallSave
import Undead.Conflicts
import Undead.Types

import Control.Monad.Reader
import qualified Data.Set as S
import qualified Data.Map as M

type Assignments a = Reader (M.Map Aloc Rloc) a

lower :: Program -> N.Program
lower = lowerProg . callSave

lowerProg :: Program -> N.Program
lowerProg (Program blocks body) = N.Program (lowerBlocks blocks)
                                            (lowerBody body)

lowerBlocks :: [Block] -> [N.Block]
lowerBlocks = map lowerBlock

lowerBlock :: Block -> N.Block
lowerBlock (Block label body) = N.Block label (lowerBody body)

lowerBody :: Body -> N.Body
lowerBody body@(Body tail) = N.Body (runReader (lowerTail tail) assignments)
                           where (nodes, edges) = conflicts body
                                 edges' = S.map locsToNodes edges
                                 assignments = G.colour nodes edges' localLocations

locsToNodes :: (Mloc, Mloc) -> (G.Node Aloc Rloc, G.Node Aloc Rloc)
locsToNodes (a, b) = (locToNode a, locToNode b)

locToNode :: Mloc -> G.Node Aloc Rloc
locToNode (MAloc aloc) = G.Node aloc
locToNode (MRloc rloc) = G.Colour rloc

lowerTail :: Tail -> Assignments N.Tail
lowerTail (Seq stmts tail) = N.Seq <$> lowerStmts stmts
                                   <*> lowerTail tail
lowerTail (TIf p c a) = N.TIf <$> lowerPred p
                              <*> lowerTail c
                              <*> lowerTail a
lowerTail (Jump place _) = N.Jump <$> lowerPlace place

lowerStmts :: [TStmt] -> Assignments [N.Stmt]
lowerStmts = mapM lowerStmt

lowerStmt :: TStmt -> Assignments N.Stmt
lowerStmt (_, Set loc triv) = N.Set <$> lowerLoc loc
                                    <*> lowerTriv triv
lowerStmt (_, BinOp op loc triv) = N.BinOp op <$> lowerLoc loc
                                              <*> lowerTriv triv
lowerStmt (_, MSet ptr offset val) = N.MSet <$> lowerTriv ptr
                                            <*> lowerTriv offset
                                            <*> lowerTriv val
lowerStmt (_, MRef loc ptr offset) = N.MRef <$> lowerLoc loc
                                            <*> lowerTriv ptr
                                            <*> lowerTriv offset
lowerStmt (_, JumpRet label _) = return (N.JumpRet label)
lowerStmt (_, If p c a) = N.If <$> lowerPred p
                               <*> lowerStmts c
                               <*> lowerStmts a

lowerPred :: Pred -> Assignments N.Pred
lowerPred (Bool b) = return (N.Bool b)
lowerPred (RelOp op loc triv) = N.RelOp op <$> lowerLoc loc
                                           <*> lowerTriv triv
lowerPred (Not pred) = N.Not <$> lowerPred pred
lowerPred (PSeq stmts pred) = N.PSeq <$> lowerStmts stmts
                                     <*> lowerPred pred
lowerPred (PIf p c a) = N.PIf <$> lowerPred p
                              <*> lowerPred c
                              <*> lowerPred a

lowerLoc :: Mloc -> Assignments Rloc
lowerLoc (MAloc aloc) = do
                        assignments <- ask
                        return (assignments M.! aloc)
lowerLoc (MRloc rloc) = return rloc

lowerTriv :: MTriv -> Assignments RTriv
lowerTriv (MMloc loc) = RRloc <$> lowerLoc loc
lowerTriv (MLit lit) = return (RLit lit)

lowerPlace :: MPlace -> Assignments RPlace
lowerPlace (PMloc loc) = PRloc <$> lowerLoc loc
lowerPlace (MLabel label) = return (PLabel label)
