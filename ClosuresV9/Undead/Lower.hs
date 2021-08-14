module Undead.Lower
( lower
)
where

import qualified Compiler.Locations as Locations
import Compiler.Types
import qualified Graph.Colour as G
import qualified Nested.Types as N
import Undead.Conflicts
import Undead.SaveUndead
import Undead.Types

import Control.Monad.Reader
import qualified Data.Map as M
import qualified Data.Set as S

type Register = Reader (M.Map Aloc Rloc)

lower :: Program -> N.Program
lower = lowerProg . saveUndead

lowerProg :: Program -> N.Program
lowerProg (Program labels blocks body) = N.Program labels (lowerBlocks blocks)
                                                          (lowerBody body)

lowerBlocks :: [Block] -> [N.Block]
lowerBlocks = map lowerBlock

lowerBlock :: Block -> N.Block
lowerBlock (Block label body) = N.Block label (lowerBody body)

lowerBody :: Body -> N.Body
lowerBody body@(Body tail) = let (nodes, edges) = conflicts body
                                 edges' = S.map conflictsToNodes edges
                                 assignments = G.colour nodes edges' Locations.localLocations
                             in N.Body (runReader (lowerTail tail) assignments)

lowerTail :: Tail -> Register N.Tail
lowerTail (Jump triv _) = N.Jump <$> lowerTriv triv
lowerTail (TSeq stmts tail) = N.TSeq <$> lowerStmts stmts
                                     <*> lowerTail tail
lowerTail (TIf p c a) = N.TIf <$> lowerPred p
                              <*> lowerTail c
                              <*> lowerTail a

lowerStmts :: [TStmt] -> Register [N.Stmt]
lowerStmts = mapM lowerStmt

lowerStmt :: TStmt -> Register N.Stmt
lowerStmt (_, Set loc triv) = N.Set <$> lowerLoc loc
                                    <*> lowerTriv triv
lowerStmt (_, NumOp op loc triv) = N.NumOp op <$> lowerLoc loc
                                              <*> lowerTriv triv
lowerStmt (_, MRef loc ptr offset) = N.MRef <$> lowerLoc loc
                                            <*> lowerTriv ptr
                                            <*> lowerTriv offset
lowerStmt (_, MSet ptr offset val) = N.MSet <$> lowerTriv ptr
                                            <*> lowerTriv offset
                                            <*> lowerTriv val
lowerStmt (_, JumpRet triv _) = N.JumpRet <$> lowerTriv triv
lowerStmt (_, If p c a) = N.If <$> lowerPred p
                               <*> lowerStmts c
                               <*> lowerStmts a

lowerPred :: Pred -> Register N.Pred
lowerPred (Bool b) = return (N.Bool b)
lowerPred (RelOp op loc triv) = N.RelOp op <$> lowerLoc loc
                                           <*> lowerTriv triv
lowerPred (Not pred) = N.Not <$> lowerPred pred
lowerPred (PSeq stmts pred) = N.PSeq <$> lowerStmts stmts
                                     <*> lowerPred pred
lowerPred (PIf p c a) = N.PIf <$> lowerPred p
                              <*> lowerPred c
                              <*> lowerPred a

lowerTriv :: MTriv -> Register RTriv
lowerTriv (MLit lit) = return (RLit lit)
lowerTriv (MMloc mloc) = RRloc <$> lowerLoc mloc
lowerTriv (MLabel label) = return (RLabel label)

lowerLoc :: Mloc -> Register Rloc
lowerLoc (MRloc rloc) = return rloc
lowerLoc (MAloc aloc) = do
                        assignments <- ask
                        return (assignments M.! aloc)

conflictsToNodes :: (Mloc, Mloc) -> (G.Node Aloc Rloc, G.Node Aloc Rloc)
conflictsToNodes (a, b) = (conflictToNode a, conflictToNode b)

conflictToNode :: Mloc -> G.Node Aloc Rloc
conflictToNode (MAloc aloc) = G.Node aloc
conflictToNode (MRloc rloc) = G.Colour rloc
