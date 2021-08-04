module Undead.Conflicts
( conflicts
)
where

import Compiler.Types
import Undead.Types

import Control.Monad.State
import Data.Set as S

type Conflicts a = State (Set Aloc, Set (Mloc, Mloc)) a

conflicts :: Body -> (Set Aloc, Set (Mloc, Mloc))
conflicts (Body tail) = execState (conflictsTail tail) (S.empty, S.empty)

conflictsTail :: Tail -> Conflicts ()
conflictsTail (Seq stmts tail) = do
                                 conflictsStmts stmts
                                 conflictsTail tail
conflictsTail (TIf p c a) = do
                            conflictsPred p
                            conflictsTail c
                            conflictsTail a
conflictsTail (Jump _ _) = return ()

conflictsStmts :: [TStmt] -> Conflicts ()
conflictsStmts = (>> return ()) . mapM conflictsStmt

conflictsStmt :: TStmt -> Conflicts ()
conflictsStmt (undeadOut, Set loc triv) = conflictWith loc (removeTriv triv undeadOut)
conflictsStmt (undeadOut, BinOp _ loc triv) = conflictWith loc undeadOut
conflictsStmt (_, MSet _ _ _) = return ()
conflictsStmt (undeadOut, MRef loc ptr offset) = conflictWith loc undeadOut
conflictsStmt (undeadOut, JumpRet _ _) = return ()
conflictsStmt (_, If p c a) = do
                              conflictsPred p
                              conflictsStmts c
                              conflictsStmts a

conflictsPred :: Pred -> Conflicts ()
conflictsPred (Bool _) = return ()
conflictsPred (RelOp _ _ _) = return ()
conflictsPred (Not pred) = conflictsPred pred
conflictsPred (PSeq stmts pred) = do
                                  conflictsStmts stmts
                                  conflictsPred pred
conflictsPred (PIf p c a) = do
                            conflictsPred p
                            conflictsPred c
                            conflictsPred a

removeTriv :: MTriv -> Set Mloc -> Set Mloc
removeTriv (MMloc mloc) undead = S.delete mloc undead
removeTriv _ undead = undead

conflictWith :: Mloc -> Set Mloc -> Conflicts ()
conflictWith loc undead = do
                          (nodes, edges) <- get
                          let edges' = S.foldr (S.insert . (,) loc) edges undead
                              nodes' = case loc of
                                            MAloc aloc -> S.insert aloc nodes
                                            MRloc _ -> nodes
                          put (nodes', edges')
