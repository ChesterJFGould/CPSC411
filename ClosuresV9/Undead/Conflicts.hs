module Undead.Conflicts
( conflicts
)
where

import Compiler.Types
import Undead.Types

import Control.Monad.State
import qualified Data.Set as S

type Conflict = State (S.Set Aloc, S.Set (Mloc, Mloc))

conflicts :: Body -> (S.Set Aloc, S.Set (Mloc, Mloc))
conflicts (Body tail) = execState (conflictsTail tail) (S.empty, S.empty)

conflictsTail :: Tail -> Conflict ()
conflictsTail (Jump _ _) = return ()
conflictsTail (TSeq stmts tail) = do
                                  conflictsStmts stmts
                                  conflictsTail tail
conflictsTail (TIf p c a) = do
                            conflictsPred p
                            conflictsTail c
                            conflictsTail a

conflictsStmts :: [TStmt] -> Conflict ()
conflictsStmts stmts = do
                       mapM conflictsStmt stmts
                       return ()

conflictsStmt :: TStmt -> Conflict ()
conflictsStmt (undeadOut, Set loc triv) = conflictWith loc (withoutTriv triv undeadOut)
conflictsStmt (undeadOut, NumOp _ loc _) = conflictWith loc undeadOut
conflictsStmt (undeadOut, MRef loc _ _) = conflictWith loc undeadOut
conflictsStmt (_, If p c a) = do
                              conflictsPred p
                              conflictsStmts c
                              conflictsStmts c
conflictsStmt _ = return ()

conflictsPred :: Pred -> Conflict ()
conflictsPred (PSeq stmts pred) = do
                                  conflictsStmts stmts
                                  conflictsPred pred
conflictsPred (PIf p c a) = do
                            conflictsPred p
                            conflictsPred c
                            conflictsPred a
conflictsPred _ = return ()

conflictWith :: Mloc -> S.Set Mloc -> Conflict ()
conflictWith loc undead = do
                          (nodes, edges) <- get
                          let nodes' = case loc of
                                            MAloc aloc -> S.insert aloc nodes
                                            _ -> nodes
                              newConflicts = S.map (\loc' -> (loc, loc')) undead
                              edges' = S.union edges newConflicts
                          put (nodes', edges')

withoutTriv :: MTriv -> S.Set Mloc -> S.Set Mloc
withoutTriv (MMloc mloc) undead = S.delete mloc undead
withoutTriv _ undead = undead
