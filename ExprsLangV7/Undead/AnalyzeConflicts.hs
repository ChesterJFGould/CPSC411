module Undead.AnalyzeConflicts
( analyzeConflicts
)
where

import Compiler.Types
import Undead.Types

import Control.Monad.State
import qualified Data.Set as S

type Conflicts a = State (S.Set Aloc, S.Set (Mloc, Mloc)) a

analyzeConflicts :: Body -> (S.Set Aloc, S.Set (Mloc, Mloc))
analyzeConflicts (Body tail) = execState (analyzeTail tail) (S.empty, S.empty)

analyzeTail :: Tail -> Conflicts ()
analyzeTail (Seq stmts tail) = sequence_ [ analyzeStmts stmts
                                         , analyzeTail tail ]
analyzeTail (TIf p c a) = sequence_ [ analyzePred p
                                    , analyzeTail c
                                    , analyzeTail a ]
analyzeTail (Jump place _) = return ()

analyzeStmts :: [TStmt] -> Conflicts ()
analyzeStmts = mapM_ analyzeStmt

analyzeStmt :: TStmt -> Conflicts ()
analyzeStmt (undeadOut, Set loc triv) = conflict loc (withoutTriv triv undeadOut)
analyzeStmt (undeadOut, BinOp op loc _) = conflict loc undeadOut
analyzeStmt (_, If p c a) = sequence_ [ analyzePred p
                                      , analyzeStmts c
                                      , analyzeStmts a ]
analyzeStmt (_, JumpRet _ _) = return ()

analyzePred :: Pred -> Conflicts ()
analyzePred (Bool _) = return ()
analyzePred (RelOp _ _ _) = return ()
analyzePred (Not pred) = analyzePred pred
analyzePred (PSeq stmts pred) = sequence_ [ analyzeStmts stmts
                                          , analyzePred pred ]
analyzePred (PIf p c a) = mapM_ analyzePred [p, c, a]

conflict :: Mloc -> S.Set Mloc -> Conflicts ()
conflict loc undead = do
                      (nodes, edges) <- get
                      let undeadList = S.toList undead
                          edges' = foldr (S.insert . ((,) loc)) edges undeadList
                          nodes' = case loc of
                                        MAloc aloc -> S.insert aloc nodes
                                        MRloc _ -> nodes
                      put (nodes', edges')
                      return ()

withoutTriv :: MTriv -> S.Set Mloc -> S.Set Mloc
withoutTriv (TMloc loc) = S.delete loc
withoutTriv (MPtr _) = id
