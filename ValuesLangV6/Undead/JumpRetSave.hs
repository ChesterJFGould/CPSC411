module Undead.JumpRetSave
( jumpRetSave
)
where

import Compiler.Types
import Compiler.Values
import Undead.Patch
import Undead.Types

import Control.Monad.Writer
import Data.List as L
import Data.Set as S

type Corrections a = Writer [(Set Aloc, Set (MLoc, MLoc))] a

jumpRetSave :: Program -> Program
jumpRetSave (Program defs body) = Program (L.map correctBlock defs) (correctBody body)

correctBlock :: Block -> Block
correctBlock (Block label body) = Block label (correctBody body)

correctBody :: TBody -> TBody
correctBody (tag, Body body) = (mergeCorrections tag corrections, Body body')
                             where (body', corrections) = runWriter (correctTail body)

mergeCorrections :: (Set Aloc, Set (MLoc, MLoc)) -> [(Set Aloc, Set (MLoc, MLoc))] -> (Set Aloc, Set (MLoc, MLoc))
mergeCorrections acc [] = acc
mergeCorrections (nodes, edges) ((nodes', edges') : rest) = mergeCorrections (nodes'', edges'') rest
                                                          where nodes'' = S.union nodes' nodes
                                                                edges'' = S.union edges' edges

correctTail :: TTail -> Corrections TTail
correctTail (uo, TSeq stmts tail) = tag uo ( TSeq <$> correctStmts stmts
                                                  <*> correctTail tail )
correctTail (uo, TIf p c a) = tag uo ( TIf <$> correctPred p
                                           <*> correctTail c
                                           <*> correctTail a )
correctTail ttail@(uo, Jump _ _) = return ttail

correctStmts :: [TStmt] -> Corrections [TStmt]
correctStmts = (>>= return . concat) . mapM correctStmt

correctStmt :: TStmt -> Corrections [TStmt]
correctStmt stmt@(_,(Set _ _)) = return [ stmt ]
correctStmt stmt@(_, (BinOp _ _ _)) = return [ stmt ]
correctStmt (uo, If p c a) = (pureM . tag uo) ( If <$> correctPred p
                                                   <*> correctStmts c
                                                   <*> correctStmts a )
correctStmt (undeadOut, stmt@(JumpRet _ _)) = patchJumpRet undeadOut stmt

correctPred :: TPred -> Corrections TPred
correctPred pred@(_, (Bool _)) = return pred
correctPred pred@(_, (RelOp _ _ _)) = return pred
correctPred (uo, Not pred) = tag uo ( Not <$> correctPred pred)
correctPred (uo, PSeq stmts pred) = tag uo ( PSeq <$> correctStmts stmts
                                                  <*> correctPred pred )
correctPred (uo, PIf p c a) = tag uo ( PIf <$> correctPred p
                                           <*> correctPred c
                                           <*> correctPred a )

patchJumpRet :: Set MLoc -> Stmt -> Corrections [TStmt]
patchJumpRet undeadOut jumpRet = tell [ corrections ] >> return stmts'
                               where (stmts', corrections) = patchStmts undeadOut stmts
                                     stmts = wrapJumpRet undeadOut jumpRet

wrapJumpRet :: Set MLoc -> Stmt -> [Stmt]
wrapJumpRet undeadOut jumpRet = zipWith Set saveLocations (L.map Loc locs)
                                ++
                                [ BinOp Sub framePointer frameSize
                                , jumpRet
                                , BinOp Add framePointer frameSize ]
                                ++
                                zipWith Set locs (L.map Loc saveLocations)
                              where locs = (S.toList . S.filter isAloc) undeadOut
                                    framePointer = (MRloc . Reg) frameRegister
                                    frameSize = (Int . fromIntegral) (length locs * 8)

isAloc :: MLoc -> Bool
isAloc (MAloc _) = True
isAloc (MRloc _) = False

saveLocations :: [MLoc]
saveLocations = L.map (MRloc . LAddr) generalAddresses

tag :: a -> Corrections b -> Corrections (a, b)
tag t patch = (,) t <$> patch

pureM = fmap pure
