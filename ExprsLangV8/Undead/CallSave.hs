module Undead.CallSave
( callSave
)
where

import Compiler.Locs
import Compiler.Types
import Undead.Patch
import Undead.Types

import Data.Maybe
import qualified Data.Set as S

callSave :: Program -> Program
callSave (Program blocks body) = Program (callSaveBlocks blocks)
                                         (callSaveBody body)

callSaveBlocks :: [Block] -> [Block]
callSaveBlocks = map callSaveBlock

callSaveBlock :: Block -> Block
callSaveBlock (Block label body) = Block label (callSaveBody body)

callSaveBody :: Body -> Body
callSaveBody (Body tail) = Body (callSaveTail tail)

callSaveTail :: Tail -> Tail
callSaveTail (Seq stmts tail) = Seq (callSaveStmts stmts)
                                    (callSaveTail tail)
callSaveTail (TIf p c a) = TIf (callSavePred p)
                               (callSaveTail c)
                               (callSaveTail a)
callSaveTail (Jump place used) = Jump place used

callSaveStmts :: [TStmt] -> [TStmt]
callSaveStmts = concat . map callSaveStmt

callSaveStmt :: TStmt -> [TStmt]
callSaveStmt (undeadOut, stmt@(Set _ _)) = [ (undeadOut, stmt) ]
callSaveStmt (undeadOut, stmt@(BinOp _ _ _)) = [ (undeadOut, stmt) ]
callSaveStmt (undeadOut, stmt@(MSet _ _ _)) = [ (undeadOut, stmt) ]
callSaveStmt (undeadOut, stmt@(MRef _ _ _)) = [ (undeadOut, stmt) ]
callSaveStmt (undeadOut, jmpRet@(JumpRet label used)) = wrapJumpRet undeadOut jmpRet
callSaveStmt (undeadOut, If p c a) = [ ( undeadOut
                                       , If (callSavePred p)
                                            (callSaveStmts c)
                                            (callSaveStmts a) ) ]

callSavePred :: Pred -> Pred
callSavePred pred@(Bool _) = pred
callSavePred pred@(RelOp _ _ _) = pred
callSavePred (Not pred) = Not (callSavePred pred)
callSavePred (PSeq stmts pred) = PSeq (callSaveStmts stmts)
                                      (callSavePred pred)
callSavePred (PIf p c a) = PIf (callSavePred p)
                               (callSavePred c)
                               (callSavePred a)

wrapJumpRet :: S.Set Mloc -> Stmt -> [TStmt]
wrapJumpRet undead jmpRet = patch undead
                                  (concat [ saveAlocs undeadAlocs
                                          , [ BinOp Sub framePointer frameSize
                                            , jmpRet
                                            , BinOp Add framePointer frameSize ]
                                          , loadAlocs undeadAlocs ])
                          where undeadAlocs = (mapMaybe asAloc . S.toList) undead
                                framePointer = (MRloc . Reg) frameRegister
                                frameSize = (MLit . Lit . fromIntegral) ((length undeadAlocs) * 8)

saveAlocs :: [Aloc] -> [Stmt]
saveAlocs undead = zipWith Set
                           (map (MRloc . Addr) stackAddresses)
                           (map (MMloc . MAloc) undead)

loadAlocs :: [Aloc] -> [Stmt]
loadAlocs undead = zipWith Set
                           (map MAloc undead)
                           (map (MMloc . MRloc . Addr) stackAddresses)

asAloc :: Mloc -> Maybe Aloc
asAloc (MAloc aloc) = Just aloc
asAloc (MRloc _) = Nothing
