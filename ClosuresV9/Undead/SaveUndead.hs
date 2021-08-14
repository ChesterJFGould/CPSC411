module Undead.SaveUndead
( saveUndead
)
where

import qualified Compiler.Locations as Locations
import Compiler.Types
import Undead.Types

import Control.Monad.RevState
import Data.Maybe
import qualified Data.Set as S

type Patch = State (S.Set Mloc)

saveUndead :: Program -> Program
saveUndead (Program labels blocks body) = Program labels (saveBlocks blocks)
                                                         (saveBody body)

saveBlocks :: [Block] -> [Block]
saveBlocks = map saveBlock

saveBlock :: Block -> Block
saveBlock (Block label body) = Block label (saveBody body)

saveBody :: Body -> Body
saveBody (Body tail) = Body (saveTail tail)

saveTail :: Tail -> Tail
saveTail (Jump triv used) = Jump triv used
saveTail (TSeq stmts tail) = TSeq (saveStmts stmts)
                                  (saveTail tail)
saveTail (TIf p c a) = TIf (savePred p)
                           (saveTail c)
                           (saveTail a)

saveStmts :: [TStmt] -> [TStmt]
saveStmts = concat . map saveStmt

saveStmt :: TStmt -> [TStmt]
saveStmt (undeadOut, jumpRetStmt@(JumpRet _ _)) = let alocs = mapMaybe (mlocToAloc)
                                                                       (S.toList undeadOut)
                                                      saveUndeadStmts = saveAlocs alocs
                                                      loadUndeadStmts = loadAlocs alocs
                                                      frameSize = 8 * (length alocs)
                                                      frameSizeTriv = (MLit (Lit (fromIntegral frameSize)))
                                                      framePointer = (MRloc . Reg) Locations.frameRegister
                                                      stmts = saveUndeadStmts
                                                              ++
                                                              [ NumOp Sub framePointer frameSizeTriv
                                                              , jumpRetStmt
                                                              , NumOp Add framePointer frameSizeTriv
                                                              ]
                                                              ++
                                                              loadUndeadStmts
                                                      patchedStmts = patch undeadOut stmts
                                                   in patchedStmts
saveStmt (undeadOut, If p c a) = [ ( undeadOut
                                   , If (savePred p)
                                        (saveStmts c)
                                        (saveStmts a)
                                   )
                                 ]
saveStmt stmt = [ stmt ]

savePred :: Pred -> Pred
savePred (Not pred) = Not (savePred pred)
savePred (PSeq stmts pred) = PSeq (saveStmts stmts)
                                  (savePred pred)
savePred (PIf p c a) = PIf (savePred p)
                           (savePred c)
                           (savePred a)
savePred pred = pred

mlocToAloc :: Mloc -> Maybe Aloc
mlocToAloc (MAloc aloc) = Just aloc
mlocToAloc _ = Nothing

saveAlocs :: [Aloc] -> [Stmt]
saveAlocs alocs = let saveMlocs = map MRloc Locations.saveLocations
                      alocTrivs = map (MMloc . MAloc) alocs
                      saveAlocStmts = zipWith Set saveMlocs alocTrivs
                  in saveAlocStmts

loadAlocs :: [Aloc] -> [Stmt]
loadAlocs alocs = let saveTrivs = map (MMloc . MRloc) Locations.saveLocations
                      alocMlocs = map MAloc alocs
                      loadAlocStmts = zipWith Set alocMlocs saveTrivs
                  in loadAlocStmts

patch :: S.Set Mloc -> [Stmt] -> [TStmt]
patch undeadOut stmts = evalState (patchStmts stmts) undeadOut

patchStmts :: [Stmt] -> Patch [TStmt]
patchStmts = mapM patchStmt

patchStmt :: Stmt -> Patch TStmt
patchStmt (Set loc triv) = do
                           setTrivUndead triv
                           setLocDead loc
                           wrapUndead (Set loc triv)
patchStmt (NumOp op loc triv) = do
                                setTrivUndead triv
                                setLocUndead loc
                                wrapUndead (NumOp op loc triv)
patchStmt (MRef loc ptr offset) = do
                                  setTrivUndead offset
                                  setTrivUndead ptr
                                  setLocDead loc
                                  wrapUndead (MRef loc ptr offset)
patchStmt (MSet ptr offset val) = do
                                  setTrivUndead val
                                  setTrivUndead offset
                                  setTrivUndead ptr
                                  wrapUndead (MSet ptr offset val)
patchStmt (JumpRet triv used) = do
                                mapM setLocUndead used
                                setTrivUndead triv
                                wrapUndead (JumpRet triv used)
patchStmt stmt = error "Attempted to patch non-trivial statement"

setLocUndead :: Mloc -> Patch ()
setLocUndead = modify . S.insert

setTrivUndead :: MTriv -> Patch ()
setTrivUndead (MMloc mloc) = setLocUndead mloc
setTrivUndead _ = return ()

setLocDead :: Mloc -> Patch ()
setLocDead = modify . S.delete

wrapUndead :: Stmt -> Patch TStmt
wrapUndead stmt = do
                  undead <- get
                  return (undead, stmt)
