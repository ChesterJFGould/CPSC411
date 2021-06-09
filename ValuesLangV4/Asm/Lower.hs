{-# LANGUAGE RecursiveDo #-}

module Asm.Lower
( lower
)
where

import Asm.Types

import qualified Nested.Types as N

import Control.Monad.Tardis
import Data.List as L
import Data.Map as M
import Data.Set as S

lower :: Program -> N.Program
lower (Program top) = N.Program $ evalTardis (lowerTop top) (S.empty, M.empty)

lowerTop :: Top -> Tardis (Set Aloc) (Map Aloc N.Loc) N.Top
lowerTop (Halt triv) = do
                       case triv of
                            TAloc aloc -> modifyBackwards (S.insert aloc)
                            _ -> return ()
                       triv' <- lowerTriv triv
                       return $ N.Halt triv'
lowerTop (TIf p c a) = lowerIf p c a lowerTop N.TIf
lowerTop (Seq stmts top) = do
                           stmts' <- lowerStmts stmts
                           top' <- lowerTop top
                           return $ N.Seq stmts' top'

lowerStmts :: [Stmt] -> Tardis (Set Aloc) (Map Aloc N.Loc) [N.Stmt]
lowerStmts stmts = mapM lowerStmt stmts

lowerStmt :: Stmt -> Tardis (Set Aloc) (Map Aloc N.Loc) N.Stmt
lowerStmt (Set aloc triv) = do
                            case triv of
                                 TAloc aloc -> modifyBackwards (S.insert aloc)
                                 _ -> return ()
                            undeadOut <- getFuture
                            loc <- nextLoc
                            modifyBackwards (S.delete aloc)
                            triv' <- lowerTriv triv
                            modifyForwards (M.insert aloc loc)
                            return $ N.Set loc triv'
lowerStmt (If p c a) = lowerIf p c a lowerStmts N.If
lowerStmt (BinOp op aloc triv) = do
                                 modifyBackwards (S.insert aloc)
                                 case triv of
                                      TAloc aloc -> modifyBackwards (S.insert aloc)
                                      _ -> return ()
                                 loc <- lowerAloc aloc
                                 triv' <- lowerTriv triv
                                 return $ N.BinOp (lowerOp op) loc triv'

lowerPred :: Pred -> Tardis (Set Aloc) (Map Aloc N.Loc) N.Pred
lowerPred (Bool b) = return $ N.Bool b
lowerPred (RelOp op aloc triv) = do
                                 modifyBackwards (S.insert aloc)
                                 case triv of
                                      TAloc aloc -> modifyBackwards (S.insert aloc)
                                      _ -> return ()
                                 loc <- lowerAloc aloc
                                 triv' <- lowerTriv triv
                                 return $ N.RelOp (lowerRelOp op) loc triv'
lowerPred (Not pred) = lowerPred pred >>= return . N.Not
lowerPred (PIf p c a) = lowerIf p c a lowerPred N.PIf
lowerPred (PSeq stmts body) = do
                              stmts' <- lowerStmts stmts
                              body' <- lowerPred body
                              return $ N.PSeq stmts' body'

lowerIf :: Pred -> a -> a -> (a -> Tardis (Set Aloc) (Map Aloc N.Loc) b)
           -> (N.Pred -> b -> b -> c) -> Tardis (Set Aloc) (Map Aloc N.Loc) c
lowerIf p c a lowerBody consIf = mdo
                                 p' <- lowerPred p
                                 sendPast (S.union cUndeadIn aUndeadIn)
                                 cUndeadIn <- getFuture
                                 c' <- lowerBody c
                                 sendPast undeadOut
                                 aUndeadIn <- getFuture
                                 a' <- lowerBody a
                                 undeadOut <- getFuture
                                 return $ consIf p' c' a'

lowerTriv :: Triv -> Tardis (Set Aloc) (Map Aloc N.Loc) N.Triv
lowerTriv (Int i) = return $ N.Int i
lowerTriv (TAloc aloc) = lowerAloc aloc >>= return . N.Loc

lowerAloc :: Aloc -> Tardis (Set Aloc) (Map Aloc N.Loc) N.Loc
lowerAloc aloc = getPast >>= return . (! aloc)

lowerOp :: Op -> N.Op
lowerOp Add = N.Add
lowerOp Mul = N.Mul

lowerRelOp :: RelOp -> N.RelOp
lowerRelOp Lt = N.Lt
lowerRelOp Gt = N.Gt
lowerRelOp Eq = N.Eq
lowerRelOp Lte = N.Lte
lowerRelOp Gte = N.Gte
lowerRelOp Neq = N.Neq

locations :: [N.Loc]
locations = L.map N.Reg [ N.RSP
                        , N.RAX
                        , N.RBX
                        , N.RCX
                        , N.RDX
                        , N.RSI
                        , N.RDI
                        , N.R8
                        , N.R9
                        , N.R12
                        , N.R13
                        , N.R14
                        , N.R15 ]
            ++ L.map N.Addr [0..]

firstNotIn :: [N.Loc] -> Set N.Loc -> N.Loc
firstNotIn (loc : rest) locs
           | loc `S.member` locs = firstNotIn rest locs
           | otherwise = loc

nextLoc :: Tardis (Set Aloc) (Map Aloc N.Loc) N.Loc
nextLoc = do
          undeadIn <- getFuture
          env <- getPast
          return $ firstNotIn locations (S.map (env !) undeadIn)
