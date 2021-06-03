module Asm.Lower
( lower
)
where

import Asm.Types

import qualified Para.Types as P

import Control.Monad.State
import Control.Monad.Writer
import Data.Map as M hiding (map)
import Data.Set as S

lower :: Program -> P.Program
lower (Program stmts out) = P.Program (map (lowerStmt alocMap) stmts)
                                                   (lowerTriv alocMap out)

lowerStmt :: Map Aloc Int -> Stmt -> P.Stmt
lowerStmt alocMap (Stmt op aloc triv) = P.Stmt (lowerOp op)
                                               (lowerAloc alocMap aloc)
                                               (lowerTriv alocMap triv)

lower' :: (Stmt, Set Aloc) -> WriterT [Stmt] (State (Map Aloc P.Loc)) ()
lower' (Stmt Set aloc triv, undeadOut) = do
	                                 loc <- nextLoc undeadOut -- Put aloc into next unused loc
	                                 triv' <- lowerTriv triv
	                                 lift $ modify (M.insert aloc loc) -- Associate aloc with loc in the env
	                                 return $ Stmt P.Set loc triv'


lowerOp :: Op -> P.Op
lowerOp Set = P.Set
lowerOp Add = P.Add
lowerOp Mul = P.Mul

lowerTriv :: Map Aloc Int -> Triv -> P.Triv
lowerTriv _ (Int i) = P.Int i
lowerTriv alocMap (Aloc aloc) = P.Loc $ lowerAloc alocMap aloc

lowerAloc :: Map Aloc Int -> Aloc -> P.Loc
lowerAloc = ((.) . (.)) P.Addr (!)

genUndead :: Program -> [(Stmt, Set Aloc)]
genUndead (Program stmts (Aloc aloc)) = zip stmts $ reverse $ genUndead' (insert aloc empty) $ reverse stmts
genUndead (Program stmts _) = zip stmts $ reverse $ genUndead' empty $ reverse stmts

genUndead' :: Set Aloc -> [Stmt] -> [Set Aloc]
genUndead' undeadOut [] = []
genUndead' undeadOut (Stmt Set aloc (Aloc aloc') : rest) =  undeadOut : genUndead' undeadIn rest
                                                         where undeadIn = (insert aloc' . delete aloc) undeadOut
genUndead' undeadOut (Stmt _ _ (Aloc aloc) : rest) = undeadOut : genUndead' undeadIn rest
                                                   where undeadIn = insert aloc undeadOut
genUndead' undeadOut (_ : rest) = undeadOut : genUndead' undeadOut rest

locations :: [P.Loc]
locations = [ Reg RSP
            , Reg RBP
            , Reg RAX
            , Reg RBX
            , Reg RCX
            , Reg RDX
            , Reg RSI
            , Reg RDI
            , Reg R8
            , Reg R9
            , Reg R10
            , Reg R11
            , Reg R12
            , Reg R13
            , Reg R14
            , Reg R15 ]
            ++ map Addr [0..]

firstNotIn :: [P.Loc] -> [P.Loc] -> P.Loc
firstNotIn (loc : rest) locs
           | a `in` b = firstNotIn rest locs
           | otherwise = a

nextLoc :: Set Aloc -> State (Map Aloc P.Loc) P.Loc
nextLoc undeadOut = get >>= (return . firstNotIn locations . (flip S.map) undeadOut . (!))
