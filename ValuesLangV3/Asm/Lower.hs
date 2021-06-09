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
import Debug.Trace

lower :: Program -> P.Program
lower prog@(Program stmts out) = trace ((unlines . fmap show) stmts) $ P.Program stmts' out'
                               where ((_, stmts'), env) = (flip runState) M.empty
                                                           $ runWriterT (mapM lower' $ genUndead prog)
                                     out' = case out of
                                                 (Aloc aloc) -> P.Loc $ (trace (show $ env ! aloc) (env ! aloc))
                                                 (Int i) -> P.Int i

lower' :: (Stmt, Set Aloc) -> WriterT [P.Stmt] (State (Map Aloc P.Loc)) ()
lower' (Stmt Set aloc triv, undeadOut) = do
                                         loc <- lift $ nextLoc (S.delete aloc undeadOut) -- Put aloc into next unused loc
                                         triv' <- lift $ lowerTriv triv
                                         lift $ modify (M.insert aloc loc) -- Associate aloc with loc in the env
                                         tell $ [P.Stmt P.Set loc triv']
lower' (Stmt op aloc triv, undeadOut) = do
                                        aloc' <- lift $ get >>= (return . (! aloc))
                                        triv' <- lift $ lowerTriv triv
                                        tell $ [P.Stmt (lowerOp op) aloc' triv']


lowerOp :: Op -> P.Op
lowerOp Set = P.Set
lowerOp Add = P.Add
lowerOp Mul = P.Mul

lowerTriv :: Triv -> State (Map Aloc P.Loc) P.Triv
lowerTriv (Int i) = return $ P.Int i
lowerTriv (Aloc aloc) = get >>= (return . P.Loc . (! aloc))

genUndead :: Program -> [(Stmt, Set Aloc)]
genUndead (Program stmts (Aloc aloc)) = zip stmts $ reverse $ genUndead' (S.insert aloc S.empty) $ reverse stmts
genUndead (Program stmts _) = zip stmts $ reverse $ genUndead' S.empty $ reverse stmts

genUndead' :: Set Aloc -> [Stmt] -> [Set Aloc]
genUndead' undeadOut [] = []
genUndead' undeadOut (Stmt Set aloc (Aloc aloc') : rest) =  undeadOut : genUndead' undeadIn rest
                                                         where undeadIn = (S.insert aloc' . S.delete aloc) undeadOut
genUndead' undeadOut (Stmt Set aloc _ : rest) = undeadOut : genUndead' undeadIn rest
                                              where undeadIn = S.delete aloc undeadOut
genUndead' undeadOut (Stmt _ _ (Aloc aloc) : rest) = undeadOut : genUndead' undeadIn rest
                                                   where undeadIn = S.insert aloc undeadOut
genUndead' undeadOut (_ : rest) = undeadOut : genUndead' undeadOut rest

locations :: [P.Loc]
locations = [ P.Reg P.RSP
            , P.Reg P.RAX
            , P.Reg P.RBX
            , P.Reg P.RCX
            , P.Reg P.RDX
            , P.Reg P.RSI
            , P.Reg P.RDI
            , P.Reg P.R8
            , P.Reg P.R9
            , P.Reg P.R12
            , P.Reg P.R13
            , P.Reg P.R14
            , P.Reg P.R15 ]
            ++ Prelude.map P.Addr [0..]

firstNotIn :: [P.Loc] -> Set P.Loc -> P.Loc
firstNotIn (loc : rest) locs
           | loc `S.member` locs = firstNotIn rest locs
           | otherwise = loc

nextLoc :: Set Aloc -> State (Map Aloc P.Loc) P.Loc
nextLoc undeadOut = get >>= (return . firstNotIn locations . (flip S.map) undeadOut . (!))
