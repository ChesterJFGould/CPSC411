module Asm.Lower
( lower
)
where

import Asm.Types

import qualified Para.Types as P

import Data.Map as M hiding (map)

lower :: Program -> P.Program
lower (Program (stmts, out) _ alocMap) = P.Program (map (lowerStmt alocMap) stmts)
                                                   (lowerTriv alocMap out)

lowerStmt :: Map Aloc Int -> Stmt -> P.Stmt
lowerStmt alocMap (Stmt op aloc triv) = P.Stmt (lowerOp op)
                                               (lowerAloc alocMap aloc)
                                               (lowerTriv alocMap triv)

lowerOp :: Op -> P.Op
lowerOp Set = P.Set
lowerOp Add = P.Add
lowerOp Mul = P.Mul

lowerTriv :: Map Aloc Int -> Triv -> P.Triv
lowerTriv _ (Int i) = P.Int i
lowerTriv alocMap (Aloc aloc) = P.Loc $ lowerAloc alocMap aloc

lowerAloc :: Map Aloc Int -> Aloc -> P.Loc
lowerAloc = ((.) . (.)) P.Addr (!)
