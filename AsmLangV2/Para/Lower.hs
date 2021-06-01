module Para.Lower
( lower
)
where

import Para.Types

import qualified Asm as A

import Data.Map hiding (map)

lower :: A.Program -> Program
lower (A.Program (stmts, out) _ locMap) = Program (lowerStmts locMap stmts)
                                                  (lowerTriv locMap out)

lowerStmts :: Map A.Aloc Int -> [A.Stmt] -> [Stmt]
lowerStmts locMap = map (lowerStmt locMap)

lowerStmt :: Map A.Aloc Int -> A.Stmt -> Stmt
lowerStmt locMap (A.Stmt op aloc triv) = Stmt (lowerOp op)
                                              (lowerAloc locMap aloc)
                                              (lowerTriv locMap triv)

lowerOp :: A.Op -> Op
lowerOp A.Set = Set
lowerOp A.Add = Add
lowerOp A.Mul = Mul

lowerAloc :: Map A.Aloc Int -> A.Aloc -> Loc
lowerAloc locMap aloc = Addr $ locMap ! aloc

lowerTriv :: Map A.Aloc Int -> A.Triv -> Triv
lowerTriv _ (A.Int64 i) = Int64 i
lowerTriv locMap (A.Aloc aloc) = Loc $ lowerAloc locMap aloc
