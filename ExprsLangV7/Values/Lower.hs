module Values.Lower
( lower
)
where

import qualified Compiler.Locs as Locs
import Compiler.Types
import Values.Types

import qualified Monadic.Types as M

import Data.Maybe

lower :: Program -> M.Program
lower (Program defs body) = M.Program (map lowerFunc defs) (lowerBody [] body)

lowerFunc :: Func -> M.Block
lowerFunc (Func name args body) = M.Block name (lowerBody args body)

lowerBody :: [Aloc] -> Body -> M.Body
lowerBody args (Body expr) = M.Body (M.TSeq (zipWith M.Set
                                                     (map MAloc args)
                                                     (map (M.Triv . TMloc . MRloc) Locs.call))
                                            (lowerTail expr))

lowerTail :: Expr -> M.Tail
lowerTail expr@(Triv _) = M.Expr (lowerExpr expr)
lowerTail expr@(BinOp _ _ _) = M.Expr (lowerExpr expr)
lowerTail (Let assignments expr) = lowerLet M.TSeq lowerTail assignments expr
lowerTail (If p c a) = lowerIf M.TIf lowerTail p c a
lowerTail (Call f args) = M.TSeq argStmts (M.Jump f undeadOut)
                        where (argStmts, undeadOut) = lowerArgs args

lowerExpr :: Expr -> M.Expr
lowerExpr (Triv triv) = M.Triv (lowerTriv triv)
lowerExpr (BinOp op a b) = M.BinOp op (lowerTriv a) (lowerTriv b)
lowerExpr (Let assignments expr) = lowerLet M.Seq lowerExpr assignments expr
lowerExpr (If p c a) = lowerIf M.If lowerExpr p c a
lowerExpr (Call f args) = M.Seq argStmts
                                (M.Seq [M.JumpRet f undeadOut]
                                       ((M.Triv . TMloc . MRloc . Reg) Locs.return))
                        where (argStmts, undeadOut) = lowerArgs args

lowerPred :: Pred -> M.Pred
lowerPred (Bool b) = M.Bool b
lowerPred (RelOp op a b) = M.RelOp op (lowerTriv a) (lowerTriv b)
lowerPred (Not pred) = M.Not (lowerPred pred)
lowerPred (PLet assignments pred) = lowerLet M.PSeq lowerPred assignments pred
lowerPred (PIf p c a) = lowerIf M.PIf lowerPred p c a

lowerLet :: ([M.Stmt] -> a -> a) -> (b -> a) -> [(Aloc, Expr)] -> b -> a
lowerLet cons lowerer assignments body = cons assignments' body'
                                       where assignments' = zipWith M.Set
                                                                    (map MAloc alocs)
                                                                    (map lowerExpr vals)
                                             alocs = map fst assignments
                                             vals = map snd assignments
                                             body' = lowerer body

lowerIf :: (M.Pred -> a -> a -> a) -> (b -> a) -> Pred -> b -> b -> a
lowerIf cons lowerer p c a = cons (lowerPred p) (lowerer c) (lowerer a)

lowerArgs :: [ATriv] -> ([M.Stmt], [Mloc])
lowerArgs args = (stmts, undeadOut)
               where stmts = addrStmts ++ regStmts
                     addrStmts = zipWith M.Set
                                         (map (MRloc . LAddr) Locs.callAddrs)
                                         (map (M.Triv . lowerTriv) (drop numRegs args))
                     regStmts = zipWith M.Set
                                        (map (MRloc . Reg) Locs.callRegs)
                                        (map (M.Triv . lowerTriv) args)
                     numRegs = length Locs.callRegs
                     undeadOut = map MRloc (take (length args) Locs.call)
                     maybeAloc (TAloc aloc) = Just aloc
                     maybeAloc (APtr _) = Nothing

lowerTriv :: ATriv -> MTriv
lowerTriv (TAloc aloc) = (TMloc . MAloc) aloc
lowerTriv (APtr ptr) = MPtr ptr
