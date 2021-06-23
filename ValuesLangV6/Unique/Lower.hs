module Unique.Lower
( lower
)
where

import Compiler.Types
import Compiler.Values
import Unique.Types

import qualified Monadic.Types as M

lower :: Program -> M.Program
lower (Program defs body) = M.Program (map lowerFunc defs) (lowerBody [] body)

lowerFunc :: Func -> M.Block
lowerFunc (Func f args body) = M.Block f (lowerBody args body)

lowerBody :: [Aloc] -> Body -> M.Body
lowerBody args (Body lr tail) = M.Body lr (M.TSeq stmts (lowerTail tail))
                              where stmts = zipWith M.Set args' locs
                                    args' = map (MAloc) args
                                    locs = map (M.Triv . M.Loc . MRloc) callLocations

lowerTail :: Tail -> M.Tail
lowerTail (Expr expr) = M.Expr (lowerExpr expr)
lowerTail (TLet assignments tail) = lowerLet M.TSeq lowerTail assignments tail
lowerTail (TIf p c a) = lowerIf M.TIf lowerTail p c a
lowerTail (TCall f args) = (M.TSeq (lowerArgs args) (M.Jump f (argsUndeadOut args)))

lowerExpr :: Expr -> M.Expr
lowerExpr (Triv triv) = M.Triv (lowerTriv triv)
lowerExpr (BinOp op l r) = M.BinOp op (lowerTriv l) (lowerTriv r)
lowerExpr (Let assignments expr) = lowerLet M.Seq lowerExpr assignments expr
lowerExpr (If p c a) = lowerIf M.If lowerExpr p c a
lowerExpr (Call f args) = M.Seq (lowerArgs args) (M.Seq [M.JumpRet f (argsUndeadOut args)]
                                                 ((M.Triv . M.Loc . MRloc . Reg) returnRegister))

lowerPred :: Pred -> M.Pred
lowerPred (Bool b) = M.Bool b
lowerPred (RelOp op l r) = M.RelOp op (lowerTriv l) (lowerTriv r)
lowerPred (Not pred) = M.Not (lowerPred pred)
lowerPred (PLet assignments pred) = lowerLet M.PSeq lowerPred assignments pred
lowerPred (PIf p c a) = lowerIf M.PIf lowerPred p c a

lowerLet :: ([M.Stmt] -> a -> a) -> (b -> a) -> [(Aloc, Expr)] -> b -> a
lowerLet cons lowerer assignments body = cons stmts (lowerer body)
                                       where stmts = zipWith M.Set alocs vals
                                             alocs = map (MAloc . fst) assignments
                                             vals = map (lowerExpr . snd) assignments

lowerArgs :: [Triv] -> [M.Stmt]
lowerArgs args = stmts
               where stmts = setAddrs ++ setRegs -- Put stack variables first to reduce live range of regs
                     setAddrs = zipWith M.Set addrs argAddrs -- Create stack variable sets
                     addrs = map (MRloc . LAddr) callAddresses -- Make callAddresses usable
                     argAddrs = drop (length callRegisters) args' -- Get args that won't be put into registers
                     setRegs = zipWith M.Set regs args' -- Create register sets
                     regs = map (MRloc . Reg) callRegisters -- Make callRegisters usable
                     args' = map (M.Triv . lowerTriv) args -- Lower args

argsUndeadOut :: [Triv] -> [MLoc]
argsUndeadOut args = map MRloc (take (length args) callLocations)

lowerIf :: (M.Pred -> a -> a -> a) -> (b -> a) -> Pred -> b -> b -> a
lowerIf cons lowerer p c a = cons (lowerPred p) (lowerer c) (lowerer a)

lowerTriv :: Triv -> M.Triv
lowerTriv (Int i) = M.Int i
lowerTriv (TAloc aloc) = (M.Loc . MAloc) aloc
