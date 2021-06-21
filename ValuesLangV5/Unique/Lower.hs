module Unique.Lower
( lower
)
where

import Unique.Types

import qualified Monadic.Types as M

lower :: Program -> M.Program
lower (Program defs body) = M.Program defs' body'
                          where defs' = map lowerFunc defs
                                body' = lowerTail body

lowerFunc :: Func -> M.Block
lowerFunc (Func f args body) = M.Block f' body''
                             where f' = lowerLabel f
                                   body'' = M.TSeq preamble body'
                                   preamble = map (uncurry M.Set) (zip args' callLocs')
                                   callLocs' = map (M.Triv . M.TLoc) callLocs
                                   args' = map lowerAloc args
                                   body' = lowerTail body

lowerTail :: Tail -> M.Tail
lowerTail (Expr expr) = M.Expr (lowerExpr expr)
lowerTail (TLet assignments tail) = lowerLet lowerTail M.TSeq assignments tail
lowerTail (TIf p c a) = lowerIf lowerTail M.TIf p c a
lowerTail (Call f args) = lowerCall f args

lowerExpr :: Expr -> M.Expr
lowerExpr (Triv triv) = M.Triv (lowerTriv triv)
lowerExpr (BinOp op left right) = M.BinOp (lowerOp op) (lowerTriv left) (lowerTriv right)
lowerExpr (Let assignments expr) = lowerLet lowerExpr M.Seq assignments expr
lowerExpr (If p c a) = lowerIf lowerExpr M.If p c a

lowerPred :: Pred -> M.Pred
lowerPred (Bool b) = M.Bool b
lowerPred (RelOp op left right) = M.RelOp (lowerRelOp op) (lowerTriv left) (lowerTriv right)
lowerPred (Not pred) = M.Not (lowerPred pred)
lowerPred (PLet assignments pred) = lowerLet lowerPred M.PSeq assignments pred
lowerPred (PIf p c a) = lowerIf lowerPred M.PIf p c a

lowerLet :: (a -> b) -> ([M.Stmt] -> b -> b) -> [(Aloc, Expr)] -> a -> b
lowerLet lowerer cons assignments body = cons stmts body'
                                       where stmts = map (uncurry M.Set) (zip vars vals)
                                             body' = lowerer body
                                             vars = map (lowerAloc . fst) assignments
                                             vals = map (lowerExpr . snd) assignments

lowerIf :: (a -> b) -> (M.Pred -> b -> b -> b) -> Pred -> a -> a -> b
lowerIf lowerer cons p c a = cons p' c' a'
                           where p' = lowerPred p
                                 c' = lowerer c
                                 a' = lowerer a

lowerCall :: Label -> [Triv] -> M.Tail
lowerCall f args = M.TSeq stmts jump
                 where stmts = stackStmts ++ regStmts
                       jump = M.Jump f' locs
                       f' = lowerLabel f
                       locs = take (length args) callLocs
                       stackStmts = map (uncurry M.Set) (zip callAddrs stackArgs)
                       regStmts = map (uncurry M.Set) (zip callRegs regArgs)
                       (regArgs, stackArgs) = splitAt 6 (map M.Triv args')
                       args' = map lowerTriv args

lowerTriv :: Triv -> M.Triv
lowerTriv (Int i) = M.Int i
lowerTriv (TAloc aloc) = M.TLoc (lowerAloc aloc)

lowerAloc :: Aloc -> M.Loc
lowerAloc (Aloc v i) = M.Aloc v i

lowerLabel :: Label -> M.Label
lowerLabel (Label n) = M.Label n

lowerOp :: Op -> M.Op
lowerOp Add = M.Add
lowerOp Mul = M.Mul

lowerRelOp :: RelOp -> M.RelOp
lowerRelOp Lt = M.Lt
lowerRelOp Gt = M.Gt
lowerRelOp Eq = M.Eq
lowerRelOp Lte = M.Lte
lowerRelOp Gte = M.Gte
lowerRelOp Neq = M.Neq

callLocs :: [M.Loc]
callLocs = callRegs ++ callAddrs

callRegs :: [M.Loc]
callRegs = map M.Reg [ M.RDI
                     , M.RSI
                     , M.RDX
                     , M.RCX
                     , M.R8
                     , M.R9 ]

callAddrs :: [M.Loc]
callAddrs = map M.Addr [0..]
