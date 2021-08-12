module Bits.Lower
( lower
)
where

import Bits.Types
import Compiler.Types
import qualified Consts.Types as C

import Control.Monad.Reader
import Control.Monad.Writer
import Data.Maybe
import qualified Data.Set as S

type Consts = Writer [(Label, C.Expr)]

type Labels = Reader (S.Set Label)

lower :: Program -> C.Program
lower (Program defs body) = let (maybeFuncs, consts) = runWriter (mapM lowerDef defs)
                                funcs = catMaybes maybeFuncs
                                labels = map fst consts
                                zeroExpr = C.Triv (ALit (Lit 0))
                                labelToExpr = C.Triv . ALabel
                                labelSet = S.fromList labels
                                constStmts = map (\(label, expr) -> C.MSet (labelToExpr label)
                                                                           zeroExpr
                                                                           (runReader (labelsExpr expr)
                                                                                      labelSet))
                                                 consts
                                C.Body bodyExpr = runReader (labelsBody (lowerBody body)) labelSet
                                funcs' = runReader (labelsFuncs funcs) labelSet
                                prog = C.Program labels funcs' (C.Body (C.Seq constStmts bodyExpr))
                            in prog

lowerDef :: Def -> Consts (Maybe C.Func)
lowerDef (Const label body) = do
                              let C.Body expr' = lowerBody body
                              tell [ (label, expr') ]
                              return Nothing
lowerDef (Func label env arg body) = do
                                     let body' = lowerBody body
                                     return (Just (C.Func label env arg body'))

lowerBody :: Body -> C.Body
lowerBody (Body expr) = C.Body (lowerExpr expr)

lowerExpr :: Expr -> C.Expr
lowerExpr (Triv triv) = C.Triv triv
lowerExpr (NumOp op a b) = C.NumOp op (lowerExpr a)
                                      (lowerExpr b)
lowerExpr (Apply f env arg) = C.Apply (lowerExpr f)
                                      (lowerExpr env)
                                      (lowerExpr arg)
lowerExpr (Let var val body) = C.Let var (lowerExpr val)
                                         (lowerExpr body)
lowerExpr (Alloc expr) = C.Alloc (lowerExpr expr)
lowerExpr (MRef ptr offset) = C.MRef (lowerExpr ptr)
                                     (lowerExpr offset)
lowerExpr (If p c a) = C.If (lowerPred p)
                            (lowerExpr c)
                            (lowerExpr a)
lowerExpr (Seq stmts expr) = C.Seq (lowerStmts stmts)
                                   (lowerExpr expr)

lowerStmts :: [Stmt] -> [C.Stmt]
lowerStmts = map lowerStmt

lowerStmt :: Stmt -> C.Stmt
lowerStmt (MSet ptr offset val) = C.MSet (lowerExpr ptr)
                                         (lowerExpr offset)
                                         (lowerExpr val)

lowerPred :: Pred -> C.Pred
lowerPred (Bool b) = C.Bool b
lowerPred (RelOp op a b) = C.RelOp op (lowerExpr a)
                                      (lowerExpr b)
lowerPred (Not pred) = C.Not (lowerPred pred)
lowerPred (PLet var val pred) = C.PLet var (lowerExpr val)
                                           (lowerPred pred)
lowerPred (PIf p c a) = C.PIf (lowerPred p)
                              (lowerPred c)
                              (lowerPred a)

labelsFuncs :: [C.Func] -> Labels [C.Func]
labelsFuncs = mapM labelsFunc

labelsFunc :: C.Func -> Labels C.Func
labelsFunc (C.Func label env arg body) = C.Func label env arg <$> labelsBody body

labelsBody :: C.Body -> Labels C.Body
labelsBody (C.Body expr) = C.Body <$> labelsExpr expr

labelsExpr :: C.Expr -> Labels C.Expr
labelsExpr (C.Triv triv) = labelsTriv triv
labelsExpr (C.NumOp op a b) = C.NumOp op <$> labelsExpr a
                                         <*> labelsExpr b
labelsExpr (C.Apply f env arg) = C.Apply <$> labelsExpr f
                                         <*> labelsExpr env
                                         <*> labelsExpr arg
labelsExpr (C.Let var val body) = C.Let var <$> labelsExpr val
                                            <*> labelsExpr body
labelsExpr (C.Alloc expr) = C.Alloc <$> labelsExpr expr
labelsExpr (C.MRef ptr offset) = C.MRef <$> labelsExpr ptr
                                        <*> labelsExpr offset
labelsExpr (C.If p c a) = C.If <$> labelsPred p
                               <*> labelsExpr c
                               <*> labelsExpr a
labelsExpr (C.Seq stmts expr) = C.Seq <$> labelsStmts stmts
                                      <*> labelsExpr expr

labelsStmts :: [C.Stmt] -> Labels [C.Stmt]
labelsStmts = mapM labelsStmt

labelsStmt :: C.Stmt -> Labels C.Stmt
labelsStmt (C.MSet ptr offset val) = C.MSet <$> labelsExpr ptr
                                            <*> labelsExpr offset
                                            <*> labelsExpr val

labelsPred :: C.Pred -> Labels C.Pred
labelsPred (C.Bool b) = return (C.Bool b)
labelsPred (C.RelOp op a b) = C.RelOp op <$> labelsExpr a
                                         <*> labelsExpr b
labelsPred (C.Not pred) = C.Not <$> labelsPred pred
labelsPred (C.PLet var val pred) = C.PLet var <$> labelsExpr val
                                              <*> labelsPred pred

labelsPred (C.PIf p c a) = C.PIf <$> labelsPred p
                                 <*> labelsPred c
                                 <*> labelsPred a

labelsTriv :: ATriv -> Labels C.Expr
labelsTriv triv@(ALit _) = return (C.Triv triv)
labelsTriv triv@(AAloc _) = return (C.Triv triv)
labelsTriv triv@(ALabel label) = do
                                 globalLabelSet <- ask
                                 if label `S.member` globalLabelSet
                                 then return (C.MRef (C.Triv triv) (C.Triv (ALit (Lit 0))))
                                 else return (C.Triv triv)
