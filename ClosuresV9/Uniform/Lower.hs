module Uniform.Lower
( lower
)
where

import Compiler.Gensym
import Compiler.Types
import qualified Closures.Types as C
import Uniform.Types

import Control.Monad.Reader
import Control.Monad.Writer
import qualified Data.Set as S
import qualified Data.Map as M

type Lift = WriterT [C.Def] (Gensym)

lower :: Program -> Gensym C.Program
lower (Program defs body) = do
                            ((defs', body'), closureDefs) <- runWriterT ( do
                                                                          defs' <- lowerDefs defs
                                                                          body' <- lowerBody body
                                                                          return (defs', body')
                                                                        )
                            let defs'' = closureDefs ++ defs'
                            return (C.Program defs'' body')

lowerDefs :: [Def] -> Lift [C.Def]
lowerDefs = mapM lowerDef

lowerDef :: Def -> Lift C.Def
lowerDef (Def label body) = C.Const label <$> lowerBody body

lowerBody :: Body -> Lift C.Body
lowerBody (Body expr) = C.Body <$> lowerExpr expr

lowerExpr :: Expr -> Lift C.Expr
lowerExpr (Value value) = return (C.Value value)
lowerExpr (Apply f arg) = C.ApplyClosure <$> lowerExpr f
                                         <*> lowerExpr arg
lowerExpr (Let var val body) = C.Let var <$> lowerExpr val
                                         <*> lowerExpr body
lowerExpr (Lambda arg body) = lambdaLift arg body
lowerExpr (If p c a) = C.If <$> lowerExpr p
                            <*> lowerExpr c
                            <*> lowerExpr a
lowerExpr (BinOp op a b) = C.BinOp op <$> lowerExpr a
                                      <*> lowerExpr b

lambdaLift :: Aloc -> Expr -> Lift C.Expr
lambdaLift arg body = do
                      body' <- lowerExpr body
                      let freeVars = S.delete arg (freeVarsExpr body')
                          freeVarsList = S.toList freeVars
                      envAloc <- lift (genAloc "env")
                      let envRefs = map (C.ClosureRef envAloc) [0..]
                          envRefLookup = M.fromList (zip freeVarsList envRefs)
                          body'' = alocReplace envRefLookup body'
                      closureLabel <- lift (genLabel "closureDef")
                      let closureDef = C.Func closureLabel envAloc arg (C.Body body'')
                      tell [ closureDef ]
                      let freeVarExprs = map (C.Value . VAloc) freeVarsList
                      return (C.Closure closureLabel freeVarExprs)

freeVarsExpr :: C.Expr -> S.Set Aloc
freeVarsExpr (C.Value value) = freeVarsValue value
freeVarsExpr (C.BinOp _ a b) = S.union (freeVarsExpr a)
                                       (freeVarsExpr b)
freeVarsExpr (C.ApplyClosure f arg) = S.union (freeVarsExpr f)
                                              (freeVarsExpr arg)
freeVarsExpr (C.Let var val body) = S.union (freeVarsExpr val)
                                          (S.delete var (freeVarsExpr body))
freeVarsExpr (C.Closure _ exprs) = S.unions (map freeVarsExpr exprs)
freeVarsExpr (C.ClosureRef _ _) = error "Tried take the free variables of an expression with a ClosureRef"
freeVarsExpr (C.If p c a) = S.union (freeVarsExpr p)
                                    (S.union (freeVarsExpr c)
                                             (freeVarsExpr a))

freeVarsValue :: Value -> S.Set Aloc
freeVarsValue (VInt _) = S.empty
freeVarsValue (VBool _) = S.empty
freeVarsValue (VAloc aloc) = S.singleton aloc
freeVarsValue (VLabel _) = S.empty

alocReplace :: M.Map Aloc C.Expr -> C.Expr -> C.Expr
alocReplace env expr = runReader (alocReplaceExpr expr) env

alocReplaceExpr :: C.Expr -> Reader (M.Map Aloc C.Expr) C.Expr
alocReplaceExpr (C.Value val) = alocReplaceValue val
alocReplaceExpr (C.BinOp op a b) = C.BinOp op <$> alocReplaceExpr a
                                            <*> alocReplaceExpr b
alocReplaceExpr (C.ApplyClosure f arg) = C.ApplyClosure <$> alocReplaceExpr f
                                                      <*> alocReplaceExpr arg
alocReplaceExpr (C.Let var val body) = C.Let var <$> alocReplaceExpr val
                                               <*> alocReplaceExpr body
alocReplaceExpr (C.Closure label exprs) = C.Closure label <$> mapM alocReplaceExpr exprs
alocReplaceExpr (C.ClosureRef _ _) = error "Tried to replace alocs in an expression with a ClosureRef"
alocReplaceExpr (C.If p c a) = C.If <$> alocReplaceExpr p
                                    <*> alocReplaceExpr c
                                    <*> alocReplaceExpr a

alocReplaceValue :: Value -> Reader (M.Map Aloc C.Expr) C.Expr
alocReplaceValue val@(VInt _) = return (C.Value val)
alocReplaceValue val@(VBool _) = return (C.Value val)
alocReplaceValue val@(VAloc aloc) = do
                                    env <- ask
                                    case M.lookup aloc env of
                                         Nothing -> return (C.Value val)
                                         Just expr -> return expr
alocReplaceValue val@(VLabel _) = return (C.Value val)
