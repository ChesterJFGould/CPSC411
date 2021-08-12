module Exprs.Lower
( lower
)
where

import Compiler.Gensym
import qualified Compiler.Types as C
import Exprs.Types
import qualified Unique.Types as U

import Control.Monad.State
import Data.Char
import qualified Data.Map as M

type Unique = StateT (M.Map Var C.Value) (Gensym)

lower :: Program -> Gensym U.Program
lower (Program programDef) = U.Program <$> evalStateT (lowerProgramDef programDef) M.empty

lowerProgramDef :: ProgramDef -> Unique U.ProgramDef
lowerProgramDef (ProgramBody body) = U.ProgramBody <$> lowerBody body
lowerProgramDef (LetDef def programDef) = do
                                          label <- lift (genDefLabel def)
                                          def' <- lowerDef label def
                                          putDef label def
                                          U.LetDef def' <$> lowerProgramDef programDef
lowerProgramDef (LetRecDef defs programDef) = do
                                              defLabels <- lift (mapM genDefLabel defs)
                                              mapM (uncurry putDef) (zip defLabels defs)
                                              defs' <- mapM (uncurry lowerDef) (zip defLabels defs)
                                              U.LetRecDef defs' <$> lowerProgramDef programDef

genDefLabel :: Def -> Gensym C.Label
genDefLabel (Def _ (Var template) _ _) = genLabel (lowerTemplate template)

lowerDef :: C.Label -> Def -> Unique U.Def
lowerDef label (Def _ _ args body) = encapsulate ( do
                                                   args' <- lift (mapM lowerAloc args)
                                                   mapM (uncurry putAloc) (zip args args')
                                                   body' <- lowerBody body
                                                   return (U.Def label args' body')
                                                 )

lowerBody :: Body -> Unique U.Body
lowerBody (Body expr) = U.Body <$> lowerExpr expr

lowerExpr :: Expr -> Unique U.Expr
lowerExpr (Value value) = U.Value <$> lowerValue value
lowerExpr (BinOp op a b) = U.BinOp (lowerOp op) <$> lowerExpr a
                                                <*> lowerExpr b
lowerExpr (Apply f arg) = U.Apply <$> lowerExpr f
                                  <*> lowerExpr arg
lowerExpr (Let var val body) = encapsulate ( do
                                             var' <- lift (lowerAloc var)
                                             val' <- lowerExpr val
                                             putAloc var var'
                                             U.Let var' val' <$> lowerExpr body
                                           )
lowerExpr (Lambda arg _ body) = encapsulate ( do
                                              arg' <- lift (lowerAloc arg)
                                              putAloc arg arg'
                                              U.Lambda arg' <$> lowerExpr body
                                            )
lowerExpr (If p c a) = U.If <$> lowerExpr p
                            <*> lowerExpr c
                            <*> lowerExpr a

lowerValue :: Value -> Unique C.Value
lowerValue (Int i) = return (C.VInt i)
lowerValue (Bool b) = return (C.VBool b)
lowerValue (TVar v) = lowerVar v

lowerOp :: BinOp -> C.BinOp
lowerOp Add = C.BNumOp C.Add
lowerOp Sub = C.BNumOp C.Sub
lowerOp Mul = C.BNumOp C.Mul
lowerOp Lt = C.BRelOp C.Lt
lowerOp Gt = C.BRelOp C.Gt
lowerOp Eq = C.BRelOp C.Eq
lowerOp Lte = C.BRelOp C.Lte
lowerOp Gte = C.BRelOp C.Gte
lowerOp Neq = C.BRelOp C.Neq

putDef :: C.Label -> Def -> Unique ()
putDef label (Def _ var _ _) = do
                               env <- get
                               let env' = M.insert var (C.VLabel label) env
                               put env'

lowerAloc :: Var -> Gensym C.Aloc
lowerAloc (Var template) = genAloc (lowerTemplate template)

lowerVar :: Var -> Unique C.Value
lowerVar var = gets (M.! var)

putAloc :: Var -> C.Aloc -> Unique ()
putAloc var aloc = do
                   env <- get
                   let env' = M.insert var (C.VAloc aloc) env
                   put env'

encapsulate :: Unique a -> Unique a
encapsulate computation = do
                          env <- get
                          val <- computation
                          put env
                          return val

lowerTemplate :: String -> String
lowerTemplate = filter isAlphaNum
