module Exprs.Sort
( sort
)
where

import Exprs.Types
import qualified Graph.Sort as G

import qualified Data.Map as M
import qualified Data.Set as S

sort :: Program -> Program
sort (Program def) = let programDefs = defs def
                         defNames = map name programDefs
                         edges = map (usages (S.fromList defNames)) programDefs
                         graph = zip defNames edges
                         sortedNames = G.sort graph
                         defLookup = M.fromList (zip defNames programDefs)
                         usagesLookup = M.fromList (zip defNames edges)
                         programBody = body def
                         sortedDefs = sortDefs programBody defLookup usagesLookup sortedNames
                     in Program sortedDefs

defs :: ProgramDef -> [Def]
defs (ProgramBody _) = []
defs (LetDef def programDef) = def : defs programDef
defs (LetRecDef recDefs programDef) = recDefs ++ defs programDef

body :: ProgramDef -> Body
body (ProgramBody body) = body
body (LetDef _ programDef) = body programDef
body (LetRecDef _ programDef) = body programDef

name :: Def -> Var
name (Def _ name _ _) = name

usages :: S.Set Var -> Def -> [Var]
usages vars def = S.toList (S.intersection vars (freeVars def))

freeVars :: Def -> S.Set Var
freeVars (Def _ _ args body) = S.difference (freeVarsBody body) (S.fromList args)

freeVarsBody :: Body -> S.Set Var
freeVarsBody (Body expr) = freeVarsExpr expr

freeVarsExpr :: Expr -> S.Set Var
freeVarsExpr (Value value) = freeVarsValue value
freeVarsExpr (BinOp _ a b) = S.union (freeVarsExpr a) (freeVarsExpr b)
freeVarsExpr (Apply f arg) = S.union (freeVarsExpr f) (freeVarsExpr arg)
freeVarsExpr (Let var val body) = S.union (freeVarsExpr val)
                                          (S.delete var (freeVarsExpr body))
freeVarsExpr (Lambda var _ body) = S.delete var (freeVarsExpr body)
freeVarsExpr (If p c a) = S.union (freeVarsExpr p)
                                  (S.union (freeVarsExpr c)
                                           (freeVarsExpr a))

freeVarsValue :: Value -> S.Set Var
freeVarsValue (Int _) = S.empty
freeVarsValue (Bool _) = S.empty
freeVarsValue (TVar var) = S.singleton var

sortDefs :: Body -> M.Map Var Def -> M.Map Var [Var] -> [[Var]] -> ProgramDef
sortDefs body defLookup usageLookup [] = ProgramBody body
sortDefs body defLookup usageLookup ([var] : rest) = if var `elem` usages
                                                     then LetRecDef [def] (sortDefs body defLookup usageLookup rest)
                                                     else LetDef def (sortDefs body defLookup usageLookup rest)
                                                   where usages = usageLookup M.! var
                                                         def = defLookup M.! var
sortDefs body defLookup usageLookup (vars : rest) = LetRecDef defs (sortDefs body defLookup usageLookup rest)
                                                  where defs = map (defLookup M.!) vars
