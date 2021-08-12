module Unique.Lower
( lower
)
where

import Compiler.Types
import Unique.Types
import qualified Uniform.Types as U

lower :: Program -> U.Program
lower (Program programDef) = U.Program (lowerProgramDef programDef)
                                       (lowerProgramBody programDef)

lowerProgramDef :: ProgramDef -> [U.Def]
lowerProgramDef (ProgramBody _) = []
lowerProgramDef (LetDef def programDef) = lowerDef def : lowerProgramDef programDef
lowerProgramDef (LetRecDef defs programDef) = lowerDefs defs ++ lowerProgramDef programDef

lowerProgramBody :: ProgramDef -> U.Body
lowerProgramBody (ProgramBody body) = lowerBody body
lowerProgramBody (LetDef _ programDef) = lowerProgramBody programDef
lowerProgramBody (LetRecDef _ programDef) = lowerProgramBody programDef

lowerDefs :: [Def] -> [U.Def]
lowerDefs = map lowerDef

lowerDef :: Def -> U.Def
lowerDef (Def label args (Body body)) = U.Def label (U.Body (foldr U.Lambda (lowerExpr body) args))

lowerBody :: Body -> U.Body
lowerBody (Body expr) = U.Body (lowerExpr expr)

lowerExpr :: Expr -> U.Expr
lowerExpr (Value val) = U.Value val
lowerExpr (Apply f arg) = U.Apply (lowerExpr f)
                                  (lowerExpr arg)
lowerExpr (Let var val body) = U.Let var (lowerExpr val)
                                         (lowerExpr body)
lowerExpr (Lambda arg body) = U.Lambda arg (lowerExpr body)
lowerExpr (If p c a) = U.If (lowerExpr p)
                            (lowerExpr c)
                            (lowerExpr a)
lowerExpr (BinOp op a b) = U.BinOp op (lowerExpr a)
                                      (lowerExpr b)
