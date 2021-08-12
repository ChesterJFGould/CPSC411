module Exprs.TypeCheck
( typeCheck
)
where

import Exprs.Types

import Control.Monad.Except
import Control.Monad.State
import qualified Data.Map as M

type Check = ExceptT String (State (M.Map Var Type))

typeCheck :: Program -> Either String Program
typeCheck prog@(Program programDef) = do
                                      bodyTyp <- evalState (runExceptT (typeCheckProgramDef programDef)) M.empty
                                      case bodyTyp of
                                           TInt -> Right prog
                                           typ -> throwError (unwords [ "The program body must be of type Int."
                                                                      , "It is currently of type"
                                                                      , show typ ++ "."
                                                                      ])

typeCheckProgramDef :: ProgramDef -> Check Type
typeCheckProgramDef (ProgramBody body) = typeCheckBody "program body" body
typeCheckProgramDef (LetDef def programDef) = do
                                              typ <- typeCheckDef def
                                              putDef def typ
                                              typeCheckProgramDef programDef
typeCheckProgramDef (LetRecDef defs programDef) = do
                                                  let defTyps = map defType defs
                                                  mapM (uncurry putDef) (zip defs defTyps)
                                                  mapM typeCheckDef defs
                                                  typeCheckProgramDef programDef

defType :: Def -> Type
defType (Def typ _ _ _) = typ

typeCheckDef :: Def -> Check Type
typeCheckDef (Def typ (Var name) args body) = do
                                              env <- lift get
                                              calculatedTyp <- typeCheckDef' name typ args body
                                              lift (put env)
                                              if calculatedTyp == typ
                                              then return typ
                                              else throwError (unwords [ "The definition for"
                                                                       , name
                                                                       , "was declared to be of type"
                                                                       , show typ
                                                                       , "but is actually of type"
                                                                       , show calculatedTyp ++ "."
                                                                       ])

typeCheckDef' :: String -> Type -> [Var] -> Body -> Check Type
typeCheckDef' defName _ [] body = typeCheckBody defName body
typeCheckDef' defName (TFunc aT bT) (var : rest) body = do
                                                        putVar var aT
                                                        TFunc aT <$> typeCheckDef' defName bT rest body
typeCheckDef' defName _ _ _ = throwError (unwords [ "Too many arguments in definition for"
                                                  , defName ++ "." ])

typeCheckBody :: String -> Body -> Check Type
typeCheckBody defName (Body expr) = typeCheckExpr defName expr

typeCheckExpr :: String -> Expr -> Check Type
typeCheckExpr defName (Value value) = typeCheckValue defName value
typeCheckExpr defName (BinOp op a b) = typeCheckBinOp defName op a b
typeCheckExpr defName (Apply f arg) = typeCheckApply defName f arg
typeCheckExpr defName (Let var val body) = typeCheckLet defName var val body
typeCheckExpr defName (Lambda var typ body) = typeCheckLambda defName var typ body
typeCheckExpr defName (If p c a) = typeCheckIf defName p c a

typeCheckValue :: String -> Value -> Check Type
typeCheckValue defName (Int _) = return TInt
typeCheckValue defName (Bool _) = return TBool
typeCheckValue defName (TVar var@(Var varName)) = do
                                                 env <- lift get
                                                 let varT = M.lookup var env
                                                 case varT of
                                                      Just typ -> return typ
                                                      Nothing -> throwError (unwords [ "Use of undefined variable"
                                                                                     , varName
                                                                                     , "in definition for"
                                                                                     , defName ++ "."
                                                                                     ])

typeCheckBinOp :: String -> BinOp -> Expr -> Expr -> Check Type
typeCheckBinOp defName op a b = do
                                aT <- typeCheckExpr defName a
                                bT <- typeCheckExpr defName b
                                case (aT, bT) of
                                     (TInt, TInt) -> return (opResultType op)
                                     (TInt, typ) -> throwError (unwords [ "Use of binary operator"
                                                                        , show op
                                                                        , "in definition for"
                                                                        , defName
                                                                        , "expected a Right operand of type Int but"
                                                                        , "instead receive a Right operand of type"
                                                                        , show typ
                                                                        ])
                                     (typ, _) -> throwError (unwords [ "Use of binary operator"
                                                                     , show op
                                                                     , "in definition for"
                                                                     , defName
                                                                     , "expected a Right operand of type Int but"
                                                                     , "instead found a Right operand of type"
                                                                     , show typ ++ "."
                                                                     ])

typeCheckApply :: String -> Expr -> Expr -> Check Type
typeCheckApply defName f arg = do
                               fT <- typeCheckExpr defName f
                               argT <- typeCheckExpr defName arg
                               case (fT, argT) of
                                    (TFunc argT' outT, _)
                                     | argT' == argT -> return outT
                                     | otherwise -> throwError (unwords [ "Function application in definition for"
                                                                        , defName
                                                                        , "expected an argument of type"
                                                                        , show argT'
                                                                        , "but instead received an argument of type"
                                                                        , show argT ++ "."
                                                                        ])
                                    (_, _) -> throwError (unwords [ "Function application in definition for"
                                                                  , defName
                                                                  , "expected a function taking an argument of type"
                                                                  , show argT
                                                                  , "but instead found an expression of type"
                                                                  , show fT ++ "."
                                                                  ])

typeCheckLet :: String -> Var -> Expr -> Expr -> Check Type
typeCheckLet defName var val body = do
                                    env <- lift get
                                    valT <- typeCheckExpr defName val
                                    putVar var valT
                                    bodyT <- typeCheckExpr defName body
                                    lift (put env)
                                    return bodyT

typeCheckLambda :: String -> Var -> Type -> Expr -> Check Type
typeCheckLambda defName var varT body = do
                                        env <- lift get
                                        putVar var varT
                                        bodyT <- typeCheckExpr defName body
                                        lift (put env)
                                        return (TFunc varT bodyT)

typeCheckIf :: String -> Expr -> Expr -> Expr -> Check Type
typeCheckIf defName p c a = do
                            pT <- typeCheckExpr defName p
                            cT <- typeCheckExpr defName c
                            aT <- typeCheckExpr defName a
                            case (pT, cT, aT) of
                                 (TBool, _, _)
                                  | cT == aT -> return cT
                                  | otherwise -> throwError (unwords [ "If expression in definition for"
                                                                     , defName
                                                                     , "has differing types for its branches."
                                                                     , "The consequential branch has type"
                                                                     , show cT
                                                                     , "while the alternative branch has type"
                                                                     , show aT ++ "."
                                                                     ])
                                 (_, _, _) -> throwError (unwords [ "If expression in definition for"
                                                                  , defName
                                                                  , "expected a predicate of type Bool."
                                                                  , "Instead found a predicate of type"
                                                                  , show pT ++ "."
                                                                  ])

putDef :: Def -> Type -> Check ()
putDef (Def _ var _ _) = putVar var

putVar :: Var -> Type -> Check ()
putVar var typ = do
                 env <- lift get
                 let env' = M.insert var typ env
                 lift (put env')

opResultType :: BinOp -> Type
opResultType Add = TInt
opResultType Sub = TInt
opResultType Mul = TInt
opResultType Lt = TBool
opResultType Gt = TBool
opResultType Eq = TBool
opResultType Lte = TBool
opResultType Gte = TBool
opResultType Neq = TBool
