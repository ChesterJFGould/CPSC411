module Exprs.Lower
( lower
)
where

import Compiler.Types
import Compiler.Value
import Exprs.Types

import qualified Unique.Types as U

import Control.Monad.State
import Data.Bits
import Data.Char
import Data.Int
import Data.Map as M hiding (filter)
import Data.Word

type Env a = State (Map Var Aloc, Map Var Label, Int64) a

lower :: Program -> Gensym U.Program
lower (Program defs body) = do
                            i <- get
                            let lowerProg :: Env U.Program
                                lowerProg = do
                                            primDefs <- genPrimDefs
                                            mapM genFuncLabel defs -- Put gensymed function labels into env
                                            defs' <- mapM lowerFunc defs
                                            let defs'' = primDefs ++ defs'
                                            body' <- lowerBody body
                                            return (U.Program defs'' body')
                                (prog', (_, _, i')) = runState lowerProg (M.empty, M.empty, i)
                            put i'
                            return prog'

genFuncLabel :: Func -> Env Label
genFuncLabel (Func name _ _) = genLabel name

genPrimDefs :: Env [U.Func]
genPrimDefs = (++) <$> mapM genNumBinOp [ ("*", "add", U.Add)
                                        , ("-", "sub", U.Sub)
                                        , ("*", "mul", U.Mul)
                                        , ("<", "lt", U.Lt)
                                        , (">", "gt", U.Gt)
                                        , ("==", "eq", U.Eq)
                                        , ("<=", "lte", U.Lte)
                                        , (">=", "gte", U.Gte)
                                        , ("/=", "neq", U.Neq) ]
                   <*> mapM genUnOp [ ("int?", "isInt", U.IsInt)
                                    , ("bool?", "isBool", U.IsBool)
                                    , ("empty?", "isEmpty", U.IsEmpty)
                                    , ("void?", "isVoid", U.IsVoid)
                                    , ("char?", "isChar", U.IsError)
                                    , ("not", "not", U.Not) ]

genNumBinOp :: (String, String, U.BinOp) -> Env U.Func
genNumBinOp (varName, template, op) = do
                                      (alocEnv, labelEnv, i) <- get
                                      let name = Label template i
                                          a = Aloc "a" (i + 1)
                                          b = Aloc "b" (i + 2)
                                          labelEnv' = M.insert (Var varName) name labelEnv
                                      put (alocEnv, labelEnv', i + 3)
                                      return (U.Func name [a, b] (numBinOpBody op a b))

genUnOp :: (String, String, U.UnOp) -> Env U.Func
genUnOp (varName, template, op) = do
                                  (alocEnv, labelEnv, i) <- get
                                  let name = Label template i
                                      a = Aloc "a" (i + 1)
                                      body = U.Body (U.UnOp op (U.Triv (U.UAloc a)))
                                      labelEnv' = M.insert (Var varName) name labelEnv
                                  put (alocEnv, labelEnv', i + 2)
                                  return (U.Func name [a] body)

numBinOpBody :: U.BinOp -> Aloc -> Aloc -> U.Body
numBinOpBody op a b = U.Body (U.If (U.UnOp U.Not (U.UnOp U.IsInt aExpr))
                                   ((U.Triv . U.Value) (Error (genBinOpErrorCode op 0)))
                                   (U.If (U.UnOp U.Not (U.UnOp U.IsInt bExpr))
                                         ((U.Triv . U.Value) (Error (genBinOpErrorCode op 1)))
                                         (U.BinOp op aExpr bExpr)))
                    where aExpr = U.Triv (U.UAloc a)
                          bExpr = U.Triv (U.UAloc b)

genBinOpErrorCode :: U.BinOp -> Int -> Word8
genBinOpErrorCode op arg = (shift ((fromIntegral . fromEnum) op) 4)
                           +
                           fromIntegral arg

lowerFunc :: Func -> Env U.Func
lowerFunc (Func name args body) = do
                                  (alocEnv, labelEnv, _) <- get
                                  name' <- lowerLabel name
                                  args' <- mapM genAloc args
                                  body' <- lowerBody body
                                  (_, _, i) <- get
                                  put (alocEnv, labelEnv, i)
                                  return (U.Func name' args' body')

lowerBody :: Expr -> Env U.Body
lowerBody expr = U.Body <$> lowerExpr expr

lowerExpr :: Expr -> Env U.Expr
lowerExpr (Triv triv) = U.Triv <$> lowerTriv triv
lowerExpr (Apply f args) = U.Apply <$> lowerLabel f
                                   <*> mapM lowerExpr args
lowerExpr (Let assignments body) = do
                                   (alocEnv, labelEnv, _) <- get
                                   vals <- mapM (lowerExpr . snd) assignments
                                   vars <- mapM (genAloc . fst) assignments
                                   body' <- lowerExpr body
                                   (_, _, i) <- get
                                   put (alocEnv, labelEnv, i)
                                   return (U.Let (zip vars vals) body')
lowerExpr (If p c a) = U.If <$> lowerExpr p
                            <*> lowerExpr c
                            <*> lowerExpr a

lowerTriv :: Triv -> Env U.Triv
lowerTriv (TVar var) = U.UAloc <$> lowerAloc var
lowerTriv (Value v) = return (U.Value v)

lowerAloc :: Var -> Env Aloc
lowerAloc var = do
                (alocEnv, _, _) <- get
                return (alocEnv ! var)

lowerLabel :: Var -> Env Label
lowerLabel var = do
                 (_, labelEnv, _) <- get
                 return (labelEnv ! var)

genAloc :: Var -> Env Aloc
genAloc var@(Var name) = do
                         (alocEnv, labelEnv, i) <- get
                         let name' = filter isAlphaNum name
                             aloc = Aloc (filter isAlphaNum name) i
                             alocEnv' = M.insert var aloc alocEnv
                         put (alocEnv', labelEnv, i + 1)
                         return aloc

genLabel :: Var -> Env Label
genLabel var@(Var name) = do
                          (alocEnv, labelEnv, i) <- get
                          let name' = filter isAlphaNum name
                              label = Label (filter isAlphaNum name) i
                              labelEnv' = M.insert var label labelEnv
                          put (alocEnv, labelEnv', i + 1)
                          return label
