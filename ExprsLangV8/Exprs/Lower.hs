module Exprs.Lower
( lower
)
where

import Compiler.Gensym
import Compiler.Types
import Compiler.Value
import Exprs.Types
import qualified Unique.Types as U

import Control.Monad.State
import Data.Bits
import Data.Int
import Data.List as L
import Data.Map as M
import Data.Word

type Env a = StateT (Map Var Aloc, Map Var Label) (State Int64) a

lower :: Program -> Gensym U.Program
lower (Program defs body) = let lowerProg :: Env U.Program
                                lowerProg = do
                                            primDefs <- genPrimDefs
                                            mapM genPutFuncLabel defs
                                            defs' <- mapM lowerFunc defs
                                            body' <- lowerBody body
                                            return (U.Program (primDefs ++ defs') body')
                            in evalStateT lowerProg (M.empty, M.empty)

genPrimDefs :: Env [U.Func]
genPrimDefs = do
              setVecDef <- genSetVec "setvec!" "setvec"
              vecRefDef <- genVecRef "vecRef" "vecRef"
              binOpDefs <- mapM genBinOp
                                [ ("+", "add", U.Add, Just U.IsInt, Just U.IsInt, 1)
                                , ("-", "sub", U.Sub, Just U.IsInt, Just U.IsInt, 2)
                                , ("*", "mul", U.Mul, Just U.IsInt, Just U.IsInt, 3)
                                , ("<", "lt", U.Lt, Just U.IsInt, Just U.IsInt, 4)
                                , (">", "gt", U.Gt, Just U.IsInt, Just U.IsInt, 5)
                                , ("==", "eq", U.Eq, Nothing, Nothing, 0)
                                , ("<=", "lte", U.Lte, Just U.IsInt, Just U.IsInt, 6)
                                , (">=", "gte", U.Gte, Just U.IsInt, Just U.IsInt, 7)
                                , ("/=", "neq ", U.Neq, Nothing, Nothing, 0)
                                , ("::", "cons", U.Cons, Nothing, Nothing, 0) ]
              unOpDefs <- mapM genUnOp
                               [ ("int?", "isInt", U.IsInt, Nothing, 0)
                               , ("bool?", "isBool", U.IsBool, Nothing, 0)
                               , ("empty?", "isEmpty", U.IsEmpty, Nothing, 0)
                               , ("void?", "isVoid", U.IsVoid, Nothing, 0)
                               , ("char?", "isChar", U.IsChar, Nothing, 0)
                               , ("error?", "isError", U.IsError, Nothing, 0)
                               , ("pair?", "isPair", U.IsPair, Nothing, 0)
                               , ("not", "not", U.Not, Nothing, 0)
                               , ("vector?", "isVector", U.IsVector, Nothing, 0)
                               , ("car", "car", U.Car, Just U.IsPair, 9)
                               , ("cdr", "cdr", U.Cdr, Just U.IsPair, 10)
                               , ("makeVector", "makeVector", U.MakeVector, Just U.IsInt, 11)
                               , ("vectorLength", "vectorLength", U.VectorLength, Just U.IsVector, 12) ]
              return (concat [ [setVecDef, vecRefDef], binOpDefs, unOpDefs ])

genSetVec :: String -> String -> Env U.Func
genSetVec varName template = do
                             label <- lift (genLabel template)
                             putLabel (Var varName, label)
                             vec <- lift (genAloc "vec")
                             index <- lift (genAloc "index")
                             val <- lift (genAloc "val")
                             let vecAssert = assertType vec' (Just U.IsVector) (errorEncodeArg 0xF 1)
                                 indexAssert = assertType index' (Just U.IsInt) (errorEncodeArg 0xF 2)
                                 vec' = (U.Triv (U.TAloc vec))
                                 index' = (U.Triv (U.TAloc index))
                                 val' = (U.Triv (U.TAloc val))
                                 oobError = U.Triv (U.Value (Error (errorEncodeArg 0xF 2)))
                                 body = (U.Body . vecAssert . indexAssert)
                                        (U.If (U.BinOp U.Gte
                                                       index' (U.Triv (U.Value (Int 0))))
                                              (U.If (U.BinOp U.Lt
                                                             index'
                                                             (U.UnOp U.VectorLength vec'))
                                                    (U.Seq [ U.SetVec vec' index' val' ]
                                                                                  (U.Triv (U.Value Void)))
                                                    oobError)
                                              oobError)
                             return (U.Func label [vec, index, val] body)

genVecRef :: String -> String -> Env U.Func
genVecRef varName template = do
                             label <- lift (genLabel template)
                             putLabel (Var varName, label)
                             vec <- lift (genAloc "vec")
                             index <- lift (genAloc "index")
                             let vecAssert = assertType vec' (Just U.IsVector) (errorEncodeArg 0xE 1)
                                 indexAssert = assertType index' (Just U.IsInt) (errorEncodeArg 0xE 2)
                                 vec' = (U.Triv (U.TAloc vec))
                                 index' = (U.Triv (U.TAloc index))
                                 oobError = U.Triv (U.Value (Error (errorEncodeArg 0xE 2)))
                                 body = (U.Body . vecAssert . indexAssert)
                                        (U.If (U.BinOp U.Gte
                                                       index' (U.Triv (U.Value (Int 0))))
                                              (U.If (U.BinOp U.Lt
                                                             index'
                                                             (U.UnOp U.VectorLength vec'))
                                                    (U.BinOp U.VectorRef vec' index')
                                                    oobError)
                                              oobError)
                             return (U.Func label [vec, index] body)

genBinOp :: (String, String, U.BinOp, Maybe U.UnOp, Maybe U.UnOp, Word8) -> Env U.Func
genBinOp (varName, template, op, aType, bType, opcode) = do
                                                         label <- lift (genLabel template)
                                                         putLabel (Var varName, label)
                                                         a <- lift (genAloc "a")
                                                         b <- lift (genAloc "b")
                                                         let aAssert = assertType a' aType (errorEncodeArg opcode 1)
                                                             bAssert = assertType b' bType (errorEncodeArg opcode 2)
                                                             a' = (U.Triv (U.TAloc a))
                                                             b' = (U.Triv (U.TAloc b))
                                                             body = (U.Body . aAssert . bAssert) (U.BinOp op a' b')
                                                         return (U.Func label [a, b] body)

genUnOp :: (String, String, U.UnOp, Maybe U.UnOp, Word8) -> Env U.Func
genUnOp (varName, template, op, aType, opcode) = do
                                                 label <- lift (genLabel template)
                                                 putLabel (Var varName, label)
                                                 a <- lift (genAloc "a")
                                                 let aAssert = assertType a' aType (errorEncodeArg opcode 1)
                                                     a' = (U.Triv (U.TAloc a))
                                                     body = (U.Body . aAssert) (U.UnOp op a')
                                                 return (U.Func label [a] body)

assertType :: U.Expr -> Maybe U.UnOp -> Word8 -> U.Expr -> U.Expr
assertType assertee (Just pred) errorCode body = U.If (U.UnOp pred assertee)
                                                      body
                                                      (U.Triv (U.Value (Error errorCode)))
assertType _ Nothing _ body = body

errorEncodeArg :: Word8 -> Word8 -> Word8
errorEncodeArg opcode arg = (shift opcode 4) .|. (arg - 1)

genPutFuncLabel :: Func -> Env Label
genPutFuncLabel (Func var@(Var template) _ _) = do
                                                label <- lift (genLabel template)
                                                putLabel (var, label)
                                                return label

lowerFunc :: Func -> Env U.Func
lowerFunc (Func name args body) = do
                                  env <- get
                                  name' <- lowerLabel name
                                  args' <- mapM genPutAloc args
                                  body' <- lowerBody body
                                  put env
                                  return (U.Func name' args' body')

lowerBody :: Body -> Env U.Body
lowerBody (Body expr) = U.Body <$> lowerExpr expr

lowerExpr :: Expr -> Env U.Expr
lowerExpr (Triv triv) = U.Triv <$> lowerTriv triv
lowerExpr (Apply f args) = U.Apply <$> lowerLabel f
                                   <*> mapM lowerExpr args
lowerExpr (Let assignments expr) = do
                                   env <- get
                                   vals <- mapM (lowerExpr . snd) assignments
                                   vars <- mapM (genPutAloc . fst) assignments
                                   expr' <- lowerExpr expr
                                   put env
                                   return (U.Let (zip vars vals) expr')
lowerExpr (If p c a) = U.If <$> lowerExpr p
                            <*> lowerExpr c
                            <*> lowerExpr a

lowerTriv :: Triv -> Env U.Triv
lowerTriv (TVar var) = U.TAloc <$> lowerAloc var
lowerTriv (Value val) = return (U.Value val)

lowerAloc :: Var -> Env Aloc
lowerAloc var = do
                (alocEnv, _) <- get
                return (alocEnv ! var)

lowerLabel :: Var -> Env Label
lowerLabel var = do
                 (_, labelEnv) <- get
                 return (labelEnv ! var)

putAloc :: (Var, Aloc) -> Env ()
putAloc (var, aloc) = do
                      (alocEnv, labelEnv) <- get
                      let alocEnv' = M.insert var aloc alocEnv
                      put (alocEnv', labelEnv)

putLabel :: (Var, Label) -> Env ()
putLabel (var, label) = do
                        (alocEnv, labelEnv) <- get
                        let labelEnv' = M.insert var label labelEnv
                        put (alocEnv, labelEnv')

genPutAloc :: Var -> Env Aloc
genPutAloc var@(Var template) = do
                                aloc <- lift (genAloc template)
                                putAloc (var, aloc)
                                return aloc
