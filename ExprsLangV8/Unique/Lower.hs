{-# LANGUAGE BinaryLiterals #-}
module Unique.Lower
( lower
)
where
        
import qualified Bits.Types as B
import Compiler.Gensym
import Compiler.Ptrs
import Compiler.Types as C
import Compiler.Value
import Unique.Types as U

import Data.Bits
import Data.Word

lower :: Program -> Gensym B.Program
lower (Program defs body) = B.Program <$> mapM lowerFunc defs
                                      <*> lowerBody body

lowerFunc :: Func -> Gensym B.Func
lowerFunc (Func name args body) = B.Func name args <$> lowerBody body

lowerBody :: Body -> Gensym B.Body
lowerBody (Body expr) = B.Body <$> lowerExpr expr

lowerExpr :: Expr -> Gensym B.Expr
lowerExpr (Triv triv) = return (B.Triv (lowerTriv triv))
lowerExpr (UnOp op arg) = lowerUnOp op arg
lowerExpr (BinOp op a b) = lowerBinOp op a b
lowerExpr (Apply f args) = B.Apply f <$> mapM lowerExpr args
lowerExpr (Let assignments body) = B.Let <$> lowerAssignments assignments
                                         <*> lowerExpr body
lowerExpr (If p c a) = B.If <$> lowerPred p
                            <*> lowerExpr c
                            <*> lowerExpr a
lowerExpr (Seq stmts body) = B.Seq <$> lowerStmts stmts
                                   <*> lowerExpr body

-- As per usual we can do much better, but I just want to get this done.
lowerPred :: Expr -> Gensym B.Pred
lowerPred expr = do
                 expr' <- lowerExpr expr
                 return (B.RelOp C.Neq expr' (boolExpr False))

lowerStmts :: [Stmt] -> Gensym [B.Stmt]
lowerStmts = mapM lowerStmt

lowerStmt :: Stmt -> Gensym B.Stmt
lowerStmt (SetVec vec index val) = do
                                   vec' <- lowerExpr vec
                                   index' <- lowerExpr index
                                   val' <- lowerExpr val
                                   return (B.MSet (B.BinOp C.And vec' (intExpr (complement 0b111)))
                                                  (B.BinOp C.Mul
                                                           (B.BinOp C.Add
                                                                    (B.BinOp C.Shr index' (intExpr 3))
                                                                    (intExpr 1))
                                                           (intExpr 8))
                                                  val')

lowerAssignments :: [(Aloc, Expr)] -> Gensym [(Aloc, B.Expr)]
lowerAssignments = mapM lowerAssignment

lowerAssignment :: (Aloc, Expr) -> Gensym (Aloc, B.Expr)
lowerAssignment (aloc, expr) = (,) aloc <$> lowerExpr expr

lowerTriv :: Triv -> ATriv
lowerTriv (TAloc aloc) = AAloc aloc
lowerTriv (Value value) = ALit (toLit value)

lowerUnOp :: UnOp -> Expr -> Gensym B.Expr
lowerUnOp IsInt arg = hasTag intTag arg
lowerUnOp IsBool arg = do
                       arg' <- genAloc "tmp"
                       let arg'' = (Triv (TAloc arg'))
                       lowerExpr (If (BinOp U.Eq arg'' (Triv (Value (Bool True))))
                                     (Triv (Value (Bool True)))
                                     (If (BinOp U.Eq arg'' (Triv (Value (Bool False))))
                                         (Triv (Value (Bool True)))
                                         (Triv (Value (Bool False)))))
lowerUnOp IsEmpty arg = hasTag emptyTag arg
lowerUnOp IsVoid arg = hasTag voidTag arg
lowerUnOp IsChar arg = hasTag charTag arg
lowerUnOp IsError arg = hasTag errorTag arg
lowerUnOp IsPair arg = hasTag pairTag arg
lowerUnOp IsVector arg = hasTag vectorTag arg
lowerUnOp Not arg = lowerExpr (If arg (Triv (Value (Bool False))) (Triv (Value (Bool True))))
lowerUnOp Car arg = do
                    arg' <- lowerExpr arg
                    return (B.MRef (B.BinOp And arg' (intExpr (complement 0b111)))
                                   (B.Triv (ALit (Lit 0))))
lowerUnOp Cdr arg = do
                    arg' <- lowerExpr arg
                    return (B.MRef (B.BinOp And arg' (intExpr (complement 0b111)))
                                   (B.Triv (ALit (Lit 8))))
lowerUnOp MakeVector arg = do
                           arg' <- lowerExpr arg
                           return (giveTag vectorTag (B.Alloc (B.BinOp C.Mul
                                                                       (B.BinOp C.Add
                                                                                arg'
                                                                                (intExpr 1))
                                                                       (intExpr 8))))
lowerUnOp VectorLength arg = do
                             arg' <- lowerExpr arg
                             return (B.MRef (B.BinOp C.And arg' (intExpr (complement 0xF)))
                                            (intExpr 0))

hasTag :: Tag -> Expr -> Gensym B.Expr
hasTag (Tag tag mask) expr = do
                             expr' <- lowerExpr expr
                             return (B.If (B.RelOp C.Eq
                                                   (B.BinOp C.And
                                                            expr'
                                                            (intExpr mask))
                                                   (intExpr tag))
                                          (boolExpr True)
                                          (boolExpr False))

lowerBinOp :: U.BinOp -> Expr -> Expr -> Gensym B.Expr
lowerBinOp U.Add a b = B.BinOp C.Add <$> lowerExpr a
                                     <*> lowerExpr b
lowerBinOp U.Sub a b = B.BinOp C.Sub <$> lowerExpr a
                                     <*> lowerExpr b
lowerBinOp U.Mul a b = do
                       a' <- lowerExpr a
                       b' <- lowerExpr b
                       return (B.BinOp C.Mul
                                       (B.BinOp C.Shr
                                                a'
                                                (intExpr 3))
                                       b')
lowerBinOp U.Lt a b = (((.) . (.)) predExpr (B.RelOp C.Lt)) <$> lowerExpr a
                                                            <*> lowerExpr b
lowerBinOp U.Gt a b = (((.) . (.)) predExpr (B.RelOp C.Gt)) <$> lowerExpr a
                                                            <*> lowerExpr b
lowerBinOp U.Eq a b = (((.) . (.)) predExpr (B.RelOp C.Eq)) <$> lowerExpr a
                                                            <*> lowerExpr b
lowerBinOp U.Lte a b = (((.) . (.)) predExpr (B.RelOp C.Lte)) <$> lowerExpr a
                                                              <*> lowerExpr b
lowerBinOp U.Gte a b = (((.) . (.)) predExpr (B.RelOp C.Gte)) <$> lowerExpr a
                                                              <*> lowerExpr b
lowerBinOp U.Neq a b = (((.) . (.)) predExpr (B.RelOp C.Neq)) <$> lowerExpr a
                                                              <*> lowerExpr b
lowerBinOp Cons a b = do
                      tmp <- genAloc "tmp"
                      let tmp' = B.Triv (AAloc tmp)
                      a' <- lowerExpr a
                      b' <- lowerExpr b
                      return (B.Let [(tmp, B.Alloc (intExpr 16))]
                                    (B.Seq [ B.MSet tmp' (intExpr 0) a'
                                           , B.MSet tmp' (intExpr 8) b' ]
                                           (giveTag pairTag tmp')))
lowerBinOp VectorRef a b = do
                           a' <- lowerExpr a
                           b' <- lowerExpr b
                           return (B.MRef (B.BinOp C.And a' (intExpr (complement 0b111)))
                                          (B.BinOp C.Mul
                                                   (B.BinOp C.Add
                                                            (B.BinOp C.Shr
                                                                     b'
                                                                     (intExpr 3))
                                                            (intExpr 1))
                                                   (intExpr 8)))

predExpr :: B.Pred -> B.Expr
predExpr pred = B.If pred (boolExpr True) (boolExpr False)

intExpr :: Word64 -> B.Expr
intExpr = B.Triv . ALit . Lit

boolExpr :: Bool -> B.Expr
boolExpr = B.Triv . lowerTriv . Value . Bool

giveTag :: Tag -> B.Expr -> B.Expr
giveTag (Tag tag mask) expr = B.BinOp C.Ior
                                      (B.BinOp C.And expr (intExpr (complement mask)))
                                      (intExpr tag)
