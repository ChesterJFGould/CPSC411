module Values.Types where

import Compiler.Types

data Program = Program [Func] Body
             deriving Show

data Func = Func Var [Var] Body
          deriving Show

data Body = Body Tail
          deriving Show

data Tail = Expr Expr
          | TLet [(Var, Expr)] Tail
          | TIf Pred Tail Tail
          | TCall Var [Triv]
          deriving Show

data Expr = Triv Triv
          | BinOp Op Triv Triv
          | Let [(Var, Expr)] Expr
          | If Pred Expr Expr
          | Call Var [Triv]
          deriving Show

data Pred = Bool Bool
          | RelOp RelOp Triv Triv
          | Not Pred
          | PLet [(Var, Expr)] Pred
          | PIf Pred Pred Pred
          deriving Show

data Triv = Int Integer
          | TVar Var
          deriving Show

data Var = Var String
         deriving (Eq, Ord, Show)
