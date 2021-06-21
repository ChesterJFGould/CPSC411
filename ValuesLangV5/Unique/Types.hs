module Unique.Types where

data Program = Program [Func] Tail
             deriving Show

data Func = Func Label [Aloc] Tail
          deriving Show

data Tail = Expr Expr
          | TLet [(Aloc, Expr)] Tail
          | TIf Pred Tail Tail
          | Call Label [Triv]
          deriving Show

data Expr = Triv Triv
          | BinOp Op Triv Triv
          | Let [(Aloc, Expr)] Expr
          | If Pred Expr Expr
          deriving Show

data Pred = Bool Bool
          | RelOp RelOp Triv Triv
          | Not Pred
          | PLet [(Aloc, Expr)] Pred
          | PIf Pred Pred Pred
          deriving Show

data Triv = Int Integer
          | TAloc Aloc
          deriving Show

data Aloc = Aloc String Int
          deriving Show

data Label = Label String
           deriving Show

data Op = Add
        | Mul
        deriving Show

data RelOp = Lt
           | Gt
           | Eq
           | Lte
           | Gte
           | Neq
           deriving Show
