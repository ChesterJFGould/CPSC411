module Values.Types where

data Program = Program [Func] Tail
             deriving Show

data Func = Func Var [Var] Tail
          deriving Show

data Tail = Expr Expr
          | TLet [(Var, Expr)] Tail
          | TIf Pred Tail Tail
          | Call Var [Triv]
          deriving Show

data Expr = Triv Triv
          | BinOp Op Triv Triv
          | Let [(Var, Expr)] Expr
          | If Pred Expr Expr
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
