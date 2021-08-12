module Exprs.Types where

data Type = TInt
          | TBool
          | TFunc Type Type
          deriving Eq

instance Show Type where
         show TInt = "Int"
         show TBool = "Bool"
         show (TFunc a@(TFunc _ _) b) = unwords [ "(" ++ show a ++ ")"
                                                , "->"
                                                , show b
                                                ]
         show (TFunc a b) = unwords [ show a
                                    , "->"
                                    , show b
                                    ]

data Program = Program ProgramDef
             deriving Show

data ProgramDef = ProgramBody Body
                | LetDef Def ProgramDef
                | LetRecDef [Def] ProgramDef
                deriving Show

data Def = Def Type Var [Var] Body
         deriving Show

data Body = Body Expr
          deriving Show

data Expr = Value Value
          | BinOp BinOp Expr Expr
          | Apply Expr Expr
          | Let Var Expr Expr
          | Lambda Var Type Expr
          | If Expr Expr Expr
          deriving Show

data Value = Int Integer
           | Bool Bool
           | TVar Var
           deriving Show

data Var = Var String
         deriving (Eq, Ord, Show)

data BinOp = Add
           | Sub
           | Mul
           | Lt
           | Gt
           | Eq
           | Lte
           | Gte
           | Neq
           deriving Show
