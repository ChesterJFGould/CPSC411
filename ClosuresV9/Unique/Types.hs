module Unique.Types where

import Compiler.Types

data Program = Program ProgramDef
             deriving Show

data ProgramDef = ProgramBody Body
                | LetDef Def ProgramDef
                | LetRecDef [Def] ProgramDef
                deriving Show

data Def = Def Label [Aloc] Body
         deriving Show

data Body = Body Expr
          deriving Show

data Expr = Value Value
          | BinOp BinOp Expr Expr
          | Apply Expr Expr
          | Let Aloc Expr Expr
          | Lambda Aloc Expr
          | If Expr Expr Expr
          deriving Show
