module Compiler.Value where

import Data.Word

data Value = Int Integer
           | Bool Bool
           | Empty
           | Void
           | Error Word8
           | Char Char
           deriving Show

