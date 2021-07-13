module Main where

import Asm
import Bits
import Blocks
import Canonical
import Exprs
import Monadic
import Nested
import Para
import Paren
import Undead
import Unique
import Values

import Control.Monad
import Control.Monad.State
import Text.Pretty.Simple

main :: IO ()
main = do
       prelude <- readFile "Runtime/prelude.s"
       postlude <- readFile "Runtime/postlude.s"
       getContents >>= putStrLn
                       . either id id
                       . ( >>= return
                               . flip evalState 0
                               . liftM (Paren.compile prelude postlude)
                               . liftM Para.lower
                               . liftM Blocks.lower
                               . (>>= Nested.lower)
                               . liftM Undead.lower
                               . liftM Asm.lower
                               . (>>= Canonical.lower)
                               . liftM Monadic.lower
                               . liftM Values.lower
                               . (>>= Bits.lower)
                               . liftM Unique.lower
                               . Exprs.lower )
                       . parse "stdin"
