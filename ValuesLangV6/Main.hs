module Main where

import Asm
import Block
import Canonical
import Monadic
import Nested
import Para
import Paren
import Undead
import Unique
import Values

main :: IO ()
main = do
       prelude <- readFile "Runtime/prelude.s"
       postlude <- readFile "Runtime/postlude.s"
       getContents >>= ( putStrLn
                       . either id id
                       . (>>= return
                              . Paren.compile prelude postlude
                              . Para.lower
                              . Block.lower
                              . Nested.lower
                              . Undead.lower
                              . Asm.lower
                              . Canonical.lower
                              . Monadic.lower
                              . Unique.lower
                              . Values.lower)
                       . (>>= check)
                       . parse "stdin" )
