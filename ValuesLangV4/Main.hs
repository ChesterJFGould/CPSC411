module Main where

import Values
import Unique
import Monadic
import Canonical
import Asm
import Nested
import Block
import Para
import Paren

import System.IO
import System.Exit

main :: IO ()
main = do
       prelude <- readFile "Runtime/prelude.s"
       postlude <- readFile "Runtime/postlude.s"
       getContents >>= ( either ( (>> exitFailure)
                                . hPutStrLn stderr)
                                (hPutStrLn stdout)
                       . (>>= return
                              . Paren.compile prelude postlude
                              . Para.lower
                              . Block.lower
                              . Nested.lower
                              . Asm.lower
                              . Canonical.lower
                              . Monadic.lower
                              . Unique.lower
                              . Values.lower)
                       . parse "stdin" )
