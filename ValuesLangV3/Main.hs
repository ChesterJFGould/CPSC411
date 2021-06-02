module Main where

import Values
import Unique
import Cmf
import Asm
import Para
import Paren

main :: IO ()
main = do
       prelude <- readFile "prelude.s"
       postlude <- readFile "postlude.s"
       getContents >>= ( putStrLn
                       . either id id
                       . (>>= return . Paren.compile prelude postlude)
                       . (>>= return . Para.lower)
                       . (>>= return . Asm.lower)
                       . (>>= return . Cmf.lower)
                       . (>>= return . Unique.lower)
                       . (>>= return . Values.lower)
                       . parse "stdin" )
