module Main where

import System.IO (hPutStrLn, stdout, stderr)

import Asm
import Para
import Paren


main :: IO ()
main = do
       prelude <- readFile "prelude.s"
       postlude <- readFile "postlude.s"

       getContents >>= ( either (hPutStrLn stderr)
                                (hPutStrLn stdout)
                       . (>>= return . Paren.compile prelude postlude)
                       . (>>= return . Paren.lower)
                       . (>>= return . Para.lower)
                       . (>>= validate)
                       . parse "stdin" )
