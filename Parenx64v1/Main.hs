module Main where

import Language

main :: IO ()
main = getContents >>= ( putStrLn
                       . either id compile
                       . (>>= validate)
                       . parse "repl" )
