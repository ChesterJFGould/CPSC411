module Main where

import Monadic
import Unique
import Values

main :: IO ()
main = getContents >>= ( putStrLn
                       . either id show
                       . (>>= return
                              . Monadic.lower
                              . Unique.lower
                              . Values.lower)
                       . (>>= check)
                       . parse "stdin" )
