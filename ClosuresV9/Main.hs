module Main where

import Exprs

main :: IO ()
main = getContents >>= putStrLn
                       . either id show
                       . parse "stdin"
