module Main where

import Bits
import Exprs
import Unique

import Control.Monad
import Control.Monad.State

main :: IO ()
main = getContents >>= putStrLn
                       . either id show
                       . ( >>= return
                               . flip evalState 0
                               . (>>= Bits.lower)
                               . (>>= return . Unique.lower)
                               . Exprs.lower )
                       . parse "stdin"
