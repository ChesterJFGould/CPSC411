module Main where

import Asm
import Bits
import Canonical
import Exprs
import Monadic
import Unique
import Values

import Control.Monad
import Control.Monad.State
import Text.Pretty.Simple

main :: IO ()
main = getContents >>= either putStrLn pPrintForceColor
                       . (>>= return
                              . flip evalState 0
                              . liftM Asm.lower
                              . (>>= Canonical.lower)
                              . liftM Monadic.lower
                              . (>>= Values.lower)
                              . (>>= Bits.lower)
                              . (>>= Unique.lower)
                              . Exprs.lower)
                       . parse "stdin"
