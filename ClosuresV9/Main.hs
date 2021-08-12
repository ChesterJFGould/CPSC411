module Main where

import Asm
import Bits
import Canonical
import Closures
import Consts
import Exprs
import Monadic
import Pred
import Uniform
import Unique
import Values

import System.Exit
import System.IO
import Text.Pretty.Simple

import Control.Monad
import Control.Monad.State

handleError :: String -> IO ()
handleError msg = do
                  hPutStrLn stderr ("Error: " ++ msg)
                  exitFailure
                  

main :: IO ()
main = getContents >>= either handleError pPrintForceColor
                       . (>>= return
                              . flip evalState 0
                              . liftM Asm.lower
                              . (>>= Canonical.lower)
                              . liftM Monadic.lower
                              . (>>= Values.lower)
                              . (>>= Consts.lower)
                              . liftM Bits.lower
                              . (>>= Pred.lower)
                              . liftM Closures.lower
                              . (>>= Uniform.lower)
                              . liftM Unique.lower
                              . Exprs.lower
                         )
                       . (>>= Exprs.check)
                       . parse "stdin"
