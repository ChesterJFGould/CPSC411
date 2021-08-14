module Main where

import Asm
import Bits
import Blocks
import Canonical
import Closures
import Consts
import Exprs
import Monadic
import Nested
import Para
import Paren
import Pred
import Undead
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
main = do
       prelude <- readFile "Runtime/prelude.s"
       postlude <- readFile "Runtime/postlude.s"
       getContents >>= either handleError putStrLn
                       . (>>= return
                              . flip evalState 0
                              . liftM (Paren.compile prelude postlude)
                              . liftM Para.lower
                              . liftM Blocks.lower
                              . (>>= Nested.lower)
                              . liftM Undead.lower
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
