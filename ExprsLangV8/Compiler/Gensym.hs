module Compiler.Gensym where

import Compiler.Types

import Control.Monad.State
import Data.Int

type Gensym a = State Int64 a

genAloc :: String -> Gensym Aloc
genAloc template = do
                   i <- get
                   let aloc = Aloc template i
                   put (i + 1)
                   return aloc

genLabel :: String -> Gensym Label
genLabel template = do
                    i <- get
                    let label = Label template i
                    put (i + 1)
                    return label
