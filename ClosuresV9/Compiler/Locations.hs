module Compiler.Locations where

import Compiler.Types

linkRegister :: Reg
linkRegister = R15

envRegister :: Reg
envRegister = RDI

argRegister :: Reg
argRegister = RSI

returnRegister :: Reg
returnRegister = RAX

heapRegister :: Reg
heapRegister = RBP
