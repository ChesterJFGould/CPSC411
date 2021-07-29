module Compiler.Locs where

import Compiler.Types

linkRegister :: Reg
linkRegister = R15

returnRegister :: Reg
returnRegister = RAX

callLocations :: [Rloc]
callLocations = concat [ map Reg callRegisters
                       , map Addr callAddresses ]

callRegisters :: [Reg]
callRegisters = [ RDI
                , RSI
                , RDX
                , RCX
                , R8
                , R9 ]

callAddresses :: [Addr]
callAddresses = map Stack [0..]

heapPointer :: Rloc
heapPointer = Reg RSP
