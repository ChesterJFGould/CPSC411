module Compiler.Locs where

import Compiler.Types

linkRegister :: Reg
linkRegister = R15

returnRegister :: Reg
returnRegister = RAX

frameRegister :: Reg
frameRegister = RBP

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
callAddresses = stackAddresses

heapPointer :: Rloc
heapPointer = Reg RSP

stackAddresses :: [Addr]
stackAddresses = map Stack [0..]

localLocations :: [Rloc]
localLocations = concat [ map Reg localRegisters
                        , map Addr stackAddresses ]

localRegisters :: [Reg]
localRegisters = [ RAX
                 , RBX
                 , RCX
                 , RDX
                 , RSI
                 , RDI
                 , R8
                 , R9
                 , R13
                 , R14
                 , R15 ]

tempRegister1 :: Reg
tempRegister1 = R10

tempRegister2 :: Reg
tempRegister2 = R11

tempRegister3 :: Reg
tempRegister3 = R12
