module Compiler.Values where

import Compiler.Types

callLocations :: [Loc]
callLocations = map Reg callRegisters
                ++ map LAddr callAddresses

callRegisters :: [Reg]
callRegisters = [ RDI
                , RSI
                , RDX
                , RCX
                , R8
                , R9 ]

callAddresses :: [Addr]
callAddresses = map Addr [0..]

returnRegister :: Reg
returnRegister = RAX

linkRegister :: Reg
linkRegister = R15

frameRegister :: Reg
frameRegister = RBP

tempRegister1 :: Reg
tempRegister1 = R10

tempRegister2 :: Reg
tempRegister2 = R11

generalLocations :: [Loc]
generalLocations = map Reg generalRegisters
                   ++ map LAddr generalAddresses

generalRegisters :: [Reg]
generalRegisters = [ RSP
                   , RAX
                   , RBX
                   , RCX
                   , RDX
                   , RSI
                   , RDI
                   , R8
                   , R9
                   , R12
                   , R13
                   , R14
                   , R15 ]

generalAddresses :: [Addr]
generalAddresses = map Addr [0..]

