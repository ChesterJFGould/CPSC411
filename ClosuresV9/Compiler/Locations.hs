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
heapRegister = RSP

frameRegister :: Reg
frameRegister = RBP

saveLocations :: [Rloc]
saveLocations = map (Addr . Stack) [0..]

localLocations :: [Rloc]
localLocations = map Reg [ RAX
                         , RBX
                         , RCX
                         , RDX
                         , RSI
                         , RDI
                         , R8
                         , R9
                         , R13
                         , R14
                         , R15
                         ]
                 ++
                 map (Addr . Stack) [0..]

tempRegister1 :: Reg
tempRegister1 = R10

tempRegister2 :: Reg
tempRegister2 = R11

tempRegister3 :: Reg
tempRegister3 = R12
