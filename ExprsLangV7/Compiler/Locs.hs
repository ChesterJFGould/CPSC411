module Compiler.Locs where

import Compiler.Types

return :: Reg
return = RAX

link :: Reg
link = R15

frame :: Reg
frame = RBP

call :: [Rloc]
call = concat [ map Reg callRegs
              , map LAddr callAddrs ]

callRegs :: [Reg]
callRegs = [ RDI
           , RSI
           , RDX
           , RCX
           , R8
           , R9 ]

callAddrs :: [Addr]
callAddrs = map Addr [0..]

save :: [Rloc]
save = map (LAddr . Addr) [0..]

general :: [Rloc]
general = concat [ map Reg generalRegs
                 , map LAddr generalAddrs ]

generalRegs :: [Reg]
generalRegs = [ RSP
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

generalAddrs :: [Addr]
generalAddrs = map Addr [0..]

temp1 :: Reg
temp1 = R10

temp2 :: Reg
temp2 = R11
