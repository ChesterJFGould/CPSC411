module Compiler.LowerValue
( lowerValue
)
where

import Compiler.Types

lowerValue :: Value -> ATriv
lowerValue (VInt i) = ALit (Lit (fromIntegral i))
lowerValue (VBool True) = ALit (Lit 1)
lowerValue (VBool False) = ALit (Lit 0)
lowerValue (VAloc aloc) = AAloc aloc
lowerValue (VLabel label) = ALabel label
