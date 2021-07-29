module Compiler.Value where

import Compiler.Ptrs
import Compiler.Types
import Data.Bits
import Data.Word

data Value = Int Integer
           | Bool Bool
           | Empty
           | Void
           | Error Word8
           | Char Char
           deriving Show

toLit :: Value -> Lit
toLit (Int i) = Lit (tagVal (fromIntegral i) intTag)
toLit (Bool True) = trueLit
toLit (Bool False) = falseLit
toLit Empty = emptyLit
toLit Void = voidLit
toLit (Error code) = Lit (tagVal (fromIntegral code) errorTag)
toLit (Char c) = Lit (tagVal ((fromIntegral . fromEnum) c) charTag)

tagVal :: Word64 -> Tag -> Word64
tagVal val tag = setTag (shift val (tagSize tag)) tag

setTag :: Word64 -> Tag -> Word64
setTag val (Tag tag _) = tag .|. val
