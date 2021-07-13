{-# LANGUAGE BinaryLiterals #-}
module Compiler.Ptrs where
        
import Compiler.Types
import Compiler.Value
import Data.Bits
import Data.Word

data Tag = Tag { tag :: Word64
               , mask :: Word64 }

tagSize :: Tag -> Int
tagSize (Tag _ mask) = finiteBitSize mask - countLeadingZeros mask

intTag :: Tag
intTag = Tag 0b0 0b111

boolTag :: Tag
boolTag = Tag 0b110 0xFF

emptyTag :: Tag
emptyTag = Tag 0b10110 0xFF

voidTag :: Tag
voidTag = Tag 0b11110 0xFF

charTag :: Tag
charTag = Tag 0b101110 0xFF

errorTag :: Tag
errorTag = Tag 0b111110 0xFF

truePtr :: Ptr
truePtr = Ptr (tag + 0b1000)
        where (Tag tag _) = boolTag

falsePtr :: Ptr
falsePtr = Ptr tag
         where (Tag tag _) = boolTag

emptyPtr :: Ptr
emptyPtr = Ptr tag
         where (Tag tag _) = emptyTag

voidPtr :: Ptr
voidPtr = Ptr tag
        where (Tag tag _) = voidTag

toPtr :: Value -> Ptr
toPtr (Int i) = Ptr (tagVal (fromIntegral i) intTag)
toPtr (Bool True) = truePtr
toPtr (Bool False) = falsePtr
toPtr Empty = emptyPtr
toPtr Void = voidPtr
toPtr (Error code) = Ptr (tagVal (fromIntegral code) errorTag)
toPtr (Char c) = Ptr (tagVal ((fromIntegral . fromEnum) c) charTag)

tagVal :: Word64 -> Tag -> Word64
tagVal val tag = setTag (shift val (tagSize tag)) tag

setTag :: Word64 -> Tag -> Word64
setTag val (Tag tag _) = tag .|. val
