{-# LANGUAGE BinaryLiterals #-}
module Compiler.Ptrs where

import Compiler.Types
import Data.Bits
import Data.Word

data Tag = Tag { tag :: Word64
               , mask :: Word64 }

tagSize :: Tag -> Int
tagSize (Tag _ mask) = finiteBitSize mask - countLeadingZeros mask

intTag :: Tag
intTag = Tag 0b000 0b111

pairTag :: Tag
pairTag = Tag 0b001 0b111

vectorTag :: Tag
vectorTag = Tag 0b011 0b111

trueTag :: Tag
trueTag = Tag 0b00001110 0xFF

falseTag :: Tag
falseTag = Tag 0b00000110 0xFF

emptyTag :: Tag
emptyTag = Tag 0b00010110 0xFF

voidTag :: Tag
voidTag = Tag 0b00011110 0xFF

charTag :: Tag
charTag = Tag 0b00101110 0xFF

errorTag :: Tag
errorTag = Tag 0b00111110 0xFF

trueLit :: Lit
trueLit = Lit (tag trueTag)

falseLit :: Lit
falseLit = Lit (tag falseTag)

emptyLit :: Lit
emptyLit = Lit (tag emptyTag)

voidLit :: Lit
voidLit = Lit (tag voidTag)
