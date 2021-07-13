module Para.Lower
( lower
)
where

import qualified Compiler.Locs as Locs
import Compiler.Types
import Para.Types

import qualified Paren.Types as P

import Data.Word

lower :: Program -> P.Program
lower (Program stmts) = P.Program (concat (map lowerStmt stmts))

lowerStmt :: Stmt -> [P.Stmt]
lowerStmt (Set (Reg reg) (RPtr (Ptr ptr))) = [ P.SetReg reg (P.RPtr ptr) ]
lowerStmt (Set (Reg reg) (TRLabel label)) = [ P.SetReg reg (P.RLabel label) ]
lowerStmt (Set (Reg a) (TRloc (Reg b))) = [ P.SetReg a (P.RReg b) ]
lowerStmt (Set (Reg reg) (TRloc (LAddr addr))) = [ P.SetReg reg (P.RAddr addr) ]
lowerStmt (Set (LAddr addr) (RPtr (Ptr ptr)))
          | fitsIntoWord32 ptr = [ P.SetAddr addr (P.APtr (fromIntegral ptr)) ]
          | otherwise = [ P.SetReg Locs.temp1 (P.RPtr ptr)
                        , P.SetAddr addr (P.AReg Locs.temp1) ]
lowerStmt (Set (LAddr addr) (TRLabel label)) = [ P.SetAddr addr (P.ALabel label) ]
lowerStmt (Set (LAddr addr) (TRloc (Reg reg))) = [ P.SetAddr addr (P.AReg reg) ]
lowerStmt (Set (LAddr a) (TRloc (LAddr b))) = [ P.SetReg Locs.temp1 (P.RAddr b)
                                              , P.SetAddr a (P.AReg Locs.temp1) ]
lowerStmt (BinOp op (Reg reg) (RPtr (Ptr ptr)))
          | fitsIntoWord32 ptr = [ P.BinOp op reg (P.OPtr (fromIntegral ptr)) ]
          | otherwise = [ P.SetReg Locs.temp1 (P.RPtr ptr)
                        , P.BinOp op reg (P.OReg Locs.temp1) ]
lowerStmt (BinOp op (Reg reg) (TRLabel label)) = [ P.BinOp op reg (P.OLabel label) ]
lowerStmt (BinOp op (Reg a) (TRloc (Reg b))) = [ P.BinOp op a (P.OReg b) ]
lowerStmt (BinOp op (Reg reg) (TRloc (LAddr addr))) = [ P.BinOp op reg (P.OAddr addr) ]
lowerStmt (BinOp op (LAddr addr) (RPtr (Ptr ptr)))
          | fitsIntoWord32 ptr = [ P.SetReg Locs.temp1 (P.RAddr addr)
                                 , P.BinOp op Locs.temp1 (P.OPtr (fromIntegral ptr))
                                 , P.SetAddr addr (P.AReg Locs.temp1) ]
          | otherwise = [ P.SetReg Locs.temp1 (P.RAddr addr)
                        , P.SetReg Locs.temp2 (P.RPtr ptr)
                        , P.BinOp op Locs.temp1 (P.OReg Locs.temp2)
                        , P.SetAddr addr (P.AReg Locs.temp1) ]
lowerStmt (BinOp op (LAddr addr) (TRLabel label)) = [ P.SetReg Locs.temp1 (P.RAddr addr)
                                                    , P.BinOp op Locs.temp1 (P.OLabel label)
                                                    , P.SetAddr addr (P.AReg Locs.temp1) ]
lowerStmt (BinOp op (LAddr addr) (TRloc (Reg reg))) = [ P.SetReg Locs.temp1 (P.RAddr addr)
                                                      , P.BinOp op Locs.temp1 (P.OReg reg)
                                                      , P.SetAddr addr (P.AReg Locs.temp1) ]
lowerStmt (BinOp op (LAddr a) (TRloc (LAddr b))) = [ P.SetReg Locs.temp1 (P.RAddr a)
                                                   , P.BinOp op Locs.temp1 (P.OAddr b)
                                                   , P.SetAddr a (P.AReg Locs.temp1) ]
lowerStmt (Compare (Reg reg) (RPtr (Ptr ptr)))
          | fitsIntoWord32 ptr = [ P.Compare reg (P.OPtr (fromIntegral ptr)) ]
          | otherwise = [ P.SetReg Locs.temp1 (P.RPtr ptr)
                        , P.Compare reg (P.OReg Locs.temp1) ]
lowerStmt (Compare (Reg reg) (TRLabel label)) = [ P.Compare reg (P.OLabel label) ]
lowerStmt (Compare (Reg a) (TRloc (Reg b))) = [ P.Compare a (P.OReg b) ]
lowerStmt (Compare (Reg reg) (TRloc (LAddr addr))) = [ P.Compare reg (P.OAddr addr) ]
lowerStmt (Compare (LAddr addr) (RPtr (Ptr ptr)))
          | fitsIntoWord32 ptr = [ P.SetReg Locs.temp1 (P.RAddr addr)
                                 , P.Compare Locs.temp1 (P.OPtr (fromIntegral ptr)) ]
          | otherwise = [ P.SetReg Locs.temp1 (P.RAddr addr)
                        , P.SetReg Locs.temp2 (P.RPtr ptr)
                        , P.Compare Locs.temp1 (P.OReg Locs.temp2) ]
lowerStmt (Compare (LAddr addr) (TRLabel label)) = [ P.SetReg Locs.temp1 (P.RAddr addr)
                                                   , P.Compare Locs.temp1 (P.OLabel label) ]
lowerStmt (Compare (LAddr addr) (TRloc (Reg reg))) = [ P.SetReg Locs.temp1 (P.RAddr addr)
                                                     , P.Compare Locs.temp1 (P.OReg reg) ]
lowerStmt (Compare (LAddr a) (TRloc (LAddr b))) = [ P.SetReg Locs.temp1 (P.RAddr a)
                                                  , P.Compare Locs.temp1 (P.OAddr b) ]
lowerStmt (JumpIf op label) = [ P.JumpIf op label ]
lowerStmt (Jump place) = [ P.Jump place ]
lowerStmt (Labelled label stmt) = P.Labelled label hd : tl
                                where (hd : tl) = lowerStmt stmt

fitsIntoWord32 :: Word64 -> Bool
fitsIntoWord32 word = (word >= (fromIntegral (minBound :: Word32))) && (word <= (fromIntegral (maxBound :: Word32)))
