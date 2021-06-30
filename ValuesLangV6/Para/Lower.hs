module Para.Lower
( lower
)
where

import Compiler.Types
import Compiler.Values
import Para.Types

import qualified Paren.Types as P

import Data.Int

lower :: Program -> P.Program
lower (Program stmts) = P.Program ((concat . map lowerStmt) stmts)

lowerStmt :: Stmt -> [P.Stmt]
lowerStmt (Set (Reg reg) (Int i)) = [ P.SetReg reg (P.RInt (fromIntegral i)) ]
lowerStmt (Set (Reg a) (Loc (Reg b))) = [ P.SetReg a (P.RReg b) ]
lowerStmt (Set (Reg reg) (Loc (LAddr addr))) = [ P.SetReg reg (P.RAddr addr) ]
lowerStmt (Set (Reg reg) (TLabel label)) = [ P.SetReg reg (P.RLabel label) ]
lowerStmt (Set (LAddr addr) (Int i))
          | fitsInto32 i = [ P.SetAddr addr (P.AInt (fromIntegral i)) ]
          | otherwise = [ P.SetReg tempRegister1 (P.RInt (fromIntegral i))
                        , P.SetAddr addr (P.AReg tempRegister1) ]
lowerStmt (Set (LAddr addr) (Loc (Reg reg))) = [ P.SetAddr addr (P.AReg reg) ]
lowerStmt (Set (LAddr a) (Loc (LAddr b))) = [ P.SetReg tempRegister1 (P.RAddr b)
                                            , P.SetAddr a (P.AReg tempRegister1) ]
lowerStmt (Set (LAddr addr) (TLabel label)) = [ P.SetAddr addr (P.ALabel label) ]
lowerStmt (BinOp op (Reg reg) (Int i)) = [ P.BinOp op reg (P.OInt (fromIntegral i)) ]
lowerStmt (BinOp op (Reg a) (Loc (Reg b))) = [ P.BinOp op a (P.OReg b) ]
lowerStmt (BinOp op (Reg reg) (Loc (LAddr addr))) = [ P.BinOp op reg (P.OAddr addr) ]
lowerStmt (BinOp op (Reg reg) (TLabel label)) = [ P.BinOp op reg (P.OLabel label) ]
lowerStmt (BinOp op (LAddr addr) (Int i))
          | fitsInto32 i = [ P.SetReg tempRegister1 (P.RAddr addr)
                           , P.BinOp op tempRegister1 (P.OInt (fromIntegral i))
                           , P.SetAddr addr (P.AReg tempRegister1) ]
          | otherwise = [ P.SetReg tempRegister1 (P.RAddr addr)
                        , P.SetReg tempRegister2 (P.RInt (fromIntegral i))
                        , P.BinOp op tempRegister1 (P.OReg tempRegister2)
                        , P.SetAddr addr (P.AReg tempRegister1) ]
lowerStmt (BinOp op (LAddr addr) (Loc (Reg reg))) = [ P.SetReg tempRegister1 (P.RAddr addr)
                                                    , P.BinOp op tempRegister1 (P.OReg reg)
                                                    , P.SetAddr addr (P.AReg tempRegister1) ]
lowerStmt (BinOp op (LAddr a) (Loc (LAddr b))) = [ P.SetReg tempRegister1 (P.RAddr a)
                                                 , P.BinOp op tempRegister1 (P.OAddr b)
                                                 , P.SetAddr a (P.AReg tempRegister1) ]
lowerStmt (BinOp op (LAddr addr) (TLabel label)) = [ P.SetReg tempRegister1 (P.RAddr addr)
                                                   , P.BinOp op tempRegister1 (P.OLabel label)
                                                   , P.SetAddr addr (P.AReg tempRegister1) ]
lowerStmt (Compare (Reg reg) (Int i))
          | fitsInto32 i = [ P.Compare reg (P.CInt (fromIntegral i)) ]
          | otherwise = [ P.SetReg tempRegister1 (P.RInt (fromIntegral i))
                        , P.Compare reg (P.CReg tempRegister1) ]
lowerStmt (Compare (Reg a) (Loc (Reg b))) = [ P.Compare a (P.CReg b) ]
lowerStmt (Compare (Reg reg) (Loc (LAddr addr))) = [ P.Compare reg (P.CAddr addr) ]
lowerStmt (Compare (Reg reg) (TLabel label)) = [ P.Compare reg (P.CLabel label) ]
lowerStmt (Compare (LAddr addr) (Int i))
          | fitsInto32 i = [ P.SetReg tempRegister1 (P.RAddr addr)
                           , P.Compare tempRegister1 (P.CInt (fromIntegral i)) ]
          | otherwise = [ P.SetReg tempRegister1 (P.RAddr addr)
                        , P.SetReg tempRegister2 (P.RInt (fromIntegral i))
                        , P.Compare tempRegister1 (P.CReg tempRegister2) ]
lowerStmt (Compare (LAddr addr) (Loc (Reg reg))) = [ P.SetReg tempRegister1 (P.RAddr addr)
                                                   , P.Compare tempRegister1 (P.CReg reg) ]
lowerStmt (Compare (LAddr a) (Loc (LAddr b))) = [ P.SetReg tempRegister1 (P.RAddr a)
                                                , P.Compare tempRegister1 (P.CAddr b) ]
lowerStmt (Compare (LAddr addr) (TLabel label)) = [ P.SetReg tempRegister1 (P.RAddr addr)
                                                  , P.Compare tempRegister1 (P.CLabel label) ]
lowerStmt (JumpIf op label) = [ P.JumpIf op label ]
lowerStmt (Jump (PLabel label)) = [ P.Jump (P.PLabel label) ]
lowerStmt (Jump (PLoc (Reg reg))) = [ P.Jump (P.PReg reg) ]
lowerStmt (Jump (PLoc (LAddr addr))) = [ P.Jump (P.PAddr addr) ]
lowerStmt (Labelled label stmt) = (P.Labelled label hd : tl)
                                where (hd : tl) = lowerStmt stmt

fitsInto32 :: Integer -> Bool
fitsInto32 i = (i >= fromIntegral (minBound :: Int32)) && (i <= fromIntegral (maxBound :: Int32))
