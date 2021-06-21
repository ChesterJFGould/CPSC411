module Nested.Lower
( lower
)
where

import Nested.Types

import qualified Block.Types as B

import Control.Monad.State
import Control.Monad.Writer

lower :: Program -> B.Program
lower (Program top) = B.Program blocks main
                    where (main, blocks) = (flip evalState) 0 $ runWriterT (lowerTop top >>= label "main")

lowerTop :: Top -> WriterT [B.Block] (State Int) B.Top
lowerTop (Halt triv) = return $ B.Halt $ lowerTriv triv
lowerTop (TIf p c a) = do
                       cLabel <- lowerTop c >>= label "tc"
                       aLabel <- lowerTop a >>= label "ta"
                       lowerPred cLabel aLabel p
lowerTop (Seq stmts body) = lowerTop body >>= lowerStmts stmts

lowerPred :: B.Label -> B.Label -> Pred -> WriterT [B.Block] (State Int) B.Top
lowerPred tLabel fLabel (Bool True) = return $ B.Jump tLabel
lowerPred tLabel fLabel (Bool False) = return $ B.Jump fLabel
lowerPred tLabel fLabel (RelOp op loc triv) = return $ B.If (B.RelOp op' loc' triv')
                                                            tLabel
                                                            fLabel
                                            where op' = lowerRelOp op
                                                  loc' = lowerLoc loc
                                                  triv' = lowerTriv triv
lowerPred tLabel fLabel (Not pred) = lowerPred fLabel tLabel pred
lowerPred tLabel fLabel (PIf p c a) = do
                                      cLabel <- lowerPred tLabel fLabel c >>= label "pc"
                                      aLabel <- lowerPred tLabel fLabel a >>= label "pa"
                                      lowerPred cLabel aLabel p
lowerPred tLabel fLabel (PSeq stmts body) = lowerPred tLabel fLabel body >>= lowerStmts stmts

lowerStmts :: [Stmt] -> B.Top -> WriterT [B.Block] (State Int) B.Top
lowerStmts = lowerStmts' []

lowerStmts' :: [B.Stmt] -> [Stmt] -> B.Top -> WriterT [B.Block] (State Int) B.Top
lowerStmts' acc [] next = return $ B.Seq (reverse acc) next
lowerStmts' acc (Set loc triv : rest) next = lowerStmts' acc' rest next
                                           where acc' = B.Set loc' triv' : acc
                                                 loc' = lowerLoc loc
                                                 triv' = lowerTriv triv
lowerStmts' acc (BinOp op loc triv : rest) next = lowerStmts' acc' rest next
                                                where acc' = B.BinOp op' loc' triv' : acc
                                                      op' = lowerOp op
                                                      loc' = lowerLoc loc
                                                      triv' = lowerTriv triv
lowerStmts' acc (If p c a : rest) next = do
                                         after <- lowerStmts rest next
                                         next' <- label "next" after >>= return . B.Jump
                                         cLabel <- lowerStmts c next' >>= label "seq"
                                         aLabel <- lowerStmts a next' >>= label "seq"
                                         p' <- lowerPred cLabel aLabel p
                                         return $ B.Seq (reverse acc) p'

label :: String -> B.Top -> WriterT [B.Block] (State Int) B.Label
label template top = do
                     i <- get
                     let label = B.Label template i
                     tell [ B.Block label top ]
                     put (i + 1)
                     return label

lowerTriv :: Triv -> B.Triv
lowerTriv (Int i) = B.Int i
lowerTriv (Loc loc) = B.Loc $ lowerLoc loc

lowerLoc :: Loc -> B.Loc
lowerLoc (Reg reg) = B.Reg $ lowerReg reg
lowerLoc (Addr addr) = B.Addr addr

lowerReg :: Reg -> B.Reg
lowerReg RSP = B.RSP
lowerReg RBP = B.RBP
lowerReg RAX = B.RAX
lowerReg RBX = B.RBX
lowerReg RCX = B.RCX
lowerReg RDX = B.RDX
lowerReg RSI = B.RSI
lowerReg RDI = B.RDI
lowerReg R8 = B.R8
lowerReg R9 = B.R9
lowerReg R10 = B.R10
lowerReg R11 = B.R11
lowerReg R12 = B.R12
lowerReg R13 = B.R13
lowerReg R14 = B.R14
lowerReg R15 = B.R15

lowerOp :: Op -> B.Op
lowerOp Add = B.Add
lowerOp Mul = B.Mul

lowerRelOp :: RelOp -> B.RelOp
lowerRelOp Lt = B.Lt
lowerRelOp Gt = B.Gt
lowerRelOp Eq = B.Eq
lowerRelOp Lte = B.Lte
lowerRelOp Gte = B.Gte
lowerRelOp Neq = B.Neq
