module Nested.Lower
( lower
)
where

import Nested.Types

import qualified Block.Types as B

import Control.Monad.State
import Control.Monad.Writer

lower :: Program -> B.Program
lower (Program defs body) = B.Program defs' body'
                          where (body', defs') = (flip evalState) 0
                                                 ( runWriterT (mapM lowerBlock defs
                                                               >> lowerTail body) )

lowerBlock :: Block -> WriterT [B.Block] (State Int) ()
lowerBlock (Block label body) = do
                                body' <- lowerTail body
                                tell [ B.Block (lowerLabel label) body' ]

lowerTail :: Tail -> WriterT [B.Block] (State Int) B.Tail
lowerTail (Halt triv) = return (B.Halt (lowerTriv triv))
lowerTail (TSeq stmts tail) = lowerTail tail >>= lowerStmts stmts
lowerTail (TIf p c a) = do
                        cLabel <- lowerTail c >>= label "cBlock"
                        aLabel <- lowerTail a >>= label "aBlock"
                        lowerPred cLabel aLabel p
lowerTail (Jump label) = return (B.Jump (lowerLabel label))

lowerStmts :: [Stmt] -> B.Tail -> WriterT [B.Block] (State Int) B.Tail
lowerStmts stmts next = lowerStmts' [] next stmts

lowerStmts' :: [B.Stmt] -> B.Tail -> [Stmt] -> WriterT [B.Block] (State Int) B.Tail
lowerStmts' acc next [] = return (B.Seq (reverse acc) next)
lowerStmts' acc next (Set loc triv : rest) = lowerStmts' ( B.Set (lowerLoc loc)
                                                                 (lowerTriv triv)
                                                         : acc )
                                                         next
                                                         rest
lowerStmts' acc next (BinOp op loc triv : rest) = lowerStmts' ( B.BinOp (lowerOp op)
                                                                        (lowerLoc loc)
                                                                        (lowerTriv triv)
                                                              : acc )
                                                              next
                                                              rest
lowerStmts' acc next (If p c a : rest) = do
                                         next' <- lowerStmts rest next
                                         next'' <- label "next" next' >>= return . B.Jump
                                         cLabel <- lowerStmts c next'' >>= label "seq"
                                         aLabel <- lowerStmts a next'' >>= label "seq"
                                         p' <- lowerPred cLabel aLabel p
                                         return (B.Seq (reverse acc) p')

lowerPred :: B.Label -> B.Label -> Pred -> WriterT [B.Block] (State Int) B.Tail
lowerPred tLabel fLabel (Bool True) = return (B.Jump tLabel)
lowerPred tLabel fLabel (Bool False) = return (B.Jump fLabel)
lowerPred tLabel fLabel (RelOp op loc triv) = return (B.If (B.RelOp (lowerRelOp op)
                                                                    (lowerLoc loc)
                                                                    (lowerTriv triv))
                                                           tLabel
                                                           fLabel)
lowerPred tLabel fLabel (Not pred) = lowerPred fLabel tLabel pred
lowerPred tLabel fLabel (PSeq stmts pred) = lowerPred tLabel fLabel pred
                                            >>= lowerStmts stmts
lowerPred tLabel fLabel (PIf p c a) = do
                                      tLabel' <- lowerPred tLabel fLabel c >>= label "t"
                                      fLabel' <- lowerPred tLabel fLabel a >>= label "f"
                                      lowerPred tLabel' fLabel' p

label :: String -> B.Tail -> WriterT [B.Block] (State Int) B.Label
label name body = do
                  i <- lift get
                  let label = B.Label (name ++ show i)
                  lift (put (i + 1))
                  tell [ B.Block label body ]
                  return label

lowerTriv :: Triv -> B.Triv
lowerTriv (Int i) = B.Int i
lowerTriv (Loc loc) = B.Loc (lowerLoc loc)

lowerLoc :: Loc -> B.Loc
lowerLoc (Reg reg) = B.Reg (lowerReg reg)
lowerLoc (Addr i) = B.Addr i

lowerLabel :: Label -> B.Label
lowerLabel (Label label) = B.Label label

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
