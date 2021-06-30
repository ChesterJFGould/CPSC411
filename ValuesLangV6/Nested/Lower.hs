module Nested.Lower
( lower
)
where

import Compiler.Types
import Compiler.Values
import Nested.Types

import qualified Block.Types as B

import Control.Monad.State
import Control.Monad.Writer

type Blocks a = WriterT [B.Block] (State Int) a

lower :: Program -> B.Program
lower (Program defs body) = B.Program defs' body'
                          where (body', defs') = evalState (runWriterT lowerer) 0
                                lowerer = mapM lowerBlock defs >> lowerTail body

lowerBlock :: Block -> Blocks ()
lowerBlock (Block label tail) = do
                                tail' <- lowerTail tail
                                tell [ B.Block label tail' ]

lowerTail :: Tail -> Blocks B.Tail
lowerTail (TSeq stmts tail) = lowerTail tail >>= lowerStmts stmts
lowerTail (TIf p c a) = do
                        tLabel <- lowerTail c >>= label "true"
                        fLabel <- lowerTail a >>= label "false"
                        lowerPred p tLabel fLabel
lowerTail (Jump place) = (return . B.Jump . lowerPlace) place

lowerStmts :: [Stmt] -> B.Tail -> Blocks B.Tail
lowerStmts stmts tail = lowerStmts' tail [] stmts

lowerStmts' :: B.Tail -> [B.Stmt] -> [Stmt] -> Blocks B.Tail
lowerStmts' tail acc [] = return (B.Seq (reverse acc) tail)
lowerStmts' tail acc (Set loc triv : rest) = lowerStmts' tail (B.Set loc (lowerTriv triv) : acc) rest
lowerStmts' tail acc (BinOp op loc triv : rest) = lowerStmts' tail (B.BinOp op loc (lowerTriv triv) : acc) rest
lowerStmts' tail acc (If p c a : rest) = do
                                  afterLabel <- lowerStmts' tail acc rest >>= label "after"
                                  let after = B.Jump (B.PLabel afterLabel)
                                  c' <- lowerStmts c after >>= label "true"
                                  a' <- lowerStmts a after >>= label "false"
                                  lowerPred p c' a'
lowerStmts' tail acc (JumpRet l : rest) = do
                                          returnLabel <- lowerStmts rest tail >>= label "return"
                                          -- We can do this as all registers in use have been saved.
                                          return ( B.Seq (reverse acc)
                                                         (B.Seq [ B.Set (Reg linkRegister) (B.TLabel returnLabel) ]
                                                                (B.Jump (B.PLabel l))) )

lowerPred :: Pred -> Label -> Label -> Blocks B.Tail
lowerPred (Bool True) tLabel fLabel = return (B.Jump (B.PLabel tLabel))
lowerPred (Bool False) tLabel fLabel = return (B.Jump (B.PLabel fLabel))
lowerPred (RelOp op loc triv) tLabel fLabel = return (B.If (B.RelOp op loc (lowerTriv triv)) tLabel fLabel)
lowerPred (Not pred) tLabel fLabel = lowerPred pred fLabel tLabel
lowerPred (PSeq stmts pred) tLabel fLabel = lowerPred pred tLabel fLabel >>= lowerStmts stmts
lowerPred (PIf p c a) tLabel fLabel = do
                                      tLabel' <- lowerPred c tLabel fLabel >>= label "true"
                                      fLabel' <- lowerPred a tLabel fLabel >>= label "false"
                                      lowerPred p tLabel' fLabel'

lowerTriv :: Triv -> B.Triv
lowerTriv (Int i) = B.Int i
lowerTriv (Loc loc) = B.Loc loc

lowerPlace :: Place -> B.Place
lowerPlace (PLabel label) = B.PLabel label
lowerPlace (PLoc loc) = B.PLoc loc

label :: String -> B.Tail -> Blocks Label
label template tail = do
                      label <- lift (gensym template)
                      tell [ B.Block label tail ]
                      return label

gensym :: String -> State Int Label
gensym template = do
                  i <- get
                  let label = Label (template ++ show i)
                  put (i + 1)
                  return label
