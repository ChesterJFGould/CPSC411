module Nested.Lower
( lower
)
where

import qualified Compiler.Locs as Locs
import Compiler.Types
import Nested.Types

import qualified Blocks.Types as B

import Control.Monad.State
import Control.Monad.Writer

type Blocks a = WriterT [B.Block] Gensym a

lower :: Program -> Gensym B.Program
lower (Program blocks body) = do
                              (body', blocks') <- runWriterT (mapM lowerBlock blocks >> lowerBody body)
                              return (B.Program blocks' body')

lowerBlock :: Block -> Blocks ()
lowerBlock (Block label body) = (B.Block label <$> lowerBody body) >>= tell . pure

lowerBody :: Body -> Blocks B.Body
lowerBody (Body tail) = B.Body <$> lowerTail tail

lowerTail :: Tail -> Blocks B.Tail
lowerTail (Seq stmts tail) = lowerTail tail >>= lowerStmts stmts
lowerTail (TIf p c a) = do
                        c' <- lowerTail c
                        a' <- lowerTail a
                        lowerIf p c' a'
lowerTail (Jump place) = return (B.Jump place)

lowerStmts :: [Stmt] -> B.Tail -> Blocks B.Tail
lowerStmts stmts tail = lowerStmts' [] tail stmts

lowerStmts' :: [B.Stmt] -> B.Tail -> [Stmt] -> Blocks B.Tail
lowerStmts' acc tail [] = return (B.Seq (reverse acc) tail)
lowerStmts' acc tail (Set loc triv : rest) = lowerStmts' (B.Set loc triv : acc) tail rest
lowerStmts' acc tail (BinOp op loc triv : rest) = lowerStmts' (B.BinOp op loc triv : acc) tail rest
-- This is WRONG, we just discard acc. Oops
lowerStmts' acc tail (If p c a : rest) = do
                                         tail' <- (B.Jump . RLabel) <$> (lowerStmts rest tail >>= label "continuation")
                                         c' <- lowerStmts c tail'
                                         a' <- lowerStmts a tail'
                                         lowerIf p c' a'

lowerStmts' acc tail (JumpRet jumpLabel : rest) = do
                                                  tailLabel <- lowerStmts rest tail >>= label "continuation"
                                                  return (B.Seq (reverse acc)
                                                                (B.Seq [ B.Set (Reg (Locs.link)) (TRLabel tailLabel) ]
                                                                       (B.Jump (RLabel jumpLabel))))

lowerPred :: Pred -> Label -> Label -> Blocks B.Tail
lowerPred (Bool True) tLabel fLabel = return (B.Jump (RLabel tLabel))
lowerPred (Bool False) tLabel fLabel = return (B.Jump (RLabel fLabel))
lowerPred (RelOp op a b) tLabel fLabel = return (B.If (B.RelOp op a b) tLabel fLabel)
lowerPred (Not pred) tLabel fLabel = lowerPred pred fLabel tLabel
lowerPred (PSeq stmts pred) tLabel fLabel = lowerPred pred tLabel fLabel >>= lowerStmts stmts
lowerPred (PIf p c a) tLabel fLabel = do
                                      c' <- lowerPred c tLabel fLabel
                                      a' <- lowerPred a tLabel fLabel
                                      lowerIf p c' a'

lowerIf :: Pred -> B.Tail -> B.Tail -> Blocks B.Tail
lowerIf p c a = join (lowerPred p <$> (label "consequence" c)
                                  <*> (label "alternative" a))

label :: String -> B.Tail -> Blocks Label
label template tail = do
                      i <- lift get
                      let label = Label template i
                      lift (put (i + 1))
                      tell [ B.Block label (B.Body tail) ]
                      return label
