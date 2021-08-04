module Nested.Lower
( lower
)
where

import qualified Blocks.Types as B
import Compiler.Gensym
import Compiler.Locs
import Compiler.Types
import Nested.Types

import Control.Monad.Writer

type Blocks a = WriterT [B.Block] Gensym a

lower :: Program -> Gensym B.Program
lower (Program blocks body) = do
                              (body', blocks') <- runWriterT (mapM lowerBlock blocks
                                                              >> lowerBody body)
                              return (B.Program blocks' body')

lowerBlock :: Block -> Blocks ()
lowerBlock (Block label body) = do
                                body' <- lowerBody body
                                tell [ B.Block label body' ]

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
lowerStmts' acc tail (MSet ptr offset val : rest) = lowerStmts' (B.MSet ptr offset val : acc) tail rest
lowerStmts' acc tail (MRef loc ptr offset : rest) = lowerStmts' (B.MRef loc ptr offset : acc) tail rest
lowerStmts' acc tail (JumpRet label' : rest) = do
                                               contLabel <- lowerStmts rest tail >>= label "continuation"
                                               return (B.Seq (reverse acc)
                                                             (B.Seq [ B.Set (Reg linkRegister)
                                                                            (RLabel contLabel) ]
                                                                    (B.Jump (PLabel label'))))
lowerStmts' acc tail (If p c a : rest) = do
                                         tail' <- (B.Jump . PLabel) <$> (lowerStmts rest tail >>= label "continuation")
                                         c' <- lowerStmts c tail'
                                         a' <- lowerStmts a tail'
                                         B.Seq (reverse acc) <$> lowerIf p c' a'

lowerPred :: Pred -> Label -> Label -> Blocks B.Tail
lowerPred (Bool True) tLabel _ = return (B.Jump (PLabel tLabel))
lowerPred (Bool False) _ fLabel = return (B.Jump (PLabel fLabel))
lowerPred (RelOp op loc triv) tLabel fLabel = return (B.If (B.RelOp op loc triv)
                                                           tLabel
                                                           fLabel)
lowerPred (Not pred) tLabel fLabel = lowerPred pred fLabel tLabel
lowerPred (PSeq stmts pred) tLabel fLabel = lowerPred pred tLabel fLabel >>= lowerStmts stmts
lowerPred (PIf p c a) tLabel fLabel = do
                                      c' <- lowerPred c tLabel fLabel
                                      a' <- lowerPred a tLabel fLabel
                                      lowerIf p c' a'

lowerIf :: Pred -> B.Tail -> B.Tail -> Blocks B.Tail
lowerIf p c a = do
                cLabel <- label "consequence" c
                aLabel <- label "alternative" a
                lowerPred p cLabel aLabel

label :: String -> B.Tail -> Blocks Label
label template tail = do
                      label <- lift (genLabel template)
                      tell [ B.Block label (B.Body tail) ]
                      return label
