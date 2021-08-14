module Nested.Lower
( lower
)
where

import qualified Blocks.Types as B
import Compiler.Gensym
import qualified Compiler.Locations as Locations
import Compiler.Types
import Nested.Types

import Control.Monad.Writer

type Blocks = WriterT [B.Block] (Gensym)

lower :: Program -> Gensym B.Program
lower (Program labels blocks body) = do
                                     let blocksComputation = do
                                                             mapM lowerBlock blocks
                                                             lowerBody body
                                     (body', blocks') <- runWriterT blocksComputation
                                     return (B.Program labels blocks' body')

lowerBlock :: Block -> Blocks ()
lowerBlock (Block label body) = do
                                body' <- lowerBody body
                                tell [ B.Block label body' ]

lowerBody :: Body -> Blocks B.Body
lowerBody (Body tail) = B.Body <$> lowerTail tail

lowerTail :: Tail -> Blocks B.Tail
lowerTail (Jump triv) = return (B.Jump triv)
lowerTail (TSeq stmts tail) = do
                              tail' <- lowerTail tail
                              lowerStmts tail' stmts
lowerTail (TIf p c a) = do
                        c' <- lowerTail c
                        a' <- lowerTail a
                        lowerIf p c' a'

lowerStmts :: B.Tail -> [Stmt] -> Blocks B.Tail
lowerStmts = lowerStmts' []

lowerStmts' :: [B.Stmt] -> B.Tail -> [Stmt] -> Blocks B.Tail
lowerStmts' acc tail [] = return (B.Seq (reverse acc) tail)
lowerStmts' acc tail (Set loc triv : rest) = lowerStmts' (B.Set loc triv : acc) tail rest
lowerStmts' acc tail (NumOp op loc triv : rest) = lowerStmts' (B.NumOp op loc triv : acc) tail rest
lowerStmts' acc tail (MRef loc ptr offset : rest) = lowerStmts' (B.MRef loc ptr offset : acc) tail rest
lowerStmts' acc tail (MSet ptr offset val : rest) = lowerStmts' (B.MSet ptr offset val : acc) tail rest
lowerStmts' acc tail (JumpRet triv : rest) = do
                                             contLabel <- lowerStmts tail rest >>= label "continuation"
                                             let contTriv = RLabel contLabel
                                                 linkLoc = Reg Locations.linkRegister
                                                 loadLinkStmt = B.Set linkLoc contTriv
                                             return ( B.Seq (reverse acc)
                                                            ( B.Seq [ loadLinkStmt ]
                                                                    ( B.Jump triv )
                                                            )
                                                    )
lowerStmts' acc tail (If p c a : rest) = do
                                  tailLabel <- lowerStmts tail rest >>= label "ifContinuation"
                                  let jumpToTail = B.Jump (RLabel tailLabel)
                                  c' <- lowerStmts jumpToTail c
                                  a' <- lowerStmts jumpToTail a
                                  ifTail <- lowerIf p c' a'
                                  return ( B.Seq (reverse acc)
                                                 ifTail
                                         )

lowerIf :: Pred -> B.Tail -> B.Tail -> Blocks B.Tail
lowerIf p c a = do
                cLabel <- label "consequence" c
                aLabel <- label "alternative" a
                lowerPred p cLabel aLabel

lowerPred :: Pred -> Label -> Label -> Blocks B.Tail
lowerPred (Bool True) cLabel aLabel = return (B.Jump (RLabel cLabel))
lowerPred (Bool False) cLabel aLabel = return (B.Jump (RLabel aLabel))
lowerPred (RelOp op loc triv) cLabel aLabel = return (B.If (B.RelOp op loc triv) cLabel aLabel)
lowerPred (Not pred) cLabel aLabel = lowerPred pred aLabel cLabel
lowerPred (PSeq stmts pred) cLabel aLabel = do
                                            predTail <- lowerPred pred cLabel aLabel
                                            lowerStmts predTail stmts
lowerPred (PIf p c a) cLabel aLabel = do
                                      cTail <- lowerPred c cLabel aLabel
                                      aTail <- lowerPred a cLabel aLabel
                                      lowerIf p cTail aTail

label :: String -> B.Tail -> Blocks Label
label template tail = do
                      tailLabel <- lift (genLabel template)
                      tell [ B.Block tailLabel (B.Body tail) ]
                      return tailLabel
