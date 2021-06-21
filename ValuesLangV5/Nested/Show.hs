module Nested.Show where

import Nested.Types

instance Show Program where
         show (Program defs tail) = unlines (map show defs ++ ["main: ", show tail])

instance Show Block where
         show (Block label body) = unlines [show label ++ ":", show body]

instance Show Tail where
         show (Halt triv) = unwords ["halt", show triv]
         show (TSeq stmts tail) = unlines (map show stmts ++ [show tail])
         show (TIf p c a) = unlines [ "if {"
                                    , show p
                                    , "} then {"
                                    , show c
                                    , "} else {"
                                    , show a
                                    , "}"
                                    ]
         show (Jump label) = unwords ["jump", show label]

instance Show Stmt where
         show (Set loc triv) = unwords [show loc, ":=", show triv]
         show (BinOp op loc triv) = unwords [show loc, show op, show triv]
         show (If p c a) = unlines [ "if {"
                                   , show p
                                   , "} then {"
                                   , unlines (map show c)
                                   , "} else {"
                                   , unlines (map show a)
                                   , "}"
                                   ]

instance Show Pred where
         show (Bool b) = show b
         show (RelOp op loc triv) = unwords [show loc, show op, show triv]
         show (Not pred) = unwords ["not", show pred]
         show (PSeq stmts pred) = unlines (map show stmts ++ [show pred])
         show (PIf p c a) = unlines [ "if {"
                                    , show p
                                    , "} then {"
                                    , show c
                                    , "} else {"
                                    , show a
                                    , "}"
                                    ]

instance Show Triv where
         show (Int i) = show i
         show (Loc loc) = show loc

instance Show Loc where
         show (Reg reg) = show reg
         show (Addr i) = unwords ["[ RBP -", show (i * 8), "]"]

instance Show Label where
         show (Label l) = l

instance Show Op where
         show Add = "+="
         show Mul = "*="
