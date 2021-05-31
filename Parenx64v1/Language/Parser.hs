module Language.Parser
( Language.Parser.parse
)
where

import Language.Types

import Text.Megaparsec as M
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer hiding (space)
import Data.Void

parse :: String -> String -> Either String Program
parse = ((.) . (.)) (either (Left . errorBundlePretty) Right) parseProg

parseProg :: String -> String -> Either (ParseErrorBundle String Void) Program
parseProg = M.parse prog

prog = (Program <$> stmt `sepEndBy` space1) <* eof
stmt = do
       reg <- reg
       space
       op <- op
       space
       val <- triv
       return $ op reg val
op = assocParse ops
triv = (Int <$> int)
       <|> (Reg <$> reg)
int = signed (pure ()) decimal
reg = assocParse regs

ops = [ (":=", Set)
      , ("+=", Add)
      , ("*=", Mult) ]

regs = [ ("RSP", RSP)
       , ("RBP", RBP)
       , ("RAX", RAX)
       , ("RBX", RBX)
       , ("RCX", RCX)
       , ("RDX", RDX)
       , ("RSI", RSI)
       , ("R8", R8)
       , ("R9", R9)
       , ("R10", R10)
       , ("R11", R11)
       , ("R12", R12)
       , ("R13", R13)
       , ("R14", R14)
       , ("R15", R15) ]

assocParse :: [(String, a)] -> Parsec Void String a
assocParse [(s, val)] = string' s >> pure val
assocParse ((s, val) : ps) = (string' s >> pure val) <|> assocParse ps
