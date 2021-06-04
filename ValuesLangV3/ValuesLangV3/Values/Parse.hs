module Values.Parse
( Values.Parse.parse
)
where

import Values.Types

import Data.Void
import Text.Megaparsec as M
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer hiding (space)

parse :: String -> String -> Either String Program
parse = ((.) . (.)) (either (Left . errorBundlePretty) Right) parseProg

parseProg :: String -> String -> Either (ParseErrorBundle String Void) Program
parseProg = M.parse prog

prog = Program <$> (space *> expr <* space <* eof)
expr = let' <|> try binop <|> (Triv <$> triv)
let' = Let <$> (string "let" >> assignments)
           <*> (string "in" >> space >> expr)
assignments = (space *> assignment <* space) `sepBy` (char '|')
assignment = (,) <$> (var <* space)
                 <*> (char '=' >> space >> expr)
triv = int <|> (TVar <$> var)
int = Int <$> signed (return ()) decimal
var = Var <$> some alphaNumChar
binop = do
        left <- triv
        space
        op <- op
        space
        right <- triv
        return $ BinOp op left right
op = assocParse ops

ops = [ ("+", Add)
      , ("*", Mul) ]

assocParse :: [(String, a)] -> Parsec Void String a
assocParse [(token, val)] = string token >> return val
assocParse ((token, val) : rest) = (string token >> return val) <|> assocParse rest
