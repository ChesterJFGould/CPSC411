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
expr = let' <|> if' <|> (try binop) <|> (Triv <$> triv)
triv = int <|> (TVar <$> var)
int = Int <$> signed (return ()) decimal
var = Var <$> some alphaNumChar
let' = Let <$> (string "let" >> space >> assignments)
           <*> (space >> string "in" >> space >> expr)
assignments = (space *> assignment <* space) `sepBy` (char '|')
assignment = (,) <$> (var <* space)
                 <*> (string "=" >> space >> expr)
if' = If <$> (string "if" >> space >> predi)
         <*> (space >> string "then" >> space >> expr)
         <*> (space >> string "else" >> space >> expr)
predi = plet <|> pif <|> pnot <|> bool <|> relop
plet = PLet <$> (string "let" >> space >> assignments)
            <*> (space >> string "in" >> space >> predi)
pif = PIf <$> (string "if" >> space >> predi)
          <*> (space >> string "then" >> space >> predi)
          <*> (space >> string "else" >> space >> predi)
pnot = Not <$> (string "not" >> space >> predi)
bool = assocParse [ ("true", Bool True)
                  , ("false", Bool False) ]
relop = do
        left <- triv
        space
        op <- rop
        space
        right <- triv
        return $ RelOp op left right
rops = [ ("<=", Lte)
       , (">=", Gte)
       , ("!=", Neq)
       , ("<", Lt)
       , (">", Gt)
       , ("=", Eq) ]
rop = assocParse rops
binop = do
        left <- triv
        space
        op <- op
        space
        right <- triv
        return $ BinOp op left right
ops = [ ("+", Add)
      , ("*", Mul) ]
op = assocParse ops

assocParse :: [(String, a)] -> Parsec Void String a
assocParse [(token, val)] = string token >> return val
assocParse ((token, val) : rest) = (string token >> return val) <|> assocParse rest
