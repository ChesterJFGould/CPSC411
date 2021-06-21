module Values.Parse
( Values.Parse.parse
)
where

import Values.Types

import Data.Foldable
import Debug.Trace
import Data.Void
import Text.Megaparsec as M
import Text.Megaparsec.Char hiding (symbolChar, hspace)
import Text.Megaparsec.Char.Lexer hiding (space, symbol)

symbolChar = alphaNumChar

binops = [ ("+", Add)
         , ("*", Mul) ]

relops = [ ("<", Lt)
         , (">", Gt)
         , ("==", Eq)
         , ("<=", Lte)
         , (">=", Gte)
         , ("/=", Neq) ]

bools = [ ("true", True)
        , ("false", False) ]

keywords = ["let", "in", "if", "then", "else", "true", "false"]

hspace :: Parsec Void String ()
hspace = hidden space

parse :: String -> String -> Either String Program
parse = ((.) . (.)) (either (Left . errorBundlePretty) Right) parseProg

parseProg :: String -> String -> Either (ParseErrorBundle String Void) Program
parseProg = M.parse prog

prog = Program <$> (hspace >> (func <* hspace) `sepEndBy` char ';')
               <*> (hspace >> (tail' <?> "program body") <* hspace <* eof)
               <?> "program"

func = try (hspace
            >> Func <$> var
                    <*> (hspace >> var `sepEndBy1` hspace <* char '='))
                    <*> (hspace >> (tail' <?> "function body"))
                    <?> "function definition"


tail' = asum [ let' TLet tail'
             , if' TIf tail'
             , try call
             , Expr <$> expr ]
             <?> "tail"

expr = asum [ let' Let expr
            , if' If expr
            , try binop
            , Triv <$> triv ]
            <?> "expr"

pred' = asum [ let' PLet pred'
             , if' PIf pred'
             , not'
             , relop
             , bool ]
             <?> "pred"

triv = (int <|> (TVar <$> var))
       <?> "triv"

let' :: ([(Var, Expr)] -> a -> b) -> Parsec Void String a -> Parsec Void String b
let' cons body = cons <$> (symbol "let" >> hspace >> assignments)
                      <*> (hspace >> symbol "in" >> hspace >> body)
                      <?> "let expression"

assignments = (hspace *> assignment <* space) `sepBy` (char '|')

assignment = (,) <$> var
                 <*> (hspace >> char '=' >> hspace >> expr)
                 <?> "assignment"

if' :: (Pred -> a -> a -> b) -> Parsec Void String a -> Parsec Void String b
if' cons body = cons <$> (symbol "if" >> hspace >> pred')
                     <*> (hspace >> symbol "then" >> hspace >> body)
                     <*> (hspace >> symbol "else" >> hspace >> body)
                     <?> "if expression"

symbol s = try $ string s <* notFollowedBy symbolChar

call = Call <$> var
            <*> (hspace >> triv `sepEndBy1` space1)
            <?> "function call"

binop = flip BinOp <$> triv
                   <*> (hspace >> (assocParse string binops <?> "binary operator"))
                   <*> (hspace >> triv)
                   <?> "binary operation"

relop = flip RelOp <$> triv
                   <*> (hspace >> (assocParse string relops <?> "relational operator"))
                   <*> (hspace >> triv)
                   <?> "relational operation"

not' = symbol "not" >> hspace >> pred'
       <?> "not predicate"

bool = Bool <$> (assocParse symbol bools)
            <?> "boolean"

var = notFollowedBy keyword
      >> (((.) . (.)) Var (:)) <$> letterChar
                               <*> many symbolChar
                               <?> "variable"


keyword = (asum $ map symbol keywords)
          <?> "keyword"

int = Int <$> (try (signed (return ()) decimal) <* notFollowedBy symbolChar)
          <?> "integer"

assocParse :: (String -> Parsec Void String String) -> [(String, a)] -> Parsec Void String a
assocParse p [(token, val)] = p token >> return val
assocParse p ((token, val) : rest) = (p token >> return val) <|> assocParse p rest
