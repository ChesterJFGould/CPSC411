module Exprs.Parse
( Exprs.Parse.parse
)
where

import Compiler.Value
import Exprs.Types

import Data.Foldable
import Data.Void
import Text.Megaparsec as M
import Text.Megaparsec.Char hiding (char', symbolChar, hspace, hspace1)
import Text.Megaparsec.Char.Lexer hiding (symbol, space)

type Parser a = Parsec Void String a

keywords = [ "if", "then", "else", "let", "in", "true", "false", "void", "error" ]

ops = ( [ binopsr ["::"]
        , binopsl ["<", ">", "==", "<=", ">=", "/="]
        , binopsl ["+", "-"]
        , binopsl ["*"]
        , apply ]
      , value )

simpleTrivs = [ ("true", Bool True)
              , ("false", Bool False)
              , ("void", Void) ]

parse :: String -> String -> Either String Program
parse = ((.) . (.)) (either (Left . errorBundlePretty) Right) parseProg

parseProg :: String -> String -> Either (ParseErrorBundle String Void) Program
parseProg = M.parse prog

prog = Program <$> (hspace >> (func <* hspace) `sepEndBy` char ';')
               <*> (hspace >> (body <?> "program body") <* hspace <* eof)

func = try (hspace
            >> Func <$> (var <?> "function name")
                    <*> (hspace >> var `sepEndBy` hspace <* char '='))
                    <*> (hspace >> (body <?> "function body"))

body = Body <$> expr

expr = asum [ let'
            , if'
            , precedence ops ]
       <?> "expression"

let' = Let <$> (symbol "let" >> hspace1 >> assignments)
           <*> (hspace >> symbol "in" >> hspace1 >> expr)
           <?> "let expression"

assignments = (hspace *> assignment <* hspace) `sepBy` char '|'

assignment = (,) <$> var
                 <*> (hspace >> char '=' >> hspace >> expr)
                 <?> "let assignment"

if' = If <$> (symbol "if" >> hspace >> expr)
         <*> (hspace >> symbol "then" >> hspace >> expr)
         <*> (hspace >> symbol "else" >> hspace >> expr)
         <?> "if expression"

apply :: Parser Expr -> Parser Expr
apply p = try ( Apply <$> (var <* hspace1)
                      <*> (p `sepEndBy1` hspace) )
          <|> p
          <?> "function application"

value = triv <|> (char '(' *> hspace *> expr <* hspace <* char ')')
             <?> "value"

triv = Triv <$> asum [ TVar <$> var
                     , Value <$> asum [ int
                                      , assoc simpleTrivs
                                      , empty'
                                      , error'
                                      , char' ]
                             <?> "literal" ]

var = notFollowedBy (asum (map symbol keywords))
      >> ((((.) . (.)) Var (:)) <$> letterChar
                                <*> many symbolChar)

empty' = (char '[' >> hspace >> char ']') >> return Empty

error' = Error <$> (symbol "error" >> hspace1 >> decimal)

char' = Char <$> (char '\'' *> anySingle <* char '\'')

int = Int <$> decimal

symbol s = string s <* notFollowedBy symbolChar

precedence :: ([Parser a -> Parser a], Parser a) -> Parser a
precedence (parsers, base) = foldr ($) base parsers

binopsl :: [String] -> Parser Expr -> Parser Expr
binopsl ops p = do
               left <- p
               binopsl' left (asum (map string ops)) p

binopsl' :: Expr -> Parser String -> Parser Expr -> Parser Expr
binopsl' left op p = do
                    try ( do
                          hspace
                          op' <- op
                          hspace
                          right <- p
                          binopsl' (Apply (Var op') [left, right]) op p )
                    <|> return left

binopsr :: [String] -> Parser Expr -> Parser Expr
binopsr ops p = binopsr' (asum (map string ops)) p

binopsr' :: Parser String -> Parser Expr -> Parser Expr
binopsr' op p = do
                left <- p
                ( try ( do
                      hspace
                      op' <- op
                      hspace
                      right <- binopsr' op p
                      return (Apply (Var op') [left, right]) )
                  <|> return left )

assoc :: [(String, a)] -> Parser a
assoc [(token, val)] = symbol token >> return val
assoc ((token, val) : rest) = (symbol token >> return val) <|> assoc rest

symbolChar = alphaNumChar <|> char '\'' <|> char '?' <|> char '!'

hspace = hidden space

hspace1 = hidden space1
