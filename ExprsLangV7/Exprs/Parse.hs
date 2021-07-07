module Exprs.Parse
( Exprs.Parse.parse
)
where

import Exprs.Types
import Compiler.Types
import Compiler.Value

import Data.Foldable
import Data.Void
import Text.Megaparsec as M
import Text.Megaparsec.Char hiding (char', hspace, hspace1, symbolChar)
import Text.Megaparsec.Char.Lexer hiding (space, symbol)

precedences = ( [ binops ["<", ">", "==", "<=", ">=", "/="]
                , binops ["+", "-"]
                , binops ["*"]
                , apply ]
              , value )

keywords = [ "let", "in", "if", "then", "else", "true", "false", "error", "void" ]

simpleTrivs = [ ("true", Bool True)
              , ("false", Bool False)
              , ("void", Void) ]

symbolChar = alphaNumChar <|> char '\'' <|> char '?'

hspace = hidden space
hspace1 = hidden space1

parse :: String -> String -> Either String Program
parse = ((.) . (.)) (either (Left . errorBundlePretty) Right) parseProg

parseProg :: String -> String -> Either (ParseErrorBundle String Void) Program
parseProg = M.parse prog


prog = Program <$> (hspace >> (func <* hspace) `sepEndBy` char ';')
               <*> (hspace >> (expr <?> "program body") <* hspace <* eof)
               <?> "program"

func = try (hspace
            >> Func <$> var
                    <*> (hspace >> var `sepEndBy1` hspace1 <* char '='))
                    <*> (hspace >> (expr <?> "function body"))
                    <?> "function definition"

expr = asum [ let'
            , if'
            , precedence precedences ]
       <?> "expression"

let' = Let <$> (symbol "let" >> hspace >> assignments)
           <*> (hspace >> symbol "in" >> hspace >> expr)
           <?> "let expression"

assignments = (hspace *> assignment <* hspace) `sepBy1` char '|'
assignment = (,) <$> var
                 <*> (hspace >> char '=' >> hspace >> expr)
                 <?> "assignment"

if' = If <$> (symbol "if" >> hspace *> expr <* hspace)
         <*> (symbol "then" >> hspace *> expr <* hspace)
         <*> (symbol "else" >> hspace *> expr)
         <?> "if expression"

apply :: Parsec Void String Expr -> Parsec Void String Expr
apply p = try ( Apply <$> (var <* hspace1)
                      <*> ((p <?> "function argument") `sepEndBy1` hspace1) )
          <|> p
          <?> "function application"

value = triv <|> (char '(' *> hspace *> expr <* hspace <* char ')')

var = notFollowedBy (asum (map symbol keywords))
      >> (Var <$> ((:) <$> letterChar
                       <*> many symbolChar))
      <?> "variable"

symbol s = string s <* notFollowedBy symbolChar

triv = Triv <$> ( asum [ TVar <$> var
                       , Value <$> asum [ int
                                        , assoc simpleTrivs
                                        , empty'
                                        , error'
                                        , char' ] ] )
            <?> "value"

int = Int <$> decimal
          <?> "integer"

empty' = (char '[' >> hspace >> char ']') >> return Empty
         <?> "empty list"

error' = Error <$> (symbol "error" >> hspace1 >> (decimal <?> "error code"))
               <?> "error"

char' = Char <$> (char '\'' *> anySingle <* char '\'')
             <?> "char"

precedence :: ([Parsec Void String a -> Parsec Void String a], Parsec Void String a) -> Parsec Void String a
precedence ([], p) = p
precedence (p : rest, base) = p (precedence (rest, base))

binops :: [String] -> Parsec Void String Expr -> Parsec Void String Expr
binops ops p = do
               left <- p
               binops' left (asum (map symbol ops)) p

binops' :: Expr -> Parsec Void String String -> Parsec Void String Expr -> Parsec Void String Expr
binops' left op p = try ( do
                          hspace
                          op' <- op <?> "binary operator"
                          hspace
                          right <- p
                          binops' (Apply (Var op') [left, right]) op p )
                    <|> return left

assoc :: [(String, a)] -> Parsec Void String a
assoc [(token, val)] = symbol token >> return val
assoc ((token, val) : rest) = (symbol token >> return val) <|> assoc rest
