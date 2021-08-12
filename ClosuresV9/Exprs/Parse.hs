module Exprs.Parse
( Exprs.Parse.parse
)
where

import Exprs.Sort
import Exprs.Types

import Data.Foldable
import qualified Data.List.NonEmpty as NE
import Data.Void
import Text.Megaparsec as M
import Text.Megaparsec.Char hiding (hspace, symbolChar)
import Text.Megaparsec.Char.Lexer hiding (space, symbol)

import Debug.Trace

type Parser a = Parsec Void String a

exprOps = ( [ binopsl [ ("+", BinOp Add)
                      , ("-", BinOp Sub)
                      ]
            , binopsl [ ("==", BinOp Eq)
                      , ("<=", BinOp Lte)
                      , (">=", BinOp Gte)
                      , ("/=", BinOp Neq)
                      , (">", BinOp Gt)
                      , ("<", BinOp Lt)
                      ]
            , binopsl [ ("*", BinOp Mul) ]
            , application
            ]
          , value
          )

typeOps = ( [ binopsr [ ("->", TFunc) ] ]
          , typeValue
          )

keywords = ["let", "in", "if", "then", "else", "True", "False"]

parse :: String -> String -> Either String Program
parse file input = (either (Left . errorBundlePretty) (Right . sort)) (parseProg file input)

parseProg :: String -> String -> Either (ParseErrorBundle String Void) Program
parseProg = M.parse prog

prog :: Parser Program
prog = Program <$> (LetRecDef <$> ((def <* hspace) `sepEndBy` (char ';'))
                              <*> (ProgramBody <$> (hspace *> body <* hspace <* eof)))

def :: Parser Def
def = do
      name <- try (hspace *> var <* hspace <* char ':')
      hspace
      t <- type'
      hspace
      name' <- var
      if name' /= name
      then let (Var name'') = name'
           in unexpected (Label (NE.fromList name''))
      else return ()
      hspace
      args <- var `sepEndBy` hspace
      char '='
      hspace
      body' <- body
      return (Def t name args body')

body :: Parser Body
body = Body <$> expr

expr :: Parser Expr
expr = precedence exprOps

value :: Parser Expr
value = asum [ let'
             , if'
             , lambda
             , triv
             , parens expr
             ]

let' :: Parser Expr
let' = Let <$> (symbol "let" >> hspace >> var)
           <*> (hspace >> char '=' >> hspace >> expr)
           <*> (hspace >> symbol "in" >> hspace >> expr)

if' :: Parser Expr
if' = If <$> (symbol "if" >> hspace >> expr)
         <*> (hspace >> symbol "then" >> hspace >> expr)
         <*> (hspace >> symbol "else" >> hspace >> expr)

lambda :: Parser Expr
lambda = Lambda <$> (char 'λ' >> hspace >> var)
                <*> (hspace >> char ':' >> hspace >> type')
                <*> (hspace >> char '.' >> hspace >> expr)

triv :: Parser Expr
triv = Value <$> asum [ int
                      , bool
                      , TVar <$> var
                      ]

int :: Parser Value
int = Int <$> decimal

bool :: Parser Value
bool = Bool <$> asum [ symbol "True" >> return True
                     , symbol "False" >> return False
                     ]

var :: Parser Var
var = Var <$> (notFollowedBy (asum (map symbol keywords))
               >> (:) <$> letterChar
                      <*> many symbolChar)
          <?> "variable"

application :: Parser Expr -> Parser Expr
application p = do
                f <- p
                application' f p

application' :: Expr -> Parser Expr -> Parser Expr
application' f p = try ( do
                         hspace
                         arg <- p
                         application' (Apply f arg) p
                       )
                   <|> return f

type' :: Parser Type
type' = precedence typeOps <?> "type"

typeValue = asum [ string "Int" >> return TInt
                 , string "Bool" >> return TBool
                 , parens type'
                 ]

symbol :: String -> Parser String
symbol s = string s <* notFollowedBy symbolChar

parens :: Parser a -> Parser a
parens p = char '(' *> hspace *> p <* hspace <* char ')'

precedence :: ([Parser a -> Parser a], Parser a) -> Parser a
precedence (ops, base) = foldr ($) base ops

binopsl :: [(String, a -> a -> a)] -> Parser a -> Parser a
binopsl ops p = do
                left <- p
                binopsl' left (asum (map binopOp ops)) p

binopsl' :: a -> Parser (a -> a -> a) -> Parser a -> Parser a
binopsl' left op p = try ( do
                           hspace
                           op' <- op
                           hspace
                           right <- p
                           binopsl' (op' left right) op p
                         )
                     <|> return left

binopsr :: [(String, a -> a -> a)] -> Parser a -> Parser a
binopsr ops p = binopsr' (asum (map binopOp ops)) p

binopsr' :: Parser (a -> a -> a) -> Parser a -> Parser a
binopsr' op p = try ( do
                      left <- p
                      hspace
                      op' <- op
                      hspace
                      right <- binopsr' op p
                      return (op' left right)
                    )
                <|> p

binopOp :: (String, a -> a -> a) -> Parser (a -> a -> a)
binopOp (token, op) = string token >> return op

symbolChar :: Parser Char
symbolChar = alphaNumChar <|> char '\'' <|> char '?'

hspace :: Parser ()
hspace = hidden space
