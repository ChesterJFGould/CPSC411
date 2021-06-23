module Values.Parse
( Values.Parse.parse
)
where

import Compiler.Types
import Values.Types

import Text.Megaparsec as M
import Text.Megaparsec.Char hiding (symbolChar, hspace)
import Text.Megaparsec.Char.Lexer hiding (space, symbol)
import Data.Foldable
import Data.Void

binops = [ ("+", Add)
         , ("-", Sub)
         , ("*", Mul) ]

relops = [ ("<", Lt)
         , (">", Gt)
         , ("==", Eq)
         , ("<=", Lte)
         , (">=", Gte)
         , ("/=", Neq) ]

bools = [ ("true", True)
        , ("false", False) ]

keywords = [ "let", "in", "if", "then", "else", "true", "false", "not" ]

symbolChar = alphaNumChar

parse :: String -> String -> Either String Program
parse = ((.) . (.)) (either (Left . errorBundlePretty) Right) parseProg

parseProg :: String -> String -> Either (ParseErrorBundle String Void) Program
parseProg = M.parse prog

hspace :: Parsec Void String ()
hspace = hidden space

prog = Program <$> (hspace >> (func <* hspace) `sepEndBy` char ';')
               <*> (hspace >> (Body <$> (tail' <?> "program body")) <* hspace <* eof)
               <?> "program"

func = try (hspace
            >> Func <$> var
                    <*> (hspace >> var `sepEndBy1` space1 <* char '='))
                    <*> (hspace >> (Body <$> tail'))

tail' = asum [ let' TLet tail'
             , if' TIf tail'
             , try (call TCall)
             , Expr <$> expr ]

expr = asum [ let' Let expr
            , if' If expr
            , try binop
            , try (call Call)
            , Triv <$> triv ]

pred' = asum [ let' PLet pred'
             , if' PIf pred'
             , not'
             , try relop
             , bool ]

triv = asum [ int
            , TVar <$> var ]

let' :: ([(Var, Expr)] -> a -> a) -> (Parsec Void String a) -> Parsec Void String a
let' cons parser = cons <$> (symbol "let" >> hspace >> assignments)
                        <*> (hspace >> symbol "in" >> hspace >> parser)

assignments = (hspace *> assignment <* hspace) `sepBy` char '|'

assignment = (,) <$> var
                 <*> (hspace >> char '=' >> hspace >> expr)

if' :: (Pred -> a -> a -> a) -> (Parsec Void String a) -> Parsec Void String a
if' cons parser = cons <$> (symbol "if" >> hspace >> pred')
                       <*> (hspace >> symbol "then" >> hspace >> parser)
                       <*> (hspace >> symbol "else" >> hspace >> parser)

symbol s = try (string s <* notFollowedBy symbolChar)

call :: (Var -> [Triv] -> a) -> Parsec Void String a
call cons = cons <$> var
                 <*> (hspace >> triv `sepEndBy1` space1)

binop = flip BinOp <$> triv
                   <*> (hspace >> assocParse string binops)
                   <*> (hspace >> triv)

not' = Not <$> (symbol "not" >> space1 >> pred')

relop = flip RelOp <$> triv
                   <*> (hspace >> assocParse string relops)
                   <*> (hspace >> triv)

bool = Bool <$> assocParse symbol bools

int = Int <$> signed (return ()) decimal

var = notFollowedBy keyword
      >> ((.) . (.)) Var (:) <$> letterChar
                             <*> many symbolChar
                             <?> "variable"

keyword = (asum . map symbol) keywords

assocParse :: (String -> Parsec Void String String) -> [(String, a)] -> Parsec Void String a
assocParse parser [(token, val)] = parser token >> return val
assocParse parser ((token, val) : rest) = (parser token >> return val) <|> assocParse parser rest
