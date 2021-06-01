module Asm.Parse where

import Asm.Types

import Data.Map as M
import Data.Set as S
import Data.Void
import Text.Megaparsec as P
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer hiding (space)

parse :: String -> String -> Either String Program
parse fileName = ( >>= ( return
                       . genAddrs
                       . genAlocs) )
                 . parseProg fileName

parseProg :: String -> String -> Either String ([Stmt], Triv)
parseProg = ((.) . (.)) (either (Left . errorBundlePretty) Right) $ P.parse program

program = (,) <$> stmt `sepEndBy` space1
              <*> (string "halt" *> space1 *> triv <* space <* eof)

stmt = do
       aloc <- aloc
       space
       op <- assocParser ops
       space
       val <- triv
       return $ Stmt op aloc val
triv = int <|> (Aloc <$> aloc)
aloc = Var <$> ((notFollowedBy $ string "halt") >> some alphaNumChar)
int = Int64 <$> signed (return ()) decimal

ops = [ (":=", Set)
      , ("+=", Add)
      , ("*=", Mul) ]

assocParser :: [(String, a)] -> Parsec Void String a
assocParser [(token, val)] = string token >> return val
assocParser ((token, val) : rest) = (string token >> return val) <|> assocParser rest

genAlocs :: ([Stmt], Triv) -> (([Stmt], Triv), Set Aloc)
genAlocs (stmts, triv) = ((stmts, triv), S.fromList . getAlocs $ stmts)

getAlocs :: [Stmt] -> [Aloc]
getAlocs [] = []
getAlocs ((Stmt _ aloc _) : rest) = aloc : getAlocs rest

genAddrs :: (([Stmt], Triv), Set Aloc) -> Program
genAddrs (prog, alocs) = Program prog alocs $ M.fromList $ zip (S.toList alocs) [0..]
