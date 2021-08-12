module Exprs.DefCheck
( defCheck
)
where

import Exprs.Types

defCheck :: Program -> Either String Program
defCheck (Program programDef) = Program <$> defCheckDef [] programDef

defCheckDef :: [String] -> ProgramDef -> Either String ProgramDef
defCheckDef _ def@(ProgramBody _) = Right def
defCheckDef names (LetDef def programDef) = do
                                            checkDuplicateDefs names def
                                            LetDef def <$> defCheckDef (name def : names) programDef
defCheckDef names (LetRecDef defs programDef) = do
                                                mapM (checkDuplicateDefs names) defs
                                                LetRecDef <$> mapM (checkIsFuncDef defs) defs
                                                          <*> defCheckDef (map name defs ++ names) programDef

checkIsFuncDef :: [Def] -> Def -> Either String Def
checkIsFuncDef defs def@(Def _ name [] body) = if isLambda body
                                               then Right def
                                               else Left (funcDefError defs name)
checkIsFuncDef defs def@(Def _ _ _ _) = Right def

funcDefError :: [Def] -> Var -> String
funcDefError defs (Var defName) = unwords [ "Definition for"
                                          , defName
                                          , "must be an immediate function"
                                          , "definition as it is in a mutually"
                                          , "recursive group with"
                                          , show names ]
                                where names = map name defs

name :: Def -> String
name (Def _ (Var n) _ _) = n

isLambda :: Body -> Bool
isLambda (Body (Lambda _ _ _)) = True
isLambda (Body _) = False

checkDuplicateDefs :: [String] -> Def -> Either String ()
checkDuplicateDefs names (Def _ (Var name) _ _)
                   | name `elem` names = Left (unwords [ "Duplicate definitions for"
                                                       , name
                                                       ])
                   | otherwise = Right ()
