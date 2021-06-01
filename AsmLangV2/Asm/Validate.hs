module Asm.Validate
( validate
)
where

import Asm.Types

import Data.Set hiding (valid, map, foldr)

iff pred c a val = if pred val then c else a

validate :: Program -> Either String Program
validate prog@(Program (stmts, (Aloc out)) _ _) = validate' stmts empty
                                                  >>= iff (member out)
                                                          (Right prog)
                                                          (Left $ unwords [ "uninitialized variable used in: halt"
                                                                          , show out ])
validate prog@(Program (stmts, _) _ _) = validate' stmts empty >> Right prog

validate' :: [Stmt] -> Set Aloc -> Either String (Set Aloc)
validate' [] env = return env
validate' (stmt : rest) env = valid stmt env >>= validate' rest

valid :: Stmt -> Set Aloc -> Either String (Set Aloc)
valid stmt env
      | allInitialized (requires stmt) env = Right $ insertAll (initializes stmt) env
      | otherwise = Left (unwords ["uninitialized variable used in:", show stmt])

initializes :: Stmt -> [Aloc]
initializes (Stmt Set aloc _) = [aloc]
initializes _ = []

requires :: Stmt -> [Aloc]
requires (Stmt Set _ triv) = requiresTriv triv
requires (Stmt _ aloc triv) = aloc : requiresTriv triv

requiresTriv :: Triv -> [Aloc]
requiresTriv (Aloc aloc) = [aloc]
requiresTriv _ = []

allInitialized :: [Aloc] -> Set Aloc -> Bool
allInitialized alocs env = and $ map (flip member $ env) alocs

insertAll :: [Aloc] -> Set Aloc -> Set Aloc
insertAll = flip $ foldr insert
