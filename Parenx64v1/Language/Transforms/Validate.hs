module Language.Transforms.Validate
( validate
)
where

import Language.Types

import Data.Set as S hiding (valid, map, foldr)

validate :: Program -> Either String Program
validate prog@(Program stmts) = validate'  stmts S.empty>> return prog

validate' :: [Stmt] -> Set Reg-> Either String ()
validate' [] env
          | member RAX env = Right ()
          | otherwise = Left "RAX register is not initialized in program"
validate' (stmt : rest) env = valid stmt env >>= validate' rest

valid :: Stmt -> Set Reg -> Either String (Set Reg)
valid stmt env
      | allIn (requires stmt) env = Right $ insertAll (sets stmt) env
      | otherwise = Left (unwords ["uninitialized register used in", show stmt])

sets :: Stmt -> [Reg]
sets (Set reg _) = [reg]
sets _ = []

requires :: Stmt -> [Reg]
requires (Set reg triv) = requiresTriv triv
requires (Add reg triv) = reg : requiresTriv triv
requires (Mult reg triv) = reg : requiresTriv triv

requiresTriv :: Triv -> [Reg]
requiresTriv (Reg reg) = [reg]
requiresTriv _ = []

allIn :: Ord a => [a] -> Set a -> Bool
allIn lst set = and $ map (flip S.member $ set) lst

insertAll :: Ord a => [a] -> Set a -> Set a
insertAll lst set = foldr insert set lst
