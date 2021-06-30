module Graph.Colour
( colour
, Node (..)
)
where

import Data.List as L
import Data.Map as M
import Data.Maybe
import Data.Set as S

data Node v c = Node v
              | Colour c
              deriving (Eq, Ord)

colour :: (Ord v, Ord c) => [c] -> Set v -> Set (Node v c, Node v c) -> Map v c
colour colours nodes edges = L.foldr (colourNode colours) M.empty nodeNeighbours''
                           where nodesList = S.toList nodes
                                 nodeNeighbours = L.map (flip neighbours edges) nodesList
                                 nodeNeighbours' = zip nodesList nodeNeighbours
                                 nodeNeighbours'' = sortBy secondLength nodeNeighbours'

colourNode :: (Ord v, Ord c) => [c] -> (v, Set (Node v c)) -> Map v c -> Map v c
colourNode colours (node, conflicts) assignments = assignments'
                                                 where colourConflicts = getColours assignments conflicts
                                                       nodeColour = firstNotIn colours colourConflicts
                                                       assignments' = M.insert node nodeColour assignments

getColours :: (Ord v, Ord c) => Map v c -> Set (Node v c) -> Set c
getColours assignments = ( S.map fromJust
                         . S.filter isJust
                         . S.map (colourOrLookup assignments) )

colourOrLookup :: (Ord v) => Map v c -> Node v c -> Maybe c
colourOrLookup assignments (Node v) = M.lookup v assignments
colourOrLookup assignments (Colour c) = Just c

neighbours :: (Ord v, Ord c) => v -> Set (Node v c, Node v c) -> Set (Node v c)
neighbours node = ( S.map fromJust
                  . S.filter isJust
                  . S.map (containsThenOther node) )

containsThenOther :: (Eq v, Eq c) => v -> (Node v c, Node v c) -> Maybe (Node v c)
containsThenOther v (a, b)
                  | Node v == a = Just b
                  | Node v == b = Just a
                  | otherwise = Nothing

secondLength :: Ord t => (a, Set t) -> (a, Set t) -> Ordering
secondLength (_, a) (_, b) = compare (length b) (length a)

firstNotIn :: Ord c => [c] -> Set c -> c
firstNotIn (c : rest) set
           | c `S.member` set = firstNotIn rest set
           | otherwise = c
