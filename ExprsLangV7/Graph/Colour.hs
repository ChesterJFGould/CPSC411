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

colour :: (Ord v, Ord c) => Set v -> Set (Node v c, Node v c) -> [c] -> Map v c
colour nodes edges colours = L.foldr (colourNode colours edges) M.empty (S.toList nodes)

colourNode :: (Ord v, Ord c) => [c] -> Set (Node v c, Node v c) -> v -> Map v c -> Map v c
colourNode colours conflicts node assignments = M.insert node nodeColour assignments
                                              where nodeNeighbours = neighbours node conflicts
                                                    nodeConflicts = asColours assignments nodeNeighbours
                                                    nodeColour = mex colours nodeConflicts

neighbours :: (Ord v, Ord c) => v -> Set (Node v c, Node v c) -> Set (Node v c)
neighbours node = S.map fromJust
                  . S.filter isJust
                  . S.map (containsThenOther node)

containsThenOther :: (Eq v, Eq c) => v -> (Node v c, Node v c) -> Maybe (Node v c)
containsThenOther node (a, b)
                  | (Node node) == a = Just b
                  | (Node node) == b = Just a
                  | otherwise = Nothing

asColours :: (Ord v, Ord c) => Map v c -> Set (Node v c) -> Set c
asColours assignments = S.map fromJust
                        . S.filter isJust
                        . S.map (asColour assignments)

asColour :: (Ord v, Ord c) => Map v c -> Node v c -> Maybe c
asColour _ (Colour c) = Just c
asColour assignments (Node node) = M.lookup node assignments

mex :: (Ord c) => [c] -> Set c -> c
mex (c : rest) conflicts
    | c `S.member` conflicts = mex rest conflicts
    | otherwise = c
