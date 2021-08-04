module Graph.Colour
( colour
, Node (..)
)
where

import qualified Data.Map as M
import Data.Maybe
import qualified Data.Set as S

data Node v c = Node v
              | Colour c
              deriving (Eq, Ord)

colour :: (Ord v, Ord c) => S.Set v -> S.Set (Node v c, Node v c) -> [c] -> M.Map v c
colour nodes edges colours = foldr (colourNode edges colours) M.empty (S.toList nodes)

colourNode :: (Ord v, Ord c) => S.Set (Node v c, Node v c) -> [c] -> v -> M.Map v c -> M.Map v c
colourNode edges colours node assignments = M.insert node nodeColour assignments
                                          where nodeColour = mex colours conflicts
                                                conflicts = neighbourColours assignments node edges

neighbourColours :: (Ord v, Ord c) => M.Map v c -> v -> S.Set (Node v c, Node v c) -> S.Set c
neighbourColours assignments node edges = ( S.map fromJust
                                          . S.filter isJust
                                          . S.map (neighbourColor assignments node)
                                          ) edges

neighbourColor :: (Ord v, Ord c) => M.Map v c -> v -> (Node v c, Node v c) -> Maybe c
neighbourColor assignments node (a, b)
                                | a == Node node = maybeColour b
                                | b == Node node = maybeColour a
                                | otherwise = Nothing
                                where maybeColour (Colour c) = Just c
                                      maybeColour (Node v) = M.lookup v assignments

mex :: Ord c => [c] -> S.Set c -> c
mex (c : rest) conflicts
    | c `S.member` conflicts = mex rest conflicts
    | otherwise = c
