module Graph.Colour where

import Control.Monad.State
import Data.List as L
import Data.Map as M
import Data.Maybe
import Data.Set as S

data CNode v c = Colour c
               | Node v
               deriving (Eq, Ord, Show)

colour :: (Ord v, Ord c) => Set v -> Set (CNode v c, CNode v c) -> [c] -> Map v c
colour nodes edges colours = L.foldr (colourNode colours) M.empty nodeConflicts
                           where nodeConflicts = (sortByLength . zip nodes') conflicts'
                                 conflicts' = L.map (conflicts edges) nodes'
                                 nodes' = S.toList nodes

colourNode :: (Ord v, Ord c) => [c] -> (v, [CNode v c]) -> Map v c -> Map v c
colourNode colours (node, conflicts) env = M.insert node colour env
                                         where colour = mex colours colourConflicts
                                               colourConflicts = extractColours env conflicts

conflicts :: (Eq v, Eq c) => Set (CNode v c, CNode v c) -> v -> [CNode v c]
conflicts edges node = ( L.map fromJust
                       . L.filter isJust
                       . L.map ((containsThenOther . Node) node)
                       . S.toList)
                       edges

containsThenOther :: (Eq v, Eq c) => CNode v c -> (CNode v c, CNode v c) -> Maybe (CNode v c)
containsThenOther val (a, b)
                  | a == val = Just b
                  | b == val = Just a
                  | otherwise = Nothing

mex :: (Ord c) => [c] -> Set c -> c
mex (c : rest) conflicts
    | c `S.member` conflicts = mex rest conflicts
    | otherwise = c

extractColours :: (Ord v, Ord c) => Map v c -> [CNode v c] -> Set c
extractColours env = S.map fromJust
                     . S.filter isJust
                     . S.fromList
                     . L.map (asColour env)

asColour :: (Ord v, Ord c) => Map v c -> CNode v c -> Maybe c
asColour env (Colour c) = Just c
asColour env (Node v) = M.lookup v env

sortByLength :: [(v, [c])] -> [(v, [c])]
sortByLength = sortBy (\(_, a) (_, b) -> compare (length b) (length a))
