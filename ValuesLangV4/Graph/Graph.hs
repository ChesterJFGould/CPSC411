module Graph
( colour
)
where

import qualified Graph.Colour as C
import Graph.FromNodesEdges

import Data.IntMap as IM
import Data.List as L
import Data.Map as M
import Data.Set as S

colour :: Ord a => Set a -> Set (a, a) -> Map a Int
colour nodes edges = colourMap
                   where colourMap = M.fromList
                                     $ zip (L.map (ids IM.!) labels)
                                           colours
	                 (labels, colours) = (unzip . IM.toList . C.colour) graph
	                 (graph, ids) = fromNodesEdges nodes edges
