module Graph.FromNodesEdges
( fromNodesEdges
)
where

import Graph.Types

import Data.IntMap as IM
import Data.List as L
import Data.Map as M
import Data.Maybe
import Data.Set as S

fromNodesEdges :: Ord a => Set a -> Set (a, a) -> (Undirected, IntMap a)
fromNodesEdges nodes edges = (graph, ids)
                           where ids = IM.fromList
                                       $ zip [0..] nodeList
	                         graph = Undirected
	                                 $ IM.fromList
                                         $ zip [0..] (L.map ( S.map (labels M.!)
                                                            . (flip neighbours) edges )
                                                            nodeList)
                                 nodes' = L.map (labels M.!) nodeList
                                 labels = M.fromList
                                          $ zip nodeList [0..]
                                 nodeList = S.toList nodes

neighbours :: Ord a => a -> Set (a, a) -> Set a
neighbours node = S.mapMonotonic fromJust
                  . S.filter isJust
                  . S.map (containsThenOther node)

containsThenOther :: Eq a => a -> (a, a) -> Maybe a
containsThenOther a (b, c)
                  | a == b = Just c
                  | a == c = Just b
                  | otherwise = Nothing
