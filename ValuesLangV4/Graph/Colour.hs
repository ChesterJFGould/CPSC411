module Graph.Colour
( colour
)
where

import Graph.Types

import Control.Monad.State
import Data.IntMap as IM
import Data.List as L
import Data.Maybe
import Data.Set as S

colour :: Undirected -> IntMap Int
colour (Undirected nodes) = execState (( mapM (uncurry colourNode)
                                       . sortByLength
                                       . IM.toList) nodes)
                                      IM.empty

colourNode :: Int -> Set Int -> State (IntMap Int) ()
colourNode node edges = coloured edges
                        >>= modify' -- Modify state
                            . IM.insert node -- Make modify function to insert (node, mex)
                            . mex -- Get lowest unused colour

coloured :: Set Int -> State (IntMap Int) (Set Int)
coloured nodes = get >>= return
                         . S.map fromJust -- Unpack coloured nodes
                         . S.filter isJust -- Filter coloured nodes
                         . (flip S.map) nodes -- Lookup nodes in state
                         . flip IM.lookup -- Make map function from state

mex :: Set Int -> Int
mex set = [0..] `firstNotIn` set

firstNotIn :: [Int] -> Set Int -> Int
firstNotIn (i : rest) set
           | i `S.member` set = firstNotIn rest set
           | otherwise = i

sortByLength :: [(Int, Set Int)] -> [(Int, Set Int)]
sortByLength = sortBy (\(_, a) (_, b) -> compare (S.size a) (S.size b))
