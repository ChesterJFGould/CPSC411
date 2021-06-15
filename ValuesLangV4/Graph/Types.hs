module Graph.Types where

import Data.Set
import Data.IntMap

data Undirected = Undirected (IntMap (Set Int))
