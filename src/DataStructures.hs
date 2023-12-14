module DataStructures(Graph(..), Vertex, Clique, SimpleGraph(..), AdjacencyList) where
import Data.Map as Map

-- Basic Data Structures
type Vertex = Int
type Clique = [Vertex]
class Graph graph where
    num_vertices :: graph -> Int
    num_edges :: graph -> Int
    vertices :: graph -> [ Vertex ]
    is_connected :: graph -> Vertex -> Vertex -> Bool
    neighbours :: graph -> Vertex -> [ Vertex ]
-- The Graph data structure
type AdjacencyList = Map.Map Vertex [Vertex]
data SimpleGraph = SimpleGraph Int Int AdjacencyList deriving Show

instance Graph SimpleGraph where
    num_vertices (SimpleGraph n _ _) = n
    num_edges (SimpleGraph _ m _) = m
    vertices (SimpleGraph _ _ adjList) = Map.keys adjList
    is_connected (SimpleGraph _ _ adjList) v1 v2 = v2 `elem` (adjList Map.! v1)
    neighbours (SimpleGraph _ _ adjList) v = adjList Map.! v
