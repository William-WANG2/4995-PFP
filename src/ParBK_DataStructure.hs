module ParBK_DataStructure(getMaximalCliques, ComputationTree(..)) where
import DataStructures (Graph(..), Vertex, Clique)

-- | The 'ComputationState' type represents the state of a computation in the Bron-Kerbosch algorithm.
-- It consists of a tuple with three elements:
-- 1. A 'Clique': Represents the current partial clique being explored.
-- 2. A list of 'Vertex': These are the candidate vertices that can potentially be added to the current clique.
-- 3. A list of 'Vertex': These are the vertices that have been excluded from the current clique.
-- This state is used to track the progress of the algorithm at any given point.
type ComputationState = (Clique, [Vertex], [Vertex])

-- | The 'ComputationTree' data type represents a tree structure that is used to model the recursive exploration
-- of cliques in the Bron-Kerbosch algorithm. It has two constructors:
-- 1. 'Branch': Represents a non-terminal node in the computation tree. It contains a 'ComputationState'
--    (which includes the current clique, candidates, and excluded vertices) and a list of 'ComputationTree'
--    elements representing the subsequent states of computation branching from this node.
-- 2. 'Node': Represents a terminal node in the computation tree. It contains a 'Clique', which is a maximal
--    clique that has been found by the algorithm. These nodes are the leaves of the tree where the computation ends.
data ComputationTree = Branch ComputationState [ComputationTree] | Node Clique

-- bronKerbosch :: Clique -> [Vertex] -> [Vertex] -> [Clique]
-- bronKerbosch partialClique candidateVertices excludedVertices
--     | null candidateVertices && null excludedVertices = [partialClique]
--     | otherwise = exploreCandidatess candidateVertices excludedVertices
--   where
--     exploreCandidates :: [Vertex] -> [Vertex] -> [Clique]
--     exploreCandidates [] _ = []
--     exploreCandidates (currentVertex : remainingCandidates) currentExcluded =
--         bronKerbosch (currentVertex : partialClique)
--                      (remainingCandidates `restrictVertices` currentVertex)
--                      (currentExcluded `restrictVertices` currentVertex) ++
--         exploreCandidates remainingCandidates (currentVertex : currentExcluded)

--     restrictVertices :: [Vertex] -> Vertex -> [Vertex]
--     restrictVertices vertices vertex = filter (is_connected graph vertex) vertices


getMaximalCliques :: Graph graph => graph -> ComputationTree
getMaximalCliques graph = bronKerbosch [] (vertices graph) [] where
  bronKerbosch :: Clique -> [Vertex] -> [Vertex] -> ComputationTree
  bronKerbosch partialClique candidateVertices excludedVertices =
    Branch (partialClique, candidateVertices, excludedVertices) $
    if null candidateVertices && null excludedVertices
      then [Node partialClique]
      else exploreCandidates candidateVertices excludedVertices
    where
      exploreCandidates :: [Vertex] -> [Vertex] -> [ComputationTree]
      exploreCandidates [] _ = []
      exploreCandidates (currentVertex : remainingCandidates) currentExcluded =
          bronKerbosch (currentVertex : partialClique)
                      (remainingCandidates `restrictVertices` currentVertex)
                      (currentExcluded `restrictVertices` currentVertex) :
          exploreCandidates remainingCandidates (currentVertex : currentExcluded)

      restrictVertices :: [Vertex] -> Vertex -> [Vertex]
      restrictVertices curvertices vertex = filter (is_connected graph vertex) curvertices