module ParBK_Naive(getMaximalCliques) where
import DataStructures (Graph(..), Vertex, Clique)

import Control.Parallel

getMaximalCliques :: Graph graph => graph -> [Clique]
getMaximalCliques graph = bronKerbosch [] (vertices graph) [] where
  bronKerbosch :: Clique -> [Vertex] -> [Vertex] -> [Clique]
  bronKerbosch partialClique candidateVertices excludedVertices
      | null candidateVertices && null excludedVertices = [partialClique]
      | otherwise = exploreCandidates candidateVertices excludedVertices
    where
      exploreCandidates :: [Vertex] -> [Vertex] -> [Clique]
      exploreCandidates [] _ = []
      exploreCandidates (currentVertex : remainingCandidates) currentExcluded =
          let newCliques = bronKerbosch (currentVertex : partialClique)
                                        (remainingCandidates `restrictVertices` currentVertex)
                                        (currentExcluded `restrictVertices` currentVertex)
          in newCliques `par` (exploreCandidates remainingCandidates (currentVertex : currentExcluded) `pseq` (newCliques ++ exploreCandidates remainingCandidates (currentVertex : currentExcluded)))

      restrictVertices :: [Vertex] -> Vertex -> [Vertex]
      restrictVertices curvertices vertex = filter (is_connected graph vertex) curvertices
