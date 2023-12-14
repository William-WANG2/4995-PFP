module SeqBK(getMaximalCliques) where
import DataStructures (Graph(..), Vertex, Clique)

-- | Finds all maximal cliques in a given graph.
getMaximalCliques :: Graph graph => graph -> [Clique]
getMaximalCliques graph = bronKerbosch [] (vertices graph) [] where
  -- | The 'bronKerbosch' function implements the Bron-Kerbosch algorithm to find maximal cliques in an undirected graph.
  -- It takes a partial clique, a set of candidates to be added to the clique, and a set of excluded vertices,
  -- and returns a list of all maximal cliques.
  bronKerbosch :: Clique -> [Vertex] -> [Vertex] -> [Clique]
  bronKerbosch partialClique candidateVertices excludedVertices
      | null candidateVertices && null excludedVertices = [partialClique]
      | otherwise = exploreCandidates candidateVertices excludedVertices
    where
      -- | The 'exploreCandidates' function explores possible cliques by considering each candidate vertex.
      -- It either includes the vertex in the clique and moves it from candidates to excluded,
      -- or moves the vertex directly to excluded without adding it to the clique.
      exploreCandidates :: [Vertex] -> [Vertex] -> [Clique]
      exploreCandidates [] _ = []
      exploreCandidates (currentVertex : remainingCandidates) currentExcluded =
          bronKerbosch (currentVertex : partialClique)
                      (remainingCandidates `restrictVertices` currentVertex)
                      (currentExcluded `restrictVertices` currentVertex) ++
          exploreCandidates remainingCandidates (currentVertex : currentExcluded)

      -- 'restrictVertices' only includes vertices connected to the current vertex.
      restrictVertices :: [Vertex] -> Vertex -> [Vertex]
      restrictVertices curvertices vertex = filter (is_connected graph vertex) curvertices
