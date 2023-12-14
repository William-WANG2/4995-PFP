module GenerateGraph(generateEdges_Random, generateEdges_kClique, writeGraphToFile) where
import System.IO()
import System.Random
import qualified Data.Set as Set

-- Function to insert an edge into a set, ensuring no duplicate or reverse edges
insertUniqueEdge :: Set.Set (Int, Int) -> (Int, Int) -> Set.Set (Int, Int)
insertUniqueEdge edgeSet edge@(a, b)
    | a == b || Set.member edge edgeSet || Set.member (b, a) edgeSet = edgeSet
    | otherwise = Set.insert edge edgeSet

-- Function to generate an edge with 50% probability for each pair of vertices
generateEdges_Random :: Int -> IO [(Int, Int)]
generateEdges_Random n = do
    gen <- newStdGen
    let pairs = [(i, j) | i <- [0..n-1], j <- [0..n-1], i < j]
    let (edges, _) = foldl insertEdgeIfRandom (Set.empty, gen) pairs
    return $ Set.toList edges
  where
    insertEdgeIfRandom (acc, gen) (a, b) =
        let (value, newGen) = randomR (0.0 :: Double, 1.0 :: Double) gen in
        if value < 0.5 then (insertUniqueEdge acc (a, b), newGen) else (acc, newGen)
------------------------------------------------------------------------------------------------------------------------


-- Function to shuffle a list in a random manner
shuffle :: [a] -> IO [a]
shuffle x = if length x < 2 then return x else do
    i <- randomRIO (0, length(x)-1)
    r <- shuffle (take i x ++ drop (i+1) x)
    return (x!!i : r)

-- Function to remap the edges with shuffled vertex indices
remapEdges :: [(Int, Int)] -> [Int] -> [(Int, Int)]
remapEdges edges mapping = map (\(a, b) -> (mapping !! (a), mapping !! (b))) edges

-- Function to generate a clique of size k
generateClique :: Int -> Set.Set (Int, Int)
generateClique k = Set.fromList [(i, j) | i <- [0..k-1], j <- [i+1..k-1]]

-- Updated function to generate edges with a clique of size at least k
generateEdges_kClique :: Int -> Int -> IO [(Int, Int)]
generateEdges_kClique n k = do
    gen <- newStdGen
    let clique = generateClique k
    let pairs = [(i, j) | i <- [0..n-1], j <- [0..n-1], i < j, not (Set.member (i, j) clique || Set.member (j, i) clique)]
    let (edges, _) = foldl insertEdgeIfRandom (clique, gen) pairs
    shuffledVertices <- shuffle [0..n-1]
    return $ remapEdges (Set.toList edges) shuffledVertices
  where
    insertEdgeIfRandom (acc, gen) (a, b) =
        let (value, newGen) = randomR (0.0 :: Double, 1.0 :: Double) gen in
        if value < 0.5 then (Set.insert (a, b) acc, newGen) else (acc, newGen)


------------------------------------------------------------------------------------------------------------------------


-- Function to write the graph to a file
writeGraphToFile :: FilePath -> Int -> Int -> [(Int, Int)] -> IO ()
writeGraphToFile filename n m edges = do
    let content = unlines $ (show n ++ " " ++ show m) : map (\(a, b) -> show a ++ " " ++ show b) edges
    writeFile filename content
