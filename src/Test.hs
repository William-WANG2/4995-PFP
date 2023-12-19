module Test(validateAlgorithm) where

import DataStructures (Graph(..), Vertex, Clique, SimpleGraph(..))
import qualified Data.Map as Map

-- Revised readGraph function
readGraph :: String -> IO SimpleGraph
readGraph fileName = do
    contents <- readFile fileName
    let ls = lines contents
    case map read . words $ head ls of
        n:m:_ -> do
            let edges = map (parseEdge . words) $ take m $ tail ls
                adjList = foldl (\acc edge -> case edge of
                                  Just (a, b) -> Map.insertWith (++) a [b] $ Map.insertWith (++) b [a] acc
                                  Nothing -> acc) Map.empty edges
            return $ SimpleGraph n m adjList
        _ -> error "Invalid input format for graph"

-- Function to parse an edge
parseEdge :: [String] -> Maybe (Vertex, Vertex)
parseEdge [a, b] = Just (read a, read b)
parseEdge _      = Nothing


-- Read cliques from a file
readCliques :: String -> IO [Clique]
readCliques fileName = do
    contents <- readFile fileName
    return $ map (map read . words) $ lines contents

-- Check if a set of vertices forms a clique
isClique :: SimpleGraph -> [Vertex] -> Bool
isClique graph vs = all (\(v1, v2) -> is_connected graph v1 v2) [(v1, v2) | v1 <- vs, v2 <- vs, v1 /= v2]

-- Check if a clique is maximal
isMaximalClique :: SimpleGraph -> Clique -> Bool
isMaximalClique graph clique = isClique graph clique &&
    (all (\v -> not (isClique graph $ v:clique)) $ filter (`notElem` clique) $ vertices graph)

-- Validate cliques
validateCliques :: SimpleGraph -> [Clique] -> Bool
validateCliques graph = all (isMaximalClique graph)

-- Main function to validate algorithm output
validateAlgorithm :: String -> String -> IO ()
validateAlgorithm inputFile outputFile = do
    graph <- readGraph inputFile
    cliques <- readCliques outputFile
    putStrLn $ if validateCliques graph cliques then "Valid" else "Invalid"

