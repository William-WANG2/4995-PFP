module Util(readGraph, printResult_Naive, extractClique, printResult_DataStructure, printResult_DataStructure_ParBasic, printResult_DataStructure_Par, extractCliquePar_Depth, extractCliquePar_Chunk) where

import qualified Data.Map as Map
import Data.Maybe (listToMaybe, mapMaybe)
import System.IO()
import DataStructures (Clique, Vertex, SimpleGraph(..), AdjacencyList)
import ParBK_DataStructure (ComputationTree(..))
import Control.Parallel.Strategies (parMap, rdeepseq)
import Data.List.Split (chunksOf)

readGraph :: FilePath -> IO SimpleGraph
readGraph path = do
    contents <- readFile path  -- Read the contents of the file
    let ls = lines contents  -- Split the file contents into lines
    case map read . words <$> listToMaybe ls :: Maybe [Int] of
        Just [n, m] -> do
            let edges = mapMaybe (parseEdge . words) $ tail ls
                adjList = buildAdjacencyList n edges
            return $ SimpleGraph n m adjList
        _ -> error "Invalid input format: first line must contain exactly two integers."

parseEdge :: [String] -> Maybe (Vertex, Vertex)
parseEdge [a, b] = Just (read a, read b)
parseEdge _ = Nothing

-- | Constructs an adjacency list representation of a graph.
-- The adjacency list is a map where each vertex key is mapped to a list of adjacent vertices.
-- This representation is used to efficiently query connections between vertices.
buildAdjacencyList :: Int -> [(Vertex, Vertex)] -> AdjacencyList
buildAdjacencyList n edges = foldl addEdge emptyAdjList edges
  where
    emptyAdjList = Map.fromList [(i, []) | i <- [0..(n-1)]]  -- Initialize an empty adjacency list
    addEdge adj (v1, v2) =
        -- Add each edge to the adjacency list by updating the list of neighbors for both vertices
        Map.adjust (v2:) v1 $ Map.adjust (v1:) v2 adj

printResult_Naive :: [Clique] -> String -> IO ()
printResult_Naive cliques outfile = do
    let numCliques = length cliques
    putStrLn $ "Found " ++ show numCliques ++ " maximal cliques."
    putStrLn $ "Writing results to: " ++ outfile
    writeFile outfile $ unlines $ map showClique cliques
  where
    showClique :: Clique -> String
    showClique = unwords . map show

extractClique :: ComputationTree -> [Clique]
extractClique (Branch _ xs) = concat (map extractClique xs)
extractClique (Node x) = [x]


extractCliquePar :: ComputationTree -> [Clique]
extractCliquePar (Branch _ xs) = concat (parMap rdeepseq extractCliquePar xs)
extractCliquePar (Node x) = [x]

extractCliquePar_Depth :: Int -> ComputationTree -> [Clique]
extractCliquePar_Depth depth (Branch _ xs)
    | depth > 0 = concat (parMap rdeepseq (extractCliquePar_Depth (depth - 1)) xs)
    | otherwise = concatMap (extractCliquePar_Depth depth) xs
extractCliquePar_Depth _ (Node x) = [x]

extractCliquePar_Chunk :: Int -> ComputationTree -> [Clique]
extractCliquePar_Chunk numChunks (Branch _ xs) =
    concat $ parMap rdeepseq (concatMap (extractCliquePar_Chunk numChunks)) (chunkTrees xs numChunks)
extractCliquePar_Chunk _ (Node x) = [x]

chunkTrees :: [ComputationTree] -> Int -> [[ComputationTree]]
chunkTrees trees numChunks = chunksOf chunkSize trees
  where chunkSize = max 1 (length trees `div` numChunks) -- Calculate chunk size, avoid division by zero


printResult_DataStructure :: ComputationTree -> String -> IO ()
printResult_DataStructure tree outfile = do
    let cliques = extractClique tree
        numCliques = length cliques
    putStrLn $ "Found " ++ show numCliques ++ " maximal cliques."
    putStrLn $ "Writing results to: " ++ outfile
    writeFile outfile $ unlines $ map showClique cliques
  where
    showClique :: Clique -> String
    showClique = unwords . map show


printResult_DataStructure_ParBasic :: ComputationTree -> String -> IO ()
printResult_DataStructure_ParBasic tree outfile = do
    let cliques = extractCliquePar tree
        numCliques = length cliques
    putStrLn $ "Found " ++ show numCliques ++ " maximal cliques."
    putStrLn $ "Writing results to: " ++ outfile
    writeFile outfile $ unlines $ map showClique cliques
  where
    showClique :: Clique -> String
    showClique = unwords . map show

printResult_DataStructure_Par :: (Int -> ComputationTree -> [Clique]) -> Int -> ComputationTree -> String -> IO ()
printResult_DataStructure_Par passed_extractCliquePar parameter tree outfile = do
    let cliques = passed_extractCliquePar parameter tree
        numCliques = length cliques
    putStrLn $ "Found " ++ show numCliques ++ " maximal cliques."
    putStrLn $ "Writing results to: " ++ outfile
    writeFile outfile $ unlines $ map showClique cliques
  where
    showClique :: Clique -> String
    showClique = unwords . map show