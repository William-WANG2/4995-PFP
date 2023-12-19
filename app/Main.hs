module Main (main) where

import System.Environment (getArgs)
import System.Directory (doesFileExist)
import DataStructures()
import ParBK_Naive (getMaximalCliques)
import SeqBK (getMaximalCliques)
import ParBK_DataStructure(getMaximalCliques)
import ParBK_Strategy(strategy_basic, strategy_chunked, strategy_depthLimited)
import Util (readGraph, printResult_Naive, printResult_DataStructure, printResult_DataStructure_ParBasic, printResult_DataStructure_Par, extractClique, extractCliquePar_Depth, extractCliquePar_Chunk)
import Control.Parallel.Strategies(using)
import GenerateGraph (generateEdges_Random, generateEdges_kClique, writeGraphToFile)
import Test (validateAlgorithm)

main :: IO ()
main = do
    args <- getArgs
    case args of
        -- Generate graph functionality
        "gen":"random":n:outputfile:[] -> generateRandomGraph n outputfile
        "gen":"kclique":n:k:outputfile:[] -> generateKCliqueGraph n k outputfile

        -- Compute maximal clique functionality
        "compute":readpath:writepath:mode -> computeMaximalClique readpath writepath mode

        -- Test functionality
        "test":inputfile:outputfile:[] -> validateAlgorithm inputfile outputfile

        -- Error handling for incorrect usage
        _ -> putStrLn "Usage: gen <mode> <args>..<args> <outfile> | compute <readpath> <writepath> <mode> (<mode arg>) | test <inputfile> <outputfile>"

-- Function to generate a random graph
generateRandomGraph :: String -> String -> IO ()
generateRandomGraph n outputfile = do
    putStrLn $ "Generating random graph with " ++ n ++ " nodes and " ++ "writing to " ++ outputfile
    -- Implement the logic to generate a random graph here
    if ((read n :: Int) < 1)
        then putStrLn "Invalid input: n must be greater than or equal to 1."
        else do
            edges <- generateEdges_Random (read n :: Int)
            writeGraphToFile outputfile (read n :: Int) (length edges) edges
            putStrLn $ "Graph written to: " ++ outputfile

-- Function to generate a k-clique graph
generateKCliqueGraph :: String  -> String -> String -> IO ()
generateKCliqueGraph n k outputfile = do
    putStrLn $ "Generating k-clique graph with " ++ n ++ " nodes, " ++ "with a clique with at least " ++ k ++ " vertices, writing to " ++ outputfile
    -- Implement the logic to generate a k-clique graph here
    if ((read n :: Int) < 1) || ((read n :: Int) < (read k :: Int))
        then putStrLn "Invalid input: n must be greater than or equal to k and n must also be greater than or equal to 1."
        else do
            edges <- generateEdges_kClique (read n :: Int) (read k :: Int)
            putStrLn $ "Edge generated!"
            writeGraphToFile outputfile (read n :: Int) (length edges) edges
            putStrLn $ "Graph written to: " ++ outputfile

-- Function to compute maximal cliques
computeMaximalClique :: String -> String -> [String] -> IO ()
computeMaximalClique readpath writepath mode = do
    fileExists <- doesFileExist readpath
    if not fileExists
        then putStrLn $ "Input file does not exist: " ++ readpath
        else do
            graph <- readGraph readpath
            case mode of
                ["seq"] -> printResult_Naive (SeqBK.getMaximalCliques graph) writepath
                ["par_naive"] -> printResult_Naive (ParBK_Naive.getMaximalCliques graph) writepath
                ["par_basic"] -> printResult_DataStructure_ParBasic ((ParBK_DataStructure.getMaximalCliques graph) `using` ParBK_Strategy.strategy_basic) writepath
                "par_chunked":chunkSize:[] -> printResult_DataStructure_Par extractCliquePar_Chunk (read chunkSize :: Int) ((ParBK_DataStructure.getMaximalCliques graph) `using` ParBK_Strategy.strategy_chunked (read chunkSize :: Int)) writepath
                "par_depthLimited":maxDepth:[] -> printResult_DataStructure_Par extractCliquePar_Depth (read maxDepth :: Int) ((ParBK_DataStructure.getMaximalCliques graph) `using` ParBK_Strategy.strategy_depthLimited (read maxDepth :: Int)) writepath
                _ -> putStrLn "Invalid mode: must be either 'seq', 'par_naive', 'par_basic', 'par_chunked <chunkSize>' or 'par_depthLimited <maxDepth>'"
            putStrLn $ "Results written to: " ++ writepath

