module ParBK_Strategy(strategy_basic, strategy_chunked, strategy_depthLimited) where
import Control.Parallel.Strategies
import ParBK_DataStructure (ComputationTree(..))

-- 1. Basic strategy for parallelism

strategy_basic :: Strategy ComputationTree
strategy_basic (Branch state xs) = fmap (Branch state) (parList strategy_basic xs)
strategy_basic (Node clique) = fmap Node (rdeepseq clique)

-- 2. Strategy using parListChunked with chunkSize as a parameter

strategy_chunked :: Int -> Strategy ComputationTree
strategy_chunked chunkSize (Branch state xs) = fmap (Branch state) (parListChunk chunkSize (strategy_chunked chunkSize) xs)
strategy_chunked _ (Node clique) = fmap Node (rdeepseq clique)

-- 3. Strategy using parList with a depth limit as a parameter

strategy_depthLimited :: Int -> Strategy ComputationTree
strategy_depthLimited depth (Branch state xs)
    | depth > 0 = fmap (Branch state) (parList (strategy_depthLimited (depth - 1)) xs)
    | otherwise = fmap (Branch state) (evalList (strategy_depthLimited depth) xs)
strategy_depthLimited _ (Node clique) = fmap Node (rdeepseq clique)