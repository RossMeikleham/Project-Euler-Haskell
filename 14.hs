-- Problem 14
--
-- The following iterative sequence is defined for the set of positive
-- integers:
--
-- n → n/2 (n is even)
-- n → 3n + 1 (n is odd)
--
-- Using the rule above and starting with 13, we generate the following
-- sequence:
-- 13 → 40 → 20 → 10 → 5 → 16 → 8 → 4 → 2 → 1
--
-- It can be seen that this sequence (starting at 13 and finishing at 1)
-- contains 10 terms. Although it has not been proved yet (Collatz
-- Problem), it is thought that all starting numbers finish at 1.
--
-- Which starting number, under one million, produces the longest chain?


import Data.List
import Data.Ord
import qualified Data.Map as Map

euler14 = fst $ maximumBy (comparing snd) $ Map.toList $ f [2..1000000] startMap
    where startMap = Map.singleton 1 1

f :: [Int] -> Map.Map Int Int -> Map.Map Int Int
f [] m = m
f (x:xs) m = f xs (collatz x m) 
    

collatz :: Int -> Map.Map Int Int ->  Map.Map Int Int
collatz n m = collatz' n n 0 m

collatz' :: Int -> Int -> Int -> Map.Map Int Int -> Map.Map Int Int 
collatz' n c l m = 
    case Map.lookup c m of
        Nothing -> collatz' n (nextCollatz c) (l + 1) m
        Just o  -> Map.insert n (l + o) m

nextCollatz :: Int -> Int 
nextCollatz n
    | n `mod` 2 == 0 = n `div` 2
    | otherwise      = (n * 3) + 1

