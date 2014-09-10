{-
Problem 44

Pentagonal numbers are generated by the formula, 
    Pn=n(3n−1)/2. The first ten pentagonal numbers are:

    1, 5, 12, 22, 35, 51, 70, 92, 117, 145, ...

It can be seen that P4 + P7 = 22 + 70 = 92 = P8. 
However, their difference, 70 − 22 = 48, is not pentagonal.

Find the pair of pentagonal numbers, Pj and Pk, 
for which their sum and difference are pentagonal 
and D = |Pk − Pj| is minimised; what is the value of D?
-}

import Data.List
import Data.Maybe

euler44 = a - b
        where (a,b) = fromJust $ find (\(a,b) -> 
                differencePentagonal a b && sumPentagonal a b)
                [(pentagonal a, pentagonal b) | a <- [1..], b <- [1..a]]

sumPentagonal a b = isPentagonal (a + b)

differencePentagonal a b = isPentagonal (a - b)

pentagonal :: Int -> Int
pentagonal n = (n * (3 * n - 1)) `div` 2

isPentagonal :: Int -> Bool
isPentagonal p = 
    n == (fromIntegral .truncate) n 
    where n = (sqrt ((24 * (fromIntegral p)) + 1) + 1.0) / 6.0

pentagonalGen :: [Int]
pentagonalGen = map (\n -> (n * (3 * n - 1)) `div` 2) [1..]
