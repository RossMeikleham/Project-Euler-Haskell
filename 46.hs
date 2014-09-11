{-
Problem 46
 It was proposed by Christian Goldbach that 
 every odd composite number can be written as the sum of a prime and twice a square.

9 = 7 + 2×1^2
15 = 7 + 2×2^2
21 = 3 + 2×3^2
25 = 7 + 2×3^2
27 = 19 + 2×2^2
33 = 31 + 2×1^2

It turns out that the conjecture was false.

What is the smallest odd composite that cannot be written 
as the sum of a prime and twice a square?
-}

-- We can exclude 2 as a prime as 2 x n is even, and to make
-- and odd composite would need to add an odd prime

import Primes
import Data.List
import Data.Maybe
import Math.NumberTheory.Powers.Squares


n = 1000000 

primes = tail $ getPrimesUpTo n
composites = filter (odd) $ getCompositesUpTo n

euler46 = find (not . isConjecture) composites

isConjecture c =
    isJust $ find (isSquare') [(c - p) `div` 2 | p <- (takeWhile (< c) primes )]
