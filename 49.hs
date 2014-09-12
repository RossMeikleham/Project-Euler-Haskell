-- 
-- Problem 49
--
-- The arithmetic sequence, 1487, 4817, 8147, in which each of the terms
-- increases by 3330, is unusual in two ways: (i) each of the three terms
-- are prime, and, (ii) each of the 4-digit numbers are permutations of one
-- another.
--
-- There are no arithmetic sequences made up of three 1-, 2-, or 3-digit
-- primes, exhibiting this property, but there is one other 4-digit
-- increasing sequence.
--
-- What 12-digit number do you form by concatenating the three terms in
-- this sequence?

import Data.List
import Primes

nums = [1..10000]
primes = getPrimesUpTo 10000

euler49 = concatMap (show) $ take 3 $ iterate (+3330) result

result = 
    last $ take 2 $ 
        filter (\x -> let xyz = ((take 3) . (iterate (+3330))) x in 
            arePermutations xyz && arePrime xyz) nums

    where sx x = show x
          arePermutations (x:xs) = 
              and $ map (\n -> (sort . show) x == (sort . show) n) xs
          arePrime xs = and $ (map (isPrime)) xs
