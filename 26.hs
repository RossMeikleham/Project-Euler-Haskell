{- A unit fraction contains 1 in the numerator. 
 - The decimal representation of the unit fractions with denominators 2 to 10 are given:

    1/2	= 	0.5
    1/3	= 	0.(3)
    1/4	= 	0.25
    1/5	= 	0.2
    1/6	= 	0.1(6)
    1/7	= 	0.(142857)
    1/8	= 	0.125
    1/9	= 	0.(1)
    1/10	= 	0.1

Where 0.1(6) means 0.166666..., and has a 1-digit recurring cycle. 
It can be seen that 1/7 has a 6-digit recurring cycle.

Find the value of d < 1000 for which 1/d 
contains the longest recurring cycle in its decimal fraction part
-}

import Primes
 
-- Longest cycle will be of length n for 1/p
-- satifying p < 1000 and Fermat's little theorem :
-- (10^n - 1) mod p = 0,  1/p can also give up to p - 1 repeating digits
--
-- so for each prime to find the cycle length 
-- we're looking for smallest k s.t k satisfies
-- (10^k - 1) mod p = 0 
--
-- Then the largest k value of all primes below 1000
-- will give the prime with the longest cycle
--
-- If we start from the largest prime and work our way down
-- until we reach a prime p with the maximum possible cycle length p -1
-- that should be the prime with the longest cycle length below 1000 as
-- the next prime will be at  most p - 2 which can give a maximum 
-- cycle of length p - 3. So as long as there exists a prime below 1000
-- with a cycle of p -1 then this method should work


euler26 = head $ dropWhile (\p -> getReciprocalLengthPrime p < p - 1) (reverse $ getPrimesUpTo' 999) 

getReciprocalLengthPrime p 
    | p == 2 || p == 5 = 0 
    | otherwise = recipLengthPrime 1 p
    where recipLengthPrime k p = 
            if (10^k - 1) `mod` p == 0 
                then k
                else recipLengthPrime (k + 1) p 

getPrimesUpTo' :: Int -> [Integer]
getPrimesUpTo' n = map (fromIntegral) $ getPrimesUpTo n     
