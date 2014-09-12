-- Problem 47
--
-- The first two consecutive numbers to have two distinct prime factors
-- are:
--
-- 14 = 2 × 7
-- 15 = 3 × 5
--
-- The first three consecutive numbers to have three distinct prime factors
-- are:
--
-- 644 = 2² × 7 × 23
-- 645 = 3 × 5 × 43
-- 646 = 2 × 17 × 19.
--
-- Find the first four consecutive integers to have four distinct prime
-- factors. What is the first of these numbers?
--
import Primes
import Data.Array.Unboxed;

p = 1000000
n = 1000000
primes = getPrimesUpTo p
--Replace all primes with 1 so unecessary factor calculations
--are not  taken place as the result will always be 1
numbers = [if prime then 2 else x | (x,prime) <- assocs $ obtainPrimes n]


-- gcd of consecutive numbers must be 1
-- all 4 must not be prime as each needs to have 4 prime factors
euler47 = getConsecutive 4 numbers

getConsecutive :: Int -> [Int] -> Maybe Int
getConsecutive _ [] = Nothing
getConsecutive n xs
    | consecCount >= n = Just s
    | otherwise = getConsecutive n (drop (consecCount + 1) xs)
    where consecCount = length $ takeWhile ((== n) . length. primeFactors) xs
          s = head xs


--Obtain a list of prime factorisation of n
primeFactors :: Int -> [Int]
primeFactors n = primeFactors' n primes []

primeFactors' :: Int -> [Int] -> [Int] -> [Int]
primeFactors' n (p:ps) factors
    | n == 1 = factors
    | n `mod` p == 0 = primeFactors' (n `div` pf) ps ([pf] ++ factors)
    | otherwise = primeFactors' n ps factors
        where pf = primeFactor n p 

primeFactor n p
    | n == 1 || n `mod` p /= 0 = 1
    | otherwise = (*) p $ primeFactor (n `div` p) p
