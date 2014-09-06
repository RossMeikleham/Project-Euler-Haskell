-- Problem 7
--Listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, we can see that the 6th prime is 13.
--What is the 10001st prime number?
import Primes

euler7 = nthPrime 10001

nthPrime :: Int -> Int
nthPrime n = nthPrime' 0 n

nthPrime' :: Int -> Int -> Int
nthPrime' cur nth 
    | nth == 0  = cur
    | otherwise = nthPrime' (nextPrime $ cur) (nth - 1)
