module Primes where

import Data.List
import Data.Array.ST
import Control.Monad.ST
import Control.Monad
import Data.Array.Unboxed

-- Very simple and inefficient primes library for
-- help with solving certain Project Euler Problems

--Given a positive integer x return
--the closest prime y where y > x
nextPrime :: Int -> Int              
nextPrime x 
    | isPrime (x + 1) = (x + 1)
    | otherwise       = nextPrime (x + 1)


-- Check if given number is prime
isPrime :: Int -> Bool
isPrime x  
    | x < 2     = False
    | otherwise = null [y | y <- [2..sqrtx], x `mod` y == 0]
    where sqrtx = truncate $ sqrt $ fromIntegral x


--Sieve of Eratosthenes for calculating primes below a given number
getPrimesUpTo :: Int -> [Int]
getPrimesUpTo n
    | n < 2 = []
    | otherwise = [x | (x, True) <- assocs $ obtainPrimes n]


getCompositesUpTo :: Int -> [Int]
getCompositesUpTo n
    | n < 2 = [1]
    | otherwise = [x | (x, False) <- assocs $ obtainPrimes n]

-- Generate Array of Bools, each index containing true
-- if that index is a prime, false otherwise
obtainPrimes :: Int -> UArray Int Bool
obtainPrimes n = runSTUArray $ do 
    sieve <- newArray (2, n) True
    forM_ [2..n] $ \p -> do
        isPrime <- readArray sieve p
        when isPrime $ do
            forM_ [p*2, p*3 .. n] $ \m -> do
                writeArray sieve m False
    return sieve
