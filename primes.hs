--module Primes where

import Data.List
import Data.Array.ST
import Control.Monad.ST
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
obtainPrimesUpTo :: Int -> [Int]
obtainPrimesUpTo n
    | n < 2 = []
    | otherwise = primes $ elems $ obtainPrimes 1 initialArray
    where initialArray = listArray (1, n) initialList
          initialList = [False] ++ replicate (n - 1) True

-- Convert List of Bools containing primes at each 'True' index
-- Into a list containing all primes 
primes :: [Bool] -> [Int]
primes xs = primes' 1 xs []


primes' :: Int -> [Bool] -> [Int] -> [Int]
primes' _ [] ys = ys 
primes' n (x:xs) ys 
    | x == True = primes' (n + 1) xs (ys ++ [n]) 
    | otherwise = primes' (n + 1) xs ys

obtainPrimes :: Int -> UArray Int Bool -> UArray Int Bool
obtainPrimes n arr = 
    case getNextPrime n arr of
        Nothing -> arr
        Just y -> obtainPrimes y $ markNotPrimes (2 * y) y arr


getNextPrime :: Int -> UArray Int Bool -> Maybe (Int)
getNextPrime n arr 
    | n >= (snd $ bounds arr) = Nothing -- No primes in sieve 
    | arr ! (n + 1) == True = Just (n + 1)
    | otherwise           = getNextPrime (n + 1) arr

-- Sieve out all multiples of given non prime  which cannot be prime numbers
markNotPrimes :: Int -> Int -> UArray Int Bool -> UArray Int Bool 
markNotPrimes n inc arr = runSTUArray $ do
    a <- thaw arr
    (bot, top) <- getBounds a
    mapM (\x -> writeArray a x False) $ [2,4..20000] -- nonPrimes top
    return a
    where nonPrimes top = takeWhile (<= top) $ nonPrimes' n inc
          nonPrimes' n inc = [n] ++ nonPrimes' (n + inc) inc 

main = 
    do 
        print $ sum $ obtainPrimesUpTo 20000
       


