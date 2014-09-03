module Primes where
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

{-
--Sieve of Ethrastholes for calculating primes below a given number
obtainPrimesUpTo :: Int -> [Int]
obtainPrimesUpTo n
    | n < 2 = []
    | otherwise obtainPrimesUpTo` n $ V.fromList 
        $ [Just False, Just True] ++  replicate Nothing


obtainPrimesUpTo' :: Int -> V.Vector (Maybe Bool) -> V.Vector (Maybe Bool)
obtainPrimesUpTo' n arr = 

getNextPrime :: V.Vector (Maybe Bool) -> Int -> Int
getNextPrime arr n = 

markNotPrimes :: Int -> V.Vector (Maybe Bool) -> V.Vector (Maybe Bool)
markNotPrimes n arr = 
-}








