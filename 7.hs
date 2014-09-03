-- Problem 7
--Listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, we can see that the 6th prime is 13.
--What is the 10001st prime number?


euler7 = nthPrime 10001

nthPrime :: Int -> Int
nthPrime n = nthPrime' 0 n

nthPrime' :: Int -> Int -> Int
nthPrime' cur nth 
    | nth == 0  = cur
    | otherwise = nthPrime' (nextPrime $ cur) (nth - 1)

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
