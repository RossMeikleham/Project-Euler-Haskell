import Data.Maybe

--3
--The prime factors of 13195 are 5, 7, 13 and 29. What is the largest prime factor of 
--the number 600851475143?
--
euler3 = largestPrimeFactor 600851475143

-- Given an Integer finds the largest Prime Factor,
-- if Integer has no prime Factors returns Nothing
largestPrimeFactor ::Int -> Maybe Int
largestPrimeFactor x 
    | x < 2     = Nothing
    | otherwise = Just $ largestPrimeFactor' x 2 


largestPrimeFactor' :: Int -> Int -> Int
largestPrimeFactor' n lp  
    | n < 2           = lp
    | n `mod` lp == 0 = largestPrimeFactor' (n `div` lp) lp  
    | otherwise       = largestPrimeFactor' n $ nextPrime lp 


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
