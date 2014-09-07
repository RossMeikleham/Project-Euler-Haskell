-- Problem 21
-- 
-- Let d(n) be defined as the sum of proper divisors of n (numbers less
-- than n which divide evenly into n).
-- If d(a) = b and d(b) = a, where a ≠ b, then a and b are an amicable pair
-- and each of a and b are called amicable numbers.
--
-- For example, the proper divisors of 220 are 1, 2, 4, 5, 10, 11, 20, 22,
-- 44, 55 and 110; therefore d(220) = 284. The proper divisors of 284 are
-- 1, 2, 4, 71 and 142; so d(284) = 220.
--
-- Evaluate the sum of all the amicable numbers under 10000.

                                                           
euler21 = sum [n | n <- [1..9999], amicable n]
    where amicable n = n /= n2 && 
            n == (sumProperDivisors) n2
            where n2 = sumProperDivisors n
          

sumProperDivisors n 
    | n == 1    = 0
    | otherwise = (sum factors) - n
        where factors = concatMap (\(x,y)-> [x,y]) $ factorPairs n
     
factorPairs :: Int -> [(Int, Int)]
factorPairs x = [ (y, x `div` y) | y <- [1..truncate (sqrt (fromIntegral x))], x `mod` y == 0]

