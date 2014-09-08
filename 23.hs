-- Problem 23
--
-- A perfect number is a number for which the sum of its proper divisors is
-- exactly equal to the number. For example, the sum of the proper divisors
-- of 28 would be 1 + 2 + 4 + 7 + 14 = 28, which means that 28 is a perfect
-- number.
--
-- A number n is called deficient if the sum of its proper divisors is less
-- than n and it is called abundant if this sum exceeds n.
--
-- As 12 is the smallest abundant number, 1 + 2 + 3 + 4 + 6 = 16, the
-- smallest number that can be written as the sum of two abundant numbers
-- is 24. By mathematical analysis, it can be shown that all integers
-- greater than 28123 can be written as the sum of two abundant numbers.
-- However, this upper limit cannot be reduced any further by analysis even
-- though it is known that the greatest number that cannot be expressed as
-- the sum of two abundant numbers is less than this limit.
--
-- Find the sum of all the positive integers which cannot be written as the
-- sum of two abundant numbers.

import Data.List
import Data.Array.ST
import Control.Monad.ST
import Control.Monad
import Data.Array.Unboxed

euler23 = sum [x | (x, True) <- (assocs . obtainNonAbundantSums) $ abundantNos maxNo]

maxNo = 28123

obtainNonAbundantSums :: [Int] -> UArray Int Bool
obtainNonAbundantSums abNos= runSTUArray $ do
    arr <- newArray (1, maxNo) True
    forM_ abNos $ \m -> do
        let xs = takeWhile (\a -> m + a <= maxNo) $ dropWhile (< m) abNos
        forM_ xs $ \n -> writeArray arr (m + n) False
    return arr

abundantNos n = filter (\n -> sumProperDivisors n > n) [1..n] 

sumProperDivisors n 
  | n == 1    = 0
  | otherwise = sum factors - n
    where factors = concatMap (\(x,y)-> if x /= y then [x,y] else [x]) $ factorPairs n
     
factorPairs :: Int -> [(Int, Int)]
factorPairs x = [ (y, x `div` y) | y <- [1..truncate (sqrt (fromIntegral x))], x `mod` y == 0]


      
