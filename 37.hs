--Problem 37
--
--The number 3797 has an interesting property. Being prime itself, it is
--possible to continuously remove digits from left to right, and remain
--prime at each stage: 3797, 797, 97, and 7. Similarly we can work from
--right to left: 3797, 379, 37, and 3.
--
--Find the sum of the only eleven primes that are both truncatable from
--left to right and right to left.
--
--NOTE: 2, 3, 5, and 7 are not considered to be truncatable primes.

import Primes

euler37 = sum $ filter (isRightTruncatePrime) $ 
            filter (isLeftTruncatePrime) $
                dropWhile (< 10) $ getPrimesUpTo 750000

isLeftTruncatePrime n = 
    case head $ dropWhile (isPrime) (iterate (`div` 10) n) of
        0 -> True
        _ -> False

isRightTruncatePrime n =
    case fst $ head $ dropWhile(\(n,p) -> 
        isPrime n) $ iterate (\(n,p) -> 
            (n `mod` (10^p), p - 1)) (n, (+) 1 $ truncate $ logBase 10 (fromIntegral n)) of
                0 -> True
                _ -> False   
