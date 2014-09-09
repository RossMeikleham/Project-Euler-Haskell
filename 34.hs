{- 

Problem 34

145 is a curious number, as 1! + 4! + 5! = 1 + 24 + 120 = 145.

Find the sum of all numbers which are equal to the sum of the factorial of their digits.

Note: as 1! = 1 and 2! = 2 are not sums they are not included.
-}

import Data.Array
import Data.Char

euler34 = sum [n | n <- [3..upperBound], n == factorialDigits n]

upperBound = 40585

factorialDigits :: Int -> Int
factorialDigits n = sum $ map (factorials !) (map (digitToInt) (show n))
    where factorials = listArray (0,9) [1, 1, 2, 6, 24, 120, 720, 5040, 40320, 362880s a curious number, as 1! + 4! + 5! = 1 + 24 + 120 = 145.
    
    Find the sum of all numbers which are equal to the sum of the factorial of their digits.
    
    Note: as 1! = 1 and 2! = 2 are not sums they are not included.
    ]
