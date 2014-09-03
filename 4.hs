-- Problem 4 : Largest pallindrome product
--
-- A palindromic number reads the same both ways. 
-- The largest palindrome made from the product of two 2-digit numbers is 9009 = 91 × 99.
--
-- Find the largest palindrome made from the product of two 3-digit numbers.

import Data.List

euler4 = take 1 $ [x | x <- reverse products, isPalindromic x]
    where products = sort [x * y | x<-[100..999], y <- [100..999]]

isPalindromic :: Int -> Bool 
isPalindromic n = n == reverseInt n

reverseInt :: Int -> Int 
reverseInt x = reverseInt' x 0

reverseInt' :: Int -> Int -> Int
reverseInt' o r
    | o /= 0 = reverseInt' (o `div` 10) $ (r * 10) + (o `mod` 10)
    | otherwise = r
