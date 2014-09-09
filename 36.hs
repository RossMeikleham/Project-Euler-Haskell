-- Double-base palindromes
-- Problem 36
--
-- The decimal number, 585 = 10010010012 (binary), is palindromic in both
-- bases.
--
-- Find the sum of all numbers, less than one million, which are
-- palindromic in base 10 and base 2.
--
-- (Please note that the palindromic number, in either base, may not
-- include leading zeros.)

-- can ommit even numbers since end in 0 in binary and the start of
-- a binary number must be 1
euler36 = sum $ filter (\n -> isPalindromicBase 10 n && isPalindromicBase 2 n) [1,3..999999]

isPalindromicBase :: Int -> Int -> Bool 
isPalindromicBase b n = n == reverseIntBase b n

reverseIntBase :: Int -> Int -> Int 
reverseIntBase b x = reverseIntBase' b x 0

reverseIntBase' :: Int -> Int -> Int -> Int
reverseIntBase' b o r
    | o /= 0 = reverseIntBase' b (o `div` b) $ (r * b) + (o `mod` b)
    | otherwise = r
