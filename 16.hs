-- Problem 16 
--
-- 2^15 = 32768 and the sum of its digits is 3 + 2 + 7 + 6 + 8 = 26.
--
-- What is the sum of the digits of the number 2^1000?

num = 2^1000

euler16 = digits num

digits n = sum $ digits' n []


digits' n xs
    | n == 0    = xs
    | otherwise = digits' (n `div` 10) ((n `mod` 10) : xs) 

