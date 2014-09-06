-- Problem 6
--
-- The sum of the squares of the first ten natural numbers is,
-- 1^2 + 2^2 + ... + 10^2 = 385
--
-- The square of the sum of the first ten natural numbers is,
-- (1 + 2 + ... + 10)^2 = 552 = 3025
--
-- Hence the difference between the sum of the squares of the first ten
-- natural numbers and the square of the sum is 3025 − 385 = 2640.
--
-- Find the difference between the sum of the squares of the first one
-- hundred natural numbers and the square of the sum.


euler6 = result 100
    where result n =  ((sumFrom1To n) ^ 2) - sumOfSquares1To n

sumFrom1To :: Int -> Int
sumFrom1To n = (n * (n + 1)) `div` 2

sumOfSquares1To :: Int -> Int
sumOfSquares1To n = (n * (n + 1) * ((2 * n) + 1)) `div` 6
