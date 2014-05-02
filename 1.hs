import Data.Numbers.Primes

--1
--If we list all the natural numbers below 10 that are multiples of 3 or 5, we get 3, 5, 6 and 9. The sum of these multiples is 23.
--Find the sum of all the multiples of 3 or 5 below 1000.


euler1 = sum  [x | x <- [1..999], ((x `mod` 3) == 0) || ((x `mod` 5) == 0)]


--2
--Each new term in the Fibonacci sequence is generated by adding the previous two terms. 
--By starting with 1 and 2, the first 10 terms will be:
--1, 2, 3, 5, 8, 13, 21, 34, 55, 89, ...

--By considering the terms in the Fibonacci sequence whose values do not exceed four million,
--find the sum of the even-valued terms.


fibs = 0 : 1 : zipWith (+) fibs (tail fibs)
euler2 = sum $  filter (even) $ takeWhile(< 4000000) fibs


--3
--The prime factors of 13195 are 5, 7, 13 and 29. What is the largest prime factor of 
--the number 600851475143?
--
euler3 = maximum $ primeFactors x
    where x = 600851475143





