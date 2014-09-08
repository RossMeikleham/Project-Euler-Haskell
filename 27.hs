-- Problem 27
--
-- Euler discovered the remarkable quadratic formula:
--
-- n² + n + 41
--
-- It turns out that the formula will produce 40 primes for the consecutive
-- values n = 0 to 39. However, when n = 40, 402 + 40 + 41 = 40(40 + 1)
-- + 41 is divisible by 41, and certainly when n = 41, 41² + 41 + 41 is
-- clearly divisible by 41.
--
-- The incredible formula  n² − 79n + 1601 was discovered, which produces
-- 80 primes for the consecutive values n = 0 to 79. The product of the
-- coefficients, −79 and 1601, is −126479.
--
-- Considering quadratics of the form:
--
--     n² + an + b, where |a| < 1000 and |b| < 1000
--
--         where |n| is the modulus/absolute value of n
--             e.g. |11| = 11 and |−4| = 4
--
--             Find the product of the coefficients, a and b, for the
--             quadratic expression that produces the maximum number of
--             primes for consecutive values of n, starting with n = 0.


-- Working:
--
-- (b -1)^2 + 1  gives double the primes of b
-- as b = 41 gives 40 primes, b = 1601 gives 80 primes
--
-- b also must be prime as when n = 0 reduces the equation to b
-- b + a + 1 also must be prime, b + 1 is even meaning a must be odd
-- unless b is 2 then a must be even
--
-- (1) n^2 - 79n + 1601 = n^2 - 80n + n + 40^2 + 1
--   = (n - 40)^2 + n - 40 + 41 
--
-- (2) n^2 + n + 41 
--
-- Both equations are of form (n - m)^2 + (n - m) + 41 
-- for (1) m = 40
-- for (2) m = 0
--
-- So we need to pick values of m between 0 and 40
-- such that |m^2 - m + 41| = |b| < 1000
--           |-2m + 1| = |a| < 1000 

import Primes
import Data.List
import Data.Ord


euler27 = let m = getM in (getA m) * (getB m)

getM = fst $ maximumBy (comparing snd) 
    [(m, getConsecutivePrimes m) | m <- filter isValidM [1..40]]

isValidM m = abs (getA m) < 1000 && abs (getB m) < 1000

getB m = m^2 - m + 41 
getA m = 1 - 2 * m 

getConsecutivePrimes m = length $ takeWhile (isPrime) 
    $ map (\n -> (n - m)^2 + (n - m) + 41) [0..]





