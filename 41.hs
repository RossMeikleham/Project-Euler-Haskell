{-
Problem 41

We shall say that an n-digit number is pandigital if it makes use of all the digits 1 to n exactly once. For example, 2143 is a 4-digit pandigital and is also prime.

What is the largest n-digit pandigital prime that exists?
-}

{- largest possible would be a 7 digit number starting with 7
 - 
 - 9 digits isn't possible as using the divisibility by 3 rule
 - which states if the sum of a numbers digits is divisible by 3
 - the number itself is divisible by 3
 -
 - 1 + 2 + ... + 9 = 45 which is divisble by 3
 -
 - Similarly for 8
 - 1 + 2 + ... + 8 = 36 which is divisble by 3
 -
 - For 7
 - 1 + 2 + .. + 7 = 28 which isn't divisible by 3 
 -
 - start with 7654321
 - then check 7654312 etc until prime found
 -
 -  i.e. order permutations of 7 digits from highest to lowest,
 -  attempt to find first prime. 
 -}

import Primes
import Data.List
import Data.Maybe

euler41 = find (isPrime) $ reverse $ sort $ map (read) $ permutations "7654321"

