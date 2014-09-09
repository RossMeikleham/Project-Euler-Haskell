-- Problem 35
--
-- The number, 197, is called a circular prime because all rotations of the
-- digits: 197, 971, and 719, are themselves prime.
--
-- There are thirteen such primes below 100: 2, 3, 5, 7, 11, 13, 17, 31,
-- 37, 71, 73, 79, and 97.
--
-- How many circular primes are there below one million?


-- Can remove any numbers above 2 which have an even digit
-- as one of their permutations will be even having the even 
-- digit as the least significant, which isn't prime

import Data.Char
import Data.List
import Primes

-- 2,5 gets removed due to even removal, so re-add at end
euler35 = length $ [2,5] ++ (concat $ map (filter isCircularPrime) reduced) 
    where reduced = map (filter (\n -> not (containsEven n || contains5 n))) primeDigits

isCircularPrime n = 
    case find (not . isPrime) (map (digitsToInt) $ rotations n) of
        Just _  -> False
        Nothing -> True

digitsToInt :: [Int] -> Int
digitsToInt xs = sum $ map (\(x,y) -> x * 10^y) $ zip (reverse xs) [0..]

rotations n = init (zipWith (++) (tails xs) (inits xs))
    where xs = noToDigit n


containsEven n = 
    case find (\x -> x `mod` 2 == 0) (noToDigit n) of
        Just _ -> True
        Nothing -> False

contains5 n = case find (5 ==) (noToDigit n) of
        Just _ -> True
        Nothing -> False


noToDigit n = map (digitToInt) (show n)

-- Split ordered list of ints
-- into lists containing the same no of digits
-- e.g. [1,2,10,100] -> [[1,2],[10],[100]]
primeDigits =
    let primes = getPrimesUpTo 999999 in
    primeDigits' 1 primes [] 
    
primeDigits' _ [] ys = ys
primeDigits' n xs ys = primeDigits' (n + 1) right (ys ++ [left])
    where (left,right) = (takeWhile (<10^n) xs, dropWhile(<10^n) xs)
        
