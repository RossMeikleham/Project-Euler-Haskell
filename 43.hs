{- 

Problem 43

The number, 1406357289, is a 0 to 9 pandigital number because it is made up of each of the digits 0 to 9 in some order, but it also has a rather interesting sub-string divisibility property.

Let d1 be the 1st digit, d2 be the 2nd digit, and so on. In this way, we note the following:

    d2d3d4=406 is divisible by 2
    d3d4d5=063 is divisible by 3
    d4d5d6=635 is divisible by 5
    d5d6d7=357 is divisible by 7
    d6d7d8=572 is divisible by 11
    d7d8d9=728 is divisible by 13
    d8d9d10=289 is divisible by 17

Find the sum of all 0 to 9 pandigital numbers with this property.

-}

-- d4 must be even and d5 must be 0 or 5

import Data.List
import Data.Char

euler43 = sum $ map (readInt) $ filter (satisfiesProperty) initialPerms


initialPerms :: [String]
initialPerms = 
    filter (\x -> x !! 0 /= '0' 
        && (even $ digitToInt (x !! 3)) 
        && (x !! 5 == '5' || x !! 5 == '0')) $
        permutations "9876543210"

satisfiesProperty :: String -> Bool
satisfiesProperty (p:ps) =  satisfiesIndex 1 3
                         && satisfiesIndex 3 7
                         && satisfiesIndex 4 11
                         && satisfiesIndex 5 13
                         && satisfiesIndex 6 17
    where satisfiesIndex a m = 
            (readInt . take 3 . drop a) ps `mod` m == 0

readInt :: String -> Int
readInt = read
