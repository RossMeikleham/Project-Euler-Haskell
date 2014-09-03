--Problem 9
--
--A Pythagorean triplet is a set of three natural numbers, a < b < c, for
--which, a^2 + b^2 = c^2
--
--For example, 32 + 42 = 9 + 16 = 25 = 52.
--
--There exists exactly one Pythagorean triplet for which a + b + c = 1000.
--Find the product abc.

import Control.Applicative
import Data.Maybe

euler9 =  
    takeFirst isJust [ (mul3) <$> Just a <*> Just b <*> c 
                | a <- [1..1000],  b <- [calculateB a], c <- [calculateC a b]]
          where mul3 a b c = a * b * c

-- Obtains the first item in the list which satisfies the given condition
takeFirst :: (a -> Bool) -> [a] -> Maybe (a)
takeFirst _ []  = Nothing
takeFirst f (x:xs) 
    | f x == True = Just x
    | otherwise   = takeFirst f xs

-- Given a value of a and both equations 
-- (1) a^2 + b^2 = c^2
-- (2) a + b + c = 1000
-- can work out b by rearranging (2) to give c = 1000 -  a - b
-- and then substituting into (1) and rearranging to work out b
calculateB :: Int -> Int
calculateB a = (1000^2 - 2000 * a) `div` (2000 - 2 * a)


-- With both a and b simple to work out c using
-- either or the 2 formulas. If both formulas dont give
-- the same value of c for the supplied a and b then
-- the a and b values are not satisfactory to solve the equation 
calculateC :: Int -> Int -> Maybe (Int)
calculateC a b 
    | c1 == c2  = Just c1
    | otherwise = Nothing
    where c1 = truncate $ sqrt $ fromIntegral (a^2 + b^2)
          c2 = 1000 - a - b
