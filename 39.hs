{-
Problem 39

If p is the perimeter of a right angle triangle with integral length sides, {a,b,c}, there are exactly three solutions for p = 120.

{20,48,52}, {24,45,51}, {30,40,50}

For which value of p ≤ 1000, is the number of solutions maximised?
-}

{- ----- Working ------
 -
 - constraints : (1) a + b + c = p
 -               (2) a^2 + b^2 = c^2
 -
 - a, b and c must be integers
 -
 - maximise p, p <= 1000 to get max number of solutions
 -
 - Rearranging (1) and substituting into (2) gives:
 - c = p - a - b -> (p - a - b)^2 = a^2 + b^2
 -               =>  a^2 + 2ab - 2ap + b^2 -2bp + p^2 = a^2 + b^2
 -               =>  2ab - 2ap - 2bp + p^2 = 0    
 -               =>  2ab - 2bp = 2ap - p^2
 -               =>  b = (2an - p^2)/ (2(a - p))
 -
 - Moreover we can reduce to only checking even values of p as
 -  for a^2 + b^2 = c^2  
 -  if a and b are even, a^2 and b^2 are even => c is even => p is even
 -  if a is odd and b is odd a^2 is odd and b^2 is odd and  then c^2 is
 -  even => c is even => p is even
 -  if one of a and b is odd then c^2 is odd => c is odd
 -  odd + even + odd  gives an even p
-} 
 
import Data.Ratio 
import Data.List
import Data.Maybe
import Data.Ord

euler39 = snd $ 
          maximumBy (comparing fst) $ 
          zip (map (getNoTs) [2,4..1000]) [2,4..1000] 

getNoTs p = length $ catMaybes [getABC a p | a <- [1..p `div` 2]] 

getABC :: Int -> Int -> Maybe (Int, Int, Int)
getABC a p = getB a p >>= \b -> getC a b p >>= \c -> return (a, b, c) 
         
-- Returns Just b if supplied a and n
-- can generate an integer b for triangle,
-- otherwise returns Nothing
getB :: Int -> Int -> Maybe Int
getB a p
    | a == p || denominator b /= 1 = Nothing
    | otherwise                   = Just $ numerator b
    where b = ((2 * a * p) - p^2) % (2 * (a - p))

-- Returns Just c if supplied a, b and n
-- can generate a valid c >= 0 for triangle
-- Otherwise returns Nothing
getC :: Int -> Int -> Int -> Maybe Int
getC a b p
    | c >= 1    = Just c
    | otherwise = Nothing
    where c = p - a - b
