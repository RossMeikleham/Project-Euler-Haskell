{-

Problem 45

Triangle, pentagonal, and hexagonal numbers are generated by the following formulae:
Triangle        Tn=n(n+1)/2         1, 3, 6, 10, 15, ...
Pentagonal      Pn=n(3n−1)/2        1, 5, 12, 22, 35, ...
Hexagonal       Hn=n(2n−1)      1, 6, 15, 28, 45, ...

It can be verified that T285 = P165 = H143 = 40755.

Find the next triangle number that is also pentagonal and hexagonal.
-}

import Data.List

-- All triangle numbers are hexagonal numbers, so we can ignore
-- triangles
euler45 = find (isPentagonal) $ map (\n -> (2 * n^2) - n) [144..]

isPentagonal :: Int -> Bool
isPentagonal p = 
    n == (fromIntegral .truncate) n 
    where n = (sqrt ((24 * (fromIntegral p)) + 1) + 1.0) / 6.0

