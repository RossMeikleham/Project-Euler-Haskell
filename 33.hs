-- Problem 33
--
-- The fraction 49/98 is a curious fraction, as an inexperienced
-- mathematician in attempting to simplify it may incorrectly believe that
-- 49/98 = 4/8, which is correct, is obtained by cancelling the 9s.
--
-- We shall consider fractions like, 30/50 = 3/5, to be trivial examples.
--
-- There are exactly four non-trivial examples of this type of fraction,
-- less than one in value, and containing two digits in the numerator and
-- denominator.
--
-- If the product of these four fractions is given in its lowest common
-- terms, find the value of the denominator.
import Data.Ratio

euler33 = denominator $ product $ map (\(n,d) -> n % d) $ 
            [(n,d) | n <- [10..98], d <- [n+1..99],  isDigitCancelling n d]

isDigitCancelling :: Int -> Int -> Bool
isDigitCancelling n d =
        d `mod` 10 /= 0
    &&  n `mod` 10 == d `div` 10 
    &&  (n `div` 10) % (d `mod` 10) == n % d
