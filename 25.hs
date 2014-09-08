-- Problem 25
--
-- First fibonacci number with 1000 digits

euler25 = snd $ head $ dropWhile (\(m,n) -> m < 10^999) fibsIndexed
    where fibsIndexed = zip fibs [0..]
    
fibs :: [Integer]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)
