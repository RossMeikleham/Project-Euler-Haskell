import Data.Ratio

calcE :: Int -> Rational
calcE n = calcE' 0
  where 
    calcE' :: Int -> Rational
    calcE' i = cur + next 
        where 
          cur 
            | i == 0 = 2 
            | (i + 1) `mod` 3 == 0 = fromIntegral ((i + 1) `div` 3) * 2  
            | otherwise = 1                
            
          next = if n == (i + 1) then 0 else 1 / calcE' (i + 1)


digits :: Integer -> [Integer]
digits 0 = []
digits i = digits (i `div` 10) ++ [i `mod` 10]

euler65 = print $ sum $ digits $ numerator $ calcE 100
