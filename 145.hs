import Data.Bits

reverseNum :: Int -> Int
reverseNum i = reverse' 0 i

reverse' :: Int -> Int -> Int
reverse' rev 0 = rev
reverse' rev orig = reverse' ((rev * 10) + (orig `mod` 10)) (orig `div` 10)

containsOdd :: Int -> Bool 
containsOdd 0 = True
containsOdd i = if (i `mod` 10) .&. 1 == 1
                    then containsOdd (i `div` 10)
                    else False

isReversible :: Int -> Bool
isReversible i = i `mod` 10 /= 0 && containsOdd (i + (reverseNum i))

euler145 = length $ filter isReversible [1..999999999]

main = print euler145
