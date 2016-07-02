
data RomanNumeral = I | V | X | L | C | D | M
    deriving (Show, Eq, Ord)

convertThousands :: Int -> [RomanNumeral]
convertThousands n = replicate n M

convertHundreds :: Int -> [RomanNumeral]
convertHundreds n    
    | n < 4 = replicate n C
    | n == 4 = [C, D]
    | n == 5 = [D]
    | n < 9 = D : replicate (n - 5) C
    | n == 9 = [C, M]

convertTens :: Int -> [RomanNumeral]
convertTens n    
    | n < 4 = replicate n X
    | n == 4 = [X, L]
    | n == 5 = [L]
    | n < 9 = L : replicate (n - 5) X
    | n == 9 = [X, C]

convertOnes :: Int -> [RomanNumeral]
convertOnes n
    | n < 4 = replicate n I
    | n == 4 = [I, V]
    | n == 5 = [V]
    | n < 9 = V : replicate (n - 5) I
    | n == 9 = [I, X]

intToRoman :: Int -> [RomanNumeral]
intToRoman n =
    convertThousands (n `div` 1000) ++
    convertHundreds  ((n `mod` 1000) `div` 100) ++
    convertTens ((n `mod` 100) `div` 10) ++
    convertOnes (n `mod` 10) 

singleRomanToInt :: RomanNumeral -> Int
singleRomanToInt M = 1000
singleRomanToInt D = 500
singleRomanToInt C = 100
singleRomanToInt L = 50
singleRomanToInt X = 10
singleRomanToInt V = 5
singleRomanToInt I = 1

romanToInt :: [RomanNumeral] -> Int
romanToInt [] = 0

romanToInt (x:y:xs) =
    if x < y
        then singleRomanToInt y - singleRomanToInt x + romanToInt xs
        else singleRomanToInt x + romanToInt (y:xs)

romanToInt (x:xs) = 
    singleRomanToInt x + romanToInt xs


showNumerals :: [RomanNumeral] -> String
showNumerals numerals = concat $ map show numerals

parseNumeral :: Char -> RomanNumeral
parseNumeral 'M' = M
parseNumeral 'D' = D
parseNumeral 'C' = C
parseNumeral 'L' = L
parseNumeral 'X' = X
parseNumeral 'V' = V
parseNumeral 'I' = I

parseNumerals :: String -> [RomanNumeral]
parseNumerals = map parseNumeral


file = "p089_roman.txt"

readNumerals :: IO [[RomanNumeral]]
readNumerals = do
    contents <- readFile file
    return $ map parseNumerals $ words contents    


charsSaved :: [RomanNumeral] -> Int
charsSaved numerals = length numerals - length optimisedNumerals
    where optimisedNumerals = intToRoman $ romanToInt numerals

euler89 = do 
    numerals <- readNumerals
    print $ sum $ map charsSaved numerals
