-- Problem 22
--
--Using names.txt (right click and 'Save Link/Target As...'), a 46K text
--file containing over five-thousand first names, begin by sorting it into
--alphabetical order. Then working out the alphabetical value for each
--name, multiply this value by its alphabetical position in the list to
--obtain a name score.
--
--For example, when the list is sorted into alphabetical order, COLIN,
--which is worth 3 + 15 + 12 + 9 + 14 = 53, is the 938th name in the list.
--So, COLIN would obtain a score of 938 × 53 = 49714.
--
--What is the total of all the name scores in the file?
 
import qualified Data.Vector as V
import Data.List.Split
import Data.List

fileName = "p022_names.txt"

euler22 = do
    file <- readFile fileName
    print $ V.foldl (\n (i,xs) -> n + ((i + 1) * (sum xs))) 0 $ indexedVec file
        where indexedVec = V.indexed . getValues

getValues :: String -> V.Vector [Int]
getValues s =V.fromList $  map (map (getValue)) $ (sort . getNames) s
    where getNames = (splitOn ",") . (stripChars  "\"") 
          getValue c = (fromEnum c) - 64

stripChars :: String -> String -> String
stripChars = filter . flip notElem
