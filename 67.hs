-- Problem 67
--
-- By starting at the top of the triangle below and moving to adjacent
-- numbers on the row below, the maximum total from top to bottom is 23.
--
-- 3
-- 7 4
-- 2 4 6
-- 8 5 9 3
--
-- That is, 3 + 7 + 4 + 9 = 23.
--
-- Find the maximum total from top to bottom in p067_triangle.txt
import Data.List.Split

fileName = "p067_triangle.txt"

euler67 = do 
    file <- readFile fileName 
    print $ maxPath $ [[]] ++ (linesToTriangle $ lines file)

linesToTriangle :: [String] -> [[Int]]
linesToTriangle strs = map (lineToTriangle) strs
    where lineToTriangle s = map (read) (splitOn " " s) 

                  
maxPath :: [[Int]] -> Int
maxPath ts = maximum $ head $ mapPaths 0 [[]] ts
                    
mapPaths :: Int -> [[Int]] -> [[Int]] -> [[Int]]
mapPaths y s@(n:ns) ts
  | y >= length ts = s
  | otherwise = mapPaths (y + 1) newTree ts
      where newTree = [maxPathRow y (ts !! y) n] ++ s

maxPathRow :: Int -> [Int] -> [Int] -> [Int] 
maxPathRow y curRow prevRow = [maxPathRowCol x curRow prevRow | x <- [0..y-1]]

maxPathRowCol :: Int -> [Int] -> [Int] -> Int
maxPathRowCol x curRow prevRow = (curRow !! x) + max leftPLen rightPLen
  where 
        leftPLen  = if (x == 0) then 0 else prevRow !! (x - 1)
        rightPLen = if (x == (length curRow) - 1) then 0 else prevRow !! x
