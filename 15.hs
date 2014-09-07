-- Problem 15
--
--Starting in the top left corner of a 2×2 grid, and only being able to
--move to the right and down, there are exactly 6 routes to the bottom
--right corner.
--
--How many such routes are there through a 20×20 grid?


-- Number of paths to each vertex is equal to the sum of the paths
-- leading to the left and above

euler15 = iterate (scanl (+) 1 . tail) (repeat 1) !! 3 !! 1
