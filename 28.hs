{- 

Problem 28

Starting with the number 1 and moving to the right 
in a clockwise direction a 5 by 5 spiral is formed as follows:

21 22 23 24 25
20  7  8  9 10
19  6  1  2 11
18  5  4  3 12
17 16 15 14 13

It can be verified that the sum of the numbers on the diagonals is 101.

What is the sum of the numbers on the diagonals in a 1001 by 1001 
spiral formed in the same way?
-} 

-- Working
--
-- for 1x1 : no diagonals only 1
--
-- for 3x3 : 
--  top right is 3^2 = 9
--  top left is 3^2 - 2 = 7
--  bottom left is 3^2 - 4 = 5
--  bottom right is 3^2 - 6 = 3
--
-- for 5x5 : 
--  top right is 5^2 = 25
--  top left is 5^2 - 4 = 21
--  bottom left is 5^2 - 8 = 17
--  bottom right is 5^2 - 12 = 13
--
--  using induction for nxn:
--
--  top right is n^2 
--  top left is n^2 - (n - 1) 
--  bottom left is n^2 - 2(n - 1)
--  bottom right is n^2 - 3(n - 1)
--  
--  total is 4n^2 - 6(n - 1) = 4n^2 -6n + 6
--
--  sum odd n from 1 to 1001

euler28 = foldl ((. diag) . (+)) (1) [3,5..1001]
    where diag n = 4 * (n^2) - (6 * n) + 6
