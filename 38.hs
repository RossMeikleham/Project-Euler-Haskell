-- Problem 38
--
--  Take the number 192 and multiply it by each of 1, 2, and 3:
--
--     192 × 1 = 192
--         192 × 2 = 384
--             192 × 3 = 576
--
--             By concatenating each product we get the 1 to 9 pandigital,
--             192384576. We will call 192384576 the concatenated product
--             of 192 and (1,2,3)
--
--             The same can be achieved by starting with 9 and multiplying
--             by 1, 2, 3, 4, and 5, giving the pandigital, 918273645,
--             which is the concatenated product of 9 and (1,2,3,4,5).
--
--             What is the largest 1 to 9 pandigital 9-digit number that
--             can be formed as the concatenated product of an integer with
--             (1,2, ... , n) where n > 1?

-- Assume number starts with 9 as that will generate largest 
-- possible pandigital number. In the chance this method doesnt generate
-- a single pandigital number then the next thing to do would be
-- to move to 8.
--
-- 2 digit number -> 1 * 9x = 2 digits
--                   2 * 9x = 3 digits
--                   3 * 9x = 3 digits 
--                   4 * 9x = 3 digits
--                
-- can't reach 9 digits starting with a 2 digit number starting with 9
--
-- 3 digit number -> 1 * 9xx = 3 digits
--                   2 * 9xx = 4 digits
--                   3 * 9xx = 4 digits
--
--can't reach 9 digits starting with 3 digit number starting with 9
--
-- 4 digit number -> 1 * 9xxx = 4 digits
--                   2 * 9xxx = 5 digits
-- 
-- 9 digits reached
--
-- number must be inclusive between 9000 - 9999

import Data.Char
import Data.List

euler38 = maximum [n | n <- map (combined) [9000..9999], isPandigital n]
    where combined m = (m * 10^5) + (m * 2)

isPandigital n = (not . elem '0') strN 
             &&  strN == nub strN
                where strN = show n

