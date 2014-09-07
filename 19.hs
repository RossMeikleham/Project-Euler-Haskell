{- Problem 19



You are given the following information, but you may prefer to do some research for yourself.

    1 Jan 1900 was a Monday.
    Thirty days has September,
    April, June and November.
    All the rest have thirty-one,
    Saving February alone,
    Which has twenty-eight, rain or shine.
    And on leap years, twenty-nine.
    A leap year occurs on any year evenly divisible by 4, 
    but not on a century unless it is divisible by 400.

How many Sundays fell on the first of the month 
during the twentieth century (1 Jan 1901 to 31 Dec 2000)?
-}

euler19 = length 
    $ filter (\d -> d `mod` 7 == 5) -- 1/1/1901 is Tuesday so 5 days after is Sunday 
    $ [getDaysSince1901 y m | y <- [1901..2000], m <- [0..11]]  

getDaysSince1901 :: Int -> Int -> Int
getDaysSince1901 year month  = 
    foldl (+) 0 $ [if leapYear y then 366 else 365 | y <- [1901..(year - 1)]]
                  ++  (map (days) $ take month monthDays)  
    where days = if leapYear year then fst else snd
          leapYear n = n `mod` 4  == 0 && (n `mod` 400 == 0 || n `mod` 100 /= 0) 
          monthDays = [(31, 31), (29, 28), (31, 31), (30, 30), (31, 31), (30, 30),
                       (31, 31), (31, 31), (30, 30), (31, 31), (30, 30), (31, 31)
                  ]
