{-
Problem 31

In England the currency is made up of pound, £, and pence, p, 
and there are eight coins in general circulation:

    1p, 2p, 5p, 10p, 20p, 50p, £1 (100p) and £2 (200p).

It is possible to make £2 in the following way:

    1×£1 + 1×50p + 2×20p + 1×5p + 1×2p + 3×1p

How many different ways can £2 be made using any number of coins?
-}

    
import Data.List
import Data.Array.ST
import Control.Monad.ST
import Control.Monad
import Data.Array.Unboxed

euler21 = last $ elems $ calcWays 200

calcWays :: Int -> UArray Int Int
calcWays n = runSTUArray $ do 
    change <- newArray (0, 200) 1
    forM_ coins $ \c -> do
        forM_ [c..200] $ \n -> do
            a <- readArray change n
            b <- readArray change (n - c)
            writeArray change n (a + b)
    return change
        where coins = [2, 5, 10, 20, 50, 100, 200]
