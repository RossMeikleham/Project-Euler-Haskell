{- Problem 50

The prime 41, can be written as the sum of six consecutive primes:
41 = 2 + 3 + 5 + 7 + 11 + 13

This is the longest sum of consecutive primes that adds to a prime 
below one-hundred.

The longest sum of consecutive primes below one-thousand that adds 
to a prime, contains 21 terms, and is equal to 953.

Which prime, below one-million, can be written as the sum of the most consecutive primes?
-}

import Primes
import Control.Monad.State.Lazy
import qualified Data.Vector.Unboxed as VU


primes = getPrimesUpTo 4000
cSumPrimes = VU.fromList $ scanl (+) 0 primes


data CPrimes = CPrimes {
    cumulativeSumPrimes :: VU.Vector Int, -- Int,
    curLeft :: Int,
    curRight :: Int,
    curMaxLength :: Int,
    curPrime :: Int
} deriving Show

type PrimeState a = State CPrimes a

setMaxPathLength :: Int -> PrimeState ()
setMaxPathLength i = get >>= \s -> put $ s {curMaxLength = i} 

getCurrentPathLength :: PrimeState Int
getCurrentPathLength =  get >>= \s -> return $ curRight s - curLeft s

getCurrentMaxPathLength :: PrimeState Int
getCurrentMaxPathLength = get >>= \s -> return $ curMaxLength s

setPrime :: Int -> PrimeState ()
setPrime i = get >>= \s -> put $ s {curPrime = i} 


outOfBounds :: PrimeState Bool
outOfBounds = do
    s <- get
    let left =  curLeft s
        right = curRight s
        cSumPrimes = cumulativeSumPrimes s
    return $ left < 0 || right < 0 || left >= VU.length cSumPrimes || right >= VU.length cSumPrimes ||
            cSumPrimes VU.! right >= 1000000

moveLeft :: Int -> PrimeState ()
moveLeft i = get >>= \s -> put $ s {curLeft = (curLeft s) + i}

setLeft :: Int -> PrimeState ()
setLeft i = get >>= \s -> put $ s {curLeft = i}

moveRight :: Int -> PrimeState ()
moveRight i = get >>= \s -> put $ s {curRight = (curRight s) + i}
   
getCPrimeSum :: PrimeState Int
getCPrimeSum = do
    s <- get
    let cSumPrimes = cumulativeSumPrimes s
    return $ (cSumPrimes VU.! (curRight s)) -  (cSumPrimes VU.! (curLeft s))    


euler50 = print $ curPrime $ execState getLongest (CPrimes cSumPrimes 0 1 0 0)


getLongest :: PrimeState ()
getLongest = do 
    ooBounds <- outOfBounds
    if ooBounds
      then return ()
      else do
          p <- getCPrimeSum 
          if isPrime p
              then do
                  curLen <- getCurrentPathLength
                  maxLen <- getCurrentMaxPathLength

                  when (curLen > maxLen) (do                     
                      setPrime p
                      setMaxPathLength curLen)
                  setLeft 0
                  moveRight 1

              else do
                  moveLeft 1
                  
          getLongest
