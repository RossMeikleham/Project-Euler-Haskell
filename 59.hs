{-# LANGUAGE BangPatterns #-}

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC8
import Data.List.Split
import Data.Bits
import Data.Word
import Prelude

filename = "p059_cipher.txt"

word :: BS.ByteString
word =  BSC8.pack "chapter" 

-- All possible key combinations 
combinations :: [(Word8, Word8, Word8)]
combinations = [(x,y,z) | x <- [97..122], y <- [97..122], z <- [97..122]]

applyKey :: (Word8, Word8, Word8) -> [Word8] -> BS.ByteString
applyKey key cipherText = BS.pack $ applyKey' [] key cipherText
  where
    applyKey' :: [Word8] -> (Word8, Word8, Word8) -> [Word8] -> [Word8]
    applyKey' res _ [] = res
    applyKey' res (x, _, _) (a:[])   = res ++ [a `xor` x]
    applyKey' res (x, y, _) (a:b:[]) = res ++ [a `xor` x, b `xor` y]
    applyKey' res key@(x, y, z) (a:b:c:xs) =  
        applyKey' (res ++ [a `xor` x, b `xor` y, c `xor` z]) key xs

getCipherText :: IO [Word8]
getCipherText = do
    contents <- readFile filename 
    return $ map read $ splitOn "," contents

findKey :: [(Word8, Word8, Word8)] -> [Word8] -> Maybe (Word8, Word8, Word8)
findKey [] _ = Nothing
findKey (key:keys) cipherText = 
        if word `BS.isInfixOf` decryptedText
            then return key
            else findKey keys cipherText
    where
        decryptedText = applyKey key cipherText

euler59 = do
    cipherText <- getCipherText
    let !key = findKey combinations cipherText
    putStrLn "Key:"
    print key
    case key of
        Just k -> do
            let originalText = applyKey k cipherText 
            putStrLn "Answer:"
            let answer = BS.foldl' (\a b -> a + (fromIntegral b) :: Int) 0 originalText 
            print answer
            putStrLn "Text:"
            BSC8.putStrLn originalText
            
        Nothing -> return ()
