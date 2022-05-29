module CryptoSquare (encode) where

import Data.Char (toLower)
import Data.List (transpose)

specialCharacters :: String
specialCharacters = ",.; !@#$%^&*()/\\?"

findProduct :: Int -> Int -> Int -> (Int, Int)
findProduct x y target
  | isInConstraints && x * y >= target  = (max x y, min x y)
  | otherwise = findProduct newX newY target
    where isInConstraints = abs (x - y) <= 1
          newX = if x == y then x + 1 else x
          newY = if x == y then y else y + 1

normalizeForCrypto :: String -> String
normalizeForCrypto = map toLower . filter (`notElem` specialCharacters)

chunkWord :: String -> Int -> [String]
chunkWord xs wordSize
  | length xs <= wordSize = [xs ++ replicate lengthAndWordSizeDiff ' ']
  | otherwise = take wordSize xs : chunkWord (drop wordSize xs) wordSize
  where lengthAndWordSizeDiff = abs (length xs - wordSize)


encode :: String -> String
encode [] = ""
encode xs = unwords $ transpose chunkedWord
    where normalizedXs = normalizeForCrypto xs
          (cols, _) = findProduct 1 1 (length normalizedXs)
          chunkedWord = chunkWord (normalizeForCrypto normalizedXs) cols
