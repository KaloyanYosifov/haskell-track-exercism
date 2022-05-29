module Anagram (anagramsFor) where

import           Data.Char (toLower)
import           Data.List (sort)

lowerString :: String -> String
lowerString = map toLower

hasTheSameLetters :: String -> String -> Bool
hasTheSameLetters [] _ = False
hasTheSameLetters xs xs'
    | length xs /= length xs' = False
    | otherwise = sort xsL == sort xsL'
  where xsL = lowerString xs
        xsL' = lowerString xs'

anagramsFor :: String -> [String] -> [String]
anagramsFor xs = filter (\xs' -> hasTheSameLetters xs xs' && lowerString xs' /= lowerString xs)
