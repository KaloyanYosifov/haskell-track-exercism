module Acronym (abbreviate) where

import Data.Char (toUpper, isLetter)

isAllCaps :: String -> Bool
isAllCaps [] = True
isAllCaps (x:xs)
  | x `elem` ['A'..'Z'] = isAllCaps xs
  | otherwise = False

isAlphabetCharOrApostrophe :: Char -> Bool
isAlphabetCharOrApostrophe x = x == '\'' || isLetter x

splitWords :: String -> [String]
splitWords [] = []
splitWords xs
    | null second = [first]
    | otherwise = first:splitWords (tail second)
    where (first, second) = span isAlphabetCharOrApostrophe xs

filterOutAllCaps :: [String] -> [String]
filterOutAllCaps = map (\x -> if isAllCaps x then [head x] else x)

abbreviate :: String -> String
abbreviate = concatMap getAcronyms . filterOutAllCaps . filter (not . null) . splitWords
    where getAcronyms [] = ""
          getAcronyms (x:xs) = toUpper x : filter (`elem` ['A'..'Z']) xs
