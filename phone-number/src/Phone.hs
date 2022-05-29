module Phone (number) where

import           Data.Char (isNumber)

isValidNumber :: String -> Bool
isValidNumber [c, _, _, c1, _, _, _, _, _, _] = c > '1' && c1 > '1'
isValidNumber _                               = False

number' :: String -> Maybe String
number' xs
    | length xs == 11 && head xs == '1' = number' $ tail xs
    | isValidNumber xs = Just xs
    | otherwise = Nothing

number :: String -> Maybe String
number = number' . filter isNumber
