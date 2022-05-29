module Pangram (isPangram) where

import Data.List (nub)
import Data.Char (toLower)

alphabet :: String
alphabet=['a'..'z']

isPangram :: String -> Bool
isPangram text = length (nub $ filter (`elem` alphabet) $ map toLower text) == 26
