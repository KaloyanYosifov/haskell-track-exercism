module Hamming (distance) where

distance :: String -> String -> Maybe Int
distance [] [] = Just 0
distance _ [] = Nothing
distance [] _ = Nothing
distance xs@(x:xs') ys@(y:ys')
    | length xs /= length ys = Nothing
    | x /= y = (+1) <$> distance xs' ys'
    | otherwise = distance xs' ys'
