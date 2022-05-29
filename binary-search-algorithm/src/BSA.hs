module BSA (search) where

sumMaybeInt :: (Num a) => Maybe a -> Maybe a -> Maybe a
sumMaybeInt Nothing _          = Nothing
sumMaybeInt _ Nothing          = Nothing
sumMaybeInt (Just v) (Just v1) = Just (v + v1)

search :: (Ord a) => [a] -> a -> Maybe Int
search [] _ = Nothing
search [x] v = if x == v then Just 0 else Nothing
search xs v
    | x == v = Just (length firstPart)
    | v > x = Just (length firstPart + 1) `sumMaybeInt` search secondPart v
    | v < x = search firstPart v
    | otherwise = Nothing
    where (firstPart, x:secondPart) = splitAt (length xs `div` 2) xs
