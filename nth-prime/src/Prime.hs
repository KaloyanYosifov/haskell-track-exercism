module Prime (nth) where

isqrt :: Int -> Int
isqrt = floor . sqrt . fromIntegral

divides :: Int -> Int -> Bool
divides n = (== 0) . mod n

factorsOf :: Int -> [Int]
factorsOf n = filter (divides n) [1..isqrt n]

nth :: Int -> Maybe Integer
nth n
    | n <= 0 = Nothing
    | otherwise = Just $ getPrimeNumberAtN $ 2:[x | x <- [3..], odd x && length (factorsOf x) == 1]
    where getPrimeNumberAtN = fromIntegral . last . take n
