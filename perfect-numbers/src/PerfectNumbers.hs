module PerfectNumbers (classify, Classification(..)) where

data Classification = Deficient | Perfect | Abundant deriving (Eq, Show)

divisorsOfNumber :: Int -> [Int]
divisorsOfNumber x = divisorsOfNumber' x (x - 1)
    where divisorsOfNumber' :: Int -> Int -> [Int]
          divisorsOfNumber' n d
            | d <= 0 = []
            | n `mod` d == 0 = d:divisorsOfNumber' n (d - 1)
            | otherwise = divisorsOfNumber' n $ d - 1

classify :: Int -> Maybe Classification
classify x
    | x <= 0 = Nothing
    | x == aliquotSummed = Just Perfect
    | x < aliquotSummed = Just Abundant
    | otherwise = Just Deficient
  where aliquotSummed = sum $ divisorsOfNumber x
