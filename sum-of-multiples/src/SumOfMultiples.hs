module SumOfMultiples (sumOfMultiples) where

sumOfMultiples :: [Integer] -> Integer -> Integer
sumOfMultiples factors limit = sum [x | x <- [1..(limit - 1)], any (\v -> x `mod` v == 0) normalizedFactors ]
    where normalizedFactors = filter (/= 0) factors
