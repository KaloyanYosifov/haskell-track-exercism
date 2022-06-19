module Triangle (rows) where

rows' :: Int -> [[Integer]]
rows' x
    | x == 0 = []
    | x == 1 = [[1]]
    | otherwise = (1:evaluatedRow):previousRows
    where previousRows = rows' (x - 1)
          previousRow = head previousRows
          evaluatedRow = zipWith (+) (init previousRow) (tail previousRow) ++ [1]

rows :: Int -> [[Integer]]
rows = reverse . rows'
