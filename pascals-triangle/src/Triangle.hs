module Triangle (rows) where

rows' :: Int -> [[Integer]]
rows' x
    | x == 0 = []
    | x == 1 = [[1]]
    | otherwise = evaluatedRow:previousRows
    where previousRows = rows' (x - 1)
          previousRow = head previousRows
          evaluatedRow = zipWith (+) (0:previousRow) (previousRow ++ [0])

rows :: Int -> [[Integer]]
rows = reverse . rows'
