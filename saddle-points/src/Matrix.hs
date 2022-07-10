module Matrix (saddlePoints) where

import           Data.Array

type Point = (Int, Int)
type SaddleArray = Array Point Int

fetchRows :: Point -> SaddleArray -> [Int]
fetchRows (x, _) arr = [arr ! (x, c) | c <- [minCol..maxCol]]
    where ((_, minCol), (_, maxCol)) = bounds arr

fetchCols :: Point -> SaddleArray -> [Int]
fetchCols (_, y) arr = [arr ! (r, y) | r <- [minRow..maxRow]]
    where ((minRow, _), (maxRow, _)) = bounds arr

loop :: Point -> SaddleArray -> [Point]
loop point@(x, y) arr
    | isGEOrEQToInRow && isLTOrEQToInCol = point:getNextPoint
    | otherwise = getNextPoint
    where cols = fetchCols point arr
          rows = fetchRows point arr
          pointValue = arr ! point
          isGEOrEQToInRow = all (<= pointValue) rows
          isLTOrEQToInCol = all (>= pointValue) cols
          ((_, minCol), (maxRow, maxCol)) = bounds arr
          getNextPoint
            | x == maxRow && y == maxCol = []
            | y == maxCol = loop (x + 1, minCol) arr
            | otherwise = loop (x, y + 1) arr

saddlePoints :: SaddleArray -> [Point]
saddlePoints matrix
    | null matrix = []
    | otherwise = loop (1, 1) matrix
