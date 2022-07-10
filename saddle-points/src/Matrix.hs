module Matrix (saddlePoints) where

import           Data.Array

type Point = (Int, Int)
type SaddleArray = Array Point Int

fetchCols :: Point -> SaddleArray -> [Int]
fetchCols (_, y) arr = fetchCols' minRow
    where fetchCols' :: Int -> [Int]
          fetchCols' row
            | row == maxRow = [arr ! (row, y)]
            | otherwise = (arr ! (row, y)):fetchCols' (row + 1)
          ((minRow, _), (maxRow, _)) = bounds arr

fetchRows :: Point -> SaddleArray -> [Int]
fetchRows (x, _) arr = fetchRows' minCol
    where fetchRows' :: Int -> [Int]
          fetchRows' col
            | col == maxCol = [arr ! (x, col)]
            | otherwise = (arr ! (x, col)):fetchRows' (col + 1)
          ((_, minCol), (_, maxCol)) = bounds arr

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
