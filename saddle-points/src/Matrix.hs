module Matrix (saddlePoints) where

import           Data.Array (Array)

newtype Point = Point (Integer, Integer)

saddlePoints :: Array i e -> [i]
saddlePoints matrix = error "You need to implement this function."
