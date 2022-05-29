module LuciansLusciousLasagna (elapsedTimeInMinutes, expectedMinutesInOven, preparationTimeInMinutes) where

expectedMinutesInOven :: Int
expectedMinutesInOven = 40

preparationTimeInMinutes :: Int -> Int
preparationTimeInMinutes l = l * 2

elapsedTimeInMinutes :: Int -> Int -> Int
elapsedTimeInMinutes l m = m + (preparationTimeInMinutes l)
