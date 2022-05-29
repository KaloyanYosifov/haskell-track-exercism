module Clock (addDelta, fromHourMin, toString) where

data Clock = Clock { hours   :: Int
                   , minutes :: Int
                   }
  deriving (Eq)

parseHours :: Int -> Int
parseHours x
    | x /= 0 = x `mod` 24
    | otherwise = x

parseMinutes :: Int -> Int
parseMinutes x
    | x /= 0 = x `mod` 60
    | otherwise = x

getOverHours :: Int -> Int
getOverHours x
  | x == 0 = 0
  | otherwise = floor (fromIntegral x / 60)

fromHourMin :: Int -> Int -> Clock
fromHourMin h m = addDelta h m (Clock 0 0)

toString :: Clock -> String
toString (Clock h m) = printFormatted (show h) ++ ":" ++ printFormatted (show m)
    where printFormatted x
            | length x <= 1 = "0" ++ x
            | otherwise = x

addDelta :: Int -> Int -> Clock -> Clock
addDelta 0 0 clock@(Clock _ _) = clock
addDelta h m (Clock cH cM) = Clock hoursToAdd minutesToAdd
  where minutesToAdd = parseMinutes $ cM + m
        hoursToAdd = parseHours $ cH + h + getOverHours (cM + m)
