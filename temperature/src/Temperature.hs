module Temperature (tempToC, tempToF) where

tempToC :: Integer -> Float
tempToC = nearestHundreth .  (/1.8) . fromIntegral . (32 `subtract`)
    where nearestHundreth :: Float -> Float
          nearestHundreth x = fromIntegral (round (x * 100)) / 100

tempToF :: Float -> Integer
tempToF = ceiling . (+32) . (*1.8)
