module CollatzConjecture (collatz) where

collatz :: Integer -> Maybe Integer
collatz x
    | x <= 0 = Nothing
    | x == 1 = Just 0
    | odd x = (+1) <$> collatz (x * 3 + 1)
    | otherwise = (+1) <$> collatz (x `div` 2)


rather :: String -> Maybe String
rather "" = Nothing
rather test = Just test

fasting = (++ " not bad") <$> rather ""
