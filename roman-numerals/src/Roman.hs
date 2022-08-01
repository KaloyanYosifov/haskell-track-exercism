module Roman (numerals) where

numerals :: Integer -> Maybe String
numerals n
    | n == 0 = Just ""
    | n - 1000 >= 0 = Just "M" <> numerals (n - 1000)
    | n - 900 >= 0 = Just "CM" <> numerals (n - 900)
    | n - 500 >= 0 = Just "D" <> numerals (n - 500)
    | n - 400 >= 0 = Just "CD" <> numerals (n - 400)
    | n - 100 >= 0 = Just "C" <> numerals (n - 100)
    | n - 90 >= 0 = Just "XC" <> numerals (n - 90)
    | n - 50 >= 0 = Just "L" <> numerals (n - 50)
    | n - 40 >= 0 = Just "XL" <> numerals (n - 40)
    | n - 10 >= 0 = Just "X" <> numerals (n - 10)
    | n - 9 >= 0 = Just "IX" <> numerals (n - 9)
    | n - 5 >= 0 = Just "V" <> numerals (n - 5)
    | n - 4 >= 0 = Just "IV" <> numerals (n - 4)
    | n - 1 >= 0 = Just "I" <> numerals (n - 1)
    | otherwise = Nothing
