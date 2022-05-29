module DNA (toRNA) where

toRNA :: String -> Either Char String
toRNA [] = Right ""
toRNA [e] = case e of
            'G' -> Right "C"
            'C' -> Right "G"
            'T' -> Right "A"
            'A' -> Right "U"
            _ -> Left e
toRNA (x:xs) = case toRNA [x] of
                 Left a -> Left a
                 Right xValue -> case toRNA xs of
                    Left a -> Left a
                    Right xsValue -> Right (xValue ++ xsValue)
