module SecretHandshake (handshake) where

secrets :: [String]
secrets = ["wink", "double blink", "close your eyes", "jump"]

decimalToBinary :: Int -> String
decimalToBinary 0 = "0"
decimalToBinary 1 = "1"
decimalToBinary x = decimalToBinary newN ++ show remainder
    where (newN, remainder) = abs x `divMod` 2

binaryToString :: String -> [String]
binaryToString [] = []
binaryToString (x:xs)
  | x == '0' = binaryToString xs
  | length (x:xs) == maxBinaryLength = reverse $ binaryToString xs
  | otherwise = binaryToString xs ++ [secrets !! length xs]
  where maxBinaryLength = 5

handshake :: Int -> [String]
handshake n = binaryToString $ decimalToBinary n
