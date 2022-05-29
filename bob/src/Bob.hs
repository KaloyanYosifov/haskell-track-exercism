module Bob (responseFor) where

import           Data.Char (isSpace, isUpper)

trim :: String -> String
trim = f . f
   where f = reverse . dropWhile isSpace

isAllUpper :: String -> Bool
isAllUpper "" = False
isAllUpper xs
  | filteredXs == "" = False
  | otherwise = isAllUpper' xs
  where isAllUpper' "" = True
        isAllUpper' (x:xs')
            | x `notElem` ( ['A'..'Z'] ++ ['a'..'z']) = (xs == "") || isAllUpper' xs'
            | otherwise = isUpper x && isAllUpper' xs'
        filteredXs = filter (`elem` (['a'..'z'] ++ ['A'..'Z'])) xs

responseFor :: String -> String
responseFor xs
  | xs' == "" = "Fine. Be that way!"
  | isAllUpper xs' && endsWithQuestionMark xs' = "Calm down, I know what I'm doing!"
  | isAllUpper xs' = "Whoa, chill out!"
  | endsWithQuestionMark xs' = "Sure."
  | otherwise = "Whatever."
  where endsWithQuestionMark = (=='?') . last
        xs' = trim xs
