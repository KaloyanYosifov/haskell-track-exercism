module TPalindrome(isPalindrome, createAPerson) where

import Data.List (elemIndices)

isPalindrome' :: String -> Bool
isPalindrome' word = let firstIndex = head $ elemIndices '(' word
                         secondIndex = last $ elemIndices ')' word
                         baseWord = take firstIndex word ++ "*" ++ drop (secondIndex + 1) word
                         innerWord = take (secondIndex - firstIndex - 1) (drop (firstIndex + 1) word)
                     in baseWord == reverse baseWord && isPalindrome innerWord

isPalindrome :: String -> Bool
isPalindrome word
    | openParenthesis == 0 || openParenthesis /= closedParenthesis = word == reverse word
    | otherwise = isPalindrome' word
    where openParenthesis = length $ elemIndices '(' word
          closedParenthesis = length $ elemIndices ')' word


data Person = Person { firstName :: String, lastName:: String, age :: Int } deriving (Show)

createAPerson :: String -> Int -> Person
createAPerson name age = Person firstName lastName age
    where firstName = head $ words name
          lastName = last $ words name
