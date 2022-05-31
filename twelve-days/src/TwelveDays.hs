module TwelveDays (recite) where

import           Data.List (intercalate)

ordinals :: [String]
ordinals = [
            "first",
            "second",
            "third",
            "fourth",
            "fifth",
            "sixth",
            "seventh",
            "eighth",
            "ninth",
            "tenth",
            "eleventh",
            "twelfth"
           ]

generateBaseText :: Int -> String
generateBaseText number = "On the " ++ (ordinals !! number) ++ " day of Christmas my true love gave to me: "

basePhrase :: String
basePhrase ="a Partridge in a Pear Tree."

phrases :: [String]
phrases = [
        "two Turtle Doves",
        "three French Hens",
        "four Calling Birds",
        "five Gold Rings",
        "six Geese-a-Laying",
        "seven Swans-a-Swimming",
        "eight Maids-a-Milking",
        "nine Ladies Dancing",
        "ten Lords-a-Leaping",
        "eleven Pipers Piping",
        "twelve Drummers Drumming"
        ]


getPhrase :: Int -> String
getPhrase start
    | start == 1 = basePhrase
    | otherwise = (intercalate ", " [x | x <- reverse $ take (start - 1) phrases]) ++ ", and " ++ basePhrase

recite :: Int -> Int -> [String]
recite start stop
  | start == stop = [fullText]
  | otherwise = fullText:recite (start + 1) stop
  where fullText = generateBaseText (start - 1) ++ getPhrase start
