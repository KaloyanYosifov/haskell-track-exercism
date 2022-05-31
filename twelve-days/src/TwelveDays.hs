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

recite :: Int -> Int -> [String]
recite start stop
  | stop == 1 = [baseText ++ basePhrase]
  | start == stop = [baseText ++ recite']
  | otherwise = recite start (stop - 1) ++ [baseText ++ recite']
  where baseText = generateBaseText stop'
        stop' = stop - 1
        recite' = (intercalate ", " [x | x <- reverse $ take stop' phrases]) ++ ", and " ++ basePhrase
