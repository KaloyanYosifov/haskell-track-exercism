module DNA (nucleotideCounts, Nucleotide(..)) where

import Data.List (foldl')
import Data.Map (Map, empty, insertWith)

data Nucleotide = A | C | G | T deriving (Eq, Ord, Show, Enum, Read)

getNucleotide :: Char -> Either String Nucleotide
getNucleotide 'A' = Right A
getNucleotide 'C' = Right C
getNucleotide 'G' = Right G
getNucleotide 'T' = Right T
getNucleotide _ = Left "error"

nucleotideCounts :: String -> Either String (Map Nucleotide Int)
nucleotideCounts = foldl'
        (\acc x -> do
                n <- getNucleotide x
                insertWith (+) n 1 <$> acc) (Right empty)
