module Strain (keep, discard) where

discard :: (a -> Bool) -> [a] -> [a]
discard p xs = [x | x <- xs, rP x]
    where rP v = not $ p v

keep :: (a -> Bool) -> [a] -> [a]
keep p xs = [x | x <- xs, p x]
