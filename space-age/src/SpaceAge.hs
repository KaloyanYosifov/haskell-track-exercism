module SpaceAge (Planet(..), ageOn) where

data Planet = Mercury
            | Venus
            | Earth
            | Mars
            | Jupiter
            | Saturn
            | Uranus
            | Neptune
            deriving (Eq, Ord)

secondsInEarth :: Float
secondsInEarth = 31557600.0

ageOn :: Planet -> Float -> Float
ageOn planet seconds = let earthAge = seconds / secondsInEarth
                       in case planet of
                        Earth -> earthAge
                        Mercury -> earthAge / 0.2408467
                        Venus -> earthAge / 0.61519726
                        Mars -> earthAge / 1.8808158
                        Jupiter -> earthAge / 11.862615
                        Saturn -> earthAge / 29.447498
                        Uranus -> earthAge / 84.016846
                        Neptune -> earthAge / 164.79132
