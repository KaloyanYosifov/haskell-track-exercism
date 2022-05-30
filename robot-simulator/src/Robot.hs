module Robot
    ( Bearing(East,North,South,West)
    , bearing
    , coordinates
    , mkRobot
    , move
    ) where

data Bearing = North
             | East
             | South
             | West
             deriving (Enum, Eq, Show)

data Robot = Robot {
                  x         :: Integer
                , y         :: Integer
                , direction :: Bearing
             }

moveBasedOnBearing :: Robot -> Robot
moveBasedOnBearing robot@(Robot x' y' dir)
  | dir == North = Robot x' (y' + 1) dir
  | dir == East = Robot (x' + 1) y' dir
  | dir == West = Robot (x' - 1) y' dir
  | dir == South = Robot x' (y' - 1) dir
  | otherwise = robot

bearing :: Robot -> Bearing
bearing = direction

coordinates :: Robot -> (Integer, Integer)
coordinates robot = (x robot, y robot)

mkRobot :: Bearing -> (Integer, Integer) -> Robot
mkRobot dir coord = Robot {x = fst coord, y = snd coord, direction = dir}

turn :: (Int -> Int) -> Bearing -> Bearing
turn f dir = toEnum (f (fromEnum dir) `mod` 4)

move :: Robot -> String -> Robot
move robot [] = robot
move robot@(Robot x' y' dir) [d]
  | d == 'A' = moveBasedOnBearing robot
  | d == 'R' = Robot x' y' $ turn (+1) dir
  | d == 'L' = Robot x' y' $ turn (1 `subtract`) dir
  | otherwise = robot
move robot (d:instructions) = move (move robot [d]) instructions
