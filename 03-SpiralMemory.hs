data Direction = North | East | South | West deriving (Show)

data Point = Point { x :: Int, y :: Int, number :: Int } deriving (Show)

data Spiral = Spiral { point :: Point, direction :: Direction, size :: Int, step :: Int, turns :: Int } deriving (Show)

addPoint :: Spiral -> Spiral
addPoint (Spiral currentPoint direction size step turns) =
    if shouldTurn
    then
        if shouldIncreaseSize
        then Spiral { point = nextIfTurn, direction = nextDirection direction, size = size + 1, step = 1, turns = 1 }
        else Spiral { point = nextIfTurn, direction = nextDirection direction, size = size, step = 1, turns = turns + 1 }
    else Spiral { point = next, direction = direction, size = size, step = step + 1, turns = turns }
    where next = nextPoint currentPoint direction
          nextIfTurn = nextPoint currentPoint direction
          shouldTurn = size == step
          shouldIncreaseSize = turns == 2

startPoint :: Point
startPoint = Point { x = 0, y = 0, number = 1 }

createSpiral :: Spiral
createSpiral = Spiral { point = startPoint, direction = West, size = 1, step = 1, turns = 1 }

nextPoint :: Point -> Direction -> Point
nextPoint (Point x y number) North = Point { x = x, y = y + 1, number = number + 1 }
nextPoint (Point x y number) East = Point { x = x - 1, y = y, number = number + 1 }
nextPoint (Point x y number) South = Point { x = x, y = y - 1, number = number + 1 }
nextPoint (Point x y number) West = Point { x = x + 1, y = y, number = number + 1 }

nextDirection :: Direction -> Direction
nextDirection North = East
nextDirection East = South
nextDirection South = West
nextDirection West = North

spiral :: Int -> Spiral
spiral number = iterate addPoint createSpiral !! (number - 1)

distance :: Point -> Int
distance (Point x y _) = abs x + abs y

main :: IO ()
main = do
    putStrLn $ show $ distance $ point $ spiral 312051
