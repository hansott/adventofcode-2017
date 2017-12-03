data Direction = North | East | South | West deriving (Show)

data Point = Point { x :: Int, y :: Int, number :: Int } deriving (Show)

data Spiral = Spiral { points :: [Point], direction :: Direction, size :: Int, step :: Int, turns :: Int } deriving (Show)

addPoint :: Spiral -> Spiral
addPoint (Spiral currentPoints direction size step turns) =
    if shouldTurn
    then
        if shouldIncreaseSize
        then Spiral { points = currentPoints ++ [nextIfTurn], direction = nextDirection direction, size = size + 1, step = 1, turns = 0 }
        else Spiral { points = currentPoints ++ [nextIfTurn], direction = nextDirection direction, size = size, step = 1, turns = turns + 1 }
    else Spiral { points = currentPoints ++ [next], direction = direction, size = size, step = step + 1, turns = turns }
    where lastPoint = last currentPoints
          next = nextPoint lastPoint direction
          nextIfTurn = nextPoint lastPoint direction
          shouldTurn = size == step
          shouldIncreaseSize = turns == 2

startPoint :: Point
startPoint = Point { x = 0, y = 0, number = 1 }

startDirection :: Direction
startDirection = West

createSpiral :: Spiral
createSpiral = Spiral { points = [startPoint], direction = startDirection, size = 1, step = 1, turns = 0 }

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

input :: String
input = "312051"

toInt :: String -> Int
toInt x = read x :: Int

main :: IO ()
main = do
    putStrLn "yo"
