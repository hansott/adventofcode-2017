type Instruction = Int

data State = State { instructions :: [Instruction], index :: Int } deriving (Show)

isValidIndex :: State -> Int -> Bool
isValidIndex state index = index >= 0 && index < (length $ instructions state)

increment :: [Instruction] -> Int -> [Instruction]
increment instructions index = take index instructions ++ [nextValue] ++ drop (index + 1) instructions
    where currentValue = instructions !! index
          nextValue = currentValue + 1

followInstruction :: State -> State
followInstruction state =
    if isValidIndex state current
    then State { instructions = increment inst current, index = current + (inst !! current) }
    else state
    where current = index state
          inst = instructions state

toInstructions :: String -> [Instruction]
toInstructions input = map (\x -> read x :: Instruction) $ lines input

createState :: [Instruction] -> State
createState instructions = State { instructions = instructions, index = 0 }

getStepsTillOutOfBounds :: State -> Int -> Int
getStepsTillOutOfBounds state steps =
    if isValidIndex state current
    then getStepsTillOutOfBounds (followInstruction state) (steps + 1)
    else steps
    where current = index state

getStepsTillOutOfBounds' state = getStepsTillOutOfBounds state 0

main :: IO ()
main = do
    contents <- readFile "05-MazeOfTrampolines.txt"
    putStrLn $ show $ getStepsTillOutOfBounds' $ createState $ toInstructions contents
