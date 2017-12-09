type Instruction = Int

data State = State { instructions :: [Instruction], index :: Int } deriving (Show)

isValidIndex :: State -> Int -> Bool
isValidIndex state index = index >= 0 && index < (length $ instructions state)

increment1 :: [Instruction] -> Int -> [Instruction]
increment1 instructions index = take index instructions ++ [nextValue] ++ drop (index + 1) instructions
    where currentValue = instructions !! index
          nextValue = currentValue + 1

increment2 :: [Instruction] -> Int -> [Instruction]
increment2 instructions index = take index instructions ++ [nextValue] ++ drop (index + 1) instructions
    where currentValue = instructions !! index
          nextValue = if currentValue >= 3 then currentValue - 1 else currentValue + 1


followInstruction :: State -> ([Instruction] -> Int -> [Instruction]) -> State
followInstruction state increment =
    if isValidIndex state current
    then State { instructions = increment inst current, index = current + (inst !! current) }
    else state
    where current = index state
          inst = instructions state

toInstructions :: String -> [Instruction]
toInstructions input = map (\x -> read x :: Instruction) $ lines input

createState :: [Instruction] -> State
createState instructions = State { instructions = instructions, index = 0 }

getStepsTillOutOfBounds :: (State, Int) -> ([Instruction] -> Int -> [Instruction]) -> (State, Int)
getStepsTillOutOfBounds state steps increment =
    if isValidIndex state current
    then getStepsTillOutOfBounds (followInstruction state increment) (steps + 1) increment
    else steps
    where current = index state

getStepsTillOutOfBounds1 state = getStepsTillOutOfBounds state 0 increment1

getStepsTillOutOfBounds2 state = getStepsTillOutOfBounds state 0 increment2

main :: IO ()
main = do
    contents <- readFile "05-MazeOfTrampolines.txt"
    putStrLn $ show $ getStepsTillOutOfBounds1 $ createState $ toInstructions contents
    putStrLn $ show $ getStepsTillOutOfBounds2 $ createState $ toInstructions contents
