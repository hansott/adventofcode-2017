import Data.List.Split
import Data.List

type ID = Int
type Pipe = (ID, ID)

test :: String
test = "0 <-> 2\n\
\1 <-> 1\n\
\2 <-> 0, 3, 4\n\
\3 <-> 2, 4\n\
\4 <-> 2, 3, 6\n\
\5 <-> 6\n\
\6 <-> 4, 5"

parseLine :: String -> (ID, [ID])
parseLine line = (pipe, pipes)
    where tokens = splitOn "<->" line
          pipe = read $ head tokens :: ID
          commaSeperated = drop 1 tokens
          pipes = map (\x -> read x :: ID) $ splitOn "," $ head commaSeperated

parse :: String -> [(ID, [ID])]
parse input = map parseLine lns
    where lns = lines input

connect :: [(ID, [ID])] -> [Pipe]
connect conns = concat $ map toPipe conns
    where toPipe = \(pipe, pipes) -> zip (repeat pipe) pipes

withId :: ID -> Pipe -> Bool 
withId id (a, b) = a == id || b == id

getId :: Pipe -> ID
getId (a, _) = a

addId :: [ID] -> Pipe -> [ID]
addId ids (a, b) =
    if a `elem` ids
    then
        if b `elem` ids
        then ids
        else b:ids
    else
        if b `elem` ids
        then a:ids
        else a:b:ids

uniqueIds :: [Pipe] -> [ID]
uniqueIds pipes = foldl addId [] pipes

connectedTo :: [Pipe] -> [ID] -> ID -> [Pipe]
connectedTo pipes seenIds id =
    if id `elem` seenIds
    then []
    else directly ++ indirectly
    where directly = filter (withId id) pipes
          indirectly = concat $ map (connectedTo pipes (id:seenIds)) $ map getId directly  

connectedTo' :: [Pipe] -> ID -> [Pipe]
connectedTo' pipes id = connectedTo pipes [] id

pipes :: String -> [Pipe]
pipes input = connect $ parse input

main :: IO ()
main = do
    contents <- readFile "12-DigitalPlumber.txt"
    putStrLn $ show $ length $ uniqueIds $ connectedTo' (pipes contents) 0
