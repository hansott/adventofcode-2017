import Data.Maybe
import Data.List

type Bank = Int
type Memory = [Bank]

update :: Memory -> Int -> Int -> Memory
update mem index blocks = take index mem ++ [blocks] ++ drop (index + 1) mem

nextIndex :: Memory -> Int -> Int
nextIndex mem index = if (index + 1) < length mem then index + 1 else 0

redis :: Memory -> Int -> Int -> Memory
redis mem 0 index = mem
redis mem blocks index = redis updatedMem (blocks - 1) newIndex
    where newValue = (mem !! index) + 1
          updatedMem = update mem index newValue
          newIndex = nextIndex mem index

redistribute :: Memory -> Memory
redistribute mem = redis memToRedis high startIndex
    where high = maximum mem
          highIndex = fromJust $ elemIndex high mem
          memToRedis = update mem highIndex 0
          startIndex = nextIndex mem highIndex

redistributeTill :: Memory -> [Memory] -> Int -> Int
redistributeTill mem history steps =
    if redistributed `elem` history
    then steps + 1
    else redistributeTill redistributed newHistory (steps + 1)
    where redistributed = redistribute mem
          newHistory = redistributed : history

solve :: Memory -> Int
solve mem = redistributeTill mem [] 0

main :: IO ()
main = do
    contents <- readFile "06-MemoryReallocation.txt"
    putStrLn $ show $ solve $ map (\bank -> read bank :: Bank) (words contents)

