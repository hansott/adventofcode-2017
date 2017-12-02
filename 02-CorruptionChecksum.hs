import Data.List.Split

diff :: [Int] -> Int
diff x = maximum x - minimum x

toSpreadsheet :: String -> [[Int]]
toSpreadsheet input = init $ map (\x -> map toInt x) $ map (\x -> splitOn "\t" x) $ splitOn "\n" input
    where toInt x = read x :: Int

solve :: String -> Int
solve input = sum $ map diff $ toSpreadsheet input

main :: IO ()
main = do
    contents <- readFile "02-CorruptionCheckSum.txt"
    putStrLn $ show $ solve contents
