import Data.List.Split

diff :: [Int] -> Int
diff x = maximum x - minimum x

toSpreadsheet :: String -> [[Int]]
toSpreadsheet input = init $ map (\x -> map toInt x) $ map (\x -> splitOn "\t" x) $ splitOn "\n" input
    where toInt x = read x :: Int

solve :: String -> Int
solve input = sum $ map diff $ toSpreadsheet input

evenPair :: [Int] -> (Int, Int)
evenPair row = head [ (a, b) | a <- row, b <- row, a /= b, even $ rem a b, mod a b == 0 ]

evenQuotient :: [Int] -> Int
evenQuotient row = quot (fst pair) (snd pair)
    where pair = evenPair row

solve2 :: String -> Int
solve2 input = sum $ map evenQuotient $ toSpreadsheet input

main :: IO ()
main = do
    contents <- readFile "02-CorruptionCheckSum.txt"
    putStrLn $ show $ solve contents
    putStrLn $ show $ solve2 contents
