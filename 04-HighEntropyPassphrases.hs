import Data.List

type Passphrase = String

isValidPassphrase :: Passphrase -> Bool
isValidPassphrase passphrase = (length $ wordsOfPassphrase) == (length $ nub $ wordsOfPassphrase)
    where wordsOfPassphrase = words passphrase

main :: IO ()
main = do
    contents <- readFile "04-HighEntropyPassphrases.txt"
    validPasshprases <- return $ filter isValidPassphrase $ lines contents
    putStrLn $ "There are " ++ (show $ length validPasshprases) ++ " valid passphrase(s)"
