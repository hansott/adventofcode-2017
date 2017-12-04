import Data.List

type Passphrase = String

hasUniqueWords :: Passphrase -> Bool
hasUniqueWords passphrase = (length $ wordsOfPassphrase) == (length $ nub $ wordsOfPassphrase)
    where wordsOfPassphrase = words passphrase

passphrasePolicy1 :: Passphrase -> Bool
passphrasePolicy1 = hasUniqueWords

main :: IO ()
main = do
    contents <- readFile "04-HighEntropyPassphrases.txt"
    validPasshprases <- return $ filter passphrasePolicy1 $ lines contents
    putStrLn $ "There are " ++ (show $ length validPasshprases) ++ " valid passphrase(s) for policy 1"
