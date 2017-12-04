import Data.List

type Passphrase = String

hasUniqueWords :: Passphrase -> Bool
hasUniqueWords passphrase = (length $ wordsOfPassphrase) == (length $ nub $ wordsOfPassphrase)
    where wordsOfPassphrase = words passphrase

isAnagram :: (String, String) -> Bool
isAnagram (a, b) = length a == length b && sort a == sort b

noAnagrams :: Passphrase -> Bool
noAnagrams passphrase = length (filter isAnagram combinations) == 0
    where wordsPass = words passphrase
          combinations = [ (a, b) | a <- wordsPass, b <- wordsPass, a /= b ]

passphrasePolicy1 :: Passphrase -> Bool
passphrasePolicy1 = hasUniqueWords

passphrasePolicy2 :: Passphrase -> Bool
passphrasePolicy2 passphrase = hasUniqueWords passphrase && noAnagrams passphrase

main :: IO ()
main = do
    contents <- readFile "04-HighEntropyPassphrases.txt"
    validPasshprasesPolicy1 <- return $ filter passphrasePolicy1 $ lines contents
    putStrLn $ "There are " ++ (show $ length validPasshprasesPolicy1) ++ " valid passphrases for policy 1"
    validPassphrasesPolicy2 <- return $ filter passphrasePolicy2 $ lines contents
    putStrLn $ "There are " ++ (show $ length validPassphrasesPolicy2) ++ " valid passphrases for policy 2"
