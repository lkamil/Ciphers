import Data.Char
import System.Environment


main :: IO ()
main = do
    args <- getArgs
    case args of
        ["-e", k, t] -> putStrLn $ encrypt (mToLow k) (mToLow t)
        ["-d", k, t] -> putStrLn $ decrypt (mToLow k) (mToLow t)
        []           -> putStrLn "usage: [-e | -d] key text"


encrypt :: [Char] -> [Char] -> [Char]
encrypt key text = vigenere encryptChar key text

decrypt :: [Char] -> [Char] -> [Char]
decrypt key text = vigenere decryptChar key text

vigenere :: (Char -> Char -> Char) -> [Char] -> [Char] -> [Char] 
vigenere direction key [] = []
vigenere direction key text = zipWith direction (cycle key) text

encryptChar :: Char -> Char -> Char
encryptChar key letter
    | result > 122 = chr $ result - 26
    | otherwise    = chr result
    where result   = (ord letter) - 97 + (ord key)

decryptChar :: Char -> Char -> Char
decryptChar key letter
    | result < 97 = chr $ result + 26
    | otherwise = chr result
    where result = (ord letter) + 97 - (ord key)

mToLow :: [Char] -> [Char]
mToLow text = map toLower text
