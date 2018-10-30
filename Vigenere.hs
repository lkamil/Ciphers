import Data.Char

encryptText :: [Char] -> [Char] -> [Char]
encryptText key text = vigenere encryptChar key text

decryptText :: [Char] -> [Char] -> [Char]
decryptText key text = vigenere decryptChar key text

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
