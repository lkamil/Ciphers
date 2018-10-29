import Data.Char

text = "qouoevxnvhi"


encryptChar :: Char -> Char -> Char
encryptChar key letter
    | result > 122 = chr $ result - 26
    | otherwise    = chr result
    where result   = (ord letter) - 97 + (ord key)

encryptText key [] = []
encryptText key text = encryptChar (head $ cycle key) (head $ map toLower text) : (encryptText (drop 1 key') (drop 1 text))
    where key' = cycle key




