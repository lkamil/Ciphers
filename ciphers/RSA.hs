import System.Random
import Data.List
import Data.Char
import System.Environment

main :: IO ()
main = do
    args <- getArgs
    case args of
        -- generate keys using random prime numbers
        ["-g"] -> do
            primes <- get3Primes
            putStrLn $ show $ publicKey primes
            putStrLn $ show $ privateKey primes
        -- generate keys given 3 prime numbers
        ["-g", p0, p1, p2] -> do
            putStrLn $ show $ publicKey $ map read [p0, p1, p2]
            putStrLn $ show $ privateKey $ map read [p0, p1, p2]
        -- encode a message
        ["-e", m] -> do
            primes <- get3Primes
            let puk = publicKey primes
            putStrLn $ show $ puk
            putStrLn $ show $ privateKey primes
            putStrLn $ show $ encryptMessage puk m
        -- encode a message with given primes
        ["-e", p0, p1, p2, m] ->
            putStrLn $ show $ encryptMessage (publicKey $ map read [p0, p1, p2]) m
        -- encode a message with given keys
        ["-e", puk0, puk1, m] ->
            putStrLn $ show $ encryptMessage (read puk0, read puk1) m
        -- decode a message with given keys
        ["-d", prk0, prk1, m] ->
            putStrLn $ show $ decryptMessage (read prk0, read prk1) m
        -- _ -> print help


-- Encrypt/Decrypt given a public/private Key

encrypt :: Integral a => (a, a) -> a -> a 
encrypt puk val = mod (val ^ e) n
    where e = fst puk
          n = snd puk

decrypt :: Integral a => (a, a) -> a -> a
decrypt prk message = mod (message ^ d) n
    where d = fst prk
          n = snd prk


-- Generate public/private key given three numbers

publicKey :: [Integer] -> (Integer, Integer)
publicKey [p1, p2, p3] = (p1, p2 * p3)

privateKey :: Integral a => [a] -> (a, a)
privateKey [p1, p2, p3] = ((extractSec $ extEuclid (p1, m)), m)
    where m = (p2 - 1) * (p3 - 1)


-- Generate array of primes

primeGen :: [Integer]
primeGen = primeGenH [x | x <- [2..1000], mod x 2 /= 0]

primeGenH :: Integral a => [a] -> [a]
primeGenH [] = []
primeGenH (x:xs) = x : (primeGenH $ filter (\y -> mod y x /= 0) xs)

get3Primes = do
    r0 <- randomPrime
    r1 <- randomPrime
    r2 <- randomPrime
    return [r0, r1, r2]


-- pick a random prime number
randomPrime :: IO Integer
randomPrime = 
    let ps = primeGen
    in fmap (ps !!) $ randomRIO (0, (length ps)-1)

-- calculate the multiplicative inverse with the extended Euclidean algorithm
extEuclid (a, 0) = (a, 1, 0)
extEuclid (a, b) = (d, t, s - t * (div a b))
    where (d, s, t) = extEuclid (b, mod a b)

extractSec (_, a, _) = a


-- TODO: implement a convenient message to number (hex?) converter

-- encryptMessage :: (Integral a, Read a) => (a, a) -> String -
encryptMessage puk message = encrypt puk (read message :: Integer)
decryptMessage prk message = decrypt prk (read message :: Integer)

convertToNums :: Read a => String -> a
convertToNums message = read message
convertBack nums = show nums
