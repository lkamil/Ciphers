import System.Random
import Data.List
import Data.Char
import System.Environment

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["-g"] -> do
            r0 <- randomPrime
            r1 <- randomPrime
            r2 <- randomPrime
            putStrLn $ show $ publicKey [r0, r1, r2]
            putStrLn $ show $ privateKey [r0, r1, r2]
        ["-g", p0, p1, p2] -> do
            putStrLn $ show $ publicKey $ map read [p0, p1, p2]
            putStrLn $ show $ privateKey $ map read [p0, p1, p2]
        ["-e", m] -> do
            r0 <- randomPrime
            r1 <- randomPrime
            r2 <- randomPrime
            let puk = publicKey [r0, r1, r2]
            putStrLn $ show $ puk
            putStrLn $ show $ privateKey [r0, r1, r2]
            putStrLn $ show $ encryptMessage puk m
        ["-e", p0, p1, p2, m] ->
            putStrLn $ show $ encryptMessage (publicKey $ map read [p0, p1, p2]) m
        ["-e", puk0, puk1, m] ->
            putStrLn $ show $ encryptMessage (read puk0, read puk1) m
        ["-d", prk0, prk1, m] ->
            putStrLn $ decryptMessage (read prk0, read prk1) m
        m -> do
            r0 <- randomPrime
            r1 <- randomPrime
            r2 <- randomPrime
            -- putStrLn $ encryptMessage (publicKey [r0, r1, r2]) (unlines m)
            putStrLn $ show $ publicKey [r0, r1, r2]

-- encrypt gets two arguments: the public key and the message that should be enrypted
encrypt :: Integral a => (a, a) -> a -> a 
encrypt puk message = mod (message ^ e) n
    where e = fst puk
          n = snd puk

decrypt :: Integral a => (a, a) -> a -> a
decrypt prk message = mod (message ^ d) n
    where d = fst prk
          n = snd prk

publicKey :: Integral a => [a] -> (a, a)
publicKey [p1, p2, p3] = (p1, p2 * p3)

privateKey :: Integral a => [a] -> (a, a)
privateKey [p1, p2, p3] = ((extractSec $ extEuclid (p1, m)), m)
    where m = (p2 - 1) * (p3 - 1)

primeGen :: [Int]
primeGen = primeGenH [x | x <- [2..1000], mod x 2 /= 0]

primeGenH :: Integral a => [a] -> [a]
primeGenH [] = []
primeGenH (x:xs) = x : (primeGenH $ filter (\y -> mod y x /= 0) xs)

randomPrime :: IO Int
randomPrime = 
    let ps = primeGen
    in fmap (ps !!) $ randomRIO (0, (length ps)-1)

extEuclid (a, 0) = (a, 1, 0)
extEuclid (a, b) = (d, t, s - t * (div a b))
    where (d, s, t) = extEuclid (b, mod a b)

extractSec (_, a, _) = a

encryptMessage puk message = map (encrypt puk) (convertToNums message)
decryptMessage prk message = convertBack $ map (decrypt prk) (convertToNums message)

-- Text to Numbers Conversion

convertToNums message = map (ord) message

convertBack nums = map (chr) nums
