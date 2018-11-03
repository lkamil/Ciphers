import System.Random
import Data.List
import System.Environment

main :: IO ()
main = do
    args <- getArgs
    case args of
        [a, b, c] -> putStrLn $ show $ publicKey $ map read [a, b, c]
        _ -> do
            r0 <- randomPrime
            r1 <- randomPrime
            r2 <- randomPrime
            putStrLn $ show [r0, r1, r2]

publicKey :: Integral a => [a] -> (a, a)
publicKey [p1, p2, p3] = (p1, p2 * p3)

privateKey :: Integral a => [a] -> (a, a)
privateKey [p1, p2, p3] = ((extractSec $ extEuclid (p1, m)), m)
    where m = (p2 - 1) * (p3 - 1)

primeGen :: [Int]
primeGen = primeGenH [x | x <- [2..100], mod x 2 /= 0]

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
    

