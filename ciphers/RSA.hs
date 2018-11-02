import System.Random
import Data.List

-- main :: IO ()
-- main = do
--     r0 <- randomPrime
--     r1 <- randomPrime
--     r2 <- randomPrime
--     print [r0, r1, r2]

primes = [23, 11, 13]

publicKey = (primes !! 0, (primes !! 1) * (primes !! 2))

privateKey = ((extractSec $ extEuclid ((head primes), m)), m)
    where m = ((primes !! 1) - 1) * ((primes !! 2) - 1)

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
    

