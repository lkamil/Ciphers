import System.Random
import Data.List

primes :: [Int]
primes = primesH [x | x <- [2..100], mod x 2 /= 0]

primesH :: Integral a => [a] -> [a]
primesH [] = []
primesH (x:xs) = x : (primesH $ filter (\y -> mod y x /= 0) xs)

randomPrime :: IO Int
randomPrime = 
    let ps = primes 
    in fmap (ps !!) $ randomRIO (0, (length ps))

