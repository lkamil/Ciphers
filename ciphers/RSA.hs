import System.Random
import Data.List

primeGen :: [Int]
primeGen = primeGenH [x | x <- [2..100], mod x 2 /= 0]

primeGenH :: Integral a => [a] -> [a]
primeGenH [] = []
primeGenH (x:xs) = x : (primeGenH $ filter (\y -> mod y x /= 0) xs)

randomPrime :: [Int]
randomPrime = 
    let ps = primeGen
    in map (ps !!) (take 3 $ (randomRs (0, (length ps)) (mkStdGen 6486478)))

