import Data.List.Ordered

primesTo m = 2 : sieve [3,5..m]
    where
    sieve (p:xs)
       | p*p > m   = p : xs
       | otherwise = p : sieve (xs `minus` [p*p, p*p+2*p..])

eulerSolution :: Int
eulerSolution = (primesTo 1000000) !! 10000

main :: IO ()
main = print(eulerSolution)
