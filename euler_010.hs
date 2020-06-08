import Data.List.Ordered

primesTo :: (Num a, Enum a, Ord a) => a -> [a]
primesTo m = 2 : sieve [3,5..m]
    where
    sieve (p:xs)
       | p*p > m   = p : xs
       | otherwise = p : sieve (xs `minus` [p*p, p*p+2*p..])

main :: IO()
main = print(sum (primesTo 2000000))
