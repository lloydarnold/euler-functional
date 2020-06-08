import Data.List.Ordered

primesTo :: (Num a, Enum a, Ord a) => a -> [a]
primesTo m = 2 : sieve [3,5..m]
    where
    sieve (p:xs)
       | p*p > m   = p : xs
       | otherwise = p : sieve (xs `minus` [p*p, p*p+2*p..])

relPrimes :: (Integral a) => Integer -> [a]
relPrimes n = primesTo $ round $ sqrt a
          where a = fromInteger n

trialDivDr :: (Integral a) => a -> [a] -> [a]
trialDivDr _ [] = []
trialDivDr n (x:xs)
         | n `mod` x == 0 = x : trialDivDr ( n `div` x ) ( primesTo n )
         | n  == x        = [x]
         | otherwise      = trialDivDr n xs

trialDiv :: Integer -> [Integer]
trialDiv n = trialDivDr n $ relPrimes n

eulerSolution :: Integer
eulerSolution = maximum $ trialDiv 600851475143
