import Data.List.Ordered

worker n = group $ quicksort $ trialDiv n

worker2 []    = 0
worker2 x:xs  = doThatTing x + worker2 xs

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

-- trialDiv returns prime factors of number
trialDiv :: Integer -> [Integer]
trialDiv n = trialDivDr n $ relPrimes n

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
    let smallerSorted = quicksort (filter (<=x) xs)
        biggerSorted = quicksort (filter (>x) xs)
    in  smallerSorted ++ [x] ++ biggerSorted
