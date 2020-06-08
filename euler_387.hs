{-- A Harshad or Niven number is a number that is divisible by the sum of its digits.
201 is a Harshad number because it is divisible by 3 (the sum of its digits.)
When we truncate the last digit from 201, we get 20, which is a Harshad number.
When we truncate the last digit from 20, we get 2, which is also a Harshad number.
Let's call a Harshad number that, while recursively truncating the last digit, always results in a Harshad number a right truncatable Harshad number.

--}

import Data.Array
import Data.List.Ordered

limit = 100000000000000
-- limit = 10000

-- can't have rt trunc primes < 10
eulerSolution :: Integer
eulerSolution = sum $ filter (strongRtPrimeHarshad) $ filter (isPrime) [10..limit]

primesTo :: (Num a, Enum a, Ord a) => a -> [a]
primesTo m = 2 : sieve [3,5..m]
    where
    sieve (p:xs)
       | p*p > m   = p : xs
       | otherwise = p : sieve (xs `minus` [p*p, p*p+2*p..])

digitSum :: (Integral a) => a -> a
digitSum 0 = 0
digitSum n = n `mod` 10 + digitSum (n `div` 10)

strongRtPrimeHarshad :: Integer -> Bool
strongRtPrimeHarshad n = strongHarshad g && rtHarshad g
                        where g = rtTrunc n

isHarshad :: Integer -> Bool
isHarshad n = n `mod` digitSum n == 0

primes = 2:([3..] ‘minus‘ composites)
where
composites = union [multiples p | p <− primes]
multiples n = map (n*) [n..]
(x:xs) ‘minus‘ (y:ys) | x < y = x:(xs ‘minus‘ (y:ys))
| x == y = xs ‘minus‘ ys
| x > y = (x:xs) ‘minus‘ ys
union = foldr merge []
where
merge (x:xs) ys = x:merge’ xs ys
merge’ (x:xs) (y:ys) | x < y = x:merge’ xs (y:ys)
| x == y = x:merge’ xs ys
| x > y = y:merge’ (x:xs) ys

strongHarshad :: Integer -> Bool
strongHarshad n = n `mod` x == 0 && isPrime ( n `div` x )
                where x = digitSum n

rtHarshad :: Integer -> Bool
rtHarshad n
        | n == 0               = True
        | isHarshad n == False = False
        | otherwise            = rtHarshad $ rtTrunc n

rtTrunc :: Integer -> Integer
rtTrunc n = n `div` 10

isPrime :: (Integral a) => a -> Bool
isPrime 2 = True
isPrime k = if k > 1 then null [ x | x <- [2..isqrt k], k `mod` x == 0] else False

-- returns nearest integer ABOVE square root
isqrt :: (Integral a) => a -> a
isqrt n = ceiling $ sqrt $ fromIntegral n
