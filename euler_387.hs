{-- A Harshad or Niven number is a number that is divisible by the sum of its digits.
201 is a Harshad number because it is divisible by 3 (the sum of its digits.)
When we truncate the last digit from 201, we get 20, which is a Harshad number.
When we truncate the last digit from 20, we get 2, which is also a Harshad number.
Let's call a Harshad number that, while recursively truncating the last digit, always results in a Harshad number a right truncatable Harshad number.

--}

import Data.Array

limit = 100000

digitSum :: (Integral a) => a -> a
digitSum 0 = 0
digitSum n = n `mod` 10 + digitSum (n `div` 10)

isHarshad :: Integer -> Bool
isHarshad n = n `mod` digitSum n == 0

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

primeState :: Array Integer Integer
primeState = listArray (0,limit) [arrayPopulate n | n <- [1..limit]]

arrayPopulate :: (Integral a, Num p) => a -> p
arrayPopulate n
          | isPrimeInit n == False = 0
          | otherwise              = 1

isPrime :: Integer -> Bool
isPrime n = integralToBool $ primeState ! ( n-1 )

integralToBool :: (Integral a) => a -> Bool
integralToBool 0 = False
integralToBool 1 = True

isPrimeInit :: (Integral a) => a -> Bool
isPrimeInit k = if k > 1 then null [ x | x <- [2..isqrt k], k `mod` x == 0] else False

-- returns nearest integer ABOVE square root
isqrt :: (Integral a) => a -> a
isqrt n = ceiling $ sqrt $ fromIntegral n
