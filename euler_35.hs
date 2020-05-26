import Data.List.Ordered
import Data.Array
import Data.List

main :: IO ()
main = print( eulerSolution )

exclude = [0,2,4,5,6,8]

eulerSolution :: (Integral a) => a
eulerSolution = 2 + ( length' 0 $ filter (circP) $ primesTo 1000000 )

circP :: (Integral a) => a -> Bool
circP n = if ( intersect exclude $ listify n ) == [] then circPWorker n $ nextRot n
                                                     else False

circPWorker :: (Integral a) => a -> a -> Bool
circPWorker n g
          | n == g            = True
          | not (isPrime g)   = False
          | otherwise         = circPWorker n $ nextRot g

nextRot :: (Integral a) => a -> a
nextRot n = listToNum $ nrWorker $ listify n

nrWorker :: (Integral a) => [a] -> [a]
nrWorker (x:xs) = xs ++ [x]

primesTo :: (Num a, Enum a, Ord a) => a -> [a]
primesTo m = 2 : sieve [3,5..m]
    where
    sieve (p:xs)
       | p*p > m   = p : xs
       | otherwise = p : sieve (xs `minus` [p*p, p*p+2*p..])

listToNumHlpr :: (Integral a) => [a] -> a -> a
listToNumHlpr [] n = n
listToNumHlpr (x:xs) n = listToNumHlpr xs $ n + x * ( powers 10 ( length' 0 xs) )

listToNum :: (Integral a) => [a] -> a
listToNum xs = listToNumHlpr xs 0

length' :: (Integral a) => a -> [a] -> a
length' a []     = a
length' a (x:xs) = length' (a+1) xs

powers :: (Integral a) => a -> a -> a
powers 0 _ = 0
powers _ 0 = 1
powers n i
       | even i = (powers n (i `div` 2)) * (powers n (i `div` 2))
       | odd i  = n * powers n (i-1)

-- revListify :: (Integral a) => a -> [a]
revListify 0 = []
revListify n =  fromIntegral (n `mod` 10) : revListify (n `div` 10)

--listify :: (Integral a) => a -> [a]
listify n = reverse $ revListify n

primeState :: Array Integer Integer
primeState = listArray (0,1000000) [arrayPopulate n | n <- [1..1000000]]

arrayPopulate :: (Integral a, Num p) => a -> p
arrayPopulate n
          | isPrimeInit n == False = 0
          | otherwise              = 1

isPrime :: (Integral a) => a -> Bool
isPrime n = integralToBool $ primeState ! ( fromIntegral n-1 )

integralToBool :: (Integral a) => a -> Bool
integralToBool 0 = False
integralToBool 1 = True

isPrimeInit :: (Integral a) => a -> Bool
isPrimeInit k = if k > 1 then null [ x | x <- [2..isqrt k], k `mod` x == 0] else False

-- returns nearest integer ABOVE square root
isqrt :: (Integral a) => a -> a
isqrt n = ceiling $ sqrt $ fromIntegral n
