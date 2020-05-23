circP :: (Integral a) => a -> Bool
circP n = True

circPWorker :: (Integral a) => a -> a -> Bool
circPWorker n g
          | n == g        = True
          | (isPrime g)   = False
          | otherwise     = circPWorker n $ nextRot g

nextRot :: (Integral a) => a -> a
nextRot n = revListify n

nrWorker :: (Integral a) => [a] -> [a]
nrWorker (x:xs) = xs ++ [x]

revListToNum :: (Integral a) => [a] -> a -> a
revListToNum [] n = n
revListToNum (x:xs) n = revListToNum xs $ n + x * ( powers 10 ( length xs ) )

powers :: (Integral a) => a -> a -> a
powers 0 _ = 0
powers _ 0 = 1
powers n i
       | even i = (powers n (i `div` 2)) * (powers n (i `div` 2))
       | odd i  = n * powers n (i-1)

revListify :: (Integral a) => a -> [a]
revListify 0 = []
revListify n = n `mod` 10 : revListify (n `div` 10)

isPrime :: (Integral a) => a -> Bool
isPrime k = if k > 1 then null [ x | x <- [2..isqrt k], k `mod` x == 0] else False

-- returns nearest integer ABOVE square root
isqrt :: (Integral a) => a -> a
isqrt n = ceiling $ sqrt $ fromIntegral n
