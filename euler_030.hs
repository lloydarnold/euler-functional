sumDigToPower :: (Integral a) => a -> a -> a
sumDigToPower 0 _ = 0
sumDigToPower a b = powers (a `mod` 10) b + sumDigToPower (a `div` 10) b

powers :: (Integral a) => a -> a -> a
powers 0 _ = 0
powers _ 0 = 1
powers n i
       | even i = (powers n (i `div` 2)) * (powers n (i `div` 2))
       | odd i  = n * powers n (i-1)

eulerSolution :: Int
eulerSolution = sum (filter (\x -> x == sumDigToPower x 5) [2..354294])
