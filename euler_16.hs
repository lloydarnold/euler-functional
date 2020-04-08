digitSum :: (Integral a) => a -> a
digitSum 0 = 0
digitSum n = n `mod` 10 + digitSum (n `div` 10)

powers :: Integer -> Integer -> Integer
powers 0 _ = 0
powers _ 0 = 1
powers n i
       | even i = (powers n (i `div` 2)) * (powers n (i `div` 2))
       | odd i  = n * powers n (i-1)

eulerSolution :: Integer
eulerSolution = digitSum (powers 2 1000)
