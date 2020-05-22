digitSum :: (Integral a) => a -> a
digitSum 0 = 0
digitSum n = n `mod` 10 + digitSum (n `div` 10)

powers :: (Integral a) => a -> a -> a
powers 0 _ = 0
powers _ 0 = 1
powers n i
       | even i = (powers n (i `div` 2)) * (powers n (i `div` 2))
       | odd i  = n * powers n (i-1)

eulerList :: (Integral a) => [a]
eulerList = map (digitSum) [ powers a b | a <- [1..100], b <- [1..100]]

eulerSolution :: (Integral a) => a
eulerSolution = maximum ( eulerList )
