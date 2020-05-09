factorial :: (Integral a) => a -> a
factorial 1 = 1
factorial n = n * (factorial (n-1))

digitSum :: (Integral a) => a -> a
digitSum 0 = 0
digitSum n = n `mod` 10 + digitSum (n `div` 10)

eulerSolution :: Integer
eulerSolution = digitSum (factorial 100)
