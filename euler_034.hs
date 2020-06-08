eulerFuncTR :: (Integral a) => a -> a -> a
eulerFuncTR 0 y = y
eulerFuncTR x y = eulerFuncTR (x `div` 10) (factorial (x `mod` 10))

eulerFunc :: (Integral a) => a -> a
eulerFunc 0 = 0
eulerFunc n = factorial (n `mod` 10) + eulerFunc (n `div` 10)

factorialTR :: (Integral a) => a -> a -> a
factorialTR 0 g = g
factorialTR n g = factorialTR (n-1) (n*g)

factorial :: (Integral a) => a -> a
factorial n = factorialTR n 1

genList :: (Integral a) => a -> [a]
genList n = filter(\x -> x == eulerFunc x) [3..n]

eulerSolution :: Int
eulerSolution = sum (genList 2540160)
