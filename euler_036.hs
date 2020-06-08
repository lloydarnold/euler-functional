bConvRev :: (Integral a) => a -> [a]
bConvRev 0 = [0]
bConvRev 1 = [1]
bConvRev n = n `mod` 2 : bConvRev (n `div` 2)

revListify :: (Integral a) => a -> [a]
revListify 0 = []
revListify n = n `mod` 10 : revListify (n `div` 10)

genDoublePalindromes :: (Integral a) => a -> [a]
genDoublePalindromes n = filter (\x -> bConvRev x == reverse (bConvRev x)) (filter (\y -> revListify y == reverse (revListify y)) [1..n])

eulerSolution :: Int
eulerSolution = sum (genDoublePalindromes 1000000)
