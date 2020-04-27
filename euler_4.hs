revListify :: (Integral a) => a -> [a]
revListify 0 = []
revListify n = n `mod` 10 : revListify (n `div` 10)

threeDigProds :: [Integer]
threeDigProds = [x * y | x <- [100..999], y <- [100..999]]

eulerSolution :: Integer
eulerSolution = maximum $ filter (\x -> revListify x == reverse (revListify x)) threeDigProds
